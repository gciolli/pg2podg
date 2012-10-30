-- pg2podg, an extension for 2-player open deterministic games
-- Copyright (C) 2010, 2011, 2012 Gianni Ciolli <gianni.ciolli@2ndQuadrant.it>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
------------------------------------------------------------------------

-- complain if script is sourced in psql, rather than via CREATE EXTENSION
\echo Use "CREATE EXTENSION pg2podg" to load this file. \quit

--------------------------------------------------------------------------------
-- Notation. We denote with "game" a valid state of a given game.             --
--                                                                            --
-- Prerequisites. We require the following database objects:                  --
--                                                                            --
-- * type game                                                                --
--                                                                            --
-- * operator ## game : text                                                  --
--     returning the human-readable form of a given game                      --
--                                                                            --
-- * operator %% game : text                                                  --
--     returning the text ID of a given game. Uniqueness is not		      --
--     mandatory, as it is used only in display and for regression	      --
--     tests (e.g. FEN for Chess)    		    			      --
--                                                                            --
-- * type move                                                                --
--                                                                            --
-- * operator game ^ move : game                                              --
--     representing the application of a valid move to a given game           --
--                                                                            --
-- * function new_game () : game                                              --
--     producing an initial game                                              --
--                                                                            --
-- * function score(game) : double precision                                  --
--     scoring a given game (finite value IFF the game is not finished)       --
--                                                                            --
-- * function gain(game) : double precision				      --
--     equivalent to score(game) - score(parent_game), but		      --
--     it might be computed separately for efficiency  			      --
--                                                                            --
-- * function valid_moves(game) : SETOF move                                  --
--                                                                            --
-- Notation. We say that a game G' is visible from a game G if there is       --
-- a sequence of 0+ valid moves M1,...,Mk such that                           --
--                                                                            --
--   G ^ M1 ^ ... ^ Mk = G' .                                                 --
--									      --
-- The cone of a given game G is the set of all the games G' which are	      --
-- visible from G.						      	      --
--									      --
--------------------------------------------------------------------------------

--
-- Utility functions
--

CREATE FUNCTION to_text(double precision)
RETURNS text
LANGUAGE SQL
AS $BODY$
SELECT CASE $1
WHEN double precision  'Infinity' THEN '+oo'
WHEN double precision '-Infinity' THEN '-oo'
ELSE round($1 :: numeric, 3) :: text
END $BODY$;

--
-- Known games are stored in the "games" table.
--

CREATE TABLE games
( id BIGSERIAL PRIMARY KEY
, parent bigint REFERENCES games(id)
, move move
, game game NOT NULL
);

CREATE TABLE gains
( id BIGSERIAL PRIMARY KEY
, gain double precision NOT NULL
);

CREATE FUNCTION hash(game)
RETURNS text
IMMUTABLE
LANGUAGE SQL
AS $$ SELECT md5($1 :: text) $$;

--
-- Remember that the SQL standard doesn't allow UNIQUE constraints on
-- expressions...
--
CREATE UNIQUE INDEX games_game_key ON games((hash(game)));

--
-- We begin with the initial game.
--

INSERT INTO games(game)
  SELECT new_game();

--
-- The following function computes new games inside a given cone, up
-- to a given target, which is specified by a (maximum) time and a
-- (minimum) depth. The maximum time applies only after depth 1 has
-- been completed, so that we consider at least all the available next
-- moves. Maximum time applies only if NOT NULL.
--

CREATE FUNCTION research_games
( cone_vertex bigint
, time_target interval
, depth_target int
, starting_timestamp timestamp with time zone DEFAULT clock_timestamp()
) RETURNS void
LANGUAGE plpgsql AS $BODY$
BEGIN
	WITH RECURSIVE cone(id, parent, depth) AS (
	  SELECT cone_vertex
	  , NULL::bigint
	  , 0 AS depth
	UNION ALL
	  SELECT g.id
	  , g.parent
	  , c.depth + 1 AS depth
	  FROM cone c
	  JOIN games g ON c.id = g.parent
	), buds(id, parent, depth) AS (
	  SELECT p.*
	  FROM cone p
	  LEFT JOIN cone c ON c.parent = p.id
	  WHERE c.id IS NULL
	), r(id, parent, move, game, depth) AS (
	  SELECT b.id
	  , b.parent
	  , g.move
	  , g.game
	  , b.depth
	  FROM buds b
	  JOIN games g ON g.id = b.id
	UNION ALL
	  SELECT id
	  , parent
	  , move
	  , game^move AS game
	  , depth
	  FROM (
	    SELECT nextval('games_id_seq') AS id
	    , r.id AS parent
	    , r.game
	    , valid_moves(r.game) AS move
	    , r.depth + 1 AS depth
	    FROM r
	    WHERE depth = 0 OR
	      (
	        (
		  time_target IS NULL OR
	          age(clock_timestamp(), starting_timestamp) < time_target
		)
	        AND depth < depth_target
	      )
	  ) x(id, parent, game, move, depth)
	), new_games(id, parent, move, game) AS (
	  SELECT r.id
	  , r.parent
	  , r.move
	  , r.game
	  FROM r
	  LEFT JOIN games g ON r.id = g.id
	  WHERE g.id IS NULL
	)
	INSERT INTO games
	TABLE new_games;
END;
$BODY$;

CREATE FUNCTION compute_gains()
RETURNS void
LANGUAGE plpgsql
AS $BODY$
BEGIN
	INSERT INTO gains(id,gain)
	SELECT c.id, gain(p.game, c.game)
	FROM games c
	JOIN games p ON c.parent = p.id
	LEFT JOIN gains g ON c.id = g.id
	WHERE g.id IS NULL;
END;
$BODY$;

--
-- The unlogged "choices" table stores the set of next moves currently
-- under consideration.
--

CREATE UNLOGGED TABLE choices
( game bigint
, total_gain double precision
, gains text[]
);

CREATE VIEW choices_txtid AS
SELECT c.*, %% g.game AS txtid
FROM choices c
JOIN games g ON c.game = g.id;

CREATE FUNCTION array_sum(double precision[])
RETURNS double precision
LANGUAGE SQL
AS $$
SELECT sum(x) FROM unnest($1) f(x)
$$;

--
-- This function replaces the contents of the "choices" table with a
-- fresh evaluation of all the valid next moves.
--

CREATE FUNCTION evaluate_choices
( start_game bigint
, depth_target int
) RETURNS void
LANGUAGE plpgsql
AS $BODY$
BEGIN
	TRUNCATE choices;
	WITH RECURSIVE
	--
	-- (1) compute the cone of required games
	--
	cone(id,parent,depth) AS (
	  SELECT id
	  , parent
	  , 1 :: int AS depth
	  FROM games
	  WHERE parent = start_game
	UNION ALL
	  SELECT c.id
	  , c.parent
	  , p.depth + 1 AS depth
	  FROM cone p
	  JOIN games c ON c.parent = p.id
	  WHERE p.depth < depth_target
	--
	-- (2) compute g0(cone), reusing any previous computation and
	--     saving any new computation for later reuse
	--
	), g0_hit(id, g0) AS (
	  SELECT cone.id
	  , gains.gain
	  FROM cone
	  JOIN gains ON cone.id = gains.id
	), g0_miss(id, g0) AS (
	  INSERT INTO gains(id, gain)
	  SELECT a.id
	  , score(games.game) AS g0
	  FROM (
	    SELECT cone.id
	    FROM cone
	    LEFT JOIN gains ON cone.id = gains.id
	    WHERE gains.id IS NULL
	  ) a JOIN games ON a.id = games.id
	  RETURNING *
	), g0 AS (
	  SELECT id, g0 FROM g0_hit
	  UNION ALL
	  SELECT id, g0 FROM g0_miss
	--
	-- (3) recursively compute g(cone) from g0(cone)
	--
	), eva AS (
  	  SELECT cone.id
	  , cone.parent
	  , cone.depth
	  , 1 :: int AS height
	  , g0.g0 AS g
	  , '{}' :: text[] AS sub_gs
	  FROM g0 JOIN cone ON g0.id = cone.id
	UNION ALL
	  SELECT id
	  , parent
	  , depth
	  , height
	  , g
	  , sub_gs
	  FROM (
	    SELECT cone.id
	    , cone.parent
	    , cone.depth
	    , eva.height + 1 AS height
	    , g0.g0 - max(eva.g)        OVER w AS g
	    , array_agg(to_text(eva.g)) OVER w AS sub_gs
	    , row_number()              OVER w AS n
	    FROM cone
	    JOIN eva ON cone.id = eva.parent
	    JOIN g0 ON cone.id = g0.id
	    WINDOW w AS (PARTITION BY cone.id)
	    --
	    -- Note: we are simulating an aggregate via a Window
	    -- function followed by a select, because the current CTE
	    -- implementation forbids the former but not the latter.
	    -- 
	  ) a
	  WHERE n = 1
	), informed_choices AS (
	  SELECT id
	  , g
	  , sub_gs
	  FROM (
	    SELECT id
	    , g
	    , sub_gs
	    , height
	    , max(height) OVER () AS max_height
	    FROM eva
	  ) a
	  WHERE height = max_height
	)
	INSERT INTO choices
	SELECT id, g, sub_gs
	FROM informed_choices;
END;
$BODY$;

COMMENT ON FUNCTION evaluate_choices(bigint,int) IS '
We model fuzzy logic via the completion of the real axis: plus
and minus infinity denote respectively victory and defeat, while
finite numbers represent cases where the outcome is uncertain, with 0
representing a 50/50 split.
 
To each future game G we attach a gain corresponding to the move M
such that G0 ^ M = G, where G0 is the parent game of G. Precisely:
 
* (W) +infinity if M is a winning move
* (L) -infinity if M is a losing move
* (O) a finite value otherwise
 
We denote sequences of moves by [M1,M2,...].
 
The first move in the sequence is always valued as it is, because if
we choose that sequence then that move is certain.
 
Subsequent moves will contribute with a "dampened" gain, e.g. a
winning move in fifth position is not certain and should be valued
with a finite number, because it depends on the actions of the other
player, which are unpredictable in general.
 
As a special case, however, we assume that the next player is clever
enough to always choose an immediately winning move, and to always
avoid an immediately losing move. Therefore, a sequence [O,W] is
equivalent to a single losing move [L] (the symmetrical statement
is not useful here: a sequence [O,L] will always be discarded
unless there are no other [O,...]  alternatives).
 
Summing up, we have the following four cases:
 
* [W]        ->  +Infinity
* [L]        ->  -Infinity
* [O,W]      ->  -Infinity
* [O,O,...]  ->  a finite number
';

--
-- User interface
--

--
-- The unlogged "status" table stores the current state
--

CREATE UNLOGGED TABLE status AS
SELECT id
, game
, NULL :: interval AS research_time
, NULL :: interval AS evaluate_time
, false :: boolean AS give_up
FROM games
WHERE false;

--
-- Two useful views, representing the fact that a draw is better than
-- a loss and worse than a win (see "apply_best_choice" below).
--

CREATE VIEW choices_when_losing AS
  SELECT c.*,
  row_number() OVER (
    ORDER BY CASE total_gain
             WHEN double precision 'NaN' THEN double precision 'Infinity' 
             WHEN double precision 'Infinity' THEN double precision 'NaN' 
             ELSE total_gain
             END DESC
  ) AS ord
  FROM choices c;

CREATE VIEW choices_when_not_losing AS
  SELECT c.*
  , row_number() OVER (
    ORDER BY CASE
             WHEN total_gain = double precision 'NaN' THEN -1
             WHEN total_gain < 0 THEN total_gain - 2
             ELSE total_gain
             END DESC
  ) AS ord
  FROM choices c;

--
-- This function refreshes the evaluation of valid choices, and then
-- updates the current game state by applying the best available
-- choice. It returns false if there are no moves available.
--

CREATE FUNCTION apply_best_choice
( depth_target int
) RETURNS boolean
LANGUAGE plpgsql
AS $BODY$
DECLARE
	x record;
	y bigint;
	t games;
BEGIN
	SELECT id, game INTO STRICT x
	FROM status;
	
	PERFORM evaluate_choices(x.id, depth_target := depth_target );

	--
	-- Our willingness to accept a draw depends on whether we are
	-- losing or not. Precisely, for any C positive and finite:
	--
	-- * if we are not losing:
	--
	--     Infinity > C > 0 > NaN > -C > -Infinity
	--
	--   so that NaN acts like a "0 minus".
	--
	-- * if we are losing:
	--
	--     Infinity > NaN > C > 0 > -C > -Infinity
	--
	--   so that NaN acts like a smaller "Infinity".
	--
	-- On the other hand, IEEE specifies that
	--
	--     NaN > Infinity > C > 0 > -C > -Infinity
	--
	-- hence we remap values so that the IEEE ordering induces the
	-- desired ordering.

	IF score(x.game) < 0 THEN
		-- we are losing, so draw = smaller Infinity
		SELECT game INTO y
		FROM choices_when_losing
		ORDER BY ord
		LIMIT 1;
	ELSE
		-- we are not losing, so draw = 0 minus
		SELECT game INTO y
		FROM choices_when_not_losing
		ORDER BY ord
		LIMIT 1;
	END IF;

	IF NOT FOUND THEN
		RETURN false;
	ELSE
		SELECT * INTO STRICT t
		FROM games
		WHERE id = y;
		
		UPDATE status
		SET (id,game) = (t.id,t.game);
		
		RETURN true;
	END IF;
END;
$BODY$;

--
-- Function to display the status in a VT100 terminal
--

CREATE FUNCTION status_display_vt100
( iter int
, dt1 interval
, dt2 interval
, erase boolean DEFAULT true
) RETURNS text
LANGUAGE plpgsql
AS $BODY$
BEGIN
	RETURN	CASE WHEN erase THEN E'[2J[H' ELSE '' END
		|| iter
		|| ' turns to go ('
		|| coalesce((SELECT count(1)::text FROM games), '<NULL>')
		|| ' games)'
		|| coalesce('; dt = '
			|| EXTRACT(epoch FROM date_trunc('millisecond', dt1))
			|| 's + '
			|| EXTRACT(epoch FROM date_trunc('millisecond', dt2))
			|| 's','')
		|| E'\n'
		|| coalesce((SELECT (CASE WHEN erase
		   		    	  THEN ## game
					  ELSE # game END)
		             FROM status), '<NULL>')
		|| E'\n'
		|| CASE WHEN erase THEN E'[11;0H' ELSE '' END;
END;
$BODY$;

--
-- This function performs one AI iteration.
--

CREATE FUNCTION ui_CPU_moves
( time_target interval DEFAULT '1 second'
, depth_target int DEFAULT 2
) RETURNS void
LANGUAGE plpgsql AS
$BODY$
DECLARE
	cone_vertex int;
	t timestamp with time zone;
BEGIN
	SELECT id
	INTO STRICT cone_vertex
	FROM status;
	t := clock_timestamp();
	PERFORM research_games
	  ( cone_vertex  := cone_vertex
	  , time_target  := time_target
	  , depth_target := depth_target
	  );
	UPDATE status
	SET research_time = clock_timestamp() - t;
	t := clock_timestamp();
	PERFORM compute_gains();
	UPDATE status
	SET evaluate_time = clock_timestamp() - t;
	UPDATE status
	SET give_up = NOT apply_best_choice(depth_target);
END;
$BODY$;

--
-- This function performs one Human iteration.
--

CREATE FUNCTION ui_Human_moves
( human_move text
) RETURNS void
LANGUAGE plpgsql AS
$BODY$
DECLARE
	x RECORD;
	m move;
	g game;
	i bigint;
BEGIN
	UPDATE status
	SET (research_time,evaluate_time) = (NULL,NULL);
	CASE human_move
	WHEN 'bye' THEN
		UPDATE status SET give_up = true;
	ELSE
		SELECT id, game
		INTO STRICT x
		FROM status;
		m := parse_move(human_move, x.game);
		PERFORM 1
		FROM valid_moves(x.game) vm(x1,y1,x2,y2,ppc)
		WHERE ROW(x1,y1,x2,y2,ppc) :: move = m;
		IF NOT FOUND THEN
			RAISE EXCEPTION 'Move % is not valid', # m;
		END IF;
		g := x.game^m;
		SELECT id
		INTO i
		FROM games
		WHERE game = g
		AND parent = x.id;
		IF NOT FOUND THEN
			INSERT INTO games(game, parent, move)
			VALUES (g, x.id, m)
			RETURNING id
			INTO i;
		END IF;
		UPDATE status
		SET (id,game) = (i,g);
	END CASE;
END;
$BODY$;

--
-- This function displays the final state of a finished game.
--

CREATE FUNCTION display_game_end
( iter int DEFAULT NULL
, regress boolean DEFAULT false
) RETURNS text
LANGUAGE plpgsql
AS $BODY$
DECLARE
	v_gain double precision;
	v_x text;
BEGIN
	SELECT gains.gain
	INTO STRICT v_gain
	FROM status
	LEFT JOIN gains ON status.id = gains.id;
	v_x := CASE v_gain
	       WHEN double precision 'Infinity' THEN 'with a victory'
	       WHEN double precision 'NaN' THEN 'with a draw'
	       ELSE COALESCE('after ' || iter :: text || ' iterations','')
	       END;
	IF regress THEN
		RETURN 'game ended ' || v_x;
	ELSE
		RETURN E'[1K[12;0H[31mgame ended ' || v_x || '[m[2A';
	END IF;
END;
$BODY$;

--
-- This function performs a given number of full AI iterations in a
-- single transaction. It can be used to run a single-transaction CPU
-- v CPU game, as well as to run a regression test.
--

CREATE FUNCTION ui_loop
( iter int DEFAULT 1
, time_target interval DEFAULT '1 second'
, depth_target int DEFAULT 2
, restart boolean DEFAULT false
, regress boolean DEFAULT false
, pause float DEFAULT 0
) RETURNS void
LANGUAGE plpgsql
AS $BODY$
DECLARE
	cone_vertex int;
	t timestamp with time zone;
	v_dt1 interval;
	v_dt2 interval;
	v_iter int := 0;
	v_r RECORD;
BEGIN
	IF restart THEN
		TRUNCATE status;
		INSERT INTO status
		SELECT id, game
		FROM games
		WHERE parent IS NULL
		ORDER BY id
		LIMIT 1;
	END IF;
	LOOP
		IF regress THEN
			RAISE INFO E'% turns to go (% games)\nID: %'
			, iter - v_iter
			, (SELECT count(1) FROM games)
			, (SELECT %% game FROM status)
			;
		ELSE
			RAISE INFO '%',
			      status_display_vt100(iter - v_iter, v_dt1, v_dt2);
			PERFORM pg_sleep(pause);
		END IF;
		EXIT WHEN v_iter = iter;
		PERFORM ui_CPU_moves(time_target, depth_target);
		SELECT research_time, evaluate_time, give_up
		INTO STRICT v_r
		FROM status;
		IF v_r.give_up THEN
			EXIT;
		END IF;
		v_dt1 := v_r.research_time;
		v_dt2 := v_r.evaluate_time;
		IF regress THEN
			RAISE INFO E'Score: %\nChoices:\n\t%'
			, (SELECT to_text(score(game))
			   FROM status
			   LIMIT 1)
			, (SELECT array_to_string(
			     array_agg(to_text(total_gain) || ' ' ||
			     	       c.gains     :: text || ' ' ||
				       c.txtid
			     	       ORDER BY total_gain DESC
				       , txtid)
			     , E'\n\t')
			   FROM choices_txtid c)
			;
		END IF;
		v_iter := v_iter + 1;
	END LOOP;
	RAISE INFO '%', display_game_end(iter := v_iter, regress := regress);
END;
$BODY$;

--
-- This function produces an SQL script that plays one turn and then
-- produces another SQL script that does the same, starting a
-- multi-transactional game.
--
-- Side is an integer between 1 and 6:
--
-- 1,2,3 is white
-- 4,5,6 is black
--
-- The match is: player N v player (7 - N)
--
-- 2 and 4 are humans; 1, 3, 5, 6 are CPU.
--
-- In particular:
--
-- a game starting with 1 is CPU v CPU
-- a game starting with 2 is Human v CPU
-- a game starting with 3 is CPU v Human
--

CREATE FUNCTION ui_multi_loop
( side int
, iter int
, time_target interval DEFAULT '1 second'
, depth_target int DEFAULT 2
) RETURNS SETOF text
LANGUAGE plpgsql
AS $BODY$
DECLARE
	parity int := 1;
	v_give_up boolean;
BEGIN
	IF side < 4 THEN parity := 2; END IF;
	SELECT give_up
	INTO v_give_up
	FROM status LIMIT 1;
	IF NOT FOUND THEN
		RAISE EXCEPTION 'No game is currently loaded.';
	END IF;
	RETURN NEXT
$_$--
-- (1) display current game
-- 

SELECT status_display_vt100($_$ || iter || $_$, research_time, evaluate_time)
FROM status;$_$;

	IF v_give_up THEN
		RETURN NEXT $_$\echo $_$ || display_game_end();
		RETURN NEXT $_$\echo$_$;
		RETURN NEXT $_$\echo$_$;
		RETURN NEXT $_$\q$_$;
	ELSE
		IF side = 2 OR side = 4 THEN
			RETURN NEXT $_$
--
-- (2) Human moves
-- 

\prompt 'Your move? ' next_human_move
SELECT * FROM ui_Human_moves(:'next_human_move');$_$;
		ELSE
			RETURN NEXT $_$
--
-- (2) CPU moves
-- 

SELECT * FROM ui_CPU_moves(interval '$_$ || time_target || $_$', $_$ || depth_target || $_$);$_$;
		END IF;

		IF iter > 0 THEN
			RETURN NEXT $_$
--
-- (3) tail invocation of ui_multi_loop
--
  
\pset format unaligned
\pset tuples_only t
\o var-play-$_$ || parity || $_$.sql
SELECT * FROM ui_multi_loop
( side := $_$ || (7 - side) :: text || $_$
, iter := $_$ || (iter - 1) :: text || $_$
, time_target := interval '$_$ || time_target :: text || $_$'
, depth_target := $_$ || depth_target :: text || $_$
);
\o
\i var-play-$_$ || parity || $_$.sql$_$;
		END IF;
	END IF;
END;
$BODY$;
