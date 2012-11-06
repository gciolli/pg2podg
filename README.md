pg2podg 0.1.3
=============

pg2podg is a PostgreSQL 9.1+ extension for two-player open deterministic
games.

Build
-----

To build **pg2podg**, just type:

    make
    make installcheck
    make install

If you encounter an error such as:

    "Makefile", line 8: Need an operator

you need to use GNU make, which may well be installed on your system as
`gmake`:

    gmake
    gmake install
    gmake installcheck

If you encounter an error such as:

    make: pg_config: Command not found

be sure that you have `pg_config` installed and in your path. If you
used a package management system such as RPM to install PostgreSQL, be
sure that the `-devel` package is also installed. If necessary tell the
build process where to find it:

    env PG_CONFIG=/path/to/pg_config make && make installcheck && make install

Usage
-----

Once pg2podg is installed, you can add it to a database. You must be
running PostgreSQL 9.1 or greater, so it's a simple as connecting to a
database as a super user and running:

    CREATE EXTENSION pg2podg;

Dependencies
------------

Extension pg2podg must be installed on top of existing database objects
that are specific of a certain game, which normally are provided by a
separate PostgreSQL extension.

For instance, in the case of Chess, the required extension is
**pgchess**; it is enough to create the pg2podg extension after having
created the pgchess extension.

Both pgchess and pg2podg are available via [the PostgreSQL Extension
Network](http://pgxn.org).

Upgrades from previous versions
-------------------------------

Currently the only way to upgrade from a previous version of pg2podg is
to drop the extension, uninstall the old version, install the new
version and finally (re)create the extension.

The pg2podg extension contains two persistent tables `games` and
`gains`, whose contents are not preserved by the above procedure; future
versions of pg2podg will provide facilities to allow the user to export
and import their contents.

Copyright and Licence
---------------------

Copyright (c) 2010, 2011, 2012 Gianni Ciolli.

This module is free software; you can redistribute it and/or modify it
under the [GNU General Public License version 3 or
later](http://www.gnu.org/copyleft/gpl.html).
