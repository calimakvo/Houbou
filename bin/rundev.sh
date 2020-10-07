#!/bin/bash

export YESOD_PGUSER=devuser
export YESOD_PGPASS=devuser
export YESOD_PGHOST=localhost
export YESOD_PGPORT=5432
export YESOD_PGDATABASE=houbou
export YESOD_PGPOOLSIZE=10

stack exec -- yesod devel
