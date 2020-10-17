#!/bin/bash

USER="devuser"
PASS="devuser"
HOST="localhost"
PORT="5432"
DBNAME="houbou"
DIR="./migrate/"

migrate migrate postgresql://${USER}:${PASS}@${HOST}:${PORT}/${DBNAME} ${DIR}

if [ $? -gt 0 ]; then
    echo "Check initialize migration."
    echo "  migrate init postgres://${USER}:${PASS}@${HOST}:${PORT}/${DBNAME}"
fi
