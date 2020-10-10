#!/bin/bash

DIR="data"
PARAM=${1:?}
PREFIX=`date "+%Y%m%d%H%M%S"`
FILENAME="${PREFIX}_${PARAM}.migration"

if [ ! -d ${DIR} ]; then
    echo "Run command in the top directory of your Houbou project."
    exit 1
fi

touch ${DIR}/${FILENAME}

echo "created ... ${DIR}/${FILENAME}"
