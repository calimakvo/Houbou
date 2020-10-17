#!/bin/bash

DIR="migrate"
PARAM=${1:?}
PREFIX=`date "+%Y%m%d%H%M%S"`
FILENAME="${PREFIX}_${PARAM}.migration"

touch ${DIR}/${FILENAME}

echo "created ... ${DIR}/${FILENAME}"
