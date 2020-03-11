#!/bin/sh
set -eu

DB_NAME=coconut-dev

echo '⏮  delete DB if exists and create...';
dropdb --if-exists "$DB_NAME" 
createdb $DB_NAME;
echo '👌 Done.'
