#!/bin/sh
set -eu

echo '🚂 Migrate schema...';
stack runghc src/App/Util/Migrate.hs;
echo '👌 Done.'
