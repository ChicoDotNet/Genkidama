#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."
composer validate --strict --no-check-publish
find src public tests -type f -name '*.php' -print0 | xargs -0 -n1 php -l >/dev/null
vendor/bin/phpunit
