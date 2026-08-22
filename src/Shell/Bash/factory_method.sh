#!/usr/bin/env bash
set -euo pipefail

postgres_connect() { printf '%s\n' 'PostgreSQL connect'; }
postgres_query() { printf '%s\n' 'PostgreSQL query'; }
mysql_connect() { printf '%s\n' 'MySQL connect'; }
mysql_query() { printf '%s\n' 'MySQL query'; }

create_postgres() { printf '%s' 'postgres'; }
create_mysql() { printf '%s' 'mysql'; }

use_database() {
  local create_database="$1"
  local product
  product="$($create_database)"
  "${product}_connect"
  "${product}_query"
}

use_database create_postgres
use_database create_mysql
