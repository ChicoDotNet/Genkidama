#!/usr/bin/env bash
set -euo pipefail

curl -fsSL https://github.com/VirtusLab/scala-cli/releases/latest/download/scala-cli-x86_64-pc-linux.gz -o /tmp/scala-cli.gz
gzip -df /tmp/scala-cli.gz
sudo install -m 0755 /tmp/scala-cli /usr/local/bin/scala-cli

curl -fsSL https://github.com/clojure/brew-install/releases/latest/download/linux-install.sh -o /tmp/clojure-install.sh
chmod +x /tmp/clojure-install.sh
sudo /tmp/clojure-install.sh

scala-cli version
clojure --version
