#!/bin/bash
# Prepares the material to build a Docker image

if [[ ! -d config ]]; then
  echo "Please switch to repository root directory first"
  exit 1
fi

set -o xtrace
rm -rf .stack-work/dist
cp deploy/deploy-settings.yml config/settings.yml
stack build
TARGET=deploy/dist
rm -rf $TARGET
mkdir -p "$TARGET"
cp "$(find -executable -type f | grep circuit-yesod$ | head -n1)" "$TARGET"
cp -r static $TARGET
cp -r config $TARGET
