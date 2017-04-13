#!/bin/bash
# Prepares the material to build a Docker image
set -o xtrace

cp deploy/deploy-settings.yml config/settings.yml
stack build
TARGET=deploy/dist
mkdir "$TARGET"
cp "$(find -executable -type f | grep circuit-yesod$ | head -n1)" "$TARGET"
cp -r static $TARGET
cp -r config $TARGET
