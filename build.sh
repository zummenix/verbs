#!/bin/bash

set -ex

cp -rf "src/styles/" "target/styles/"
elm make "src/Main.elm" --output "target/index.html" --warn
