#!/usr/bin/env bash

set -ex

elm make "src/Main.elm" --output "target/index.html"
