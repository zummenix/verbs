#!/bin/bash

set -ex

elm make "src/Main.elm" --output "target/index.html" --warn
