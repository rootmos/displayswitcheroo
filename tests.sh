#!/bin/bash

set -o nounset -o pipefail -o errexit

APP=${APP-displayswitcheroo}

which "$APP"

xvfb-run "$APP" list
