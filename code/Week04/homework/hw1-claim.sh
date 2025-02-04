#!/usr/bin/env bash

set -eu
set pipefail
# set -x

export DEADLINE_DATETIME='2025-02-25T01:17:24Z'

MISTERY_CMD=claim ./hw1-mistery-validator.ts
