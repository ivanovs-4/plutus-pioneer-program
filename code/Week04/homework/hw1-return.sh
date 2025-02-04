#!/usr/bin/env bash

set -eu
set pipefail
# set -x

# export DEADLINE_DATETIME='2025-02-25T01:17:24Z'
export DEADLINE_DATETIME='2023-03-19T00:00:00Z'

MISTERY_CMD=return-unclaimed ./hw1-mistery-validator.ts
