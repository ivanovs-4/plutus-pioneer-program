#!/usr/bin/env bash

set -eu
set pipefail
# set -x

export DEADLINE_DATETIME='2023-03-19T00:00:00Z'

MISTERY_CMD=send ./hw1-mistery-validator.ts
