#!/usr/bin/env bash

set -eu
set pipefail
# set -x

export DEADLINE_DATETIME=$(date -u +"%FT%TZ" -d '+3 weeks')
echo "deadline: $DEADLINE_DATETIME"

MISTERY_CMD=send ./hw1-mistery-validator.ts

# deadline: 2025-02-25T01:17:24Z

# $./hs1-send-to-mistery-validator-deadline-is-in-the-future.sh
# addr_test1qpentr2dydeqlwhgsmk3l495dyc7kczckr6vmmsk2mcj4x5av5c7lhecc950cunr4f8c69zy9cjp2p3qquw65ud370cs9ctgpv
# addr_test1qqgrn6rplm9vy9eyhz37860xergwj95zme8h0905dhhjk6g0zuamh2gcqkw9wsz5nswm4kz59a73tv7s2kf3m9qvh94shx3glc
# Mistery address:
# addr_test1wp7tkpxv6k9q2nzlm2spq0zv47k5eqkxnefgveqf9fezjhqh9n87x
# https://preview.cardanoscan.io/transaction/13e2f2bbc36768d3ec4dbfe27292f37c25abe486c9a7d409bebe0c97d8b36c29

