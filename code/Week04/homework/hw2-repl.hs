
:set prompt  "> "
:set -XOverloadedStrings
:set -fno-warn-type-defaults
:set -w
:set +m
:set -v0

import Plutus.V2.Ledger.Api
import Homework2 as H
import Utilities

:t H.saveVal

-- addr = "addr_test1qpentr2dydeqlwhgsmk3l495dyc7kczckr6vmmsk2mcj4x5av5c7lhecc950cunr4f8c69zy9cjp2p3qquw65ud370cs9ctgpv"
pkh = PubKeyHash $ toBuiltin $ bytesFromHex "73358d4d23720fbae886ed1fd4b46931eb6058b0f4cdee1656f12a9a"

-- create ./assets/parameterized-Mistery.plutus
H.saveVal pkh


