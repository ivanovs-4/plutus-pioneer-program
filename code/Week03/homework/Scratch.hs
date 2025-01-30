
:set prompt  "> "
:set -XOverloadedStrings
:set -fno-warn-type-defaults
:set -w
:set +m
:set -v0

import Plutus.V1.Ledger.Interval as J
import Data.Function

j = J.interval


j 1 2
j 2 1
J.singleton 3

f = \x -> x {ivTo = ivTo x & \(UpperBound n _) -> UpperBound n False}
(j 2 3)
f (j 2 3)
contains (f (j 1 3)) (j 2 3)

contains (J.from 1) (j 2 3)
contains (J.from 2) (j 1 3)
