
/workspace/scripts/create-key-pair.sh ali
/workspace/scripts/query-address.sh addr_test1vqleud6hluyu0z9j6k2larkse05puwagsra2tskvrdrgcvqctjpn3

/workspace/scripts/create-key-pair.sh bab
/workspace/scripts/query-address.sh addr_test1vpu8kmnyccqtw8ta8wurxwuesaae5je2v29qknz4zecp3qqpxl4e6

./scripts/make-gift.sh ali 44de08240559aa9e112130c13ff3b88c0009d76d9f9ae7211598a49bac8ed33f#0
# -> https://preview.cardanoscan.io/transaction/6d4a76fe24e20e80f73d6342fb9a7216f84fb4d9d8e9c933f77bc301caf6649f

/workspace/scripts/query-address.sh addr_test1wqag3rt979nep9g2wtdwu8mr4gz6m4kjdpp5zp705km8wys6t2kla | grep 1234567
# -> 6d4a76fe24e20e80f73d6342fb9a7216f84fb4d9d8e9c933f77bc301caf6649f

./scripts/collect-gift.sh ali \
  6d4a76fe24e20e80f73d6342fb9a7216f84fb4d9d8e9c933f77bc301caf6649f#1 \
  6d4a76fe24e20e80f73d6342fb9a7216f84fb4d9d8e9c933f77bc301caf6649f#0
# -> https://preview.cardanoscan.io/transaction/d15af17941a6e076f477bec28f2ad0a06c3a1b509beb5839483a9479c885496b

