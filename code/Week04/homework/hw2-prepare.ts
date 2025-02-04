#! /usr/bin/env -S nix shell nixpkgs#deno -c deno --allow-read --allow-env --allow-net

import {
    Lucid,
    Blockfrost,
    getAddressDetails,
    Address,
    AddressDetails,
} from "https://deno.land/x/lucid@0.10.11/mod.ts";

import "jsr:@std/dotenv/load";
const blockfrostProjectId = Deno.env.get("BLOCKFROST_PROJECT_ID");
const lucid = await Lucid.new(
  new Blockfrost(
    "https://cardano-preview.blockfrost.io/api/v0",
    blockfrostProjectId
  ),
  "Preview"
);

const walletSeedBeneficiary = Deno.env.get("PPP_WALLET_SEED_BENEFICIARY");

lucid.selectWalletFromSeed(walletSeedBeneficiary);
const addrBeneficiary: Address = await lucid.wallet.address();
console.log("ADDR:" + addrBeneficiary);

const detailsBen: AddressDetails = getAddressDetails(addrBeneficiary);
const beneficiaryPKH: string = detailsBen.paymentCredential.hash

console.log("PKH:" + beneficiaryPKH);

// ADDR:addr_test1qpentr2dydeqlwhgsmk3l495dyc7kczckr6vmmsk2mcj4x5av5c7lhecc950cunr4f8c69zy9cjp2p3qquw65ud370cs9ctgpv
// PKH:73358d4d23720fbae886ed1fd4b46931eb6058b0f4cdee1656f12a9a
