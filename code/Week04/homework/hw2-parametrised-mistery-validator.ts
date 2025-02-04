#! /usr/bin/env -S nix shell nixpkgs#deno -c deno --allow-read --allow-env --allow-net

import "jsr:@std/dotenv/load";
const blockfrostProjectId = Deno.env.get("BLOCKFROST_PROJECT_ID");

import {
    Data,
    Lucid,
    Blockfrost,
    getAddressDetails,
    SpendingValidator,
    TxHash,
    Datum,
    UTxO,
    Address,
    AddressDetails,
} from "https://deno.land/x/lucid@0.10.11/mod.ts";

const walletSeedSpender = Deno.env.get("PPP_WALLET_SEED_SPENDER");
const walletSeedBeneficiary = Deno.env.get("PPP_WALLET_SEED_BENEFICIARY");


// set blockfrost endpoint
const lucid = await Lucid.new(
  new Blockfrost(
    "https://cardano-preview.blockfrost.io/api/v0",
    blockfrostProjectId
  ),
  "Preview"
);

lucid.selectWalletFromSeed(walletSeedBeneficiary);
const addrBeneficiary: Address = await lucid.wallet.address();
console.log(addrBeneficiary);

lucid.selectWalletFromSeed(walletSeedSpender);
const addrSpender: Address = await lucid.wallet.address();
console.log(addrSpender);

const misteryScriptJson = JSON.parse(await Deno.readTextFile("../assets/parameterized-Mistery.plutus"));
// console.log(misteryScriptJson.cborHex);

const parametrizedMisteryScript: SpendingValidator = {
    type: "PlutusV2",
    script: misteryScriptJson.cborHex
};
const parametrizedMisteryAddress: Address = lucid.utils.validatorToAddress(parametrizedMisteryScript);
console.log("Parametrized mistery address:");
console.log(parametrizedMisteryAddress);

const detailsBen: AddressDetails = getAddressDetails(addrBeneficiary);
const beneficiaryPKH: string = detailsBen.paymentCredential.hash

async function sendFunds(amount: bigint): Promise<TxHash> {
    const deadlineDateTxt = await Deno.env.get("DEADLINE_DATETIME");
    const deadlineDate: Date = new Date(deadlineDateTxt)
    const datum: Data.Integer = BigInt(deadlineDate.getTime());

    const dtm: Datum = Data.to<Data.Integer>(datum,Data.Integer);
    const tx = await lucid
      .newTx()
      .payToContract(parametrizedMisteryAddress, { inline: dtm }, { lovelace: amount })
      .complete();
    const signedTx = await tx.sign().complete();
    const txHash = await signedTx.submit();
    return txHash
}

async function listUTxO(): Promise<TxHash> {
    const utxoAtScript: UTxO[] = await lucid.utxosAt(parametrizedMisteryAddress);
    const ourUTxO: UTxO[] = utxoAtScript.filter(
      // filter by deadline
      (utxo) => Data.from<Data.Integer>(utxo.datum) <= Date.now()
    );
    console.log(ourUTxO);
    return ourUTxO;
}

async function claimVestedFunds(): Promise<TxHash> {
    const ourUTxO: UTxO[] = await listUTxO();
    lucid.selectWalletFromSeed(walletSeedBeneficiary);
    if (ourUTxO && ourUTxO.length > 0) {
        const tx = await lucid
            .newTx()
            .collectFrom(ourUTxO, Data.void())
            .addSignerKey(beneficiaryPKH)
            .attachSpendingValidator(parametrizedMisteryScript)
            .validFrom(Date.now()-100000)
            .complete();
        const signedTx = await tx.sign().complete();
        const txHash = await signedTx.submit();
        console.log("https://preview.cardanoscan.io/transaction/" + txHash);
        return txHash
    }
    else return "No UTxO's found that can be claimed"
}

switch (await Deno.env.get("MISTERY_CMD")) {

  case "send":
    console.log("https://preview.cardanoscan.io/transaction/" + await sendFunds(2220000000n));
    break;

  case "claim":
    console.log(await claimVestedFunds());
    break;

  default:
    await listUTxO();
    break;

}

