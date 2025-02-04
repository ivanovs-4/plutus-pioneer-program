#! /usr/bin/env -S nix shell nixpkgs#deno -c deno --allow-read --allow-env --allow-net

import "jsr:@std/dotenv/load";
const blockfrostProjectId = Deno.env.get("BLOCKFROST_PROJECT_ID");
// console.log(blockfrostProjectId);

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
const walletSeedBeneficiary1 = Deno.env.get("PPP_WALLET_SEED_BENEFICIARY");
const walletSeedBeneficiary2 = walletSeedSpender;


// set blockfrost endpoint
const lucid = await Lucid.new(
  new Blockfrost(
    "https://cardano-preview.blockfrost.io/api/v0",
    blockfrostProjectId
  ),
  "Preview"
);

lucid.selectWalletFromSeed(walletSeedBeneficiary1);
const addrBeneficiary1: Address = await lucid.wallet.address();
console.log(addrBeneficiary1);

lucid.selectWalletFromSeed(walletSeedSpender);
const addrSpender: Address = await lucid.wallet.address();
console.log(addrSpender);

const misteryScriptJson = JSON.parse(await Deno.readTextFile("../assets/mistery1.plutus"));
// console.log(misteryScriptJson.cborHex);

const misteryScript: SpendingValidator = {
    type: "PlutusV2",
    script: misteryScriptJson.cborHex
};
const misteryAddress: Address = lucid.utils.validatorToAddress(misteryScript);
console.log("Mistery address:");
console.log(misteryAddress);

// https://cardano.stackexchange.com/questions/11039/lucid-cardano-datum-error
const MisteryDatum = Data.Object({
    beneficiary1: Data.Bytes,
    beneficiary2: Data.Bytes,
    deadline: Data.Integer,
});
type MisteryDatum = Data.Static<typeof MisteryDatum>;

const deadlineDateTxt = await Deno.env.get("DEADLINE_DATETIME");
const deadlineDate: Date = new Date(deadlineDateTxt)
const deadlinePosIx = BigInt(deadlineDate.getTime());

const detailsBen1: AddressDetails = getAddressDetails(addrBeneficiary1);
const beneficiary1PKH: string = detailsBen1.paymentCredential.hash

// Set the mistery beneficiary2 to our own key (to return funds after deadline)
const detailsBen2: AddressDetails = getAddressDetails(addrSpender);
const beneficiary2PKH: string = detailsBen2.paymentCredential.hash

const datum: MisteryDatum = {
    beneficiary1: beneficiary1PKH,
    beneficiary2: beneficiary2PKH,
    deadline: deadlinePosIx,
};

async function sendFunds(amount: bigint): Promise<TxHash> {

    const dtm: Datum = Data.to<MisteryDatum>(datum,MisteryDatum);
    const tx = await lucid
      .newTx()
      .payToContract(misteryAddress, { inline: dtm }, { lovelace: amount })
      .complete();
    const signedTx = await tx.sign().complete();
    const txHash = await signedTx.submit();
    return txHash
}

async function listUTxO(): Promise<TxHash> {
    const dtm: Datum = Data.to<MisteryDatum>(datum,MisteryDatum);
    const utxoAtScript: UTxO[] = await lucid.utxosAt(misteryAddress);
    const ourUTxO: UTxO[] = utxoAtScript.filter((utxo) => utxo.datum == dtm);
    console.log(ourUTxO);
    return ourUTxO;
}

async function claimVestedFunds(): Promise<TxHash> {
    const ourUTxO: UTxO[] = await listUTxO();
    lucid.selectWalletFromSeed(walletSeedBeneficiary1);
    if (ourUTxO && ourUTxO.length > 0) {
        const tx = await lucid
            .newTx()
            .collectFrom(ourUTxO, Data.void())
            .addSignerKey(beneficiary1PKH)
            .attachSpendingValidator(misteryScript)
            .validFrom(Date.now()-100000)
            .validTo(Date.now()+1000000)
            .complete();
        const signedTx = await tx.sign().complete();
        const txHash = await signedTx.submit();
        console.log("https://preview.cardanoscan.io/transaction/" + txHash);
        return txHash
    }
    else return "No UTxO's found that can be claimed"
}

async function returnUnclaimedVestedFunds(): Promise<TxHash> {
    const ourUTxO: UTxO[] = await listUTxO();
    lucid.selectWalletFromSeed(walletSeedBeneficiary2);
    if (ourUTxO && ourUTxO.length > 0) {
        const tx = await lucid
            .newTx()
            .collectFrom(ourUTxO, Data.void())
            .addSignerKey(beneficiary2PKH)
            .attachSpendingValidator(misteryScript)
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
    console.log("https://preview.cardanoscan.io/transaction/" + await sendFunds(271828182n));
    break;

  case "claim":
    console.log(await claimVestedFunds());
    break;

  case "return-unclaimed":
    console.log(await returnUnclaimedVestedFunds());
    break;

  default:
    console.log(await listUTxO());
    break;

}

