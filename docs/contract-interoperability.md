# Contract interoperability

One of the key feature of yallo is the ability for contract to interact with other contracts easily. We provide a syntax for calling other contracts or deploy new one.

## Calling other contracts

The first way to call another contract is using their interface abstraction as follow; we first get the instance of an IToken \(see _Contract structure_ page\) at a given _tokenContractAddress_. We then point to the _getBalance_ entrypoint passing an address to investigate and the reference to a nat entrypoint from this contract.

```text
let op: operation = IToken.of(tokenContractAddress).getBalance(addrToInvestigate, this.checkBalanceCallback); 
```

The previous code creates a operation with 0mtz inside; if we want to call another contract passing an amount, we do:

```text
let op: operation = Tezos.transfer (IToken.of(tokenContractAddress).getBalance, (addrToInvestigate, this.checkBalanceCallback), 12mtz);
```

We can also get _getBalance_ without using the interface IToken:

```text
let getBalance: (address, nat contract) contract = Tezos.contract(tokenContractAddress, "getBalance") in
let op = getBalance(addrToInvestigate, this.checkBalanceCallback);
```

