# Yallo-lang
Yallo is a yet another high level experimental language for Tezos, with the purpose of 
providing a better abstraction for integrating different contracts.

This is only a research project, is not (yet) intendeed for real usage.


## OOlike abstraction example

We first define an interface describing the signature of a token contract

```java
interface IToken {
	entry transfer(from: address, to: address, val: nat);
	entry getBalance(ad: address, cb: nat callback);
}
```

Now we can implement our token contract; the constructor is only an helper for building an initial
storage value during compilation. 

```java
contract Token implements IToken {
	field balances: (address, nat) big_map;
	field totalSupply: nat;
	field symbol: string;

	constructor (supply: nat, symbol: string) {
		this.balances = empty;
		this.totalSupply = supply;
		this.symbol = symbol;
	}

	entry transfer(from: address, to: address, val: nat) {
		...
	}

	entry getBalance(ad: address, cb: nat callback) {
		...
		var op: operation = cb(balance);
		return [op];
	}
}
```

From another contract, if we want to use the token getBalance entry we'll do as follow:

```java
#import "IToken.yallo";

const tokenContractAddress: address = "...";

contract usingAToken {
	...

	entry checkBalance(a: address) {
		return [IToken.of(tokenContractAddress).getBalance(a, this.checkBalanceCallback)];
	}

	entry checkBalanceCallback(b: nat) {

	}
}
```

Or, if we want to deploy a toke contract from another contract:

```java
#import "Token.yallo";

contract usingAToken {
	...

	entry deployToken() {
		var (a, op) = Tezos.createContract (Token(100, "ourToken"), None, 0);
		return [op];
	}
}
```
