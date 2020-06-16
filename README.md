# Yallo-lang
Yallo is a yet another high level experimental language for Tezos, with the purpose of 
providing a better abstraction for integrating different contracts.

Internally, the language is a functional language with side effects (storage assignments).

This is only a research project, it is not (yet) intendeed for real usage.


## Usage

```bash
./_build/default/src/yallo.exe compile contract_file.yallo -out-lang ligo -contract TestContract
```


## OOlike abstraction example

We first define an interface describing the signature of a token contract, and another interface extending
IToken with a getTotalSupply.

```java
interface IToken {
	entry transfer(from: address, to: address, val: nat);
	entry getBalance(ad: address, cb: nat contract);
}

interface ITokenWithGetTotalSupply extends IToken {
	entry getTotalSupply (cb: nat contract);
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
		let a: nat = this.balances.get(from);
		let b: nat = this.balances.get(to);
		assert (a > val);
		this.balances.update(from, a - val);
		this.balances.update(to, b + val); 
		[]
	}

	entry getBalance(ad: address, cb: nat contract) {
		let balance: nat = this.balances.get(ad);
		let op: operation = cb(balance);
		[op]
	}
}
```

From another contract, if we want to use the token getBalance entry we'll do as follow:

```java
import "IToken.yallo";

const tokenContractAddress: address = @KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq;

contract usingAToken {
	field bal: nat;

	entry checkBalance(a: address) {
		[IToken.of(tokenContractAddress).getBalance(a, this.checkBalanceCallback)]
	}

	entry checkBalanceCallback(b: nat) {
		this.bal = b;
		[]
	}
}
```


Or, if we want to deploy a token contract from another contract:

```java
import "Token.yallo";

contract deployAToken {
	field tokenAddress: address;

	entry deployToken() {
		let (a: address, op: operation) = Tezos.createContract (Token(100, "ourToken"), None, 0);
		this.tokenAddress = a;
		[op]
	}
}
```

## License

```
Copyright (c) 2020 Davide Gessa

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
```