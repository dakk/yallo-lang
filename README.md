# Yallo-lang
Yallo is a yet another high level experimental language for Tezos, with the purpose of 
providing a better abstraction for integrating different contracts.

This is only a research project, it is not (yet) intendeed for real usage.


## Progress
- [x] Grammar
- [x] Parser
- [ ] AST
- [ ] Typecheck
- [ ] Output


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

Or, if we want to deploy a token contract from another contract:

```java
#import "Token.yallo";

contract usingAToken {
	...

	entry deployToken() {
		var (a: address, op: operation) = Tezos.createContract (Token(100, "ourToken"), None, 0);
		return [op];
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