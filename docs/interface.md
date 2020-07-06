# Interface

An interface describes entrypoints of a contract.

```csharp
interface Token {
    entry transfer (fr: address, to: address, am: nat);
    view getBalance(a: address): nat;
}
```

Yallo implements a single inheritance mechanism for interfaces, so interface can be extended:

```csharp
interface MintableToken extends Token {
    entry mint ();
}
```

A contract can implement an interface \(only one\); if implements an interface, it should implements all the entries with the given signatures.

```csharp
contract AToken implements Token {
    field balances: (addres, nat) map;
    
    view getBalance (a: address): nat {
        this.balances.get(a, 0n)
    }
    
    entry transfer (to: address, am: nat) {
        ...
    }
}
```

