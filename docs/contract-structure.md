# Contract structure

A contract is composed by three blocks:

* field declarations
* constructor \(optional\)
* entries and views

```csharp
contract AContract {
    field a: nat;
    field b: string;
    
    constructor (z: nat) {
        this.a = z;
        this.b = "string";
    }
    
    entry anEntry(v: nat) {
        this.a = v;
        []
    }
    
    view aView(): nat {
        this.a
    }
}
```

### Fields

A field represents a named value in the contract storage:

```text
field fieldName: fieldType;
```

### Constructor

A constructor is an optional block, which allows to build an initial storage value for the contract; if defined, all storage field should be assigned in the body. The constructor could receive parameters, used to compute the initial storage: this is useful when a contract want to instantiate another contract using _createContract_.

```csharp
constructor (aParam: atype) {
    this.field1 = aParam;
    this.field2 = (aParam, "aString);
}
```

### Entries and Views

Entries and views defined the callable functionalities of the smart contract; they have a name and they could receive parameters. 

An entry body is essentially an expression which always evalutes to an _operation list_. Entry body expression can modify the contract storage using particular expression with side effects \(assignments and type specific modifiers\).

```csharp
entry anEntry (p: nat) {
    this.natField = p * 2n;
    []
}
```

Views instead, are syntatic sugar for the callback pattern and they're translated to normal entries in the interal AST. Views have a custom return type

```csharp
view aView (p: nat): nat {
    p * 2n;
}
```

Which is equivalent to:

```csharp
entry aView (p: nat, cb: nat callback) {
    [ cb (p * 2n) ]
}
```



