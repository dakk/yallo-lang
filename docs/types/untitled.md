# Base Types

## Numerical \(nat, int, timestamp, mutez\)

### Nat

Nat type represents natural numbers \(positive integers U { 0 }\); a nat literal should be always followed by an _n._

```c
const a: nat = 12n;
```

A nat value can be converted to _int_ type using the _int\(nat\)_ builtin function:

```c
const b: int = int(12n);
```

### Int

Int type represents integer numbers; we define an _int_:

```c
const a: int = 12;
```

We can check if the _int_ is a _nat_:

```c
const b1: bool = isNat(12);  // true
const b2: bool = isNat(-12); // false
```

Transform an _int_ to a _nat_:

```c
const c: nat = abs(-12);
```

And negate a _nat_ or _int_ \(the result type is always an int\):

```c
const d1: int = neg(12n);   // = -12
const d2: int = neg(-12);   // = 12
```

### Mutez

Mutez type represents a tez amount. A mutez literal si preceeded by mtz or tz; 1000000mtz = 1tz.

```c
const a: mutez = 1tz;
const b: mutez = 1mtz;
```

### Timestamp

Timestamp type represent an unix timestamp \(seconds since Jan 01 1970\).

```c
const a: timestamp = 1593765393;
```

We also have an helper called _Timestamp.duration_ which produces an int value for a duration. 

```c
const a: int = Timestamp.duration (45, "seconds");
const b: int = Timestamp.duration (45, "minutes");
const c: int = Timestamp.duration (45, "hours");
const d: int = Timestamp.duration (45, "days");
const e: int = Timestamp.duration (45, "weeks");
const f: int = Timestamp.duration (45, "years");
```

We can get the current _timestamp_ using the _now_ function:

```c
const a: timestamp = Timestamp.now();
const b: timestamp = a + Timestamp.duration(1, "days");
```

## Bool

Bool type represents a boolean, which has two possible values: _true_ and _false._

```cpp
const aTrue: bool = true;
const aFalse: bool = false;
```

## Enum

Enum type are unit variants; low level are represented as nat, so they are comparable \(only for equality\).

```cpp
type anEnum = enum (Started | Stopped);

let av: anEnum = anEnum#Started;
let b: bool = av = anEnum#Stopped; // false
```

## String

String are sequences of characters.

```cpp
let a: string = "Hello World";
```

We can get the length of a string with _size:_

```cpp
let b: nat = a.size();
```

And get a _slice_:

```cpp
let c: string = a.slice(1, 5);
```

## Bytes

Bytes are sequences of bytes. Like strings you can get the _length_ and a _slice_. 

```cpp
let a: bytes = "...";
let b: nat = a.size();
let c: bytes = a.slice(1, 5);
```

Bytes type is useful for encoding/decoding Michelson data using _pack_ and _unpack_:

```cpp
let n: nat = 12n;
let a: bytes = Bytes.pack (n); // pack a nat
let b: nat option = (Bytes.unpack (a): nat option);

let c: bool = n = (Option.getSome(b));
```

## Unit

The unit type is a type which has only a value _Unit._

```cpp
const a: unit = Unit;
```



