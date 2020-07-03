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

## String & Bytes

## Unit

The unit type is a type which has only a value _Unit._

```cpp
const a: unit = Unit;
```



