# Composed Types

## Option

Options represents a value that could be defined _Some\(value\)_ or not _None_.

```csharp
const a: nat option = None;
const b: nat option = Some(12n);
```

The option type offers some helpers:

```csharp
const c: bool = isNone(a); // returns true if a is None
const d: bool = isSome(a); // returns true if a is Some(_)
const e: nat = getSome(b): // extract the value (if any) or fail
```

## List

List type represents list of same-type elements; a list can be declared as follow:

```csharp
let l: nat list = (List.empty(): nat list); // empty list
let l1: nat list = [12n, 13n]; // literal list
```

```csharp
let ls: nat = l.size();
```

```csharp
let lh: nat = l.head();
```

```csharp
let lt: nat list = l.tail();
```

```csharp
let l2: nat list = l.prepend(14n);
```

```csharp
let l3: nat list = l.mapWith((a: nat) => (a * 2n));
```

```csharp
let l4: nat list = l.filter((a: nat) => (a <= 13n));
```

{% hint style="info" %}
_prepend_, _mapWith_ and _filter_ are applied in-place if used over storage fields. Their types are _unit_ instead of _nat list._ To apply as expression, surround the storage field with _copy\(\)_.
{% endhint %}

## Set

Sets are list of different elements of the same-type.

```csharp
let s: nat set = (Set.empty(): nat set); // empty set
let s1: nat set = ([12n, 13n]: nat set); // literal set
```

```csharp
let ss: nat = s.size();
```

```csharp
let b: bool = s.mem(13n);
```

```csharp
let s2: nat set = s1.update(18n, true); // add
let s2: nat set = s2.update(18n, false); // remove
```

```csharp
let ssum: nat = s2.fold((acc: nat, l: nat) => (acc + l), 0n);
```

{% hint style="info" %}
_update_ are applied in-place if used over storage fields. Their types are _unit_ instead of _nat set._ To apply as expression, surround the storage field with _copy\(\)_.
{% endhint %}

## Tuple

A tuple is a pair, triple, etc of different types; you can define a tuple as follow:

```csharp
let atuple: (int, string) = (12, "hello");
```

## Record

## Map & Big\_map

\('a, 'b\) Map and \('a, 'b\) Big\_map are collections of _'b_ indexed by a key _'a_; _'a_ should be a comparable.

{% hint style="info" %}
_update, remove, mapWith, fold, filter_ are applied in-place if used over storage fields. Their types are _unit_ instead of _nat set._ To apply as expression, surround the storage field with _copy\(\)_.
{% endhint %}

### Map

```csharp
let m: (string, nat) map = (Map.empty(): (string, nat) map);
```

```csharp
let m: (string, nat) map = [ { "Hello": 12 } ];
```

```csharp
let a: nat = m.size();
```

```csharp
let b: nat = m.get("Ciao", 0); // failsafe get
let b1: nat = m.get("Ciao"); // failable get
let c: nat option = m.getOpt("Ciao");
```

```csharp
let b: bool = m.mem("Ciao");
```

```csharp
let c: nat = m.fold((acc: nat, k: string, v: nat) => (acc+v), 0n);
```

```csharp
let m2: (string, nat) map = m.mapWith((k: string, v: nat) => (v * 2n)); 
```

```csharp
let m2: (string, nat) map = m.filter((k: string, v: nat) => (v > 2n)); 
```

```csharp
let m2: (string, nat) map = m.remove("Hello"); 
```

```csharp
let m2: (string, nat) map = m.update("Hello", 42n); 
```

### Big\_map

```csharp
let bm: (string, nat) big_map = (BigMap.empty(): (string, nat) big_map);
```

```csharp
let b: nat = bm.get("Ciao", 0); // failsafe get
let b1: nat = bm.get("Ciao"); // failable get
let c: nat option = bm.getOpt("Ciao");
```

```csharp
let b: bool = bm.mem("Ciao");
```

```csharp
let bm2: (string, nat) big_map = m.remove("Hello"); 
```

```csharp
let bm2: (string, nat) big_map = bm.update("Hello", 42n); 
```

## Lambda

Lambda type represent anonymous functions.

```csharp
const e_sum: int -> int = (a: int) => (0 + 1);
const e_gt: int -> bool = (a: int) => (0 > 1);
const e_gt2: int -> bool = (a: int) => (a > 1);
const e_comp: (int, int) -> bool = (a: int, b: int) => ((a * 8) > (b - 12));
const e_comp2: (int, int) -> bool = (a: int, b: int) => ((a * (b - 8)) > (b - 12));
const e_apply: int -> bool = (a: int) => (e_comp (12, 13));

const e_apply2: int -> int = (a: int) => (((b: int) => (b))(12));
const e_apply3: int -> int = (a: int) => (e_apply2 (12));
```

## In-place modification

While calling some of the composed type specific functions, if the left-hand is storage field, the modification will be automatically assigned to the field storage.

```csharp
this.flist.prepend (12n); // this has type unit, an modify flist prepending 12n
```

To avoid this behaviour, you can use the _copy_ helper function.

```csharp
let a: nat list = copy(this.flist).prepend(12n);
```

