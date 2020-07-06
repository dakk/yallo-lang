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

{% hint style="info" %}
_update_ are applied in-place if used over storage fields. Their types are _unit_ instead of _nat set._ To apply as expression, surround the storage field with _copy\(\)_.
{% endhint %}

## Tuple

## Record

## Map & Big\_map

## Lambda

