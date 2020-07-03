# Composed Types

## Option

Options represents a value that could be defined _Some\(value\)_ or not _None_.

```text
const a: nat option = None;
const b: nat option = Some(12n);
```

The option type offeres some helpers:

```text
const c: bool = isNone(a); // returns true if a is None
const d: bool = isSome(a); // returns true if a is Some(_)
const e: nat = getSome(b): // extract the value (if any) or fail
```

## List

## Set

## Tuple

## Record

## Map & Big\_map

## Lambda

