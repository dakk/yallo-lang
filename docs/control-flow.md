# Control flow

## If then else

```text
if (a = 12n) then (15) else (12)
```

## Pattern matching

```ocaml
match a with | 12 -> true | _ -> false
```

```ocaml
type a = enum (Hello | World | No);
const b = match a#Hello with | a#Hello -> true | a#World -> false;
const c = match 2 with | 1 -> a#Hello | 2 -> a#World | _ -> a#No;

const as = Some(12);
const d = match as.isSome() with | true -> as.getSome() | false -> 13;
```

