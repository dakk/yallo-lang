# Expressions

Expression blocks are semicolon separated list of expression. The last expression define the type of the whole expression. 

```text
e1; e2
```

The semicolon let the developer to ignore the result of an expression, but it is only possible for unit expression; the following expression will fail to compile:

```text
12 + 15; // ignored int expression
13
```

Depending of where you declare an expression, the scope will change; for instance inside an entrypoint expression you can access the contract fields.

You can bind a name to an expression using _let_ syntax:

```text
let a = 12;
a
```

Which can be written also as:

```text
let a = 12 in a
```

Let is also useful for tuple destructuring:

```text
const a: (int, nat) = (12, 13n);
const b: int = let (x, y) = a in x;
```

