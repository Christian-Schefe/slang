# SLANG

A simple interpreted scripting language inspired by Python and Rust (and written in Rust, too).<br>
It is a piping hot mess of code, I didn't research anything at all about making a programming language.

### Features

-   interpreted language
-   dynamic typing
-   expression-based syntax
-   higher-order functions
-   whitespace-insensitive

### To be implemented

-   CLI (currently, 'program.slang' is executed)
-   I/O
-   comments
-   collections
-   data structures
-   for loop (yes, currently there is no for loop)
-   correct scoping (it is currently questionable at best)

## Syntax

### Variable Definitions

```
let x = 5;
let y = "hello world";
```

### Variable Assignment

```
let x = 5;
x = 6;
```

### Output

```
let x = "hello world";
print(x); // prints 'hello world'
```

### Function Definition

```
fn my_function(a, b) {
    return a + b;
}
```

### If-Else Clause

```
let sign = if (x >= 0) 1 else -1;

let val = if (condition) {
    x
} else {
    5 * x
};
```

### Algebra

```
let x = 5 + 3 * (-3 + 2) - 8;
print(x) // prints -6
```

### Data Types

```
let x = 5;
let y = "hi";
let z = true;

fn func(a, b) ();
let w = func;
print(x, y, z, w); // prints '5 hi true ["a", "b"] -> Value(Unit)'
```

### Scoping (WIP)

**Only** function calls are scoped at the moment, and they are completely isolated from the outside world.<br>
Functions and variables defined outside **cannot** be accessed.

```
let x = 5;
{
    let y = x + 1;
};
print(y); // prints '6'
```

<br>

```
let x = 5;

fn func() {
    x = 3; // RUNTIME ERROR
    let z = x + 1; // RUNTIME ERROR
    let x = 3; // valid, as x has not been defined yet
}
```
