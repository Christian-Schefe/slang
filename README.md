# SLANG

A simple interpreted scripting language inspired by Python and Rust (and written in Rust, too).<br>
I didn't research anything at all about making a programming language, and the code is a bit messy. Some things may be a bit unusual (required semicolons after a function definition, for example).

### Features

-   interpreted language
-   dynamic typing
-   expression-based syntax
-   higher-order functions
-   whitespace-insensitive

### To be implemented

-   [ ] data structures
-   [ ] more built-in functions
-   [ ] list iteration

### Subject to change

-   [ ] change statements to expressions (and modify for loop)
-   [ ] expect brackets around if-else bodies?
-   [ ] change `return` behaviour

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

### Comments

```
let x = 5; # this is a comment
x = # this is an inline comment # 6
```

### Output

```
let x = "hello world";
print(x); # prints 'hello world'
```

### Function Definition

```
let my_function = |a, b| {
    return a + b;
}

let my_closure = |a, b| a + b;
```

### If-Else Clause

```
let sign = if x >= 0 {1} else {-1};
let sign_fn = |x| if x >= 0 {1} else {-1};

let val = if condition {
    x
} else {
    5 * x
};
```

### Algebra

```
let x = 5 + 3 * (-3 + 2) - 8;
print(x) # prints -6
```

### Data Types

```
let x = 5;
let y = "hi";
let z = true;

let w = |a, b| a + b;
print(x, y, z, w); # prints '5 "hi" true ["a", "b"] -> ...'
```
