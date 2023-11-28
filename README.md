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

-   [ ] CLI (currently, 'program.slang' is executed)
-   [ ] Assignment Operators (+=, -=, etc.)
-   [ ] I/O
-   [ ] collections
-   [ ] data structures

### TODO

-   [ ] test scoping
-   [ ] change statements to expressions (and modify for loop)
-   [ ] expect brackets around if-else bodies?
-   [ ] change `return` behaviour and implement empty returns without `()`;

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
print(x) # prints -6
```

### Data Types

```
let x = 5;
let y = "hi";
let z = true;

fn func(a, b) ();
let w = func;
print(x, y, z, w); # prints '5 hi true ["a", "b"] -> Value(Unit)'
```

### Loops (WIP)

Neither `break` nor `continue` are implemented.

```
let count = 0;
while (count < 5) {
    print(count);
    count = count + 1;
};

for (let i = 0; i < 5; i = i + 1;) {
    print(i);
};
```

### Scoping (WIP)

May not work as expected?

```
let x = 5;
{
    let y = x + 1;
};
print(y); # prints '6'
```

<br>

```
let x = 5;

fn func() {
    x = 3; # RUNTIME ERROR
    let z = x + 1; # RUNTIME ERROR
    let x = 3; # valid, as x has not been defined yet
}
```

### Lists

```
let list = [5, 3, 2, "hello", "world"];

# nested lists, expressions, etc.
let list2 = [1, 2, 3 + 4, (5 + 3), [-3, list], { let y = 5; y }];

print(list); # prints [5, 3, 2, "hello", "world"]
print(list2); # prints [1, 2, 7, 8, [-3, [5, 3, 2, "hello", "world"]], 5]

# accessing elements
let el = list[3];
print(el, list[4]); # prints 'hello world';

# complex expressions inside index
let el2 = list[(3 - 1)];
let el3 = list[{ let i = 4; i }];

print(el2, el3); # prints '2 world'
```
