let fibonacci = |n| {
    let loop = |i, a, b| {
        a if i == 0 else self(i - 1, b, a + b)
    };
    loop(n, 0, 1)
};


let x = fibonacci(30);
print(x);
