
let tailfib = |n| {
    let loop = |i, a, b| {
        a if i == 0 else self(i - 1, b, a + b)
    };
    loop(n, 0, 1)
};


let x = tailfib(30);
print(x, 30);
5.print();

let obj = {
    a: 3,
    c: 6 * 3
};
obj.print();
(|a| a.print())(5);
