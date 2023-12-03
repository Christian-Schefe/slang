
let tailfib = |n| {
    let loop = |loop, i, a, b| {
        a if i == 0 else loop(loop, i - 1, b, a + b)
    };
    loop(loop, n, 0, 1)
};


let x = tailfib(30);