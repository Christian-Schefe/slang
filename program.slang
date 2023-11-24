fn executor(func, arg1, arg2) {
    let x = 0;
    if (x == 0) {
        func(arg1, arg2);
    }
};

fn print_x(x, x2) {
    print(x, x2);
};

executor(print_x, executor, 0);
