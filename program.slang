

let bc = |_| {
    let a = 5;
    let ac = |_| {
        print(a)
    };
    ac
};

let a = 3;
bc()();
