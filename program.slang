let bc = |_| {
    let a = 5;
    let ac = |_| {
        let y = a;
    };
    ac
};

let a = 3;
bc()();

for i in range(3, 100000) {
    bc()();
};
print("4")
