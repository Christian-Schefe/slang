let x = 5;
let y = 7;
fn func(a, b) {
    return a + b * 3;
};

fn func2(a, b) {
    return a + b ;
};

print(x);
print(y);
let result = func(func2(0 - 2, 5), y);

print(result);
