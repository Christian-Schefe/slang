let i = 0;
print(while true {
    i += 1;
    if i > 10 {
        break i
    } else {
        print(i)
    }
});

let x = -3;
let sign = if x >= 0 {1} else {-1};
let sign_fn = |x| if x >= 0 {1} else {-1};

let val = if false {
    x
} else {
    5 * x
};
print(sign);
print(sign_fn(3) + sign_fn(-4));
print(val);

x = 5 + 3 * (-3 + 2) - 8;
print(x); # prints -6

x = 5;
let y = "hi";
let z = true;

let w = |a, b| a + b;
print(x, y, z, w); # prints '5 hi true ["a", "b"] -> Value(Unit)'
