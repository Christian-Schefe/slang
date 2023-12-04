let list = [2, 3];
let x = list[1];
let y = |a| a;
let as = true || false;
list[1] = 2;
let z = y(5);
let obj = {
    a: 5,
    c: x + z,
    d: |a, b| a || b,
    rec: |a| a.rec
};

let get = |c| [2, 2, 1] if c else [1, 2, 7];

obj.a = get(false, true);
obj.a[2] = 0;
get(true)[0];
let res = (-3 + 5 == 2) == false;
let f = obj.rec(obj);
print(x);