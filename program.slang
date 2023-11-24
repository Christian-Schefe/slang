let x = 0;
let y = 1;
let z = 0;
let count = 100;

while (count > 0) {
    z = x + y;
    x = y;
    y = z;
    print(x);

    count = count - 1;
};
