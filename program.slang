fn modify(ob, x) {
    ob += x;
    ob
};

let b = [1, 2];

print(b);

modify(b, 5);
print(b);
b = modify(b, 5);

print(b);
