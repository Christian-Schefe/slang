let x = 0;
# iterative
let y = 1;
let z = 0;

let amount = 10;
while (amount > 0) {
    amount = amount - 1;
    z = x + y;
    x = y;
    y = z;
};
print("iterative:", x);

# recursive
fn fibonacci(rec, num) {
    if (num == 0) 0
    else if (num == 1) 1
    else (rec(rec, num - 1) + rec(rec, num - 2))
};

print("recursive:", fibonacci(fibonacci, 10));
