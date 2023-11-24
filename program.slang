fn is_even(num) {
    while (num >= 2) {
        num = num - 2;
    };
    num == 0
};

let c = 5;
while (c >= 0) {
    print("is even:", c, is_even(c));
    c = c - 1;
};
