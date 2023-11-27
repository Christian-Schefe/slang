let i = random(1, 100);

while (true) {
    let j = int(input("guess: ", 5));
    if (j == i) {
        print("Found!");
        i = random(1, 100);
    } else if (j < i) {
        print("Too low!");
    } else {
        print("Too high!");
    };
};
