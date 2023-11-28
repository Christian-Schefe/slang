let i = random(1, 100);

let continue_loop = true;
while (continue_loop) {
    let j = int(input("guess: ", 5));
    if (j == i) {
        print("Found!");
        continue_loop = false;
    } else if (j < i) {
        print("Too low!");
    } else {
        print("Too high!");
    };
};
