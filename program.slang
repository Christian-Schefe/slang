let a = "hi";

fn modify_arr(arr) {
    a += a;
};

for (let i = 0; i < 5; i += 1) modify_arr(a);

print(a);
