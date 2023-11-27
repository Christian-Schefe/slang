fn a(x) {
    print(x);
    fn c() {
        print(x);
    };
    c();
};

let val = "hi";

fn b() {
    a(val);
    val = 5;
    let val = 7;
    a(val);
    fn x() {
        val = true;
        a(val);
    };
    x();
};

b();
b();
