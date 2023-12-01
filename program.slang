let var = {val1: ["where are you", "hello world"]};

fn exec(var, func) {
    func(var)
};

fn map(var, func) {
    let var_len = len(var);
    let result = [];
    for (let i = 0; i < var_len; i += 1) {
        result += [func(var[i])];
    };
    result
};

let b = map(var.val1,  { fn mod(el) exec(" ", el.split); mod });

print(b);
