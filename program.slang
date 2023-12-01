let var = {val1: ["where are you", "hello world"] + ["where are you", "hello world"]};

fn exec(var, func) func(var);

let b = var.val1.map({ fn mod(el) exec(" ", el.split); mod });
var.val1.map(print);
let c = var.val1.filter({ fn mod(el) el.len() >= 11; mod });

print(b);
print(c);
