let lines = read("input.txt").lines();
let digits = lines.map(list).map(|el| el.filter(|c| c >= "1" && c <= "9").map(int));
let first_and_last = digits.map(|d| [d[0], d[d.len() - 1]]);
let values = first_and_last.map(|d| 10 * d[0] + d[1]);
let sum = 0;
for num in values {
    sum += num;
};
print(sum);

let ends_with = |s, s2| {
    let i = s.len() - 1;
    let sub_s = "";
    while i >= 0 {
        sub_s = s[i] + sub_s;
        if sub_s == s2 {
            return true;
        };
        if sub_s.len() > s2.len() {
            return false;
        };
        i -= 1;
    };
    false
};

let get_first_digit = |ends_with, s| {
    let i = 0;
    let sub_s = "";
    while i < s.len() {
        sub_s = sub_s + s[i];
        if ends_with(sub_s, "one") {
            return 1;
        };
        if ends_with(sub_s, "two") {
            return 2;
        };
        if ends_with(sub_s, "three") {
            return 3;
        };
        if ends_with(sub_s, "four") {
            return 4;
        };
        if ends_with(sub_s, "five") {
            return 5;
        };
        if ends_with(sub_s, "six") {
            return 6;
        };
        if ends_with(sub_s, "seven") {
            return 7;
        };
        if ends_with(sub_s, "eight") {
            return 8;
        };
        if ends_with(sub_s, "nine") {
            return 9;
        };
        i += 1;
    };
};

let mapper = |line, a, b| {
    b(a, line)
};

print(lines.map(mapper))
