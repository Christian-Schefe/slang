let list = [5, 3, 2, "hello", "world"];

# nested lists, expressions, etc.
let list2 = [1, 2, 3 + 4, (5 + 3), [-3, list], { let y = 5; y }];

print(list); # prints [5, 3, 2, "hello", "world"]
print(list2); # prints [1, 2, 7, 8, [-3, [5, 3, 2, "hello", "world"]], 5]

# accessing elements
let el = list[3];
print(el, list[4]); # prints 'hello world';

# complex expressions inside index
let el2 = list[(3 - 1)];
let el3 = list[{ let i = 4; i }];

print(el2, el3); # prints '2 world'
