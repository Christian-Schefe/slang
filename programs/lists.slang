let list = [5, 3, 2, "hello", "world"];

# nested lists, expressions, etc.
let list2 = [1, 2, 3 + 4, (5 + 3), [-3, list], { let y = 5; y }];

print(list); # prints [5, 3, 2, "hello", "world"]
print(list2); # prints [1, 2, 7, 8, [-3, [5, 3, 2, "hello", "world"]], 5]
