let board = [[0, {a: -2}], [2, [3, 4]]];
let ob = {a: -2};
ob.a = board;

let two = board[1][1][0];
print(two);
print(ob);

ob.a[1][1] = 4;
print(board[0][1]);
board[0][1].a = board;
let val = board[0][1].a[1][1];

print(ob);
print(board);
print(val);
