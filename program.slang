let board = [[0, 0, 0], [0, 0, 0], [0, 0, 0]];

let cur_player = true;

for (let i = 0; i < 9; i += 1) {
    let pos = int(input("board pos: "));
    let l = board[0];
    l[pos - 1] = if (cur_player) 1 else 2;
    cur_player = !cur_player;
    print(board);
};
