let board = [[0, 0, 0], [0, 0, 0], [0, 0, 0]];

let cur_player = true;

fn mod(x, m) {
    while (x >= m) {
        x -= m;
    };
    x
};

fn div(x, m) {
    let c = 0;
    while (x >= m) {
        x -= m;
        c += 1;
    };
    c
};

for (let i = 0; i < 9; i += 1) {
    let pos = int(input("board pos: "));
    let board_i = div(pos - 1, 3);
    let l = board[board_i];
    l[mod(pos - 1, 3)] = if (cur_player) 1 else 2;
    board[board_i] = l;
    cur_player = !cur_player;
    print(board);
};
