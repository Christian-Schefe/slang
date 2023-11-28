let board = [["_", "_", "_"], ["_", "_", "_"], ["_", "_", "_"]];

let cur_player = true;

fn get(x, y) {
    let row = board[y];
    row[x]
};

fn set(x, y, val) {
    if (x >= 0 && x < 3 && y >= 0 && y < 3) {
        let row = board[y];
        if (row[x] == "_") {
            row[x] = val;
            board[y] = row;
            
            true
        } else {
            false
        }
    } else {
        false
    }
};

fn check_dir_for_win(x, y, dx, dy) {
    let v = get(x, y);
    v != "_" && v == get(x + dx, y + dy) && v == get(x + dx * 2, y + dy * 2)
};

fn print_board() {
    for (let y = 0; y < 3; y += 1) {
        print(board[y]);
    }
};

fn check_for_win() {
    check_dir_for_win(0, 0, 1, 0) ||
    check_dir_for_win(0, 1, 1, 0) ||
    check_dir_for_win(0, 2, 1, 0) ||

    check_dir_for_win(0, 0, 0, 1) ||
    check_dir_for_win(1, 0, 0, 1) ||
    check_dir_for_win(2, 0, 0, 1) ||

    check_dir_for_win(0, 0, 1, 1) ||
    check_dir_for_win(0, 2, 1, -1)
};

let is_playing = true;

while (is_playing) {
    let pos = int(input("board pos: "));
    let x = (pos - 1) % 3;
    let y = (pos - 1) / 3;
    
    if (set(x, y, if (cur_player) "x" else "o")) {
        print_board();

        if (check_for_win()) {
            print("player", if (cur_player) "'x'" else "'o'", "won!");
            is_playing = false;
        };
        cur_player = !cur_player;
    };
};
