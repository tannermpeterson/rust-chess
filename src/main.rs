use board::*;
use gloo_console::log;
use leptos::html::Div;
use leptos::*;
use leptos_use::core::*;
use leptos_use::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::io;
use std::rc::Rc;
use std::str::FromStr;

fn main() {
    let user_color: Color = if true /* rand::random() */ {
        Color::White
    } else {
        Color::Black
    };

    let use_leptos = true;
    if use_leptos {
        let board = Board::new(&promote_leptos);
        run_game_leptos(user_color, board);
    } else {
        let board = Board::new(&promote_text_based);
        run_game_text_based(user_color, board);
    }
}

fn run_game_text_based(user_color: Color, mut board: Board) {
    loop {
        println!("{}", board.to_string(user_color));

        println!("Enter Move:");

        let mut move_string = String::new();

        io::stdin()
            .read_line(&mut move_string)
            .expect("Failed to read line");

        let move_ = match parse_move(move_string, user_color) {
            Ok(move_) => move_,
            Err(e) => {
                println!("{e}");
                continue;
            }
        };

        let res = board.handle_move(move_, user_color);
        if res.is_err() {
            continue;
        }
    }
}

fn promote_text_based() -> Pieces {
    loop {
        let mut promotion = String::new();

        println!("Promote to:");

        io::stdin()
            .read_line(&mut promotion)
            .expect("Failed to read line");

        let promotion_chars: Vec<char> = promotion.trim().chars().collect();

        if promotion_chars.len() != 1 {
            println!("{}", MoveErrors::InvalidPromotion);
            continue;
        }

        match Pieces::from_str(&promotion[..1]) {
            Ok(piece) => match piece {
                Pieces::K | Pieces::P => {
                    println!("{}", MoveErrors::InvalidPromotion)
                }
                _ => {
                    return piece;
                }
            },
            Err(_) => println!("{}", MoveErrors::InvalidPromotion),
        };
    }
}

fn run_game_leptos(user_color: Color, mut board: Board) {
    mount_to_body(move || view! {<Display board=board user_color=user_color/>})
}

fn promote_leptos() -> Pieces {
    // TODO
    Pieces::Q
}

struct MoveTracker {
    piece_info: PieceInfo,
    position: Position,
    perspective: Color,
    update_fn: Option<WriteSignal<Position>>,
    moves: Option<HashMap<BoardIdxs, String>>,
}

impl MoveTracker {
    pub fn new(piece_info: PieceInfo, position: Position, perspective: Color) -> MoveTracker {
        MoveTracker {
            piece_info,
            position,
            perspective,
            update_fn: None,
            moves: None,
        }
    }

    pub fn set_update_fn(&mut self, update_fn: WriteSignal<Position>) {
        self.update_fn = Some(update_fn);
    }

    pub fn update_position(&mut self, new_position: Position) {
        self.position = new_position;
        self.set_position()
    }

    pub fn set_position(&mut self) {
        if let Some(update_fn) = &mut self.update_fn {
            update_fn(self.position);
        }
    }

    pub fn set_legal_moves(&mut self, moves: HashMap<BoardIdxs, String>) {
        self.moves = Some(moves);
    }

    pub fn set_perspective(&mut self, perspective: Color) {
        if perspective != self.perspective {
            self.update_position(flip_position(self.position));
        }
        self.perspective = perspective;
    }
}

#[component]
fn Display(board: Board, user_color: Color) -> impl IntoView {
    let (perspective, set_perspective) = create_signal(user_color);
    view! {
        <BoardView board=board perspective=perspective/>
        <button on:click=move |_| set_perspective(perspective().other())>"Flip"</button>
    }
}

#[component]
fn BoardView(board: Board, perspective: ReadSignal<Color>) -> impl IntoView {
    let board = Rc::new(RefCell::new(board));

    let mut board_view = Vec::new();
    let mut piece_view = Vec::new();

    let mut rank = BOARD_LEN - 1;
    let starting_file = 0;

    while rank < BOARD_LEN {
        let mut file_ = starting_file;

        let mut row_view = Vec::new();
        while file_ < BOARD_LEN {
            /* Create square */
            let square_color = if (rank + file_) % 2 == 0 {
                "#7f682f"
            } else {
                "#d6c291"
            };
            let th_style = format!("background: {}; height: 50px; width: 50px", square_color);
            row_view.push(view! { <th style=th_style/> });

            // create piece if one starts on this square
            let square_occupant = &board.borrow().squares[rank][file_];
            if square_occupant.is_none() {
                file_ = file_.wrapping_add_signed(1);
                continue;
            }
            let piece_info = square_occupant.unwrap();
            let piece_str = match piece_info.piece {
                Pieces::K => "♚",
                Pieces::Q => "♛",
                Pieces::R => "♜",
                Pieces::B => "♝",
                Pieces::N => "♞",
                Pieces::P => "♟",
            };
            let text_color = if piece_info.color == Color::White {
                "#FFF"
            } else {
                "#000"
            };
            let piece_div_style = format!("color: {}; font-size: 60px; ", text_color);

            let mt_og = Rc::new(RefCell::new(MoveTracker::new(
                piece_info,
                get_position_from_board_idxs(BoardIdxs(rank, file_), perspective.get_untracked()),
                perspective.get_untracked(),
            )));
            let mt_clone_on_start = Rc::clone(&mt_og);
            let mt_clone_on_end = Rc::clone(&mt_og);

            let board_clone_on_start = Rc::clone(&board);
            let board_clone_on_end = Rc::clone(&board);

            let el = create_node_ref::<Div>();
            // `style` is a helper string "left: {x}px; top: {y}px;"
            let UseDraggableReturn {
                set_position,
                style,
                ..
            } = use_draggable_with_options(
                el,
                UseDraggableOptions::default()
                    .initial_value(mt_og.borrow().position)
                    .on_start(move |UseDraggableCallbackArgs { position, event }| {
                        let mut mt = mt_clone_on_start.borrow_mut();
                        let board_clone = board_clone_on_start.borrow();
                        log!(format!("{}, {}", mt.position.x, mt.position.y));
                        let board_idxs = get_board_idxs_from_position(mt.position, mt.perspective);
                        let BoardIdxs(rank_idx, file_idx) = board_idxs;
                        log!(format!("ON START, {}, {}", rank_idx, file_idx));

                        if board_clone.squares[rank_idx][file_idx].is_some() {
                            let legal_moves = board_clone.get_moves_for_piece_at(board_idxs);
                            log!(format!("moves: {:?}", legal_moves));
                            mt.set_legal_moves(legal_moves);
                            true
                        } else {
                            false
                        }
                    })
                    .on_end(move |UseDraggableCallbackArgs { position, event }| {
                        let mut mt = mt_clone_on_end.borrow_mut();

                        let board_idxs = get_board_idxs_from_position(position, mt.perspective);
                        log!(format!(
                            "({}, {}), ({}, {})",
                            position.x, position.y, board_idxs.0, board_idxs.1
                        ));

                        if let Some(moves) = mt.moves.as_ref() {
                            if let Some(move_string) = moves.get(&board_idxs).clone() {
                                log!(move_string.clone());

                                let piece_color = mt.piece_info.color;

                                let move_ = match parse_move(move_string.to_owned(), piece_color) {
                                    Ok(move_) => move_,
                                    Err(e) => {
                                        log!(format!("{}", e));
                                        mt.set_position();
                                        return;
                                    }
                                };
                                let res = board_clone_on_end
                                    .borrow_mut()
                                    .handle_move(move_, piece_color);
                                if let Err(e) = res {
                                    log!(format!("{}", e));
                                    mt.set_position();
                                    return;
                                }
                                let new_pos =
                                    get_position_from_board_idxs(board_idxs, mt.perspective);
                                mt.update_position(new_pos);
                            } else {
                                log!(format!("Move not found"));
                                mt.set_position();
                            }
                        } else {
                            log!(format!("No moves found"));
                            mt.set_position();
                        }
                    }),
            );

            mt_og.borrow_mut().set_update_fn(set_position);

            // TODO move mt_og into a vec

            create_effect(move |_| {
                mt_og.borrow_mut().set_perspective(perspective());
            });

            piece_view.push(view! {
                <div node_ref=el style=move || format!("position: fixed; {}; {}", style.get(), piece_div_style)>
                    {piece_str}
                </div>
            });

            file_ = file_.wrapping_add_signed(1);
        }

        board_view.push(view! { <tr>{row_view}</tr> });
        rank = rank.wrapping_add_signed(-1);
    }

    view! {
        <table>
            <tbody>
                {board_view}
                {piece_view}
            </tbody>
        </table>
    }
}

// TODO these 3 functions should be grouped together in a mod or added the to the MoveTracker
// struct
fn flip_position(position: Position) -> Position {
    Position {
        x: 414.0 - position.x,
        y: 378.0 - position.y,
    }
}

fn get_board_idxs_from_position(mut position: Position, perspective: Color) -> BoardIdxs {
    if perspective == Color::Black {
        position = flip_position(position);
    }
    let file_idx = (position.x as usize + 9) / 54;
    let rank_idx = BOARD_LEN - 1 - ((position.y as usize + 28) / 54);
    BoardIdxs(rank_idx, file_idx)
}

fn get_position_from_board_idxs(board_idxs: BoardIdxs, perspective: Color) -> Position {
    let BoardIdxs(rank_idx, file_idx) = board_idxs;
    let mut position = Position {
        x: 54.0 * (file_idx as f64) + 18.0,
        y: 54.0 * (BOARD_LEN - 1 - rank_idx) as f64,
    };
    if perspective == Color::Black {
        position = flip_position(position);
    }
    position
}

mod board {
    use std::collections::HashMap;
    use std::str::FromStr;

    #[derive(strum_macros::Display, strum_macros::EnumString, Debug, PartialEq, Copy, Clone)]
    pub enum Pieces {
        K,
        Q,
        B,
        N,
        R,
        P,
    }

    #[derive(PartialEq, Eq, Hash, Debug, Copy, Clone)]
    pub enum Color {
        White,
        Black,
    }

    impl Color {
        pub fn other(&self) -> Color {
            if *self == Color::White {
                Color::Black
            } else {
                Color::White
            }
        }
    }

    #[derive(Debug)]
    pub struct Square {
        pub file_: char,
        pub rank: u8,
    }

    #[derive(Debug, Copy, Clone)]
    pub struct PieceInfo {
        pub piece: Pieces,
        pub color: Color,
        pub move_count: u16,
    }

    #[derive(PartialEq, Debug)]
    pub enum CoordComponent {
        File(char),
        Rank(u8),
    }

    #[derive(Debug)]
    pub enum Move {
        Standard(SinglePieceMove),
        Castle(CastleDirection),
    }

    #[derive(Debug)]
    pub struct SinglePieceMove {
        pub piece_info: PieceInfo,
        pub square: Square,
        pub disambiguator: Option<CoordComponent>,
    }

    #[derive(PartialEq, Debug)]
    pub enum CastleDirection {
        Short,
        Long,
    }

    pub struct MoveIdxs {
        pub starting_idxs: BoardIdxs,
        // target and capture idxs can be different due to en passant rule
        pub target_idxs: BoardIdxs,
        pub capture_idxs: Option<BoardIdxs>,
    }

    pub struct MoveRecord {
        move_: Move,
        capture: Option<Pieces>,
        promotion: Option<Pieces>,
        target_idxs: BoardIdxs,
    }

    pub const BOARD_LEN: usize = 8;
    type BoardRank = [Option<PieceInfo>; BOARD_LEN];
    type BoardArr = [BoardRank; BOARD_LEN];
    #[derive(PartialEq, Clone, Eq, Hash, Debug)]
    pub struct BoardIdxs(pub usize, pub usize);

    pub struct Board {
        pub squares: BoardArr,
        pub moves: Vec<MoveRecord>,
        promote: &'static dyn Fn() -> Pieces,
        king_idxs: HashMap<Color, BoardIdxs>,
    }

    impl Board {
        pub fn new(promote: &'static dyn Fn() -> Pieces) -> Board {
            Board {
                // element [0][0] is a1
                squares: [
                    Board::piece_rank(Color::White),
                    Board::pawn_rank(Color::White),
                    Board::empty_rank(),
                    Board::empty_rank(),
                    Board::empty_rank(),
                    Board::empty_rank(),
                    Board::pawn_rank(Color::Black),
                    Board::piece_rank(Color::Black),
                ],
                moves: Vec::new(),
                promote,
                king_idxs: HashMap::new(),
            }
        }

        pub fn handle_move(&mut self, move_: Move, color: Color) -> Result<(), MoveErrors> {
            let mut capture: Option<Pieces> = None;
            let mut new_king_idxs: Option<BoardIdxs> = None;
            let mut promotion: Option<Pieces> = None;
            let mut target_idxs_for_record: Option<BoardIdxs> = None;
            match move_ {
                Move::Standard(ref move_) => {
                    let move_idxs = self.is_legal_move(&move_, color)?;

                    let MoveIdxs {
                        starting_idxs,
                        target_idxs,
                        capture_idxs,
                    } = move_idxs;

                    // The piece being moved should never be None, so ok to unwrap here
                    let mut piece_moved = self.squares[starting_idxs.0][starting_idxs.1].unwrap();
                    piece_moved.move_count += 1;

                    if piece_moved.piece == Pieces::P
                        && (color == Color::White && move_.square.rank == 8)
                        || (color == Color::Black && move_.square.rank == 1)
                    {
                        piece_moved.piece = (self.promote)();
                        promotion = Some(piece_moved.piece);
                    }

                    let mut capture_info = self.squares[target_idxs.0][target_idxs.1];
                    self.squares[target_idxs.0][target_idxs.1] = Some(piece_moved);
                    self.squares[starting_idxs.0][starting_idxs.1] = None;
                    if let Some(capture_idxs) = capture_idxs {
                        self.squares[capture_idxs.0][capture_idxs.1] = None;
                        capture_info = self.squares[capture_idxs.0][capture_idxs.1];
                    }

                    if let Some(piece_info) = capture_info {
                        capture = Some(piece_info.piece);
                    };
                    if piece_moved.piece == Pieces::K {
                        new_king_idxs = Some(target_idxs.clone())
                    };

                    target_idxs_for_record = Some(target_idxs);
                }
                Move::Castle(ref direction) => {
                    let move_idxs_arr = self.is_legal_castle(&direction, color)?;

                    for move_idxs in move_idxs_arr.into_iter() {
                        let MoveIdxs {
                            starting_idxs,
                            target_idxs,
                            capture_idxs: _,
                        } = move_idxs;

                        // The piece being moved should never be None, so ok to unwrap here
                        let mut piece_moved =
                            self.squares[starting_idxs.0][starting_idxs.1].unwrap();
                        piece_moved.move_count += 1;
                        self.squares[target_idxs.0][target_idxs.1] = Some(piece_moved);
                        self.squares[starting_idxs.0][starting_idxs.1] = None;
                        if piece_moved.piece == Pieces::K {
                            new_king_idxs = Some(target_idxs.clone());
                            target_idxs_for_record = Some(target_idxs);
                        };
                    }
                }
            };

            self.moves.push(MoveRecord {
                move_,
                capture,
                promotion,
                target_idxs: target_idxs_for_record.unwrap(),
            });

            if let Some(idxs) = new_king_idxs {
                self.king_idxs.entry(color).and_modify(|e| *e = idxs);
            };

            Ok(())
        }

        pub fn to_string(&self, perspective: Color) -> String {
            let mut board_str = String::from("\n");

            let use_white_perspective = perspective == Color::White;

            let mut rank = if use_white_perspective {
                BOARD_LEN - 1
            } else {
                0
            };
            let rank_inc: isize = if use_white_perspective { -1 } else { 1 };
            let final_rank = rank.wrapping_add_signed(rank_inc * (BOARD_LEN - 1) as isize);
            let starting_file = if use_white_perspective {
                0
            } else {
                BOARD_LEN - 1
            };
            let file_inc: isize = if use_white_perspective { 1 } else { -1 };
            let final_file = starting_file.wrapping_add_signed(file_inc * (BOARD_LEN - 1) as isize);

            while rank < BOARD_LEN {
                let mut file_ = starting_file;

                while file_ < BOARD_LEN {
                    let square_occupant = &self.squares[rank][file_];
                    let square_str = match square_occupant {
                        Some(piece_info) => {
                            let piece_str = piece_info.piece.to_string();
                            if piece_info.color == Color::White {
                                piece_str
                            } else {
                                piece_str.to_lowercase()
                            }
                        }
                        None => String::from(" "),
                    };
                    board_str.push_str(&square_str);
                    if file_ != final_file {
                        board_str.push_str(" | ");
                    }

                    file_ = file_.wrapping_add_signed(file_inc);
                }
                if rank != final_rank {
                    board_str.push_str("\n--|---|---|---|---|---|---|---");
                }
                board_str.push_str("\n");

                rank = rank.wrapping_add_signed(rank_inc);
            }

            board_str
        }

        fn search_for_piece(
            &self,
            target_rank_idx: usize,
            target_file_idx: usize,
            desired_piece: Pieces,
            color: Color,
            rank_inc: isize,
            file_inc: isize,
            endless: bool,
        ) -> Option<BoardIdxs> {
            let mut rank_idx = target_rank_idx.wrapping_add_signed(rank_inc);
            let mut file_idx = target_file_idx.wrapping_add_signed(file_inc);

            while rank_idx < 8 && file_idx < 8 {
                if let Some(piece_info) = &self.squares[rank_idx][file_idx] {
                    return if piece_info.piece == desired_piece && piece_info.color == color {
                        Some(BoardIdxs(rank_idx, file_idx))
                    } else {
                        None
                    };
                }

                rank_idx = rank_idx.wrapping_add_signed(rank_inc);
                file_idx = file_idx.wrapping_add_signed(file_inc);

                if !endless {
                    break;
                }
            }

            None
        }

        fn is_legal_castle(
            &self,
            direction: &CastleDirection,
            color: Color,
        ) -> Result<[MoveIdxs; 2], MoveErrors> {
            let starting_rank_idx = if color == Color::White { 0 } else { 7 };
            let king_starting_file_idx = file_to_idx('e');
            let rook_starting_file_idx = file_to_idx(if *direction == CastleDirection::Short {
                'h'
            } else {
                'a'
            });

            // make sure king and rook are in their original positions and have not moved
            for starting_file_idx in [king_starting_file_idx, rook_starting_file_idx].into_iter() {
                if let Some(piece_info) = &self.squares[starting_rank_idx][starting_file_idx] {
                    if piece_info.piece != Pieces::K {
                        return Err(MoveErrors::NoPieceToMakeMove);
                    }
                    if piece_info.move_count != 0 {
                        return Err(MoveErrors::IllegalCastle);
                    };
                } else {
                    return Err(MoveErrors::NoPieceToMakeMove);
                }
            }

            // make sure there are no pieces in between the king and rook and also
            // make sure the the king is not in check at any point during the move
            let mut count = 0;
            for file_idx in king_starting_file_idx..rook_starting_file_idx {
                // the king only moves through the the first 3 squares, so only check if they are
                // attacked
                if count < 3
                    && self
                        .square_attack_count(BoardIdxs(starting_rank_idx, file_idx), color.other())
                        > 0
                {
                    return Err(MoveErrors::IllegalCastle);
                }
                // do not need to check for another piece on the king's starting position
                if file_idx != king_starting_file_idx
                    && self.squares[starting_rank_idx][file_idx].is_some()
                {
                    return Err(MoveErrors::IllegalCastle);
                }
                count += 1;
            }

            let king_target_file_idx = file_to_idx(if *direction == CastleDirection::Short {
                'g'
            } else {
                'c'
            });
            let rook_target_file_idx = file_to_idx(if *direction == CastleDirection::Short {
                'f'
            } else {
                'd'
            });

            Ok([
                MoveIdxs {
                    starting_idxs: BoardIdxs(starting_rank_idx, king_starting_file_idx),
                    target_idxs: BoardIdxs(starting_rank_idx, king_target_file_idx),
                    capture_idxs: None,
                },
                MoveIdxs {
                    starting_idxs: BoardIdxs(starting_rank_idx, rook_starting_file_idx),
                    target_idxs: BoardIdxs(starting_rank_idx, rook_target_file_idx),
                    capture_idxs: None,
                },
            ])
        }

        // TODO test this
        fn square_attack_count(&self, idxs: BoardIdxs, attacker_color: Color) -> u32 {
            let mut count = 0;

            let BoardIdxs(target_rank_idx, target_file_idx) = idxs;

            // don't need to check if a pawn is attacking the square if the target is on the back rank
            // of that color
            if (attacker_color == Color::White && target_rank_idx != 0) || target_rank_idx != 7 {
                let backward_inc: isize = if attacker_color == Color::White {
                    -1
                } else {
                    1
                };
                let prev_rank_idx = target_rank_idx.wrapping_add_signed(backward_inc);
                // check that we're not going off the edge of board looking for a pawn
                if target_file_idx != 7 {
                    if let Some(piece_info) =
                        &self.squares[prev_rank_idx][target_file_idx.wrapping_add(1)]
                    {
                        count += (piece_info.piece == Pieces::K
                            && piece_info.color == attacker_color)
                            as u32;
                    }
                }
                if target_file_idx != 0 {
                    if let Some(piece_info) =
                        &self.squares[prev_rank_idx][target_file_idx.wrapping_sub(1)]
                    {
                        count += (piece_info.piece == Pieces::K
                            && piece_info.color == attacker_color)
                            as u32;
                    }
                }
            }
            for (piece, incs, endless) in [
                (Pieces::N, KNIGHT_INCS.to_vec(), false),
                (Pieces::B, BISHOP_INCS.to_vec(), true),
                (Pieces::R, ROOK_INCS.to_vec(), true),
                (Pieces::Q, KING_QUEEN_INCS.to_vec(), true),
                (Pieces::K, KING_QUEEN_INCS.to_vec(), false),
            ]
            .into_iter()
            {
                for (rank_inc, file_inc) in incs {
                    let res = self.search_for_piece(
                        target_rank_idx,
                        target_file_idx,
                        piece,
                        attacker_color,
                        rank_inc,
                        file_inc,
                        endless,
                    );
                    if res.is_some() {
                        count += 1;
                    }
                }
            }

            count
        }

        // TODO test this fn
        fn is_legal_move(
            &self,
            move_: &SinglePieceMove,
            color: Color,
        ) -> Result<MoveIdxs, MoveErrors> {
            let target_rank_idx = (move_.square.rank as usize) - 1;
            let target_file_idx = file_to_idx(move_.square.file_);

            let target_idxs = BoardIdxs(target_rank_idx, target_file_idx);

            // No moves are valid if a piece of the same color is occupying the target square
            if let Some(piece_info) = &self.squares[target_rank_idx][target_file_idx] {
                if piece_info.color == color {
                    return Err(MoveErrors::SameColorPieceOnTarget);
                }
            }

            // TODO make sure the move wouldn't cause a check for the same color king, try to find
            // a way to do this without copying the entire board

            // TODO clean up the error messages, need to be ordered better so the error messages make sense to user

            match move_.piece_info.piece {
                Pieces::P => {
                    let backward_inc: isize = if color == Color::White { -1 } else { 1 };
                    let prev_rank_idx = target_rank_idx.wrapping_add_signed(backward_inc);

                    match move_.disambiguator {
                        None => {
                            if self.squares[target_rank_idx][target_file_idx].is_some() {
                                // if any other piece is on this square the move is not legal
                                Err(MoveErrors::PawnPushToOccupiedSquare)
                            } else if (color == Color::White && move_.square.rank == 1)
                                || move_.square.rank == 8
                            {
                                // no pawn can ever move to its color's own back rank
                                Err(MoveErrors::PawnPushToBackRank)
                            } else if let Some(piece_info) =
                                &self.squares[prev_rank_idx][target_file_idx]
                            {
                                // push 1 square
                                if piece_info.piece == Pieces::P && piece_info.color == color {
                                    Ok(MoveIdxs {
                                        starting_idxs: BoardIdxs(prev_rank_idx, target_file_idx),
                                        target_idxs,
                                        capture_idxs: None,
                                    })
                                } else {
                                    Err(MoveErrors::NoPieceToMakeMove)
                                }
                            } else if let Some(piece_info) = &self.squares
                                [target_rank_idx.wrapping_add_signed(backward_inc * 2)]
                                [target_file_idx]
                            {
                                // push 2 squares
                                if piece_info.piece == Pieces::P
                                    && piece_info.color == color
                                    && piece_info.move_count == 0
                                {
                                    let starting_rank_idx =
                                        target_rank_idx.wrapping_add_signed(backward_inc * 2);
                                    Ok(MoveIdxs {
                                        starting_idxs: BoardIdxs(
                                            starting_rank_idx,
                                            target_file_idx,
                                        ),
                                        target_idxs,
                                        capture_idxs: None,
                                    })
                                } else {
                                    Err(MoveErrors::NoPieceToMakeMove)
                                }
                            } else {
                                Err(MoveErrors::NoPieceToMakeMove)
                            }
                        }
                        Some(CoordComponent::File(starting_file)) => {
                            let starting_file_idx = file_to_idx(starting_file);
                            // regardless of capture en passant or not, a pawn of the given color must be one rank bank on the file specified in the disambiguator
                            if let Some(starting_square_occupant) =
                                &self.squares[prev_rank_idx][starting_file_idx]
                            {
                                if starting_square_occupant.piece != Pieces::P {
                                    return Err(MoveErrors::NoPieceToMakeMove);
                                }

                                if self.squares[target_rank_idx][target_file_idx].is_some() {
                                    // regular capture
                                    Ok(MoveIdxs {
                                        starting_idxs: BoardIdxs(prev_rank_idx, starting_file_idx),
                                        target_idxs,
                                        capture_idxs: None,
                                    })
                                } else if let Some(other_piece_info) =
                                    &self.squares[prev_rank_idx][target_file_idx]
                                {
                                    // en passant
                                    if other_piece_info.piece == Pieces::P
                                        && other_piece_info.move_count == 1
                                        && self.prev_move_target_idxs().is_some_and(|ti| {
                                            *ti == BoardIdxs(prev_rank_idx, target_file_idx)
                                        })
                                    // TODO need to make sure this piece is on the correct rank
                                    {
                                        Ok(MoveIdxs {
                                            starting_idxs: BoardIdxs(
                                                prev_rank_idx,
                                                starting_file_idx,
                                            ),
                                            target_idxs,
                                            capture_idxs: Some(BoardIdxs(
                                                prev_rank_idx,
                                                target_file_idx,
                                            )),
                                        })
                                    } else {
                                        Err(MoveErrors::InvalidCapture)
                                    }
                                } else {
                                    Err(MoveErrors::InvalidCapture)
                                }
                            } else {
                                Err(MoveErrors::NoPieceToMakeMove)
                            }
                        }
                        _ => Err(MoveErrors::InvalidDisambiguator),
                    }
                }
                piece => {
                    let (incs, endless): (Vec<(isize, isize)>, bool) = match piece {
                        Pieces::N => (KNIGHT_INCS.to_vec(), false),
                        Pieces::B => (BISHOP_INCS.to_vec(), true),
                        Pieces::R => (ROOK_INCS.to_vec(), true),
                        Pieces::Q => (KING_QUEEN_INCS.to_vec(), true),
                        Pieces::K => (KING_QUEEN_INCS.to_vec(), false),
                        Pieces::P => panic!("pawns should've already been handled"),
                    };

                    match &move_.disambiguator {
                        None => {
                            // TODO more checks needed here
                            let mut idxs: Option<BoardIdxs> = None;
                            for (rank_inc, file_inc) in incs {
                                let res = self.search_for_piece(
                                    target_rank_idx,
                                    target_file_idx,
                                    piece,
                                    color,
                                    rank_inc,
                                    file_inc,
                                    endless,
                                );
                                if res.is_some() {
                                    if idxs.is_some() {
                                        return Err(MoveErrors::DisambiguatorRequired);
                                    } else {
                                        idxs = res;
                                    }
                                }
                            }

                            if let Some(starting_idxs) = idxs {
                                Ok(MoveIdxs {
                                    starting_idxs,
                                    target_idxs,
                                    capture_idxs: None,
                                })
                            } else {
                                Err(MoveErrors::NoPieceToMakeMove)
                            }
                        }
                        Some(disambiguator) => {
                            // TODO figure out a better way of searching for the piece: to diff of disambig vs file/rank and apply to rank/file?
                            Err(MoveErrors::NoPieceToMakeMove)
                        }
                    }
                }
            }
        }

        // TODO this should probably return a result instead of an empty hasmap
        pub fn get_moves_for_piece_at(&self, board_idxs: BoardIdxs) -> HashMap<BoardIdxs, String> {
            let BoardIdxs(rank_idx, file_idx) = board_idxs;

            let mut moves = HashMap::new();

            let piece_info = self.squares[rank_idx][file_idx];
            if piece_info.is_none() {
                return moves;
            }
            let piece_info = piece_info.unwrap();

            // TODO make sure these moves wouldn't cause a check / fail to remove check
            match piece_info.piece {
                Pieces::P => {
                    let forward_inc: isize = if piece_info.color == Color::White {
                        1
                    } else {
                        -1
                    };
                    let next_rank_idx = rank_idx.wrapping_add_signed(forward_inc);

                    if self.squares[next_rank_idx][file_idx].is_none() {
                        let idxs = BoardIdxs(next_rank_idx, file_idx);
                        let move_str = board_idxs_to_square_name(idxs.clone());
                        moves.insert(idxs, move_str);

                        // not checking if this is on the opponent's back rank because the pawn
                        // should've been promoted and thus would no longer be a pawn
                        let next_next_rank_idx = next_rank_idx.wrapping_add_signed(forward_inc);
                        if piece_info.move_count == 0
                            && self.squares[next_next_rank_idx][file_idx].is_none()
                        {
                            let idxs = BoardIdxs(next_next_rank_idx, file_idx);
                            let move_str = board_idxs_to_square_name(idxs.clone());
                            moves.insert(idxs, move_str);
                        }
                    }

                    let mut other_file_idxs = Vec::new();
                    if file_idx > 0 {
                        other_file_idxs.push(file_idx.wrapping_sub(1));
                    }
                    if file_idx < 7 {
                        other_file_idxs.push(file_idx.wrapping_add(1));
                    }
                    for other_file_idx in other_file_idxs {
                        if self.squares[next_rank_idx][other_file_idx].is_some_and(
                            |other_piece_info| other_piece_info.color == piece_info.color.other(),
                        ) || (true
                            && is_en_passantable_rank_idx(rank_idx, piece_info.color)
                            && self
                                .prev_move_target_idxs()
                                .is_some_and(|ti| *ti == BoardIdxs(rank_idx, other_file_idx))
                            && self.squares[rank_idx][other_file_idx].is_some_and(
                                |other_piece_info| {
                                    other_piece_info.color == piece_info.color.other()
                                        && other_piece_info.piece == Pieces::P
                                },
                            ))
                        {
                            let idxs = BoardIdxs(next_rank_idx, other_file_idx);
                            let target_square_name = board_idxs_to_square_name(idxs.clone());
                            let current_file = idx_to_file(file_idx);
                            let move_str = format!("{}{}", current_file, target_square_name);
                            moves.insert(idxs, move_str);
                        }
                    }
                }
                piece => {
                    // TODO could make this a function since it's used often
                    let (incs, endless): (Vec<(isize, isize)>, bool) = match piece {
                        Pieces::N => (KNIGHT_INCS.to_vec(), false),
                        Pieces::B => (BISHOP_INCS.to_vec(), true),
                        Pieces::R => (ROOK_INCS.to_vec(), true),
                        Pieces::Q => (KING_QUEEN_INCS.to_vec(), true),
                        Pieces::K => (KING_QUEEN_INCS.to_vec(), false),
                        Pieces::P => panic!("pawns should've already been handled"),
                    };

                    // TODO just make this a regular function on the board struct and take the
                    // piece as an arg instead of doing this move
                    let move_str_start = piece.to_string();
                    let create_move_str = move |idxs| {
                        let target_square_name = board_idxs_to_square_name(idxs);
                        format!("{}{}", move_str_start, target_square_name)
                    };

                    for (rank_inc, file_inc) in incs {
                        let (mut rank_idx_iter, mut file_idx_iter) = (
                            rank_idx.wrapping_add_signed(rank_inc),
                            file_idx.wrapping_add_signed(file_inc),
                        );
                        while rank_idx_iter < BOARD_LEN && file_idx_iter < BOARD_LEN {
                            let idxs = BoardIdxs(rank_idx_iter, file_idx_iter);
                            if let Some(other_piece_info) =
                                &self.squares[rank_idx_iter][file_idx_iter]
                            {
                                if other_piece_info.color != piece_info.color {
                                    moves.insert(idxs.clone(), create_move_str(idxs));
                                }
                                break;
                            }
                            moves.insert(idxs.clone(), create_move_str(idxs));

                            if !endless {
                                break;
                            }

                            rank_idx_iter = rank_idx_iter.wrapping_add_signed(rank_inc);
                            file_idx_iter = file_idx_iter.wrapping_add_signed(file_inc);
                        }
                    }
                    // TODO if the piece is not a king, check to see if there is another piece of the
                    // same type that could move to this square and if so, add a disambiguator
                    // TODO if the piece is the king, check castling
                }
            }

            moves
        }

        fn prev_move_target_idxs(&self) -> Option<&BoardIdxs> {
            self.moves
                .last()
                .and_then(|move_record| Some(&move_record.target_idxs))
        }

        fn piece_rank(color: Color) -> BoardRank {
            [
                Pieces::R,
                Pieces::N,
                Pieces::B,
                Pieces::Q,
                Pieces::K,
                Pieces::B,
                Pieces::N,
                Pieces::R,
            ]
            .map(|piece| {
                Some(PieceInfo {
                    piece,
                    color,
                    move_count: 0,
                })
            })
        }

        fn pawn_rank(color: Color) -> BoardRank {
            [Pieces::P; BOARD_LEN].map(|piece| {
                Some(PieceInfo {
                    piece,
                    color,
                    move_count: 0,
                })
            })
        }

        fn empty_rank() -> BoardRank {
            [None; BOARD_LEN]
        }
    }

    #[derive(strum_macros::Display, Debug)]
    pub enum MoveErrors {
        InvalidNotation,
        InvalidPiece,
        InvalidRank,
        InvalidFile,
        InvalidDisambiguator,
        SameColorPieceOnTarget,
        PawnPushToOccupiedSquare,
        PawnPushToBackRank,
        NoPieceToMakeMove,
        InvalidCapture,
        DisambiguatorRequired,
        InvalidPromotion,
        IllegalCastle,
    }

    const KNIGHT_INCS: [(isize, isize); 8] = [
        (1, 2),
        (2, 1),
        (2, -1),
        (1, -2),
        (-1, -2),
        (-2, -1),
        (-2, 1),
        (-1, 2),
    ];
    const ROOK_INCS: [(isize, isize); 4] = [(1, 0), (0, 1), (-1, 0), (0, -1)];
    const BISHOP_INCS: [(isize, isize); 4] = [(1, 1), (1, -1), (-1, 1), (-1, -1)];
    const KING_QUEEN_INCS: [(isize, isize); 8] = [
        (1, 0),
        (0, 1),
        (-1, 0),
        (0, -1),
        (1, 1),
        (1, -1),
        (-1, 1),
        (-1, -1),
    ];

    pub fn parse_move(move_string: String, color: Color) -> Result<Move, MoveErrors> {
        let move_chars: Vec<char> = move_string.trim().chars().collect();
        let move_len = move_chars.len();

        if move_string == "O-O" {
            return Ok(Move::Castle(CastleDirection::Short));
        }
        if move_string == "O-O-O" {
            return Ok(Move::Castle(CastleDirection::Long));
        }

        if move_len < 2 || 4 < move_len || !move_string.is_ascii() {
            return Err(MoveErrors::InvalidNotation);
        }

        let is_pawn_move = is_valid_file(move_chars[0]);

        let piece_info = if is_pawn_move {
            PieceInfo {
                piece: Pieces::P,
                color,
                move_count: 0,
            }
        } else {
            match Pieces::from_str(&move_string[0..1]) {
                Ok(piece) => PieceInfo {
                    piece,
                    color,
                    move_count: 0,
                },
                Err(_) => return Err(MoveErrors::InvalidPiece),
            }
        };

        let square = Square {
            file_: parse_file(move_chars[move_len - 2])?,
            rank: parse_rank(move_chars[move_len - 1])?,
        };

        let max_len = if is_pawn_move { 3 } else { 4 };
        let disambiguator: Option<CoordComponent> = if move_len == max_len {
            // TODO disambiguator requires move validation, i.e. disallow Naa1
            let char_ = move_chars[move_len - 3];
            let coord_comp = if char_.is_digit(10) {
                CoordComponent::Rank(parse_rank(char_)?)
            } else {
                CoordComponent::File(parse_file(char_)?)
            };
            Some(coord_comp)
        } else {
            None
        };

        let move_ = Move::Standard(SinglePieceMove {
            piece_info,
            square,
            disambiguator,
        });

        Ok(move_)
    }

    fn parse_rank(char_: char) -> Result<u8, MoveErrors> {
        let rank = match char_.to_digit(10) {
            Some(rank) => rank as u8,
            None => return Err(MoveErrors::InvalidRank),
        };

        if is_valid_rank(rank) {
            Ok(rank)
        } else {
            Err(MoveErrors::InvalidRank)
        }
    }

    fn parse_file(file_: char) -> Result<char, MoveErrors> {
        if is_valid_file(file_) {
            Ok(file_)
        } else {
            Err(MoveErrors::InvalidFile)
        }
    }

    fn is_valid_file(file_: char) -> bool {
        'a' <= file_ && file_ <= 'h'
    }

    fn is_valid_rank(rank: u8) -> bool {
        1 <= rank && rank <= 8
    }

    fn file_to_idx(file_: char) -> usize {
        (file_ as usize) - ('a' as usize)
    }

    pub fn idx_to_file(idx: usize) -> char {
        char::from_u32(('a' as u32) + (idx as u32)).unwrap_or_else(|| 'x')
    }

    pub fn board_idxs_to_square_name(board_idxs: BoardIdxs) -> String {
        let BoardIdxs(rank_idx, file_idx) = board_idxs;
        let rank = rank_idx + 1;
        let file_ = idx_to_file(file_idx);
        String::from(format!("{}{}", file_, rank))
    }

    fn is_en_passantable_rank_idx(rank_idx: usize, color: Color) -> bool {
        let en_passantable_rank_idx = if color == Color::White { 4 } else { 3 };
        rank_idx == en_passantable_rank_idx
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use board::*;

    macro_rules! match_and_run {
        ($val_result:expr, $p:path, $function: expr) => {
            if let $p(val) = $val_result {
                $function(val)
            } else {
                panic!("Invalid enum variant")
            }
        };
    }

    #[test]
    fn parse_move__single_possible_piece() {
        let result = parse_move(String::from("Bb1"), Color::White).unwrap();

        match_and_run!(result, Move::Standard, |move_: SinglePieceMove| {
            assert_eq!(move_.piece_info.piece, Pieces::B);
            assert_eq!(move_.square.file_, 'b');
            assert_eq!(move_.square.rank, 1);
            assert_eq!(move_.disambiguator, None);
        });
    }

    #[test]
    fn parse_move__multiple_possible_pieces__file_included() {
        let result = parse_move(String::from("Nce4"), Color::White).unwrap();

        match_and_run!(result, Move::Standard, |move_: SinglePieceMove| {
            assert_eq!(move_.piece_info.piece, Pieces::N);
            assert_eq!(move_.square.file_, 'e');
            assert_eq!(move_.square.rank, 4);
            assert_eq!(move_.disambiguator, Some(CoordComponent::File('c')));
        });
    }

    #[test]
    fn parse_move__multiple_possible_pieces__rank_included() {
        let result = parse_move(String::from("N5e4"), Color::White).unwrap();

        match_and_run!(result, Move::Standard, |move_: SinglePieceMove| {
            assert_eq!(move_.piece_info.piece, Pieces::N);
            assert_eq!(move_.square.file_, 'e');
            assert_eq!(move_.square.rank, 4);
            assert_eq!(move_.disambiguator, Some(CoordComponent::Rank(5)));
        });
    }

    #[test]
    fn parse_move__single_possible_pawn() {
        let result = parse_move(String::from("e4"), Color::White).unwrap();

        match_and_run!(result, Move::Standard, |move_: SinglePieceMove| {
            assert_eq!(move_.piece_info.piece, Pieces::P);
            assert_eq!(move_.square.file_, 'e');
            assert_eq!(move_.square.rank, 4);
            assert_eq!(move_.disambiguator, None);
        });
    }

    #[test]
    fn parse_move__multiple_possible_pawns() {
        let result = parse_move(String::from("cd5"), Color::White).unwrap();

        match_and_run!(result, Move::Standard, |move_: SinglePieceMove| {
            assert_eq!(move_.piece_info.piece, Pieces::P);
            assert_eq!(move_.square.file_, 'd');
            assert_eq!(move_.square.rank, 5);
            assert_eq!(move_.disambiguator, Some(CoordComponent::File('c')));
        });
    }

    #[test]
    fn parse_move__various_bad_moves() {
        assert!(parse_move(String::from(""), Color::White).is_err());
        assert!(parse_move(String::from("a"), Color::White).is_err());
        assert!(parse_move(String::from("ab"), Color::White).is_err());
        assert!(parse_move(String::from("abc"), Color::White).is_err());
        assert!(parse_move(String::from("1"), Color::White).is_err());
        assert!(parse_move(String::from("12"), Color::White).is_err());
        assert!(parse_move(String::from("123"), Color::White).is_err());
        assert!(parse_move(String::from("-"), Color::White).is_err());
        assert!(parse_move(String::from("N"), Color::White).is_err());
        assert!(parse_move(String::from("Ne"), Color::White).is_err());
        assert!(parse_move(String::from("Ne6a"), Color::White).is_err());
        assert!(parse_move(String::from("Ne67"), Color::White).is_err());
        assert!(parse_move(String::from("Ne6-"), Color::White).is_err());
        assert!(parse_move(String::from("Ae5"), Color::White).is_err());
        assert!(parse_move(String::from("Ne0"), Color::White).is_err());
        assert!(parse_move(String::from("Nq5"), Color::White).is_err());
        assert!(parse_move(String::from("z3"), Color::White).is_err());
        assert!(parse_move(String::from("zb3"), Color::White).is_err());
        assert!(parse_move(String::from("az3"), Color::White).is_err());
        assert!(parse_move(String::from("µ"), Color::White).is_err());
        assert!(parse_move(String::from("µe5"), Color::White).is_err());
        assert!(parse_move(String::from("Nµ5"), Color::White).is_err());
        assert!(parse_move(String::from("Neµ"), Color::White).is_err());
        assert_eq!(&5, &5);
    }
    // TODO make a function to init a board with a list of (piece, idxs) for testing
}
