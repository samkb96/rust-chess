use crate::engines::*;
use crate::game_state::*;
use crate::mechanics::*;
use crate::constants::*;
use macroquad::prelude::*;
use std::collections::HashMap;

const BOT_INPUT: bool = true;

pub const WINDOW_WIDTH: f32 = SQUARE_SIZE * 14.;
pub const WINDOW_HEIGHT: f32 = SQUARE_SIZE * 9.5;
const SQUARE_SIZE: f32 = 100.;

const X_OFFSET: f32 = 440.;
const Y_OFFSET: f32 = 120.;

const DARK_COLOUR: Color = color_u8!(167, 128, 99, 255);
const LIGHT_COLOUR: Color = color_u8!(238, 238, 210, 255);
const LEGAL_MOVE_HIGHLIGHT_COLOUR: Color = color_u8!(223, 130, 53, 100);
const LAST_MOVE_HIGHLIGHT_COLOUR: Color = color_u8!(161, 12, 14, 100);
const DEEMPHASISED_COLOUR: Color = color_u8!(153, 134, 119, 175);

pub struct Board {
    human_input: bool, 
    drag_state: DragState,
    drag_mouse_position: Option<Vec2>,
    legal_move_highlights: Vec<usize>,
    last_move_highlight: Option<usize>,
    pending_promotion: Option<PendingPromotion>,
    bot_move_pending: bool,
}

impl Board {
    pub fn initialise(human_input: bool) -> Self {
        Board {
            human_input, 
            drag_state: DragState::None,
            drag_mouse_position: None,
            legal_move_highlights: vec![],
            last_move_highlight: None,
            pending_promotion: None,
            bot_move_pending: false,
        }
    }

    pub fn draw_to_screen(&self, game_state: &GameState, texture: &PieceTextures, font: &Font) {
        // draw board
        for square_idx in 0..64 {
            let coord = BoardCoordinate::from_usize(square_idx);
            BoardCoordinate::draw_board_square(&coord, font);
        }

        // draw highlights on top
        self.legal_move_highlights();
        self.last_move_highlights();

        // draw pieces on top of highlights
        for square_idx in 0..64 {
            let coord = BoardCoordinate::from_usize(square_idx);
            if let Some(piece) = game_state.bitboards.piece_at_square(square_idx) {
                let skip_drawing = self.drag_state.dragging_from_square(coord);
                if !skip_drawing {
                    coord.draw_piece_at_square(piece, texture);
                }
            }
        }

        // dragged piece drawn on top of everything
        if let Some(mouse_position) = self.drag_mouse_position {
            if let DragState::Started { piece, .. } | DragState::Dragging { piece, .. } =
                self.drag_state
            {
                Self::draw_piece_at_mouse_position(&piece, mouse_position, texture)
            }
        }

        self.draw_promotion_picker(texture, font)
    }

    fn get_picker_squares(target: BoardCoordinate) -> [BoardCoordinate; 4] {
        let file = target.file;
        match target.rank {
            0 => [
                    BoardCoordinate { rank: 0, file },
                    BoardCoordinate { rank: 1, file },
                    BoardCoordinate { rank: 2, file },
                    BoardCoordinate { rank: 3, file }
            ],
            7 => [
                BoardCoordinate { rank: 4, file },
                BoardCoordinate { rank: 5, file },
                BoardCoordinate { rank: 6, file },
                BoardCoordinate { rank: 7, file }
            ],
            _ => unreachable!("Calling get_picker_squares when pawn isn't at final rank")
        }
    }

    fn draw_promotion_picker(&self, texture: &PieceTextures, font: &Font) {
        let Some(pending_promotion) = self.pending_promotion else { return; };

        let promoting_side = pending_promotion.piece.colour;

        // first, draw a transparent rectangle over the whole board
        let board_top_left = BoardCoordinate { rank: 7, file: 0 };
        let board_top_left_pos = board_top_left.top_left_coordinate();

        draw_rectangle(
            board_top_left_pos.0,
            board_top_left_pos.1,
            SQUARE_SIZE * 8.0,
            SQUARE_SIZE * 8.0,
            DEEMPHASISED_COLOUR
        );

        // re-draw a 4-square picker region emanating from the target square

        let picker_squares = Board::get_picker_squares(pending_promotion.target);
        for i in 0..4 { picker_squares[i].draw_board_square(font) }

        // add textures for promotion choices to this picker region

        let target_square_top_left = pending_promotion.target.top_left_coordinate();

        // get coords of target square's top left
        let target_x = target_square_top_left.0;
        let target_y = target_square_top_left.1;

        // rectangle's top left x is the same. y depends on the rank
        let picker_x = target_x;
        let picker_y = match pending_promotion.piece.colour {
            PieceColour::White =>
                target_y.max(Y_OFFSET),
            PieceColour::Black =>
                (target_y - SQUARE_SIZE * 3.0).min(Y_OFFSET + SQUARE_SIZE * 4.0)
        };

        // reorder choices depending on pawn colour, so queen is always at the edge
        let mut choices = [
            PieceKind::Queen,
            PieceKind::Rook,
            PieceKind::Bishop,
            PieceKind::Knight
        ];

        if promoting_side == PieceColour::Black {
            choices.reverse()
        };

        // draw the piece textures to the picker region

        for (index, &kind) in choices.iter().enumerate() {
            let piece = Piece {
                kind,
                colour: pending_promotion.piece.colour
            };

            let y_pos_base = picker_y + index as f32 * SQUARE_SIZE;

            let piece_texture = texture.get(&piece);
            let piece_width = piece_texture.width();
            let piece_height = piece_texture.height();

            let piece_x_centre = picker_x + (SQUARE_SIZE - piece_width) / 2.0;
            let piece_y_centre = y_pos_base + (SQUARE_SIZE - piece_height) / 2.0;

            draw_texture_ex(
                piece_texture,
                piece_x_centre,
                piece_y_centre,
                WHITE,
                DrawTextureParams {
                    ..Default::default()
                }
            );
        }
    }

    pub fn update(&mut self, game_state: &mut GameState, mouse_position: Vec2) {
        if is_key_pressed(KeyCode::Z) {
            game_state.unmake_move()
        }

        self.promotion_picker_handler(game_state, mouse_position);

        match self.drag_state {
            DragState::None => self.dragstate_handler_none(game_state, mouse_position),
            DragState::Started { piece, origin } => {
                self.dragstate_handler_started(mouse_position, piece, origin)
            }
            DragState::Dragging { piece, origin } => {
                self.dragstate_handler_dragging(game_state, mouse_position, piece, origin)
            }
        }
    }

    fn promotion_choice_from_mouse_pos(&self, mouse_position: Vec2) -> Option<PieceKind> {

        let pending_promotion = self.pending_promotion
                .expect("No pending promotion in gamestate while handling picker, somehow");

        let side_to_promote = pending_promotion.piece.colour;
        let picker_file = pending_promotion.target.file;

        let chosen_coordinate =
            BoardCoordinate::mouse_pos_to_coordinate(mouse_position)?; // clicked off board

        if chosen_coordinate.file != picker_file { return None }; // clicked outside picker file

        let piece_kind = match (side_to_promote, chosen_coordinate.rank) {
            (PieceColour::White, 7) | (PieceColour::Black, 0) => PieceKind::Queen,
            (PieceColour::White, 6) | (PieceColour::Black, 1) => PieceKind::Rook,
            (PieceColour::White, 5) | (PieceColour::Black, 2) => PieceKind::Bishop,
            (PieceColour::White, 4) | (PieceColour::Black, 3) => PieceKind::Knight,
            _ => return None
        };

        Some(piece_kind)
    }

    fn promotion_picker_handler(&mut self, game_state: &mut GameState, mouse_position: Vec2) {

        // don't return any selection until we click
        if !is_mouse_button_pressed(MouseButton::Left) { return; }

        // if no promotion situation to handle, don't bother
        let Some(pending_promotion) = self.pending_promotion else { return; };

        // get promotion choice from successful mouse click on picker, otherwise return;
        let Some(choice_of_promotion_piece) =
            self.promotion_choice_from_mouse_pos(mouse_position) else { return };
        println!("Piece chosen");
        let candidate_move = self.candidate_move(
            game_state,
            pending_promotion.origin,
            pending_promotion.target,
            pending_promotion.piece,
            Some(choice_of_promotion_piece)
        );

        // move should already be legal, checked in is_legal_promotion_attempt()

        game_state.make_move(candidate_move);
        self.last_move_highlight = Some(candidate_move.end_square);
        // TODO: add pending_bot_move bool field in the struct
        // set to true here
        // make the bot move at the beginning of update if the bool is true
        // this means the piece is set down in the right square, and bot only calculates next frame
        // need to rework bot logic in the promotion picker too
        if BOT_INPUT {
            self.last_move_highlight = bot_move(game_state).map(|mv| mv.end_square);
        }
        self.pending_promotion = None;

    }

    fn draw_piece_at_mouse_position(piece: &Piece, mouse_position: Vec2, textures: &PieceTextures) {
        let texture = textures.get(piece);
        let (width, height) = (texture.width(), texture.height());
        let x = mouse_position[0] - width / 2.0;
        let y = mouse_position[1] - height / 2.0;
        let x_bounded = if x < 0. { 0.1 } else { x };
        let y_bounded = if y < 0. { 0.1 } else { y };
        let scaled_up = Vec2::from((width * 1.09, height * 1.09));

        draw_texture_ex(
            texture,
            x_bounded,
            y_bounded,
            WHITE,
            DrawTextureParams {
                dest_size: Some(scaled_up),
                ..Default::default()
            },
        );
    }

    fn legal_move_highlights(&self) {
        for square_index in self.legal_move_highlights.iter() {
            let rank = square_index / 8;
            let file = square_index % 8;
            let square_coords = (
                file as f32 * SQUARE_SIZE + X_OFFSET,
                (7 - rank) as f32 * SQUARE_SIZE + Y_OFFSET,
            );
            draw_rectangle(
                square_coords.0,
                square_coords.1,
                SQUARE_SIZE,
                SQUARE_SIZE,
                LEGAL_MOVE_HIGHLIGHT_COLOUR,
            );
        }
    }

    fn last_move_highlights(&self) {
        if let Some(square_idx) = self.last_move_highlight {
            if !self.legal_move_highlights.contains(&square_idx) {
                let rank = square_idx / 8;
                let file = square_idx % 8;
                let square_coords = (
                    file as f32 * SQUARE_SIZE + X_OFFSET,
                    (7 - rank) as f32 * SQUARE_SIZE + Y_OFFSET,
                );
                draw_rectangle(
                    square_coords.0,
                    square_coords.1,
                    SQUARE_SIZE,
                    SQUARE_SIZE,
                    LAST_MOVE_HIGHLIGHT_COLOUR,
                );
            }
        }
    }

    fn dragstate_handler_none(&mut self, game_state: &GameState, mouse_position: Vec2) {
        if is_mouse_button_down(MouseButton::Left) {
            if let Some(coordinate) = BoardCoordinate::mouse_pos_to_coordinate(mouse_position) {
                if let Some(piece) = game_state.bitboards.piece_at_square(coordinate.to_usize()) {
                    self.drag_state = DragState::Started {
                        piece,
                        origin: coordinate,
                    };
                    self.drag_mouse_position = Some(mouse_position);
                }
            }
        }
    }

    fn dragstate_handler_started(&mut self, mouse_position: Vec2, piece: Piece, origin: BoardCoordinate) {
        if is_mouse_button_down(MouseButton::Left) {
            // if piece is held, move into drag mode
            self.drag_state = DragState::Dragging { piece, origin };
            self.drag_mouse_position = Some(mouse_position);
        } else {
            // if mouse has been released immediately, reset everything
            self.drag_state = DragState::None;
            self.drag_mouse_position = None;
        }
    }

    fn dragstate_handler_dragging(
        &mut self,
        game_state: &mut GameState,
        mouse_position: Vec2,
        piece: Piece,
        origin: BoardCoordinate,
    ) {
        self.drag_mouse_position = Some(mouse_position);
        let relevant_legal_moves = game_state.legal_moves_from(origin.to_usize());

        self.legal_move_highlights = relevant_legal_moves.iter().map(|m| m.end_square).collect();

        if is_mouse_button_released(MouseButton::Left) {
            let target = BoardCoordinate::mouse_pos_to_coordinate(mouse_position).unwrap_or(origin);

            // separate control flow if we're pushing a pawn to the last rank
            // we check that promotion is legal here to avoid bringing up the picker
            if self.is_legal_promotion_attempt(game_state, piece, origin, target, &relevant_legal_moves) {
                // set up pending promotion if this happens
                // we initialise with piece = pawn
                // this piece kind is then set to the actual promotion choice within the handler
                self.pending_promotion = Some(PendingPromotion { origin, target, piece });

                // we're not dragging anymore; promotion picker is click-only
                // clear dragstate and highlights
                self.legal_move_highlights = Vec::new();
                self.drag_mouse_position = None;
                self.drag_state = DragState::None;
            }

            let candidate_move = self.candidate_move(game_state, origin, target, piece, None);

            if relevant_legal_moves.contains(&candidate_move) {

                game_state.make_move(candidate_move);
                self.last_move_highlight = Some(candidate_move.end_square);
                if BOT_INPUT {
                    self.last_move_highlight = bot_move(game_state).map(|mv| mv.end_square);
                }
            };

            self.legal_move_highlights = Vec::new();
            self.drag_mouse_position = None;
            self.drag_state = DragState::None;
        }
    }

    fn is_legal_promotion_attempt(
        &self,
        game_state: &GameState,
        piece: Piece,
        origin: BoardCoordinate,
        target: BoardCoordinate,
        relevant_legal_moves: &Moves
    ) -> bool {
        // early return if we're not moving a pawn

        if piece.kind != PieceKind::Pawn { return false; };

        let piece_colour = piece.colour;
        let target_rank = target.rank;

        // return false for pawns of appropriate colour on inappropriate rank
        if 7 * (1 - piece_colour as u8) != target_rank { return false; };

        // passing all these checks means we are attempting to promote
        // just need to check for legality

        let candidate_move = self.candidate_move(
            game_state,
            origin,
            target,
            piece,
            Some(PieceKind::Queen)
        );

        if relevant_legal_moves.contains(&candidate_move) { return true };

        unreachable!("Logic error in is_legal_promotion_attempt")
    }

    fn candidate_move(
        &self,
        game_state: &GameState,
        origin: BoardCoordinate,
        target: BoardCoordinate,
        piece: Piece,
        promotion: Option<PieceKind> 
    ) -> Move {
        let (start, end) = (origin.to_usize(), target.to_usize());
        // need to implement promotion choice
        let target_square_occupant = game_state
            .bitboards
            .piece_at_square(end)
            .map(|piece| piece.kind);

        let candidate_move_type = Self::generate_move_type(origin, target, piece, target_square_occupant);

        if let Some(promotion_choice) = promotion {
            return Move::promotion(start, end, promotion_choice, target_square_occupant)
        }
        match candidate_move_type {
            MoveType::Normal => {
                if let Some(captured) = target_square_occupant {
                    Move::capture(start, end, piece.kind, captured)
                } else {
                    Move::quiet(start, end, piece.kind)
                }
            },
            MoveType::EnPassant => Move::en_passant(start, end),
            MoveType::CastleKingside => Move::castling(end, MoveType::CastleKingside),
            MoveType::CastleQueenside => Move::castling(end, MoveType::CastleQueenside),
        }
    }

    fn generate_move_type(
        origin: BoardCoordinate,
        target: BoardCoordinate,
        piece: Piece,
        captured: Option<PieceKind>
    ) -> MoveType {
        let piece_kind = piece.kind as usize;

        if (1..=4).contains(&piece_kind) {
            return MoveType::Normal // only pawns and kings get special moves
        }

        match piece_kind {
            0 => {

                // needs to be diagonal for en passant
                let is_diagonal_move = [7, 9].contains(&origin.to_usize().abs_diff(target.to_usize()));
                if !is_diagonal_move {
                    return MoveType::Normal
                }

                // landing square has to be empty for en passant
                let is_empty_target = captured.is_none();
                if !is_empty_target {
                    return MoveType::Normal
                }

                MoveType::EnPassant
            },
            5 => {

                // two square movement indicates castling
                let moved_two_squares = origin.file.abs_diff(target.file) == 2;

                if !moved_two_squares {
                    return MoveType::Normal
                }
                // direction gives side
                let moved_rightward = origin.file < target.file;
                if moved_rightward {MoveType::CastleKingside} else {MoveType::CastleQueenside}
                
            },
            _ => unreachable!("handled this already")
        }
    }
}

#[derive(Copy, Clone, Debug)]
enum DragState {
    None,
    Started {
        piece: Piece,
        origin: BoardCoordinate,
    },
    Dragging {
        piece: Piece,
        origin: BoardCoordinate,
    },
}
impl DragState {
    fn dragging_from_square(&self, square_idx: BoardCoordinate) -> bool {
        match self {
            DragState::Started { piece: _, origin } | DragState::Dragging { piece: _, origin } => {
                origin == &square_idx
            }
            _ => false,
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct BoardCoordinate {
    rank: u8,
    file: u8,
}
impl BoardCoordinate {
    fn to_usize(self) -> usize {
        (self.rank * 8 + self.file) as usize
    }

    pub fn from_usize(square_index: usize) -> Self {
        BoardCoordinate {
            rank: (square_index / 8) as u8,
            file: (square_index % 8) as u8,
        }
    }

    fn top_left_coordinate(&self) -> (f32, f32) {
        (
            X_OFFSET + self.file as f32 * SQUARE_SIZE,
            Y_OFFSET + (7 - self.rank) as f32 * SQUARE_SIZE,
        )
    }

    fn draw_board_square(&self, font: &Font) {
        let (square_colour, text_colour) = match (self.rank + self.file) % 2 {
            0 => (DARK_COLOUR, LIGHT_COLOUR),
            1 => (LIGHT_COLOUR, DARK_COLOUR),
            _ => unreachable!(),
        };

        let top_left = self.top_left_coordinate();

        let text_coords = (top_left.0 + 5.0, top_left.1 + SQUARE_SIZE - 5.0);

        let square_name = self.square_name();

        draw_rectangle(
            top_left.0,
            top_left.1,
            SQUARE_SIZE,
            SQUARE_SIZE,
            square_colour,
        );

        draw_text_ex(
            &square_name,
            text_coords.0,
            text_coords.1,
            TextParams {
                font: Some(font),
                font_size: 14,
                color: text_colour,
                ..Default::default()
            },
        );
    }

    fn draw_piece_at_square(&self, piece: Piece, textures: &PieceTextures) {
        let texture = textures.get(&piece);
        let top_left = self.top_left_coordinate();
        let (height, width) = (texture.height(), texture.width());

        const VERTICAL_LIFT: f32 = 15.0;

        draw_texture_ex(
            texture,
            top_left.0 + (SQUARE_SIZE - width) / 2.0, // centre aligned
            top_left.1 + SQUARE_SIZE - height - VERTICAL_LIFT, // bottoms aligned
            WHITE,
            DrawTextureParams {
                ..Default::default()
            },
        );
    }

    pub fn square_name(&self) -> String {
        format!(
            "{}{}",
            file_from_int(self.file + 1).expect("Illegal file index"),
            self.rank + 1
        )
    }

    fn mouse_pos_to_coordinate(mouse: Vec2) -> Option<BoardCoordinate> {
        let file = ((mouse.x - X_OFFSET) / SQUARE_SIZE).floor() as i32;
        let rank = 7 - ((mouse.y - Y_OFFSET) / SQUARE_SIZE).floor() as i32;

        if (0..8).contains(&file) & (0..8).contains(&rank) {
            Some(BoardCoordinate {
                rank: rank as u8,
                file: file as u8,
            })
        } else {
            None
        }
    }
}

async fn load_piece_texture(path: &str) -> Texture2D {
    let texture = load_texture(path)
        .await
        .unwrap_or_else(|_| panic!("failed to load texture {path}"));
    texture.set_filter(FilterMode::Linear);
    texture
}

pub struct PieceTextures {
    map: HashMap<Piece, Texture2D>,
}

impl PieceTextures {
    pub async fn new() -> Self {
        let mut map = HashMap::new();

        for &colour in &PIECE_COLOURS {
            for &kind in &PIECE_KINDS {
                let piece = Piece { kind, colour };
                let image_name = piece.to_image_name();
                let filename = format!("assets/textures/{}.png", image_name.as_str());
                map.insert(piece, load_piece_texture(&filename).await);
            }
        }
        Self { map }
    }

    fn get(&self, &piece: &Piece) -> &Texture2D {
        self.map
            .get(&(piece))
            .expect("Unable to load piece texture for piece {piece}")
    }
}

#[derive(Copy, Clone, Debug)]
struct PendingPromotion {
    origin: BoardCoordinate,
    target: BoardCoordinate,
    piece: Piece
}

pub fn file_from_int(n: u8) -> Option<char> {
    if (1..=8).contains(&n) {
        Some((b'a' + (n - 1)) as char)
    } else {
        None
    }
}

pub fn kill_game() -> bool {
    is_key_pressed(KeyCode::Escape)
}

pub fn draw_framerate(font: &Font) {
    let fps = get_fps();
    let fps_text = format!("FPS: {fps}");
    draw_text_ex(
        &fps_text,
        30.,
        50.,
        TextParams {
            font: Some(font),
            font_size: 24,
            color: WHITE,
            ..Default::default()
        },
    );
}

pub fn window_conf() -> Conf {
    Conf {
        window_title: "Chess".to_string(),
        window_width: 800,
        window_height: 800,
        fullscreen: true,
        ..Default::default()
    }
}
