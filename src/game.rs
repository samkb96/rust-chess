use crate::engine::*;
use crate::game_state::*;
use crate::mechanics::*;
use crate::constants::*;
use macroquad::prelude::*;
use std::collections::HashMap;

const BOT_INPUT: bool = false;

pub const WINDOW_WIDTH: f32 = SQUARE_SIZE * 14.;
pub const WINDOW_HEIGHT: f32 = SQUARE_SIZE * 9.5;
const SQUARE_SIZE: f32 = 100.;

const X_OFFSET: f32 = 440.;
const Y_OFFSET: f32 = 120.;

const DARK_COLOUR: Color = color_u8!(167, 128, 99, 255);
const LIGHT_COLOUR: Color = color_u8!(238, 238, 210, 255);
const LEGAL_MOVE_HIGHLIGHT_COLOUR: Color = color_u8!(223, 130, 53, 50);
const LAST_MOVE_HIGHLIGHT_COLOUR: Color = color_u8!(161, 12, 14, 50);


pub struct Board {
    drag_state: DragState,
    drag_mouse_position: Option<Vec2>,
    legal_move_highlights: Vec<usize>,
    last_move_highlight: Option<usize>,
}

impl Board {
    pub fn initialise() -> Self {
        Board {
            drag_state: DragState::None,
            drag_mouse_position: None,
            legal_move_highlights: vec![],
            last_move_highlight: None,
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
    }

    pub fn update(&mut self, game_state: &mut GameState, mouse_position: Vec2) {
        if is_key_pressed(KeyCode::Z) {
            game_state.unmake_move()
        }

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

    fn dragstate_handler_started(
        &mut self,
        mouse_position: Vec2,
        piece: Piece,
        origin: BoardCoordinate,
    ) {
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

            let candidate_move = self.candidate_move(game_state, origin, target, piece);

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

    fn candidate_move(
        &self,
        game_state: &GameState,
        origin: BoardCoordinate,
        target: BoardCoordinate,
        piece: Piece,
    ) -> Move {
        // need to implement promotion choice
        let target_square_occupant = game_state
            .bitboards
            .piece_at_square(target.to_usize())
            .map(|piece| piece.kind);

        let candidate_move_type = Self::generate_move_type(origin, target, piece);

        Move {
            start_square: origin.to_usize(),
            end_square: target.to_usize(),
            piece_moved: piece.kind,
            captured: target_square_occupant,
            promotion: None,
            move_type: candidate_move_type,
        } // TODO promotion, en passant, castling done here but needs implementing elsewhere
    }

    fn generate_move_type(
        origin: BoardCoordinate,
        target: BoardCoordinate,
        piece: Piece,
    ) -> MoveType {
        let mut move_type = MoveType::Normal;
        if (piece.kind == PieceKind::King) & (origin.file.abs_diff(target.file) == 2) {
            move_type = if origin.file > target.file {
                MoveType::CastleQueenside
            } else {
                MoveType::CastleKingside
            };
        }
        move_type
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
