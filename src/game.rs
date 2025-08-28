use crate::mechanics::*;
use arrayvec::ArrayString;
use core::fmt::Write;
use macroquad::prelude::*;
use std::collections::HashMap;

pub const WINDOW_WIDTH: f32 = SQUARE_SIZE * 14.;
pub const WINDOW_HEIGHT: f32 = SQUARE_SIZE * 9.5;
pub const SQUARE_SIZE: f32 = 100.;

pub const X_OFFSET: f32 = 440.;
pub const Y_OFFSET: f32 = 120.;

const BOARD_SIZE: usize = 8;
const DARK_COLOUR: Color = color_u8!(118, 150, 86, 255);
const LIGHT_COLOUR: Color = color_u8!(238, 238, 210, 255);
const LEGAL_MOVE_HIGHLIGHT_COLOUR: Color = color_u8!(223, 83, 53, 56);
const SQUARE_COLOURS: [SquareColour; 2] = [SquareColour::Dark, SquareColour::Light];

pub const STARTING_POSITION_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

pub struct DisplayBoard {
    squares: [[Square; 8]; 8],
    pub game_state: GameState,
    drag_state: DragState,
    drag_mouse_position: Option<Vec2>,
    legal_move_highlights: Vec<usize>,
    last_move_highlight: Option<usize>
}

/// main DisplayBoard methods called in main loop
pub trait CoreMethods {
    fn initialise(initial_game_state: GameState) -> Self;
    fn draw_to_screen(&self, texture: &PieceTextures, font: &Font);
    fn update(&mut self);
    fn draw_debug_info(&self, font: &Font);
}

// helpers used in drag-drop match cases
trait DragStateHandling {
    fn dragstate_handler_none(&mut self, mouse_position: Vec2);
    fn dragstate_handler_started(
        &mut self,
        mouse_position: Vec2,
        piece: Piece,
        origin: BoardCoordinate,
    );
    fn dragstate_handler_dragging(
        &mut self,
        mouse_position: Vec2,
        piece: Piece,
        origin: BoardCoordinate,
    );
    fn reset_on_null_move(&mut self, origin: BoardCoordinate, piece: Piece);
    fn make_verified_move(&mut self, target: BoardCoordinate, piece: Piece);
}

/// operations
trait SquareMethods {
    fn square_at_coordinate(&self, coordinate: BoardCoordinate) -> &Square;
    fn square_at_mutable(&mut self, coordinate: BoardCoordinate) -> &mut Square;
    fn add_to_square(&mut self, coordinate: BoardCoordinate, piece: Piece);
    fn clear_square(&mut self, coordinate: BoardCoordinate) -> Option<Piece>;
}

trait MouseFunctions {
    fn mouse_pos_to_coordinate(mouse: Vec2) -> Option<BoardCoordinate>;
    fn coordinate_to_screen_position(coordinate: BoardCoordinate) -> Vec2;
}

trait HelperFunctions {
    fn is_move_legal(candidate_move: &Move, legal_moves: &[Move]) -> bool;
    fn legal_move_highlights(&self);
    //fn create_candidate_move(start: BoardCoordinate,end: BoardCoordinate, promotion: Option<PieceKind>) -> Move;
}

impl CoreMethods for DisplayBoard {
    fn initialise(initial_game_state: GameState) -> Self {
        // initialise all squares as empty and light
        let mut squares = [[Square {
            piece: None,
            colour: SquareColour::Light,
        }; 8]; 8];

        for rank in 0..8 {
            for file in 0..8 {
                let square_colour = SQUARE_COLOURS[(rank + file) % 2];
                squares[rank][file].colour = square_colour;
                let square_index = rank * 8 + file;

                squares[rank][file].piece =
                    initial_game_state.bitboards.piece_at_square(square_index);
            }
        }

        DisplayBoard {
            squares,
            game_state: initial_game_state,
            drag_state: DragState::None,
            drag_mouse_position: None,
            legal_move_highlights: Vec::new(),
            last_move_highlight: None,
        }
    }

    fn draw_to_screen(&self, texture: &PieceTextures, font: &Font) {
        for idx in 0..64 {
            let rank = idx / 8;
            let file = idx % 8;
            if let Some(square) = self.squares.get(rank).and_then(|r| r.get(file)) {
                draw_square(rank, file, square.colour, square.piece, texture, font)
            }

        }

        self.legal_move_highlights();

        if let Some(mouse_position) = self.drag_mouse_position {
            if let DragState::Started { piece, .. } | DragState::Dragging { piece, .. } =
                self.drag_state
            {
                draw_piece_at_mouse_position(piece, mouse_position, texture)
            }
        }

    }

    fn update(&mut self) {
        let mouse_position: Vec2 = mouse_position().into();

        match self.drag_state {
            DragState::None => self.dragstate_handler_none(mouse_position),
            DragState::Started { piece, origin } => {
                self.dragstate_handler_started(mouse_position, piece, origin)
            }
            DragState::Dragging { piece, origin } => {
                self.dragstate_handler_dragging(mouse_position, piece, origin)
            }
        }
    }

    fn draw_debug_info(&self, font: &Font) {
        let mouse_position = mouse_position();
        let mouse_vec2: Vec2 = mouse_position.into();
        let coord = Self::mouse_pos_to_coordinate(mouse_vec2);
        let drag_state_str = match &self.drag_state {
            DragState::None => "DragState: None".to_string(),
            DragState::Started {
                piece: _,
                origin: _,
            } => "DragState: Started".to_string(),
            DragState::Dragging {
                piece: _,
                origin: _,
            } => "DragState: Dragging".to_string(),
        };
        let mouse_str = format!("Mouse: ({:.1}, {:.1})", mouse_vec2.x, mouse_vec2.y);
        let coord_str = match coord {
            Some(c) => format!("Coord: ({}, {})", c.rank, c.file),
            None => "Coord: None".to_string(),
        };
        let piece_str = match coord {
            Some(c) => {
                let piece = self.squares[c.rank as usize][c.file as usize].piece;
                match piece {
                    Some(Piece { kind, colour }) => format!("Piece: {colour:?} {kind:?}"),
                    None => "Piece: None".to_string(),
                }
            }
            None => "Piece: N/A".to_string(),
        };

        let lines = [drag_state_str, mouse_str, coord_str, piece_str];
        let mut y = Y_OFFSET + 40.;
        for line in &lines {
            draw_text_ex(
                line,
                X_OFFSET - 180.,
                y,
                TextParams {
                    font: Some(font),
                    font_size: 14,
                    color: RED,
                    ..Default::default()
                },
            );
            y += 24.0; // vertical spacing between lines
        }
    }
}

impl DragStateHandling for DisplayBoard {
    fn dragstate_handler_none(&mut self, mouse_position: Vec2) {
        if is_mouse_button_down(MouseButton::Right) {
            if let Some(coordinate) = Self::mouse_pos_to_coordinate(mouse_position) {
                if let Some(piece) = self.clear_square(coordinate) {
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
        if is_mouse_button_down(MouseButton::Right) {
            // if piece is held, move into drag mode
            self.drag_state = DragState::Dragging { piece, origin };
            self.drag_mouse_position = Some(mouse_position);
        } else {
            // if mouse has been released immediately, reset everything
            self.add_to_square(origin, piece);
            self.drag_state = DragState::None;
            self.drag_mouse_position = None;
        }
    }

    fn dragstate_handler_dragging(
        &mut self,
        mouse_position: Vec2,
        piece: Piece,
        origin: BoardCoordinate,
    ) {
        self.drag_mouse_position = Some(mouse_position);
        let relevant_legal_moves =
                GameState::legal_moves_from(&self.game_state, origin.to_usize());
            self.legal_move_highlights = relevant_legal_moves
                .iter()
                .map(|m| m.end_square)
                .collect();
        if is_mouse_button_released(MouseButton::Right) {
            let target = Self::mouse_pos_to_coordinate(mouse_position).unwrap_or(origin);
            let candidate_move = Move {
                start_square: origin.to_usize(),
                end_square: target.to_usize(),
                piece_moved: piece.kind,
                captured: None,
                promotion: None,
                move_type: MoveType::Normal,
            }; // TODO all other move types need handling (capture, promotion, en passant, castling)

            if Self::is_move_legal(&candidate_move, &relevant_legal_moves) {    
                self.make_verified_move(target, piece);
                self.game_state.make_move(candidate_move);
            } else {
                self.reset_on_null_move(origin, piece)
            };
            self.legal_move_highlights = Vec::new();
            
        }
    }

    fn make_verified_move(&mut self, target: BoardCoordinate, piece: Piece) {
        self.add_to_square(target, piece);
        self.drag_state = DragState::None;
        self.drag_mouse_position = None;
    }

    fn reset_on_null_move(&mut self, origin: BoardCoordinate, piece: Piece) {
        self.add_to_square(origin, piece);
        self.drag_state = DragState::None;
        self.drag_mouse_position = None;
    }
}

impl SquareMethods for DisplayBoard {
    fn square_at_coordinate(&self, coordinate: BoardCoordinate) -> &Square {
        &self.squares[coordinate.rank as usize][coordinate.file as usize]
    }

    fn square_at_mutable(&mut self, coordinate: BoardCoordinate) -> &mut Square {
        &mut self.squares[coordinate.rank as usize][coordinate.file as usize]
    }

    fn add_to_square(&mut self, coordinate: BoardCoordinate, piece: Piece) {
        let square = self.square_at_mutable(coordinate);
        square.piece = Some(piece);
    }

    fn clear_square(&mut self, coordinate: BoardCoordinate) -> Option<Piece> {
        let square = self.square_at_mutable(coordinate);
        square.piece.take()
    }
}

impl MouseFunctions for DisplayBoard {
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

    fn coordinate_to_screen_position(coordinate: BoardCoordinate) -> Vec2 {
        Vec2::new(
            coordinate.file as f32 * SQUARE_SIZE + X_OFFSET,
            coordinate.rank as f32 * SQUARE_SIZE + Y_OFFSET,
        )
    }
}

impl HelperFunctions for DisplayBoard {
    fn is_move_legal(candidate_move: &Move, legal_moves: &[Move]) -> bool {
        legal_moves.contains(candidate_move)
    }

    fn legal_move_highlights(&self) {
        for square_index in self.legal_move_highlights.iter() {
            let rank = square_index / 8;
            let file = square_index % 8;
            let square_coords = (
                file as f32 * SQUARE_SIZE + X_OFFSET,
                (BOARD_SIZE - 1 - rank) as f32 * SQUARE_SIZE + Y_OFFSET,
            );
            draw_rectangle(
                square_coords.0, 
                square_coords.1, 
                SQUARE_SIZE, 
                SQUARE_SIZE, 
                LEGAL_MOVE_HIGHLIGHT_COLOUR);
        }
    }
    /*
    fn create_candidate_move(start: BoardCoordinate, end: BoardCoordinate, promotion_piece: Option<PieceKind>) -> Move {
        Move {
            start_square: start.to_usize(),
            end_square: end.to_usize(),
            piece_moved:
            capture:
            promotion: promotion_piece,
        }
    }
    */
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

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct BoardCoordinate {
    rank: u8,
    file: u8,
}
impl BoardCoordinate {
    fn to_usize(self) -> usize {
        (self.rank * 8 + self.file) as usize
    }

    fn from_usize(square_index: usize) -> Self {
        BoardCoordinate {
            rank: (square_index / 8) as u8,
            file: (square_index % 8) as u8,
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum SquareColour {
    Light,
    Dark,
}

#[derive(Copy, Clone, Debug)]
struct Square {
    piece: Option<Piece>,
    colour: SquareColour,
}

fn draw_square(
    rank: usize,
    file: usize,
    square_colour: SquareColour,
    square_piece: Option<Piece>,
    textures: &PieceTextures,
    font: &Font,
) {
    if rank >= BOARD_SIZE || file >= BOARD_SIZE {
        return;
    }

    let square_coords = (
        file as f32 * SQUARE_SIZE,
        (BOARD_SIZE - 1 - rank) as f32 * SQUARE_SIZE,
    );
    let text_coords = (square_coords.0 + 5., square_coords.1 + SQUARE_SIZE - 5.);

    let mut square_name = ArrayString::<2>::new();
    write!(
        &mut square_name,
        "{}{}",
        file_from_int((file + 1) as u8).expect("Illegal file index"),
        (rank + 1)
    )
    .unwrap();

    let (square_rgb, text_rgb) = match square_colour {
        SquareColour::Light => (LIGHT_COLOUR, DARK_COLOUR),
        SquareColour::Dark => (DARK_COLOUR, LIGHT_COLOUR),
    };

    draw_rectangle(
        square_coords.0 + X_OFFSET,
        square_coords.1 + Y_OFFSET,
        SQUARE_SIZE,
        SQUARE_SIZE,
        square_rgb,
    );
    draw_text_ex(
        square_name.as_str(),
        text_coords.0 + X_OFFSET,
        text_coords.1 + Y_OFFSET,
        TextParams {
            font: Some(font),
            font_size: 14,
            color: text_rgb,
            ..Default::default()
        },
    );
    if let Some(piece) = square_piece {
        draw_piece(rank, file, piece, textures)
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
        self.map.get(&(piece)).unwrap()
    }
}

fn draw_piece(rank: usize, file: usize, piece: Piece, textures: &PieceTextures) {
    let texture = textures.get(&piece);

    let x = file as f32 * SQUARE_SIZE;
    let y = (7 - rank) as f32 * SQUARE_SIZE;

    let height = texture.height();
    let width = texture.width();

    const VERTICAL_LIFT: f32 = 15.;

    draw_texture_ex(
        texture,
        x + (SQUARE_SIZE - width) / 2.0 + X_OFFSET, // centre aligned
        y + SQUARE_SIZE - height - VERTICAL_LIFT + Y_OFFSET, // bottoms aligned
        WHITE,
        DrawTextureParams {
            ..Default::default()
        },
    );
}

fn draw_piece_at_mouse_position(piece: Piece, mouse_position: Vec2, textures: &PieceTextures) {
    let texture = textures.get(&piece);

    let x = mouse_position[0] - texture.width() / 2.0;
    let y = mouse_position[1] - texture.height() / 2.0;
    let x_bounded = if x < 0. { 0.1 } else { x };
    let y_bounded = if y < 0. { 0.1 } else { y };

    draw_texture_ex(
        texture,
        x_bounded,
        y_bounded,
        WHITE,
        DrawTextureParams {
            ..Default::default()
        },
    );
}

pub fn file_from_int(n: u8) -> Option<char> {
    if (1..=8).contains(&n) {
        Some((b'a' + (n - 1)) as char)
    } else {
        None
    }
}

/* // to flesh out later
pub enum SoundEvent {
    MoveStart, // sound made
    MoveMade, // sound made
    MoveFail, // sound made
    Capture,  // sound made
    Castling, // sound made
    Check,
    Promotion,
    Checkmate,
    Draw,
}
pub struct SoundFX {
    move_start: Sound,
    move_made: Sound,
    move_fail: Sound,
    capture: Sound,
    castling: Sound,
    check: Sound,
    promotion: Sound,
    checkmate: Sound,
    draw: Sound,
}
impl SoundFX {
    async fn load(file: &str) -> Sound {
        load_sound(&format!("assets/sounds/{}.wav", file)
            .await
            .unwrap_or_else(|_| panic!("Failed to load sound {}", file))
        )
    }
}
impl SoundEffect {
    pub fn PlaySound(self) {
        match self {
            MoveStart =>
            ...
        }
    }
}
*/

pub fn kill_game() -> bool {
    is_key_pressed(KeyCode::Escape)
}

pub fn draw_framerate(font: &Font) {
    let fps = get_fps();
    let fps_text = format!("FPS: {fps}");
    draw_text_ex(
        &fps_text,
        X_OFFSET - 180., // position to the left of the board
        Y_OFFSET + 10.,  // slightly above your debug info
        TextParams {
            font: Some(font),
            font_size: 18,
            color: GREEN,
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
