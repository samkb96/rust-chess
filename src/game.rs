use crate::mechanics::*;
use arrayvec::ArrayString;
use core::fmt::Write;
use macroquad::prelude::*;
use std::collections::HashMap;
use std::cmp::max;

pub const WINDOW_SIZE: f32 = SQUARE_SIZE * 9.;
const SQUARE_SIZE: f32 = 100.;
const BOARD_SIZE: usize = 8;
const DARK_COLOUR: Color = color_u8!(118, 150, 86, 255);
const LIGHT_COLOUR: Color = color_u8!(238, 238, 210, 255);
const SQUARE_COLOURS: [SquareColour; 2] = [SquareColour::Dark, SquareColour::Light];

pub const STARTING_POSITION_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

/// DisplayBoard
///
/// # Methods
///
/// - from_bitboards: [ `&BitBoards` ] -> [ `Self` ]
/// - draw_to_screen: [ `&self, &PieceTexture, &Font` ]
/// - update: [ `&mut self` ]
/// - square_at_coordinate: [ `&self, BoardCoordinate` ] -> [ `&Square` ]
/// - square_at_mutable: [ `&mut self, BoardCoordinate` ] -> [ `&mut Square` ]
/// - add_to_square: [ `&mut self, BoardCoordinate` ]
/// - clear_square: [ `&mut self, BoardCoordinate` ] -> [ `Option(Piece)` ]
/// 
/// # Associated functions
/// 
/// - mouse_pos_to_coordinate: [ `Vec2` -> `BoardCoordinate` ]
/// - coordinate_to_screen_position: [ `BoardCoordinate` -> `Vec2` ]
pub struct DisplayBoard {
    /// squares: Vec<Vec<Square; 8>;8>
    squares: [[Square; 8]; 8],
    drag_state: DragState,
    drag_mouse_position: Option<Vec2>,
}
impl DisplayBoard {
    /// **Creates a DisplayBoard from bitboards**
    pub fn from_bitboards(bitboards: &BitBoards) -> Self {
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
                squares[rank][file].piece = BitBoards::piece_at_square(bitboards, square_index)
            }
        }

        DisplayBoard {
            squares,
            drag_state: DragState::None,
            drag_mouse_position: None,
        }
    }

    /// **Draws the DisplayBoard to the screen**
    pub fn draw_to_screen(&self, texture: &PieceTextures, font: &Font) {

        for idx in 0..64 {
            let rank = idx / 8;
            let file = idx % 8;
            if let Some(square) = self.squares.get(rank).and_then(|r| r.get(file)) {
                draw_square(rank, file, square.colour, square.piece, texture, font)
            }
        }

        if let Some(mouse_position) = self.drag_mouse_position {
            if let DragState::Started { piece, .. } | DragState::Dragging { piece, .. } = self.drag_state {
                draw_piece_at_mouse_position(piece, mouse_position, texture)
            }
        }
    }

    pub fn update(&mut self) {
        let mouse_position: Vec2 = mouse_position().into();
        
        match self.drag_state {
            DragState::None => self.dragstate_handler_none(mouse_position),
            DragState::Started { piece, origin} => self.dragstate_handler_started(mouse_position, piece, origin),
            DragState::Dragging { piece, origin } => self.dragstate_handler_dragging(mouse_position, piece, origin),
            }
        }
    

    fn dragstate_handler_none(&mut self, mouse_position: Vec2) {
        // get piece on clicked square, mark as dragging with initial mouse location. only log piece and origin once
        if is_mouse_button_down(MouseButton::Left) {
            if let Some(coordinate) = Self::mouse_pos_to_coordinate(mouse_position) {
                if let Some(piece) = self.clear_square(coordinate) {
                    self.drag_state = DragState::Started{ piece, origin: coordinate };
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
            self.add_to_square(origin, piece);
            self.drag_state = DragState::None;
            self.drag_mouse_position = None;
        }
    }
    
    fn dragstate_handler_dragging(&mut self, mouse_position: Vec2, piece: Piece, origin: BoardCoordinate) {
        self.drag_mouse_position = Some(mouse_position);
        if is_mouse_button_released(MouseButton::Left) {
            let target_coordinate = Self::mouse_pos_to_coordinate(mouse_position).unwrap_or(origin);
            self.add_to_square(target_coordinate, piece);
            self.drag_state = DragState::None;
            self.drag_mouse_position = None;
        }
    }
    
    /*
    fn square_at_coordinate(&self, coordinate: BoardCoordinate) -> &Square {
        &self.squares[coordinate.rank as usize][coordinate.file as usize]
    }
    */

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

    fn mouse_pos_to_coordinate(mouse: Vec2) -> Option<BoardCoordinate> {
        let file = (mouse.x / SQUARE_SIZE).floor() as i32;
        let rank = 7 - (mouse.y / SQUARE_SIZE).floor() as i32;

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
            coordinate.file as f32 * SQUARE_SIZE,
            coordinate.rank as f32 * SQUARE_SIZE,
        )
    }
}

#[derive(Copy, Clone)]
enum DragState {
    None,
    Started { piece: Piece, origin: BoardCoordinate },
    Dragging { piece: Piece, origin: BoardCoordinate },
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
struct BoardCoordinate {
    rank: u8,
    file: u8,
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum SquareColour {
    Light,
    Dark,
}

#[derive(Copy, Clone)]
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
        square_coords.0,
        square_coords.1,
        SQUARE_SIZE,
        SQUARE_SIZE,
        square_rgb,
    );
    draw_text_ex(
        square_name.as_str(),
        text_coords.0,
        text_coords.1,
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
        .unwrap_or_else(|_| panic!("failed to load texture {}", path));
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
                let filename = format!("images/{}.png", image_name.as_str());
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
        x + (SQUARE_SIZE - width) / 2.0,          // centre aligned
        y + SQUARE_SIZE - height - VERTICAL_LIFT, // bottoms aligned
        WHITE,
        DrawTextureParams {
            ..Default::default()
        },
    );
}

fn draw_piece_at_mouse_position(piece: Piece, mouse_position: Vec2, textures: &PieceTextures) {
    let texture = textures.get(&piece);
    
    let x = mouse_position[0] - texture.width() / 2.0;
    let y= mouse_position[1] - texture.height() / 2.0;
    let x_bounded = if x < 0. {0.1} else {x};
    let y_bounded = if y < 0. {0.1} else {y};

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

fn file_from_int(n: u8) -> Option<char> {
    if (1..=8).contains(&n) {
        Some((b'a' + (n - 1)) as char)
    } else {
        None
    }
}

