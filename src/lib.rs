//! Library for handling traditional roguelike games.

#[cfg(target_os = "windows")]
use crossterm::{cursor, execute, queue, terminal};
use std::collections::{HashMap, VecDeque};
use std::io::{self, Write};
use std::{fmt, ops, thread, time};

/// Trait for types that are able to be used as tiles in a [Map].
/// By implementing this trait for a type you are asserting that its
/// display implementation never outputs a newline character or more
/// than one character at a time, and that the default value represents
/// an empty tile to be used where there is no tile in the map.
pub trait Tile: fmt::Display + Default {}

/// Trait for types that are able to be used as visual effects in a [Map].
/// Should never print multiple characters or a newline.
pub trait Vfx: fmt::Display {
    /// Update the tile if it is a visual effect. Returns true
    /// if it should be deleted, otherwise returns false.
    fn update_vfx(&mut self) -> bool;
}

/// Types that actively do something in a [Map] instead of just being
/// like a tile.
pub trait Entity: fmt::Display {
    /// The Tile type associated with this entity.
    type Tile: Tile;
	/// The visual effect type associated with this entity.
	type Vfx: Vfx;
    /// The printable type for displaying information used by this entity.
    type Msg: fmt::Display;

    /// Queue everything that needs to change as a result of this entity
    /// after a frame. Pos is the current position of the entity in the map.
    fn update(&self, cmd: &mut Commands<'_, Self>, pos: (usize, usize))
    where
        Self: Sized;

    /// Returns a value used for determining update order; a lower value means
    /// the entity should update sooner than other entities.
    fn priority(&self) -> usize;
}

/// Allows one to queue things to do to a [Map]. Can be dereferenced into a
/// Map for reading.
pub struct Commands<'a, E: Entity> {
    inner: &'a Map<E>,
    cmds: VecDeque<Cmd<E>>,
}

impl<E: Entity> ops::Deref for Commands<'_, E> {
    type Target = Map<E>;

    fn deref(&self) -> &Self::Target {
        self.inner
    }
}

impl<'a, E: Entity> Commands<'a, E> {
    fn new(inner: &'a Map<E>) -> Commands<'a, E> {
        Self {
            inner,
            cmds: VecDeque::new(),
        }
    }

    /// Queues the command for execution.
    pub fn queue(&mut self, cmd: Cmd<E>) {
        self.cmds.push_back(cmd);
    }
}

type EditTile<T> = Box<dyn Fn(&mut T)>;
type EditEntity<E> = Box<dyn Fn(&mut E)>;

#[derive(Clone, Copy)]
enum Pos {
    Other(usize, usize),
    This,
}

enum CmdInner<E: Entity> {
    ModTile(EditTile<E::Tile>),
    ModEnt(EditEntity<E>),
    DelTile,
    DelEnt,
    CreateTile(E::Tile),
    CreateVfx(E::Vfx),
    CreateEnt(E),
    MoveTo(usize, usize),
    Disp(usize, usize),
    Message(E::Msg),
    ClearRow,
    Null,
}

/// Something to do to a [Map].
///
/// Do note that modification, creation and deletion of tiles are mutually
/// exclusive; trying to modify a tile will remove the tile to
/// be created from the Cmd, and vice versa.
pub struct Cmd<E: Entity> {
    pos: Pos,
    action: CmdInner<E>,
}

impl<E: Entity> Cmd<E> {
    /// Starts building a Cmd at the given pos.
    pub fn new_on(x: usize, y: usize) -> Self {
        Self {
            pos: Pos::Other(x, y),
            action: CmdInner::Null,
        }
    }

    /// Starts building a Cmd at this position, i.e. the position
    /// of the current entity.
    pub fn new_here() -> Self {
        Self {
            pos: Pos::This,
            action: CmdInner::Null,
        }
    }

    /// Sets the next message to display. The position the message is displayed
    /// at is the co-ordinates provided applied to the terminal window.
    /// Not recommended to display messages on the actual map.
    /// Also not automatically cleared.
    pub fn display_message(self, msg: E::Msg) -> Self {
        Self {
            action: CmdInner::Message(msg),
            ..self
        }
    }

    /// Clears all text on the row defined by the current position.
    pub fn clear_row(self) -> Self {
        Self {
            action: CmdInner::ClearRow,
            ..self
        }
    }

    /// Sets the position to move the entity to.
    pub fn move_to(self, x: usize, y: usize) -> Self {
        Self {
            action: CmdInner::MoveTo(x, y),
            ..self
        }
    }

    /// Sets the vector by which to displace the entity.
    pub fn displace(self, x: usize, y: usize) -> Self {
        Self {
            action: CmdInner::Disp(x, y),
            ..self
        }
    }

    /// Defines the function used to modify the tile.
    pub fn modify_tile(self, f: EditTile<E::Tile>) -> Self {
        Self {
            action: CmdInner::ModTile(f),
            ..self
        }
    }

    /// Defines the function used to modify the entity.
    pub fn modify_entity(self, f: EditEntity<E>) -> Self {
        Self {
            action: CmdInner::ModEnt(f),
            ..self
        }
    }

    /// Adds the tile to create.
    pub fn create_tile(self, tile: E::Tile) -> Self {
        Self {
            action: CmdInner::CreateTile(tile),
            ..self
        }
    }

    /// Adds the effect to create.
    pub fn create_effect(self, vfx: E::Vfx) -> Self {
        Self {
            action: CmdInner::CreateVfx(vfx),
            ..self
        }
    }

    /// Adds the entity to create.
    pub fn create_entity(self, ent: E) -> Self {
        Self {
            action: CmdInner::CreateEnt(ent),
            ..self
        }
    }

    /// Causes the tile at this position to be deleted.
    pub fn delete_tile(self) -> Self {
        Self {
            action: CmdInner::DelTile,
            ..self
        }
    }

    /// Causes the entity at this position to be deleted.
    pub fn delete_entity(self) -> Self {
        Self {
            action: CmdInner::DelEnt,
            ..self
        }
    }
}

/// Contains all tiles in the game grid.
pub struct Map<E: Entity> {
    /// Width of the grid in tiles.
    pub wid: usize,
    /// Height of the grid in tiles.
    pub hgt: usize,
    map: HashMap<(usize, usize), E::Tile>,
    vfx: HashMap<(usize, usize), E::Vfx>,
    entities: HashMap<(usize, usize), E>,
}

impl<E: Entity> Map<E> {
    /// Returns an empty map instance with the provided size.
    pub fn new(wid: usize, hgt: usize) -> Self {
        Self {
            wid,
            hgt,
            map: HashMap::with_capacity(wid * hgt),
            vfx: HashMap::with_capacity(wid * hgt),
            entities: HashMap::with_capacity(wid * hgt),
        }
    }

    /// Inserts the given tile into the map.
    pub fn insert_tile(&mut self, tile: E::Tile, x: usize, y: usize) {
        self.map.insert((x, y), tile);
    }

    /// Inserts the given entity into the map.
    pub fn insert_entity(&mut self, ent: E, x: usize, y: usize) {
        self.entities.insert((x, y), ent);
    }

    /// Get the tile at the given position.
    #[inline]
    pub fn get_map(&self, x: usize, y: usize) -> Option<&E::Tile> {
        self.map.get(&(x, y))
    }

    #[inline]
    fn get_effect(&self, x: usize, y: usize) -> Option<&E::Vfx> {
        self.vfx.get(&(x, y))
    }

    /// Get the entity at the given position.
    #[inline]
    pub fn get_ent(&self, x: usize, y: usize) -> Option<&E> {
        self.entities.get(&(x, y))
    }

    /// Return all entities in the map with positions in an
    /// arbitrary order.
    pub fn get_entities(&self) -> impl Iterator<Item = (&(usize, usize), &E)> {
        self.entities.iter()
    }

    /// Updates all entities. If at any point, visual effects
    /// would exist, they are repeatedly updated until that is
    /// no longer the case, with a pause of delay milliseconds
    /// between each update.
    pub fn update(&mut self, win_settings: WindowSettings, delay: u64) {
        let delay = time::Duration::from_millis(delay);

        let mut e_keys: Vec<_> = self.entities.keys().copied().collect();
        e_keys.sort_by_key(|e| self.entities[e].priority());

        for i in 0..e_keys.len() {
            let ek = e_keys[i];
            let mut comms = Commands::new(self);

            // Get and apply changes.
            self.entities[&ek].update(&mut comms, ek);
            for cmd in comms.cmds {
                let pos = match cmd.pos {
                    Pos::Other(x, y) => (x, y),
                    Pos::This => ek,
                };

                let mut new_pos = None;

                match cmd.action {
                    CmdInner::CreateTile(t) => {
                        self.map.insert(pos, t);
                    }
                    CmdInner::CreateVfx(v) => {
                        self.vfx.insert(pos, v);
                    }
                    CmdInner::CreateEnt(e) => {
                        self.entities.insert(pos, e);
                    }
                    CmdInner::ModTile(f) => {
                        f(self.map.get_mut(&pos).expect("No tile?"));
                    }
                    CmdInner::ModEnt(f) => {
                        f(self.entities.get_mut(&pos).expect("No tile?"));
                    }
                    CmdInner::DelTile => {
                        self.map.remove(&pos);
                    }
                    CmdInner::DelEnt => {
                        self.entities.remove(&pos);
                    }
                    CmdInner::MoveTo(x, y) => new_pos = Some((x, y)),
                    CmdInner::Disp(x, y) => new_pos = Some((ek.0 + x, ek.1 + y)),
                    CmdInner::Message(msg) => {
                        let _ = execute!(
                            io::stdout(),
                            cursor::MoveTo(pos.0 as u16, pos.1 as u16),
                            crossterm::style::Print(msg)
                        );
                    }
                    CmdInner::ClearRow => {
                        let _ = execute!(
                            io::stdout(),
                            cursor::MoveTo(0, pos.1 as u16),
                            terminal::Clear(terminal::ClearType::CurrentLine)
                        );
                    }
                    CmdInner::Null => (),
                }

                if let Some(new_pos) = new_pos {
                    let rem = self.entities.remove(&pos).unwrap();

                    // Check if the old_pos is in e_keys and update it if so; it would
                    // no longer be a valid key otherwise.
                    if let Some(p) = e_keys.iter().position(|elem| *elem == pos) {
                        e_keys[p] = new_pos;
                    }

                    self.entities.insert(new_pos, rem);
                }
            }

            // Repeatedly update the window until no more visual effects exist.
            loop {
                thread::sleep(delay);
                let win = Window::new(self, win_settings);
                win.display();

                if self.vfx.is_empty() {
                    break;
                } else {
                    let mut dead = Vec::new();

                    for (p, vf) in self.vfx.iter_mut() {
                        // Update and make a note of those that no longer need to be.
                        if !vf.update_vfx() {
                            dead.push(*p);
                        }
                    }

                    for d in dead {
                        self.vfx.remove(&d);
                    }
                }
            }
        }
    }
}

/// A window into a map, so that only part of it has to be
/// displayed at once. Do note that the origin is also the
/// top left corner of the map.
#[derive(Clone, Copy)]
pub struct WindowSettings {
    /// Leftmost co-ord of the window.
    pub left: usize,
    /// Upper most co-ord of the window.
    pub top: usize,
    /// Width of the window.
    pub wid: usize,
    /// Height of the window.
    pub hgt: usize,
    /// Number of cells by which the window is offset to the right.
    pub x_offset: u16,
    /// Number of cells by which the window is offset downwards.
    pub y_offset: u16,
}

impl WindowSettings {
    /// Create a new template with the given dimensions.
    pub fn new(
        left: usize,
        top: usize,
        wid: usize,
        hgt: usize,
        x_offset: u16,
        y_offset: u16,
    ) -> Self {
        Self {
            left,
            top,
            wid,
            hgt,
            x_offset,
            y_offset,
        }
    }
}

/// A window into a map, so that only part of it has to be
/// displayed at once. Do note that the origin is also the
/// top left corner of the map.
struct Window<'a, E: Entity> {
    left: usize,
    top: usize,
    wid: usize,
    hgt: usize,
    x_offset: u16,
    y_offset: u16,
    inner: &'a Map<E>,
}

#[allow(unused_must_use)]
impl<'a, E: Entity> Window<'a, E> {
    /// Create a new window into the given map with the given dimensions.
    fn new(inner: &'a Map<E>, settings: WindowSettings) -> Window<'a, E> {
        let WindowSettings {
            left,
            top,
            wid,
            hgt,
            x_offset,
            y_offset,
        } = settings;
        Self {
            inner,
            left,
            top,
            wid,
            hgt,
            x_offset,
            y_offset,
        }
    }

    /// Clears the old grid displayed in the terminal and replaces it with the
    /// current one.
    /// Consumes the window as it is no longer needed; the real grid should now
    /// be updated.
    fn display(self) {
        #[cfg(target_os = "windows")]
        {
            let mut handle = io::stdout();

            queue!(
                handle,
                cursor::Hide,
                cursor::MoveTo(0, self.y_offset),
                crossterm::style::Print(self),
            );
            handle.flush();
        }
        #[cfg(not(target_os = "windows"))]
        print!("{self}");
    }
}

impl<E: Entity> fmt::Display for Window<'_, E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let def: E::Tile = Default::default();
        let spaces = String::from_utf8(vec![b' '; self.x_offset.into()]).unwrap();

        for y in self.top..self.top + self.hgt {
            write!(f, "{spaces}")?;
            for x in self.left..self.left + self.wid {
                match self.get_effect(x, y) {
                    Some(v) => write!(f, "{v}")?,
                    None => match self.get_ent(x, y) {
                        Some(e) => write!(f, "{e}")?,
                        None => match self.get_map(x, y) {
                            Some(t) => write!(f, "{t}")?,
                            None => write!(f, "{def}")?,
                        },
                    },
                };
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

impl<E: Entity> ops::Deref for Window<'_, E> {
    type Target = Map<E>;

    fn deref(&self) -> &Self::Target {
        self.inner
    }
}
