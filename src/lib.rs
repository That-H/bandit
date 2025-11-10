//! Library for handling traditional roguelike games.

#[cfg(target_os = "windows")]
use crossterm::{cursor, execute, queue, terminal};
use std::collections::{BinaryHeap, HashMap, VecDeque};
use std::io::{self, Write};
use std::{cmp, fmt, ops};

pub use point::Point;

/// Trait for types that are able to be used as tiles in a [Map].
/// By implementing this trait for a type you are asserting that its
/// display implementation never outputs a newline character or more
/// than one character at a time, and that the default value represents
/// an empty tile to be used where there is no tile in the map.
pub trait Tile: fmt::Display + Default {}

/// Trait for types that are able to be used as visual effects in a [Map].
pub trait Vfx {
	type Txt: fmt::Display;
	
    /// Update the visual effect. Returns true
    /// if it should be deleted, otherwise returns false.
    fn update(&mut self) -> bool;
	
	/// Takes as input the character at the position of the effect
	/// and returns a new message.
	fn modify_txt(&self, txt: &str) -> Self::Txt;
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
    fn update(&self, cmd: &mut Commands<'_, Self>, pos: Point)
    where
        Self: Sized;

    /// Returns a value used for determining update order; a higher value means
    /// the entity should update sooner than other entities. A priority of 0
	/// means the entity should not be updated.
    fn priority(&self) -> u32;
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
type MessageLaterTile<E> = Box<dyn Fn(&<E as Entity>::Tile) -> <E as Entity>::Msg>;
type MessageLaterEnt<E> = Box<dyn Fn(&E) -> <E as Entity>::Msg>;

#[derive(Clone, Copy, Default)]
enum Pos {
    Other(Point),
	#[default]
    This,
}

#[derive(Default)]
enum CmdInner<E: Entity> {
    ModTile(EditTile<E::Tile>),
    ModEnt(EditEntity<E>),
    DelTile,
    DelEnt,
    CreateTile(E::Tile),
    CreateVfx(E::Vfx),
    CreateEnt(E),
    MoveTo(Point),
    Disp(Point),
    Message(E::Msg),
	MessageLaterTile(MessageLaterTile<E>),
	MessageLaterEnt(MessageLaterEnt<E>),
    ClearRow,
	#[default]
    Null,
}

/// Something to do to a [Map].
///
/// Do note that modification, creation and deletion of tiles are mutually
/// exclusive; trying to modify a tile will remove the tile to
/// be created from the Cmd, and vice versa.
pub struct Cmd<E: Entity> {
    pos: Pos,
	get_pos: Point,
    action: CmdInner<E>,
}

impl<E: Entity> Cmd<E> {
    /// Starts building a Cmd at the given pos.
    pub fn new_on(pos: Point) -> Self {
        Self {
            pos: Pos::Other(pos),
			..Default::default()
        }
    }

    /// Starts building a Cmd at this position, i.e. the position
    /// of the current entity.
    pub fn new_here() -> Self {
        Self::default()
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
	
	/// Computes the message to display using the provided closure given
	/// the tile at get_pos.
    pub fn display_message_later_ent(self, msg_later: MessageLaterEnt<E>, get_pos: Point) -> Self {
        Self {
            action: CmdInner::MessageLaterEnt(msg_later),
			get_pos,
            ..self
        }
    }
	
	/// Computes the message to display using the provided closure given
	/// the entity at the position of the cmd.
    pub fn display_message_later_tile(self, msg_later: MessageLaterTile<E>, get_pos: Point) -> Self {
        Self {
            action: CmdInner::MessageLaterTile(msg_later),
			get_pos,
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
    pub fn move_to(self, pos: Point) -> Self {
        Self {
            action: CmdInner::MoveTo(pos),
            ..self
        }
    }

    /// Sets the vector by which to displace the entity.
    pub fn displace(self, disp: Point) -> Self {
        Self {
            action: CmdInner::Disp(disp),
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

impl<E: Entity> Default for Cmd<E> {
	fn default() -> Self {
		Self {
			pos: Pos::default(),
			get_pos: Point::ORIGIN,
			action: CmdInner::default(),
		}
	}
}

/// Contains all tiles in the game grid.
pub struct Map<E: Entity> {
    /// Width of the grid in tiles.
    pub wid: usize,
    /// Height of the grid in tiles.
    pub hgt: usize,
    map: HashMap<Point, E::Tile>,
    vfx: HashMap<Point, E::Vfx>,
    entities: HashMap<Point, E>,
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
    pub fn insert_tile(&mut self, tile: E::Tile, pos: Point) {
        self.map.insert(pos, tile);
    }

    /// Inserts the given entity into the map.
    pub fn insert_entity(&mut self, ent: E, pos: Point) {
        self.entities.insert(pos, ent);
    }

    /// Get the tile at the given position.
    #[inline]
    pub fn get_map(&self, pos: Point) -> Option<&E::Tile> {
        self.map.get(&pos)
    }

    #[inline]
    fn get_effect(&self, pos: Point) -> Option<&E::Vfx> {
        self.vfx.get(&pos)
    }

    /// Get the entity at the given position.
    #[inline]
    pub fn get_ent(&self, pos: Point) -> Option<&E> {
        self.entities.get(&pos)
    }

    /// Return all entities in the map with positions in an
    /// arbitrary order.
    pub fn get_entities(&self) -> impl Iterator<Item = (&Point, &E)> {
        self.entities.iter()
    }
	
	/// Returns the entity (and co-ordinates) with the highest priority.
	/// Will not return anything if there are no entities with a priority
	/// above 0.
	pub fn get_highest_priority(&self) -> Option<(&Point, &E)> {
		self.entities.iter().filter(|(_k, e)| e.priority() > 0).max_by_key(|(_k, e)| e.priority())
	}
	
    /// Updates the highest priority entity. Returns whether or not any 
	/// entities were updated. Entities will not be updated if there aren't
	/// any with a priority above 0.
    pub fn update(&mut self) -> bool {
		if let Some((ek, ent)) = self.get_highest_priority() { 
			let mut comms = Commands::new(self);
			
			// Convenience.
			let mut ek = *ek;
			
			// Get and apply changes.
			ent.update(&mut comms, ek);
			for cmd in comms.cmds {
				let pos = match cmd.pos {
					Pos::Other(pos) => pos,
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
						f(self.entities.get_mut(&pos).expect("No entity?"));
					}
					CmdInner::DelTile => {
						self.map.remove(&pos);
					}
					CmdInner::DelEnt => {
						self.entities.remove(&pos);
					}
					CmdInner::MoveTo(pos) => { 
						new_pos = Some(pos);
						ek = pos;
					},
					CmdInner::Disp(disp) => new_pos = Some(ek + disp),
					CmdInner::Message(msg) => {
						let _ = execute!(
							io::stdout(),
							cursor::MoveTo(pos.x as u16, pos.y as u16),
							crossterm::style::Print(msg)
						);
					}
					CmdInner::MessageLaterEnt(msg_later) => {
						let _ = execute!(
							io::stdout(),
							cursor::MoveTo(pos.x as u16, pos.y as u16),
							crossterm::style::Print(msg_later(self.get_ent(cmd.get_pos).expect("No entity?")))
						);
					}
					CmdInner::MessageLaterTile(msg_later) => {
						let _ = execute!(
							io::stdout(),
							cursor::MoveTo(pos.x as u16, pos.y as u16),
							crossterm::style::Print(msg_later(self.get_map(cmd.get_pos).expect("No tile?")))
						);
					}
					CmdInner::ClearRow => {
						let _ = execute!(
							io::stdout(),
							cursor::MoveTo(0, pos.y as u16),
							terminal::Clear(terminal::ClearType::CurrentLine)
						);
					}
					CmdInner::Null => (),
				}

				if let Some(new_pos) = new_pos {
					// Take it out of the old place and put it in the new place if necessary.
					let rem = self.entities.remove(&pos).unwrap();

					self.entities.insert(new_pos, rem);
				}
			}
			true
        } else {
			false
		}
    }
	
	/// Update all visual effects currently in the map, and remove any 
	/// that return true when updated. Return the number of vfx left.
	pub fn update_vfx(&mut self) -> usize {
		let mut dead = Vec::new();

		for (p, vf) in self.vfx.iter_mut() {
			// Update and make a note of those that no longer need to be.
			if vf.update() {
				dead.push(*p);
			}
		}

		for d in dead {
			self.vfx.remove(&d);
		}
		
		self.vfx.len()
	}
	
    /// Uses the A* algorithm to find the shortest path from the start to
    /// the goal. Will not find a path if there isn't one with a length lower
    /// than dist limit. Uses the walkable predicate to decide whether or not a
	/// a tile is allowed to be part of the path.
    pub fn pathfind<Ti: Fn(Point) -> bool>(
        &self,
        start: Point,
        goals: impl IntoIterator<Item = Point>,
        dist_lim: usize,
        walkable: Ti,
		neighbours: &[Point],
    ) -> Option<Vec<Point>> {
		let goals: Vec<Point> = goals.into_iter().collect();
		
		if goals.len() == 0 {
			return None;
		}
		
        // Use the manhattan distance as the heuristic function.
        let h = |p: Point| goals.iter().map(|pos| pos.manhattan_dist(p)).min().unwrap();

        let mut open_set = BinaryHeap::new();
        open_set.push(PathItem { pos: start, f_score: 0 });

        let mut came_from = HashMap::new();

        let mut g_score = HashMap::new();
        g_score.insert(start, 0);

        let mut f_score: HashMap<Point, usize> = HashMap::new();
        f_score.insert(start, 0);

        while let Some(cur) = open_set.pop() {
            for neighbour in neighbours.iter() {
				let mut cur_pos = cur.pos;
				let cost = (neighbour.x.abs() + neighbour.y.abs()) as usize;
				let neighbour = *neighbour + cur_pos;	
				
				let win = goals.contains(&neighbour);
				
				// If you can't go there, don't.
				if !walkable(neighbour) {
					continue;
                }

                let tentative = g_score.get(&cur_pos).copied().unwrap_or(dist_lim) + cost;
				
				if tentative > dist_lim {
					continue;
				}
				
                if tentative < g_score.get(&neighbour).copied().unwrap_or(dist_lim) {
                    came_from.insert(neighbour, cur_pos);

                    g_score.insert(neighbour, tentative);
					// Ok to cast as manhattan_dist is always positive.
					let this_f = tentative + h(neighbour) as usize;
                    f_score.insert(neighbour, this_f);
					open_set.push(PathItem { pos: neighbour, f_score: this_f });
                }
				
				if win {
					cur_pos = neighbour;
					
					let mut total_path = vec![cur_pos];
					while came_from.contains_key(&cur_pos) {
						cur_pos = came_from[&cur_pos];
						total_path.insert(0, cur_pos);
					}
					return Some(total_path);
				}
            }
        }

        None
    }
	
	/// Display this map using a window with the given settings.
	pub fn display_with(&self, settings: WindowSettings) {
		let win = Window::new(self, settings);
		
		win.display();
	}
}

#[derive(Clone, Copy, Eq)]
struct PathItem {
	pos: Point,
	f_score: usize,
}

impl PartialEq for PathItem {
	fn eq(&self, other: &Self) -> bool {
		self.f_score == other.f_score
	}
}

impl Ord for PathItem {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        other.f_score.cmp(&self.f_score)
    }
}

impl PartialOrd for PathItem {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

/// A window into a map, so that only part of it has to be
/// displayed at once. Do note that the origin is also the
/// top left corner of the map.
#[derive(Clone, Copy)]
pub struct WindowSettings {
    /// Position of the top left corner of the window.
    pub top_left: Point,
    /// Width of the window.
    pub wid: u16,
    /// Height of the window.
    pub hgt: u16,
    /// Number of cells by which the window is offset to the right.
    pub x_offset: u16,
    /// Number of cells by which the window is offset downwards.
    pub y_offset: u16,
}

impl WindowSettings {
    /// Create a new template with the given dimensions.
    pub fn new(
        top_left: Point,
        wid: u16,
        hgt: u16,
        x_offset: u16,
        y_offset: u16,
    ) -> Self {
        Self {
            top_left,
            wid,
            hgt,
            x_offset,
            y_offset,
        }
    }
	
	/// Relocates the window's top left corner to the given position.
	pub fn move_to(&mut self, pos: Point) {
		self.top_left = pos;
	}
	
	/// Centres the window on the given position.
	pub fn centre_on(&mut self, pos: Point) {
		self.top_left = pos - Point::new(self.wid as i32 / 2, -(self.hgt as i32) / 2);
	}
}

/// A window into a map, so that only part of it has to be
/// displayed at once. Do note that the origin is also the
/// top left corner of the map.
struct Window<'a, E: Entity> {
    top_left: Point,
    wid: u16,
    hgt: u16,
    x_offset: u16,
    y_offset: u16,
    inner: &'a Map<E>,
}

#[allow(unused_must_use)]
impl<'a, E: Entity> Window<'a, E> {
    /// Create a new window into the given map with the given dimensions.
    fn new(inner: &'a Map<E>, settings: WindowSettings) -> Window<'a, E> {
        let WindowSettings {
            top_left,
            wid,
            hgt,
            x_offset,
            y_offset,
        } = settings;
        Self {
            inner,
            top_left,
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
        let def = E::Tile::default().to_string();
        let spaces = String::from_utf8(vec![b' '; self.x_offset.into()]).unwrap();
		
        for y in (self.top_left.y - self.hgt as i32..self.top_left.y).rev() {
            write!(f, "{spaces}")?;
            for x in self.top_left.x..self.top_left.x + self.wid as i32 {
				let pos = Point::from((x, y));
				let txt = match self.get_ent(pos) {
					Some(e) => e.to_string(),
					None => match self.get_map(pos) {
						Some(t) => t.to_string(),
						None => def.clone(),
					},
				};
                match self.get_effect(pos) {
                    Some(v) => write!(f, "{}", v.modify_txt(&txt))?,
                    None => write!(f, "{txt}")?,
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
