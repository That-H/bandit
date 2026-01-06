//! Library for handling traditional roguelike games.

use std::collections::{BinaryHeap, HashMap, VecDeque};
use std::{cmp, fmt, ops};

pub use point::Point;

#[cfg(feature = "windowed")]
pub use windowed;

/// Trait for types that are able to be used as tiles in a [Map].
/// By implementing this trait for a type you are asserting that its
/// display implementation never outputs a newline character or more
/// than one character at a time, and that the default value represents
/// an empty tile to be used where there is no tile in the map.
pub trait Tile: Default {
    /// Type used to display this Tile.
    type Repr: fmt::Display + Clone;

    /// Create a [Display]able representation of this tile.
    fn repr(&self) -> Self::Repr;
}

/// Trait for types that are able to be used as visual effects in a [Map].
pub trait Vfx {
    /// Type used to display this effect.
    type Txt: fmt::Display;

    /// Update the visual effect. Returns true
    /// if it should be deleted, otherwise returns false.
    fn update(&mut self) -> bool;

    /// Takes as input the representation at the position of the effect
    /// and returns a new message.
    fn modify_txt(&self, txt: &Self::Txt) -> Self::Txt;
}

/// Types that actively do something in a [Map] instead of just being
/// like a tile.
pub trait Entity {
    /// The Tile type associated with this entity.
    type Tile: Tile;
    /// The visual effect type associated with this entity.
    type Vfx: Vfx<Txt = <<Self as Entity>::Tile as Tile>::Repr>;

    /// Create a [Display]able representation of this entity.
    fn repr(&self) -> <<Self as Entity>::Tile as Tile>::Repr;

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

    /// Queue all the commands from the iterator.
    pub fn queue_many<I: IntoIterator<Item = Cmd<E>>>(&mut self, cmd_iter: I) {
        self.cmds.extend(cmd_iter);
    }
}

type EditTile<T> = Box<dyn Fn(&mut T)>;
type EditEntity<E> = Box<dyn Fn(&mut E)>;

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
            action: CmdInner::default(),
        }
    }
}

/// Contains all tiles in the game grid.
#[derive(Clone)]
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

    /// Get the tile at the given position mutably.
    #[inline]
    pub fn get_map_mut(&mut self, pos: Point) -> Option<&mut E::Tile> {
        self.map.get_mut(&pos)
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

    /// Get the entity at the given position mutably.
    #[inline]
    pub fn get_ent_mut(&mut self, pos: Point) -> Option<&mut E> {
        self.entities.get_mut(&pos)
    }

    /// Return all entities in the map with positions in an
    /// arbitrary order.
    pub fn get_entities(&self) -> impl Iterator<Item = (&Point, &E)> {
        self.entities.iter()
    }

    /// Returns the entity (and co-ordinates) with the highest priority.
    /// Will not return anything if there are no entities with a priority
    /// above 0. Prefers entities closer to the top left if there is a tie with priorities.
    pub fn get_highest_priority(&self) -> Option<(&Point, &E)> {
        self.entities
            .iter()
            .filter(|(_k, e)| e.priority() > 0)
            .max_by(|(k, e), (k2, e2)| {
                e.priority()
                    .cmp(&e2.priority())
                    .then_with(|| k2.x.cmp(&k.x))
                    .then_with(|| k.y.cmp(&k2.y))
            })
    }

    /// Updates the highest priority entity. Returns whether or not any
    /// entities were updated. Entities will not be updated if there aren't
    /// any with a priority above 0.
    pub fn update(&mut self) -> bool {
        match self.get_highest_priority() {
            Some((p, _e)) => self.force_update(*p),
            None => false,
        }
    }

    /// Updates the entity at the given position, regardless of its priority.
    pub fn force_update(&mut self, mut e_pos: Point) -> bool {
        if let Some(ent) = self.get_ent(e_pos) {
            let mut comms = Commands::new(self);

            // Get and apply changes.
            ent.update(&mut comms, e_pos);
            for cmd in comms.cmds {
                let pos = match cmd.pos {
                    Pos::Other(pos) => pos,
                    Pos::This => e_pos,
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
                    CmdInner::MoveTo(to) => {
                        new_pos = Some(to);
                        if pos == e_pos {
                            e_pos = to;
                        }
                    }
                    CmdInner::Disp(disp) => new_pos = Some(e_pos + disp),
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

        if goals.is_empty() || neighbours.is_empty() {
            return None;
        }
        let min_move_dist = neighbours
            .iter()
            .map(|p| Point::ORIGIN.manhattan_dist(*p))
            .max()
            .unwrap();

        // Use the manhattan distance as the heuristic function. Slightly modified to account for
        // moves having an additional cost of 1.
        let h = |p: Point| {
            let min_dist = goals.iter().map(|pos| pos.manhattan_dist(p)).min().unwrap();
            min_dist + min_dist / min_move_dist
        };

        let mut open_set = BinaryHeap::new();
        open_set.push(PathItem {
            pos: start,
            f_score: 0,
        });

        let mut came_from = HashMap::new();

        let mut g_score = HashMap::new();
        g_score.insert(start, 0);

        let mut f_score: HashMap<Point, usize> = HashMap::new();
        f_score.insert(start, 0);

        while let Some(cur) = open_set.pop() {
            for neighbour in neighbours.iter() {
                let mut cur_pos = cur.pos;
                let cost = (neighbour.x.abs() + neighbour.y.abs()) as usize + 1;
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
                    open_set.push(PathItem {
                        pos: neighbour,
                        f_score: this_f,
                    });
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

    /// Creates a representation of the current state of the subsection
    /// of the map defined by the provided dimensions.
    pub fn to_chars(
        &self,
        top_left: Point,
        map_wid: u32,
        map_hgt: u32,
    ) -> Vec<Vec<<<E as Entity>::Tile as Tile>::Repr>> {
        let mut out = Vec::new();
        let hgt = map_hgt as i32;
        let wid = map_wid as i32;

        let def = E::Tile::default().repr();

        for y in (top_left.y - hgt..top_left.y).rev() {
            let mut cur_out = Vec::new();
            for x in top_left.x..top_left.x + wid {
                let pos = Point::new(x, y);
                let txt = match self.get_ent(pos) {
                    Some(e) => e.repr(),
                    None => match self.get_map(pos) {
                        Some(t) => t.repr(),
                        None => def.clone(),
                    },
                };
                match self.get_effect(pos) {
                    Some(v) => cur_out.push(v.modify_txt(&txt)),
                    None => cur_out.push(txt),
                };
            }
            out.push(cur_out);
        }

        out
    }

    /// Display this map into a window. Overwrites all previous contents.
    #[cfg(feature = "windowed")]
    pub fn display_into(
        &self,
        win: &mut windowed::Window<<<E as Entity>::Tile as Tile>::Repr>,
        map_top_left: Point,
        map_wid: u32,
        map_hgt: u32,
    ) {
        let data = self.to_chars(map_top_left, map_wid, map_hgt);

        win.data = data;
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
        other
            .f_score
            .cmp(&self.f_score)
            // Ensure they never compare equal unless everything is the same,
            // in which case it wouldn't matter.
            .then_with(|| self.pos.x.cmp(&other.pos.x))
            .then_with(|| self.pos.y.cmp(&other.pos.y))
    }
}

impl PartialOrd for PathItem {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}
