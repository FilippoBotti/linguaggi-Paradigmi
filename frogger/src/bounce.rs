use std::any::Any;
use std::cmp::{min, max};

use crate::actor::*;
use crate::rand::*;


pub struct Vehicle {
    pos: Pt,
    size: Pt,
    speed: i32,
    vehicle_type: i32,
}
impl Vehicle {
    pub fn new(pos: Pt, speed: i32, vehicle_type: i32, size: Pt) -> Vehicle {
        Vehicle{pos: pos, size: size, speed: speed, vehicle_type: vehicle_type}
    }
}
impl Actor for Vehicle {
    fn act(&mut self, arena: &mut ArenaStatus) {
        let arena_w : i32 = arena.size().x;

        if self.pos.x + self.size.x < 0 {
            self.pos.x = arena_w - self.size.x;
        }
        if self.pos.x  > arena_w {
            self.pos.x = 0;
        }
        
        self.pos.x = self.pos.x + self.speed;
    }
    fn pos(&self) -> Pt { self.pos }
    fn size(&self) -> Pt { self.size }
    fn sprite(&self) -> Option<Pt> { 
        if self.speed < 0{
            match self.vehicle_type {
                0 => return Some(pt(190, 30)),
                1 => return Some(pt(225, 33)),
                2 => return Some(pt(255, 33)),
                3 => return Some(pt(285, 33)),
                4 => return Some(pt(190, 65)),
                _ => return Some(pt(190, 30)),
            }
        }
        else {
            match self.vehicle_type {
                0 => return Some(pt(190, 0)),
                1 => return Some(pt(225, 0)),
                2 => return Some(pt(255, 0)),
                3 => return Some(pt(285, 0)),
                4 => return Some(pt(257, 65)),
                _ => return Some(pt(190, 0)),
            }
        }
    }
    fn alive(&self) -> bool { true }
    fn as_any(&self) -> &dyn Any { self }
    fn speed(&self) -> i32 { self.speed }
}

pub struct FrogWinner {
    pos: Pt,
    size: Pt,
    is_reached: bool,
    wait_time_to_show_sprite: i32,
    is_animation_enabled: bool,
}
impl FrogWinner {
    pub fn new(pos: Pt) -> FrogWinner {
        FrogWinner{pos: pos, size: pt(30,30), is_reached: false, wait_time_to_show_sprite: 60, is_animation_enabled: false}
    }
}
impl Actor for FrogWinner {
    fn act(&mut self, arena: &mut ArenaStatus) {
        for other in arena.collisions() {
            if let Some(_) = other.as_any().downcast_ref::<Frog>() {
                self.is_animation_enabled = true;
            }
        }
        if self.is_animation_enabled {
            self.wait_time_to_show_sprite = max(self.wait_time_to_show_sprite - 1, 0);
        }

        if self.wait_time_to_show_sprite == 0{
            self.is_reached = true;
        }

        
    }
    fn pos(&self) -> Pt { self.pos }
    fn size(&self) -> Pt { self.size }
    fn sprite(&self) -> Option<Pt> { 
        if self.is_reached  {
            return Some(pt(95,33));
        }
        else {
            return None;
        }
    }
    fn alive(&self) -> bool { true }
    fn as_any(&self) -> &dyn Any { self }
    fn speed(&self) -> i32 { 0 }
}

pub struct Raft {
    pos: Pt,
    size: Pt,
    speed: i32
}
impl Raft {
    pub fn new(pos: Pt, speed: i32, size: Pt) -> Raft {
        Raft{pos: pos, size: size, speed: speed}
    }
}
impl Actor for Raft {
    fn act(&mut self, arena: &mut ArenaStatus) {
        let arena_w : i32 = arena.size().x;
        if self.pos.x + self.size.x < 0 {
            self.pos.x = arena_w ;
        }
        if self.pos.x > arena_w {
            self.pos.x = - self.size.x;
        }
        self.pos.x = self.pos.x + self.speed;
    }
    fn pos(&self) -> Pt { self.pos }
    fn size(&self) -> Pt { self.size }
    fn sprite(&self) -> Option<Pt> { Some(pt(190 + (100 - self.size.x),95)) }
    fn alive(&self) -> bool { true }
    fn as_any(&self) -> &dyn Any { self }
    fn speed(&self) -> i32 { self.speed }
}



pub struct Frog {
    pos: Pt,
    step: Pt,
    size: Pt,
    speed: i32,
    lives: i32,
    vehicle_collide: i32,
    water_collide: i32,
    safe_place_collide: i32,
    is_collision_enabled: bool,
    count: i32,
    steps: i32,
    dragging: i32,
    moving_direction: i32,
}
impl Frog {
    pub fn new(pos: Pt) -> Frog {
        Frog{pos: pos, step: pt(0, 0), size: pt(30, 30),
            speed: 4, lives: 3, vehicle_collide: 0, water_collide:0, safe_place_collide: 0, dragging: 0, count:0, steps: 8, moving_direction: 0, is_collision_enabled: true}
    }
    fn lives(&self) -> i32 { self.lives }
}
impl Actor for Frog {
    fn act(&mut self, arena: &mut ArenaStatus) {
        if self.is_collision_enabled {
            for other in arena.collisions() {
                if let Some(_) = other.as_any().downcast_ref::<Vehicle>() {
                    self.vehicle_collide = 60;
                    self.lives -= 1;
                    self.is_collision_enabled = false;
                }
                if let Some(_) = other.as_any().downcast_ref::<Raft>() {
                    self.dragging = other.speed();
                }
                if let Some(frog_winnger) = other.as_any().downcast_ref::<FrogWinner>() {
                    self.is_collision_enabled = false;
                    if !frog_winnger.is_reached {
                        self.safe_place_collide = 60;
                        self.pos = other.pos();
                    }
                    else {
                        self.vehicle_collide = 60;
                        self.lives -= 1;
                        //disattivo collisioni ma perde vite...
                    }
                }

            }
        }
        
        let keys = arena.current_keys();
        if self.count == 0 {
            if keys.contains(&"ArrowUp") {
                self.step.y = -self.speed;
                self.step.x = 0;
                self.count = self.steps;
                self.moving_direction = 1;
            } else if keys.contains(&"ArrowDown") {
                self.step.y = self.speed;
                self.count = self.steps;
                self.moving_direction = 2;
                self.step.x = 0;
            }
            if keys.contains(&"ArrowLeft") {
                self.step.x = -self.speed;
                self.step.y = 0;
                self.count = self.steps;
                self.moving_direction = 3;
            } else if keys.contains(&"ArrowRight") {
                self.step.x = self.speed;
                self.step.y = 0;
                self.count = self.steps;
                self.moving_direction = 4;
            }
        }
        
        if self.count > 0 && self.is_collision_enabled{
            self.count -= 1;
            self.pos = self.pos + self.step;
            
        }
        self.vehicle_collide = max(self.vehicle_collide - 1, 0);
        self.water_collide = max(self.water_collide - 1, 0);
        self.safe_place_collide = max(self.safe_place_collide - 1, 0);

        self.pos.x += self.dragging;


        if self.pos.y <= 220  && self.dragging == 0 && self.count < 4 && self.is_collision_enabled {
            self.water_collide = 60;
            self.lives -=1;
            self.is_collision_enabled = false;
            
        }

        self.dragging = 0;

        if !self.is_collision_enabled {
            self.count = 0;
        }

        let scr = arena.size() - self.size;
        self.pos.x = min(max(self.pos.x, 0), scr.x);  // clamp
        self.pos.y = min(max(self.pos.y, 0), scr.y);  // clamp
        
        if !self.is_collision_enabled && (self.vehicle_collide == 0 && self.water_collide == 0 && self.safe_place_collide == 0) {
            self.pos = pt(arena.size().x/2-self.size.x/2, arena.size().y-33);
            self.is_collision_enabled = true;
        }

    }
    fn pos(&self) -> Pt { self.pos }
    fn size(&self) -> Pt { self.size }
    fn sprite(&self) -> Option<Pt> {
        if self.vehicle_collide != 0{
            match self.vehicle_collide {
                41..=60 => return Some(pt(225, 195)),
                21..=40 => return Some(pt(255, 195)),
                _ => return Some(pt(2, 193)),
            }
        }
        if self.water_collide != 0{
            match self.water_collide {
                41..=60 => return Some(pt(100, 193)),
                21..=40 => return Some(pt(130, 193)),
                _ => return Some(pt(2, 193)),
            }
        }
        if self.safe_place_collide != 0 {
            match self.safe_place_collide {
                41..=60 => return Some(pt(65, 0)),
                21..=40 => return Some(pt(160,2)),
                _ => return Some(pt(95,33)),
            }
        }
        if self.count > 0 && self.is_collision_enabled {
            match self.moving_direction {
                1 => {
                    if self.count < 3 {
                        return Some(pt(30,0));
                    }
                    return Some(pt(0,0));
                }
                2 => {
                    if self.count < 3 {
                        return Some(pt(130,30));
                    }
                    return Some(pt(160, 30));
                }
                3 => {
                    if self.count > 3 {
                        return Some(pt(130,2))
                    }
                    return Some(pt(100,2));
                }
                4 => {
                    if self.count > 3 {
                        return Some(pt(30,33));
                    }
                    return Some(pt(65,33));
                }
                _ => {
                    Some(pt(65, 0))
                }
            }
        }
        else {
            match self.moving_direction {
                1 => {
                    return Some(pt(65, 0));
                }
                2 => {
                    return Some(pt(95,33));
                }
                3 => {
                    return Some(pt(160,2));
                }
                4 => {
                    return Some(pt(0,33));
                }
                _ => {
                    Some(pt(65, 0))
                }
            }
        }
    }
    fn alive(&self) -> bool { self.lives > 0 }
    fn as_any(&self) -> &dyn Any { self }
    fn speed(&self) -> i32 { self.speed }
}


pub struct BounceGame {
    arena: Arena,
    playtime: i32
}
impl BounceGame {
    // fn randpt(size: Pt) -> Pt {
    //     let mut p = pt(randint(0, size.x), randint(0, size.y));
    //     while (p.x - size.x / 2).pow(2) + (p.y - size.y / 2).pow(2) < 10000 {
    //         p = pt(randint(0, size.x), randint(0, size.y));
    //     }
    //     return p;
    // }
    pub fn new(size: Pt, n_vehicles: i32, n_rafts: i32) -> BounceGame {
        let mut arena = Arena::new(size);
        let frog_pos = arena.size().y-33;
        let mut pos= frog_pos;
        let mut vehicle_size = pt(30,30);

        for vehicle_index in 0..n_vehicles {
            pos-=32;
            let speed;
            if vehicle_index % 2 == 0 {
                speed = 4;
            }
            else {
                speed = -4;
            }
            let position : Pt = pt(randint(0, arena.size().x-1),pos);
            if vehicle_index == n_vehicles-1 {
                vehicle_size = pt(65,30);
            }
            arena.spawn(Box::new(FrogWinner::new(pt(50,35))));
            arena.spawn(Box::new(FrogWinner::new(pt(177,35))));
            arena.spawn(Box::new(FrogWinner::new(pt(306,35))));
            arena.spawn(Box::new(FrogWinner::new(pt(433,35))));
            arena.spawn(Box::new(FrogWinner::new(pt(560,35))));
            arena.spawn(Box::new(Vehicle::new(position, speed, vehicle_index, vehicle_size)));
            //arena.spawn(Box::new(Vehicle::new(pt(position.x+40,position.y), speed, vehicle_index, vehicle_size)));
        }
        pos = 222;
        for raft_index in 0..n_rafts {
            pos-=31;
            let speed;
            if raft_index % 2 == 1 {
                speed = 4;
            }
            else {
                speed = -4;
            }
            let position : Pt = pt(randint(0, arena.size().x-1),pos);
            arena.spawn(Box::new(Raft::new(position,speed, pt(100, 27))));
        }
        arena.spawn(Box::new(Frog::new(pt(arena.size().x/2 - 15, frog_pos))));

        BounceGame{arena: arena, playtime: 120}
    }
    pub fn game_over(&self) -> bool { self.remaining_lives() <= 0 }
    pub fn game_won(&self) -> bool { self.remaining_time() <= 0 }
    pub fn remaining_time(&self) -> i32 {
        self.playtime - self.arena.count() / 30
    }
    pub fn remaining_lives(&self) -> i32 {
        let mut lives = 0;
        let actors = self.actors();
        for b in actors{
            if let Some(hero) = b.as_any().downcast_ref::<Frog>() {
                lives = hero.lives();
            }
        }
        lives
    }
    pub fn tick(&mut self, keys: String) { self.arena.tick(keys); }
    pub fn size(&self) -> Pt { self.arena.size() }
    pub fn actors(&self) -> &Vec<Box<dyn Actor>> { self.arena.actors() }
}
