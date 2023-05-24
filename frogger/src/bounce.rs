use std::any::Any;
use std::cmp::{min, max};

use crate::actor::*;
use crate::rand::*;


pub struct Vehicle {
    pos: Pt,
    step: i32,
    size: Pt,
    speed: i32
}
impl Vehicle {
    pub fn new(pos: Pt, speed: i32) -> Vehicle {
        Vehicle{pos: pos, step: 4, size: pt(30, 30), speed: speed}
    }
}
impl Actor for Vehicle {
    fn act(&mut self, arena: &mut ArenaStatus) {
        // for other in arena.collisions() {
        //     if let Some(_) = other.as_any().downcast_ref::<Raft>() {
        //     } else {
        //         let diff = self.pos - other.pos();
        //         self.step.x = self.speed * if diff.x > 0 { 1 } else { -1 };
        //         self.step.y = self.speed * if diff.y > 0 { 1 } else { -1 };
        //     }
        // }


        let arena_w : i32 = arena.size().x;

        if self.pos.x < 0 {
            self.pos.x = arena_w - self.size.x;
        }
        if self.pos.x + self.size.x > arena_w {
            self.pos.x = 0;
        }
        
        self.pos.x = self.pos.x + self.speed;
    }
    fn pos(&self) -> Pt { self.pos }
    fn size(&self) -> Pt { self.size }
    fn sprite(&self) -> Option<Pt> { 
        if self.speed > 0{
            return Some(pt(190, 30));
        }
        else {
            return Some(pt(190, 0));
        }
    }
    fn alive(&self) -> bool { true }
    fn as_any(&self) -> &dyn Any { self }
    fn speed(&self) -> i32 { self.speed }
}


pub struct Raft {
    pos: Pt,
    step: i32,
    size: Pt,
    speed: i32
}
impl Raft {
    pub fn new(pos: Pt, speed: i32) -> Raft {
        Raft{pos: pos, step: 4, size: pt(100, 30), speed: speed}
    }
}
impl Actor for Raft {
    fn act(&mut self, arena: &mut ArenaStatus) {
        let arena_w : i32 = arena.size().x;
        if self.pos.x < 0 {
            self.pos.x = arena_w - self.size.x;
        }
        if self.pos.x + self.size.x > arena_w {
            self.pos.x = 0;
        }
        self.pos.x = self.pos.x + self.step;
    }
    fn pos(&self) -> Pt { self.pos }
    fn size(&self) -> Pt { self.size }
    fn sprite(&self) -> Option<Pt> { Some(pt(190,95)) }
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
    blinking: i32,
    count: i32,
    steps: i32,
    dragging: i32,
    moving_direction: i32,
}
impl Frog {
    pub fn new(pos: Pt) -> Frog {
        Frog{pos: pos, step: pt(0, 0), size: pt(30, 30),
            speed: 4, lives: 3, blinking: 0, dragging: 0, count:0, steps: 8, moving_direction: 0}
    }
    fn lives(&self) -> i32 { self.lives }
}
impl Actor for Frog {
    fn act(&mut self, arena: &mut ArenaStatus) {
        if self.blinking == 0 {
            for other in arena.collisions() {
                if let Some(_) = other.as_any().downcast_ref::<Vehicle>() {
                    self.blinking = 30;
                    self.lives -= 1;
                    self.pos = pt(arena.size().x/2-self.size.x/2, arena.size().y-33);
                }
                if let Some(_) = other.as_any().downcast_ref::<Raft>() {
                    self.dragging = other.speed();
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
        
        if self.count > 0 && self.blinking == 0{
            self.count -= 1;
            self.pos = self.pos + self.step;
        }
        self.blinking = max(self.blinking - 1, 0);
        self.pos.x += self.dragging;
        if self.pos.y <= 220 && self.pos.y >= 60 && self.dragging == 0 && self.count < 4{
            self.pos = pt(arena.size().x/2-self.size.x/2, arena.size().y-33);
            self.blinking = 30;
            self.lives -=1;
        }
        self.dragging = 0;
        if self.blinking > 0 {
            self.count = 0;
        }
        let scr = arena.size() - self.size;
        self.pos.x = min(max(self.pos.x, 0), scr.x);  // clamp
        self.pos.y = min(max(self.pos.y, 0), scr.y);  // clamp
        

    }
    fn pos(&self) -> Pt { self.pos }
    fn size(&self) -> Pt { self.size }
    fn sprite(&self) -> Option<Pt> {
        if self.blinking != 0{
            if self.blinking % 2 == 0 {
                return Some(pt(65, 0));
            } else {
                return None;
            }
        }
        if self.count > 0 && self.blinking == 0{
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
                    return Some(pt(65, 0))
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
    fn randpt(size: Pt) -> Pt {
        let mut p = pt(randint(0, size.x), randint(0, size.y));
        while (p.x - size.x / 2).pow(2) + (p.y - size.y / 2).pow(2) < 10000 {
            p = pt(randint(0, size.x), randint(0, size.y));
        }
        return p;
    }
    pub fn new(size: Pt, n_vehicles: i32, n_rafts: i32) -> BounceGame {
        let mut arena = Arena::new(size);
        let mut pos=arena.size().y-33;
        arena.spawn(Box::new(Frog::new(pt(arena.size().x/2 - 15, pos))));
        for vehicle_index in 0..n_vehicles {
            pos-=32;
            let speed;
            if vehicle_index % 2 == 1 {
                speed = 4;
            }
            else {
                speed = -4;
            }
            let position : Pt = pt(randint(0, arena.size().x-1),pos);
            arena.spawn(Box::new(Vehicle::new(position, speed)));
        }
        for _ in 0..n_rafts {
            arena.spawn(Box::new(Raft::new(pt(0,190),4)));
        }
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
        if let Some(b) = actors.first() {
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
