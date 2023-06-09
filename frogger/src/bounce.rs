// BOTTI FILIPPO MATRI. 333653
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
    // movimento dei veicoli che ricompaiono da lato opposto una volta scomparsi
    fn act(&mut self, arena: &mut ArenaStatus) {
        let arena_w : i32 = arena.size().x;

        if self.pos.x + self.size.x < 0 {
            self.pos.x = arena_w;
        }
        if self.pos.x  > arena_w {
            self.pos.x = -self.size.x;
        }
        
        self.pos.x = self.pos.x + self.speed;
    }
    fn pos(&self) -> Pt { self.pos }
    fn size(&self) -> Pt { self.size }
    // sprite dei veicoli in base al tipo di veicolo e alla direzione
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

// placeholder che spawna una rana nella posizione goal raggiunta dal player (quando ce ne sono 5 l'utente vince la partita)
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
        // utilizzo dei contatori per mostrare la rana solo dopo l'animazione di vittoria 
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

// tronchi
pub struct Raft {
    pos: Pt,
    size: Pt,
    speed: i32, 
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
    // idealmente di dimensione anche inferiore rispetto alla sprite intera
    fn sprite(&self) -> Option<Pt> { Some(pt(190 + (100 - self.size.x),95)) }
    fn alive(&self) -> bool { true }
    fn as_any(&self) -> &dyn Any { self }
    fn speed(&self) -> i32 { self.speed }
}

// simili ai tronchi ma con la possibilità di andare sott'acqua
pub struct Turtle {
    pos: Pt,
    size: Pt,
    speed: i32, 
    is_under_water: bool,
    under_water_time: i32,
    animation_time: i32,
    can_go_under_water: bool
}
impl Turtle {
    pub fn new(pos: Pt, speed: i32, under_water_time: i32, can_go_under_water: bool) -> Turtle {
        Turtle{pos: pos, size: pt(30,30), speed: speed, is_under_water: false, 
            under_water_time: under_water_time, animation_time: 30, can_go_under_water: can_go_under_water}
    }
}
impl Actor for Turtle {
    fn act(&mut self, arena: &mut ArenaStatus) {
        // se può andare sott'acqua allora ogni 2 minuti si alternerà tra sopra e sotto
        if  self.under_water_time == 0 && self.can_go_under_water{
            self.is_under_water = !self.is_under_water;
            self.under_water_time = 120;
        }

        // movimento
        let arena_w : i32 = arena.size().x;
        if self.pos.x + self.size.x < 0 {
            self.pos.x = arena_w ;
        }
        if self.pos.x > arena_w {
            self.pos.x = - self.size.x;
        }
        self.pos.x = self.pos.x + self.speed;

        //contatori per andare sott'acqua e per l'animazione
        self.under_water_time = max(self.under_water_time - 1, 0);
        self.animation_time = max(self.animation_time - 1, 0);

        if self.animation_time == 0 {
            self.animation_time = 30;
        }
    }
    fn pos(&self) -> Pt { self.pos }
    fn size(&self) -> Pt { self.size }
    fn sprite(&self) -> Option<Pt> { 
        // animazione per andare sott'acqua
        if self.under_water_time != 0 {
            if self.is_under_water {
                match self.under_water_time {
                    110..=120 => return Some(pt(190, 155)),
                    100..=109 => return Some(pt(225, 160)),
                    0..=10 =>return Some(pt(190, 155)),
                    _ => return None,
                }
            }
           
        }
        // animazione base
        match self.animation_time {
            21..=30 => return Some (pt(220, 130)),
            10..=20 => return Some (pt(255, 130)),
            _ => return Some(pt(190, 130)),

        }
    }
    fn alive(&self) -> bool { true }
    fn as_any(&self) -> &dyn Any { self }
    fn speed(&self) -> i32 { self.speed }
}


// Frog: si muove attraverso i tasti
// vehicle_collide: contatore animazione collisione con veicolo
// water_collide: contatore animazione "collisione" con acqua
// safe_place_collide: contatore animazione collisione con frogwinner
// count: contatore animazione movimento tramite tasti
// is_collision_enabled: booleano che attiva/disattiva le collisioni (utile per disabilitare le collisioni durante l'animazione della morte della rana)
// frog_safe: numero di rane arrivate a destinazione
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
    frog_safe: i32,
}
impl Frog {
    pub fn new(pos: Pt) -> Frog {
        Frog{pos: pos, step: pt(0, 0), size: pt(30, 30),
            speed: 4, lives: 3, vehicle_collide: 0, water_collide:0, safe_place_collide: 0,
             dragging: 0, count:0, steps: 8, moving_direction: 0, is_collision_enabled: true,
            frog_safe: 0}
    }
    fn lives(&self) -> i32 { self.lives }
    fn is_all_frog_safe(&self) -> bool {
        return self.frog_safe == 5;
    }
    pub fn is_dead_animation(&self) -> bool { self.water_collide >0 || self.vehicle_collide>0 }

    pub fn is_win_animation(&self) -> bool { self.safe_place_collide==0 }
}
impl Actor for Frog {
    fn act(&mut self, arena: &mut ArenaStatus) {
        //se collisioni abilitate
        if self.is_collision_enabled {
            for other in arena.collisions() {
                // collisione con veicolo, perdo una vita
                if let Some(_) = other.as_any().downcast_ref::<Vehicle>() {
                    self.vehicle_collide = 60;
                    self.lives -= 1;
                    self.is_collision_enabled = false;
                }
                // collisione con tartaruga, mi aggancio ad essa se è sopra l'acqua
                if let Some(turtle) = other.as_any().downcast_ref::<Turtle>() {
                    if !turtle.is_under_water {
                        self.dragging = other.speed();
                    }
                }
                // collisione con tronco, mi aggancio ad esso
                if let Some(_) = other.as_any().downcast_ref::<Raft>() {
                    self.dragging = other.speed();
                }
                // collisione con frogwinner, se non sono ancora arrivato in quello spot allora faccio partire l'animazione
                // aumento di 1 il numero di rane salvate
                // altrimenti perdo una vita
                if let Some(frog_winnger) = other.as_any().downcast_ref::<FrogWinner>() {
                    self.is_collision_enabled = false;
                    if !frog_winnger.is_reached {
                        self.safe_place_collide = 60;
                        self.pos = other.pos();
                        self.frog_safe +=1;
                        
                    }
                    else {
                        self.vehicle_collide = 60;
                        self.lives -= 1;
                    }
                }

            }
        }
        
        // mi muovo con le freccette e mi salvo la direzione per l'animazione della sprite
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
        
        // mi muovo, animazione
        if self.count > 0 && self.is_collision_enabled{
            self.count -= 1;
            self.pos = self.pos + self.step;
            
        }
        // decremento i contatori
        self.vehicle_collide = max(self.vehicle_collide - 1, 0);
        self.water_collide = max(self.water_collide - 1, 0);
        self.safe_place_collide = max(self.safe_place_collide - 1, 0);

        //seguo il tronco/tartaruga se sono agganciato
        self.pos.x += self.dragging;

        //se cado in acqua perdo una vita
        if self.pos.y <= 250  && self.dragging == 0 && self.count < 4 && self.is_collision_enabled {
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
        
        // riabilito le collisioni 
        if !self.is_collision_enabled && (self.vehicle_collide == 0 && self.water_collide == 0 && self.safe_place_collide == 0) {
            self.pos = pt(arena.size().x/2-self.size.x/2, arena.size().y-33);
            self.is_collision_enabled = true;
        }

    }
    fn pos(&self) -> Pt { self.pos }
    fn size(&self) -> Pt { self.size }
    // gestione delle sprite a seconda dell'animazione
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
    pub fn new(size: Pt, n_vehicles: i32, n_rafts: i32) -> BounceGame {
        let mut arena = Arena::new(size);
        let frog_pos = arena.size().y-33;
        let mut pos= frog_pos;
        let mut vehicle_size = pt(30,30);

        // spawno i veicoli con le rispettive velocità e sprite, ne spawno più di 2/3 per riga a seconda della riga
        for vehicle_index in 0..n_vehicles {
            pos-=32;
            let mut speed;
            if vehicle_index % 2 == 0 {
                speed = 4;
            }
            else {
                speed = -4;
            }
            if vehicle_index == 2 {
                speed = 5;
            }
            let position : Pt = pt(randint(0, arena.size().x/3),pos);
            if vehicle_index == n_vehicles-1 {
                vehicle_size = pt(65,30);
            }
            if vehicle_index != 2 {
                arena.spawn(Box::new(Vehicle::new(pt(randint(position.x+50, arena.size().x),position.y), speed, vehicle_index, vehicle_size)));
            }
            else {
                let new_postion = randint(position.x+50, arena.size().x-120);
                arena.spawn(Box::new(Vehicle::new(pt(new_postion,position.y), speed, vehicle_index, vehicle_size)));
                arena.spawn(Box::new(Vehicle::new(pt(randint(new_postion + 70, arena.size().x)-30,position.y), speed, vehicle_index, vehicle_size)));
            }
            arena.spawn(Box::new(Vehicle::new(position, speed, vehicle_index, vehicle_size)));
        }

        // spawno tronchi e tartarughe (raggruppate di 3 in 3) a seconda della riga, solo un gruppo di tartarughe per riga potrà andare sott'acqua
        pos = 252;
        let mut pos_x : i32;
        for raft_index in 0..n_rafts {
            pos-=31;
            let speed;
            match raft_index {
                0 | 3 => {
                    pos_x = 0 + raft_index * 40;
                    for element in 0..9{
                        let position : Pt = pt(pos_x + element*30,pos);
                        if element % 3 == 2 {
                            pos_x += 90;
                        }
                        arena.spawn(Box::new(Turtle::new(position,-2, 120, element<3)));
                    }
                }
                1 | 4 => {
                    speed = 2;
                    pos_x = 150 + raft_index * 40;
                    //spawno più tronchi vicini per creare uno più grande
                    for element in 0..3{
                        let position : Pt = pt(pos_x + element*30,pos);
                        arena.spawn(Box::new(Raft::new(position,speed, pt(100, 27))));
                    }
                    pos_x = arena.size().x;
                    arena.spawn(Box::new(Raft::new(pt(pos_x,pos),speed, pt(100, 27))));
                    if raft_index == 1 {
                        arena.spawn(Box::new(Raft::new(pt(pos_x-150,pos),speed, pt(100, 27))));
                    }
                    else {
                        arena.spawn(Box::new(Raft::new(pt(pos_x-120,pos),speed, pt(100, 27))));
                    }
                }
                _ => {
                    {
                        speed = -2;
                        pos_x = 150 - raft_index * 30;
                        for element in 0..3{
                            let position : Pt = pt(pos_x + element*30,pos);
                            arena.spawn(Box::new(Raft::new(position,speed, pt(100, 27))));
                        }
                        pos_x = arena.size().x;
                        arena.spawn(Box::new(Raft::new(pt(pos_x,pos),speed, pt(100, 27))));
                        arena.spawn(Box::new(Raft::new(pt(pos_x-120,pos),speed, pt(100, 27))));
                        }
                }

            }
            
            // arena.spawn(Box::new(Raft::new(position,speed, pt(100, 27))));
        }
        //spawno i frogwinner placeholder e la rana
        arena.spawn(Box::new(FrogWinner::new(pt(50,65))));
        arena.spawn(Box::new(FrogWinner::new(pt(177,65))));
        arena.spawn(Box::new(FrogWinner::new(pt(306,65))));
        arena.spawn(Box::new(FrogWinner::new(pt(433,65))));
        arena.spawn(Box::new(FrogWinner::new(pt(560,65))));
        arena.spawn(Box::new(Frog::new(pt(arena.size().x/2 - 15, frog_pos))));

        BounceGame{arena: arena, playtime: 120}
    }
    // gioco finito se perdo tutte le vite o finisco il tempo
    pub fn game_over(&self) -> bool { return self.remaining_lives() <= 0 || self.remaining_time() <=0}
    // gioco vinto se ho salvato tutte le rane, ho finito l'animazione e ho ancora tempo a disposizione
    pub fn game_won(&self) -> bool { 
        let actors = self.actors();
        for b in actors{
            if let Some(hero) = b.as_any().downcast_ref::<Frog>() {
                return hero.is_all_frog_safe() && self.remaining_time() >= 0 && hero.is_win_animation();
            }
        }
        return false;
    }
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
