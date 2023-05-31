// BOTTI FILIPPO MATRI. 333653
use bounce::Frog;
use pt2d::pt;
use wasm_bindgen::prelude::*;
use std::cell::RefCell;

pub mod actor;
pub mod bounce;
pub mod g2d;
pub mod pt2d;
pub mod rand;

pub struct BounceGui {
    game: bounce::BounceGame,
    heart_animation: i32,
}
impl BounceGui {
    pub fn new() -> BounceGui {
        let game = bounce::BounceGame::new(pt2d::pt(640,480), 5, 5);
        BounceGui{game, heart_animation: 30}
    }
    pub fn setup(&self) {
        g2d::init_canvas(self.game.size());
        g2d::main_loop(30);
    }
    pub fn tick(&mut self) {
        g2d::clear_canvas();
        // disegnamo il background e l'intestazione iniziale
        g2d::draw_image_clip("frogger-bg.png".to_string(), pt(0,30) ,pt(0,15), pt(640,450));
        g2d::draw_image_clip("bg.png".to_string(), pt(0,0) ,pt(0,0), pt(640,30));
        g2d::draw_image_clip("frogger.png".to_string(), pt(0,0) ,pt(3,255), pt(225,30));
        for b in self.game.actors() {
            if let Some(frog) = b.as_any().downcast_ref::<Frog>(){
                if frog.is_dead_animation(){
                    self.heart_animation -=1;
                    if self.heart_animation % 2 == 0 {
                        // quando l'utente perde una vita l'icona lampeggia finchè vi è l'animazione della morte della rana
                        g2d::draw_image_clip("life.png".to_string(), pt(300+self.game.remaining_lives()*20,5) ,pt(0,0), pt(20,20));
                    }
                }
            }
            if let Some(img) = b.sprite() {
                g2d::draw_image_clip("frogger.png".to_string(), b.pos(), img, b.size());
                
            } else {
                //g2d::fill_rect(b.pos(), b.size());
            }
        }
        // disegnamo le vite rimanenti, anzichè usare un campo testuale
        for heart in 0..self.game.remaining_lives() {
            g2d::draw_image_clip("life.png".to_string(), pt(300+heart*20,5) ,pt(0,0), pt(20,20));
        }
        // disegnamo il tempo rimanente come una barra sul gradiente rosso/verde che si riduce via via al passare dei secondi
        g2d::draw_image_clip("time.png".to_string(), pt(399,0) ,pt(0,0), pt(self.game.remaining_time()*2+1,30));
        // let txt = format!("Lives: {} Time: {}",
        //     self.game.remaining_lives(), self.game.remaining_time());
        // g2d::draw_text(txt, pt2d::pt(225, 0), 24);

        if self.game.game_over() {
            g2d::alert("Game over".to_string());
            g2d::close_canvas();
        } else if self.game.game_won() {
            g2d::alert("Game won".to_string());
            g2d::close_canvas();
        } else {
            self.game.tick(g2d::current_keys());  // Game logic
        }
    }
}

thread_local! {
    static GUI: RefCell<BounceGui> = RefCell::new(BounceGui::new());
}

#[wasm_bindgen]
pub fn tick() {
    GUI.with(|g| {
        g.borrow_mut().tick();
    });
}

#[wasm_bindgen]
pub fn setup() {
    GUI.with(|g| {
        g.borrow_mut().setup();
    });
}
