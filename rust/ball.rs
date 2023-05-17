struct Ball {
    x: i32,
    y : i32,
    w: i32,
    h: i32,
    dx : i32,
    dy : i32
}

struct Arena {
    w: i32,
    h: i32
}

fn move_ball_ownership(mut ball: Ball, arena: Arena) -> (Ball, Arena){
    if ball.x + ball.w >= arena.w || ball.x < 0{
        ball.dx = -ball.dx;
    }
    if ball.y + ball.h > arena.h || ball.y < 0 {
        ball.dy = -ball.dy;
    }

    if ball.x + ball.dx < 0 || ball.x + ball.dx > arena.w - ball.w{
        ball.dx = - ball.dx
    }
    if ball.y + ball.dy < 0 || ball.y + ball.dy > arena.h - ball.h{
        ball.dy = - ball.dy
    }

    ball.x += ball.dx;
    ball.y += ball.dy;

    (ball, arena)
}

fn move_ball(ball:&mut Ball, arena: Arena) {
    if ball.x + ball.w >= arena.w || ball.x < 0{
        ball.dx = -ball.dx;
    }
    if ball.y + ball.h > arena.h || ball.y < 0 {
        ball.dy = -ball.dy;
    }

    if ball.x + ball.dx < 0 || ball.x + ball.dx > arena.w - ball.w{
        ball.dx = - ball.dx
    }
    if ball.y + ball.dy < 0 || ball.y + ball.dy > arena.h - ball.h{
        ball.dy = - ball.dy
    }

    ball.x += ball.dx;
    ball.y += ball.dy;
}

fn main() {
    let mut ball =  Ball{
       x: 295,
       y: 235,
       w: 10,
       h:10,
       dx: 5,
       dy: 5};
    
    let mut arena = Arena{
        w: 300,
        h:240,
    };

    println!("Posizioni iniziali {} {} ",ball.x, ball.y);
    (ball,arena) = move_ball_ownership(ball,arena);


    println!("Posizioni dopo move {} {} ",ball.x, ball.y);
    
    move_ball(&mut ball, arena);
    println!("Posizioni dopo move {} {} ",ball.x, ball.y);
}