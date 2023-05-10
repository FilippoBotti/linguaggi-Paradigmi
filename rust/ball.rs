struct Ball {
    x: i32,
    y : i32
}

fn moveBall(ball:&mut Ball) {
    ball.x += 5;
    ball.y += 5;
}

fn main() {
    let mut ball =  Ball{
       x: 0,
       y: 0};
    
       println!("Posizioni iniziali {} {} ",ball.x, ball.y);
    moveBall(&mut ball);

    println!("Posizioni dopo move {} {} ",ball.x, ball.y);
}