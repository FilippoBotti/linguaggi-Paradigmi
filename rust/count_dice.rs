fn rand_int(nmin: i32, nmax: i32, seed: u32) -> (i32, u32) {
    let mut seed : u32 = seed;
    // From "Xorshift RNGs" by George Marsaglia
    seed ^= seed << 13;
    seed ^= seed >> 17;
    seed ^= seed << 5;
    let range = (nmax + 1 - nmin) as u32;
    let val = nmin + (seed % range) as i32;
    (val, seed)
}

fn time_seed() -> u32 {
    use std::time::SystemTime as st;
    let now = st::now().duration_since(st::UNIX_EPOCH).unwrap();
    now.as_millis() as u32
}

fn main(){

    let mut counter = [0u8; 11];  
    let mut seed = time_seed();
    let mut first_dice : i32;
    let mut second_dice : i32;
    for i in 0..1000 {
        (first_dice,seed)  = rand_int(1,6,seed);
        (second_dice, seed)  = rand_int(1,6,seed);
        println!("Lancio: {}-esimo risultati: {} {} ",i+1,first_dice,second_dice);
        let sum  = first_dice+second_dice - 2;
        counter[sum as usize]+=1;
    }

    let mut i = 2;
    for element in counter{
        println!("Le occorrenze di {} risultano : {}" ,i , element );
        i+=1;
    }
}