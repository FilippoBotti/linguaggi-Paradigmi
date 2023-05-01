
fn change_max(skyscraper : [i32;5])->u8{

    let mut max = 0;
    let mut counter = 0;

    for element in skyscraper {
        if element>max {
            max = element;
            counter+=1;
        }
    }

    return counter;
}

fn main(){
    let skyscraper : [i32;5] = [1,2,3,4,5];


    let counter = change_max(skyscraper);
    println!("Il massimo cambia {} volte", counter);
}