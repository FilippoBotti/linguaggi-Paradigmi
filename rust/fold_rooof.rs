

fn main(){
    let numbers_iterator = [2,3,4,5].iter();



    let count_roof = numbers_iterator.fold((0,0) , |(acc,max),& x| if x>max {(acc+1,x)} else {(acc,max)});

    println!("Max cambia {:?} volte ", count_roof.0);
}