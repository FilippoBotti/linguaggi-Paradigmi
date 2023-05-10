fn get_phone_number(name: String, phone_book: &[(String, i32)]) -> Result<i32, String>{
    for tupla in phone_book{
        if tupla.0 == name{
            return Ok(tupla.1);
        }
    }
    return Err(String::from("non presente"));

}
fn main(){
    let book = [
        (String::from("Filo"),1),
        (String::from("Marco"),1),
        (String::from("LKuca"),1)
    ];

    println!("Lo trova? {:?}" , get_phone_number(String::from("Filo"), &book));
}