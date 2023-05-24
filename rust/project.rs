#[derive(Copy, Clone, Debug)]

pub struct Pt {
    pub x: i32,
    pub y: i32
}

pub fn pt(x: i32, y: i32) -> Pt { Pt{x: x, y: y} }


pub trait Vehicle {
    fn get_id(&self)-> i32;
    fn pos(&self) -> Pt;
    fn equipment(&self) -> i32;
    fn get_intervent_time(&self, pt: Pt) -> f32;
    fn is_available(&self) -> bool;
    fn get_velocity(&self) -> i32;
    fn set_available(&mut self, available: bool);
}

//sia ambulanza che elicottero vengono creati con disponibilita=true
pub struct Ambulance {
    id: i32,
    pos: Pt,
    equipment: i32,
    available: bool,
    velocity: i32
}
impl Ambulance {
    pub fn new(id: i32, pos: Pt, equipment: i32) -> Ambulance {
        Ambulance{ id: id, pos: pos, equipment: equipment, available: true, velocity: 100}
    }
}
impl Vehicle for Ambulance {
    fn get_id(&self) -> i32{
        return self.id;
    }
    fn pos(&self) -> Pt{
        return self.pos;
    }
    fn equipment(&self) -> i32{
        return self.equipment;
    }
    //calcolo tempo di intervento rispetto ad un punto dato distanza/velocità
    // distanza = x + y
    fn get_intervent_time(&self, dest: Pt) -> f32{
        let diff = (dest.y-self.pos.y).abs() + (dest.x-self.pos.x).abs();
        let get_intervent_time: f32 = (diff as f32)/(self.velocity as f32) ;
        return get_intervent_time;
    }
    fn is_available(&self) -> bool{
        return self.available;
    } 
    fn set_available(&mut self, available: bool){
        self.available = available;
    } 
    fn get_velocity(&self) -> i32 {
        return self.velocity;
    }
}


pub struct Helicopter {
    id: i32,
    pos: Pt,
    equipment: i32,
    available: bool,
    velocity: i32
}
impl Helicopter {
    pub fn new(id: i32, pos: Pt, equipment: i32) -> Helicopter {
        Helicopter{ id: id, pos: pos, equipment: equipment, available: true, velocity: 250}
    }
}
impl Vehicle for Helicopter {
    fn get_id(&self) -> i32{
        return self.id;
    }
    fn pos(&self) -> Pt{
        return self.pos;
    }
    fn equipment(&self) -> i32{
        return self.equipment;
    }
    //calcolo tempo di intervento rispetto ad un punto dato distanza/velocità.
    // distanza = sqrt(x^2 + y^2) per pitagora
    // aggiungo 5 minuti
    fn get_intervent_time(&self, dest: Pt) -> f32{
        let diff = f32::sqrt(((dest.y-self.pos.y)*(dest.y-self.pos.y) + (dest.x-self.pos.x)*(dest.x-self.pos.x)) as f32);
        let get_intervent_time: f32 = diff/(self.velocity as f32);
        return 5.0+get_intervent_time;
    }
    fn is_available(&self) -> bool{
        return self.available;
    } 
    fn get_velocity(&self) -> i32 {
        return self.velocity;
    }
    fn set_available(&mut self, available: bool){
        self.available = available;
    }
}

pub struct Emergency  {
    vehicles: Vec<Box<dyn Vehicle>>,
}

impl Emergency {

    fn new(vehicles : Vec<Box <dyn Vehicle>>) -> Emergency {
        Emergency { vehicles: vehicles }
    }

    fn get_available_vehicle(&mut self, point: Pt, emergency: i32) -> f32 {
        // gestisco il minimo che cambia
        let mut min_time : f32 = 1000000.0;
        // per gestire l'indisponibilità dei veicoli sfrutto un'ulteriore variabile inizializzata a -1
        let mut result : f32 = -1.0;
        // gestisco l'indice
        let mut index = 0;
        let mut selected_index : usize = 0;
        // ciclo verificando che l'equipaggiamento sia sufficiente e che il veicolo impieghi il tempo minimo
        for vehicle in self.vehicles.iter_mut() {
            if vehicle.is_available() && vehicle.equipment() >= emergency && vehicle.get_intervent_time(point) < min_time {
                min_time = vehicle.get_intervent_time(point);
                result = min_time;
                selected_index = index
            }
            index +=1;
        }
        // se ho trovato allora vado a rendere il veicolo indisponibile
        if result != -1.0 {
            self.vehicles[selected_index].set_available(false);
        }
        return result;
    }

    fn add_vehicle(&mut self, vehicle: Box<dyn Vehicle>) {
        self.vehicles.push(vehicle);
    }

    fn print_vehicle(&self){
        println!("Veicoli [");
        for vehicle in self.vehicles.iter() {
            println!("id: {}\npos:{},{}\nequipment:{}\ndisponibilità:{},", vehicle.get_id(), vehicle.pos().x, vehicle.pos().y, vehicle.equipment(),vehicle.is_available());
        }
        println!("]");
    }

}



fn main(){
    use std::io::*;

    // init delle variabili che mi servono per creare nuovi veicoli e aggiungerli
    let mut id : i32 = 0;
    let mut x: i32 = 0;
    let mut y: i32 = 0;
    let mut livello_emergenza: i32 = 0;
    let mut tipo_veicolo: i32 = 0;

    let mut exit = false;

    // inserisco due veicoli via codice
    let a: Ambulance = Ambulance::new(id,pt(0,0),1);
    id +=1;
    let b: Helicopter = Helicopter::new(id,pt(30,20),2);
    id+=1;

    let mut vehicles = Vec::<Box<dyn Vehicle>>::new();
    vehicles.push(Box::new(a));

    // creo la flotta passandovi un vettore con un solo veicolo
    let mut emergency_vehicles: Emergency = Emergency::new(vehicles);
 

    // aggiungo un veicolo alla flotta
    emergency_vehicles.add_vehicle(Box::new(b));


    // loop interattivo con pattern matching per l'input utente

    while !exit {
        println!("1 per aggiungere, 2 per stampare i veicoli, 3 per richiedere un veicolo, 4 per uscire");
        let cord = stdin().lock().lines().next().unwrap().unwrap();
        match cord.parse::<i32>() {
            Ok(i) => {
                match i {
                    // chiedo le coordinate del veicolo, il livello di attrezzatura e il tipo di veicolo (la disponibilità è true per default)
                    1 => {
                        println!("Coordinata x?");
                        let cord = stdin().lock().lines().next().unwrap().unwrap();
                        match cord.parse::<i32>() {
                            Ok(i) => x = i,
                            Err(..) => println!("Errore, hai inserito: {}", cord),
                        };
                        println!("Coordinata y?");
                        let cord = stdin().lock().lines().next().unwrap().unwrap();
                        match cord.parse::<i32>() {
                            Ok(i) => y = i,
                            Err(..) => println!("Errore, hai inserito: {}", cord),
                        };
                        println!("Livello attrezzatura?");
                        let cord = stdin().lock().lines().next().unwrap().unwrap();
                        match cord.parse::<i32>() {
                            Ok(i) => {
                                if i>0 && i<=4 {
                                    livello_emergenza = i;
                                }
                                else {
                                    livello_emergenza = 1;
                                }
                            },
                            Err(..) => println!("Errore, hai inserito: {}", cord),
                        };
                        println!("0 per ambulanza, 1 per elicottero?");
                        let cord = stdin().lock().lines().next().unwrap().unwrap();
                        match cord.parse::<i32>() {
                            Ok(i) => {
                                if i>0 && i<=2 {
                                    tipo_veicolo = i;
                                } 
                                else {
                                    tipo_veicolo = 0;
                                }
                            },
                            Err(..) => println!("Errore, hai inserito: {}", cord),
                        };
                        if tipo_veicolo == 0 {
                            let new_vehicle = Ambulance::new(id,pt(x,y),livello_emergenza);
                            emergency_vehicles.add_vehicle(Box::new(new_vehicle));
                            id+=1;
                        }
                        else {
                            let new_vehicle = Helicopter::new(id,pt(x,y),livello_emergenza);
                            emergency_vehicles.add_vehicle(Box::new(new_vehicle));
                            id+=1;
                        }
                    }
                    // stampo la flotta
                    2 => emergency_vehicles.print_vehicle(),
                    // richiedo emergenza inserendo le coordinate del punto e l'attrezzatura richiesta, stampo il tempo che impiega il veicolo più veloce
                    3 => {
                        println!("Coordinata x di dove vuoi richiedere assistenza?");
                        let cord = stdin().lock().lines().next().unwrap().unwrap();
                        match cord.parse::<i32>() {
                            Ok(i) => x = i,
                            Err(..) => println!("Errore, hai inserito: {}", cord),
                        };
                        println!("Coordinata y di dove vuoi richiedere assistenza?");
                        let cord = stdin().lock().lines().next().unwrap().unwrap();
                        match cord.parse::<i32>() {
                            Ok(i) => y = i,
                            Err(..) => println!("Errore, hai inserito: {}", cord),
                        };
                        println!("Livello attrezzatura minima?");
                        let cord = stdin().lock().lines().next().unwrap().unwrap();
                        match cord.parse::<i32>() {
                            Ok(i) => {
                                if i>0 && i<=4 {
                                    livello_emergenza = i;
                                }
                            },
                            Err(..) => println!("Errore, hai inserito: {}", cord),
                        };
                        let time = emergency_vehicles.get_available_vehicle(pt(x,y),livello_emergenza);
                        if  time != -1.0 {
                            println!("Il veicolo arriverà in: {}", time)
                        } 
                        else {
                            println!("Tutti i veicoli sono occupati");
                        }
                    },
                    _ => exit = true,
                }
            },
            Err(..) => println!("Errore, hai inserito: {}", cord),
        };
    }


}

