pub trait Expression {
    fn eval(&self) -> f32;
}

pub struct Literal {
    val: f32
}
impl Literal {
    pub fn new(val: f32) -> Literal {
        Literal{ val: val}
    }
}
impl Expression for Literal {
    fn eval(&self) -> f32{
        return self.val;
    }
}

pub struct Sum {
    val1: Box<dyn Expression>,
    val2: Box<dyn Expression>,
}
impl Sum {
    pub fn new(val1: Box<dyn Expression>, val2: Box<dyn Expression>) -> Sum {
        Sum{ val1: val1, val2: val2}
    }
}
impl Expression for Sum {
    fn eval(&self) -> f32{
        return self.val1.eval() + self.val2.eval();
    }
}

pub struct Product {
    val1: Box<dyn Expression>,
    val2: Box<dyn Expression>,
}
impl Product {
    pub fn new(val1: Box<dyn Expression>, val2: Box<dyn Expression>) -> Product {
        Product{ val1: val1, val2: val2}
    }
}
impl Expression for Product {
    fn eval(&self) -> f32{
        return self.val1.eval() * self.val2.eval();
    }
}


fn parser (tokens: &mut Vec<&str>) -> Box<dyn Expression> {
    let token : &str  = tokens.remove(0);
    if token >= "0" && token <="9" {
        return Box::new(Literal::new(token.parse().unwrap()));
    }
    else {
        let a: Box<dyn Expression> = parser(tokens);
        let b: Box<dyn Expression> = parser(tokens);

        if token == "+"{
            println!("Somma");
            return Box::new(Sum::new(a, b));
        }
        else {
            if token == "*"{
                println!("Prodotto");
                return Box::new(Product::new(a, b));
            }
        }
    }

    return Box::new(Literal::new(0.0));
    
}

fn main() {
    
    let mut polish: Vec<&str> = "* 5 + 4 * 3 10".split(" ").collect::<Vec<_>>();
    
    // let lit1 = Literal::new(3.0);
    // let lit2 = Literal::new(2.0);
    // let lit3 = Literal::new(4.0);
    // let lit4 = Literal::new(5.0);
    // let prod1 = Product::new(Box::new(lit1),Box::new(lit2));
    // let sum1 = Sum::new(Box::new(lit3),Box::new(prod1));
    // let prod2 = Product::new(Box::new(sum1),Box::new(lit4));
    println!("Risultato infix * 5 + 4 * 3 2 = {} ", parser(&mut polish).eval());
}