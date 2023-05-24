pub enum Expression {
    Literal(f32),  
    Sum(Box<Expression>,Box<Expression>),
    Product(Box<Expression>,Box<Expression>),
}
impl Expression {
    pub fn eval(&self) -> f32 {
        match self {
            Expression::Literal(val) => *val,
            Expression::Sum(val1, val2) =>  val1.eval() + val2.eval(),
            Expression::Product(val1,val2) =>  val1.eval()*val2.eval(),
        }
    }
}

fn parser (tokens: &mut Vec<&str>) -> Expression {
    let token : &str  = tokens.remove(0);
    if token >= "0" && token <="9" {
        return Expression::Literal(token.parse().unwrap());
    }
    else {
        let a: Expression = parser(tokens);
        let b: Expression = parser(tokens);

        if token == "+"{
            println!("Somma");
            return Expression::Sum(Box::new(a), Box::new(b));
        }
        else {
            if token == "*"{
                println!("Prodotto");
                return Expression::Product(Box::new(a), Box::new(b));
            }
        }
    }

    return Expression::Literal(0.0);
    
}

fn main() {
    // let prod1 = Expression::Product(Box::new(Expression::Literal(3.0)),Box::new(Expression::Literal(2.0)));
    // let sum1 = Expression::Sum(Box::new(Expression::Literal(4.0)), Box::new(prod1));         
    // let prod2 = Expression::Product(Box::new(sum1), Box::new(Expression::Literal(5.0)));
    let mut polish: Vec<&str> = "* 5 + 4 * 3 2".split(" ").collect::<Vec<_>>();
    
    println!("Risultato infix * 5 + 4 * 3 2 = {} ", parser(&mut polish).eval());
}