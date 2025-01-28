use pesticide::pesticide;

#[pesticide(target_path = "../target")]
pub mod ast {
    #[derive(Debug)]
    pub enum Value {
        #[silent(r#" "null" "#)]
        Null,
        Bool(bool),
        Int(i64),
        Float(f64),
        String(String),
    }

    #[derive(Debug)]
    pub struct Op2Expr {
        pub left: Value,
        #[any("+", "-", "*", "/", "%", "**")]
        pub op: String,
        pub right: Value,
    }
}

fn main() {
    let expr = ast::Op2Expr::try_parse("2 + 3").unwrap();
    println!("ast: {:?}", expr);
}
