use pesticide::pesticide;

#[pesticide(target_path = "../target")]
pub mod ast {
    #[any("+", "-", "*", "/", "%", "**")]
    pub enum Oper {}
}

fn main() {
    println!("hello");
}
