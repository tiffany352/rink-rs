fn main() {
    println!("size: {} bytes", rink_data::DATA.len());

    println!("{:#?}", rink_data::load_data());
}
