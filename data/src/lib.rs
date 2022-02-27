use rink_format::UnitsData;

pub const DATA: &'static [u8] = include_bytes!(concat!(env!("OUT_DIR"), "/rink-data.bincode"));

pub fn load_data() -> UnitsData {
    bincode::deserialize::<UnitsData>(DATA).unwrap()
}
