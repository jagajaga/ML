use std::io::BufferedReader;
use std::io::File;

fn main() {
   let path = Path::new("data-set.txt");
    let mut file = BufferedReader::new(File::open(&path));
    let lines: Vec<String> = file.lines().map(|x| x.unwrap()).collect();
    println!("{}", lines);
}
