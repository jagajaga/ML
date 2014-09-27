use std::io::BufferedReader;
use std::io::File;
use std::rand::{task_rng, Rng};

fn norm(numbers : Vec<f32>) -> f32 {
    let mut result = 0f32;
    for &i in numbers.iter() {
        result += i;
    }
    result / numbers.len() as f32
}

fn main() {
    let path = Path::new("data/data-set.txt");
    let mut file = BufferedReader::new(File::open(&path));
    let data : Vec<Vec<f32>> = file.lines().map(
        |x| x.unwrap().as_slice().trim_right_chars('\n').split(',').collect::<Vec<&str>>().iter().map(
            |&x| from_str(x).expect("lolwhat")).collect::<Vec<f32>>()).collect();
    let fst_norm = norm(data.iter().map(|x| x[0]).collect());
    let snd_norm = norm(data.iter().map(|x| x[1]).collect());
    let mut normalized_data : Vec<[f32, .. 3]> = data.iter().map(|x| [x[0] / fst_norm, x[1] / snd_norm, x[2]]).collect();
    let slice = normalized_data.as_mut_slice();
    task_rng.shuffle(slice);
}

