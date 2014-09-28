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

fn distance(a : (f32, f32), b : (f32, f32)) -> f32 {
    let (x1, y1) = a;
    let (x2, y2) = b;
    ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1)).sqrt()
}

// 1nn
fn simple(data : Vec<[f32, .. 3]>, point : (f32, f32)) -> bool {
    let mut min = std::f32::MAX_VALUE;
    let mut value = false;
    for v in data.iter() {
        let d = distance(point, (v[0], v[1]));
        if (d < min) {
            min = d;
            value = (v[2] != 0f32);
        }
    }
    value
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
//    let slice = normalized_data.as_mut_slice();
//    task_rng.shuffle(slice);
    simple(normalized_data, (0.5, 0.4));
}

