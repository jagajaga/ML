use std::io::BufferedReader;
use std::io::File;
use std::rand::{task_rng, Rng};

type Scalar = f64;
type DataVec = Vec<(Point, bool)>;

struct Point {
    x : Scalar,
    y : Scalar,
}

fn distance(a : Point, b : Point) -> Scalar {
    let dx = a.x - b.x;
    let dy = a.y - b.y;
    (dx * dx + dy * dy).sqrt()
}

fn norm(numbers : Vec<Scalar>) -> Scalar {
    let mut result : Scalar = 0.0;
    for &i in numbers.iter() {
        result += i;
    }
    result / numbers.len() as Scalar
}

fn knn(data : &mut DataVec, k : uint, point : Point) -> bool {
//    data.sort_by(|a, b| a.cmp(b, point));
    false
}

fn main() {
    let path = Path::new("data/data-set.txt");
    let mut file = BufferedReader::new(File::open(&path));
    let data : Vec<Vec<Scalar>> = file.lines().map(
        |x| x.unwrap().as_slice().trim_right_chars('\n').split(',').collect::<Vec<&str>>().iter().map(
            |&x| from_str(x).expect("lolwhat")).collect::<Vec<Scalar>>()).collect();
    let norm0 = norm(data.iter().map(|x| x[0]).collect());
    let norm1 = norm(data.iter().map(|x| x[1]).collect());
    let mut normalized_data : DataVec = data.iter().map(|v| (Point { x: v[0] / norm0, y: v[1] / norm1 }, v[2] != 0.0) ).collect();
    let mut rng = task_rng();
    rng.shuffle(normalized_data.as_mut_slice());
    println!("{}", knn(&mut normalized_data, 2, Point {x: 0.5, y: 0.25} ));
}

