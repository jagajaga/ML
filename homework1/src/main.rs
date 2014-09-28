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
    numbers.iter().fold(0.0, |sum, &x| sum + x.abs()) / numbers.len() as Scalar
}

fn knn(data : &mut DataVec, k : uint, point : Point) -> bool {
    data.sort_by(|&(a, _), &(b, _)| {
            let da = distance(a, point);
            let db = distance(b, point);
            if da < db { Less }
            else if da > db { Greater }
            else { Equal }
    });
    let mut result : i32 = 0;
    data.slice_to(k).iter().map(|&(_, a)| {
        if a {
            result -= 1;
        }
        else {
            result += 1;
        }
    });
    if result < 0 { false } else { true }
}

struct TestResult {
    true_positive : uint,
    false_positive : uint,
    false_negative : uint,
}

impl TestResult {
    fn sum(&self, a : &TestResult) -> TestResult {
        TestResult {
            true_positive : self.true_positive + a.true_positive, 
            false_positive : self.false_positive + a.false_positive, 
            false_negative : self.false_negative + a.false_negative, 
        }
    }
}

fn test(data : &mut DataVec, k : uint, test : & DataVec) -> TestResult {
    let mut true_positive = 0u;
    let mut false_positive = 0u;
    let mut false_negative = 0u;

    for &(point, expected) in test.iter() {
        let result = knn(data, k, point);
        if result && (result == expected) {
            true_positive += 1;
        }
        if result && (result != expected) {
            false_positive += 1;
        }
        if !result && (result != expected) {
            false_negative += 1;
        }
    }

    TestResult {
        true_positive : true_positive,
        false_positive : false_positive,
        false_negative : false_negative,
    }
}

fn f1(test_result : &TestResult) -> Scalar {
    let prec = test_result.true_positive as Scalar / (test_result.true_positive + test_result.false_positive) as Scalar;
    let recall = test_result.true_positive as Scalar / (test_result.true_positive + test_result.false_negative) as Scalar;
    2.0 * prec * recall / (prec + recall)
}

//fn get_best_k(data : &mut DataVec, test : & DataVec) -> uint {
    //let part = data.len() / 4;
    //let fst = data.slice(0, part);
    //let snd = data.slice(part, part * 2);
    //let trd = data.slice(part * 2 + 1, part * 3);
    //let fth = data.slice(part * 3 + 1, data.len());
    //let mut result = 0;
    //for i in range(1i, 9) {
        
    //}
//}

fn main() {
    let path = Path::new("data/data-set.txt");
    let mut file = BufferedReader::new(File::open(&path));
    let data : Vec<Vec<Scalar>> = file.lines().map(
        |x| x.unwrap().as_slice().trim_right_chars('\n').split(',').collect::<Vec<&str>>().iter().map(
            |&x| from_str(x).expect("lolwhat")).collect::<Vec<Scalar>>()).collect();
    let norm0 = norm(data.iter().map(|x| x[0]).collect());
    let norm1 = norm(data.iter().map(|x| x[1]).collect());
    let mut normalized_data : DataVec = data.iter().map(|v| (Point { x: v[0] / norm0, y: v[1] / norm1 }, v[2] != 0.0) ).collect();
    task_rng().shuffle(normalized_data.as_mut_slice());
    println!("{}", knn(&mut normalized_data, 2, Point {x: 0.5, y: 0.25} ));
}

