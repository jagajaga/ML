use std::io::BufferedReader;
use std::io::File;
use std::rand::{task_rng, Rng};

type Scalar = f64;
type DataVec = Vec<(Point, bool)>;


#[deriving(PartialEq,Show,Clone)]
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

fn knn(data : &DataVec, k : uint, point : Point) -> bool {
    let mut sorted_vec = data.clone();
    sorted_vec.sort_by(|&(a, _), &(b, _)| {
            let da = distance(a, point);
            let db = distance(b, point);
            if da < db { Less }
            else if da > db { Greater }
            else { Equal }
    });
    let mut result : i32 = 0;
    for &(_, a) in sorted_vec.slice_to(k).iter() {
        if !a {
            result = result - 1;
        }
        else {
            result = result + 1;
        }
    };
    if result <= 0 { false } else { true }
}

#[deriving(PartialEq,Show,Clone)]
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

fn test(data : &DataVec, k : uint, test : &[(Point,bool)]) -> TestResult {
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

    let result = TestResult {
        true_positive : true_positive,
        false_positive : false_positive,
        false_negative : false_negative,
    };
    result
}

fn f1(test_result : &TestResult) -> Scalar {
    let prec = test_result.true_positive as Scalar / (test_result.true_positive + test_result.false_positive) as Scalar;
    let recall = test_result.true_positive as Scalar / (test_result.true_positive + test_result.false_negative) as Scalar;
    2.0 * prec * recall / (prec + recall)
}

fn get_best_k(data : &DataVec) -> uint {
    let part = data.len() / 4;
    let fst = (data.slice(0, part));
    let snd = (data.slice(part + 1, part * 2));
    let trd = (data.slice(part * 2 + 1, part * 3));
    let fth = (data.slice(part * 3 + 1, data.len()));
    let mut result = 0f64;
    let mut best_k : uint = 0;
    for k in range(1u, fth.len()) {
        if k % 2 == 0 { continue; }
        let f1_result = f1(&test(&data.iter().filter(|&x| !fst.contains(x)).map(|&x| x).collect::<DataVec>(), k, fst).sum(&test(&data.iter().filter(|&x| !snd.contains(x)).map(|&x| x).collect::<DataVec>(), k, snd)).sum(&test(&data.iter().filter(|&x| !trd.contains(x)).map(|&x| x).collect::<DataVec>(), k, trd)).sum(&test(&data.iter().filter(|&x| !fth.contains(x)).map(|&x| x).collect::<DataVec>(), k, fth)));
        if result < f1_result {
            result = f1_result;
            best_k = k;
        }
    }
    best_k
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
    task_rng().shuffle(normalized_data.as_mut_slice());
    let learning_data = (normalized_data.slice(0, normalized_data.len() / 5 * 4)).to_vec();
    let test_data = (normalized_data.slice(normalized_data.len() / 5 * 4 + 1, normalized_data.len()));
    let best_k = get_best_k(&learning_data);
    let test_debug = test(&learning_data, best_k, test_data);
    let result = f1(&test_debug);
    println!("result: {}", result);
}

