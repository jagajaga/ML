use std::io::fs;
use std::io::BufferedReader;
use std::io::File;
use std::io::fs::PathExtensions;
use std::collections::HashMap;

#[deriving(PartialEq,Show,Clone)]
struct Message {
    subject : Vec<uint>,
    text : Vec<uint>,
    is_spam : bool,
}

struct Classifier {
    spam_probability : HashMap<uint, f64>,
}

fn increment_map_value(map : &mut HashMap<uint, uint>, key : & uint) {
    let x = map.find_mut(key);
    match (x) {
        Some(n) => *n += 1,
        None => {
            map.insert(*key, 1u);
        },
    }
}

impl Classifier {
    pub fn new(data : &Vec<&Vec<Message>>) -> Classifier {
        let mut prob = HashMap::new();
        let mut occurences = HashMap::new();
        let mut spam_occurences = HashMap::new();
        for part in data.iter() {
            for message in part.iter() {
                for word in message.subject.iter() {
                    increment_map_value(&mut occurences, word);
                    if message.is_spam {
                        increment_map_value(&mut spam_occurences, word);
                    }
                }
                for word in message.text.iter() {
                    increment_map_value(&mut occurences, word);
                    if message.is_spam {
                        increment_map_value(&mut spam_occurences, word);
                    }
                }
            }
        }
        for (word, n) in occurences.iter() {
            let x = spam_occurences.find(word);
            match (x) {
                Some(v) => {
                    prob.insert(*word, *v as f64 / *n as f64 * 0.98 + 0.01);
                },
                None => {
                    prob.insert(*word, 0.0 * 0.98 + 0.01);
                },
            }
        }
        Classifier{ spam_probability : prob }
    }

    pub fn is_spam(self, msg : & Message) -> bool {
        let mut spam_f : f64 = 0.0;
        let mut not_spam_f : f64 = 0.0;
        for word in msg.subject.iter() {
            let x = self.spam_probability.find(word);
            match (x) {
                Some(v) => {
                    spam_f -= v.ln();
                    not_spam_f -= (1.0 - *v).ln();
                },
                None => (),
            }
        }
        spam_f < not_spam_f
    }
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

fn main() {
    let mut msgs_array = Vec::new();
    let folder = Path::new("data");
    for value in fs::walk_dir(&folder).unwrap() {
        if value.is_dir() {
            let mut temp = Vec::new();
            for value in fs::walk_dir(&value).unwrap() {
                if value.is_file() {
                    let mut buf = BufferedReader::new(File::open(&value));
                    let mut file = buf.lines();
                    let is_spam = value.as_str().unwrap().contains("spmsg");
                    let subject = 
                    file.next().map(
                            |x| x.ok().unwrap().as_slice().trim_right_chars('\n').slice_from("Subject: ".len()).split(' ').filter(|x| !x.is_empty()).map(
                                |x| from_str(x).expect("NaN in subject")).collect::<Vec<uint>>()
                    ).unwrap();
                    let text = 
                    file.last().map(
                            |x| x.ok().unwrap().as_slice().trim_right_chars('\n').split(' ').filter(|x| !x.is_empty()).map(
                                |x| from_str(x).expect("NaN in text")).collect::<Vec<uint>>()
                    ).unwrap();
                    let message = Message {subject : subject, 
                                        text : text, 
                                        is_spam : is_spam};
                    temp.push(message);
                }
            }
            msgs_array.push(temp);
        }
    }
    let parts_size = msgs_array.len();
    for i in range(0, parts_size) {
        let mut data = Vec::new();
        for j in range(0, parts_size) {
            if i != j {
                data.push(&msgs_array[i]);
            }
        }
        let cls = Classifier::new(&data);

        let mut true_positive = 0u;
        let mut false_positive = 0u;
        let mut false_negative = 0u;
        for msg in msgs_array[i].iter() {
            let expected = msg.is_spam;
            let result = cls.is_spam(msg);

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
    }
}

