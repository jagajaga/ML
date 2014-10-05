use std::io::fs;
use std::io::BufferedReader;
use std::io::File;
use std::io;

#[deriving(PartialEq,Show,Clone)]
struct Message {
    subject : Vec<uint>,
    text : Vec<uint>,
    is_spam : bool,
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
                    let is_spam = value.as_str().expect("No a file?").contains("spmsg");
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
}
