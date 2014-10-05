use std::io::fs;
use std::io::BufferedReader;
use std::io::File;
use std::io;

#[deriving(PartialEq,Show,Clone)]
struct Message<'a> {
    subject : &'a[uint],
    text : &'a[uint],
    is_spam : bool,
}


fn main() {
    //let mut msgs_array = Vec::new();
    let folder = Path::new("data");
    println!("{}", folder.as_str());
    for value in fs::walk_dir(&folder).unwrap() {
        if value.is_file() {
            let mut buf = BufferedReader::new(File::open(&value));
            let mut file = buf.lines();
            let is_spam = value.as_str().expect("No a file?").contains("spmsg");
            let subject = file.next().map(|x| x.ok().expect("No values in subject").as_slice().trim_right_chars('\n').slice_from("Subject: ".len()).split(' ').filter(|x| !x.is_empty()).map(|x| from_str(x).expect("No numbers in subject")).collect::<Vec<uint>>()).collect::<Vec<Vec<uint>>>().as_slice().concat_vec(); 
            let text = file.last().unwrap();
            println!("{} {}", subject, text);
        }
    }

    //let mut file = BufferedReader::new(File::open(&path));
    //let data : Vec<Vec<Scalar>> = file.lines().map(
        //|x| x.unwrap().as_slice().trim_right_chars('\n').split(',').collect::<Vec<&str>>().iter().map(
            //|&x| from_str(x).expect("lolwhat")).collect::<Vec<Scalar>>()).collect();
    //msgs_array.push(vec![Message{subject : vec![1], text : vec![2], is_spam : true}]);
    //println!("{}", msgs_array);
}
