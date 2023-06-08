use std::fs::read_to_string;

fn read_lines(filename: &str) -> Vec<String> {
    read_to_string(filename) 
        .unwrap()
        .lines()
        .map(String::from)
        .collect()
}

fn main() {
    let lines = read_lines("01.input");
    let parts = lines[0].chars().collect::<Vec<_>>();
    let mut story = 0;
    let mut i = 0;
    for c in &parts {
        i += 1;
        match c {
            ')' => story -= 1,
            '(' => story += 1,
            _ => ()
        }
        if story < 0 {
            dbg!(i);
            panic!();
        }
    }
    // dbg!(parts);
    dbg!(story);
}