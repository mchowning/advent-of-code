use std::collections::HashSet;
use std::fs;

pub fn day1() {
    let part_1_result = part1();
    let part_1_passed = part_1_result == 470;

    let part_2_result = part2();
    let part_2_passed = part_2_result == 790;


    if part_1_passed {
        println!("Part 1 passed!");
    } else {
        println!("Part 1 FAILED! Expected 470 but got {}", part_1_result);
    }

    if part_2_passed {
        println!("Part 2 passed!");
    } else {
        println!("Part 2 FAILED! Expected 790 but got {}", part_2_result);
    };
}

fn part1() -> i32 {
    read_file()
        .lines()
        .map(|line|
            line.parse::<i32>().unwrap()
        ).sum()
}

fn part2() -> i32 {
    let set = &mut HashSet::new();
    let running_total = &mut 0;

    for i in read_file()
        .lines()
        .map(|line|
            line.parse::<i32>().unwrap()
        ).cycle()
    {
        *running_total += i;
        if set.contains(running_total) {
            // Found duplicate, so stop searching
            break
        } else {
            set.insert(*running_total);
        }
    }
    *running_total
}

fn read_file() -> String {
    fs::read_to_string("../input.txt")
        .expect("Something went wrong reading input")
}
