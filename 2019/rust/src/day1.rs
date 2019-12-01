use std::fs;

pub fn part1() -> i32 {
    parse_lines(&read_input())
        .map(|line|
            calculate_fuel(line)
        )
        .sum()
}

pub fn part2() -> i32 {

    parse_lines(&read_input())
        .map(|line| {
            calculate_fuel_recursive(line)
//            recursive_fuel(line).sum::<i32>()
        })
        .sum()
}

fn calculate_fuel(start: i32) -> i32 {
    (start / 3) - 2
}

fn calculate_fuel_recursive(start: i32) -> i32 {
    let fuel = calculate_fuel(start);
    if fuel <= 0 {
        0
    } else {
        fuel + calculate_fuel_recursive(fuel)
    }
}

//fn recursive_fuel(start: i32) -> impl Iterator<Item=i32> {
//    std::iter::successors(
//        Some(start),
//        | &num | {
//            let fuel = calculate_fuel(num);
//            if fuel <= 0 { None } else { Some(fuel) }
//        }
//    ).skip(1) // ignore start value because it isn't fuel for any other fuel
//}

fn parse_lines<'a>(file: &'a str) -> impl Iterator<Item=i32> + 'a {
    file.lines()
        .map( |line|
            line.parse::<i32>().unwrap()
        )
}

fn read_input() -> String {
    fs::read_to_string("../inputs/day1.txt")
        .expect("Something went wrong reading input")
}
