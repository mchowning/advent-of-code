use std::fs;

pub fn part1() -> i32 {
    read_file()
        .lines()
        .map(|line|
            calculate_fuel(line.parse::<i32>().unwrap())
        )
        .sum()
}

pub fn part2() -> i32 {
    read_file()
        .lines()
        .map(|line| {
            calculate_fuel_recursive(line.parse::<i32>().unwrap())
        })
        .sum()
}

fn calculate_fuel(start: i32) -> i32 {
    (start / 3) - 2
}

fn calculate_fuel_recursive(start: i32) -> i32 {
    let fuel = calculate_fuel(start);
    if fuel > 0 {
        fuel + calculate_fuel_recursive(fuel)
    } else {
        0
    }
}

fn read_file() -> String {
    fs::read_to_string("../inputs/day1.txt")
        .expect("Something went wrong reading input")
}
