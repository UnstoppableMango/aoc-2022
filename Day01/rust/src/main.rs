use std::fs;

fn get_sums(input: &str) -> Vec<u32> {
    return input.split("\n\n")
        .map(|x| x.trim_end()
            .split("\n")
            .map(|y| y.parse::<u32>().expect("To be a u32"))
            .sum::<u32>())
        .collect::<Vec<u32>>();
}

fn part1(input: &str) -> u32 {
    return get_sums(input)
        .into_iter()
        .max()
        .unwrap();
}

fn part2(input: &str) -> u32 {
    let mut sums = get_sums(input);

    sums.sort();

    return sums.into_iter()
        .rev()
        .take(3)
        .sum();
}

fn main() {
    let input: String = fs::read_to_string("../input.txt")
        .expect("Should have read the file");

    let part1_result = part1(&input);
    let part2_result = part2(&input);

    println!("{part1_result:?}");
    println!("{part2_result:?}");
}
