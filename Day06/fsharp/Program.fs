open System
open System.IO

let solve size =
    Seq.windowed size
    >> Seq.findIndex (Set >> Set.count >> (=) size)
    >> (+) size

let part1 = solve 4
let part2 = solve 14

let input = File.ReadAllText "input.txt"
input |> part1 |> Console.WriteLine
input |> part2 |> Console.WriteLine
