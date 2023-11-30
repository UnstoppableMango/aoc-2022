open System
open System.IO

let folder (cur: int, all: int list) (i: string) =
    match Int32.TryParse i with
    | true, x -> (x + cur, all)
    | _ -> (0, cur :: all)

let sumAll = Seq.fold folder (0, []) >> snd

let part1 = sumAll >> Seq.max
let part2 = sumAll >> Seq.sortDescending >> Seq.take 3 >> Seq.sum

let input = File.ReadLines "input.txt"
input |> part1 |> Console.WriteLine
input |> part2 |> Console.WriteLine
