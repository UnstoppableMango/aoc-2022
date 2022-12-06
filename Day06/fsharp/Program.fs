open System
open System.IO

let solve size (input: string) =
    input
    |> Seq.windowed size
    |> Seq.map Set
    |> Seq.findIndex (fun x -> x.Count = size)
    |> fun x -> x + size

let part1 = solve 4
let part2 = solve 14

let input = File.ReadLines "input.txt"
input |> Seq.exactlyOne |> part1 |> Console.WriteLine
input |> Seq.exactlyOne |> part2 |> Console.WriteLine
