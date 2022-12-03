open System
open System.IO

let priority =
    function
    | x when Char.IsAsciiLetterLower(x) -> (int x) - 96
    | x when Char.IsAsciiLetterUpper(x) -> (int x) - 38
    | _ -> raise (InvalidOperationException())

let part1 input =
    input
    |> Seq.map (fun x ->
        x
        |> Seq.splitInto 2
        |> Seq.map Set
        |> Set.intersectMany
        |> Seq.exactlyOne
        |> priority)
    |> Seq.sum

let part2 input =
    input
    |> Seq.chunkBySize 3
    |> Seq.map (fun x ->
        x
        |> Seq.map Set
        |> Set.intersectMany
        |> Seq.exactlyOne
        |> priority)
    |> Seq.sum

let input = File.ReadLines "input.txt"
part1 input |> Console.WriteLine
part2 input |> Console.WriteLine
