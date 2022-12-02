open System
open System.IO

let accumulate acc x =
    match String.IsNullOrWhiteSpace x with
    | true -> []::acc
    | false -> (x::(List.head acc))::(List.tail acc)

let part1 input =
    input
    |> Seq.fold accumulate [[]]
    |> Seq.map ((Seq.map Int32.Parse) >> Seq.sum)
    |> Seq.max

let part2 input =
    input
    |> Seq.fold accumulate [[]]
    |> Seq.map ((Seq.map Int32.Parse) >> Seq.sum)
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.sum

let input = File.ReadLines "input.txt"
part1 input |> Console.WriteLine
part2 input |> Console.WriteLine
