open System
open System.IO
open System.Text.Json

type Index = int * int

type Node =
    | Terrain of Index * int
    | Start of Index * int
    | End of Index * int

let height = int >> (fun x -> x - 97)

let parse =
    Seq.mapi (fun r ->
        (Seq.mapi (fun c ->
            function
            | 'S' -> Start((r, c), 0)
            | 'E' -> End((r, c), Int32.MaxValue)
            | n -> Terrain((r, c), (height n)))))

let part1 input = input

let part2 input = 69

let input = File.ReadLines "input.txt" |> parse

input
|> part1
|> JsonSerializer.Serialize
|> (fun (x: string) -> x.Replace("],", "],\n"))
|> Console.WriteLine

input |> part2 |> Console.WriteLine
