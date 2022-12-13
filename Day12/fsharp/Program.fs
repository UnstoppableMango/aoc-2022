open System
open System.IO
open System.Text.Json

type Index = int * int

type Tile =
    | Terrain
    | Start
    | End

type Node = Index * int * Tile

let fst3 (a, _, _) = a

let snd3 (_, b, _) = b

let parse =
    Seq.mapi (fun r ->
        (Seq.mapi (fun c ->
            function
            | 'S' -> (r, c), -1, Start
            | 'E' -> (r, c), Int32.MaxValue, End
            | n -> (r, c), (n |> (int >> (+) -97)), Terrain))
        >> Seq.toList)
    >> Seq.toList

let isStart =
    function
    | _, _, Start -> true
    | _ -> false

let child r c h =
    List.tryItem r
    >> Option.map(List.tryItem c)
    >> Option.flatten
    >> Option.filter (snd3 >> (>=) (h + 1))

let children (((r, c), h, _): Node) (graph: Node list list) : Node list =
    let up = graph |> (child (r + 1) c h)
    let down = graph |> (child (r - 1) c h)
    let left = graph |> (child r (c - 1) h)
    let right = graph |> (child r (c + 1) h)

    [up; down; left; right]
    |> List.choose id

let rec bfs graph =
    function
    | [] -> failwith "No solution"
    | (n: Node, trace)::xs ->
        match n with
        | _, h, End -> h::trace |> List.rev
        | _, h, _ ->
            let c = children n graph
            let q = c |> List.rev |> List.map (fun x -> x, h::trace)
            bfs graph (xs @ q)

let part1 (input: Node list list) =
    input |> List.iter (List.iter (isStart >> Console.WriteLine))
    let start = input |> List.find (not << List.isEmpty) |> List.find isStart
    let test = [start, List.empty<int>]
    bfs input test

let part2 input = 69

let input = File.ReadLines "input.txt" |> parse

input
|> part1
// |> Seq.map (Seq.map (fun (x, h, t) -> match t with | Start -> "Start" | End -> "End" | Terrain -> "t"))
// |> JsonSerializer.Serialize
// |> (fun (x: string) -> x.Replace("],", "]\n"))
|> Console.WriteLine

input |> part2 |> Console.WriteLine
