open System
open System.IO
open System.Text.Json
open System.Threading

type Tile =
    | Terrain
    | Start
    | End

type Node =
    { Row: int
      Col: int
      Height: int
      Tile: Tile
      Explored: bool }

let row n = n.Row
let col n = n.Col
let height n = n.Height
let explored n = n.Explored

let explore n = { n with Explored = true }

let setExplored n graph =
    let row = List.item n.Row graph
    let updated = List.updateAt n.Col (explore n) row
    List.updateAt n.Row updated graph

let parse =
    Seq.mapi (fun r ->
        (Seq.mapi<char, Node> (fun c ->
            function
            | 'S' -> { Row = r; Col = c; Height = 0; Tile = Start; Explored = true }
            | 'E' -> { Row = r; Col = c; Height = 26; Tile = End; Explored = false }
            | n -> { Row = r; Col = c; Height = (n |> (int >> (+) -97)); Tile = Terrain; Explored = false }))
        >> Seq.toList)
    >> Seq.toList

let plot (graph: Node list list) =
    Console.Clear()
    for r in graph do
        for n in r do
            if n.Explored then
                Console.Write "."
            else
                Console.Write (char (n.Height + 97))
        Console.WriteLine()
    Thread.Sleep 100

let plotPath (graph: Node list list) (path: Node list) =
    Console.Clear()
    for r in graph do
        for n in r do
            if List.contains n path then
                Console.Write "."
            else
                Console.Write (char (n.Height + 97))
        Console.WriteLine()
    Thread.Sleep 100

let isStart =
    function
    | { Tile = Start } -> true
    | _ -> false

let isEnd =
    function
    | { Tile = End } -> true
    | _ -> false

let child r c =
    List.tryItem r
    >> Option.map(List.tryItem c)
    >> Option.flatten

let children (n: Node) (graph: Node list list) : Node list =
    let { Row = r; Col = c; Height = h } = n
    let up = child (r + 1) c graph
    let down = child (r - 1) c graph
    let left = child r (c - 1) graph
    let right = child r (c + 1) graph

    [up; down; left; right]
    |> List.choose id
    |> List.filter (height >> (>=) (h + 1))
    |> List.filter (not << explored)

let children2 (n: Node) (graph: Node list list) : Node list =
    let { Row = r; Col = c; Height = h } = n
    let up = child (r + 1) c graph
    let down = child (r - 1) c graph
    let left = child r (c - 1) graph
    let right = child r (c + 1) graph

    [up; down; left; right]
    |> List.choose id
    |> List.filter (height >> (<=) (h - 1))
    |> List.filter (not << explored)

let rec bfs graph =
    function
    | [] -> failwith "No solution"
    | (n: Node, trace)::xs ->
        match n.Tile with
        | End -> trace |> List.rev
        | _ ->
            let c = children n graph
            let q = c |> List.map (fun x -> x, n::trace)
            bfs (List.foldBack setExplored c graph) (xs @ q)

let rec bfs2 graph =
    function
    | [] -> []
    | (n: Node, trace)::xs ->
        match n with
        | { Height = 0; Tile = Terrain } -> [trace] @ (List.map snd xs)
        | _ ->
            let c = children2 n graph
            // plot graph
            let q = c |> List.map (fun x -> x, n::trace)
            bfs2 (List.foldBack setExplored c graph) (xs @ q)

let part1 (input: Node list list) =
    let start = input |> List.pick (List.tryFind isStart)
    let path = bfs input [start, List.empty<Node>]
    // plotPath input path
    path |> List.length

let part2 (input: Node list list) =
    let start = input |> List.pick (List.tryFind isEnd)
    let path = bfs2 input [start, List.empty<Node>]
    // path |> List.map List.length
    path |> List.minBy List.length |> List.length

let input = File.ReadLines "input.txt" |> parse

input |> part1 |> Console.WriteLine
input |> part2 |> Console.WriteLine
