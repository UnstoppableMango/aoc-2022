open System
open System.IO
open System.Text.Json

type Index = int * int

let numRows = 100
let numCols = 200
let numCells = numRows * numCols
let toRow i = i / numRows
let toCol i = i % numCols

let shouldPlot (x: int, y: int) ((sx: int, sy: int), (ex: int, ey: int)) =
    if sx <> ex && sy <> ey then
        failwith "Rocks can be diagonal"

    let plotX = sx < x && x < ex
    let plotY = sy < y && y < ey

    plotX || plotY

let graph (routes: (Index * Index)[] seq) =
    let folder (a: Map<Index, char>) (i: int) =
        let key = (toRow i, toCol i)

        if routes |> Seq.exists (Seq.exists (shouldPlot key)) then
            a |> Map.add key '#'
        else
            a

    Seq.fold folder Map.empty (seq { 0 .. numCells })

let plot (graph: Map<Index, char>) =
    Console.Clear()

    for r in 0 .. numRows do
        for c in 0 .. numCols do
            graph
            |> Map.tryFind (r, c)
            |> Option.defaultValue '.'
            |> Console.Write
        Console.WriteLine()

let parse =
    Seq.map (fun (l: string) ->
        l.Split(" -> ")
        |> Array.map (fun x -> x.Split(',') |> Array.map Int32.Parse)
        |> Array.map (fun x -> (Array.head x, Array.last x))
        |> Array.map (fun (r, c) -> (r - 250, c - 50)))
    >> Seq.map Array.pairwise<Index>

let part1 input =
    let graph = graph input
    plot graph

let part2 = id

let input = File.ReadLines "input.txt" |> parse |> Seq.take 1
input |> part1 |> Console.WriteLine
input |> part2 |> Console.WriteLine


// let numRows2 = 5
// let numCols2 = 5
//
// let toRow i = i / numRows2
// let toCol i = i % numCols2
//
// let tests = [4; 6; 10; 11; 12; 23; 25]
// tests |> Seq.iter (fun x -> Console.WriteLine $"i: {x}, ({toRow x}, {toCol x})")
