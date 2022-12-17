open System
open System.IO
open System.Text.Json

type Index = int * int

let numRows = 163
let numCols = 110
let numCells = numRows * numCols
let midX = numCols - (numCols / 4) * 3
let toRow i = i / numCols
let toCol i = i % numCols

let isRock (r: int, c: int) ((sx: int, sy: int), (ex: int, ey: int)) =
    if sx <> ex && sy <> ey then
        failwith "Rocks can be diagonal"

    let plotCol = (min sx ex) <= c && c <= (max sx ex)
    let plotRow = (min sy ey) <= r && r <= (max sy ey)

    plotCol && plotRow

let translate ((sx: int, sy: int), (ex: int, ey: int)) =
    let moveX x = (x - 500) + midX

    ((scaleX sx, sy), (scaleX ex, ey))

let shouldPlot point line = isRock point (translate line)

let graph (routes: (Index * Index)[] seq) =
    let folder (a: Map<Index, char>) (i: int) =
        let key = (toRow i, toCol i)

        let t = if key = (0, numCols / 2)
                then 'O'
                else if routes |> Seq.exists (Array.exists (shouldPlot key))
                then '#'
                else '.'

        Map.add key t a

    Seq.fold folder Map.empty (seq { 0 .. (numCells - 1) })

let plot (graph: Map<Index, char>) =
    Console.Clear()

    for r in 0 .. (numRows - 1) do
        for c in 0 .. (numCols - 1) do
            graph
            |> Map.tryFind (r, c)
            |> Option.defaultValue 'X'
            |> Console.Write
        Console.WriteLine()

let parse =
    Seq.map (fun (l: string) ->
        l.Split(" -> ")
        |> Array.map (fun x -> x.Split(',') |> Array.map Int32.Parse)
        |> Array.map (fun x -> (Array.head x, Array.last x)))
    >> Seq.map Array.pairwise<Index>

let part1 input = input |> graph |> plot

let part2 = id

let input = File.ReadLines "input.txt" |> parse
input |> part1 |> Console.WriteLine
input |> part2 |> Console.WriteLine
