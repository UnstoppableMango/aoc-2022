open System
open System.IO
open System.Text.Json
open System.Threading

type Index = int * int

let numRows = 163
// let numRows = 80
let numCols = 110
let numCells = numRows * numCols
let midX = numCols - (numCols / 4) * 3
let toRow i = i / numCols
let toCol i = i % numCols

let isRock (r: int, c: int) ((sx: int, sy: int), (ex: int, ey: int)) =
    if sx <> ex && sy <> ey then
        failwith "Rocks can be diagonal"

    let isCol = (min sx ex) <= c && c <= (max sx ex)
    let isRow = (min sy ey) <= r && r <= (max sy ey)

    isCol && isRow

let translate ((sx: int, sy: int), (ex: int, ey: int)) =
    let moveX x = (x - 500) + midX

    ((moveX sx, sy), (moveX ex, ey))

let shouldPlot point line = isRock point (translate line)

let plot (graph: Map<Index, char>) =
    Console.Clear()

    for r in 0 .. (numRows - 1) do
    // for r in 130 .. (numRows - 1) do
        for c in 0 .. (numCols - 1) do
            let key = ((c + 500) - midX, r)

            let cell =
                if (r, c) = (0, midX) then
                    '0'
                else
                    graph |> Map.tryFind key |> Option.defaultValue ' '

            Console.Write cell

        Console.WriteLine()

let genBetween ((ax: int, ay: int), (bx: int, by: int)) =
    let xr = seq { ax..bx }
    let yr = seq { ay..by }
    xr |> Seq.collect (fun x -> yr |> Seq.map<int, Index> (fun y -> (x, y)))

let graph (routes: (Index * Index) [] seq) =
    routes
    |> Seq.collect (Seq.collect genBetween)
    |> Seq.fold (fun a c -> Map.add c '#' a) Map.empty

let parse =
    Seq.map (fun (l: string) ->
        l.Split(" -> ")
        |> Array.map (fun x -> x.Split(',') |> Array.map Int32.Parse)
        |> Array.map (fun x -> (Array.head x, Array.last x)))
    >> Seq.map Array.pairwise<Index>

let moves ((x, y): Index) : Index seq =
    seq {
        yield (x, y + 1)
        yield (x - 1, y + 1)
        yield (x + 1, y + 1)
    }

let fall (m: Map<Index, char>) =
    moves
    >> Seq.skipWhile (fun x -> Map.tryFind x m |> Option.isSome)
    >> Seq.tryHead

let sim (maxDepth: int) (m: Map<Index, char>) =
    let rec simi (s: Index) (a: Map<Index, char>) =
        if (snd s) > maxDepth then
            Map.add s 'X' a
        else
            match fall a s with
            | None -> Map.add s 'o' a
            | Some n ->
                // if (snd s) > (maxDepth - 10) then
                //     plot (Map.add n 'o' m)
                //     Thread.Sleep 50
                simi n a

    simi (500, 0) m

let maxY (m: Map<Index, char>) = m |> Map.keys |> Seq.map snd |> Seq.max

let solve (m: Map<Index, char>) =
    let maxDepth = maxY m
    Console.WriteLine maxDepth

    Seq.initInfinite id
    |> Seq.scan (fun a _ -> sim maxDepth a) m
    |> Seq.takeWhile (Map.values >> (not << Seq.contains 'X'))
    |> Seq.last
    // |> Map.values
    |> sim maxDepth
    |> plot
    // |> Seq.countBy id
    // |> JsonSerializer.Serialize

let part1 input = input |> graph |> solve

let part2 = id

let input = File.ReadLines "input.txt" |> parse
input |> part1 |> Console.WriteLine
// input |> part2 |> Console.WriteLine
