open System
open System.IO
open System.Threading

type Direction =
    | Left of int
    | Right of int
    | Up of int
    | Down of int

type Knot = { Row: int; Col: int }
let emptyKnot = { Row = 0; Col = 0 }

let shouldPlot = false
let shouldPlotSteps = false

let plot (knots: Knot seq) =
    let size = 22
    let head = Seq.head knots
    let graph = Array2D.create size size -1

    knots
    |> Seq.iteri (fun i c ->
        let row = c.Row + ((size / 2) - head.Row)
        let col = c.Col + ((size / 2) - head.Col)
        if graph[row, col] = -1 then graph[row, col] <- i)

    let toChar =
        function
        | -1 -> "."
        | 0 -> "H"
        | n -> $"{n}"

    Console.Clear()

    Seq.init size id
    |> Seq.map (fun i -> graph[i, *] |> Array.map toChar)
    |> Seq.rev
    |> Seq.iter (fun x -> Console.WriteLine(String.Join("", x)))

let parse (x: string) =
    let split = x.Split(' ')
    let n = Int32.Parse split[1]

    match split[0] with
    | "L" -> Left n
    | "R" -> Right n
    | "U" -> Up n
    | "D" -> Down n
    | _ -> failwith "Invalid input"

let moveLeft knot = { knot with Col = knot.Col - 1 }
let moveRight knot = { knot with Col = knot.Col + 1 }
let moveUp knot = { knot with Row = knot.Row + 1 }
let moveDown knot = { knot with Row = knot.Row - 1 }

let follow (head: Knot) (tail: Knot) =
    let dontMove a = List.contains a [ 0; 1; -1 ]
    let y = head.Row - tail.Row
    let x = head.Col - tail.Col

    match (x, y) with
    | 1, 2
    | 2, 1
    | 2, 2 ->
        { Row = tail.Row + 1
          Col = tail.Col + 1 }
    | -1, 2
    | -2, 1
    | -2, 2 ->
        { Row = tail.Row + 1
          Col = tail.Col - 1 }
    | 1, -2
    | 2, -1
    | 2, -2 ->
        { Row = tail.Row - 1
          Col = tail.Col + 1 }
    | -2, -1
    | -1, -2
    | -2, -2 ->
        { Row = tail.Row - 1
          Col = tail.Col - 1 }
    | 0, 2 -> { tail with Row = tail.Row + 1 }
    | 0, -2 -> { tail with Row = tail.Row - 1 }
    | 2, 0 -> { tail with Col = tail.Col + 1 }
    | -2, 0 -> { tail with Col = tail.Col - 1 }
    | r, c when dontMove r && dontMove c -> tail
    | _ -> failwith $"Unexpected. head: [{head.Row}, {head.Col}], tail: [{tail.Row}, {tail.Col}], x: {x}, y: {y}"

let stepOne (move: Knot -> Knot) (head: Knot) (tail: Knot) (visited: Map<Knot, int>) =
    if shouldPlotSteps then plot [head; tail]
    let nh = move head
    if shouldPlotSteps then plot [nh; tail]
    let nt = follow nh tail

    if shouldPlot then
        plot [nh; nt]
        Thread.Sleep 50

    let nv =
        visited.Change(nt, (fun x -> x |> Option.orElse (Some 0) |> Option.map ((+) 1)))

    (nh, nt, nv)

let stepOneAll (move: Knot -> Knot) (head: Knot) (tail: Knot list) (visited: Map<Knot, int>) =
    if shouldPlotSteps then plot (head :: tail)
    let nh = move head
    if shouldPlotSteps then plot (nh :: tail)
    let nt = tail |> List.scan follow nh |> List.tail

    if shouldPlot then
        plot (nh :: nt)
        Thread.Sleep 50

    let nv =
        visited.Change(nt |> List.last, (fun x -> x |> Option.orElse (Some 0) |> Option.map ((+) 1)))

    (nh, nt, nv)

let rec stepP1 (head: Knot, tail: Knot, visited: Map<Knot, int>) (direction: Direction) =
    match direction with
    | Left n when n <> 0 -> stepP1 (stepOne moveLeft head tail visited) (Left(n - 1))
    | Right n when n <> 0 -> stepP1 (stepOne moveRight head tail visited) (Right(n - 1))
    | Up n when n <> 0 -> stepP1 (stepOne moveUp head tail visited) (Up(n - 1))
    | Down n when n <> 0 -> stepP1 (stepOne moveDown head tail visited) (Down(n - 1))
    | _ -> (head, tail, visited)

let rec stepP2 (head: Knot, tail: Knot list, visited: Map<Knot, int>) (direction: Direction) =
    match direction with
    | Left n when n <> 0 -> stepP2 (stepOneAll moveLeft head tail visited) (Left(n - 1))
    | Right n when n <> 0 -> stepP2 (stepOneAll moveRight head tail visited) (Right(n - 1))
    | Up n when n <> 0 -> stepP2 (stepOneAll moveUp head tail visited) (Up(n - 1))
    | Down n when n <> 0 -> stepP2 (stepOneAll moveDown head tail visited) (Down(n - 1))
    | _ -> (head, tail, visited)

let solve folder state =
    Seq.fold folder state
    >> fun (_, _, visited) -> visited
    >> Map.values
    >> Seq.filter ((<) 0)
    >> Seq.length

let part1 = solve stepP1 (emptyKnot, emptyKnot, Map.empty)

let part2 = solve stepP2 (emptyKnot, List.replicate 9 emptyKnot, Map.empty)

let input = File.ReadLines "input.txt"
input |> Seq.map parse |> part1 |> Console.WriteLine
input |> Seq.map parse |> part2 |> Console.WriteLine
