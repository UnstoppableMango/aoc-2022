open System
open System.IO
open System.Text.Json

type Direction =
    | Left of int
    | Right of int
    | Up of int
    | Down of int

type Knot = { Row: int; Col: int }
let emptyKnot = { Row = 0; Col = 0 }

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
    | 2, 1 ->
        { Row = tail.Row + 1
          Col = tail.Col + 1 }
    | -1, 2
    | -2, 1 ->
        { Row = tail.Row + 1
          Col = tail.Col - 1 }
    | 1, -2
    | 2, -1 ->
        { Row = tail.Row - 1
          Col = tail.Col + 1 }
    | -2, -1
    | -1, -2 ->
        { Row = tail.Row - 1
          Col = tail.Col - 1 }
    | 0, 2 -> { tail with Row = tail.Row + 1 }
    | 0, -2 -> { tail with Row = tail.Row - 1 }
    | 2, 0 -> { tail with Col = tail.Col + 1 }
    | -2, 0 -> { tail with Col = tail.Col - 1 }
    | r, c when dontMove r && dontMove c -> tail
    | _ ->
        failwith
            $"Unexpected. head: {head |> JsonSerializer.Serialize}, tail: {tail |> JsonSerializer.Serialize}, x: {x}, y: {y}"

let stepOne (move: Knot -> Knot) (head: Knot) (tail: Knot) (visited: Map<Knot, int>) =
    let nh = move head
    let nt = follow nh tail

    let nv =
        visited.Change(nt, (fun x -> x |> Option.orElse (Some 0) |> Option.map ((+) 1)))

    (nh, nt, nv)

let rec stepP1 (head: Knot, tail: Knot, visited: Map<Knot, int>) (direction: Direction) =
    match direction with
    | Left n when n <> 0 -> stepP1 (stepOne moveLeft head tail visited) (Left(n - 1))
    | Right n when n <> 0 -> stepP1 (stepOne moveRight head tail visited) (Right(n - 1))
    | Up n when n <> 0 -> stepP1 (stepOne moveUp head tail visited) (Up(n - 1))
    | Down n when n <> 0 -> stepP1 (stepOne moveDown head tail visited) (Down(n - 1))
    | _ -> (head, tail, visited)
    
let rec stepP2 (head: Knot, tail: Knot seq, visited: Map<Knot, int>) (direction: Direction) =
    match direction with
    | Left n when n <> 0 -> stepP2 (stepOne moveLeft head tail visited) (Left(n - 1))
    | Right n when n <> 0 -> stepP2 (stepOne moveRight head tail visited) (Right(n - 1))
    | Up n when n <> 0 -> stepP2 (stepOne moveUp head tail visited) (Up(n - 1))
    | Down n when n <> 0 -> stepP2 (stepOne moveDown head tail visited) (Down(n - 1))
    | _ -> (head, tail, visited)

let solve folder state =
    Seq.fold folder state
    >> fun (_, _, visited) -> visited
    >> Map.values
    >> Seq.filter ((<) 0)
    >> Seq.length

let part1 = solve stepP1 (emptyKnot, emptyKnot, Map.empty)

let part2 = solve stepP2 (emptyKnot, Seq.replicate 9 emptyKnot, Map.empty)

let input = File.ReadLines "input.txt"
input |> Seq.map parse |> part1 |> Console.WriteLine
input |> Seq.map parse |> part2 |> Console.WriteLine
