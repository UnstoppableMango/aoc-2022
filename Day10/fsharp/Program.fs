open System
open System.IO
open System.Text.Json

let serializerOptions = JsonSerializerOptions(WriteIndented = true)

type Instruction =
    | AddX of int
    | NoOp

let parse (input: string) =
    let parts = input.Split(' ')

    match parts[0] with
    | "addx" -> AddX(Int32.Parse(parts[1]))
    | "noop" -> NoOp
    | _ -> failwith "Unexpected input"

type Computer = { X: int; Cycle: int }

let newComputer = { X = 1; Cycle = 0 }

let cycle c = { c with Cycle = c.Cycle + 1 }

let addX n c = { c with X = c.X + n }

let compute (c: Computer) (i: Instruction) =
    match i with
    | AddX x ->
        let a = cycle c
        let b = cycle a
        (seq { a; b }, addX x b)
    | NoOp ->
        let n = c |> cycle
        (n |> Seq.singleton, n)

let drawRow (cycles: Computer seq) =
    cycles
    |> Seq.mapi (fun i c -> List.contains i [c.X - 1 .. c.X + 1])
    |> Seq.map (fun x -> if x then '#' else '.')
    |> Seq.toArray
    |> String

let part1 (input: Instruction seq) =
    input
    |> Seq.mapFold compute newComputer
    |> (fst >> Seq.collect id)
    |> Seq.filter (fun x -> List.contains x.Cycle [ 20; 60; 100; 140; 180; 220 ])
    |> Seq.map (fun c -> c.X * c.Cycle)
    |> Seq.sum

let part2 input =
    input
    |> Seq.mapFold compute { X = 1; Cycle = 0 }
    |> (fst >> Seq.collect id)
    |> Seq.splitInto 6
    |> Seq.map drawRow
    |> fun x -> String.Join('\n', x)

let input = File.ReadLines "input.txt"
input |> Seq.map parse |> part1 |> Console.WriteLine
input |> Seq.map parse |> part2 |> Console.WriteLine
