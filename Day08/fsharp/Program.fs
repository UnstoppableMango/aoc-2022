open System
open System.IO
open System.Text.Json

let serializerOptions = JsonSerializerOptions(WriteIndented = true)

type Visibility =
    | Left
    | Right
    | Top
    | Bottom

// type Visibilities = Map<int * int, Visibility list>
//
// let addVisibility (direction: Visibility) (shouldAdd: bool) (xs: Visibility list option) =
//     if shouldAdd then
//         match xs with
//         | Some l -> Some (direction :: l)
//         | None -> Some [ direction ]
//     else
//         xs
//
// let visible (direction: Visibility) (row: int) (op: int -> int) (acc: Visibilities, tallest: int, col: int) (cur: int) =
//     (acc.Change((row, col), (addVisibility direction (cur > tallest))), max cur tallest, op col)
//
// let leftVisible = visible Left
// let rightVisible = visible Right
// let topVisible = visible Top
// let bottomVisible = visible Bottom

let mapVisible (direction: Visibility) (row: int seq) =
    row |> Seq.mapFold (fun (col, tallest) cur ->
        let v = if cur > tallest then Some direction else None
        (v, (col + 1, max cur tallest))) (0, 0)

let part1 (input: int seq seq) =
    let left = input |> Seq.map (mapVisible Left) |> Seq.map fst
    let right = input |> Seq.map Seq.rev |> Seq.map (mapVisible Right) |> Seq.map fst
    let bottom = input |> Seq.transpose |> Seq.map Seq.rev |> Seq.map ((mapVisible Bottom) >> fst) |> Seq.transpose
    let top = input |> Seq.transpose |> Seq.map ((mapVisible Top) >> fst) |> Seq.transpose
    left |> Seq.head |> Seq.zip (right |> Seq.head)

let part2 input = 69

let testInput =
    [ [ 3; 0; 3; 7; 3 ]
      [ 2; 5; 5; 1; 2 ]
      [ 6; 5; 3; 3; 2 ]
      [ 3; 3; 5; 4; 9 ]
      [ 3; 5; 3; 9; 0 ] ]

testInput
|> List.map List.toSeq
|> List.toSeq
|> part1
|> fun x -> JsonSerializer.Serialize(x, serializerOptions) |> Console.WriteLine

let input = File.ReadLines "input.txt"
// input |> Seq.map (Seq.map (Char.GetNumericValue >> int)) |> part1 |> JsonSerializer.Serialize |> Console.WriteLine
// input |> Seq.map (Seq.map (Char.GetNumericValue >> int)) |> part2 |> Console.WriteLine
