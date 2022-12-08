open System
open System.IO
open System.Text.Json

let serializerOptions = JsonSerializerOptions(WriteIndented = true)

type Visibility =
    | Left
    | Right
    | Top
    | Bottom

type Visibilities = Map<int * int, Visibility list>

// let addVisibility (shouldAdd: bool) (direction: Visibility) (xs: Visibility list option) =
//     if shouldAdd then
//         match xs with
//         | Some l -> Some (direction :: l)
//         | None -> Some [ direction ]
//     else
//         xs

let combineMap (acc: Visibilities) (row: seq<(int * int) * Visibility option>) : Visibilities =
    let add v l =
        match (v, l) with
        | Some x, Some xs -> Some(x :: xs)
        | Some x, None -> Some [ x ]
        | None, _ -> l

    row |> Seq.fold (fun inner (i, v) -> inner.Change(i, add v)) acc

// let visible (direction: Visibility) (acc: Visibilities, tallest: int) (index: int * int, cur: int) =
//     (acc.Change(index, (addVisibility (cur > tallest) direction)), max cur tallest)

let mapVisible (direction: Visibility) (t: int) (i: int * int, c: int) =
    let visibility = if c > t then Some direction else None
    ((i, visibility), max c t)

// let leftVisible = visible Left
// let rightVisible = visible Right
// let topVisible = visible Top
// let bottomVisible = visible Bottom

// let mapVisible (direction: Visibility) (row: int seq) =
//     row |> Seq.mapFold (fun (col, tallest) cur ->
//         let v = if cur > tallest then Some direction else None
//         (v, (col + 1, max cur tallest))) (0, 0)

let solve (direction: Visibility) (visibilities: Visibilities) (input: seq<seq<(int * int) * int>>) =
    input
    |> Seq.map (Seq.mapFold (mapVisible direction) 0)
    |> Seq.map fst
    |> Seq.fold combineMap visibilities

let part1 (input: int seq seq) =
    let indexed = input |> Seq.mapi (fun r -> Seq.mapi (fun c x -> ((r, c), x)))
    let mutable visibilities: Visibilities = Map.empty
    visibilities <- indexed |> solve Left visibilities
    visibilities <- indexed |> Seq.map Seq.rev |> solve Right visibilities
    visibilities <- indexed |> Seq.transpose |> solve Bottom visibilities
    visibilities <- indexed |> Seq.transpose |> Seq.map Seq.rev |> solve Top visibilities

    visibilities
    |> Map.map (fun k v ->
        match k with
        | 0, _ -> true
        | _, 0 -> true
        | _ -> v.Length > 0)
    |> Map.values
    |> Seq.sumBy (fun x -> if x then 1 else 0)
// --------------------------------------------------------------
// let left = indexed |> Seq.map (Seq.fold leftVisible (Map.empty, 0)) |> Seq.map fst |> Seq.reduce combineMap
// left
// --------------------------------------------------------------
// let left = input |> Seq.map (mapVisible Left) |> Seq.map fst
// let right = input |> Seq.map Seq.rev |> Seq.map (mapVisible Right) |> Seq.map fst
// let bottom = input |> Seq.transpose |> Seq.map Seq.rev |> Seq.map ((mapVisible Bottom) >> fst) |> Seq.transpose
// let top = input |> Seq.transpose |> Seq.map ((mapVisible Top) >> fst) |> Seq.transpose
// left |> Seq.head |> Seq.zip (right |> Seq.head)

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
