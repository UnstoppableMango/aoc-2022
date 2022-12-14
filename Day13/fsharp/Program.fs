open System
open System.IO

type Tree =
    | Node of Tree list
    | Leaf of int

// let rec parseInt i =
//     function
//     | ',' :: xs
//     | ']' :: xs -> i |> Seq.toArray |> String |> Int32.Parse
//     | c :: xs when Char.IsDigit(c) -> parseInt (c :: i) xs
//
// let rec parseLine t c =
//     match c with
//     | [] -> t
//     | '[' :: xs -> parseLine (Node [] :: t) xs
//     | ']' :: xs -> parseLine t xs
//     | c :: xs when Char.IsDigit(c) ->
//         let a = xs |> Seq.takeWhile (fun x -> x <> ',' && x <> ']') |> Seq.toArray
//         let i = String a |> Int32.Parse
//         let ys = xs |> List.skip (Array.length a)
//         parseLine (Leaf i :: t) ys
//     | c -> failwith $"Unexpected char: {c}"

let parseLine (line: string) (tree: Tree) =
    tree

let parse =
    Seq.filter (not << String.IsNullOrWhiteSpace)
    // >> Seq.map (Seq.toList >> parseLine [])
    >> Seq.map (fun l -> Seq.foldBack parseLine l (Node []))
    >> Seq.pairwise

let rec compare (a: char) (b: char) (l: char seq, r: char seq) =
    match (a, b) with
    | '[', '['
    | ']', ']' -> compare (Seq.head l) (Seq.head r) (Seq.tail l, Seq.tail r)
    | ca, cb when Char.IsDigit(ca) || Char.IsDigit(cb) ->
        failwith "TODO"
    | _, ']' -> false
    | ']', _ -> true
    | _ -> failwith "Unexpected comparison"

let part1 input = 69
let part2 input = 69

let input = File.ReadLines "input.txt" |> parse
input |> part1 |> Console.WriteLine
input |> part2 |> Console.WriteLine
