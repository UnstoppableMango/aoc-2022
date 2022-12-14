open System
open System.IO
open System.Text.Json

type Tree =
    | Node of Tree list
    | Leaf of int

let rec printable =
    function
    | Leaf n -> string n
    | Node n ->
        n |> List.map printable |> (fun x -> $"[{String.Join(',', x)}]")

// let rec parseInt i =
//     function
//     | ',' :: xs
//     | ']' :: xs -> i |> Seq.toArray |> String |> Int32.Parse
//     | c :: xs when Char.IsDigit(c) -> parseInt (c :: i) xs

// let rec parseLine t c =
//     match c with
//     | [] -> t
//     | '[' :: xs ->
//         Console.WriteLine $"Before '[': {t |> List.map printable}"
//         let res = ((parseLine [] xs) @ t)
//         Console.WriteLine $"After '[': {res |> List.map printable}"
//         res
//     | ']' :: xs -> parseLine [Node (List.rev t)] xs
//     | ',' :: xs -> parseLine t xs
//     | c :: xs when Char.IsDigit(c) ->
//         let l = xs |> List.takeWhile (fun x -> x <> ',' && x <> ']')
//         let leaf = Leaf (c :: l |> List.toArray |> String |> Int32.Parse)
//         let ys = xs |> List.skip ((List.length l) + 1)
//         match t with
//         | [] -> parseLine [leaf] ys
//         | ts -> parseLine (leaf :: ts) ys
//     | c -> failwith $"Unexpected char: {c}"

let (>>>) (f1: 'a -> 'b * 'c) (f2: 'b -> 'c -> 'd) =
    let apply a =
        let b, c = f1 a
        f2 b c
    apply

type Token =
    | Start
    | End
    | Data of int

let printToken =
    function
    | Start -> "["
    | End -> "]"
    | Data n -> string n

let rec tokenize (acc: Token list) =
    function
    | [] -> acc
    | '[' :: xs -> tokenize (Start :: acc) xs
    | ']' :: xs -> tokenize (End :: acc) xs
    | ',' :: xs -> tokenize acc xs
    | d :: ',' :: xs when Char.IsDigit(d) ->
        tokenize (Data (Int32.Parse(d |> string)) :: acc) xs
    | d :: ']' :: xs when Char.IsDigit(d) ->
        tokenize (Data (Int32.Parse(d |> string)) :: acc) (']' :: xs)
    | d1 :: d2 :: ',' :: xs when Char.IsDigit(d1) && Char.IsDigit(d2) ->
        tokenize (Data (Int32.Parse([d1; d2] |> string)) :: acc) xs
    | d1 :: d2 :: ']' :: xs when Char.IsDigit(d1) && Char.IsDigit(d2) ->
        tokenize (Data (Int32.Parse([d1; d2] |> string)) :: acc) (']' :: xs)
    | _ -> failwith "Unexpected input"

let parse =
    Seq.filter (not << String.IsNullOrWhiteSpace)
    >> Seq.map (Seq.toList >> tokenize [] >> List.rev)
    // >> Seq.map (Seq.toList >> (parseLine []) >> Node)
    >> Seq.pairwise

// let rec compare (a: char) (b: char) (l: char seq, r: char seq) =
//     match (a, b) with
//     | '[', '['
//     | ']', ']' -> compare (Seq.head l) (Seq.head r) (Seq.tail l, Seq.tail r)
//     | ca, cb when Char.IsDigit(ca) || Char.IsDigit(cb) -> failwith "TODO"
//     | _, ']' -> false
//     | ']', _ -> true
//     | _ -> failwith "Unexpected comparison"

let rec compare (left: Token list, right: Token list) =
    match (left, right) with
    | [], [] -> true // TODO: Review
    | Start :: ls, Start :: rs
    | End :: ls, End :: rs -> compare (ls, rs)
    | _ , End :: _ -> false
    | End :: _, _ -> true

let part1 (input: (Token list * Token list) seq) =
    input
    |> Seq.map compare

let part2 input = ""

let input = File.ReadLines "input.txt" |> Seq.take 2 |> parse
input |> part1 |> Seq.exactlyOne |> Console.WriteLine
input |> part2 |> Console.WriteLine
