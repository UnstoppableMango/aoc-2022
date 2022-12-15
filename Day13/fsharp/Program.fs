open System
open System.IO

type Token =
    | Start
    | End
    | Data of int

type Packet = Token list

let printToken =
    function
    | Start -> "["
    | End -> "]"
    | Data n -> string n

let rec tokenize (acc: Packet) =
    function
    | [] -> acc
    | '[' :: xs -> tokenize (Start :: acc) xs
    | ']' :: xs -> tokenize (End :: acc) xs
    | ',' :: xs -> tokenize acc xs
    | d :: ',' :: xs when Char.IsDigit(d) -> tokenize (Data(Int32.Parse(d |> string)) :: acc) xs
    | d :: ']' :: xs when Char.IsDigit(d) -> tokenize (Data(Int32.Parse(d |> string)) :: acc) (']' :: xs)
    | d1 :: d2 :: ',' :: xs when Char.IsDigit(d1) && Char.IsDigit(d2) ->
        tokenize (Data(Int32.Parse(String.Concat(d1, d2))) :: acc) xs
    | d1 :: d2 :: ']' :: xs when Char.IsDigit(d1) && Char.IsDigit(d2) ->
        tokenize (Data(Int32.Parse(String.Concat(d1, d2))) :: acc) (']' :: xs)
    | _ -> failwith "Unexpected input"

let parse =
    Seq.filter (not << String.IsNullOrWhiteSpace)
    >> Seq.map<string, Packet> (Seq.toList >> tokenize [] >> List.rev)

let rec compare (left: Packet, right: Packet) =
    match (left, right) with
    | [], [] -> true
    | Data l :: ls, Data r :: rs -> if l = r then compare (ls, rs) else l < r
    | Data l :: ls, rs -> compare (([ Start; Data l; End ] @ ls), rs)
    | ls, Data r :: rs -> compare (ls, ([ Start; Data r; End ] @ rs))
    | Start :: ls, Start :: rs
    | End :: ls, End :: rs -> compare (ls, rs)
    | _, End :: _ -> false
    | End :: _, _ -> true
    | _, [ Start ]
    | [ Start ], _ -> failwith "Shouldn't exist"
    | l, r -> failwith $"Unexpected: l: {l}, r: {r}"

let divider n : Packet = [ Start; Start; Data n; End; End ]

let dividers = [ divider 2; divider 6 ]

let part1 =
    Seq.chunkBySize 2
    >> Seq.map (fun x -> compare (Seq.head x, Seq.last x))
    >> Seq.indexed
    >> Seq.filter snd
    >> Seq.map (fst >> (+) 1)
    >> Seq.sum

let part2 =
    Seq.append dividers
    >> Seq.sortWith (fun a b ->
        if compare (a, b) then -1
        else if compare (b, a) then 1
        else 0)
    >> Seq.indexed
    >> Seq.filter (fun (_, x) -> List.contains x dividers)
    >> Seq.map (fst >> (+) 1)
    >> Seq.fold (*) 1

let input = File.ReadLines "input.txt" |> parse
input |> part1 |> Console.WriteLine
input |> part2 |> Console.WriteLine
