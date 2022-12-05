open System
open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open System.Text.Json

type Operation = { Num: int; Source: int; Target: int }

let toStack input =
    let index = input |> Seq.last |> Char.GetNumericValue |> int
    let stack = Seq.takeWhile Char.IsLetter input |> Seq.toList
    (index, stack)

let parseStackRow input =
    input |> Seq.filter Char.IsLetterOrDigit |> toStack

let parseStacks (input: string seq) =
    input
    |> Seq.transpose
    |> Seq.filter (fun x -> x |> Seq.exists Char.IsLetter)
    |> Seq.map parseStackRow
    |> Seq.fold
        (fun (acc: ImmutableDictionary<int, char list>) x -> acc.Add(fst x, snd x))
        ImmutableDictionary<int, char list>.Empty

let parseOperations (input: string seq) =
    input
    |> Seq.map (fun x -> x.Split(' '))
    |> Seq.map (fun x ->
        { Num = Array.item 1 x |> int
          Source = Array.item 3 x |> int
          Target = Array.item 5 x |> int })

let move1Core (source, target) =
    let x = Seq.head source
    let xs = Seq.tail source
    (Seq.toList xs, x :: target)

let rec move1 num stacks =
    match num with
    | 0 -> stacks
    | _ -> move1 (num - 1) (move1Core stacks)

let move2 num (source, target) =
    let x = source |> Seq.take num |> Seq.toList
    let xs = source |> Seq.skip num |> Seq.toList
    (xs, List.append x target)

let solve input algo =
    let stacks = input |> Seq.take 9 |> parseStacks
    let operations = input |> Seq.skip 10 |> parseOperations

    let moved =
        operations
        |> Seq.fold
            (fun (acc: ImmutableDictionary<int, char list>) x ->
                let source = acc.Item x.Source
                let target = acc.Item x.Target
                let moved = algo x.Num (source, target)

                acc
                    .SetItem(x.Source, (fst moved))
                    .SetItem(x.Target, (snd moved)))
            stacks
        |> Seq.map (fun x -> x.Value |> Seq.head)
        |> Seq.toArray

    String(moved)

let part1 input = solve input move1

let part2 input = solve input move2

let input = File.ReadLines "input.txt"

// input
// |> Seq.take 9
// |> parseStacks
// |> (fun x -> JsonSerializer.Serialize(x, JsonSerializerOptions(WriteIndented = true)))
// |> Console.WriteLine

part1 input |> Console.WriteLine
part2 input |> Console.WriteLine
