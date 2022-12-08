open System
open System.IO
open System.Text.Json

let serializerOptions = JsonSerializerOptions(WriteIndented = true)

type Line =
    | Cd of string
    | Ls
    | Dir of string
    | File of size: int * name: string

let parseEntry (input: string) =
    if input.StartsWith("dir") then
        Dir input[4..]
    else
        let parts = input.Split(' ')
        File(size = (parts |> Array.head |> int), name = (parts |> Array.last))

let parseCommand (input: string) =
    if input.StartsWith("cd") then Cd input[3..] else Ls

let parse (input: string) =
    if input.StartsWith('$') then
        parseCommand input[2..]
    else
        parseEntry input

let tryAdd (a: int) (b: int option) =
    match b with
    | Some x -> Some (a + x)
    | None -> Some a

let allParents (dirs: string list) =
    dirs
    |> List.mapi (fun i _ -> dirs[i..])

let addSize size (sizes: Map<string list, int>) (cur: string list) =
    sizes.Change(cur, tryAdd size)

let build (sizes: Map<string list, int>, cur: string list) (line: Line) =
    match line with
    | Ls -> (sizes, cur)
    | Cd arg ->
        match arg with
        | ".." -> (sizes, cur |> List.tail)
        | name -> (sizes, name :: cur)
    | Dir name -> (sizes.Add(name :: cur, 0), cur)
    | File (size, name) -> ((cur |> allParents |> List.fold (addSize size) sizes), cur)

let part1 input =
    input
    |> Seq.map parse
    |> Seq.fold build (Map.empty, [])
    |> fst
    |> Map.values
    |> Seq.filter (fun x -> x <= 100_000)
    |> Seq.sum

let part2 input =
    input
    |> Seq.map parse
    |> Seq.fold build (Map.empty, [])
    |> fst
    |> fun m -> (30_000_000 - (70_000_000 - m.Item ["/"]), m)
    |> fun (r, m) -> m.Values |> Seq.filter ((<) r)
    |> Seq.min

let input = File.ReadLines "input.txt"
input |> part1 |> Console.WriteLine
input |> part2 |> Console.WriteLine
