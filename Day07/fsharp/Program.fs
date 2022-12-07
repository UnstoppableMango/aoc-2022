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

let combinePath dir name =
    if dir = "/" then
        $"/{name}"
    else
        $"{dir}/{name}"

let build (map: Map<string, int>, dir: string) (cur: Line) =
    match cur with
    | Ls -> (map, dir)
    | Cd arg ->
        match arg with
        | "/" -> (map, "/")
        | ".." -> (map, dir.Substring(0, dir.LastIndexOf('/')))
        | name -> (map, combinePath dir name)
    | Dir name -> (map.Add((combinePath dir name), 0), dir)
    | File (size, name) -> (map.Change(dir, tryAdd size), dir)

let part1 input =
    input
    |> Seq.map parse
    |> Seq.fold build (Map.empty, "/") |> fst
    // |> Seq.map (fun x ->
    //     match x with
    //     | Cd cd -> $"cd {cd}"
    //     | Ls -> "ls"
    //     | Dir dir -> $"dir {dir}"
    //     | File (size, name) -> $"size: {size}, name: {name}")
    |> fun x -> JsonSerializer.Serialize(x, serializerOptions)

let part2 input = 14

let input = File.ReadLines "input.txt"
input |> part1 |> Console.WriteLine
input |> part2 |> Console.WriteLine
