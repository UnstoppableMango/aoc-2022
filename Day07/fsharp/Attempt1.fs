module Day07.Attempt1

open System
open System.Text.Json
open System.Text.Json.Serialization

let serializerOptions = JsonSerializerOptions(WriteIndented = true, ReferenceHandler = ReferenceHandler.Preserve)

type Directory =
    { Name: string
      Size: int
      Parent: Directory option
      Children: Directory list }

type Entry =
    | File of size: int * name: string
    | Dir of string

type Cd =
    | In of string
    | Out
    | Outermost

type Command =
    | Cd of Cd
    | Ls

type Line =
    | Command of Command
    | Entry of Entry

let parseCd input =
    match input with
    | "/" -> Outermost
    | ".." -> Out
    | x -> In x

let parseCommand (input: string) =
    let parts = input.Split(' ')

    match (parts |> Seq.head) with
    | "cd" -> parts |> Seq.last |> parseCd |> Cd
    | "ls" -> Ls
    | _ -> raise (InvalidOperationException("Unrecognized command"))

let parseEntry (input: string) =
    let parts = input.Split(' ') |> Seq.toList

    match parts with
    | [ "dir"; x ] -> Dir x
    | [ s; x ] -> File(size = (s |> int), name = x)
    | _ -> raise (InvalidOperationException("Unrecognized entry"))

let parse (input: string) =
    match input |> Seq.head with
    | '$' -> input |> Seq.skip 2 |> Seq.toArray |> String |> parseCommand |> Command
    | _ -> parseEntry input |> Entry

let processEntry (entry: Entry) (directory: Directory) =
    match entry with
    | Dir d ->
        { directory with
            Children =
                { Name = d
                  Children = List.empty
                  Parent = Some directory
                  Size = 0 }
                :: directory.Children }
    | File (size, name) -> { directory with Size = directory.Size + size }

let rec processCommand (command: Command) (directory: Directory) =
    match command with
    | Cd cd ->
        match cd with
        | In s -> directory.Children |> Seq.find (fun d -> d.Name = s)
        | Out -> directory.Parent.Value
        | Outermost ->
            match directory.Parent with
            | Some d -> processCommand command d
            | None -> directory
    | Ls -> directory

let traverse (acc: Directory) (cur: Line) =
    match cur with
    | Command command -> processCommand command acc
    | Entry entry -> processEntry entry acc

let part1 input =
    input
    |> Seq.map parse
    |> Seq.take 21
    |> Seq.fold
        traverse
        { Name = "/"
          Size = 0
          Parent = None
          Children = List.empty }
    // |> fun x -> x.root
    |> fun x -> JsonSerializer.Serialize(x, serializerOptions)
