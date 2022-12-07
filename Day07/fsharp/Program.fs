open System
open System.IO
open System.Text.Json
open System.Text.Json.Serialization

// type Directory =
//     { Name: string
//       Size: int
//       Parent: Directory option
//       Children: Directory list }
//
// type Entry =
//     | File of size: int * name: string
//     | Dir of string
//
// type Cd =
//     | In of string
//     | Out
//     | Outermost
//
// type Command =
//     | Cd of Cd
//     | Ls
//
// type Line =
//     | Command of Command
//     | Entry of Entry
//
// let parseCd input =
//     match input with
//     | "/" -> Outermost
//     | ".." -> Out
//     | x -> In x
//
// let parseCommand (input: string) =
//     let parts = input.Split(' ')
//
//     match (parts |> Seq.head) with
//     | "cd" -> parts |> Seq.last |> parseCd |> Cd
//     | "ls" -> Ls
//     | _ -> raise (InvalidOperationException("Unrecognized command"))
//
// let parseEntry (input: string) =
//     let parts = input.Split(' ') |> Seq.toList
//
//     match parts with
//     | [ "dir"; x ] -> Dir x
//     | [ s; x ] -> File(size = (s |> int), name = x)
//     | _ -> raise (InvalidOperationException("Unrecognized entry"))
//
// let parse (input: string) =
//     match input |> Seq.head with
//     | '$' -> input |> Seq.skip 2 |> Seq.toArray |> String |> parseCommand |> Command
//     | _ -> parseEntry input |> Entry
//
// let processEntry (entry: Entry) (directory: Directory) =
//     match entry with
//     | Dir d ->
//         { directory with
//             Children =
//                 { Name = d
//                   Children = List.empty
//                   Parent = Some directory
//                   Size = 0 }
//                 :: directory.Children }
//     | File (size, name) -> { directory with Size = directory.Size + size }
//
// let rec processCommand (command: Command) (directory: Directory) =
//     match command with
//     | Cd cd ->
//         match cd with
//         | In s -> directory.Children |> Seq.find (fun d -> d.Name = s)
//         | Out -> directory.Parent.Value
//         | Outermost ->
//             match directory.Parent with
//             | Some d -> processCommand command d
//             | None -> directory
//     | Ls -> directory
//
// let traverse (acc: Directory) (cur: Line) =
//     match cur with
//     | Command command -> processCommand command acc
//     | Entry entry -> processEntry entry acc

type NodeType =
    | File
    | Dir

type Node =
    { Name: string
      Type: NodeType
      Size: int option
      Parent: Node option
      Children: Node list }

type PrintableNode =
    { Name: string
      Size: int option
      Children: PrintableNode seq }

let rec printable (node: Node) : PrintableNode =
    { Name = node.Name
      Size = node.Size
      Children = node.Children |> Seq.map printable }

type Node with

    member x.root =
        match x.Parent with
        | Some p -> p.root
        | None -> x

let traverse2 (acc: Node) (cur: string) =
    match cur with
    | c when c.StartsWith("$") ->
        match c |> Seq.skip 2 |> Seq.toArray |> String with
        | cd when cd.StartsWith("cd") ->
            match cd |> Seq.skip 3 |> Seq.toArray |> String with
            | "/" -> acc.root
            | ".." ->
                match acc.Parent with
                | Some p -> p
                | None -> raise (InvalidOperationException("Bad input"))
            | d -> acc.Children |> Seq.find (fun x -> x.Name = d)
        | ls when ls.StartsWith("ls") -> acc // TODO: Still feels wrong
        | _ -> raise (InvalidOperationException("Unknown command"))
    | d when d.StartsWith("dir") ->
        { acc with
            Children =
                { Name = d.Split(' ') |> Seq.last
                  Type = Dir
                  Size = None
                  Parent = Some acc
                  Children = List.empty }
                :: acc.Children }
    | f ->
        { acc with
            Children =
                { Name = f.Split(' ') |> Seq.last
                  Type = File
                  Size = f.Split(' ') |> Seq.head |> int |> Some
                  Parent = Some acc
                  Children = List.empty }
                :: acc.Children }

let part1 input =
    input
    // |> Seq.map parse
    |> Seq.take 21
    |> Seq.fold
        traverse2
        { Name = "/"
          Type = Dir
          Size = None
          Parent = None
          Children = List.empty }
    |> fun x -> x.root
    |> printable
    |> fun x -> JsonSerializer.Serialize(x, JsonSerializerOptions(WriteIndented = true))

let part2 input = 14

let input = File.ReadLines "input.txt"
input |> part1 |> Console.WriteLine
input |> part2 |> Console.WriteLine
