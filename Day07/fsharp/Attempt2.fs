module Day07.Attempt2

open System
open System.Text.Json
open System.Text.Json.Serialization

let serializerOptions = JsonSerializerOptions(WriteIndented = true, ReferenceHandler = ReferenceHandler.Preserve)

type Node =
    { Name: string
      Size: int option
      Parent: Node option
      Children: Node list }

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
                  Size = None
                  Parent = Some acc
                  Children = List.empty }
                :: acc.Children }
    | f ->
        { acc with
            Children =
                { Name = f.Split(' ') |> Seq.last
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
          Size = None
          Parent = None
          Children = List.empty }
    // |> fun x -> x.root
    |> fun x -> JsonSerializer.Serialize(x, serializerOptions)
