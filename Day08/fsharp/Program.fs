open System
open System.IO
open System.Text.Json

let serializerOptions = JsonSerializerOptions(WriteIndented = true)

let solve (visibilities: Map<int * int, bool>) (input: seq<seq<(int * int) * int>>) =
    let foldInner (v: Map<int * int, bool>, t: int) (i: int * int, c: int) =
        let visible = c > t

        let add o =
            match o with
            | Some x -> Some(x || visible)
            | None -> Some visible

        (v.Change(i, add), max c t)

    let fold (v: Map<int * int, bool>) (c: seq<(int * int) * int>) = c |> Seq.fold foldInner (v, 0) |> fst

    input |> Seq.fold fold visibilities

let part1 (input: int list list) =
    let numRows = Seq.length input
    let numCols = Seq.head input |> Seq.length
    let indexed = input |> Seq.mapi (fun r -> Seq.mapi (fun c x -> ((r, c), x)))
    let mutable visibilities = Map.empty
    visibilities <- indexed |> solve visibilities
    visibilities <- indexed |> Seq.map Seq.rev |> solve visibilities
    visibilities <- indexed |> Seq.transpose |> solve visibilities
    visibilities <- indexed |> Seq.transpose |> Seq.map Seq.rev |> solve visibilities

    visibilities
    |> Seq.map (fun kvp ->
        match kvp.Key with
        | r, _ when r = 0 || r = numRows - 1 -> true
        | _, c when c = 0 || c = numCols - 1 -> true
        | _ -> kvp.Value)
    |> Seq.sumBy (fun x -> if x then 1 else 0)

let part2 (input: int list list) =
    let arr = array2D input

    let sonic (row: int) (col: int) (x: int) =
        let count v =
            v
            |> Array.tryFindIndex (fun i -> i >= x)
            |> function
                | Some i -> i + 1
                | None -> Array.length v

        let left = arr[row, .. col - 1] |> Array.rev |> count
        let right = arr[row, col + 1 ..] |> count
        let top = arr[.. row - 1, col] |> Array.rev |> count
        let bottom = arr[row + 1 .., col] |> count

        left * right * top * bottom

    input |> List.mapi (fun r -> List.mapi (sonic r)) |> List.collect id |> List.max

let input = File.ReadLines "input.txt" |> Seq.toList

input
|> List.map (Seq.map (Char.GetNumericValue >> int) >> Seq.toList)
|> part1
|> Console.WriteLine

input
|> List.map (Seq.map (Char.GetNumericValue >> int) >> Seq.toList)
|> part2
|> Console.WriteLine
