open System
open System.IO
open System.Text.Json
open FSharp.Core.Operators.Checked

type Monkey =
    { Id: int
      Items: bigint array
      Op: string
      GetChangeBy: bigint -> bigint
      DivisibleBy: int
      Inspected: uint
      WhenTrue: int
      WhenFalse: int }

let printMonkey m =
    $"Id: {m.Id}, Items: {JsonSerializer.Serialize m.Items}, Inspected: {m.Inspected}"

let parseMonkey (lines: string seq) =
    let list = lines |> Seq.toList

    let value (x: string) = x.Split(": ")[1]

    let mId = list[0][7] |> Char.GetNumericValue |> int

    let items =
        list[1]
        |> value
        |> (fun x -> x.Split(", "))
        |> Seq.map (Int32.Parse >> bigint)
        |> Seq.toArray

    let opParts =
        list[2]
        |> value
        |> (fun x -> x.Substring 10)
        |> (fun x -> x.Split(' '))

    let op = opParts[0]
    let changeBy =
        match opParts[1] with
        | "old" -> id
        | n -> (fun _ -> Int32.Parse n |> bigint)

    let divisibleBy = list[3] |> value |> (fun x -> x.Substring 12) |> Int32.Parse
    let whenTrue = list[4] |> (fun x -> x.Substring(x.Length - 1)) |> Int32.Parse
    let whenFalse = list[5] |> (fun x -> x.Substring(x.Length - 1)) |> Int32.Parse

    { Id = mId
      Items = items
      Op = op
      GetChangeBy = changeBy
      DivisibleBy = divisibleBy
      Inspected = 0u
      WhenTrue = whenTrue
      WhenFalse = whenFalse }

let parse =
    Seq.filter (not << String.IsNullOrWhiteSpace)
    >> Seq.chunkBySize 6
    >> Seq.toList
    >> Seq.fold
        (fun (m: Map<int, Monkey>) c ->
            let monkey = parseMonkey c
            m.Add(monkey.Id, monkey))
        Map.empty<int, Monkey>

let composeMany x f =
    Seq.init (x - 1) (fun _ -> f) |> Seq.fold (>>) f

let addInspected id (monkeys: Map<int, Monkey>) =
    monkeys.Change(
        id,
        Option.map (fun m ->
            { m with
                Inspected = m.Inspected + (uint m.Items.Length)
                Items = Array.empty })
    )

let throw (monkeys: Map<int, Monkey>) (id: int, item: bigint) =
    monkeys.Change(id, Option.map (fun m -> { m with Items = Array.append m.Items [| item |] }))

let getBored (x: bigint) =
    x / 3I |> float |> Math.Floor |> bigint

let calculate bore (m: Monkey) (item: bigint) = async {
    let changeBy = m.GetChangeBy item
    let n =
        match m.Op with
        | "+" -> item + changeBy
        | "*" -> item * changeBy
        | _ -> failwith "Unexpected Op"
    let b = bore n
    let f = if b % (bigint m.DivisibleBy) = 0I then m.WhenTrue else m.WhenFalse
    return (f, b)
}

let inspect bore (monkeys: Map<int, Monkey>) (i: int) =
    let current = monkeys[i]

    current.Items
    |> Array.Parallel.map (calculate bore current)
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Array.fold throw monkeys
    |> addInspected current.Id

let round bore (m: Map<int, Monkey>) =
    Seq.fold (inspect bore) m (seq { 0 .. m.Count - 1 })

let solve rounds bore =
    composeMany rounds (round bore)
    >> (fun x -> x.Values |> Seq.toList)
    >> List.map (fun m -> m.Inspected)
    >> List.sortDescending
    >> List.take 2
    >> List.map bigint
    >> List.fold (*) 1I

let part1 = solve 20 (fun x -> x / 3I |> float |> Math.Floor |> bigint)

let part2 (monkeys: Map<int, Monkey>) =
    let factor = monkeys.Values |> Seq.map (fun x -> x.DivisibleBy) |> Seq.fold (*) 1 |> bigint
    solve 10_000 (fun x -> x % factor |> double |> Math.Floor |> bigint) monkeys

let input = File.ReadLines "input.txt" |> parse
input |> part1 |> Console.WriteLine
input |> part2 |> Console.WriteLine
