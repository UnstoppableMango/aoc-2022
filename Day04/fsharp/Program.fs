open System
open System.IO
open System.Text.Json

type SectionGroup = { Start: int
                      End: int }

type SectionIds = { First: SectionGroup
                    Second: SectionGroup }

type SectionGroup with
    member x.IsSubset y =
        Set.isSubset (seq { x.Start .. x.End } |> Set) (seq { y.Start .. y.End } |> Set)

    member x.Intersect y =
        Set.intersect (seq { x.Start .. x.End } |> Set) (seq { y.Start .. y.End } |> Set)

let parseGroup (x: string) =
    let split = x.Split('-')
    let start = (Seq.head split) |> int
    let last = (Seq.last split) |> int
    { Start = start
      End = last }

let parse (x: string) =
    let split = x.Split(',')
    { First = parseGroup (Seq.head split)
      Second = parseGroup (Seq.last split) }

let solve1 (x: SectionIds) =
    x.First.IsSubset x.Second || x.Second.IsSubset x.First

let solve2 (x: SectionIds) =
    (x.First.Intersect x.Second).Count > 0

let part1 (input: string seq) =
    input
    |> Seq.map parse
    |> Seq.map solve1
    |> Seq.map (fun x -> if x then 1 else 0)
    |> Seq.sum

let part2 input =
    input
    |> Seq.map parse
    |> Seq.map solve2
    |> Seq.map (fun x -> if x then 1 else 0)
    |> Seq.sum

let input = File.ReadLines "input.txt"
part1 input |> JsonSerializer.Serialize |> Console.WriteLine
part2 input |> Console.WriteLine
