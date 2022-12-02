open System
open System.IO

type GameResult =
    | Win
    | Lose
    | Tie

type Throw =
    | Rock
    | Paper
    | Scissors

let toTuple x = (Seq.head x, Seq.last x)

let toThrow =
    function
    | "A" | "X" -> Rock
    | "B" | "Y" -> Paper
    | "C" | "Z" -> Scissors
    | _ -> raise (InvalidOperationException())

let toGameResult =
    function
    | "X" -> Lose
    | "Y" -> Tie
    | "Z" -> Win
    | _ -> raise (InvalidOperationException())

let winner =
    function
    | Rock -> Paper
    | Paper -> Scissors
    | Scissors -> Rock

let loser = winner >> winner

type Throw with
    member x.losesTo y = (winner x) = y
    member x.winsAgainst y = (loser x) = y

let throwScore =
    function
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

let resultScore =
    function
    | Win -> 6
    | Tie -> 3
    | Lose -> 0

let gameResult =
    function
    | opponent, (player: Throw) when (player.losesTo opponent) -> player, Lose
    | opponent, (player: Throw) when (player.winsAgainst opponent) -> player, Win
    | opponent, _ -> opponent, Tie

let strategyResult =
    function
    | opponent, Win -> winner opponent, Win
    | opponent, Tie -> opponent, Tie
    | opponent, Lose -> loser opponent, Lose

let score (player, result) = (throwScore player) + (resultScore result)

let part1 (input: string seq) =
    input
    |> Seq.map (fun x -> x.Split(' '))
    |> Seq.map ((Seq.map toThrow) >> toTuple >> gameResult >> score)
    |> Seq.sum

let part2 (input: string seq) =
    input
    |> Seq.map (fun x -> x.Split(' '))
    |> Seq.map toTuple
    |> Seq.map ((fun (x, y) -> (toThrow x, toGameResult y)) >> strategyResult >> score)
    |> Seq.sum

let input = File.ReadLines "input.txt"
part1 input |> Console.WriteLine
part2 input |> Console.WriteLine
