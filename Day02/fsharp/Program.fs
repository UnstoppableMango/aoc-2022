open System
open System.IO
open Shared

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
    | x -> failwith $"Unexpected throw {x}"

let toGameResult =
    function
    | "X" -> Lose
    | "Y" -> Tie
    | "Z" -> Win
    | x -> failwith $"Unexpected result {x}"

let parse snd x =
    let parts = xString.split ' ' x
    toThrow parts[0], snd parts[1]

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

let solve into calculate = Seq.map (parse into >> calculate >> score) >> Seq.sum

let part1 = solve toThrow gameResult
let part2 = solve toGameResult strategyResult

let input = File.ReadLines "input.txt"
input |> part1 |> Console.WriteLine
input |> part2 |> Console.WriteLine
