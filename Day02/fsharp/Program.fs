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

type Game = { Player: Throw; Opponent: Throw }
type Strategy = { Opponent: Throw; Result: GameResult }

let toThrow input =
    match input with
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

let getThrow strategy =
    match (strategy.Opponent, strategy.Result) with
    | Rock, Win | Paper, Tie | Scissors, Lose -> Paper
    | Rock, Tie | Paper, Lose | Scissors, Win -> Rock
    | Rock, Lose | Paper, Win | Scissors, Tie -> Scissors

let toGame throws =
    { Player = (Seq.last throws)
      Opponent = (Seq.head throws) }

let toStrategy input =
    { Opponent = (Seq.head input |> toThrow)
      Result = (Seq.last input |> toGameResult) }

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

let result game =
    match (game.Player, game.Opponent) with
    | Rock, Scissors | Paper, Rock | Scissors, Paper -> Win
    | Rock, Rock | Paper, Paper | Scissors, Scissors -> Tie
    | Scissors, Rock | Rock, Paper | Paper, Scissors -> Lose

let gameScore game =
    (throwScore game.Player) + (result game |> resultScore)

let strategyScore strategy =
    (resultScore strategy.Result) + (getThrow strategy |> throwScore)

let part1 (input: string seq) =
    input
    |> Seq.map (fun x -> x.Split(' '))
    |> Seq.map ((Seq.map toThrow) >> toGame)
    |> Seq.map gameScore
    |> Seq.sum

let part2 (input: string seq) =
    input
    |> Seq.map (fun x -> x.Split(' '))
    |> Seq.map toStrategy
    |> Seq.map strategyScore
    |> Seq.sum

let input = File.ReadLines "input.txt"
part1 input |> Console.WriteLine
part2 input |> Console.WriteLine
