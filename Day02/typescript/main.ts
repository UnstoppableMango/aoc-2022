type Throw =
  | 'rock'
  | 'paper'
  | 'scissors';

type Strategy =
  | 'win'
  | 'lose'
  | 'draw'

const toThrow = (x: string): Throw => {
  switch (x) {
    case 'A':
    case 'X':
      return 'rock';
    case 'B':
    case 'Y':
      return 'paper';
    case 'C':
    case 'Z':
      return 'scissors';
    default:
      throw Error('Invalid input');
  }
}

const strategize = (x: Strategy, opponent: Throw): Throw => {
  switch (x) {
    case 'draw':
      return opponent;
    case 'lose':
      return winners[winners[opponent]];
    case 'win':
      return winners[opponent];
  }
}

const strategies: Record<string, Strategy> = {
  'X': 'lose',
  'Y': 'draw',
  'Z': 'win',
}

const winners: Record<Throw, Throw> = {
  'rock': 'paper',
  'paper': 'scissors',
  'scissors': 'rock',
}

const throwScore: Record<Throw, number> = {
  'rock': 1,
  'paper': 2,
  'scissors': 3
}

const scoreGame = (opponent: Throw, player: Throw): number => {
  if (winners[opponent] === player)
    return 6;
  if (winners[player] === opponent)
    return 0;
  return 3;
}

const part1 = (input: string[]) => input.reduce((acc, curr) => {
  const opponent = toThrow(curr[0]);
  const player = toThrow(curr[2]);
  return acc + throwScore[player] + scoreGame(opponent, player);
}, 0);

const part2 = (input: string[]) => input.reduce((acc, curr) => {
  const opponent = toThrow(curr[0]);
  const strategy = strategies[curr[2]];
  const player = strategize(strategy, opponent);
  return acc + throwScore[player] + scoreGame(opponent, player);
}, 0);

// Learn more at https://deno.land/manual/examples/module_metadata#concepts
if (import.meta.main) {
  const input = await Deno.readTextFile('../input.txt');
  console.log(part1(input.trimEnd().split('\n')));
  console.log(part2(input.trimEnd().split('\n')));
}
