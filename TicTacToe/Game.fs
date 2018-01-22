namespace TicTacToe

type Player = X | O

module Player =
  let other = function | X -> O | O -> X

type Position = One | Two | Three

type Move = {
  X: Position
  Y: Position
  By: Player
}

type RunningGame = {
  MovesDone: Move list
  PossibleMoves: Move list
}

type GameOutcome = Won of Player | Tie

type FinishedGame = {
  MovesDone: Move list
  Outcome: GameOutcome
}

type GameState = 
  | Finished of FinishedGame
  | InProgress of RunningGame

module Game =
  let initialState = 
    let positions = [One; Two; Three]
    let cells = seq { 
      for x in positions do
         for y in positions do
            yield { X = x; Y = y; By = X }
      }
    { MovesDone = []; PossibleMoves = List.ofSeq cells }

  let evaluate (history: Move list) = 
    let playerWon player = 
      let hisMoves = history |> List.filter (fun m -> m.By = player)
      let hasCell x y = List.exists (fun m -> m.X = x && m.Y = y) hisMoves
      let hasRow i = hasCell One i && hasCell Two i && hasCell Three i
      let hasColumn i = hasCell i One && hasCell i Two && hasCell i Three
      let hasLeftDiagonal = hasCell One One && hasCell Two Two && hasCell Three Three
      let hasRightDiagonal = hasCell Three One && hasCell Two Two && hasCell One Three
      hasRow One || hasRow Two || hasRow Three
      || hasColumn One || hasColumn Two || hasColumn Three
      || hasLeftDiagonal || hasRightDiagonal
    if playerWon X then Some(Won X)
    elif playerWon O then Some(Won O)
    elif List.length history = 9 then Some Tie
    else None

  let makeMove (game: RunningGame) (move: Move): GameState =
    let movesDone = move :: game.MovesDone
    match evaluate movesDone with
    | Some result -> Finished { MovesDone = movesDone; Outcome = result }
    | None ->
      let possibleMoves = 
        List.except [move] game.PossibleMoves
        |> List.map (fun m -> { m with By = Player.other m.By })
      InProgress { MovesDone = movesDone; PossibleMoves = possibleMoves }

  let makeRound player1 player2 gameState =
    let newGameState = player1 gameState |> makeMove gameState
    match newGameState with
    | Finished _ -> newGameState
    | InProgress p -> player2 p |> makeMove p

  let isFinished = function | Finished _ -> true | _ -> false
  