namespace TicTacToe

type Player = X | O

module Player =
  let other = function | X -> O | O -> X

type Position = Left | Middle | Right

type Move = {
  X: Position
  Y: Position
  By: Player
}

type Progress = {
  MovesDone: Move list
  PossibleMoves: Move list
}

type GameResult = Won of Player | Tie

type GameState = 
  | Finished of GameResult * Move list
  | InProgress of Progress

module Game =
  let initialState = 
    let positions = [Left; Middle; Right]
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
      let hasRow i = hasCell Left i && hasCell Middle i && hasCell Right i
      let hasColumn i = hasCell i Left && hasCell i Middle && hasCell i Right
      let hasLeftDiagonal = hasCell Left Left && hasCell Middle Middle && hasCell Right Right
      let hasRightDiagonal = hasCell Right Left && hasCell Middle Middle && hasCell Left Right
      hasRow Left || hasRow Middle || hasRow Right
      || hasColumn Left || hasColumn Middle || hasColumn Right
      || hasLeftDiagonal || hasRightDiagonal
    if playerWon X then Some(Won X)
    elif playerWon O then Some(Won O)
    elif List.length history = 9 then Some Tie
    else None

  let makeMove (game: Progress) (move: Move) =
    let movesDone = move :: game.MovesDone
    match evaluate movesDone with
    | Some result -> Finished (result, movesDone)
    | None ->
      let possibleMoves = 
        List.except [move] game.PossibleMoves
        |> List.map (fun move -> { move with By = Player.other move.By })
      InProgress { MovesDone = movesDone; PossibleMoves = possibleMoves }

  let makeRound player1 player2 gameState =
    let newGameState = player1 gameState |> makeMove gameState
    match newGameState with
    | Finished _ -> newGameState
    | InProgress p -> player2 p |> makeMove p
  