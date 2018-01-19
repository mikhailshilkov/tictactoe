namespace TicTacToe.Functions

open TicTacToe

module Api = 

  [<CLIMutable>]
  type GameRequest = { 
    Name: string 
  }

  type Cell = {
    Name: string
    Value: string
  }

  type Link = {
    Rel: string
    Href: string
  }
 
  type MoveResult = {
    Result: string
    Position: Cell list
    Links: Link list
    Score: int
  }

  let serialize gameid (state: GameState) score =
    let index = function | Left -> 1 | Middle -> 2 | Right -> 3
    let mapMove move = { Name = sprintf "x%iy%i" (index move.X) (index move.Y); Value = move.By.ToString() }
    let mapLink i move = { Rel = sprintf "x%iy%i" (index move.X) (index move.Y); Href = sprintf "http://localhost:7071/api/game/%s/%i" gameid i }
    match state with
    | InProgress progress ->
      { Position = progress.MovesDone |> List.map mapMove
        Links = progress.PossibleMoves |> List.mapi mapLink
        Result = null
        Score = score }
    | Finished (r, ms) ->
      let result =
        match r with
        | Tie -> "Tie"
        | Won X -> "You Win!"
        | Won O -> "You Loose!"
      { Position = List.map mapMove ms
        Links = []
        Result = result
        Score = score }