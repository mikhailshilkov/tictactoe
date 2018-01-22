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
 
  type GameDTO = {
    Id: string
    Result: string
    BusyCells: Cell list
    Links: Link list
    Score: int
  }

  let serialize gameid (state: GameState) score =
    let index = function | One -> 1 | Two -> 2 | Three -> 3
    let mapMove move = { Name = sprintf "x%iy%i" (index move.X) (index move.Y); Value = move.By.ToString() }
    let mapLink i move = { Rel = sprintf "x%iy%i" (index move.X) (index move.Y); Href = sprintf "/game/%s/move/%i" gameid i }
    match state with
    | InProgress progress ->
      { Id = gameid
        BusyCells = progress.MovesDone |> List.map mapMove
        Links = progress.PossibleMoves |> List.mapi mapLink
        Result = null
        Score = score }
    | Finished f ->
      let result =
        match f.Outcome with
        | Tie -> "Tie"
        | Won X -> "You Win!"
        | Won O -> "You Loose!"
      { Id = gameid
        BusyCells = List.map mapMove f.MovesDone
        Links = []
        Result = result
        Score = score }