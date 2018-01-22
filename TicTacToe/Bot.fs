namespace TicTacToe

module Bot =

  let private random = System.Random()

  let private winNow player (game: RunningGame) =
    let isWin = function | Finished { Outcome = Won x } when x = player -> true | _ -> false
    game.PossibleMoves
    |> List.tryFind (fun move -> Game.makeMove game move |> isWin)

  let private notLoseNow (game: RunningGame) =
    let canLose = function 
      | InProgress p -> match winNow X p with | Some _ -> true | None -> false
      | _ -> false
    let notLosingMoves =
      game.PossibleMoves
      |> List.filter (fun move -> Game.makeMove game move |> canLose |> not)
    if List.isEmpty notLosingMoves && notLosingMoves.Length < game.PossibleMoves.Length then None
    else Some (notLosingMoves.[random.Next notLosingMoves.Length])

  let private pickRandom (game: RunningGame) = 
    Some (game.PossibleMoves.[random.Next game.PossibleMoves.Length])

  let pickMove (game: RunningGame) = 
    [winNow O; notLoseNow; pickRandom]
    |> Seq.ofList
    |> Seq.choose (fun x -> x game)
    |> Seq.head