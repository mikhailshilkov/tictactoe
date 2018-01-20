namespace TicTacToe

module Bot =

  let private r = System.Random()

  let private winNow player (game: Progress) =
    let isWin = function | Finished(Won x, _) when x = player -> true | _ -> false
    game.PossibleMoves
    |> List.tryFind (fun move -> Game.makeMove game move |> isWin)

  let notLoseNow (game: Progress) =
    let canLose = function 
      | InProgress p -> match winNow X p with | Some _ -> true | None -> false
      | _ -> false
    let notLosingMoves =
      game.PossibleMoves
      |> List.filter (fun move -> Game.makeMove game move |> canLose |> not)
    if List.isEmpty notLosingMoves then None
    else Some (notLosingMoves.[r.Next notLosingMoves.Length])

  let private pickRandom (game: Progress) = Some (game.PossibleMoves.[r.Next game.PossibleMoves.Length])

  let pickMove (game: Progress) = 
    [winNow O; notLoseNow; pickRandom]
    |> Seq.ofList
    |> Seq.choose (fun x -> x game)
    |> Seq.head