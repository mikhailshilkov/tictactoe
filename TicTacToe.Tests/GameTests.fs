namespace TicTacToe.Tests

open Expecto
open FsCheck

open TicTacToe

module GameTests =
    let makeMovesByRule f moves = 
      moves
      |> List.fold 
        (fun s i -> 
          match s with
          | InProgress p -> Game.makeMove p (f i p.PossibleMoves)
          | _ -> s
        ) (InProgress Game.initialState)

    let makeMoves moves = makeMovesByRule (fun i p -> p.[i % p.Length]) moves

    let isFinishedAfter moves =
      match makeMoves moves with 
      | Finished _ -> true
      | _ -> false

    let tests =
      testList "Domain" [
        testProperty "Other player is not equal to player" <| fun x ->
          x <> Player.other x
        testProperty "Other player of other player is the player itself" <| fun x ->
          x = Player.other (Player.other x)

        test "Initial state has 0 moves done" {
            Expect.equal Game.initialState.MovesDone.Length 0 "MovesDone should be empty"
        }
        test "Initial state has 3x3 possible moves" {
            let possibleMovesByX = Game.initialState.PossibleMoves |> List.filter (fun m -> m.By = X)
            let possibleMovesByO = Game.initialState.PossibleMoves |> List.filter (fun m -> m.By = O)
            Expect.equal possibleMovesByX.Length (3*3) "Possible moves by X should be 9"
            Expect.isEmpty possibleMovesByO "There should be no Possible moves by O"
        }
        test "Initial state possible moves are all different" {
            let distinctMoves = Game.initialState.PossibleMoves |> List.distinct
            Expect.equal distinctMoves Game.initialState.PossibleMoves "Possible moves are distinct"
        }

        testProp "The game is never finished after 4 moves" <| fun (Gen.ListOf4 xs) ->
          Expect.isFalse (isFinishedAfter xs) "Game should be still in progress"

        testProp "X and 0 make moves in turns" <| fun (Gen.ListOfNonNegative xs) ->
          let result = makeMoves xs
          let movesDone =
            match result with
            | Finished (_, ms) -> ms
            | InProgress p -> p.MovesDone
          let amountOfX = movesDone |> List.filter (fun x -> x.By = X) |> List.length
          let amountOfO = movesDone |> List.filter (fun x -> x.By = O) |> List.length
          Expect.isTrue (amountOfX = amountOfO || amountOfX = amountOfO + 1) "The amount of X is always equal or + 1 to amout of O"

        testProp "The game is always finished after 9 moves" <| fun (Gen.ListOf9 xs) ->
            Expect.isTrue (isFinishedAfter xs) "Game should be finished"

        testProp "Player wins by filling one column" <| fun player column (Gen.ListOf9 xs) ->
          let rule i p =
            let matches = List.filter (fun x -> x.By = player && x.X = column || x.By = Player.other player && x.X <> column) p
            matches.[i % matches.Length]
          let result = makeMovesByRule rule xs
          let won = match result with | Finished(Won _, _) -> true | _ -> false
          Expect.isTrue won (sprintf "Player should have won %O" result)

        testProp "Player wins by filling one row" <| fun player row (Gen.ListOf9 xs) ->
          let rule i p =
            let matches = List.filter (fun x -> x.By = player && x.Y = row || x.By = Player.other player && x.Y <> row) p
            matches.[i % matches.Length]
          let result = makeMovesByRule rule xs
          let won = match result with | Finished(Won _, _) -> true | _ -> false
          Expect.isTrue won (sprintf "Player should have won %O" result)

        testProp "Player wins by filling diagonal" <| fun player (Gen.ListOf9 xs) ->
          let diagonal p = p.X = p.Y || List.sort [p.X; p.Y] = [Left; Right]
          let rule i p =
            let matches = List.filter (fun x -> (x.By = player) = (diagonal x)) p
            if List.isEmpty matches then p.[i % p.Length]
            else matches.[i % matches.Length]
          let result = makeMovesByRule rule xs
          let won = match result with | Finished(Won _, _) -> true | _ -> false
          Expect.isTrue won (sprintf "Player should have won %O" result)

        testProp "Known tie" <| fun (Gen.ListOf9 xs) ->
          let os = [(Left, Left); (Left, Right); (Right, Middle); (Middle, Right)]
          let rule i p =
            let matches = List.filter (fun x -> x.By = X <> (List.contains (x.X, x.Y) os)) p
            matches.[i % matches.Length]
          let result = makeMovesByRule rule xs
          let tie = match result with | Finished(Tie, _) -> true | _ -> false
          Expect.isTrue tie (sprintf "Tie expected %O" result)
      ]