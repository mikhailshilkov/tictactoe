namespace TicTacToe.Tests

open Expecto
open FsCheck

open TicTacToe

module GameTests =
    let playSequenceWith f moves = 
      let playOne s i =
        match s with
        | InProgress p -> Game.makeMove p (f i p.PossibleMoves)
        | _ -> s
      List.fold playOne (InProgress Game.initialState) moves

    let playSequence moves = playSequenceWith (fun i p -> p.[i % p.Length]) moves
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
          let result = playSequence xs
          Expect.isFalse (Game.isFinished result) "Game should be still in progress"

        testProp "X and 0 have to make moves in turns" <| fun (Gen.ListOfNonNegative xs) ->
          let result = playSequence xs
          let movesDone =
            match result with
            | Finished f -> f.MovesDone
            | InProgress p -> p.MovesDone
          let amountOfX = movesDone |> List.filter (fun x -> x.By = X) |> List.length
          let amountOfO = movesDone |> List.filter (fun x -> x.By = O) |> List.length
          Expect.isTrue (amountOfX = amountOfO || amountOfX = amountOfO + 1) "The amount of X is always equal or + 1 to amout of O"

        testProp "The game is always finished after 9 moves" <| fun (Gen.ListOf9 xs) ->
            let result = playSequence xs
            Expect.isTrue (Game.isFinished result) "Game should be finished"

        testProp "Player wins by filling one column" <| fun player column (Gen.ListOf9 xs) ->
          let rule i p =
            let matches = List.filter (fun x -> x.By = player && x.X = column || x.By = Player.other player && x.X <> column) p
            matches.[i % matches.Length]
          let result = playSequenceWith rule xs
          let won = match result with | Finished { Outcome = Won _ } -> true | _ -> false
          Expect.isTrue won (sprintf "Player should have won %O" result)

        testProp "Player wins by filling one row" <| fun player row (Gen.ListOf9 xs) ->
          let rule i p =
            let matches = List.filter (fun x -> x.By = player && x.Y = row || x.By = Player.other player && x.Y <> row) p
            matches.[i % matches.Length]
          let result = playSequenceWith rule xs
          let won = match result with | Finished { Outcome = Won _ } -> true | _ -> false
          Expect.isTrue won (sprintf "Player should have won %O" result)

        testProp "Player wins by filling diagonal" <| fun player (Gen.ListOf9 xs) ->
          let diagonal p = p.X = p.Y || List.sort [p.X; p.Y] = [One; Three]
          let rule i p =
            let matches = List.filter (fun x -> (x.By = player) = (diagonal x)) p
            if List.isEmpty matches then p.[i % p.Length]
            else matches.[i % matches.Length]
          let result = playSequenceWith rule xs
          let won = match result with | Finished { Outcome = Won _ } -> true | _ -> false
          Expect.isTrue won (sprintf "Player should have won %O" result)

        testProp "Known tie" <| fun (Gen.ListOf9 xs) ->
          let os = [(One, One); (One, Three); (Three, Two); (Two, Three)]
          let rule i p =
            let matches = List.filter (fun x -> x.By = X <> (List.contains (x.X, x.Y) os)) p
            matches.[i % matches.Length]
          let result = playSequenceWith rule xs
          let tie = match result with | Finished { Outcome = Tie } -> true | _ -> false
          Expect.isTrue tie (sprintf "Tie expected %O" result)
      ]