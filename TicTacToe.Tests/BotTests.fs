namespace TicTacToe.Tests

open Expecto
open FsCheck

open TicTacToe

module BotTests =
    let tests =
      testList "Bot" [
        testProp "Bot is able to play O at any possible position" <| fun (Gen.ListOfNonNegative xs) ->
          let human i p _ = p.PossibleMoves.[i % p.PossibleMoves.Length]
          let round s i =
            match s with
            | InProgress p -> Game.makeRound (human i p) Bot.pickMove p
            | _ -> s
          let r = List.fold round (InProgress Game.initialState) xs
          ()
      ]