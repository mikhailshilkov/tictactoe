namespace TicTacToe.Tests

open Expecto
open FsCheck

open TicTacToe

module ScoringTests =
  let tests =       
    testList "Scoring" [
        testProp "The score of faster game is not lower than slower game" <| fun (Gen.Positive duration1) (Gen.Positive duration2) game ->
          let score1 = Scoring.calculateScore duration1 game
          let score2 = Scoring.calculateScore duration2 game
          if duration1 > duration2 then
            Expect.isLessThanOrEqual score1 score2 "Bigger duration has lower score (or same)"
          else
            Expect.isLessThanOrEqual score2 score1 "Bigger duration has lower score (or same)"

        testProp "The score of won game in less moves is greater than game with more moves" <| fun (Gen.Positive duration1) (Gen.Positive duration2) game1 game2 ->
          let score1 = Scoring.calculateScore duration1 (Finished(Won X, game1))
          let score2 = Scoring.calculateScore duration2 (Finished(Won X, game2))
          if game1.Length > game2.Length then
            Expect.isLessThan score1 score2 "More moves have lower score"
          elif game1.Length < game2.Length then
            Expect.isLessThan score2 score1 "More moves have lower score"
          elif duration1 > duration2 then
            Expect.isLessThanOrEqual score1 score2 "Bigger duration has lower score (or same)"
          else
            Expect.isLessThanOrEqual score2 score1 "Bigger duration has lower score (or same)"

        test "Leaderboard calculates top N entries" {
            let entries = [
              ("Carol", 100)
              ("Alice", 333)
              ("Eve", 10)
              ("Bob", 101)
              ("Dave", 99)
            ]
            let actual = Scoring.calculateLeaderboard 3 entries |> List.ofSeq
            let expected = [
              { Index = 1; Name = "Alice"; Score = 333 }
              { Index = 2; Name = "Bob"; Score = 101 }
              { Index = 3; Name = "Carol"; Score = 100 }
            ]
            Expect.equal actual expected "Leaderboard should match"
        }
            
        test "Leaderboard ignores empty names and non-positive scores" {
            let entries = [
              ("Carol", -100)
              ("", 222)
              ("Alice", 333)
              (null, 500)
              ("Bob", 101)
              ("Dave", 0)
            ]
            let actual = Scoring.calculateLeaderboard 3 entries |> List.ofSeq
            let expected = [
              { Index = 1; Name = "Alice"; Score = 333 }
              { Index = 2; Name = "Bob"; Score = 101 }
            ]
            Expect.equal actual expected "Leaderboard should match"
        }
      ]