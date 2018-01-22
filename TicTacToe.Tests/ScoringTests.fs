namespace TicTacToe.Tests

open Expecto
open FsCheck

open TicTacToe

module ScoringTests =
  let private maxmin f v1 v2 = if f v1 > f v2 then (v1, v2) else (v2, v1)

  let tests =       
    testList "Scoring" [
        testProp "The score of faster game is not lower than slower game" <| fun (Gen.Positive duration1) (Gen.Positive duration2) game ->
          let (slower, faster) = maxmin id duration1 duration2
          let scoreFaster = Scoring.calculateScore faster game
          let scoreSlower = Scoring.calculateScore slower game
          Expect.isGreaterThanOrEqual scoreFaster scoreSlower "Bigger duration has lower score (or same)"

        testProp "The score of won game in less moves is greater than game with more moves" <| fun (Gen.Positive duration1) (Gen.Positive duration2) game1 game2 ->
          let (slower, faster) = maxmin id duration1 duration2
          let (moreMoves, lessMoves) = maxmin List.length game1 game2
          let score1 = Scoring.calculateScore slower (Finished { Outcome = Won X; MovesDone = lessMoves })
          let score2 = Scoring.calculateScore faster (Finished { Outcome = Won X; MovesDone = moreMoves })
          if moreMoves.Length = lessMoves.Length then
            Expect.isGreaterThanOrEqual score1 score2 "Bigger duration has lower score (or same)"
          else
            Expect.isGreaterThan score1 score2 "More moves have lower score"
  
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