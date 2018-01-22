namespace TicTacToe

open System

type LeaderboardItem = {
  Index: int
  Name: string
  Score: int
}

module Scoring =

  let calculateScore duration (state: GameState) =
    let durationScore = (100.0 * (1.0 - duration / (duration + 10000.0))) |> int
    match state with
    | Finished { Outcome = Won X; MovesDone = ms } -> (11 - ms.Length) * 100 + durationScore
    | Finished { Outcome = Tie } -> durationScore
    | _ -> 0

  let calculateLeaderboard top ns =
    ns
    |> Seq.filter (fun entity -> snd entity > 0 && not (String.IsNullOrEmpty (fst entity)))
    |> Seq.sortByDescending snd
    |> Seq.truncate top
    |> Seq.mapi (fun index entity -> { Index = index + 1; Name = fst entity; Score = snd entity })
  