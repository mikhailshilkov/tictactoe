open Expecto
open TicTacToe.Tests

[<EntryPoint>]
let main args =
  runTestsWithArgs defaultConfig args (testList "all" [GameTests.tests; ScoringTests.tests; BotTests.tests])