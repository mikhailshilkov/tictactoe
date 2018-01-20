namespace TicTacToe.Tests

open Expecto
open FsCheck

module Gen =
    type Positive = Positive of float
    let positive() =
      Arb.generate<float>
      |> Gen.filter (fun x -> x > 0.1 && x < 100000.0 && x <> infinity && x <> -infinity)
      |> Arb.fromGen
      |> Arb.convert Positive (fun (Positive l) -> l)
        
    type ListOfNonNegative = ListOfNonNegative of int list
    let listOfNonNegative() =
        Arb.generate<int>
        |> Gen.map (fun x -> if (x < 0) then -x else x)
        |> Gen.nonEmptyListOf
        |> Arb.fromGen
        |> Arb.convert ListOfNonNegative (fun (ListOfNonNegative l) -> l)

    let listOfNArb n =
        Arb.generate<int>
        |> Gen.map (fun x -> if (x < 0) then -x else x)
        |> Gen.listOfLength n
        |> Arb.fromGen

    type 'a ListOf4 = ListOf4 of 'a list
    let listOf4Arb() = Arb.convert ListOf4 (fun (ListOf4 l) -> l) (listOfNArb 4)

    type ListOf9 = ListOf9 of int list
    let listOf9Arb() = Arb.convert ListOf9 (fun (ListOf9 l) -> l) (listOfNArb 9)

    let addToConfig config =
        {config with arbitrary = typeof<ListOf9>.DeclaringType::config.arbitrary}

[<AutoOpen>]
module Auto =
    let private config = Gen.addToConfig FsCheckConfig.defaultConfig
    let testProp name = testPropertyWithConfig config name