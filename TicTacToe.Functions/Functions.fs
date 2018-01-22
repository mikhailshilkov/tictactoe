namespace TicTacToe.Functions

open System

open Microsoft.AspNetCore.Mvc
open Microsoft.AspNetCore.Http
open Microsoft.Azure.WebJobs
open Microsoft.Azure.WebJobs.Extensions.Http
open Microsoft.WindowsAzure.Storage.Table
open Newtonsoft.Json

open TicTacToe
open Api

module Functions =

  type GameEntity() = 
    inherit TableEntity()
    member val Name: string = null with get, set
    member val StartedAt = DateTime.UtcNow with get, set
    member val State: string = null with get, set
    member val Score: int = 0 with get, set

  [<FunctionName("Landing")>]
  let landing([<HttpTrigger(AuthorizationLevel.Anonymous, "get", Route = "home")>] req: HttpRequest, 
              [<Blob("html/tictactoe.html")>] html: string) =
    ContentResult(Content = html, ContentType = "text/html")


  [<FunctionName("Start")>]
  let start([<HttpTrigger(AuthorizationLevel.Anonymous, "POST", Route = "game")>] req: GameRequest,
            [<Table("TicTacToe")>] store: ICollector<GameEntity>) =
    let gameid = Guid.NewGuid().ToString()
    let state = InProgress Game.initialState
    let serializedState = JsonConvert.SerializeObject state
    store.Add(GameEntity(PartitionKey = "default", RowKey = gameid, Name = req.Name, State = serializedState))
    ObjectResult(Api.serialize gameid state 0)

  [<FunctionName("Play")>]
  let play([<HttpTrigger(AuthorizationLevel.Anonymous, "POST", Route = "game/{gameid}/move/{index}")>] req: HttpRequest, 
           gameid: string, index: int,
           [<Table("TicTacToe", "default", "{gameid}")>] entity: GameEntity) =
    let state = JsonConvert.DeserializeObject<GameState> entity.State
    match state with
    | Finished _ -> BadRequestResult() :> IActionResult
    | InProgress p when index < 0 || index >= p.PossibleMoves.Length -> BadRequestResult() :> IActionResult
    | InProgress p -> 
      let result = Game.makeRound (fun _ -> p.PossibleMoves.[index]) Bot.pickMove p
      entity.State <- JsonConvert.SerializeObject result
      entity.Score <- Scoring.calculateScore (DateTime.UtcNow - entity.StartedAt).TotalMilliseconds result
      ObjectResult(Api.serialize gameid result entity.Score) :> IActionResult

  [<FunctionName("Leaderboard")>]
  let leaderboard([<HttpTrigger(AuthorizationLevel.Anonymous, "get", Route = "leaderboard")>] req: HttpRequest, [<Table("TicTacToe", "default")>]resultsTable: CloudTable) =
    
    let board = 
      resultsTable.ExecuteQuerySegmentedAsync(new TableQuery<GameEntity>(), null).Result
      |> Seq.map (fun entity -> (entity.Name, entity.Score))
      |> Scoring.calculateLeaderboard 10

    ObjectResult(board)
