
type Coach = {
    Name: string
    FormerPlayer: bool
}


type Stats = {
    Wins: int
    Losses: int
}


type Team = {
    Name: string
    Coach: Coach
    Stats: Stats
}


let teams = [
    {
        Name = "Los Angeles Lakers"
        Coach = { Name = "Darvin Ham"; FormerPlayer = true }
        Stats = { Wins = 3425; Losses = 2315 }
    }
    {
        Name = "Boston Celtics"
        Coach = { Name = "Joe Mazzulla"; FormerPlayer = false }
        Stats = { Wins = 3480; Losses = 2390 }
    }
    {
        Name = "Chicago Bulls"
        Coach = { Name = "Billy Donovan"; FormerPlayer = true }
        Stats = { Wins = 2245; Losses = 2075 }
    }
    {
        Name = "Golden State Warriors"
        Coach = { Name = "Steve Kerr"; FormerPlayer = true }
        Stats = { Wins = 2925; Losses = 3055 }
    }
    {
        Name = "San Antonio Spurs"
        Coach = { Name = "Gregg Popovich"; FormerPlayer = false }
        Stats = { Wins = 2285; Losses = 1645 }
    }
]


printfn "List of Teams:"
teams |> List.iter (fun team ->
    printfn "Team: %s, Coach: %s, Former Player: %b, Wins: %d, Losses: %d" 
        team.Name team.Coach.Name team.Coach.FormerPlayer team.Stats.Wins team.Stats.Losses
)


let successfulTeams = teams |> List.filter (fun team -> team.Stats.Wins > team.Stats.Losses)


printfn "\nList of Successful Teams:"
successfulTeams |> List.iter (fun team ->
    printfn "Team: %s, Coach: %s, Former Player: %b, Wins: %d, Losses: %d" 
        team.Name team.Coach.Name team.Coach.FormerPlayer team.Stats.Wins team.Stats.Losses
)


let successPercentages = 
    successfulTeams 
    |> List.map (fun team -> 
        let wins = float team.Stats.Wins
        let losses = float team.Stats.Losses
        let percentage = (wins / (wins + losses)) * 100.0
        (team.Name, percentage)
    )

printfn "\nSuccess Percentages of Successful Teams:"
successPercentages |> List.iter (fun (name, percentage) ->
    printfn "Team: %s, Success Percentage: %.2f%%" name percentage
)
//-------------------------------------------------------------------------------------------------DISCRIMINATED UNION

type Cuisine =
    | Korean
    | Turkish


type MovieType =
    | Regular
    | IMAX
    | DBOX
    | RegularWithSnacks
    | IMAXWithSnacks
    | DBOXWithSnacks


type Activity =
    | BoardGame
    | Chill
    | Movie of MovieType
    | Restaurant of Cuisine
    | LongDrive of int * float
let calculateBudget activity =
    match activity with
    | BoardGame -> 0
    | Chill -> 0
    | Movie movieType ->
        match movieType with
        | Regular -> 12
        | IMAX -> 17
        | DBOX -> 20
        | RegularWithSnacks -> 12 + 5
        | IMAXWithSnacks -> 17 + 5
        | DBOXWithSnacks -> 20 + 5
    | Restaurant cuisine ->
        match cuisine with
        | Korean -> 70
        | Turkish -> 65
    | LongDrive (kilometres, fuelPerKilometre) ->
        int (float kilometres * fuelPerKilometre)

let activities = [
    BoardGame
    Chill
    Movie Regular
    Movie IMAX
    Movie DBOX
    Movie RegularWithSnacks
    Movie IMAXWithSnacks
    Movie DBOXWithSnacks
    Restaurant Korean
    Restaurant Turkish
    LongDrive (100, 0.12)
]

activities |> List.iter (fun activity ->
    let budget = calculateBudget activity
    printfn "Activity: %A, Budget: %d CAD" activity budget
)
