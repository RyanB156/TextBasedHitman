module Program

// Text based assassin game with open world and time travel elements.
// Command Line type interface

(*
    Change log:
        11/3/2018 Changed the line history for the console so that repeated elements are skipped.
*)

open System
open System.IO
open DomainTypes
open DomainFunctions
open Parser

open CustomConsole

type RunMode = Game | TestScript
type FileMode = LoadFiles | NewFiles

[<EntryPoint>]
let main argv = 
    
    Console.Title <- "Console RPG"
    let customConsole = new ConsoleExt("")
    let rng = new Random()

    let rec inputLoop () =
        printf "Input: "
        match Console.ReadLine() with
        | "n" | "N" -> NewFiles
        | "l" | "L" -> LoadFiles
        | _ -> inputLoop ()

    printfn "NewFiles: (n)\nLoadFiles: (l)"
    let fileMode = inputLoop ()
    printfn "Loading Please Wait..."
    printfn "Enjoy the game!"
    Console.Clear()

    let processAI result =
        match result with
            | Failure f -> Failure f
            | Success (env, AIWait) -> Success env
            | Success (env, AIMove) -> AI.aiAction env AIMove   // Could make this shorter but this way is more transparent.
            | Success (env, AIAlert callTarget) -> AI.aiAction env (AIAlert callTarget)
            | Success (env, AIAlertAll callTarget) -> AI.aiAction env (AIAlertAll callTarget)

    let applyStatus result =
        match result with
        | Failure f -> Failure f
        | Success env -> Success (env |> Environment.updatePeopleStatus)

    let checkObjectivesState env =
        match env.Objectives |> List.forall (Objective.isCompleted) with
        | true -> 
            printfn "\n\n*****  You have completed all of the objectives. Escape!  *****\n\n"
            env 
        | false -> env


    let run runMode env =

        let rec gameLoop env =

            Threading.Thread.Sleep(1)
            let equippedString = 
                match env.Player.EquippedItem with 
                | Some (Weapon (MWeapon _,info)) -> sprintf "%s" info.Name
                | Some (Weapon (RWeapon weapon,info)) -> sprintf "%s: %d" info.Name weapon.AmmoCount
                | _ -> "" 

            // Set values in the prompt based on the current state of the player's disguise and their gender.
            let disguiseString = env.Player |> Player.getDisguiseString
            let gender = match env.Player.Gender with | Male -> "M" | Female -> "F" | _ -> "_"
            
            let prompt = sprintf "\n%s %s %s H:%d %s - %s $: " env.Player.Info.Name gender disguiseString env.Player.Health 
                            equippedString (Room.getName env.Room)
            customConsole.SetPrompt(prompt)

            let command = 
                match runMode with
                | Game ->
                    Parser.keyInputLoop customConsole env
                | TestScript ->
                    let fileReader = new StreamReader("TestScript.txt")
                    try
                        fileReader.ReadLine()
                    with
                    | _ -> Console.ReadKey() |> ignore; "quit"  // End of test script. Hold window open to read the results, then exit.
        
            match processInput command env |> processAI |> applyStatus with
            | Success result ->
                let result = result |> checkObjectivesState
                match result.GameStatus with // Command succeeded -> pipe new environment in.
                | PlayerDead -> printfn "--You Died. Game Over.--"; Console.ReadKey() |> ignore
                | Win -> printfn "--You Win!--\nYou made %d commands" (env.MoveCount+1); Console.ReadKey() |> ignore
                | PartialWin -> printfn "--I guess you accomplished SOMETHING anyway--\nYou made %d commands" (env.MoveCount+1); Console.ReadKey() |> ignore
                | Continue -> gameLoop (result |> Environment.addMove)
                | _ -> ()   // No ending state.
            | Failure f -> printfn "%s" f; gameLoop env // Command failed -> pipe old environment back in.

        gameLoop env


    let startGame runMode fileMode =

        match fileMode with
        | NewFiles ->
            let gun = Item.initRangedWeapon 40 VLow 15 "Pistol" "A suppressed pistol"
            let syringe = Item.initMeleeWeapon 10 None VLow "Syringe" "A medical syringe for injecting medicine"

            printf "What is your name? :"
            let name = Console.ReadLine()

            let gender = Input.inputMapLoop "Gender: " (["male",Male;"female",Female] |> Map.ofList)

            let player = Player.initPlayer name "The Player" gender []
                         |> Player.addToInventory gun
                         |> Player.addToInventory syringe
            

            let max = Person.initPerson "Max" "A timid girl" Civilian Female Straight Fearful ENeutral MGrey 0.99
            let shelby = Person.initPerson "Shelby" " A strong girl" Civilian Female Straight NeutralB EChaotic MRed 0.20

            let forcePeopleList = [max; shelby]

            let (generatedData, objectives) = WorldGeneration.spawnRooms 20 rng

            FileIO.WorldLoading.createSaves generatedData // Save rooms to their defaults.
    
            // Keep a cache of environments at each minute to allow backtracking. Player inventory must be maintained.
            let result = FileIO.RoomIO.readFromFile "spawn"
            let room, map = match result with | Success (room, map) -> room, map | Failure _ -> failwith "Loading error"
            let accolades = {Kills = 0}

            let room = room |> Room.addPeople forcePeopleList

            let env = {Player = player; Room = room; Map = map; Time = {Hour = 1; Minute = 0}; GameStatus = Continue; Rng = rng;
                        UpdatePeople = []; ExtraLives = []; Objectives = objectives; Accolades = accolades; MoveCount = 0; VisitedRooms = ["spawn"]}

            env |> run runMode

        | LoadFiles ->
            let envResult = FileIO.EnvironmentIO.readFromFile // Make sure to create an env save first. The new files option doesn't do that right now.
            match envResult with
            | Success env -> env |> run runMode
            | Failure f -> failwith f
    

    startGame Game fileMode 

    // Bug List:
    (*
        
        
    *)

    // TODO:
    (* 


    *)
    
    //startGame TestScript NewFiles


    // 4,824 lines 6/26/2018 8:21 PM

    //Console.ReadKey() |> ignore
    0 
