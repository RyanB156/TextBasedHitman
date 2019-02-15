
module Parser

open System
open DomainTypes
open DomainFunctions
open CommandTypes
open Commands

open CustomConsole


/// Matches verbs for placing an object inside another object.
let (|PlaceVerb|_|) arg =
    match arg with
    | "place" | "hide" | "store"| "put" -> Some arg
    | _ -> None
let (|PlacePrep|_|) arg =
    match arg with
    | "in" -> Some arg
    | _ -> None

// -- Match arguments to functions -- //

let oneThingFinder thing result cmd = function
    | thingName::[] -> Success ( result <| thingName)
    | [] -> Failure (sprintf "Missing %s argument for %s" thing cmd)
    | _ -> Failure (sprintf "%s expects one argument" cmd)

let onePersonFinder = oneThingFinder "person"

let oneItemFinder = oneThingFinder "item"

let oneRoomFinder = oneThingFinder "room"

let matchAmuse = onePersonFinder Amuse "AMUSE"

let matchApply = function
    | poisonName::"to"::targetName::[] -> Success (Apply (poisonName,targetName))
    | poisonName::"to"::[] -> Failure "Missing target argument for APPLY"
    | [] -> Failure "Missing poison and target argument for APPLY"
    | _ -> Failure "APPLY expects two arguments"

let matchApproach = onePersonFinder Approach "APPROACH"

let matchAttack = onePersonFinder Attack "ATTACK"

let matchCapture = onePersonFinder Capture "CAPTURE"

let matchCheerUp = onePersonFinder CheerUp "CHEERUP"

let matchChokeOut = onePersonFinder ChokeOut "CHOKEOUT"

let (|AiCmd|_|) x = if Commands.aiCommandList |> List.contains x then Some x else None

let matchCommand xs =
    let cmd3Args cmd target =
        match cmd with
        | "attack" -> AIAttack target
        | "goto" -> AIGoto target
        | "pickup" -> AIPickup target
        | _ -> failwith "Parser.matchCommand: Internal Match Error"
    let cmd2Args cmd =
        match cmd with
        | "stop" -> AIStop
        | "killyourself" -> AISuicide
        | _ -> failwith "Parser.matchCommand: Internal Match Error"
    match xs with
    | personName::AiCmd x::targetName::[] -> Success (Command (personName, cmd3Args x targetName))
    | personName::AiCmd x::[] -> Success (Command (personName, cmd2Args x))
    | personName::AiCmd x::[] -> Failure "Missing target argument for COMMAND"
    | personName::[] -> Failure "Missing command argument for COMMAND"
    | _ -> Failure "COMMAND expects three arguments"

let matchCompliment = onePersonFinder Compliment "COMPLIMENT"

let matchConsume = oneItemFinder Consume "CONSUME"

let matchDescribe = function
    | "area"::[] -> Success (Describe DescribeArea)
    | "item"::itemName::[] -> Success (Describe <| DescribeItem itemName)
    | "person"::personName::[] -> Success (Describe <| DescribePerson personName)
    | [] -> Failure "DESCRIBE expects one argument"
    | x::xs -> Failure "Invalid arguments for DESCRIBE"

let matchDisguise = onePersonFinder Disguise "DISGUISE"

let matchDishearten = onePersonFinder Dishearten "DISHEARTEN"

let matchDrop = oneItemFinder Drop "DROP"

let matchEscape = oneItemFinder Escape "ESCAPE"

let matchEquip = oneItemFinder Equip "EQUIP"

let matchFollowMe = onePersonFinder FollowMe "FOLLOWME"

let matchGive = function
    | itemName::"to"::personName::[] -> Success (Give (itemName, personName))
    | itemName::"to"::[] -> Failure ("Missing the Person argument to GIVE")
    | itemName::_::personName::[] -> Success (Give (itemName, personName))
    | _ -> Failure ("GIVE expects an item and a person as arguments")

let matchGoto = function
    | placeName::[] -> Success (Goto (Arg placeName ))
    | [] -> Success (Goto Empty)
    | _ -> Failure ("GOTO expects one argument")

let matchGotoForce = oneRoomFinder GotoForce "GOTOFORCE"

let matchHelp = function
    | [] -> Success (Help Empty)
    | x::[] -> Success (Help (Arg x))
    | x::xs -> Failure ("HELP expects one argument")

let matchInquire = function
    | [] -> Failure ("Missing question and person argument for INQUIRE")
    | x::[] -> Failure ("Missing person argument for INQUIRE")
    | x::y::[] -> Success (Inquire (x,y))
    | _ -> Failure ("INQUIRE expects two arguments")

let matchInspect = oneItemFinder Inspect "INSPECT"

let matchIntimidate = onePersonFinder Intimidate "INTIMIDATE"

let matchLeaveMe = function
    | [] -> Success LeaveMe
    | _ -> Failure ("LEAVEME does not take any arguments")

let matchPeek = oneRoomFinder Peek "PEEK"

let matchPickup = oneItemFinder Pickup "PICKUP"

let matchPlace = function
    | itemName::PlacePrep p::targetName::[] -> Success (Place (itemName, targetName))
    | PlacePrep p::xs -> Failure ("Missing item argument for PLACE")
    | itemName::PlacePrep p::[] -> Failure ("Missing target argument for PLACE")
    | _ -> Failure ("Invalid arguments to PLACE")

let matchPunch = onePersonFinder Punch "PUNCH"

let matchQuit xs = Success Quit

let matchRomance = onePersonFinder Romance "ROMANCE"
    
let matchSave xs = Success Save

let matchScout = oneRoomFinder Scout "SCOUT"

let matchSearch = function
    | "area"::[] | [] -> Success (Search SearchArea)
    | x::[] -> Success (Search (SearchItem x))
    | _ -> Failure ("Invalid argument(s) for SEARCH")

let matchSeduce = onePersonFinder Seduce "SEDUCE"

let matchSurvey = function
    | [] -> Success Survey
    | _ -> Failure ("SURVEY does not take any arguments")

let matchTakeFrom = function
    | personName::itemName::[] -> Success (TakeFrom (personName, itemName))
    | personName::[] -> Failure ("Missing target argument for TAKEFROM")
    | _ -> Failure ("TAKEFROM expects an item and person as arguments")

let matchTalk = onePersonFinder Talk "TALK"

let matchUnequip = function
    | [] -> Success Unequip
    | _ -> Failure ("UNEQUIP does not take any arguments")

let matchUnlock = oneRoomFinder Unlock "UNLOCK"

let matchView = function
    | "my"::"stats"::[] -> Success (View PlayerStats)
    | "my"::"companion"::[] -> Success (View CompanionName)
    | "items"::[] | "inventory"::[] -> Success (View Inventory)
    | "time"::[] -> Success (View Time)
    | "objectives"::[] -> Success (View Objectives)
    | "visitedrooms"::[] -> Success (View VisitedRooms)
    | "stats"::personName::[] -> Success (View <| PersonStats personName)
    | "stats"::[] -> Failure ("Missing person argument to VIEW Stats")
    | x::[] -> Failure (sprintf "Invalid argument for VIEW: %s" x)
    | x::y -> Failure ("Too many arguments for VIEW")
    | _ -> Failure ("Invalid arguments for VIEW")

// -- Match parsed out command

let runCommand cmd env = 
    match cmd with
    | Amuse personName -> env |> amuse personName
    | Apply (poisonName, targetName) -> env |> apply poisonName targetName
    | Approach personName -> env |> approach personName
    | Attack personName -> env |> attack personName
    | Capture personName -> env |> capture personName
    | CheerUp personName -> env |> cheerup personName
    | ChokeOut personName -> env |> chokeOut personName
    | Command (personName, cmd) -> env |> command personName cmd
    | Compliment personName -> env |> compliment personName
    | Consume itemName -> env |> consume itemName
    | Diagnose -> env |> printfn "%A"; Success (env, AIWait)
    | Disguise personName -> env |> disguise personName
    | Dishearten personName -> env |> dishearten personName
    | Drop itemName-> env |> drop itemName
    | Help arg -> env |> help arg
    | Describe itemName -> env |> describe itemName
    | Escape itemName -> env |> escape itemName
    | Equip itemName-> env |> equip itemName
    | FollowMe personName -> env |> followMe personName
    | Give (itemName, personName) -> env |> give itemName personName
    | Goto arg -> env |> goto arg false
    | GotoForce roomName -> env |> forceGoto (Arg roomName)
    | Inquire (question, personName) -> env |> inquire question personName
    | Inspect itemName -> env |> inspect itemName
    | Intimidate personName -> env |> intimidate personName
    | LeaveMe -> env |> leaveMe
    | Peek arg -> env |> peek arg
    | Pickup itemName-> env |> pickup itemName
    | Place (itemName, target) -> env |> place itemName target
    | Punch personName -> env |> punch personName
    | Quit -> env |> quit
    | Romance personName -> env |> romance personName
    | Save -> env |> save
    | Scout roomName -> env |> scout roomName
    | Search arg -> env |> search arg
    | Seduce personName -> env |> seduce personName
    | Survey -> env |> survey
    | TakeFrom (personName, itemName) -> env |> takeFrom personName itemName
    | Talk personName -> env |> talk personName
    | Teleport roomName -> env |> teleport roomName
    | Unequip -> env |> unequip
    | Unlock arg -> env |> unlock arg
    | View arg -> env |> view arg
    | Wait -> env |> wait
    | c -> Failure (sprintf "You messed up. %A slipped through" c)

    
/// Parses the command line. Evaluating the argument is handled inside the function call for each command.
let scanCommand (input:string) = 
    // Converts string to lowercase, removes trailing whitespace, and splits it based on spaces.
    let splitCommands = input.ToLower().Trim().Split([|' '|], StringSplitOptions.RemoveEmptyEntries)  |> Array.toList
    match splitCommands with
    | "amuse"::xs -> matchAmuse xs
    | "apply"::xs -> matchApply xs
    | "approach"::xs -> matchApproach xs
    | "attack"::xs -> matchAttack xs
    | "capture"::xs -> matchCapture xs
    | "cheerup"::xs -> matchCheerUp xs
    | "chokeout"::xs -> matchChokeOut xs
    | "command"::xs -> matchCommand xs
    | "compliment"::xs -> matchCompliment xs
    | "consume"::xs -> matchConsume xs
    | "describe"::xs -> matchDescribe xs
    | "diagnose"::xs -> Success (Diagnose)
    | "disguise"::xs -> matchDisguise xs
    | "dishearten"::xs -> matchDishearten xs
    | "drop"::xs -> matchDrop xs
    | "escape"::xs -> matchEscape xs
    | "equip"::xs -> matchEquip xs
    | "followme"::xs -> matchFollowMe xs
    | "give"::xs -> matchGive xs
    | "goto"::xs -> matchGoto xs
    | "forcegoto"::xs -> matchGotoForce xs
    | "help"::xs -> matchHelp xs
    | "inquire"::xs -> matchInquire xs
    | "inspect"::xs -> matchInspect xs
    | "intimidate"::xs -> matchIntimidate xs
    | "leaveme"::xs -> matchLeaveMe xs
    | "peek"::xs -> matchPeek xs
    | "pickup"::xs -> matchPickup xs
    | "place"::xs -> matchPlace xs
    | "punch"::xs -> matchPunch xs
    | "quit"::xs -> matchQuit xs
    | "romance"::xs -> matchRomance xs
    | "save"::xs -> matchSave xs
    | "scout"::xs -> matchScout xs
    | "search"::xs -> matchSearch xs
    | "seduce"::xs -> matchSeduce xs
    | "survey"::xs -> matchSurvey xs
    | "takefrom"::xs -> matchTakeFrom xs
    | "teleport"::roomName::xs -> Success (Teleport roomName)
    | "unequip"::xs -> matchUnequip xs
    | "talk"::xs -> matchTalk xs
    | "unlock"::xs -> matchUnlock xs
    | "view"::xs -> matchView xs
    | "wait"::xs -> Success Wait
    
    | [] -> Failure ("")
    | x::_ -> 
        match getCmdSuggestion x 2 with
            | None -> Failure (sprintf "%s is not a valid command" input)
            | Some s -> Failure (sprintf "%s is not a valid command\nDid you mean %s?" input (s.ToUpper()))
           
// Scan the command to match the arguments then send to Commands.fs to run the command.
// Then the environment is sent to AI.fs to process the AI's action.
let processInput command env =
    match scanCommand command with
        | Failure f -> Failure f
        | Success s -> runCommand s env


// ------ Key Input and Autocomplete ------ //

let implode (cs:seq<char>) =
    cs |> Seq.fold (fun s t -> s + string t) ""

let stringMatch matchStrings pattern =
    match Commands.getSuggestions matchStrings pattern with
    | x::xs -> x |> snd
    | _ -> ""

let (|PersonInquiry|_|) inquiry = if Person.inquireInfo |> List.contains inquiry then Some inquiry else None

let matchRoomPeople env name =
    name |> stringMatch (env.Room.People |> List.map (Person.getName >> String.toLower))

let matchInventoryItem env name =
    name |> stringMatch (env.Player.Items |> List.map (Item.getName >> String.toLower))

let matchRoomItem env name =
    name |> stringMatch (env.Room.Items |> List.map (Item.getName >> String.toLower))

let matchAllItems env name =
    let allItems = (env.Room.Items |> List.map (Item.getName >> String.toLower)) @ (env.Player.Items |> List.map (Item.getName >> String.toLower))
    name |> stringMatch allItems

let matchPeopleAndItems env name =
    let peopleNames = env.Room.People |> List.map (Person.getName >> String.toLower)
    let itemNames = env.Room.Items |> List.map (Item.getName >> String.toLower)
    let allNames = peopleNames @ itemNames
    name |> stringMatch allNames

let matchAdjacentRooms env name =
    name |> stringMatch (env.Map |> (fun (_,adj,_) -> adj) |> List.map (fst >> String.toLower))

let matchOverlookRooms env name =
    let (_,_,ovl) = env.Map
    match ovl with
    | None -> ""
    | Some ovlRooms -> name |> stringMatch (ovlRooms |> List.map String.toLower)

let holderItemSearch targetName itemName env =
    match env |> Environment.tryFindPersonByName targetName with
    | Some person -> itemName |> stringMatch (person.Items |> List.map (Item.getName >> String.toLower))
    | None ->
        match env |> Environment.tryFindItemByName targetName with
        | Some item -> 
            match item with
            | Container (itemList, _) -> itemName |> stringMatch (itemList |> List.map (Item.getName >> String.toLower))
            | _ -> ""
        | None -> ""

let autoComplete (incompleteText:string) env =
    let inner () =
        match incompleteText.ToLower().Trim().Split([|' '|], StringSplitOptions.RemoveEmptyEntries) |> Array.toList with
        | "amuse"::x::[] -> "amuse " + matchRoomPeople env x
        | "apply"::xs ->
            match xs with
            | poisonName::[] -> "apply " + matchInventoryItem env poisonName + " to "
            | poisonName::"to"::targetName::[] -> sprintf "apply %s to %s" poisonName (matchInventoryItem env targetName)
            | _ -> "apply "
        | "approach"::x::[] -> "approach " + matchRoomPeople env x
        | "attack"::x::[] -> "attack " + matchRoomPeople env x
        | "capture"::x::[] -> "capture " + matchRoomPeople env x
        | "cheerup"::x::[] -> "cheerup " + matchRoomPeople env x
        | "chokeout"::x::[] -> "chokeout " + matchRoomPeople env x
        | "command"::xs ->
            match xs with
            | personName::"stop"::[] -> sprintf "command %s stop" personName
            | personName::"killyourself"::[] -> sprintf "command %s killyourself" personName
            | personName::"attack"::targetName::[] -> sprintf "command %s attack %s" personName (matchRoomPeople env targetName)
            | personName::"goto"::targetName::[] -> sprintf "command %s goto %s" personName (matchAdjacentRooms env targetName)
            | personName::"pickup"::targetName::[] -> sprintf "command %s pickup %s" personName (matchRoomItem env targetName)
            | personName::commandName::[] -> sprintf "command %s %s " personName (commandName |> stringMatch Commands.aiCommandList)
            | personName::[] -> "command " + matchRoomPeople env personName + " "
            | _ -> "command "

        | "compliment"::x::[] -> "compliment " + matchRoomPeople env x
        | "consume"::x::[] -> "consume " + matchInventoryItem env x
        | "describe"::xs -> 
            match xs with
            | x::[] -> "describe " + (stringMatch ["area"; "item"; "person"] x) + " "
            | "item"::x::[] -> "describe item " + matchAllItems env x
            | "person"::x::[] -> "describe person " + matchRoomPeople env x
            | _ -> "describe "
        | "diagnose"::xs -> "diagnose"
        | "disguise"::x::[] -> "disguise " + matchRoomPeople env x
        | "dishearten"::x::[] -> "dishearten " + matchRoomPeople env x
        | "drop"::x::[] -> "drop " + matchInventoryItem env x
        | "escape"::x::[] -> "escape " + matchRoomItem env x
        | "equip"::x::[] -> "equip " + matchInventoryItem env x
        | "followme"::x::[] -> "followme " + matchRoomPeople env x
        | "give"::xs ->
            match xs with
            | itemName::[] -> "give " + (itemName |> matchInventoryItem env) + " to "
            | itemName::"to"::personName::[] -> sprintf "give %s to %s" itemName (personName |> matchRoomPeople env)
            | _ -> "give "
        | "goto"::x::[] -> "goto " + matchAdjacentRooms env x
        | "forcegoto"::x::[] -> "forcegoto " + matchAdjacentRooms env x
        | "help"::x::[] -> "help " + (Commands.getCmdSuggestion x 1 |> (function | None -> "" | Some cmdString -> cmdString))
        | "inquire"::xs ->
            match xs with
            | PersonInquiry inquiry::personName::[] -> sprintf "inquire %s %s" inquiry (matchRoomPeople env personName)
            | x::xs -> "inquire " + (stringMatch Person.inquireInfo x) + " "
            | _ -> "inquire "
        | "inspect"::x::[] -> "inspect " + matchAllItems env x
        | "intimidate"::x::[] -> "intimidate " + matchRoomPeople env x
        | "leaveme"::x::[] -> "leaveme "
        | "peek"::x::[] -> "peek " + matchAdjacentRooms env x
        | "pickup"::x::[] -> "pickup " + matchRoomItem env x
        | "place"::xs ->
            match xs with
            | itemName::[] -> "place " + (itemName |> matchInventoryItem env) + " in "
            | itemName::PlacePrep p::targetName::[] -> sprintf "place %s %s %s" itemName p (targetName |> matchRoomItem env)
            | x::[] -> "place " + x
            | _ -> "place "
        | "punch"::x::[] -> "punch " + matchRoomPeople env x
        | "quit"::xs -> "quit "
        | "romance"::x::[] -> "romance " + matchRoomPeople env x
        | "save"::xs -> "save "
        | "scout"::x::[] -> "scout " + matchOverlookRooms env x
        | "search"::x::[] -> "search " + (x |> stringMatch ("area"::(env.Room.Items |> List.map (Item.getName >> String.toLower))))
        | "seduce"::x::[] -> "seduce " + matchRoomPeople env x
        | "survey"::xs -> "survey"
        | "takefrom"::xs ->
            match xs with
            | targetName::[] -> "takefrom " + (matchPeopleAndItems env targetName) + " "
            | targetName::itemName::[] -> sprintf "takefrom %s %s" targetName (holderItemSearch targetName itemName env)
            | _ -> "takefrom "
        | "talk"::x::[] -> "talk " + matchRoomPeople env x
        | "unequip"::x::[] -> "unequip "
        | "unlock"::x::[] -> "unlock " + matchAdjacentRooms env x
        | "view"::xs -> 
            match xs with
            | "my"::x::[] -> "view my " + (x |> stringMatch ["stats"; "companion"])
            | x::[] -> "view " + (x |> stringMatch ["items"; "time"; "my"; "stats"; "objectives"; "visitedrooms"]) + " "
            | "stats"::y::[] -> "view stats " + (y |> matchRoomPeople env)
            | _ -> "view "
        | "wait"::xs -> "wait "
        | cmd::xs -> Commands.getCmdSuggestion cmd 1 |> (function | None -> "" | Some cmdString -> cmdString + " ")
        | [] -> ""
    inner ()

// Console handles key input. Returns here on a tab. Parser checks for autocomplete pattern and returns the completed string to console for more input.
let keyInputLoop (console:ConsoleExt) (env:Environment) = // Run through input and history from ConsoleExt. Then modify with context based tab autocomplete.
    
    let text = console.ReadInput()
    let rec tabCheck text =
        match text |> Seq.toList with
        | '\t'::cs -> 
            let incompleteText = cs |> implode
            console.AutoComplete(autoComplete incompleteText env) |> tabCheck
        | cs -> cs |> implode
    tabCheck text