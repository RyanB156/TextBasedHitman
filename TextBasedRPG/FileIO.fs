module FileIO

open System
open System.IO
open DomainTypes


let inline writeToFile (path:string) (object:'T) = 
    use stream = File.Open(path, FileMode.Create)
    let binFormatter = new System.Runtime.Serialization.Formatters.Binary.BinaryFormatter()
    binFormatter.Serialize(stream, object)

let inline loadFromFile (path:string) = 
    use stream = File.Open(path, FileMode.Open)
    let binFormatter = new System.Runtime.Serialization.Formatters.Binary.BinaryFormatter()
    binFormatter.Deserialize(stream)


module RoomIO = // Save the room and its map to a file
    
    let castToRoomInfo (object:obj) =
        try
            let room : RoomInfo = downcast object
            Success room
        with
            | :? InvalidCastException as exn -> Failure (sprintf "%A cannot be converted to a room" object)

    /// Save (room, roomMap) to a file using the name of the room as the filename.
    let writeToFile (roomInfo:RoomInfo) =
        let (room, _) = roomInfo
        let path = @"RoomBinaries\" + room.Info.Name + ".bin"
        roomInfo |> writeToFile path

    /// Reads room info from a file using the specified room name. Returns a failure code or a success in the form (room, roomMap).
    let readFromFile roomName =
        let path = @"RoomBinaries\" + roomName + ".bin"
        let roomObj = loadFromFile path
        roomObj |> castToRoomInfo

module EnvironmentIO =
    
    let castToEnvironment (object:obj) =
        try
            let environment : Environment = downcast object
            Success environment
        with
            | :? InvalidCastException as exn -> Failure (sprintf "%A cannot be converted to an environment" object)

    let writeToFile (environment:Environment) =
        let path = @"EnvironmentBinaries\" + "Environment" + ".bin"
        environment |> writeToFile path

    let readFromFile =
        let path = @"EnvironmentBinaries\" + "Environment" + ".bin"
        let environmentObj = loadFromFile path
        environmentObj |> castToEnvironment


module WorldLoading =

    let createSaves roomInfoList = 
        let folderPath = @"RoomBinaries\"
        let di = new DirectoryInfo(folderPath)
        // Get all files in the folder and delete them. 
        di.EnumerateFiles() |> Seq.iter (fun f -> f.Delete())   // The old system always used the same names, so each file was overridden. Not here though.

        roomInfoList |> List.iter RoomIO.writeToFile



// RoomBinaries.bin
// EnvironmentBinaries.bin
