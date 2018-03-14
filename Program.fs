// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
module Main 
open ProgramData
open System


[<EntryPoint>]
let main argv =  
    initializeGame ()
    Console.ReadLine() |> ignore
    0 // return an integer exit code
