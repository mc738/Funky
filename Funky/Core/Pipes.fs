module Funky.Core.Pipes
open System
open Funky.Core.Common.Common
open Funky.Core.Operations
open Funky.Core.Operations.Operations

[<RequireQualifiedAccess>]
type OutputType =
    | Console
    | File of string
    | Function of string

type Pipe = { Steps: Operation list; Output: OutputType }

let create output steps = {
    Steps = steps
    Output = output
}

let createDefault steps = create OutputType.Console steps

let private handleSingle output (data: string) =
    match output with
    | OutputType.Console -> Console.WriteLine(data)
    | OutputType.File p -> ()
    | OutputType.Function f -> ()
        
let private handleCollection output (data: string seq) =
    match output with
    | OutputType.Console -> data |> Seq.map (fun s -> Console.WriteLine(s)) |> ignore
    | OutputType.File p -> ()
    | OutputType.Function f -> ()
        
let private handleError output (message: string) =
    match output with
    | OutputType.Console -> Console.WriteLine(sprintf "\u001b[31mError: %s\u001b[0m" message)
    | OutputType.File p -> ()
    | OutputType.Function f -> ()

let private handleResult output (result: Result<PipeValue, string>) =
    match result with
    | Ok pv ->
        match pv with
        | Single s -> handleSingle output s
        | Collection c -> handleCollection output c
    | Error m -> handleError output m
        
    
let execute output pipe =    
    pipe.Steps
    |> List.fold (fun next op -> handleOperation op next) (Ok(Single ""))
    |> handleResult output

    
    
    
    