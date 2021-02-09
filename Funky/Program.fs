// Learn more about F# at http://fsharp.org

open System
open System.IO
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Interactive.Shell
open Funky.Core
open Funky.Core.Input

module Input =

    type Input =
        { Buffer: char seq }
        member input.AsString = String(input.Buffer |> Seq.toArray)

        member _.FromString(text: string): Input = { Buffer = text }

    let print prompt state =
        let currPos = (Console.CursorLeft, Console.CursorTop)

        Console.SetCursorPosition(0, Console.CursorTop)

        let output =
            (sprintf
                "%s%s%s"
                 prompt
                 state
                 (String
                     (' ',
                      Console.LargestWindowWidth
                      - prompt.Length
                      - state.Length)))

        Console.Write(output)

        Console.SetCursorPosition(currPos)

    let rec readInput (prompt: string, state: char array) =

        let cursorOffset = Console.CursorLeft - prompt.Length

        let input = Console.ReadKey(true)

        let (cont, newState) =
            match input.Key with
            | ConsoleKey.LeftArrow ->
                if Console.CursorLeft > prompt.Length then
                    Console.SetCursorPosition(Console.CursorLeft - 1, Console.CursorTop)
                (true, state)
            | ConsoleKey.RightArrow ->
                if Console.CursorLeft < prompt.Length + state.Length then
                    Console.SetCursorPosition(Console.CursorLeft + 1, Console.CursorTop)
                (true, state)
            | ConsoleKey.UpArrow ->
                // TODO Handle history.
                (true, state)
            | ConsoleKey.DownArrow ->
                // TODO Handle history.
                (true, state)
            | ConsoleKey.Enter ->
                Console.Write(Environment.NewLine)
                (false, state)
            | ConsoleKey.Backspace ->
                if state.Length > 0 then
                    Console.SetCursorPosition(Console.CursorLeft - 1, Console.CursorTop)
                    (true, Array.append state.[0..(cursorOffset - 2)] state.[(cursorOffset)..])
                else
                    (true, state)
            | ConsoleKey.Delete -> (true, Array.append state.[0..(cursorOffset - 1)] state.[(cursorOffset + 1)..])
            | _ ->
                let newState =
                    Array.concat [ state.[0..(cursorOffset - 1)]
                                   [| input.KeyChar |]
                                   state.[(cursorOffset)..] ]

                Console.SetCursorPosition(Console.CursorLeft + 1, Console.CursorTop)
                (true, newState)

        if cont then
            print prompt (newState |> String)
            readInput (prompt, newState)
        else
            newState

(*
module Eval =
    open System
    open System.IO
    open System.Text

    // Initialize output and input streams
    let sbOut = new StringBuilder()
    let sbErr = new StringBuilder()
    let inStream = new StringReader("")
    let outStream = new StringWriter(sbOut)
    let errStream = new StringWriter(sbErr)

    // Build command line arguments & start FSI session
    let argv = [| "C:\\fsi.exe" |]
    let allArgs = Array.append argv [|"--noninteractive"|]

    let fsiConfig = FsiEvaluationSession.GetDefaultConfiguration()
    let fsiSession = FsiEvaluationSession.Create(fsiConfig, allArgs, inStream, outStream, errStream)

    let evalExpression<'a> text =
      match fsiSession.EvalExpression(text) with
      | Some value -> value.ReflectionValue |> unbox<'a>
      | None -> failwith "Got no result!"
*)

let rec repl () =

    let prompt =
        sprintf "[%s@%s %s]$ " Environment.UserName Environment.MachineName (Directory.GetCurrentDirectory())

    //printf "%s" prompt

    Input.print prompt ""

    Console.SetCursorPosition(prompt.Length, Console.CursorTop)

    let command =
        Input.readInput (prompt, Array.empty) |> String

    // let result = Eval.evalExpression<string> command

    printfn "\tCommand: %s" command

    repl ()


[<EntryPoint>]
let main argv =
   
    let test = "echo hello | hash |> (fun s -> s.ToUpperCase()) |> post www.example.com > result.txt "
    
    let r = Parsing.parse [ "echo" ] [ "post" ] (test |> Array.ofSeq)
    
    printfn "********* %A" r
     
    //printfn "\u001b[32mWelcome to Funky! A shell written in F#.\u001b[0m"
    //Directory.SetCurrentDirectory(Environment.GetFolderPath(Environment.SpecialFolder.Personal))
    //repl ()
    0 // return an integer exit code
