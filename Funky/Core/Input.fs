module Funky.Core.Input

open System
open FSharp.Compiler.AbstractIL.Internal
open FUtil.Arrays

(*
module Parsing =

    type InlineFunctionToken = { Function: string }

    type NamedFunctionToken = { Name: string; Args: string list }

    type BuiltInToken = { Name: string; Args: string list }

    type ProgramToken = { Name: string; Args: string list }


    [<RequireQualifiedAccess>]
    type OperationToken =
        | InlineFunction of InlineFunctionToken
        | NamedFunction of NamedFunctionToken
        | BuiltIn of BuiltInToken
        | Program of ProgramToken

    [<RequireQualifiedAccess>]
    type OperatorToken =
        | PipeOperator
        | FunctionOperator
        | OutputOperator

    [<RequireQualifiedAccess>]
    type Token =
        | Operation of OperationToken
        | Operator of OperatorToken
        | Error of string

    module private Internal =

        let parseArg input i =
            let r = Chars.readTo ' ' i input

            match r with
            | Some (tokenChars, nextIndex) -> Ok(String tokenChars, nextIndex)
            | None -> Error()

        let inList (chars: char list) c =
            chars
            |> List.fold (fun state item -> if state = true then true else item = c) false

        let isOperatorChar c = inList [ '|'; '>' ] c

        let parseArgs input i =

            let rec handler (args: string list, i) =
                match i with
                | _ when inBounds input i = false -> (args, i)
                | _ when isOperatorChar input.[i] -> (args, i)
                | _ ->
                    let r = parseArg input i

                    match r with
                    | Ok (arg, nextIndex) -> handler (List.append args [ arg ], nextIndex)
                    | Error _ -> (args, i)

            handler (List.empty, i)



        let tryParseInlineFunction (input: char array) i =
            let r = Chars.readToMatched '(' ')' i input

            match r with
            | Some (tokenChars, nextIndex) ->
                let token = { Function = (String (tokenChars |> Array.rev)) }

                Ok(Token.Operation(OperationToken.InlineFunction token), nextIndex)
            | None -> Error()

        let tryParseBuiltIn (names: string list) (input: char array) i =
            let r = Chars.readTo ' ' i input

            match r with
            | Some (tokenChars, nextIndex) ->
                let name = String tokenChars

                match List.contains name names with
                | true ->
                    let (args, nextIndex) = parseArgs input nextIndex
                    let token: BuiltInToken = { Name = name; Args = args }
                    Ok(Token.Operation(OperationToken.BuiltIn token), nextIndex)
                | false -> Error()
            | None -> Error()

        let tryParseNamedFunction (names: string list) (input: char array) i =
            let r = Chars.readTo ' ' i input

            match r with
            | Some (tokenChars, nextIndex) ->
                let name = String tokenChars

                match List.contains name names with
                | true ->
                    let (args, nextIndex) = parseArgs input nextIndex
                    let token: NamedFunctionToken = { Name = name; Args = args }
                    Ok(Token.Operation(OperationToken.NamedFunction token), nextIndex)
                | false -> Error()
            | None -> Error()

        let tryParseProgram (input: char array) i =
            let r = Chars.readTo ' ' i input

            match r with
            | Some (tokenChars, nextIndex) ->
                let name = String tokenChars
                let (args, nextIndex) = parseArgs input nextIndex
                let token: ProgramToken = { Name = name; Args = args }
                Ok(Token.Operation(OperationToken.Program token), nextIndex)
            | None -> Error()

        let createError message = Token.Error message

        let tryParseOperation builtInNames functionNames (input: char array) i =
            [ tryParseInlineFunction
              tryParseBuiltIn builtInNames
              tryParseNamedFunction functionNames
              tryParseProgram ]
            |> List.fold (fun state p ->
                match state with
                | Ok _ -> state
                | Error _ -> p input i) (Error())

        let tryParseOperator (input: char array) i =
            match i with
            | _ when inBounds input i = false -> Error()
            | _ when input.[i] = '|' ->
                match lookAhead 1 input i with
                | Some c when c = '>' ->
                    let token = OperatorToken.FunctionOperator
                    Ok(Token.Operator token, i + 2)
                | _ ->
                    let token = OperatorToken.PipeOperator
                    Ok(Token.Operator token, i + 1)
            // Output operator (`>`).
            | _ when input.[i] = '>' ->
                let token = OperatorToken.OutputOperator
                Ok(Token.Operator token, i + 1)
            | _ -> Error()


        let tryParse builtInNames functionNames (input: char array) i =
            [ tryParseOperation builtInNames functionNames
              tryParseOperator ]
            |> List.fold (fun state p ->
                match state with
                | Ok _ -> state
                | Error _ -> p input i) (Error())

    let parse builtInNames functionNames (input: char array) =

        let rec handler(tokens: Token list, i: int) =
            match i with
            | _ when inBounds input i = false -> tokens
            | _ ->
                match Internal.tryParse builtInNames functionNames input i with
                | Ok (token, next) -> handler(List.append tokens [ token ], next)
                | Error _ -> List.append tokens [ Internal.createError "Could not parse." ]

        handler(List.empty, 0)
//let rec handler(tokens: Token list, i: int) =
*)



//    handler(List.empty, 0)

type InputBuffer =
    { Prompt: String
      Buffer: char seq }
    member input.Print() = ()


//let i = ()

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
            if Console.CursorLeft > prompt.Length
            then Console.SetCursorPosition(Console.CursorLeft - 1, Console.CursorTop)

            (true, state)
        | ConsoleKey.RightArrow ->
            if Console.CursorLeft < prompt.Length + state.Length
            then Console.SetCursorPosition(Console.CursorLeft + 1, Console.CursorTop)

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
