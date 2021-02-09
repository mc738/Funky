module Funky.Core.Parsing

open System
open FUtil.Arrays

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
    | Whitespace
    | Error of string



module Internal =

    let parseArg input i =
        let r = Chars.readTo ' ' i input

        match r with
        | Some (tokenChars, nextIndex) ->
            if nextIndex > i then
                Ok(String tokenChars, nextIndex)
            else
                Error()
        | None -> Error()

    let inList (chars: char list) c =
        chars
        |> List.fold (fun state item -> if state = true then true else item = c) false

    let isOperatorChar c = inList [ '|'; '>' ] c

    let parseArgs input i =

        // Handle empty args....
        
        let rec handler (args: string list, i) =
            match i with
            | _ when inBounds input i = false -> (args, i)
            | _ when isOperatorChar input.[i] -> (args, i)
            | _ ->
                let r = parseArg input i

                match r with
                | Ok (arg, nextIndex) -> handler (List.append args [ arg ], nextIndex)
                | Error _ -> (args, i)
    
        match tryGet input i with
        | Some c ->
            match c with
            | _ when c = ' ' || isOperatorChar c -> [], i - 1 // Empty args
            | _ -> handler (List.empty, i)
        | _ -> [], i



    let tryParseInlineFunction (input: char array) i =
        // Check the current index exists and is '('.
        match tryGet input i with
        | Some c when c = '(' ->
            match Chars.readToMatched '(' ')' i input with
            | Some (tokenChars, nextIndex) ->
                let token = { Function = (String (tokenChars |> Array.rev)) }

                Ok(Token.Operation(OperationToken.InlineFunction token), nextIndex + 1)
            | None -> Error()    
        | _ -> Error()

    let tryParseBuiltIn (names: string list) (input: char array) i =
        let r = Chars.readTo ' ' i input

        match r with
        | Some (tokenChars, nextIndex) ->
            let name = String tokenChars
            match List.contains name names with
            | true ->
                let (args, nextIndex) = parseArgs input (nextIndex + 1)
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
                let (args, nextIndex) = parseArgs input (nextIndex + 1)
                let token: NamedFunctionToken = { Name = name; Args = args }
                Ok(Token.Operation(OperationToken.NamedFunction token), nextIndex)
            | false -> Error()
        | None -> Error()

    let tryParseProgram (input: char array) i =
        let r = Chars.readTo ' ' i input

        match r with
        | Some (tokenChars, nextIndex) ->
            let name = String tokenChars
            let (args, nextIndex) = parseArgs input (nextIndex + 1)
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

    let tryParseWhitespace (input: char array) i =
        match i with
        | _ when inBounds input i = false -> Error()
        | _ when input.[i] = ' ' -> Ok(Token.Whitespace, i + 1)
        | _ -> Error()
    

    let tryParse builtInNames functionNames (input: char array) i =
        [ tryParseWhitespace
          tryParseOperator 
          tryParseOperation builtInNames functionNames ]
        |> List.fold (fun state p ->
            match state with
            | Ok _ -> state
            | Error _ -> p input i) (Error())

let parse builtInNames functionNames (input: char array) =

    let rec handler(tokens: Token list, i: int) =
        match i with
        | _ when input.Length - 1 > i = false -> tokens
        | _ ->
            match Internal.tryParse builtInNames functionNames input i with
            | Ok (token, next) -> handler(List.append tokens [ token ], next)
            | Error _ -> List.append tokens [ Internal.createError "Could not parse." ]

    handler(List.empty, 0)