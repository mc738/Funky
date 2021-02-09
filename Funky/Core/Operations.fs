module Funky.Core.Operations

open Funky.Core.Common.Common

module Operations =

    type Operation =
        | BuiltInExec of BuiltIn
        | ProgramExec of Program
        | NamedFunctionExec of NamedFunction
        | GenericFunctionExec of GenericFunction
        
    and BuiltIn = {
        Name: string
        Args: string list
    }

    and Program = {
        Name: string
        Args: string list
    }

    and NamedFunction = {
        Name: string
        Args: string list
    }

    and GenericFunction = {
        Body: string
    }

    let handleBuiltIn (op: BuiltIn) (value: PipeValue) =
        Ok(Single "")
        
    /// Handle an operation.
    /// Accepts a `PipedValue` representing stdin.
    /// returns a `PipeValue` represent stdout or a string representing stderr.
    /// If an error value is passed this is a no op and the value is simply passed through
    let handleOperation (op: Operation) (value: Result<PipeValue,_>) =
        match value with
        | Ok v ->        
            match op with
            | BuiltInExec op -> handleBuiltIn op v
            | ProgramExec op -> Error("Not implemented")
            | NamedFunctionExec op -> Error("Not implemented")
            | GenericFunctionExec op -> Error("Not implemented")
        | Error _ -> value