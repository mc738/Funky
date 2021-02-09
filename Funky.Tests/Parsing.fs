module Funky.Tests.Parsing

open Funky.Core.Parsing
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type ParserTests () =

    [<TestMethod>]
    member this.``Parse inline function successfully`` () =
        let input = "(fun s -> s.ToUpper())"
        
        let expected = Token.Operation (OperationToken.InlineFunction { Function = "(fun s -> s.ToUpper())" })
        
        let result = Internal.tryParseInlineFunction (input |> Array.ofSeq) 0
        
        match result with
        | Ok (actual, _) -> Assert.AreEqual(expected, actual)
        | Error _ -> Assert.Fail()
        
    [<TestMethod>]
    member this.``Parse named function successfully`` () =
        let input = "post url"

        let expected = Token.Operation (OperationToken.NamedFunction { Name = "post"; Args = [ "url" ] })
        
        let result = Internal.tryParseNamedFunction [ "post" ] (input |> Array.ofSeq) 0
        
        match result with
        | Ok (actual, _) -> Assert.AreEqual(expected, actual)
        | Error _ -> Assert.Fail()
        
        
    [<TestMethod>]
    member this.``Parse built in successfully`` () =
        let input = "ls -a"
        
        let expected = Token.Operation (OperationToken.BuiltIn { Name = "ls"; Args = [ "-a" ] })
        
        let result = Internal.tryParseBuiltIn [ "ls" ] (input |> Array.ofSeq) 0
        
        match result with
        | Ok (actual, _) -> Assert.AreEqual(expected, actual)
        | Error _ -> Assert.Fail()
        
    [<TestMethod>]
    member this.``Parse program operation`` () =
        let input = "git -a"
        
        let expected = Token.Operation (OperationToken.Program { Name = "git"; Args = [ "-a" ] })
        
        let result = Internal.tryParseProgram (input |> Array.ofSeq) 0
        
        match result with
        | Ok (actual, _) -> Assert.AreEqual(expected, actual)
        | Error _ -> Assert.Fail()
            
    [<TestMethod>]
    member this.``Parse pipe operator`` () =
        let input = "|"
        
        let expected = Token.Operator (OperatorToken.PipeOperator)
        
        let result = Internal.tryParseOperator (input |> Array.ofSeq) 0
        
        match result with
        | Ok (actual, _) -> Assert.AreEqual(expected, actual)
        | Error _ -> Assert.Fail()
        
    [<TestMethod>]
    member this.``Parse function operator`` () =
        let input = "|>"
        
        let expected = Token.Operator (OperatorToken.FunctionOperator)
        
        let result = Internal.tryParseOperator (input |> Array.ofSeq) 0
        
        match result with
        | Ok (actual, _) -> Assert.AreEqual(expected, actual)
        | Error _ -> Assert.Fail()
        
    [<TestMethod>]
    member this.``Parse output operator successfully`` () =
        let input = ">"
        
        let expected = Token.Operator (OperatorToken.OutputOperator)
        
        let result = Internal.tryParseOperator (input |> Array.ofSeq) 0
        
        match result with
        | Ok (actual, _) -> Assert.AreEqual(expected, actual)
        | Error _ -> Assert.Fail()
        
    [<TestMethod>]
    member this.``Parse test input 1 successfully`` () =
        let input = "echo Hello |> (fun s -> s.ToUpperCase()) | hash |> post url"
        
        let expected = [
            Token.Operation (OperationToken.BuiltIn { Name = "echo"; Args = [ "Hello" ] })
            Token.Whitespace
            Token.Operator (OperatorToken.FunctionOperator)
            Token.Whitespace
            Token.Operation (OperationToken.InlineFunction { Function = "(fun s -> s.ToUpperCase())" })
            Token.Whitespace
            Token.Operator (OperatorToken.PipeOperator)
            Token.Whitespace
            Token.Operation (OperationToken.Program { Name = "hash"; Args = [] })
            Token.Whitespace
            Token.Operator (OperatorToken.FunctionOperator)
            Token.Whitespace
            Token.Operation (OperationToken.NamedFunction { Name = "post"; Args = [ "url" ] })            
        ]
        
        let actual = parse [ "echo" ] [ "post" ] (input |> Array.ofSeq)
        
        Assert.AreEqual(expected, actual)
        
      