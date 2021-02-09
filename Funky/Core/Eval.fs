module Funky.Core.Eval

open System.IO
open System.Text
open FSharp.Compiler.Interactive.Shell
open Funky.Core.Common.Common

type FunkyFunction = Result<PipeValue,PipeValue> -> Result<PipeValue, PipeValue>    

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