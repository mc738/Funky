namespace Funky.Core.Common

open System

module Common =
    
    type PipeValue =
        | Single of string
        | Collection of string seq
        
    let join separator (data: string seq) =
        String.Join(separator, data)
                
    let split separator (data: string) = data.Split(separator) |> Seq.ofArray
    
    /// A helper method to convert a `PipeValue` to a `Single`.
    /// If already a `Single` this is a no op and the value is passed through. 
    let toSingle separator (value: PipeValue) =
        match value with
        | Single _ -> value
        | Collection c -> Single(join "" c)
      
    /// A helper method to convert a `PipeValue` to a `Single`.
    /// If already a `Single` this is a no op and the value is passed through. 
    let toCollection separator (value: PipeValue) =
        match value with
        | Single s -> Collection (split separator s)
        | Collection _ -> value