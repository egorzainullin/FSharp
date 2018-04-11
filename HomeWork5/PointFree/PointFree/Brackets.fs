namespace Hw5

module Brackets =
    
    let IsCorrectBracketSeq (str : string) =
        let rec IsCorrectBracketSeq' (str : string) i stack =
            if i = str.Length then (List.isEmpty stack)
            else
                match str.[i] with
                | '(' -> IsCorrectBracketSeq' str (i + 1) ('(' :: stack)
                | '{' -> IsCorrectBracketSeq' str (i + 1) ('{' :: stack)
                | '[' -> IsCorrectBracketSeq' str (i + 1) ('[' :: stack)
                | ')' -> match stack with
                         | '(' :: tail -> IsCorrectBracketSeq' str (i + 1) tail
                         | _ -> false
                | '}' -> match stack with
                         | '{' :: tail -> IsCorrectBracketSeq' str (i + 1) tail
                         | _ -> false
                | ']' -> match stack with
                         | '[' :: tail -> IsCorrectBracketSeq' str (i + 1) tail
                         | _ -> false
                | _ -> IsCorrectBracketSeq' str (i + 1) stack
        IsCorrectBracketSeq' str 0 List.empty
