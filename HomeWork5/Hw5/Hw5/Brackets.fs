namespace Hw5

module Brackets =
    
    let isCorrectBracketSeq (str : string) =
        let reverseBracket = function
            | ')' -> '('
            | ']' -> '['
            | '}' -> '{'
            | _ -> failwith "it is not bracket"
        let rec isCorrectBracketSeq' (str : string) i stack =
            if i = str.Length then (List.isEmpty stack)
            else
                match str.[i] with
                | '(' | '{' | '[' as bracket -> isCorrectBracketSeq' str (i + 1) (bracket :: stack)
                | ')' | '}' | ']' as bracket -> let reversed = reverseBracket bracket
                                                match stack with
                                                | head :: tail when head = reversed -> isCorrectBracketSeq' str (i + 1) tail
                                                | _ -> false
                | _ -> isCorrectBracketSeq' str (i + 1) stack
        isCorrectBracketSeq' str 0 List.empty