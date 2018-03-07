namespace Homework3

module Tree =

    type Tree<'T> = 
        | Tip of 'T
        | Tree of 'T * Tree<'T> * Tree<'T>

    let rec map (f : 'a -> 'a) tree = 
        match tree with
        | Tip t -> Tip(f t)
        | Tree (node, left, right) -> Tree(f node, map f left, map f right)

    type Expression =
        | Operand of int
        | Plus of Expression * Expression
        | Minus of Expression * Expression
        | Divide of Expression * Expression
        | Multiply of Expression * Expression

    let rec calculate expr = 
        match expr with
        | Operand x -> x
        | Plus (x, y) -> calculate x + calculate y
        | Minus (x, y) -> calculate x - calculate y
        | Divide (x, y) -> calculate x / calculate y
        | Multiply (x, y) -> calculate x * calculate y