module MyString

let fsttenji = '⠀'

let x i = pown 2 i

let somefun n = 
    match n with
    | 1 -> x 6
    | 2 -> x 2
    | 3 -> x 1
    | 4 -> x 0
    | 5 -> x 7
    | 6 -> x 5
    | 7 -> x 4
    | 8 -> x 3
    | _ -> x 0

let somefun2 n = 
    char (match n with
            | 1 -> int fsttenji + somefun 1
            | 2 -> int fsttenji + somefun 1 + somefun 2
            | 3 -> int fsttenji + somefun 1 + somefun 2 + somefun 3
            | 4 -> int fsttenji + somefun 1 + somefun 2 + somefun 3 + somefun 4
            | 5 -> int fsttenji + somefun 1 + somefun 2 + somefun 3 + somefun 4 + somefun 5
            | 6 -> int fsttenji + somefun 1 + somefun 2 + somefun 3 + somefun 4 + somefun 5 + somefun 6
            | 7 -> int fsttenji + somefun 1 + somefun 2 + somefun 3 + somefun 4 + somefun 5 + somefun 6 + somefun 7
            | 8 -> int fsttenji + somefun 1 + somefun 2 + somefun 3 + somefun 4 + somefun 5 + somefun 6 + somefun 7 + somefun 8
            | _ -> int fsttenji)

let int2bar n = 
    let c = n / 8
    let m = n % 8
    let ans = Array.append [|for i in 1 .. c ->
                                '⣿'
                            |] [|somefun2 m|]
    System.String ans