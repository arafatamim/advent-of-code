let tupleApply f (a, b) = f a, f b

///"2-4,6-8" -> ("2-4", "6-8")
let makePair (str: string) =
    match str.Split(",") with
    | [| x; y |] -> (x, y)
    | _ -> failwith "Unreachable"

///"2-4" -> Set [2; 3; 4]
let makeRange (str: string) =
    match str.Split("-") with
    | [| x; y |] -> Set [ int x .. int y ]
    | _ -> failwith "Unreachable"

let parsePair = makePair >> tupleApply makeRange

let input = System.IO.File.ReadLines "input.txt" |> Seq.map parsePair

// Part I

let isSubset (x, y) = Set.isSubset x y || Set.isSubset y x

///Filter range pairs that fully contains the other
let subsettingRanges = Seq.filter isSubset

printfn "Number of Elf assignment pairs that fully contain the other: %d" (input |> subsettingRanges |> Seq.length)

// Part II

let isIntersect (x, y) = Set.intersect x y <> Set.empty

///Filter range pairs that overlap one another
let intersectingRanges = Seq.filter isIntersect

printfn "Number of Elf assignment pairs that overlap: %d" (input |> intersectingRanges |> Seq.length)
