open System.IO

let input = File.ReadLines "input.txt" |> List.ofSeq

let rec group input result =
    match input with
    | [] -> result
    | "" :: rest -> group rest (0 :: result)
    | cals :: rest ->
        group
            rest
            (match result with
             | [] -> [ int cals ]
             | head :: tail -> (head + int cals) :: tail)

let elfCalories = group input [] |> List.sortDescending

let maxCalories = elfCalories |> List.max

// Part I
printfn "Most calories a single elf is carrying: %d" maxCalories

let topThreeCalories = elfCalories |> List.take 3
let totalTopThreeCalories = topThreeCalories |> List.sum

// Part II
printfn "Number of calories the top three elves are carrying: %d" totalTopThreeCalories
