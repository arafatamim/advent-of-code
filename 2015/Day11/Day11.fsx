(*
 * Advent of Code 2015 Day 11: Corporate Policy
 * https://adventofcode.com/2015/day/11
 *)

let (&&>>) f g x = f x && g x

module List =
    let groupByValue list =
        List.foldBack
            (fun x acc ->
                match acc, x with
                | [], _ -> [ [ x ] ]
                | (h :: t) :: rest, x when h = x -> (x :: h :: t) :: rest
                | acc, x -> [ x ] :: acc)
            list
            []


let hasConsecutive =
    Seq.windowed 3
    >> Seq.exists (fun x -> char (int x[0] + 1) = x[1] && char (int x[1] + 1) = x[2])


let hasDoubles =
    Seq.toList
    >> List.groupByValue
    >> List.filter (fun l -> l.Length >= 2)
    >> List.length
    >> (<=) 2


let hasNoForbidden =
    Seq.forall (fun c -> not (c = 'i') && not (c = 'o') && not (c = 'l'))


let isPasswordValid = hasDoubles &&>> hasConsecutive &&>> hasNoForbidden


let rec incrementString (str: string) =
    let s = str[0 .. str.Length - 2]

    match str[str.Length - 1] with
    | 'z' -> (incrementString s) + string 'a'
    | c -> s + string (char (int c + 1))


let rec findNewPassword password =
    let newPass = incrementString password

    if isPasswordValid newPass then newPass else findNewPassword newPass


let input = "vzbxkghb"

// Part I
let password = findNewPassword input
printfn "Santa's next best password: %s" password

// Part II
findNewPassword password |> printfn "Santa's next password after expiration: %s"
