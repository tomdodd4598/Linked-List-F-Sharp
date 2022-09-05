open Helpers
open Item

open System.Numerics
open System.Text.RegularExpressions

let validRegex = Regex("^(0|-?[1-9][0-9]*|[A-Za-z][0-9A-Z_a-z]*)$", RegexOptions.Compiled)

let isValidString str = validRegex.IsMatch str

let insertBefore (value: string) (other: Item<string>) =
    let (parse, x), (parse', y) = BigInteger.TryParse(value), BigInteger.TryParse(other.value)
    match parse && parse' with
    | true -> x <= y
    | false -> value <= other.value

let valueEquals item value = item.value.Equals value

let rec update begin' start =
    match begin' with
    | true -> ()
    | false -> printfn ""

    printfn "Awaiting input..."
    let input = System.Console.ReadLine ()
    let length = String.length input

    let update' item = update false item

    let parseFail _ =
        printfn "\nCould not parse input!"
        update' start

    match length with
    | 0 ->
        printfn "\nProgram terminated!"
        removeAll start |> ignore
        0
    | _ ->
        match input.[0] with
        | '~' ->
            match length with
            | 1 ->
                printfn "\nDeleting list..."
                update' (removeAll start)
            | _ ->
                let substr = input.[1..]
                match (isValidString substr) with
                | true ->
                    printfn "\nRemoving item..."
                    update' (removeItem start substr valueEquals)
                | false -> parseFail ()
        | _ ->
            let printList str fPrint =
                printfn str
                fPrint start
                update' start

            match input with
            | "l" -> printList "\nLoop print..." printLoop
            | "i" -> printList "\nIterator print..." printIterator
            | "a" -> printList "\nArray print..." printArray
            | "r" -> printList "\nRecursive print..." printRecursive
            | "f" -> printList "\nFold print..." printFold
            | "b" -> printList "\nFoldback print..." printFoldback
            | _ ->
                match (isValidString input) with
                | true ->
                    printfn "\nInserting item..."
                    update' (insertItem start input insertBefore)
                | false -> parseFail ()

[<EntryPoint>]
let main argv = update true None
