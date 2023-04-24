open Helpers
open Item

open System.Numerics
open System.Text.RegularExpressions

let validRegex = Regex("^(0|-?[1-9][0-9]*|[A-Za-z][0-9A-Z_a-z]*)$", RegexOptions.Compiled)

let isValidString str = validRegex.IsMatch str

let insertBefore (value: string) (item: Item<string>) =
    let (parse, x), (parse', y) = BigInteger.TryParse(value), BigInteger.TryParse(item.value)
    if parse && parse' then x <= y else value <= item.value

let valueEquals item value = item.value = value

let rec update begin' start =
    if not begin' then printfn ""

    printfn "Awaiting input..."
    let input = System.Console.ReadLine ()
    let length = String.length input

    if length = 0 then
        printfn "\nProgram terminated!"
        removeAll start |> ignore
        0
    else
        let update' item = update false item
        
        let parseFail _ =
            printfn "\nCould not parse input!"
            update' start

        if input.[0] = '~' then
            if length = 1 then
                printfn "\nDeleting list..."
                update' (removeAll start)
            else
                let substr = input.[1..]
                if isValidString substr then
                    printfn "\nRemoving item..."
                    update' (removeItem start substr valueEquals)
                else
                    parseFail ()
        else
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
                if isValidString input then
                    printfn "\nInserting item..."
                    update' (insertItem start input insertBefore)
                else
                    parseFail ()

[<EntryPoint>]
let main argv = update true None
