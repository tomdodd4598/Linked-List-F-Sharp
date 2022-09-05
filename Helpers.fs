module Helpers

open Item

let insertItem start value' insertBefore =
    printfn "Creating item: %s" (string value')

    let insert item before after =
        match (insertBefore value' item) with
        | true -> before
        | false -> after

    let fStart current _ innerVal = insert current {value = value'; next = Some current} {value = current.value; next = Some innerVal}
    let fOnly current = insert current {value = value'; next = Some current} {value = current.value; next = Some {value = value'; next = None}}
    let fMiddle previous current next innerVal = insert previous current (fStart current next innerVal)
    let fLast previous current = insert previous current (fOnly current)
    let fEmpty _ = {value = value'; next = None}

    Some (itemFoldback' fStart fOnly fMiddle fLast fEmpty id None start)

let removeItem start value' valueEquals =
    let remove item remove retain =
        match (valueEquals item value') with
        | true -> remove
        | false -> retain

    let fStart current next innerVal = remove current (true, Some next) (fst innerVal, Some {value = current.value; next = snd innerVal})
    let fOnly current = remove current (true, None) (false, Some current)
    let fMiddle previous current next innerVal = remove previous (true, Some current) (fStart current next innerVal)
    let fLast previous current = remove previous (true, Some current) (fOnly current)
    let fEmpty _ = false, None

    let removed, result = itemFoldback' fStart fOnly fMiddle fLast fEmpty id None start
    match removed with
    | true -> printfn "Removed item: %s" (string value')
    | false -> printfn "Item %s does not exist!" (string value')
    result

let removeAll _ = None

let printLoop (start: _ option) =
    let mutable item = start
    while item.IsSome do
        item <- itemPrintGetNext item.Value

let printIterator start =
    Seq.iter (fun item ->
        match item with
        | Some item' -> itemPrintGetNext item' |> ignore
        | None -> ()) (itemSequence start)

let printArray (start: Item<_> option) =
    match start with
    | Some start' ->
        let rec printIndexGetNext index =
            match start'.[index] with
            | Some item ->
                itemPrintGetNext item |> ignore
                printIndexGetNext (index + 1)
            | None -> ()
        printIndexGetNext 0
    | None -> ()

let rec printRecursive start =
    match start with
    | Some start' -> printRecursive (itemPrintGetNext start')
    | None -> ()

let printFold start =
    let fSome current _ accumulator = sprintf "%s%s, " accumulator (string current.value)
    let fLast current accumulator = sprintf "%s%s\n" accumulator (string current.value)
    let fEmpty accumulator = accumulator
    printf "%s" (itemFold fSome fLast fEmpty "" start)

let printFoldback start =
    let fSome current _ innerVal = sprintf "%s, %s" (string current.value) innerVal
    let fLast current = sprintf "%s\n" (string current.value)
    let fEmpty _ = ""
    printf "%s" (itemFoldback fSome fLast fEmpty id start)
