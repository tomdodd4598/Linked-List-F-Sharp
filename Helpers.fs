module Helpers

open Item

let insertItem start value' insertBefore =
    printfn "Creating item: %s" value'

    let insert item before after = if insertBefore value' item then before else after

    let fStart current _ innerVal = insert current {value = value'; next = Some current} {value = current.value; next = Some innerVal}
    let fOnly current = insert current {value = value'; next = Some current} {value = current.value; next = Some {value = value'; next = None}}
    let fMiddle previous current next innerVal = insert previous current (fStart current next innerVal)
    let fLast previous current = insert previous current (fOnly current)
    let fEmpty _ = {value = value'; next = None}

    Some (itemFoldback' fStart fOnly fMiddle fLast fEmpty id None start)

let removeItem start value' valueEquals =
    let remove item remove retain = if valueEquals item value' then remove else retain

    let fStart current next innerVal = remove current (true, Some next) (fst innerVal, Some {value = current.value; next = snd innerVal})
    let fOnly current = remove current (true, None) (false, Some current)
    let fMiddle previous current next innerVal = remove previous (true, Some current) (fStart current next innerVal)
    let fLast previous current = remove previous (true, Some current) (fOnly current)
    let fEmpty _ = false, None

    let removed, result = itemFoldback' fStart fOnly fMiddle fLast fEmpty id None start
    printfn (if removed then "Removed item: %s" else "Item %s does not exist!") value'
    result

let removeAll _ = None

let printLoop (start: _ option) =
    let mutable item = start
    while item.IsSome do
        item <- printGetNext item.Value

let printIterator start =
    Seq.iter (fun item ->
        match item with
        | None -> ()
        | Some item' -> printGetNext item' |> ignore) (itemSequence start)

let printArray (start: Item<_> option) =
    match start with
    | None -> ()
    | Some start' ->
        let rec indexPrintGetNext index =
            match start'.[index] with
            | None -> ()
            | Some item ->
                printGetNext item |> ignore
                indexPrintGetNext (index + 1)
        indexPrintGetNext 0

let rec printRecursive start =
    match start with
    | None -> ()
    | Some start' -> printRecursive (printGetNext start')

let printFold start =
    let fSome current _ accumulator = sprintf "%s%s, " accumulator current.value
    let fLast current accumulator = sprintf "%s%s\n" accumulator current.value
    let fEmpty accumulator = accumulator
    printf "%s" (itemFold fSome fLast fEmpty "" start)

let printFoldback start =
    let fSome current _ innerVal = sprintf "%s, %s" current.value innerVal
    let fLast current = sprintf "%s\n" current.value
    let fEmpty _ = ""
    printf "%s" (itemFoldback fSome fLast fEmpty id start)
