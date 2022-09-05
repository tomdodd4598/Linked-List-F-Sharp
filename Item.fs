module Item

type Item<'T> = { value: 'T; next: Item<'T> option }
    with
        member this.Item with get index =
            let rec atIndex item index' =
                match index' with
                | 0 -> item
                | _ ->
                    match item with
                    | Some item' -> atIndex item'.next (index' - 1)
                    | None -> None
            atIndex (Some this) index

let itemPrintGetNext item =
    match item.next with
    | Some _ -> printf "%s, " (string item.value)
    | None -> printf "%s\n" (string item.value)
    item.next

let rec itemSequence item =
    seq {
        match item with
        | Some item' ->
            yield item
            yield! itemSequence item'.next
        | None -> ()
    }

let rec itemFold fSome fLast fEmpty accumulator item =
    match item with
    | Some item' ->
        let next = item'.next
        match next with
        | Some next' -> itemFold fSome fLast fEmpty (fSome item' next' accumulator) next
        | None -> fLast item' accumulator
    | None -> fEmpty accumulator

let rec itemFold' fStart fOnly fMiddle fLast fEmpty accumulator previous current =
    match current with
    | Some current' ->
        let next = current'.next
        match next with
        | Some next' ->
            let newAccumulator =
                match previous with
                | Some previous' -> fMiddle previous' current' next' accumulator
                | None -> fStart current' next' accumulator
            itemFold' fStart fOnly fMiddle fLast fEmpty newAccumulator current next
        | None ->
            match previous with
            | Some previous' -> fLast previous' current' accumulator
            | None -> fOnly current' accumulator
    | None -> fEmpty accumulator

let rec itemFoldback fSome fLast fEmpty generator item =
    match item with
    | Some item' ->
        let next = item'.next
        match next with
        | Some next' -> itemFoldback fSome fLast fEmpty (generator << fSome item' next') next
        | None -> generator (fLast item')
    | None -> generator (fEmpty ())

let rec itemFoldback' fStart fOnly fMiddle fLast fEmpty generator previous current =
    match current with
    | Some current' ->
        let next = current'.next
        match next with
        | Some next' ->
            let newInner =
                match previous with
                | Some previous' -> fMiddle previous' current' next'
                | None -> fStart current' next'
            itemFoldback' fStart fOnly fMiddle fLast fEmpty (generator << newInner) current next
        | None ->
            match previous with
            | Some previous' -> generator (fLast previous' current')
            | None -> generator (fOnly current')
    | None -> generator (fEmpty ())
