module Item

type Item<'T> = { value: 'T; next: Item<'T> option }
    with
        member this.Item with get index =
            let rec atIndex item index' =
                if index' = 0 then
                    item
                else
                    match item with
                    | None -> None
                    | Some item' -> atIndex item'.next (index' - 1)
            atIndex (Some this) index

let printGetNext item =
    printf "%s%s" item.value (if item.next.IsNone then "\n" else ", ")
    item.next

let rec itemSequence item =
    seq {
        match item with
        | None -> ()
        | Some item' ->
            yield item
            yield! itemSequence item'.next
    }

let rec itemFold fSome fLast fEmpty accumulator item =
    match item with
    | None -> fEmpty accumulator
    | Some item' ->
        let next = item'.next
        match next with
        | None -> fLast item' accumulator
        | Some next' -> itemFold fSome fLast fEmpty (fSome item' next' accumulator) next

let rec itemFold' fStart fOnly fMiddle fLast fEmpty accumulator previous current =
    match current with
    | None -> fEmpty accumulator
    | Some current' ->
        let next = current'.next
        match next with
        | None ->
            match previous with
            | None -> fOnly current' accumulator
            | Some previous' -> fLast previous' current' accumulator
        | Some next' ->
            let newAccumulator =
                match previous with
                | None -> fStart current' next' accumulator
                | Some previous' -> fMiddle previous' current' next' accumulator
            itemFold' fStart fOnly fMiddle fLast fEmpty newAccumulator current next

let rec itemFoldback fSome fLast fEmpty generator item =
    match item with
    | None -> generator (fEmpty ())
    | Some item' ->
        let next = item'.next
        match next with
        | None -> generator (fLast item')
        | Some next' -> itemFoldback fSome fLast fEmpty (generator << fSome item' next') next

let rec itemFoldback' fStart fOnly fMiddle fLast fEmpty generator previous current =
    match current with
    | None -> generator (fEmpty ())
    | Some current' ->
        let next = current'.next
        match next with
        | None ->
            match previous with
            | None -> generator (fOnly current')
            | Some previous' -> generator (fLast previous' current')
        | Some next' ->
            let newInner =
                match previous with
                | None -> fStart current' next'
                | Some previous' -> fMiddle previous' current' next'
            itemFoldback' fStart fOnly fMiddle fLast fEmpty (generator << newInner) current next
