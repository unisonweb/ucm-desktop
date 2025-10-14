{-
   WorkspaceItems is the main data structure for working with definitions in
   a WorkspacePane. It supports holding Loading definitions and inserting
   definitions after another.

   It features a `focus` indicator and a before and after just like a Zipper.
   `focus` can be changed with `next` and `prev`.

   TODO: we don't care about this invariant any more, now that we have split panes?
   Invariants:
     It structurally can't hold the invariant that it should not contain
     duplicates, so this is instead enforced via the API; uniqueness is
     determined by Reference and Hash.
-}
-- TODO: All reference equality check should reach into definition for hash
-- equality when possible


module Code2.Workspace.WorkspaceItems exposing (..)

import Code.Definition.Reference exposing (Reference)
import Code.FullyQualifiedName exposing (FQN)
import Code2.Workspace.DefinitionMatchesState exposing (DefinitionMatchesState)
import Code2.Workspace.DefinitionWorkspaceItemState exposing (DefinitionWorkspaceItemState)
import Code2.Workspace.WorkspaceItem as WorkspaceItem exposing (WorkspaceItem)
import Code2.Workspace.WorkspaceItemRef as WorkspaceItemRef exposing (WorkspaceItemRef)
import List
import List.Extra as ListE
import Maybe.Extra as MaybeE


{-| This technically allows multiple of the same definition across the 3 fields.
This is conceptionally not allowed and is enforced by the helper functions.
-}
type WorkspaceItems
    = Empty
    | WorkspaceItems
        { before : List WorkspaceItem
        , focus : WorkspaceItem
        , after : List WorkspaceItem
        }


init : Maybe WorkspaceItem -> WorkspaceItems
init focused =
    case focused of
        Nothing ->
            Empty

        Just i ->
            singleton i


fromItems :
    List WorkspaceItem
    -> WorkspaceItem
    -> List WorkspaceItem
    -> WorkspaceItems
fromItems before focus_ after =
    WorkspaceItems { before = before, focus = focus_, after = after }


empty : WorkspaceItems
empty =
    Empty


isEmpty : WorkspaceItems -> Bool
isEmpty workspaceItems =
    case workspaceItems of
        Empty ->
            True

        WorkspaceItems _ ->
            False


length : WorkspaceItems -> Int
length items =
    items
        |> toList
        |> List.length


singleton : WorkspaceItem -> WorkspaceItems
singleton item =
    WorkspaceItems { before = [], focus = item, after = [] }



-- MODIFY


appendWithFocus : WorkspaceItems -> WorkspaceItem -> WorkspaceItems
appendWithFocus items item =
    WorkspaceItems { before = toList items, focus = item, after = [] }


prependWithFocus : WorkspaceItems -> WorkspaceItem -> WorkspaceItems
prependWithFocus workspaceItems item =
    case workspaceItems of
        Empty ->
            singleton item

        WorkspaceItems items ->
            WorkspaceItems
                { before = []
                , focus = item
                , after =
                    items.before ++ (items.focus :: items.after)
                }


{-| Insert before a Hash. If the Hash is not in WorkspaceItems, insert with
focus. If the element to insert already exists in WorkspaceItems, move it to
after the provided Hash
-}
insertWithFocusBefore :
    WorkspaceItems
    -> WorkspaceItemRef
    -> WorkspaceItem
    -> WorkspaceItems
insertWithFocusBefore items beforeRef toInsert =
    case items of
        Empty ->
            singleton toInsert

        WorkspaceItems _ ->
            if includesItem items beforeRef then
                let
                    insertBefore item =
                        if WorkspaceItem.isSameRef item beforeRef then
                            [ toInsert, item ]

                        else
                            [ item ]

                    make ( before, afterInclusive ) =
                        WorkspaceItems
                            { before = before
                            , focus = toInsert
                            , after = List.drop 1 afterInclusive
                            }
                in
                items
                    |> toList
                    |> List.concatMap insertBefore
                    |> ListE.splitWhen (WorkspaceItem.isSameByRef toInsert)
                    |> Maybe.map make
                    |> Maybe.withDefault (singleton toInsert)

            else
                prependWithFocus items toInsert


{-| Insert after a Hash. If the Hash is not in WorkspaceItems, insert at the
end. If the element to insert already exists in WorkspaceItems, move it to
after the provided Hash
-}
insertWithFocusAfter :
    WorkspaceItems
    -> WorkspaceItemRef
    -> WorkspaceItem
    -> WorkspaceItems
insertWithFocusAfter items afterRef toInsert =
    case items of
        Empty ->
            singleton toInsert

        WorkspaceItems _ ->
            if includesItem items afterRef then
                let
                    insertAfter item =
                        if WorkspaceItem.isSameRef item afterRef then
                            [ item, toInsert ]

                        else
                            [ item ]

                    make ( before, afterInclusive ) =
                        WorkspaceItems
                            { before = before
                            , focus = toInsert
                            , after = List.drop 1 afterInclusive
                            }
                in
                items
                    |> toList
                    |> List.concatMap insertAfter
                    |> ListE.splitWhen (WorkspaceItem.isSameByRef toInsert)
                    |> Maybe.map make
                    |> Maybe.withDefault (singleton toInsert)

            else
                appendWithFocus items toInsert


replace : WorkspaceItems -> WorkspaceItemRef -> WorkspaceItem -> WorkspaceItems
replace items ref newItem =
    let
        replaceMatching i =
            if WorkspaceItem.isSameRef i ref then
                newItem

            else
                i
    in
    map replaceMatching items


remove : WorkspaceItems -> WorkspaceItemRef -> WorkspaceItems
remove items ref =
    case items of
        Empty ->
            Empty

        WorkspaceItems data ->
            let
                without r =
                    ListE.filterNot (\i -> WorkspaceItem.isSameRef i r)
            in
            if WorkspaceItem.isSameRef data.focus ref then
                let
                    rightBeforeFocus =
                        ListE.last data.before

                    rightAfterFocus =
                        List.head data.after
                in
                case rightAfterFocus of
                    Just i ->
                        WorkspaceItems
                            { before = data.before
                            , focus = i
                            , after = without (WorkspaceItem.reference i) data.after
                            }

                    Nothing ->
                        case rightBeforeFocus of
                            Just i ->
                                WorkspaceItems
                                    { before = without (WorkspaceItem.reference i) data.before
                                    , focus = i
                                    , after = data.after
                                    }

                            Nothing ->
                                Empty

            else
                WorkspaceItems
                    { before = without ref data.before
                    , focus = data.focus
                    , after = without ref data.after
                    }



-- QUERY


includesItem : WorkspaceItems -> WorkspaceItemRef -> Bool
includesItem items ref =
    items
        |> references
        |> List.any (WorkspaceItemRef.same ref)


references : WorkspaceItems -> List WorkspaceItemRef
references items =
    items
        |> toList
        |> List.map WorkspaceItem.reference


definitionReferences : WorkspaceItems -> List Reference
definitionReferences items =
    items
        |> toList
        |> List.map WorkspaceItem.definitionReference
        |> MaybeE.values


fqns : WorkspaceItems -> List FQN
fqns items =
    items
        |> toList
        |> List.concatMap WorkspaceItem.allFqns


head : WorkspaceItems -> Maybe WorkspaceItem
head items =
    items
        |> toList
        |> List.head


last : WorkspaceItems -> Maybe WorkspaceItem
last items =
    items
        |> toList
        |> ListE.last



-- Focus


focus : WorkspaceItems -> Maybe WorkspaceItem
focus items =
    case items of
        Empty ->
            Nothing

        WorkspaceItems data ->
            Just data.focus


focusedReference : WorkspaceItems -> Maybe WorkspaceItemRef
focusedReference items =
    items
        |> focus
        |> Maybe.map WorkspaceItem.reference


focusOn : WorkspaceItems -> WorkspaceItemRef -> WorkspaceItems
focusOn items ref =
    let
        fromSplits ( before, afterInclusive ) =
            case afterInclusive of
                [] ->
                    Nothing

                newFocus :: after ->
                    Just
                        { before = before
                        , focus = newFocus
                        , after = after
                        }
    in
    items
        |> toList
        |> ListE.splitWhen (\i -> WorkspaceItem.isSameRef i ref)
        |> Maybe.andThen fromSplits
        |> Maybe.map WorkspaceItems
        |> Maybe.withDefault items


isFocused : WorkspaceItems -> WorkspaceItemRef -> Bool
isFocused workspaceItems ref =
    workspaceItems
        |> focus
        |> Maybe.map (\i -> WorkspaceItem.isSameRef i ref)
        |> Maybe.withDefault False


focusIndex : WorkspaceItems -> Maybe Int
focusIndex workspaceItems =
    case workspaceItems of
        Empty ->
            Nothing

        WorkspaceItems items ->
            items.before
                |> List.length
                |> Maybe.Just


{-| Moves the focused item up before the previous item, keeps focus |
-}
moveUp : WorkspaceItems -> WorkspaceItems
moveUp items =
    case items of
        Empty ->
            Empty

        WorkspaceItems data ->
            case ListE.unconsLast data.before of
                Nothing ->
                    items

                Just ( i, before ) ->
                    WorkspaceItems
                        { before = before
                        , focus = data.focus
                        , after = i :: data.after
                        }


{-| Moves the focused item down before the next item, keeps focus |
-}
moveDown : WorkspaceItems -> WorkspaceItems
moveDown items =
    case items of
        Empty ->
            Empty

        WorkspaceItems data ->
            case data.after of
                [] ->
                    items

                i :: after ->
                    WorkspaceItems
                        { before = data.before ++ [ i ]
                        , focus = data.focus
                        , after = after
                        }


next : WorkspaceItems -> WorkspaceItems
next items =
    case items of
        Empty ->
            Empty

        WorkspaceItems data ->
            case data.after of
                [] ->
                    items

                newFocus :: rest ->
                    WorkspaceItems
                        { before = data.before ++ [ data.focus ]
                        , focus = newFocus
                        , after = rest
                        }


prev : WorkspaceItems -> WorkspaceItems
prev items =
    case items of
        Empty ->
            Empty

        WorkspaceItems data ->
            case ListE.unconsLast data.before of
                Nothing ->
                    items

                Just ( newFocus, newBefore ) ->
                    WorkspaceItems
                        { before = newBefore
                        , focus = newFocus
                        , after = data.focus :: data.after
                        }



-- TRANFORM


updateDefinitionItemState :
    (DefinitionWorkspaceItemState -> DefinitionWorkspaceItemState)
    -> WorkspaceItemRef
    -> WorkspaceItems
    -> WorkspaceItems
updateDefinitionItemState f ref wItems =
    let
        update_ workspaceItem =
            case workspaceItem of
                WorkspaceItem.Success r (WorkspaceItem.DefinitionWorkspaceItem defRef state innerItem) ->
                    if ref == r then
                        WorkspaceItem.Success
                            r
                            (WorkspaceItem.DefinitionWorkspaceItem defRef (f state) innerItem)

                    else
                        workspaceItem

                _ ->
                    workspaceItem
    in
    map update_ wItems


updateDependentsItemState :
    (DefinitionMatchesState -> DefinitionMatchesState)
    -> WorkspaceItemRef
    -> WorkspaceItems
    -> WorkspaceItems
updateDependentsItemState f ref wItems =
    let
        update_ workspaceItem =
            case workspaceItem of
                WorkspaceItem.Success r (WorkspaceItem.DependentsWorkspaceItem dependentsOfRef state defItem dependents) ->
                    if ref == r then
                        WorkspaceItem.Success
                            r
                            (WorkspaceItem.DependentsWorkspaceItem dependentsOfRef (f state) defItem dependents)

                    else
                        workspaceItem

                _ ->
                    workspaceItem
    in
    map update_ wItems


updateDependenciesItemState :
    (DefinitionMatchesState -> DefinitionMatchesState)
    -> WorkspaceItemRef
    -> WorkspaceItems
    -> WorkspaceItems
updateDependenciesItemState f ref wItems =
    let
        update_ workspaceItem =
            case workspaceItem of
                WorkspaceItem.Success r (WorkspaceItem.DependenciesWorkspaceItem dependenciesOfRef state defItem dependencies) ->
                    if ref == r then
                        WorkspaceItem.Success
                            r
                            (WorkspaceItem.DependenciesWorkspaceItem dependenciesOfRef (f state) defItem dependencies)

                    else
                        workspaceItem

                _ ->
                    workspaceItem
    in
    map update_ wItems


map :
    (WorkspaceItem -> WorkspaceItem)
    -> WorkspaceItems
    -> WorkspaceItems
map f wItems =
    case wItems of
        Empty ->
            Empty

        WorkspaceItems data ->
            WorkspaceItems
                { before = List.map f data.before
                , focus = f data.focus
                , after = List.map f data.after
                }


find : (WorkspaceItem -> Bool) -> WorkspaceItems -> Maybe WorkspaceItem
find pred wItems =
    wItems
        |> toList
        |> ListE.find pred


all : (WorkspaceItem -> Bool) -> WorkspaceItems -> Bool
all pred wItems =
    wItems
        |> toList
        |> List.all pred


any : (WorkspaceItem -> Bool) -> WorkspaceItems -> Bool
any pred wItems =
    wItems
        |> toList
        |> List.any pred


mapToList : (WorkspaceItem -> Bool -> a) -> WorkspaceItems -> List a
mapToList f wItems =
    case wItems of
        Empty ->
            []

        WorkspaceItems data ->
            let
                before =
                    data.before
                        |> List.map (\i -> f i False)

                after =
                    data.after
                        |> List.map (\i -> f i False)
            in
            before ++ (f data.focus True :: after)


{-| Converting the workspace items to a list, looses the focus indicator
-}
toList : WorkspaceItems -> List WorkspaceItem
toList wItems =
    case wItems of
        Empty ->
            []

        WorkspaceItems items ->
            items.before ++ (items.focus :: items.after)
