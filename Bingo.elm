module Bingo where

import String exposing (toUpper, repeat, trimRight)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (Address)

import StartApp
import BingoUtils as Utils


title message times =
  message ++ " "
    |> toUpper
    |> repeat times
    |> trimRight
    |> text

-- MODEL
type alias Entry =
  {
    phrase: String,
    points: Int,
    wasSpoken: Bool,
    id: Int
  }

type alias Model =
  {
    entries: List Entry,
    phraseInput: String,
    pointsInput: String,
    nextId: Int
  }

newEntry phrase points id =
  {
    phrase = phrase,
    points = points,
    wasSpoken = False,
    id = id
  }

initialModel = 
  {
    entries = [
      newEntry "The Future Proof" 200 1,
      newEntry "The Future Solution" 600 2,
      newEntry "The Ample Proof" 100 3,
      newEntry "The Fixtures Proof" 400 4,
      newEntry "Never Future into" 500 5,
      newEntry "The Model Drama" 200 6
    ],
    phraseInput = "",
    pointsInput = "",
    nextId = 7

  }

-- Update

type Action = NoOp | Sort | Reset | Delete Int | Mark Int | UpdatePhraseInput String| UpdatePointsInput String | Add

update action model =
  case action of
    NoOp ->
      model
    Reset ->
      { model | entries <- List.sortBy .phrase model.entries }
    Sort ->
      { model | entries <- List.sortBy .points model.entries }

    Delete id ->
      let 
        remainingEntries =
          List.filter (\e -> e.id /= id) model.entries 
      in
        { model | entries <- remainingEntries}

    Mark id ->
      let 
        updateEntry e =
          if e.id == id then { e | wasSpoken <- (not e.wasSpoken)} else e
      in
        { model | entries <- List.map updateEntry model.entries }

    UpdatePhraseInput contents  ->
      {model | phraseInput <- contents }

    UpdatePointsInput contents ->
      { model | pointsInput <- contents }

    Add ->
      let
        entryToAdd =
          newEntry model.phraseInput (Utils.parseInt model.pointsInput) model.nextId
        isInvalidModel =
          String.isEmpty model.phraseInput || String.isEmpty model.pointsInput
      in
        if isInvalidModel
        then model
        else
          { model | phraseInput <- "", pointsInput <- "", entries <- entryToAdd :: model.entries , nextId <- model.nextId + 1}


totalPoints entries = 
  let
    spokenEntries = List.filter .wasSpoken entries
  in
    List.sum (List.map .points spokenEntries)

--VIEW

pageHeader =
  h1 [ ] [ title "bingo!" 3 ]


pageFooter =
  footer [ ]
    [ a [ href "https://pragmaticstudio.com", target "_blank" ]
        [ text "The Pragmatic Studio" ]
    ]


entryItem address entry =
  li 
    [ classList[("highlight", entry.wasSpoken)], onClick address (Mark entry.id)] 
    [ span[class "phrase"][text entry.phrase], 
      span[class "points"][text (toString entry.points)],
      span[class "delete", onClick address (Delete entry.id)][text ""]
    ]


entryForm: Address Action -> Model -> Html
entryForm address model =
  div []
      [
        input [type' "text", id "phrase", 
                placeholder "Phrase", 
                autofocus True, 
                name "Phrase", 
                value model.phraseInput, 
                Utils.onInput address UpdatePhraseInput ][],
        input [type' "number", 
                id "points", 
                placeholder "Points", 
                autofocus True, name "Points", 
                value model.pointsInput,
                Utils.onInput address UpdatePointsInput][],
        button [class "add", onClick address Add][text "Add"],
        h2 [] [text (model.phraseInput ++ " " ++ model.pointsInput)]
      ]


entryList address entries= 
  ul [] (List.map (entryItem address) entries)

sortOption address =
  button [class "sort", onClick address Sort] [text "sort"]

resetOrder address =
  button [class "sort", onClick address Reset] [text "Reset"]

total t =
  li [class "total"][span[class "Label"][text "Total"], span[class "points"][text (toString t) ]]

view address model =
  div [ id "container" ] 
      [ pageHeader, 
        entryForm address model,
        entryList address model.entries,
        total (totalPoints model.entries),
        sortOption address,
        resetOrder address,
        pageFooter 
      ]

-- Wire it all together
main =
  --view (update Sort initialModel)
  StartApp.start { model = initialModel, view = view, update = update }




