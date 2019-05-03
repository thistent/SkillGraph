module Main exposing (main)

{-|


# The Elm Architecture

In Elm, there's a standard way of structuring an application.

@docs Model, Msg, init, update, subs, view, main

-}

--import Tuple

import Array exposing (Array)
import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Color
import Curve
import Element as El
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Force exposing (State)
import Graph exposing (Graph)
import Html as H
import Html.Attributes as HA
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import Json.Decode as Decode
import Keyboard as Kb
import Path
import Shape
import SubPath exposing (SubPath)
import Task
import Time
import TypedSvg as TS
import TypedSvg.Attributes as TSA
import TypedSvg.Attributes.InPx as TSP
import TypedSvg.Core as TSC exposing (Attribute, Svg)
import TypedSvg.Types as TST exposing (Fill(..), Transform(..))


-- Model --


{-| This data structure represents all the possible states of the application by
combining the different types required into a record.
-}
type alias Model =
    { pressedKeys : List Kb.Key
    , count : Int
    , mousePos : { x : Float, y : Float }
    , drag : Maybe Drag
    , graph : Graph NodeId Entity ()
    , simulation : Force.State NodeId
    , svgPos : ( Float, Float )
    , graphChoice : GraphChoice
    }


type GraphChoice
    = StickMan
    | Cube


{-| This represents all the possible types of messages that need to be handled
in the [`update`](#update) function for transforming the [`Model`](#Model).
-}
type Msg
    = KeyMsg Kb.Msg
    | DragStart NodeId ( Float, Float )
    | DragAt ( Float, Float )
    | DragEnd ( Float, Float )
    | Tick Time.Posix
    | ViewPos (Maybe Dom.Element)
    | ChooseGraph GraphChoice


{-| Just takes in Unit (the empty tuple) which could later contain any flags
passed to the Elm application at startup, and returns an initial
[`Model`](#Model) and any command messages for actions that might be required to
get things going.
-}
init : GraphChoice -> () -> ( Model, Cmd Msg )
init graphChoice flags =
    let
        graph : Graph NodeId Entity ()
        graph =
            choiceToGraph graphChoice
                |> Graph.map
                    (\nodeId _ ->
                        dummyNodeValue ("Node: " ++ String.fromInt nodeId)
                            |> emptyEntity nodeId
                            |> Just
                    )

        {- This is to add custom force data between nodes later. -}
        toLinkRec : ( NodeId, NodeId ) -> CustomLink
        toLinkRec ( a, b ) =
            { source = a
            , target = b
            , distance = 150
            , strength = Nothing
            }

        {- The forces that act on nodes and links. -}
        forces : List (Force.Force Int)
        forces =
            [ Graph.edges graph
                |> List.map toLinkRec
                |> Force.customLinks 1
            , Graph.nodes graph
                |> List.map Tuple.first
                |> Force.manyBodyStrength -600
            , Force.center (svgSize.width / 2) (svgSize.height / 2)
            ]
    in
    ( Model []
        0
        { x = 0, y = 0 }
        Nothing
        graph
        (Force.iterations 600 <| Force.simulation forces)
        ( 0, 0 )
        graphChoice
    , Task.attempt
        (\task ->
            case task of
                Ok value ->
                    ViewPos (Just value)

                Err _ ->
                    ViewPos Nothing
        )
      <|
        Dom.getElement "svgWindow"
    )



-- Update --


{-| This is where transformations in the [`Model`](#Model) are defined taking in
the [`Msg`](#Msg) that triggered an update and the previous state to return the
new state with any additional actions that need to be preformed by the runtime
environment.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        tupleToRec : ( Float, Float ) -> { x : Float, y : Float }
        tupleToRec ( x, y ) =
            { x = x, y = y }
    in
    case msg of
        KeyMsg keyMsg ->
            ( { model | pressedKeys = Kb.update keyMsg model.pressedKeys }
            , Cmd.none
            )

        DragStart index xy ->
            ( { model
                | mousePos = tupleToRec xy
                , drag = Just <| Drag xy xy index
                , graph = model.graph
                , simulation = model.simulation
              }
            , Cmd.none
            )

        DragAt xy ->
            case model.drag of
                Just { start, index } ->
                    ( { model
                        | mousePos = tupleToRec xy
                        , drag = Just (Drag start xy index)
                        , graph =
                            Graph.update
                                index
                                (Maybe.map <| updateNode xy)
                                model.graph
                        , simulation = Force.reheat model.simulation
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model
                        | drag = Nothing
                        , graph = model.graph
                        , simulation = model.simulation
                      }
                    , Cmd.none
                    )

        DragEnd xy ->
            case model.drag of
                Just { start, index } ->
                    ( { model
                        | mousePos = tupleToRec xy
                        , drag = Nothing
                        , graph =
                            Graph.update
                                index
                                (Maybe.map (updateNode xy))
                                model.graph
                        , simulation = model.simulation
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model
                        | drag = Nothing
                        , graph = model.graph
                        , simulation = model.simulation
                      }
                    , Cmd.none
                    )

        Tick _ ->
            let
                ( newState, list ) =
                    Graph.nodes model.graph
                        |> List.map
                            Tuple.second
                        |> List.filterMap (Maybe.map identity)
                        |> Force.tick model.simulation
            in
            case model.drag of
                Nothing ->
                    ( { model
                        | count = model.count + 1
                        , drag = model.drag
                        , graph = updateGraphWithList model.graph list
                        , simulation = newState
                      }
                    , Cmd.none
                    )

                Just { current, index } ->
                    ( { model
                        | count = model.count + 1
                        , drag = model.drag
                        , graph =
                            Graph.update index
                                (Maybe.map (updateNode current))
                                (updateGraphWithList model.graph list)
                        , simulation = newState
                      }
                    , Cmd.none
                    )

        ViewPos svgDomElement ->
            let
                newPos =
                    case svgDomElement of
                        Just elem ->
                            elem |> .element |> (\{ x, y } -> ( x, y ))

                        Nothing ->
                            ( 0, 0 )
            in
            ( { model | svgPos = newPos }, Cmd.none )

        ChooseGraph graphChoice ->
            init graphChoice ()



-- Subscriptions --


{-| This is where you subscribe to changes in the environment based on the
current state of the [`Model`](#Model), like user input.

TODO: Maybe update svgPos on window resizes.

-}
subs : Model -> Sub Msg
subs model =
    case model.drag of
        Nothing ->
            if Force.isCompleted model.simulation then
                Sub.none
            else
                Events.onAnimationFrame Tick

        Just _ ->
            Sub.batch
                [ Sub.map KeyMsg Kb.subscriptions
                , Events.onMouseMove
                    (Decode.map (.offsetPos >> DragAt) Mouse.eventDecoder)
                , Events.onMouseUp
                    (Decode.map (.offsetPos >> DragEnd) Mouse.eventDecoder)
                , Events.onAnimationFrame Tick
                ]



-- View --


{-| This Just takes in the [`Model`](#Model) and returns Html/Svg that is
associated with the current state.

It's an Html Msg, because any buttons or forms on the page have the
ability to return an update [`Msg`](#Msg) when interacted with.

-}
view : Model -> H.Html Msg
view model =
    El.layout
        [ El.width El.fill
        , El.height El.fill
        , Font.family [ Font.monospace ]
        , Font.color black
        , Bg.color white
        ]
    <|
        El.column
            [ El.width El.fill
            , El.spacing 10
            ]
            [ El.el
                [ Font.size 30
                , Font.center
                , Font.color white
                , Bg.color purple
                , El.width El.fill
                , El.padding 20
                , Border.shadow
                    { offset = ( 0, 0 )
                    , size = 4
                    , blur = 8
                    , color = El.rgba 0 0 0 0.5
                    }
                ]
              <|
                El.text "Skill Graph"
            , El.el
                [ El.centerX
                , El.centerY
                , El.width El.fill
                , El.padding 20
                , Font.color black
                , Bg.color white
                ]
              <|
                El.text
                    ("Time ticks : "
                        ++ String.fromInt model.count
                        ++ " | "
                        ++ "Last drag position : ("
                        ++ String.fromFloat model.mousePos.x
                        ++ ", "
                        ++ String.fromFloat model.mousePos.y
                        ++ ")"
                    )
            , El.row [ El.width El.fill ]
                [ El.el
                    [ El.width <| El.px <| round <| svgSize.width
                    , El.height <| El.px <| round <| svgSize.height
                    , El.centerX
                    , Bg.color <| El.rgb 0.9 0.9 0.9
                    , Font.color black
                    , Border.shadow
                        { offset = ( 0, 0 )
                        , size = 2
                        , blur = 8
                        , color = El.rgba 0 0 0 0.25
                        }
                    ]
                    (El.html <|
                        TS.svg
                            [ TSA.viewBox 0 0 svgSize.width svgSize.height
                            , TSC.attribute "id" "svgWindow"
                            ]
                            [ Graph.edges model.graph
                                |> List.map (linkElement model.graph)
                                |> TS.g [ TSA.class [ "links" ] ]
                            , Graph.nodes model.graph
                                |> List.map (nodeElement model.svgPos)
                                |> TS.g [ TSA.class [ "nodes" ] ]
                            ]
                    )
                ]

            {- , El.paragraph [ El.padding 20, El.width El.fill ]
               [ El.text <|
                   Debug.toString model.svgPos
               ]
            -}
            , El.row
                [ El.spacing 20
                , El.padding 10
                , El.width <| El.px <| round svgSize.width
                , El.centerX
                ]
                [ Input.button
                    [ El.padding 10
                    , Bg.color <| El.rgb 0.8 0.8 0.9
                    , Font.color black
                    , Border.width 2
                    , Border.color darkBlue
                    , Border.shadow
                        { offset = ( 0, 0 )
                        , size = 2
                        , blur = 8
                        , color = El.rgba 0 0 0 0.25
                        }
                    ]
                    { onPress = Just <| ChooseGraph StickMan
                    , label =
                        El.el [ El.paddingXY 20 10 ]
                            (El.text "Stick Man")
                    }
                , Input.button
                    [ El.padding 10
                    , Bg.color <| El.rgb 0.8 0.8 0.9
                    , Font.color black
                    , Border.width 2
                    , Border.color darkBlue
                    , Border.shadow
                        { offset = ( 0, 0 )
                        , size = 2
                        , blur = 8
                        , color = El.rgba 0 0 0 0.25
                        }
                    ]
                    { onPress = Just <| ChooseGraph Cube
                    , label =
                        El.el [ El.paddingXY 20 10 ]
                            (El.text "Cube")
                    }
                ]
            ]



-- Main --


{-| This is where everything is tied together into an Elm application.

A Browser.element is useful when you want to embed your Elm app into another
application and respond to changes in the environment.

-}
main : Program () Model Msg
main =
    Browser.element
        { init = init Cube --StickMan
        , view = view
        , update = update
        , subscriptions = subs
        }



-- Some Default Definitions --


{-| The dimensions of the Svg window
-}
svgSize : { width : Float, height : Float }
svgSize =
    { width = 700, height = 700 }


{-| The default inner radius for the internal pie chart in each node.
-}
defaultInnerRad : Float
defaultInnerRad =
    20


{-| Sample data for the outer ring of a node.
-}
bigPie : List Float
bigPie =
    [ 8, 2, 2, 6, 6 ]


{-| Sample data for the inner ring of a node.
-}
smallPie : List Float
smallPie =
    [ 3, 6, 4, 5, 4 ]


{-| Signifies a custom force link between two nodes in the graph.
-}
type alias CustomLink =
    { source : NodeId
    , target : NodeId
    , distance : Float
    , strength : Maybe Float
    }


{-| Describes a drag state for a node when moving it.
-}
type alias Drag =
    { start : ( Float, Float )
    , current : ( Float, Float )
    , index : NodeId
    }


{-| Describes what internal information is contained in a node.

TODO: Consider what should be here, and maybe add internal information into
edges as well.

-}
type alias NodeValue =
    { name : String
    , innerData : List Float
    , outerData : List Float
    }


{-| A Force.Entity that contains a [`NodeValue`](#NodeValue) as its internal
data.
-}
type alias Entity =
    Force.Entity NodeId { value : NodeValue }


{-| Generates a new [`Entity`](#Entity).
-}
emptyEntity : NodeId -> NodeValue -> Entity
emptyEntity nodeId nodeVal =
    Force.entity nodeId nodeVal


{-| Creates a named sample NodeValue with sample data.
-}
dummyNodeValue : String -> NodeValue
dummyNodeValue name =
    NodeValue name smallPie bigPie


{-| NodeIds are just Integers.
-}
type alias NodeId =
    Int


{-| Updates an [`Entity`](#Entity)'s x and y coordinates.
-}
updateNode : ( Float, Float ) -> Entity -> Entity
updateNode ( x, y ) nodeData =
    { nodeData | x = x, y = y }


{-| Takes a Graph state and a list of changed [`Entity`](#Entity)s
and inserts changes into an output Graph.
-}
updateGraphWithList :
    Graph NodeId Entity ()
    -> List Entity
    -> Graph NodeId Entity ()
updateGraphWithList =
    List.foldr (\node graph -> Graph.insertData node.id node graph)


{-| Generates a DragStart message win the mouse is clicked on a node.
-}
onMouseDown : NodeId -> Attribute Msg
onMouseDown nodeId =
    Mouse.onDown (.offsetPos >> DragStart nodeId)


{-| Generates coordinates from a Touch.Event.
-}
touchCoordinates : ( Float, Float ) -> Touch.Event -> ( Float, Float )
touchCoordinates ( origX, origY ) touchEvent =
    List.head touchEvent.changedTouches
        |> Maybe.map .pagePos
        -- .screenPos
        |> Maybe.withDefault ( 0, 0 )
        |> (\( x, y ) -> ( x - origX, y - origY ))


{-| Default configuration for a pie chart.
-}
pieConfig : Shape.PieConfig Float
pieConfig =
    let
        pie =
            Shape.defaultPieConfig
    in
    { pie
        | innerRadius = defaultInnerRad
        , outerRadius = defaultInnerRad + 5
        , cornerRadius = 2
        , padAngle = 0.09
        , sortingFn = \_ _ -> GT
        , startAngle = radians <| 0
        , endAngle = radians <| 2 * pi
    }


{-| The array of colors that are chosen when the charts and edges are generated.
-}
colors : Array Color.Color
colors =
    [ pink
    , lightBlue
    , orange
    , green
    , darkBlue
    ]
        |> Array.fromList
        |> Array.map toSvgColor


{-| Creates a ring chart to be used in a node.
-}
annular : Entity -> List Shape.Arc -> Svg Msg
annular node arcs =
    let
        makeSlice : Int -> Shape.Arc -> Svg Msg
        makeSlice index datum =
            Path.element (Shape.arc datum)
                [ Array.get index colors
                    |> Maybe.withDefault Color.black
                    |> Fill
                    |> TSA.fill
                , TSA.stroke <| Color.rgba 0 0 0 0
                ]
    in
    TS.g [ TSA.transform [ Translate node.x node.y ] ]
        [ TS.g [] <| List.indexedMap makeSlice arcs ]


{-| Generates the Svg element for any given node in the graph.
-}
nodeElement : ( Float, Float ) -> ( NodeId, Maybe Entity ) -> Svg Msg
nodeElement svgPos ( nodeId, maybeEntity ) =
    let
        node : Entity
        node =
            case maybeEntity of
                Just ent ->
                    ent

                Nothing ->
                    emptyEntity nodeId <|
                        dummyNodeValue <|
                            "Node: "
                                ++ String.fromInt nodeId

        smallOuterRad : Float
        smallOuterRad =
            getOuterRad smallPie defaultInnerRad

        bigInnerRad : Float
        bigInnerRad =
            smallOuterRad + 3.5

        bigOuterRad : Float
        bigOuterRad =
            getOuterRad bigPie bigInnerRad

        smallPieData : List Shape.Arc
        smallPieData =
            smallPie
                |> Shape.pie
                    { pieConfig
                        | innerRadius = defaultInnerRad
                        , outerRadius = smallOuterRad
                    }

        bigPieData : List Shape.Arc
        bigPieData =
            bigPie
                |> Shape.pie
                    { pieConfig
                        | innerRadius = bigInnerRad
                        , outerRadius = bigOuterRad
                    }
    in
    TS.g
        [ onMouseDown node.id
        , Touch.onStart (touchCoordinates svgPos >> DragStart node.id)
        , Touch.onMove (touchCoordinates svgPos >> DragAt)
        , Touch.onEnd (touchCoordinates svgPos >> DragEnd)
        ]
        [ annular node smallPieData
        , annular node bigPieData

        -- Below is an invisible circle to make dragging nodes easier.
        , TS.circle
            [ TSP.cx node.x
            , TSP.cy node.y
            , TSP.r <| bigOuterRad
            , Color.rgba 0 0 0 0
                |> Fill
                |> TSA.fill
            ]
            []
        , TS.title
            [ Array.get (nodeId |> modBy (Array.length colors)) colors
                |> Maybe.withDefault Color.black
                |> Fill
                |> TSA.fill
            ]
            [ TSC.text node.value.name ]
        ]


getOuterRad : List Float -> Float -> Float
getOuterRad pie innerRad =
    innerRad + List.foldl (+) 0 pie / 3


{-| The arrow symbol on each edge line.
-}
arrowPath : SubPath
arrowPath =
    SubPath.close <|
        Curve.linear
            [ ( 0, 0 )
            , ( -10, 5 )
            , ( -5, 0 )
            , ( -10, -5 )
            ]


{-| Generates the Svg for each edge in the graph.
-}
linkElement : Graph NodeId Entity () -> ( NodeId, NodeId ) -> Svg Msg
linkElement graph ( sourceId, targetId ) =
    let
        source : Entity
        source =
            let
                maybeData =
                    Graph.getData sourceId graph
            in
            case maybeData of
                Just data ->
                    data

                Nothing ->
                    emptyEntity sourceId <| NodeValue "" [] []

        target : Entity
        target =
            let
                maybeData =
                    Graph.getData targetId graph
            in
            case maybeData of
                Just data ->
                    data

                Nothing ->
                    emptyEntity targetId <| NodeValue "" [] []

        pie : List Shape.Arc
        pie =
            Shape.pie pieConfig <|
                bigPie

        relAng : Float
        relAng =
            atan2 (target.y - source.y) (target.x - source.x)

        genPath : NodeId -> Shape.Arc -> Svg Msg
        genPath index arc =
            let
                ( xCent, yCent ) =
                    Shape.centroid
                        { arc
                            | innerRadius = getOuterRad smallPie defaultInnerRad
                            , outerRadius =
                                getOuterRad bigPie <|
                                    getOuterRad smallPie defaultInnerRad
                                        + 3.5
                        }

                absMid : ( Float, Float )
                absMid =
                    ( (source.x + target.x) / 2 + 1.75 * xCent
                    , (source.y + target.y) / 2 + 1.75 * yCent
                    )

                pathAttribs : List (TSC.Attribute Msg)
                pathAttribs =
                    [ List.drop index bigPie
                        |> List.head
                        |> Maybe.withDefault 0
                        |> (\x -> 10 * x / List.foldl (+) 0 bigPie)
                        |> TSP.strokeWidth
                    , Color.rgba 0 0 0 0
                        |> Fill
                        |> TSA.fill
                    , Array.get index colors
                        |> Maybe.withDefault Color.black
                        |> TSA.stroke
                    ]
            in
            TS.g []
                [ SubPath.element
                    (Shape.catmullRomCurveOpen 1
                        [ ( source.x, source.y )
                        , ( source.x + xCent, source.y + yCent )
                        , absMid
                        , ( target.x + xCent, target.y + yCent )
                        , ( target.x, target.y )
                        ]
                    )
                    pathAttribs
                , SubPath.element
                    (SubPath.rotate relAng <|
                        SubPath.translate absMid <|
                            arrowPath
                    )
                    pathAttribs
                ]
    in
    TS.g [] <|
        List.indexedMap genPath pie



-- Color Scheme --


toSvgColor : El.Color -> Color.Color
toSvgColor =
    -- From elm-ui color to color color.
    El.toRgb >> Color.fromRgba


white : El.Color
white =
    El.rgb 1 1 1


black : El.Color
black =
    El.rgb 0 0 0


orange : El.Color
orange =
    -- Education
    El.rgb255 0xFF 0xA9 0x1F


green : El.Color
green =
    -- Community
    El.rgb255 0x90 0xCC 0x32


lightBlue : El.Color
lightBlue =
    -- Talent
    El.rgb255 0x2A 0xBB 0xF4


darkBlue : El.Color
darkBlue =
    -- Education
    El.rgb255 0x00 0x74 0xBC


pink : El.Color
pink =
    -- Innovation
    El.rgb255 0xF2 0x5A 0xA3


purple : El.Color
purple =
    -- Default Highlight
    El.rgb255 0x52 0x30 0xB5


darkGreen : El.Color
darkGreen =
    -- Validated
    El.rgb255 0x12 0xBC 0x00


red : El.Color
red =
    -- Unvalidated
    El.rgb255 0xBC 0x00 0x12


{-| This is the graph representation of a stick man!
-}
stickManGraph : Graph NodeId () ()
stickManGraph =
    Graph.empty
        |> Graph.insert 0
        |> Graph.insertEdge 0 1
        |> Graph.insertEdge 1 2
        |> Graph.insertEdge 1 3
        |> Graph.insertEdge 1 4
        |> Graph.insertEdge 4 5
        |> Graph.insertEdge 4 6


{-| This is a graph of a cube.
-}
cubeGraph : Graph NodeId () ()
cubeGraph =
    Graph.empty
        |> Graph.insert 0
        |> Graph.insertEdge 0 1
        |> Graph.insertEdge 0 2
        |> Graph.insertEdge 0 4
        |> Graph.insertEdge 1 3
        |> Graph.insertEdge 1 5
        |> Graph.insertEdge 2 3
        |> Graph.insertEdge 2 6
        |> Graph.insertEdge 3 7
        |> Graph.insertEdge 4 5
        |> Graph.insertEdge 4 6
        |> Graph.insertEdge 5 7
        |> Graph.insertEdge 6 7


{-| Takes the GraphChoice type and returns the appropriate graph.
-}
choiceToGraph : GraphChoice -> Graph NodeId () ()
choiceToGraph graphChoice =
    case graphChoice of
        StickMan ->
            stickManGraph

        Cube ->
            cubeGraph



{- Ideas:

   - Maybe if a node in the graph represents a single value, it should just be
     signifield by a dot.
   - In that case the edges should start in the center of the node and rotate
     freely around it.

     -- Skill Graph Types --


     type alias NamedData dataType =
         { name : String
         , data : dataType
         }


     type alias Project =
         NamedData (Graph String (Goal String) Skill)


     {-| -}
     type alias Goal a =
         { description : String
         , have : List a
         , need : List a
         }


     type alias Skill =
         { description : String
         , quality : KnowledgeQuality
         }


     type alias SkillTransition =
         { description : String
         , validation : List Validation
         }


     type KnowledgeQuality
         = Likes
         | Knows
         | WorksWith
         | Teaches
         | LeadsIn


     {-| FIXME: Should "TestedIn SkillTest" be added?
     -}
     type Validation
         = EndorsedBy Entity
         | ContributedTo Project
         | Unvalidated


     type GraphEntity
         = Person String (List Skill)
         | Organization String (List Entity)




          Project
            o
          o | <- Apply Skill / SkillSet
          | o <- Goal Achieved / Skills Improved
          o   <- Improved SkillSet
        Person


          Community -- Growing Connections between People
          Education -- Growing Skill Graph
          Inovation -- Growing Goal Graph
          Talent -- Connecting People to Projects
          Inclusion -- Improving Environment for people of various backgrounds.


          Projects --
          Portfolio
          Skills (time)
          Leadership

         -- TODO: Add Workflows
         Maybe something like social toolchains

-}
