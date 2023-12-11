module GraphPrettyPrint

open Graph
open AST

// Get a list of unfilled request IDs
let getUnfilledRequests (sinkID: int) (g: Graph<'a,int>) =
    let edges: Adjacency<int>= getEdges sinkID g
    let rec generateList edgeList =
        match edgeList with
        | [] -> []
        | (id,_,reqID,cap)::xs ->
            if cap = 0 then [reqID] @ (generateList xs) else (generateList xs)
    generateList edges

// Get a list of filled request IDs
let getFilledRequests (sinkID: int) (g: Graph<'a,int>) =
    let edges: Adjacency<int>= getEdges sinkID g
    let rec generateList edgeList =
        match edgeList with
        | [] -> []
        | (id,_,reqID,cap)::xs ->
            if cap = 1 then [reqID] @ (generateList xs) else (generateList xs)
    generateList edges

// Find the offer ID the fills this request ID
let mapRequestIDtoOfferID (g: Graph<'a,int>) (requestID: int) =
    let edges: Adjacency<int> = getEdges requestID g
    let rec getOfferID edgeList =
        match edgeList with
        | [] -> -1
        | (id,_,offerID,cap)::xs ->
            if cap = 1 then offerID else (getOfferID xs)
    getOfferID edges

// Nicely formatted location List string
let rec locationListString (locList: List<string>) =
    match locList with
    | [] -> ""
    | [loc: string] -> loc
    | loc::locs -> loc + ", " + (locationListString locs)

// Nicely formatted timeRange string
let timeRangeString (timeRange: TimeRange) = 
    let startDay: string = timeRange.startTime.ToString("M/d/yyyy")
    let startTime: string = timeRange.startTime.ToString("h tt") + ""
    if timeRange.startTime <> timeRange.endTime then
        let endDay: string = timeRange.endTime.ToString("M/d/yyyy")
        let endTime: string = timeRange.endTime.ToString("h tt") + ""
        $"at {startTime} on {startDay} to {endTime} on {endDay}"
    else
        $"at {startTime} on {startDay}"

// Nicely formatted event string
let eventString (e: Event) =
    match e with
    | Offer(name: string,locList: List<string>, timeRange: TimeRange,_) ->
        $"Offer: {name} to {locationListString locList} {timeRangeString timeRange}"
    | Request(name: string,locList: List<string>, timeRange: TimeRange) ->
        $"Request: {name} to {locationListString locList} {timeRangeString timeRange}"

let prettyPrint (sinkID: int) (g: Graph<Event,int>) =
    // Get an array of unfilled request IDs
    let unfilledReqs: int list = getUnfilledRequests sinkID g

    // Get an array of filled request IDs
    let filledReqs: int list = getFilledRequests sinkID g
    
    // Get an array of offer IDs that fill the request IDs above in order
    let curriedMapRequestIDtoOfferID = mapRequestIDtoOfferID g
    let offersForReqs: int list = List.map (fun x -> curriedMapRequestIDtoOfferID x) filledReqs

    // Build formatted filled requests
    let rec stringBuilderFilled (offerIDs: int list) (requestIDs: int list) (g: Graph<Event,'c>) =
        match offerIDs,requestIDs with 
        | [],_ -> ""
        | _,[] -> ""
        | offerID::os,requestID::rs  ->
            let offer: Event = (getVertex offerID g) |> vertexData
            let request: Event = (getVertex requestID g) |> vertexData
            let newLine: string = $"\t{eventString offer} filled by {eventString request}"
            newLine + "\n" + (stringBuilderFilled os rs g)

    // Build formatted unfilled requests
    let rec stringBuilderUnfilled (requestIDs: int list) (g: Graph<Event,'c>) =
        match requestIDs with 
        | [] -> ""
        | requestID::rs  ->
            let request: Event = (getVertex requestID g) |> vertexData
            let newLine: string = eventString request
            "\t" + newLine + "\n" + (stringBuilderUnfilled rs g)
    
    // Formatted filled requests
    let resultStringFilled: string = stringBuilderFilled filledReqs offersForReqs g

    // Formatted unfilled requests
    let resultStringUnfilled: string = stringBuilderUnfilled unfilledReqs g

    // Piece together the completed output
    "MATCHED REQUESTS:\n" + resultStringFilled + "\nUNMATCHED REQUESTS:\n" + resultStringUnfilled
        