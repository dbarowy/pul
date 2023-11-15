// Sourced from: https://stevehorsfield.wordpress.com/2009/07/27/f-a-data-structure-for-modelling-directional-graphs/#comments


open Graph

Graph.empty
  |> Graph.addVertex "Abe" // Returns identifier of vertex 1 and graph
  |> (fun (v1,g) -> 
    (Graph.addVertex "Bob" g)) // Returns identifier of vertex 2 and graph
  |> snd
  |> printf "Parsed graph: %A\n\n"

Graph.empty
  |> Graph.addVertex "Abe" // Returns identifier of vertex 1 and graph
  |> (fun (v1,g) -> 
    (Graph.addVertex "Bob" g) // Returns identifier of vertex 2 and graph
    |> (fun (v2,g) -> 
      (Graph.addEdge 0 v1 v2 100 g))) // Add edge with priority between v1 and v2 and label to graph g
  |> snd
  |> printf "Parsed graph: %A\n\n"

  //Parsed graph: (3, [((1, "vertex2"), []); ((0, "vertex1"), [(2, 0, 1, "edge1")])])