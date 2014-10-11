GraphPoints[Topology[Propagator[External][Vertex[1][1], Vertex[3][1]], 
    Propagator[Loop[1]][Vertex[3][1], Vertex[4][-1]], 
    Propagator[Loop[1]][Vertex[3][1], Vertex[4][-1]], 
    Propagator[Loop[2]][Vertex[4][-1], Vertex[4][-1]]]] = 
  {{{PropagatorEndpoints -> {None, Vertex}, 
     PropagatorLabelPosition -> ComposedChar[{{10.8, 6.5}}]}, 
    {PropagatorEndpoints -> Vertex, PropagatorHeight -> 2.5, 
     PropagatorLabelPosition -> ComposedChar[{{6.5, 11.5}}]}, 
    {PropagatorEndpoints -> Vertex, PropagatorHeight -> -2.5, 
     PropagatorLabelPosition -> ComposedChar[{{13.3, 11.5}}]}, 
    {PropagatorEndpoints -> Vertex, PropagatorHeight -> 3, 
     PropagatorLabelPosition -> ComposedChar[{{10, 17.8}}]}}, 
   {Vertex[1][1] -> {10, 4}, Vertex[3][1] -> {10, 9}, 
    Vertex[4][-1] -> {10, 14}}, {}}
 
GraphPoints[Topology[Propagator[External][Vertex[1][1], Vertex[4][1]], 
    Propagator[Internal][Vertex[3][-1], Vertex[4][1]], 
    Propagator[Loop[1]][Vertex[3][-1], Vertex[3][-1]], 
    Propagator[Loop[2]][Vertex[4][1], Vertex[4][1]]]] = 
  {{{PropagatorEndpoints -> {None, Vertex}}, 
    {PropagatorEndpoints -> Vertex}, 
    {PropagatorEndpoints -> Vertex, PropagatorHeight -> 4}, 
    {PropagatorEndpoints -> Vertex, Propagator3rdPoint -> {14, 8}}}, 
   {Vertex[1][1] -> {10, 4}, Vertex[4][1] -> {10, 8}, 
    Vertex[3][-1] -> {10, 12}}, {}}
 
GraphPoints[Topology[Propagator[External][Vertex[1][1], Vertex[3][1]], 
    Propagator[Internal][Vertex[3][-2], Vertex[3][-1]], 
    Propagator[Loop[1]][Vertex[3][-1], Vertex[3][1]], 
    Propagator[Loop[1]][Vertex[3][-1], Vertex[3][1]], 
    Propagator[Loop[2]][Vertex[3][-2], Vertex[3][-2]]]] = 
  {{{PropagatorEndpoints -> {None, Vertex}}, 
    {PropagatorEndpoints -> Vertex}, {PropagatorEndpoints -> Vertex}, 
    {PropagatorEndpoints -> Vertex}, 
    {PropagatorEndpoints -> Vertex, PropagatorHeight -> 4}}, 
   {Vertex[1][1] -> {10, 3}, Vertex[3][1] -> {10, 7}, 
    Vertex[3][-1] -> {10, 11}, Vertex[3][-2] -> {10, 15}}, {}}
 
GraphPoints[Topology[Propagator[External][Vertex[1][1], Vertex[3][1]], 
    Propagator[Internal][Vertex[3][-2], Vertex[3][1]], 
    Propagator[Internal][Vertex[3][-1], Vertex[3][1]], 
    Propagator[Loop[1]][Vertex[3][-1], Vertex[3][-1]], 
    Propagator[Loop[2]][Vertex[3][-2], Vertex[3][-2]]]] = 
  {{{PropagatorEndpoints -> {None, Vertex}}, 
    {PropagatorEndpoints -> Vertex}, {PropagatorEndpoints -> Vertex}, 
    {PropagatorEndpoints -> Vertex, Propagator3rdPoint -> {17, 10}}, 
    {PropagatorEndpoints -> Vertex, Propagator3rdPoint -> {3, 10}}}, 
   {Vertex[3][1] -> {10, 10}, Vertex[1][1] -> {10, 5}, 
    Vertex[3][-2] -> {7, 10}, Vertex[3][-1] -> {13, 10}}, {}}
 
GraphPoints[_Topology] := $Undefined
 
