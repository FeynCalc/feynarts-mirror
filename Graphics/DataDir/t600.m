GraphPoints[Topology[Propagator[Loop[1]][Vertex[3][-2], Vertex[3][-1]], 
    Propagator[Loop[2]][Vertex[3][-3], Vertex[3][-2]], 
    Propagator[Loop[3]][Vertex[3][-4], Vertex[3][-3]], 
    Propagator[Loop[4]][Vertex[3][-4], Vertex[3][-1]], 
    Propagator[Loop[5]][Vertex[3][-4], Vertex[3][-1]], 
    Propagator[Loop[6]][Vertex[3][-3], Vertex[3][-2]]]] = 
  {{{PropagatorEndpoints -> Vertex}, 
    {PropagatorEndpoints -> Vertex, PropagatorHeight -> 3}, 
    {PropagatorEndpoints -> Vertex}, 
    {PropagatorEndpoints -> Vertex, PropagatorHeight -> 7}, 
    {PropagatorEndpoints -> Vertex, PropagatorHeight -> -7}, 
    {PropagatorEndpoints -> Vertex, PropagatorHeight -> -3}}, 
   {Vertex[3][-3] -> {7, 10}, Vertex[3][-2] -> {13, 10}, 
    Vertex[3][-4] -> {3, 10}, Vertex[3][-1] -> {17, 10}}, {}}
 
GraphPoints[Topology[Propagator[Loop[1]][Vertex[3][-2], Vertex[3][-1]], 
    Propagator[Loop[2]][Vertex[3][-3], Vertex[3][-2]], 
    Propagator[Loop[3]][Vertex[3][-3], Vertex[3][-1]], 
    Propagator[Loop[4]][Vertex[3][-4], Vertex[3][-3]], 
    Propagator[Loop[5]][Vertex[3][-4], Vertex[3][-2]], 
    Propagator[Loop[6]][Vertex[3][-4], Vertex[3][-1]]]] = 
  {{{PropagatorEndpoints -> Vertex}, {PropagatorEndpoints -> Vertex}, 
    {PropagatorEndpoints -> Vertex}, {PropagatorEndpoints -> Vertex}, 
    {PropagatorEndpoints -> Vertex}, {PropagatorEndpoints -> Vertex}}, 
   {Vertex[3][-4] -> {3, 3}, Vertex[3][-2] -> {17, 3}, 
    Vertex[3][-3] -> {10, 17}, Vertex[3][-1] -> {10, 8}}, {}}
 
GraphPoints[_Topology] := $Undefined
 
