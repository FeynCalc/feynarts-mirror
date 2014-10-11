GraphPoints[Topology[Propagator[Loop[1]][Vertex[4, 1][1], Vertex[4, 1][1]], 
    Propagator[Loop[2]][Vertex[4, 1][1], Vertex[4, 1][1]]]] = 
  {{{PropagatorEndpoints -> CounterTerm, Propagator3rdPoint -> {5, 10}}, 
    {PropagatorEndpoints -> CounterTerm, Propagator3rdPoint -> {15, 10}}}, 
   {Vertex[4, 1][1] -> {10, 10}}, {}}
 
GraphPoints[Topology[Propagator[Internal][Vertex[3][-2], Vertex[3][-1]], 
    Propagator[Loop[1]][Vertex[3][-1], Vertex[3][-1]], 
    Propagator[Loop[2]][Vertex[3][-2], Vertex[3][-2]]]] = 
  {{{PropagatorEndpoints -> Vertex}, 
    {PropagatorEndpoints -> Vertex, Propagator3rdPoint -> {16, 10}}, 
    {PropagatorEndpoints -> Vertex, Propagator3rdPoint -> {4, 10}}}, 
   {Vertex[3][-2] -> {8, 10}, Vertex[3][-1] -> {12, 10}}, {}}
 
GraphPoints[Topology[Propagator[Internal][Vertex[5][1], Vertex[1, 1][1]], 
    Propagator[Loop[1]][Vertex[5][1], Vertex[5][1]], 
    Propagator[Loop[2]][Vertex[5][1], Vertex[5][1]]]] = 
  {{{PropagatorEndpoints -> {Vertex, CounterTerm}}, 
    {PropagatorEndpoints -> Vertex, Propagator3rdPoint -> {6, 10}}, 
    {PropagatorEndpoints -> Vertex, Propagator3rdPoint -> {14, 10}}}, 
   {Vertex[5][1] -> {10, 10}, Vertex[1, 1][1] -> {10, 14}}, {}}
 
GraphPoints[Topology[Propagator[Internal][Vertex[3][-2], Vertex[1, 1][2]], 
    Propagator[Internal][Vertex[3][-1], Vertex[1, 1][1]], 
    Propagator[Loop[1]][Vertex[3][-2], Vertex[3][-1]], 
    Propagator[Loop[2]][Vertex[3][-2], Vertex[3][-1]]]] = 
  {{{PropagatorEndpoints -> {Vertex, CounterTerm}}, 
    {PropagatorEndpoints -> {Vertex, CounterTerm}}, 
    {PropagatorEndpoints -> Vertex}, {PropagatorEndpoints -> Vertex}}, 
   {Vertex[1, 1][2] -> {4, 10}, Vertex[1, 1][1] -> {16, 10}, 
    Vertex[3][-2] -> {8, 10}, Vertex[3][-1] -> {12, 10}}, {}}
 
GraphPoints[Topology[Propagator[Internal][Vertex[3][-2], Vertex[2, 1][1]], 
    Propagator[Internal][Vertex[3][-1], Vertex[2, 1][1]], 
    Propagator[Loop[1]][Vertex[3][-1], Vertex[3][-1]], 
    Propagator[Loop[2]][Vertex[3][-2], Vertex[3][-2]]]] = 
  {{{PropagatorEndpoints -> {Vertex, CounterTerm}}, 
    {PropagatorEndpoints -> {Vertex, CounterTerm}}, 
    {PropagatorEndpoints -> Vertex, Propagator3rdPoint -> {18, 10}}, 
    {PropagatorEndpoints -> Vertex, Propagator3rdPoint -> {2, 10}}}, 
   {Vertex[3][-2] -> {6, 10}, Vertex[3][-1] -> {14, 10}, 
    Vertex[2, 1][1] -> {10, 10}}, {}}
 
GraphPoints[Topology[Propagator[Internal][Vertex[4][1], Vertex[4][1]], 
    Propagator[Internal][Vertex[4][1], Vertex[1, 1][1]], 
    Propagator[Loop[1]][Vertex[3][1], Vertex[3][1]], 
    Propagator[Loop[2]][Vertex[3][1], Vertex[4][1]]]] = 
  {{{PropagatorEndpoints -> Vertex, PropagatorHeight -> 4}, 
    {PropagatorEndpoints -> {Vertex, CounterTerm}}, 
    {PropagatorEndpoints -> Vertex, Propagator3rdPoint -> {4, 10}}, 
    {PropagatorEndpoints -> Vertex}}, 
   {Vertex[3][1] -> {8, 10}, Vertex[4][1] -> {12, 10}, 
    Vertex[1, 1][1] -> {16, 10}}, {}}
 
GraphPoints[Topology[Propagator[Internal][Vertex[3][-3], Vertex[3][-2]], 
    Propagator[Internal][Vertex[3][-3], Vertex[3][-1]], 
    Propagator[Internal][Vertex[3][-3], Vertex[1, 1][1]], 
    Propagator[Loop[1]][Vertex[3][-1], Vertex[3][-1]], 
    Propagator[Loop[2]][Vertex[3][-2], Vertex[3][-2]]]] = 
  {{{PropagatorEndpoints -> Vertex}, {PropagatorEndpoints -> Vertex}, 
    {PropagatorEndpoints -> {Vertex, CounterTerm}}, 
    {PropagatorEndpoints -> Vertex, Propagator3rdPoint -> {2, 10}}, 
    {PropagatorEndpoints -> Vertex, Propagator3rdPoint -> {18, 10}}}, 
   {Vertex[3][-2] -> {14, 10}, Vertex[3][-3] -> {10, 10}, 
    Vertex[1, 1][1] -> {10, 14}, Vertex[3][-1] -> {6, 10}}, {}}
 
GraphPoints[_Topology] := $Undefined
