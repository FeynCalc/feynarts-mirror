GraphPoints[Topology[Propagator[Loop[1]][Vertex[2, 1][1], 
     Vertex[2, 1][1]]]] = 
  {{{PropagatorEndpoints -> CounterTerm, PropagatorHeight -> 6}}, 
   {Vertex[2, 1][1] -> {10, 7}}, {}}
 
GraphPoints[Topology[Propagator[Loop[1]][Vertex[2, 2][1], 
     Vertex[2, 2][1]]]] = 
  {{{PropagatorEndpoints -> CounterTerm, PropagatorHeight -> 6}}, 
   {Vertex[2, 2][1] -> {10, 7}}, {}}
 
GraphPoints[Topology[Propagator[Internal][Vertex[3][1], Vertex[1, 1][1]], 
    Propagator[Loop[1]][Vertex[3][1], Vertex[3][1]]]] = 
  {{{PropagatorEndpoints -> {Vertex, CounterTerm}}, 
    {PropagatorEndpoints -> Vertex, Propagator3rdPoint -> {10, 4}, 
     PropagatorLabelPosition -> ComposedChar[{{10, 5}}]}}, 
   {Vertex[3][1] -> {10, 10}, Vertex[1, 1][1] -> {10, 15}}, {}}
 
GraphPoints[Topology[Propagator[Internal][Vertex[3][1], Vertex[1, 2][1]], 
    Propagator[Loop[1]][Vertex[3][1], Vertex[3][1]]]] = 
  {{{PropagatorEndpoints -> {Vertex, CounterTerm}}, 
    {PropagatorEndpoints -> Vertex, Propagator3rdPoint -> {10, 4}, 
     PropagatorLabelPosition -> ComposedChar[{{10, 5}}]}}, 
   {Vertex[1, 2][1] -> {10, 15}, Vertex[3][1] -> {10, 10}}, {}}
 
GraphPoints[Topology[Propagator[Internal][Vertex[1, 1][1], Vertex[3, 1][1]], 
    Propagator[Loop[1]][Vertex[3, 1][1], Vertex[3, 1][1]]]] = 
  {{{PropagatorEndpoints -> CounterTerm}, 
    {PropagatorEndpoints -> CounterTerm, Propagator3rdPoint -> {6, 10}}}, 
   {Vertex[3, 1][1] -> {10, 10}, Vertex[1, 1][1] -> {14, 10}}, {}}
 
GraphPoints[Topology[Propagator[Loop[1]][Vertex[2, 1][1], Vertex[2, 1][2]], 
    Propagator[Loop[1]][Vertex[2, 1][1], Vertex[2, 1][2]]]] = 
  {{{PropagatorEndpoints -> CounterTerm, PropagatorHeight -> 3}, 
    {PropagatorEndpoints -> CounterTerm, PropagatorHeight -> -3}}, 
   {Vertex[2, 1][1] -> {10, 7}, Vertex[2, 1][2] -> {10, 13}}, {}}
 
GraphPoints[Topology[Propagator[Internal][Vertex[3][1], Vertex[1, 1][1]], 
    Propagator[Loop[1]][Vertex[3][1], Vertex[2, 1][1]], 
    Propagator[Loop[1]][Vertex[3][1], Vertex[2, 1][1]]]] = 
  {{{PropagatorEndpoints -> {Vertex, CounterTerm}}, 
    {PropagatorEndpoints -> {Vertex, CounterTerm}, PropagatorHeight -> -3}, 
    {PropagatorEndpoints -> {Vertex, CounterTerm}, PropagatorHeight -> 3}}, 
   {Vertex[3][1] -> {10, 10}, Vertex[1, 1][1] -> {10, 15}, 
    Vertex[2, 1][1] -> {10, 4}}, {}}
 
GraphPoints[Topology[Propagator[Internal][Vertex[3][1], Vertex[2, 1][1]], 
    Propagator[Internal][Vertex[1, 1][1], Vertex[2, 1][1]], 
    Propagator[Loop[1]][Vertex[3][1], Vertex[3][1]]]] = 
  {{{PropagatorEndpoints -> {Vertex, CounterTerm}}, 
    {PropagatorEndpoints -> CounterTerm}, 
    {PropagatorEndpoints -> Vertex, Propagator3rdPoint -> {10, 4}, 
     PropagatorLabelPosition -> ComposedChar[{{10, 5}}]}}, 
   {Vertex[3][1] -> {10, 10}, Vertex[2, 1][1] -> {10, 14}, 
    Vertex[1, 1][1] -> {10, 18}}, {}}
 
GraphPoints[Topology[Propagator[Internal][Vertex[3][1], Vertex[3, 1][1]], 
    Propagator[Loop[1]][Vertex[3][1], Vertex[3][1]], 
    Propagator[Loop[1]][Vertex[3, 1][1], Vertex[3, 1][1]]]] = 
  {{{PropagatorEndpoints -> {Vertex, CounterTerm}}, 
    {PropagatorEndpoints -> Vertex, Propagator3rdPoint -> {4, 10}}, 
    {PropagatorEndpoints -> CounterTerm, Propagator3rdPoint -> {16, 10}}}, 
   {Vertex[3, 1][1] -> {12, 10}, Vertex[3][1] -> {8, 10}}, {}}
 
GraphPoints[Topology[Propagator[Internal][Vertex[4][-1], Vertex[1, 1][1]], 
    Propagator[Internal][Vertex[4][-1], Vertex[1, 1][2]], 
    Propagator[Loop[1]][Vertex[4][-1], Vertex[4][-1]]]] = 
  {{{PropagatorEndpoints -> {Vertex, CounterTerm}}, 
    {PropagatorEndpoints -> {Vertex, CounterTerm}}, 
    {PropagatorEndpoints -> Vertex, PropagatorHeight -> 4}}, 
   {Vertex[1, 1][1] -> {14, 10}, Vertex[1, 1][2] -> {6, 10}, 
    Vertex[4][-1] -> {10, 10}}, {}}
 
GraphPoints[Topology[Propagator[Internal][Vertex[3][1], Vertex[3][2]], 
    Propagator[Internal][Vertex[3][2], Vertex[1, 1][1]], 
    Propagator[Internal][Vertex[3][2], Vertex[1, 1][2]], 
    Propagator[Loop[1]][Vertex[3][1], Vertex[3][1]]]] = 
  {{{PropagatorEndpoints -> Vertex}, 
    {PropagatorEndpoints -> {Vertex, CounterTerm}}, 
    {PropagatorEndpoints -> {Vertex, CounterTerm}}, 
    {PropagatorEndpoints -> Vertex, Propagator3rdPoint -> {10, 4}}}, 
   {Vertex[3][1] -> {10, 10}, Vertex[1, 1][1] -> {6, 14}, 
    Vertex[1, 1][2] -> {14, 14}, Vertex[3][2] -> {10, 14}}, {}}
 
GraphPoints[_Topology] := $Undefined
 
