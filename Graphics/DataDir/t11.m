GraphPoints[Topology[Propagator[External][Vertex[1][1], Vertex[3, 1][1]], 
    Propagator[Loop[1]][Vertex[3, 1][1], Vertex[3, 1][1]]]] = 
  {{{PropagatorEndpoints -> {None, CounterTerm}}, 
    {PropagatorEndpoints -> CounterTerm, PropagatorHeight -> 5}}, 
   {Vertex[1][1] -> {10, 5}, Vertex[3, 1][1] -> {10, 10}}, {}}
 
GraphPoints[Topology[Propagator[External][Vertex[1][1], Vertex[3][1]], 
    Propagator[Loop[1]][Vertex[3][1], Vertex[2, 1][1]], 
    Propagator[Loop[1]][Vertex[3][1], Vertex[2, 1][1]]]] = 
  {{{PropagatorEndpoints -> {None, Vertex}}, 
    {PropagatorEndpoints -> {Vertex, CounterTerm}, PropagatorHeight -> 2.5}, 
    {PropagatorEndpoints -> {Vertex, CounterTerm}, PropagatorHeight -> -2.5}}\
    , {Vertex[3][1] -> {10, 10}, Vertex[1][1] -> {10, 5}, 
    Vertex[2, 1][1] -> {10, 15}}, {}}
 
GraphPoints[Topology[Propagator[External][Vertex[1][1], Vertex[4][1]], 
    Propagator[Internal][Vertex[4][1], Vertex[1, 1][1]], 
    Propagator[Loop[1]][Vertex[4][1], Vertex[4][1]]]] = 
  {{{PropagatorEndpoints -> {None, Vertex}}, 
    {PropagatorEndpoints -> {Vertex, CounterTerm}}, 
    {PropagatorEndpoints -> Vertex, Propagator3rdPoint -> {6, 10}}}, 
   {Vertex[1][1] -> {10, 5}, Vertex[4][1] -> {10, 10}, 
    Vertex[1, 1][1] -> {10, 15}}, {}}
 
GraphPoints[Topology[Propagator[External][Vertex[1][1], Vertex[2, 1][1]], 
    Propagator[Internal][Vertex[3][-1], Vertex[2, 1][1]], 
    Propagator[Loop[1]][Vertex[3][-1], Vertex[3][-1]]]] = 
  {{{PropagatorEndpoints -> {None, CounterTerm}}, 
    {PropagatorEndpoints -> {Vertex, CounterTerm}}, 
    {PropagatorEndpoints -> Vertex, PropagatorHeight -> 4}}, 
   {Vertex[1][1] -> {10, 5}, Vertex[2, 1][1] -> {10, 10}, 
    Vertex[3][-1] -> {10, 14}}, {}}
 
GraphPoints[Topology[Propagator[External][Vertex[1][1], Vertex[3][1]], 
    Propagator[Internal][Vertex[3][-1], Vertex[3][1]], 
    Propagator[Internal][Vertex[3][1], Vertex[1, 1][1]], 
    Propagator[Loop[1]][Vertex[3][-1], Vertex[3][-1]]]] = 
  {{{PropagatorEndpoints -> {None, Vertex}}, 
    {PropagatorEndpoints -> Vertex}, 
    {PropagatorEndpoints -> {Vertex, CounterTerm}}, 
    {PropagatorEndpoints -> Vertex, Propagator3rdPoint -> {2, 10}}}, 
   {Vertex[3][1] -> {10, 10}, Vertex[1][1] -> {10, 5}, 
    Vertex[3][-1] -> {6, 10}, Vertex[1, 1][1] -> {14, 10}}, {}}
 
GraphPoints[Topology[Propagator[External][Vertex[1][1], Vertex[3][1]], 
    Propagator[Internal][Vertex[3][-1], Vertex[1, 1][1]], 
    Propagator[Loop[1]][Vertex[3][-1], Vertex[3][1]], 
    Propagator[Loop[1]][Vertex[3][-1], Vertex[3][1]]]] = 
  {{{PropagatorEndpoints -> {None, Vertex}}, 
    {PropagatorEndpoints -> {Vertex, CounterTerm}}, 
    {PropagatorEndpoints -> Vertex}, {PropagatorEndpoints -> Vertex}}, 
   {Vertex[1][1] -> {10, 5}, Vertex[3][1] -> {10, 10}, 
    Vertex[3][-1] -> {10, 14}, Vertex[1, 1][1] -> {10, 18}}, {}}
 
GraphPoints[Topology[Propagator[External][Vertex[1][1], Vertex[3][1]], 
    Propagator[Internal][Vertex[3][1], Vertex[1, 1][1]], 
    Propagator[Loop[1]][Vertex[3][1], Vertex[3][1]], 
    Propagator[Loop[1]][Vertex[3][1], Vertex[3][1]]]] = 
  {{{PropagatorEndpoints -> {None, Vertex}}, 
    {PropagatorEndpoints -> {Vertex, CounterTerm}}, 
    {PropagatorEndpoints -> Vertex}, {PropagatorEndpoints -> Vertex}}, 
   {Vertex[1][1] -> {0, 13}, Vertex[3][1] -> {0, 7}, 
    Vertex[1, 1][1] -> {20, 10}}, {}}
 
GraphPoints[_Topology] := $Undefined
 
CounterTerm[1] := 
  Graphics[{Thickness[Size/3.], 
    Line[{Hold[Point] - 15*Size*Hold[HighEnergyPhysics`Graphics`Private`SL], 
      Hold[Point] + 15*Size*Hold[HighEnergyPhysics`Graphics`Private`SL]}], 
    Line[{Hold[Point] - 15*Size*Hold[HighEnergyPhysics`Graphics`Private`BS], 
      Hold[Point] + 15*Size*Hold[HighEnergyPhysics`Graphics`Private`BS]}]}]
 
CounterTerm[2] := 
  Graphics[{Thickness[Size/3.], 
    Line[{Hold[Point] - 15*Size*Hold[HighEnergyPhysics`Graphics`Private`SL], 
      Hold[Point] + 15*Size*Hold[HighEnergyPhysics`Graphics`Private`SL]}], 
    Line[{Hold[Point] - 15*Size*Hold[HighEnergyPhysics`Graphics`Private`BS], 
      Hold[Point] + 15*Size*Hold[HighEnergyPhysics`Graphics`Private`BS]}], 
    Circle[Point, 15*Size*1.5]}]
 
CounterTerm[3] := 
  Graphics[{Thickness[Size/3.], 
    Line[{Hold[Point] - 15*Size*Hold[HighEnergyPhysics`Graphics`Private`SL], 
      Hold[Point] + 15*Size*Hold[HighEnergyPhysics`Graphics`Private`SL]}], 
    Line[{Hold[Point] - 15*Size*Hold[HighEnergyPhysics`Graphics`Private`BS], 
      Hold[Point] + 15*Size*Hold[HighEnergyPhysics`Graphics`Private`BS]}], 
    Circle[Point, 18*Size*1.8], Circle[Point, 18*Size*3.6]}]
 
Attributes[Hold] = {HoldAll}
 
HighEnergyPhysics`Graphics`Private`SL = {1.8, 1.8}
 
HighEnergyPhysics`Graphics`Private`BS = {-1.8, 1.8}
