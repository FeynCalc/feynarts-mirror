GraphPoints[Topology[Propagator[Incoming][Vertex[1][1], Vertex[2, 1][2]], 
    Propagator[Outgoing][Vertex[1][2], Vertex[2, 1][2]]]] = 
  {{{PropagatorEndpoints -> {None, CounterTerm}}, 
    {PropagatorEndpoints -> {None, CounterTerm}}}, 
   {Vertex[1][1] -> {0, 10}, Vertex[2, 1][2] -> {10, 10}, 
    Vertex[1][2] -> {20, 10}}, {}}
 
GraphPoints[Topology[Propagator[Incoming][Vertex[1][1], Vertex[2, 2][2]], 
    Propagator[Outgoing][Vertex[1][2], Vertex[2, 2][2]]]] = 
  {{{PropagatorEndpoints -> 
      {None, Graphics[{Thickness[0.3333333333333333*Size], 
         Line[{Hold[Point] - 15*Size*
             Hold[HighEnergyPhysics`Graphics`Private`SL], 
           Hold[Point] + 15*Size*
             Hold[HighEnergyPhysics`Graphics`Private`SL]}], 
         Line[{-15*Size*Hold[HighEnergyPhysics`Graphics`Private`BS] + 
            Hold[Point], 15*Size*
             Hold[HighEnergyPhysics`Graphics`Private`BS] + Hold[Point]}], 
         Circle[Point, 60*Size]}]}}, 
    {PropagatorEndpoints -> 
      {None, Graphics[{Thickness[0.3333333333333333*Size], 
         Line[{Hold[Point] - 15*Size*
             Hold[HighEnergyPhysics`Graphics`Private`SL], 
           Hold[Point] + 15*Size*
             Hold[HighEnergyPhysics`Graphics`Private`SL]}], 
         Line[{-15*Size*Hold[HighEnergyPhysics`Graphics`Private`BS] + 
            Hold[Point], 15*Size*
             Hold[HighEnergyPhysics`Graphics`Private`BS] + Hold[Point]}], 
         Circle[Point, 60*Size]}]}}}, 
   {Vertex[1][1] -> {0, 10}, Vertex[2, 2][2] -> {10, 10}, 
    Vertex[1][2] -> {20, 10}}, {}}
 
GraphPoints[Topology[Propagator[Incoming][Vertex[1][1], Vertex[3][2]], 
    Propagator[Outgoing][Vertex[1][2], Vertex[3][2]], 
    Propagator[Internal][Vertex[3][2], Vertex[1, 1][1]]]] = 
  {{{PropagatorEndpoints -> {None, Vertex}}, 
    {PropagatorEndpoints -> {None, Vertex}}, 
    {PropagatorEndpoints -> {Vertex, CounterTerm}}}, 
   {Vertex[1][1] -> {0, 10}, Vertex[3][2] -> {10, 10}, 
    Vertex[1, 1][1] -> {10, 15}, Vertex[1][2] -> {20, 10}}, {}}
 
GraphPoints[Topology[Propagator[Incoming][Vertex[1][1], Vertex[3][2]], 
    Propagator[Outgoing][Vertex[1][2], Vertex[3][2]], 
    Propagator[Internal][Vertex[3][2], Vertex[1, 2][1]]]] = 
  {{{PropagatorEndpoints -> {None, Vertex}}, 
    {PropagatorEndpoints -> {None, Vertex}}, 
    {PropagatorEndpoints -> 
      {Vertex, Graphics[{Thickness[0.3333333333333333*Size], 
         Line[{Hold[Point] - 15*Size*
             Hold[HighEnergyPhysics`Graphics`Private`SL], 
           Hold[Point] + 15*Size*
             Hold[HighEnergyPhysics`Graphics`Private`SL]}], 
         Line[{-15*Size*Hold[HighEnergyPhysics`Graphics`Private`BS] + 
            Hold[Point], 15*Size*
             Hold[HighEnergyPhysics`Graphics`Private`BS] + Hold[Point]}], 
         Circle[Point, 60*Size]}]}}}, 
   {Vertex[1][1] -> {0, 10}, Vertex[3][2] -> {10, 10}, 
    Vertex[1, 2][1] -> {10, 15}, Vertex[1][2] -> {20, 10}}, {}}
 
GraphPoints[Topology[Propagator[Incoming][Vertex[1][1], Vertex[2, 1][1]], 
    Propagator[Outgoing][Vertex[1][2], Vertex[2, 1][2]], 
    Propagator[Internal][Vertex[2, 1][1], Vertex[2, 1][2]]]] = 
  {{{PropagatorEndpoints -> {None, CounterTerm}}, 
    {PropagatorEndpoints -> {None, CounterTerm}}, 
    {PropagatorEndpoints -> CounterTerm}}, 
   {Vertex[1][1] -> {0, 10}, Vertex[1][2] -> {20, 10}, 
    Vertex[2, 1][1] -> {6, 10}, Vertex[2, 1][2] -> {14, 10}}, {}}
 
GraphPoints[Topology[Propagator[Incoming][Vertex[1][1], Vertex[3, 1][2]], 
    Propagator[Outgoing][Vertex[1][2], Vertex[3, 1][2]], 
    Propagator[Internal][Vertex[1, 1][-1], Vertex[3, 1][2]]]] = 
  {{{PropagatorEndpoints -> {None, CounterTerm}}, 
    {PropagatorEndpoints -> {None, CounterTerm}}, 
    {PropagatorEndpoints -> CounterTerm}}, 
   {Vertex[1][1] -> {0, 10}, Vertex[3, 1][2] -> {10, 10}, 
    Vertex[1][2] -> {20, 10}, Vertex[1, 1][-1] -> {10, 15}}, {}}
 
GraphPoints[Topology[Propagator[Incoming][Vertex[1][1], Vertex[3][1]], 
    Propagator[Outgoing][Vertex[1][2], Vertex[2, 1][2]], 
    Propagator[Internal][Vertex[3][1], Vertex[1, 1][-1]], 
    Propagator[Internal][Vertex[3][1], Vertex[2, 1][2]]]] = 
  {{{PropagatorEndpoints -> {None, Vertex}}, 
    {PropagatorEndpoints -> {None, CounterTerm}}, 
    {PropagatorEndpoints -> {Vertex, CounterTerm}}, 
    {PropagatorEndpoints -> {Vertex, CounterTerm}}}, 
   {Vertex[1][1] -> {0, 10}, Vertex[1][2] -> {20, 10}, 
    Vertex[3][1] -> {6, 10}, Vertex[1, 1][-1] -> {6, 15}, 
    Vertex[2, 1][2] -> {14, 10}}, {}}
 
GraphPoints[Topology[Propagator[Incoming][Vertex[1][1], Vertex[3][2]], 
    Propagator[Outgoing][Vertex[1][2], Vertex[3][2]], 
    Propagator[Internal][Vertex[3][2], Vertex[2, 1][1]], 
    Propagator[Internal][Vertex[1, 1][-1], Vertex[2, 1][1]]]] = 
  {{{PropagatorEndpoints -> {None, Vertex}}, 
    {PropagatorEndpoints -> {None, Vertex}}, 
    {PropagatorEndpoints -> {Vertex, CounterTerm}}, 
    {PropagatorEndpoints -> CounterTerm}}, 
   {Vertex[1][1] -> {0, 10}, Vertex[3][2] -> {10, 10}, 
    Vertex[2, 1][1] -> {10, 14}, Vertex[1, 1][-1] -> {10, 18}, 
    Vertex[1][2] -> {20, 10}}, {}}
 
GraphPoints[Topology[Propagator[Incoming][Vertex[1][1], Vertex[4][2]], 
    Propagator[Outgoing][Vertex[1][2], Vertex[4][2]], 
    Propagator[Internal][Vertex[4][2], Vertex[1, 1][-2]], 
    Propagator[Internal][Vertex[4][2], Vertex[1, 1][-1]]]] = 
  {{{PropagatorEndpoints -> {None, Vertex}}, 
    {PropagatorEndpoints -> {None, Vertex}}, 
    {PropagatorEndpoints -> {Vertex, CounterTerm}}, 
    {PropagatorEndpoints -> {Vertex, CounterTerm}}}, 
   {Vertex[1][1] -> {0, 10}, Vertex[4][2] -> {10, 10}, 
    Vertex[1][2] -> {20, 10}, Vertex[1, 1][-1] -> {10, 5}, 
    Vertex[1, 1][-2] -> {10, 15}}, {}}
 
GraphPoints[Topology[Propagator[Incoming][Vertex[1][1], Vertex[2, 1][1]], 
    Propagator[Outgoing][Vertex[1][2], Vertex[3][2]], 
    Propagator[Internal][Vertex[3][2], Vertex[1, 1][-1]], 
    Propagator[Internal][Vertex[3][2], Vertex[2, 1][1]]]] = 
  {{{PropagatorEndpoints -> {None, CounterTerm}}, 
    {PropagatorEndpoints -> {None, Vertex}}, 
    {PropagatorEndpoints -> {Vertex, CounterTerm}}, 
    {PropagatorEndpoints -> {Vertex, CounterTerm}}}, 
   {Vertex[1][1] -> {0, 10}, Vertex[2, 1][1] -> {6, 10}, 
    Vertex[3][2] -> {14, 10}, Vertex[1, 1][-1] -> {14, 15}, 
    Vertex[1][2] -> {20, 10}}, {}}
 
GraphPoints[Topology[Propagator[Incoming][Vertex[1][1], Vertex[3][1]], 
    Propagator[Outgoing][Vertex[1][2], Vertex[3][2]], 
    Propagator[Internal][Vertex[3][1], Vertex[3][2]], 
    Propagator[Internal][Vertex[3][1], Vertex[1, 1][-2]], 
    Propagator[Internal][Vertex[3][2], Vertex[1, 1][-1]]]] = 
  {{{PropagatorEndpoints -> {None, Vertex}}, 
    {PropagatorEndpoints -> {None, Vertex}}, 
    {PropagatorEndpoints -> Vertex}, 
    {PropagatorEndpoints -> {Vertex, CounterTerm}}, 
    {PropagatorEndpoints -> {Vertex, CounterTerm}}}, 
   {Vertex[1][1] -> {0, 10}, Vertex[3][1] -> {6, 10}, 
    Vertex[3][2] -> {14, 10}, Vertex[1, 1][-2] -> {6, 15}, 
    Vertex[1, 1][-1] -> {14, 15}, Vertex[1][2] -> {20, 10}}, {}}
 
GraphPoints[Topology[Propagator[Incoming][Vertex[1][1], Vertex[3][1]], 
    Propagator[Outgoing][Vertex[1][2], Vertex[3][2]], 
    Propagator[Internal][Vertex[3][1], Vertex[3][2]], 
    Propagator[Internal][Vertex[3][1], Vertex[1, 1][2]], 
    Propagator[Internal][Vertex[3][2], Vertex[1, 1][1]]]] = 
  {{{PropagatorEndpoints -> {None, Vertex}}, 
    {PropagatorEndpoints -> {None, Vertex}}, 
    {PropagatorEndpoints -> Vertex}, 
    {PropagatorEndpoints -> {Vertex, CounterTerm}}, 
    {PropagatorEndpoints -> {Vertex, CounterTerm}}}, 
   {Vertex[1][1] -> {0, 10}, Vertex[3][1] -> {6, 10}, 
    Vertex[3][2] -> {14, 10}, Vertex[1, 1][2] -> {6, 15}, 
    Vertex[1, 1][1] -> {14, 15}, Vertex[1][2] -> {20, 10}}, {}}
 
GraphPoints[Topology[Propagator[Incoming][Vertex[1][1], Vertex[3][2]], 
    Propagator[Outgoing][Vertex[1][2], Vertex[3][2]], 
    Propagator[Internal][Vertex[3][1], Vertex[3][2]], 
    Propagator[Internal][Vertex[3][1], Vertex[1, 1][-2]], 
    Propagator[Internal][Vertex[3][1], Vertex[1, 1][-1]]]] = 
  {{{PropagatorEndpoints -> {None, Vertex}}, 
    {PropagatorEndpoints -> {None, Vertex}}, 
    {PropagatorEndpoints -> Vertex}, 
    {PropagatorEndpoints -> {Vertex, CounterTerm}}, 
    {PropagatorEndpoints -> {Vertex, CounterTerm}}}, 
   {Vertex[1][1] -> {0, 10}, Vertex[3][2] -> {10, 10}, 
    Vertex[3][1] -> {10, 15}, Vertex[1, 1][-1] -> {6, 15}, 
    Vertex[1, 1][-2] -> {14, 15}, Vertex[1][2] -> {20, 10}}, {}}
 
GraphPoints[_Topology] := $Undefined
 
Attributes[Hold] = {HoldAll}
 
HighEnergyPhysics`Graphics`Private`SL = {1.8, 1.8}
 
HighEnergyPhysics`Graphics`Private`BS = {-1.8, 1.8}
 
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
    Circle[Point, 60*Size]}]
 
CounterTerm[3] := 
  Graphics[{Thickness[Size/3.], 
    Line[{Hold[Point] - 15*Size*Hold[HighEnergyPhysics`Graphics`Private`SL], 
      Hold[Point] + 15*Size*Hold[HighEnergyPhysics`Graphics`Private`SL]}], 
    Line[{Hold[Point] - 15*Size*Hold[HighEnergyPhysics`Graphics`Private`BS], 
      Hold[Point] + 15*Size*Hold[HighEnergyPhysics`Graphics`Private`BS]}], 
    Circle[Point, 60*Size], Circle[Point, 90*Size]}]
