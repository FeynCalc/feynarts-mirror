(*
	Topology.m
		Creation of topologies for Feynman graphs
		last modified 1 Mar 00 th
*)

Begin["`Topology`"]

(*
   Definition of starting topologies
   =================================

   The sets of starting topologies are classified according to their
   loop number "l" and counterterm order "c" by definition of 
   StartTop[l, c]. Usually its setting is a list of topologies
   that contains statements of the form "define[name] = starttop".
   The identifier "name" can then be used with StartingTopologies
   to select a subset from the set of starting topologies. Note that
   this identifier must either be declared in FeynArts.m via usage or
   else live in the Global` context ("define[Global`foo] = ...").
   If there is only one starting topology, or one always wants to
   use all of the starting topologies, the define[...] can be omitted.

   Since entering starting topologies is not an everyday job, some
   restrictions have been imposed that enable FeynArts to work with much 
   faster algorithms:

   1) There is an important distinction between positive and negative
      vertex identifiers (i.e. the v in Vertex[e][v]). Vertices with
      negative identifiers are so-called permutable vertices. They are
      used for weeding out topologically equivalent topologies in Compare.
      The algorithm is roughly the following:
      The topologies are sorted into some canonical order, and then
      compared. This simple method, however, fails whenever a graph has
      a symmetry. In that case, the indices of the symmetrical vertices
      have to be permuted to give all topologically equivalent versions.
      It is this ``power set'' of each topology that is actually compared.
      If you're not sure which vertices should be permutables, make them
      ALL permutables. This will be slower, but safer.

   2) For the correct functioning of the ExcludeTopologies -> ... option 
      it is essential that the propagators on a irreducible conglomerate 
      of loops have the SAME loop number (the n in Loop[n]), no matter how 
      many loops there actually are. For example, the two-loop starting 
      topology `Theta' has only Loop[1] propagators.

   3) Vertex identifiers must always be unique, e.g. having both a 
      Vertex[3][1] and a Vertex[4][1] within the same topology is 
      forbidden.

   4) To determine the symmetry factor, enter the topology with an
      arbitrary factor first (e.g. Topology[1][...]), then use
      SymmetryFactor to find the factor, and supplement the initial
      definition with the correct factor.
*)

StartTop[0, 0] = TopologyList[
  Topology[1][   
    Propagator[External][Vertex[1][1], Vertex[3][4]],
    Propagator[External][Vertex[1][2], Vertex[3][4]],
    Propagator[External][Vertex[1][3], Vertex[3][4]] ]
]

StartTop[0, 1] = TopologyList[
  Topology[1][
    Propagator[External][Vertex[1][1], Vertex[1, 1][2]] ]
]

StartTop[0, 2] = TopologyList[
  define[Global`CT[0, 2][1]] = Topology[2][
    Propagator[Internal][Vertex[1, 1][-1], Vertex[1, 1][-2]] ],
  define[Global`CT[0, 2][2]] = Topology[1][
    Propagator[External][Vertex[1][1], Vertex[1, 2][2]] ]
]

StartTop[0, 3] = TopologyList[
  define[Global`CT[0, 3][1]] = Topology[1][
    Propagator[External][Vertex[1][1], Vertex[1, 3][2]] ],
  define[Global`CT[0, 3][2]] = Topology[1][
    Propagator[Internal][Vertex[1, 1][1], Vertex[1, 2][2]] ],
  define[Global`CT[0, 3][3]] = Topology[2][
    Propagator[Internal][Vertex[1, 1][-1], Vertex[2, 1][1]],
    Propagator[Internal][Vertex[2, 1][1], Vertex[1, 1][-2]] ],
  define[Global`CT[0, 3][4]] = Topology[6][
    Propagator[Internal][Vertex[1, 1][-1], Vertex[3][1]],
    Propagator[Internal][Vertex[1, 1][-2], Vertex[3][1]],
    Propagator[Internal][Vertex[1, 1][-3], Vertex[3][1]] ]
]

StartTop[1, 0] = TopologyList[
  Topology[2][   
    Propagator[Loop[1]][Vertex[3][2], Vertex[3][2]],
    Propagator[External][Vertex[1][1], Vertex[3][2]] ]
]

StartTop[1, 1] = TopologyList[
  define[Global`CT[1, 1][1]] = Topology[2][
    Propagator[Loop[1]][Vertex[2, 1][1], Vertex[2, 1][1]] ],
  define[Global`CT[1, 1][2]] = Topology[2][
    Propagator[Loop[1]][Vertex[3][2], Vertex[3][2]],
    Propagator[Internal][Vertex[1, 1][1], Vertex[3][2]] ]
]

StartTop[1, 2] = TopologyList[
  define[Global`CT[1, 2][1]] = Topology[2][
    Propagator[Loop[1]][Vertex[2, 2][1], Vertex[2, 2][1]] ],
  define[Global`CT[1, 2][2]] = Topology[2][
    Propagator[Loop[1]][Vertex[3][2], Vertex[3][2]],
    Propagator[Internal][Vertex[1, 2][1], Vertex[3][2]] ],
  define[Global`CT[1, 2][3]] = Topology[2][
    Propagator[Loop[1]][Vertex[2, 1][1], Vertex[3][2]],
    Propagator[Loop[1]][Vertex[2, 1][1], Vertex[3][2]],
    Propagator[Internal][Vertex[1, 1][3], Vertex[3][2]] ],
  define[Global`CT[1, 2][4]] = Topology[2][
    Propagator[Loop[1]][Vertex[3][3], Vertex[3][3]],
    Propagator[Internal][Vertex[1, 1][1], Vertex[2, 1][2]],
    Propagator[Internal][Vertex[2, 1][2], Vertex[3][3]] ],
  define[Global`CT[1, 2][5]] = Topology[4][
    Propagator[Loop[1]][Vertex[2, 1][-1], Vertex[2, 1][-2]],
    Propagator[Loop[1]][Vertex[2, 1][-1], Vertex[2, 1][-2]] ],
  define[Global`CT[1, 2][6]] = Topology[4][
    Propagator[Loop[1]][Vertex[3][2], Vertex[3][2]],
    Propagator[Internal][Vertex[1, 1][-1], Vertex[3][1]],
    Propagator[Internal][Vertex[1, 1][-2], Vertex[3][1]],
    Propagator[Internal][Vertex[3][1], Vertex[3][2]] ],
  define[Global`CT[1, 2][7]] = Topology[2][
    Propagator[Loop[1]][Vertex[3, 1][2], Vertex[3, 1][2]],
    Propagator[Internal][Vertex[1, 1][1], Vertex[3, 1][2]] ],
  define[Global`CT[1, 2][8]] = Topology[4][
    Propagator[Loop[1]][Vertex[3][-3], Vertex[3][-4]],
    Propagator[Loop[1]][Vertex[3][-3], Vertex[3][-4]],
    Propagator[Internal][Vertex[1, 1][-1], Vertex[3][-3]],
    Propagator[Internal][Vertex[1, 1][-2], Vertex[3][-4]] ],
  define[Global`CT[1, 2][9]] = Topology[4][
    Propagator[Loop[1]][Vertex[4][1], Vertex[4][1]],
    Propagator[Internal][Vertex[1, 1][-1], Vertex[4][1]],
    Propagator[Internal][Vertex[1, 1][-2], Vertex[4][1]] ]
]

StartTop[2, 0] = TopologyList[
  define[Theta] = Topology[12][
    Propagator[Loop[1]][Vertex[3][-2], Vertex[3][-1]],
    Propagator[Loop[1]][Vertex[3][-2], Vertex[3][-1]],
    Propagator[Loop[1]][Vertex[3][-2], Vertex[3][-1]] ],
  define[Eight] = Topology[8][
    Propagator[Loop[1]][Vertex[4][1], Vertex[4][1]],
    Propagator[Loop[1]][Vertex[4][1], Vertex[4][1]] ],
  define[Bicycle] = Topology[8][   
    Propagator[Loop[1]][Vertex[3][-1], Vertex[3][-1]],
    Propagator[Loop[2]][Vertex[3][-2], Vertex[3][-2]],
    Propagator[Internal][Vertex[3][-2], Vertex[3][-1]] ]
]

StartTop[2, 1] = TopologyList[
  define[Global`CT[2, 1][1]] = Topology[6][
    Propagator[Loop[1]][Vertex[3][-1], Vertex[3, 1][-2]],
    Propagator[Loop[1]][Vertex[3][-1], Vertex[3, 1][-2]],
    Propagator[Loop[1]][Vertex[3][-1], Vertex[3, 1][-2]] ],
  define[Global`CT[2, 1][2]] = Topology[4][
    Propagator[Loop[1]][Vertex[3][-1], Vertex[3][-2]],
    Propagator[Loop[1]][Vertex[3][-1], Vertex[3][-2]],
    Propagator[Loop[1]][Vertex[3][-1], Vertex[2, 1][1]],
    Propagator[Loop[1]][Vertex[3][-2], Vertex[2, 1][1]] ],
  define[Global`CT[2, 1][3]] = Topology[8][
    Propagator[Loop[1]][Vertex[3][-1], Vertex[3][-1]],
    Propagator[Loop[2]][Vertex[3][-2], Vertex[3][-2]],
    Propagator[Internal][Vertex[3][-1], Vertex[3][-3]],
    Propagator[Internal][Vertex[3][-2], Vertex[3][-3]],
    Propagator[Internal][Vertex[3][-3], Vertex[1, 1][1]] ],
  define[Global`CT[2, 1][4]] = Topology[4][
    Propagator[Loop[1]][Vertex[4][1], Vertex[4][1]],
    Propagator[Loop[1]][Vertex[4][1], Vertex[2, 1][2]],
    Propagator[Loop[1]][Vertex[4][1], Vertex[2, 1][2]] ],
  define[Global`CT[2, 1][5]] = Topology[8][
    Propagator[Loop[1]][Vertex[4, 1][1], Vertex[4, 1][1]],
    Propagator[Loop[1]][Vertex[4, 1][1], Vertex[4, 1][1]] ], 
  define[Global`CT[2, 1][6]] = Topology[6][
    Propagator[Loop[1]][Vertex[3][2], Vertex[4][2]],
    Propagator[Loop[1]][Vertex[3][2], Vertex[4][2]],
    Propagator[Loop[1]][Vertex[3][2], Vertex[4][2]],
    Propagator[Internal][Vertex[4][2], Vertex[1, 1][1]] ],
  define[Global`CT[2, 1][7]] = Topology[4][
    Propagator[Loop[1]][Vertex[3][-1], Vertex[3][-2]],
    Propagator[Loop[1]][Vertex[3][-1], Vertex[3][-2]],
    Propagator[Loop[1]][Vertex[3][-1], Vertex[3][2]],
    Propagator[Loop[1]][Vertex[3][-2], Vertex[3][2]],
    Propagator[Internal][Vertex[3][2], Vertex[1, 1][1]] ],
  define[Global`CT[2, 1][8]] = Topology[4][
    Propagator[Loop[1]][Vertex[4][2], Vertex[4][2]],
    Propagator[Loop[1]][Vertex[4][2], Vertex[3][3]],
    Propagator[Loop[1]][Vertex[4][2], Vertex[3][3]],
    Propagator[Internal][Vertex[3][3], Vertex[1, 1][1]] ], 
  define[Global`CT[2, 1][9]] = Topology[4][
    Propagator[Loop[1]][Vertex[3][2], Vertex[3][2]],
    Propagator[Internal][Vertex[3][2], Vertex[3][3]],
    Propagator[Loop[2]][Vertex[3][3], Vertex[3][4]],
    Propagator[Loop[2]][Vertex[3][3], Vertex[3][4]],
    Propagator[Internal][Vertex[3][4], Vertex[1, 1][1]] ],
  define[Global`CT[2, 1][10]] = Topology[8][
    Propagator[Loop[1]][Vertex[3][-1], Vertex[3][-1]],
    Propagator[Internal][Vertex[3][-1], Vertex[2, 1][1]],
    Propagator[Internal][Vertex[3][-2], Vertex[2, 1][1]],
    Propagator[Loop[2]][Vertex[3][-2], Vertex[3][-2]] ],
  define[Global`CT[2, 1][11]] = Topology[4][
    Propagator[Loop[1]][Vertex[2, 1][1], Vertex[3][2]],
    Propagator[Loop[1]][Vertex[2, 1][1], Vertex[3][2]],
    Propagator[Loop[2]][Vertex[3][3], Vertex[3][3]],
    Propagator[Internal][Vertex[3][2], Vertex[3][3]] ],
  define[Global`CT[2, 1][12]] = Topology[4][
    Propagator[Loop[1]][Vertex[3, 1][1], Vertex[3, 1][1]],
    Propagator[Loop[2]][Vertex[3][2], Vertex[3][2]],
    Propagator[Internal][Vertex[3, 1][1], Vertex[3][2]] ], 
  define[Global`CT[2, 1][13]] = Topology[4][
    Propagator[Loop[1]][Vertex[3][2], Vertex[3][2]],
    Propagator[Internal][Vertex[3][2], Vertex[4][3]],
    Propagator[Loop[2]][Vertex[4][3], Vertex[4][3]],
    Propagator[Internal][Vertex[4][3], Vertex[1, 1][1]] ],
  define[Global`CT[2, 1][14]] = Topology[8][
    Propagator[Loop[1]][Vertex[5][2], Vertex[5][2]],
    Propagator[Loop[1]][Vertex[5][2], Vertex[5][2]],
    Propagator[Internal][Vertex[5][2], Vertex[1, 1][1]] ]
]

StartTop[3, 0] = TopologyList[ 
	(* Mercedes star: 4 v3 *)
  define[Three[1]] = Topology[24][ 
    Propagator[Loop[1]][Vertex[3][-1], Vertex[3][-2]],
    Propagator[Loop[1]][Vertex[3][-2], Vertex[3][-3]],
    Propagator[Loop[1]][Vertex[3][-3], Vertex[3][-1]],
    Propagator[Loop[1]][Vertex[3][-1], Vertex[3][-4]],
    Propagator[Loop[1]][Vertex[3][-2], Vertex[3][-4]],
    Propagator[Loop[1]][Vertex[3][-3], Vertex[3][-4]] ],
	(* 2 Circles: 4 v3 *)
  define[Three[2]] = Topology[16][ 
    Propagator[Loop[1]][Vertex[3][-1], Vertex[3][-2]],
    Propagator[Loop[1]][Vertex[3][-2], Vertex[3][-3]],
    Propagator[Loop[1]][Vertex[3][-2], Vertex[3][-3]],
    Propagator[Loop[1]][Vertex[3][-3], Vertex[3][-4]],
    Propagator[Loop[1]][Vertex[3][-4], Vertex[3][-1]],
    Propagator[Loop[1]][Vertex[3][-1], Vertex[3][-4]] ],
	(* 2 Circles in touch: 2 v3 + 1 v4 *)
  define[Three[3]] = Topology[8][
    Propagator[Loop[1]][Vertex[3][-1], Vertex[3][-2]],
    Propagator[Loop[1]][Vertex[3][-1], Vertex[4][1]],
    Propagator[Loop[1]][Vertex[3][-1], Vertex[4][1]],
    Propagator[Loop[1]][Vertex[3][-2], Vertex[4][1]],
    Propagator[Loop[1]][Vertex[3][-2], Vertex[4][1]] ],
	(* Extended Theta (1): 2 v3 + 1 v4 *)
  define[Three[4]] = Topology[8][
    Propagator[Loop[1]][Vertex[3][-1], Vertex[3][-2]],
    Propagator[Loop[1]][Vertex[3][-1], Vertex[3][-2]],
    Propagator[Loop[1]][Vertex[3][-1], Vertex[4][1]],
    Propagator[Loop[1]][Vertex[3][-2], Vertex[4][1]],
    Propagator[Loop[1]][Vertex[4][1], Vertex[4][1]] ],
	(* Eye of the tiger: 2 v4 *)
  define[Three[5]] = Topology[48][
    Propagator[Loop[1]][Vertex[4][-1], Vertex[4][-2]],
    Propagator[Loop[1]][Vertex[4][-1], Vertex[4][-2]],
    Propagator[Loop[1]][Vertex[4][-1], Vertex[4][-2]],
    Propagator[Loop[1]][Vertex[4][-1], Vertex[4][-2]] ],
	(* 3 Circles: 2 v4 *)
  define[Three[6]] = Topology[16][
    Propagator[Loop[1]][Vertex[4][-1], Vertex[4][-1]],
    Propagator[Loop[1]][Vertex[4][-1], Vertex[4][-2]],
    Propagator[Loop[1]][Vertex[4][-1], Vertex[4][-2]],
    Propagator[Loop[1]][Vertex[4][-2], Vertex[4][-2]] ],
	(* Extended Theta (2): 1 v3 + 1 v5 *)
  define[Three[7]] = Topology[12][
    Propagator[Loop[1]][Vertex[3][1], Vertex[5][2]],
    Propagator[Loop[1]][Vertex[3][1], Vertex[5][2]],
    Propagator[Loop[1]][Vertex[3][1], Vertex[5][2]],
    Propagator[Loop[1]][Vertex[5][2], Vertex[5][2]] ],
	(* 3-leaf clover: 1 v6 *)
  define[Three[8]] = Topology[48][
    Propagator[Loop[1]][Vertex[6][1], Vertex[6][1]],
    Propagator[Loop[1]][Vertex[6][1], Vertex[6][1]],
    Propagator[Loop[1]][Vertex[6][1], Vertex[6][1]] ],
	(* Theta + Tadpole on Vertex *)
  define[ThreeRed[1]] = Topology[12][
    Propagator[Loop[1]][Vertex[3][1], Vertex[4][2]],
    Propagator[Loop[1]][Vertex[3][1], Vertex[4][2]],
    Propagator[Loop[1]][Vertex[3][1], Vertex[4][2]],
    Propagator[Internal][Vertex[4][2], Vertex[3][3]],
    Propagator[Loop[2]][Vertex[3][3], Vertex[3][3]] ],
	(* Theta + Tadpole on Propagator *)
  define[ThreeRed[2]] = Topology[8][
    Propagator[Loop[1]][Vertex[3][-1], Vertex[3][-2]],
    Propagator[Loop[1]][Vertex[3][-1], Vertex[3][-2]],
    Propagator[Loop[1]][Vertex[3][-1], Vertex[3][1]],
    Propagator[Loop[1]][Vertex[3][-2], Vertex[3][1]],
    Propagator[Internal][Vertex[3][1], Vertex[3][2]],
    Propagator[Loop[2]][Vertex[3][2], Vertex[3][2]] ],
	(* 3 Tadpoles *)
  define[ThreeRed[3]] = Topology[48][
    Propagator[Internal][Vertex[3][1], Vertex[3][-1]],
    Propagator[Internal][Vertex[3][1], Vertex[3][-2]],
    Propagator[Internal][Vertex[3][1], Vertex[3][-3]],
    Propagator[Loop[1]][Vertex[3][-1], Vertex[3][-1]],
    Propagator[Loop[2]][Vertex[3][-2], Vertex[3][-2]],
    Propagator[Loop[3]][Vertex[3][-3], Vertex[3][-3]] ],
	(* Bicycle + Tadpole on Loop *)
  define[ThreeRed[4]] = Topology[16][
    Propagator[Loop[1]][Vertex[3][-1], Vertex[3][-1]],
    Propagator[Internal][Vertex[3][-1], Vertex[3][-2]],
    Propagator[Loop[2]][Vertex[3][-2], Vertex[3][-3]],
    Propagator[Loop[2]][Vertex[3][-2], Vertex[3][-3]],
    Propagator[Internal][Vertex[3][-3], Vertex[3][-4]],
    Propagator[Loop[3]][Vertex[3][-4], Vertex[3][-4]] ],
	(* Bicycle + Tadpole on Vertex *)
  define[ThreeRed[5]] = Topology[16][
    Propagator[Loop[1]][Vertex[3][-1], Vertex[3][-1]],
    Propagator[Internal][Vertex[3][-1], Vertex[4][1]],
    Propagator[Loop[2]][Vertex[4][1], Vertex[4][1]],
    Propagator[Internal][Vertex[4][1], Vertex[3][-2]],
    Propagator[Loop[3]][Vertex[3][-2], Vertex[3][-2]] ],
	(* Eight + Tadpole on Loop *)
  define[ThreeRed[6]] = Topology[8][
    Propagator[Loop[1]][Vertex[4][1], Vertex[4][1]],
    Propagator[Loop[1]][Vertex[4][1], Vertex[3][2]],
    Propagator[Loop[1]][Vertex[4][1], Vertex[3][2]],
    Propagator[Internal][Vertex[3][2], Vertex[3][3]],
    Propagator[Loop[2]][Vertex[3][3], Vertex[3][3]] ],
	(* Eight + Tadpole on vertex => v5 *)
  define[ThreeRed[7]] = Topology[16][
    Propagator[Loop[1]][Vertex[5][1], Vertex[5][1]],
    Propagator[Loop[1]][Vertex[5][1], Vertex[5][1]],
    Propagator[Internal][Vertex[5][1], Vertex[3][2]],
    Propagator[Loop[2]][Vertex[3][2], Vertex[3][2]] ]
]


Attributes[ TopologyList ] = {Flat}


Options[ CreateTopologies ] = {
  Adjacencies -> {3, 4},
  ExcludeTopologies -> {},
  StartingTopologies -> All,
  CountertermOrder -> 0
}

CreateTopologies::nostart = 
"No starting topologies defined for `1` loops and counter-term order `2`."

CreateTopologies::nomatch = 
"Warning: No starting topologies matched any of `1`."

CreateTopologies::badcto = 
"Bad counter-term order `1`."

CreateTopologies::badadj = 
"Adjacencies must be integers > 2."

CreateTopologies::delundef = 
"No $ExcludeTopologies function defined for `1`."

(* main function supporting i -> o input: *)

CreateTopologies[ l_Integer, i_Integer -> o_Integer, opt___Rule ] :=
  CreateTopologies[l, i + o, opt] /. 
    Propagator[External][v1:Vertex[1][j_], v2_] :>
      Propagator[ If[j > i, Outgoing, Incoming] ][v1, v2]

CreateTopologies[ l_Integer, n_Integer, options___Rule ] := 
Block[ {excl, start, cto, adj, emax, forb, tops, tree,
opt = ActualOptions[CreateTopologies, options]},

  If[ !VectorQ[adj = Flatten[{Adjacencies /. opt}],
        IntegerQ[#] && # > 2 &],
    Message[CreateTopologies::badadj];
    Return[$Aborted] ];

  If[ !IntegerQ[cto = CountertermOrder /. opt] || cto < 0,
    Message[CreateTopologies::badcto, cto];
    Return[$Aborted] ];

  If[ Head[StartTop[l, cto]] === StartTop,
    Message[CreateTopologies::nostart, l, cto];
    Return[$Aborted] ]; 

  If[ (start = StartingTopologies /. opt) === All,
    start = StartTop[l, cto],
  (* else *)
    forb = Flatten[{start}];
    start = Cases[DownValues[define],
      (_[_[Alternatives@@ forb]] :> s_) -> s];
    If[ Length[start] === 0,
      Message[CreateTopologies::nomatch, forb];
      Return[TopologyList[]] ]
  ];

  emax = Max[adj];
  start = Select[start, FreeQ[#, Vertex[e_, ___] /; e > emax]&];
	(* set recursion end: *)
  Scan[
    ( ConstructTopologies[l, cto, Count[#, Propagator[External][__]],
        emax, #] = TopologyList[#] )&,
    start ];
  tops = TopologyList@@
    (ConstructTopologies[l, cto, n, emax, #]&)/@ start;

  If[ Length[ forb = Complement[Range[3, emax], adj] ] =!= 0,
    forb = Vertex[Alternatives@@ forb, ___];
    tops = Select[tops, FreeQ[#, forb]&] ];

  excl = $ExcludeTopologies/@
    Union[Flatten[{ExcludeTopologies /. opt}]];
  If[ Length[excl] =!= 0,
    If[ Head[ excl = And@@ excl ] === And,
      excl = FlattenAt[Evaluate[excl]&, Array[{1, #}&, Length[excl]]] ];
    If[ Length[ forb = Position[excl, ToTree] ] > 1,
      excl = ReplacePart[excl, tree, Drop[#, -1]&/@ Rest[forb]] ];
    tops = Select[tops, excl] ];

  Map[Sort, tops, 2]
]


CreateCTTopologies::cterr =
"Error in CreateTopologies for `1` loops and counter-term order `2`."

CreateCTTopologies[ c_Integer,
  e:(_Integer -> _Integer) | _Integer, opt___Rule ] :=
Block[ {x},
  TopologyList@@ Array[
    ( x = CreateTopologies[#, e, CountertermOrder -> c - #, opt];
      If[x === $Aborted, Message[CreateCTTopologies::cterr, #, c - #] ];
      x )&, c, 0 ]
]


(* Recursive creation of topologies. The internal function that does
   all the work is ConstructTopologies. It saves all values once found. 
   To stop the recursion CreateTopologies defines ConstructTopologies
   with the minimum number of legs.
   ConstructTopologies always generates ALL topologies with up to emax
   edges on any vertex. Those edge numbers that are to be omitted
   (e.g. Adjacencies -> {4, 6}) are sorted out by CreateTopologies. *)

(* if the recursion failed to stop: *)

ConstructTopologies[ _, _, _?Negative, _, _ ] = TopologyList[]

(* recursive generation of topologies *)

ConstructTopologies[ l_, cto_, ext_, emax_, top_ ] :=
  ConstructTopologies[l, cto, ext, emax, top] =
    TopologyList[ AddOne[#, ext, emax]&/@
      ConstructTopologies[l, cto, ext - 1, emax, top] ]


(* Add external line to a propagator.
   Vertices added by the program are numbered starting at
   100, 101, 102... hopefully this will avoid index conflicts. *)

AddToPropagator[ Propagator[h_][from_, to_], n_ ] :=
  Topology[h /. Loop[_] -> Loop][
    Propagator[h][from, Vertex[3][n + 99]],
    Propagator[h /. External -> Internal][Vertex[3][n + 99], to],
    Propagator[External][Vertex[1][n], Vertex[3][n + 99]] ]

AddPropagator[ top_, n_ ] :=
  TopologiesCompare[
    TopologyList@@
      Array[ MapAt[AddToPropagator[#, n]&, top, #]&, Length[top] ] /.
        Topology[s_][p1___, Topology[snew_][pnew__], p2___] :> 
          Topology[s snew][p1, pnew, p2]
  ]


(* add external line to a vertex *)

AddToVertex[ Topology[s_][pr__], vert_, n_ ] :=
Block[ {newv = vert},
  ++newv[[0, 1]];
  Topology[ s If[vert[[1]] < 0, Permutable, Vertex] ][ pr,
    Propagator[External][Vertex[1][n], newv] ] /. vert -> newv
]

AddVertex[ top_, n_, emax_ ] :=
Block[ {vert},
  vert = Union[Cases[ top,
    Vertex[e_, c___][_] /; e < emax && {e, c} =!= {1}, {2} ]];
  TopologyList@@
    (TopologiesCompare[
      TopologyList@@ Cases[vert, v:#[__] :> AddToVertex[top, v, n]] ]&)/@
    Union[Head/@ vert]
]


AddOne[ top_, n_, emax_ ] :=
  TopologyList[ AddPropagator[top, n], AddVertex[top, n, emax] ]


(* Compare topologies *)

TopologiesCompare[ tops_ ] :=
Block[ {comp},
  comp =
    If[ Count[ tops[[1]], Propagator[Loop[_]][__] ] > 1 ||
          !FreeQ[tops[[1]], Vertex[_, _][_]],
      Loop | Permutable | Internal,
    (* else *)
      Loop | Permutable
    ];
  Join[ Select[tops, FreeQ[#[[0, 1]], comp]&],
    Compare[ Select[tops, !FreeQ[#[[0, 1]], comp]&] ] ] /.
    Topology[s_. __Symbol][p__] :> Topology[s][p]
]


(* Finding the symmetry factor: start with symfac = 1, append new
   propagators to loop propagators until all loop propagators have two
   additional vertices. After each step do a TopologiesCompare and choose
   the topology with the smallest symmetry factor (counting downwards,
   since we start with symfac = 1), then proceed to add the next
   propagator. The inverse of the last symfac is then the s in
   Topology[s]. *)

SymmetryFactor[ top:P$Topology ] :=
Block[ {ext, p, t = Topology[1]@@ top},
  ext = Min[ 0, Cases[top, Vertex[1][n_] -> n, {2}] ];
  While[ Length[ p = Flatten[MapIndexed[ChooseProp, List@@ t]] ] =!= 0,
    ++ext;
    t = TopologiesCompare[
      MapAt[AddToPropagator[#, ext]&, t, #]&/@ p /.
        Topology[s_][p1___, Topology[snew_][pnew__], p2___] :> 
          Topology[s snew][p1, pnew, p2] ];
    p = #[[0, 1]]&/@ t;
    t = t[[ Position[p, Min[p], 1, 1][[1, 1]] ]];
  ];
  1 / t[[0, 1]]
]


ChooseProp[ pr_, _ ] := {} /; FreeQ[pr, Loop | Vertex[_, _]]

ChooseProp[ pr_, {n_} ] :=
  Switch[ # < 100 &/@ Level[pr, {2}],
    {True, True},
      n,
    {True, False},  
      If[MemberQ[t, _[ pr[[2]], _[i_] ] /; i >= 100], {}, n],
    {False, True},
      If[MemberQ[t, _[ _[i_], pr[[1]] ] /; i >= 100], {}, n],
    _,
      {}
  ]


(* $ExcludeTopologies[name] defines the filter "name" for use with the
   ExcludeTopologies -> ... option of CreateTopologies.

   When programming own filters you must make sure that the filter
   is always defined as a pure function, i.e. func[#]&. In the filter
   function ToTree may be used which returns its argument (a topology)
   with the loops shrunk to a point named Centre[adj][n] where adj is
   the adjacency of loop n. *)

$ExcludeTopologies[ Tadpoles ] = FreeQ[ToTree[#], Centre[1]]&

$ExcludeTopologies[ TadpoleCTs ] = FreeQ[#, Vertex[1, _]]&

$ExcludeTopologies[ SelfEnergies ] = FreeQ[ToTree[#], Centre[2]]&

$ExcludeTopologies[ SelfEnergyCTs ] = FreeQ[#, Vertex[2, _]]&

$ExcludeTopologies[ Triangles ] = FreeQ[ToTree[#], Centre[3]]&

$ExcludeTopologies[ TriangleCTs ] = FreeQ[#, Vertex[3, _]]&

$ExcludeTopologies[ Boxes[m_] ]= FreeQ[ToTree[#], Centre[m]]&

$ExcludeTopologies[ BoxCTs[m_] ] = FreeQ[#, Vertex[m, _]]&

$ExcludeTopologies[ AllBoxes ] = FreeQ[ToTree[#], Centre[n_] /; n >= 4]&

$ExcludeTopologies[ AllBoxCTs ] = FreeQ[#, Vertex[n_, _] /; n >= 4]&

$ExcludeTopologies[ WFCorrections ] =
  FreeWFQ[ToTree[#], Centre[1], Centre[2]]&

$ExcludeTopologies[ WFCorrectionCTs ] =
  FreeWFQ[#, Vertex[1, _], Vertex[2, _]]&

$ExcludeTopologies[ Internal ] = FreeQ[#, Internal]&

$ExcludeTopologies[ undef_ ] :=
  (Message[CreateTopologies::delundef, undef]; Seq[])


ToTree[ top_ ] :=
Block[ {l, v, props, loops = {}},
  props[_] = {};
  tree = top /. Propagator[Loop[l_]][x__] :>
    (loops = {loops, l}; props[l] = {props[l], x}; Seq[]);
  v = Apply[Sequence, tree, 1];
  tree = Fold[
    ( l = Cases[ v, Alternatives@@ Flatten[props[#2]] ];
      #1 /. Thread[ Union[l] -> Centre[Length[l]][#2] ] )&,
    tree, Union[Flatten[loops]] ]
]


FreeWFQ[ top_, patt1_, patt2_ ] :=
Block[ {br, p},
  Catch[
    WFCheck[top, #]&/@ Union[Cases[top, patt2[_], {2}]];
    If[ Length[ p = Position[top, patt1[_], {2}] ] === 0, Throw[True] ];
    br = List@@ Curtail@@
      MapAt[branch@@ # &, Delete[top, p], Take[#, 1]&/@ p];
    WFCheck[br, #]&/@ Cases[br, branch[v_] -> v];
    True
  ]
]

WFCheck[ top_, v_ ] :=
  If[ Sort[ Cases[top, _[type_][p__] :> type /; MemberQ[{p}, v]] ] ===
        {External, Internal}, Throw[False] ]

Attributes[ Curtail ] = {Orderless, Flat}

Curtail[ br:branch[a_].., _[_][c___, a_, d___] ] :=
  Curtail[branch[c, d]] /; a[[0, 1]] - Length[{br}] < 2

End[]

