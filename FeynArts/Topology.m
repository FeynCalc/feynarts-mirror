
(* :Title: Topology *)

(* :Authors: Hagen Eck, Sepp Kueblbeck *)

(* :Summary: Creation of topologies for Feynman graphs *)

(* :Context: HighEnergyPhysics`FeynArts` *)

(* :Package Version 2.3 *)

(* :Mathematica Version 2.0 *)

(* :History:
	Created Oct, 1992 from the CreateTopologies-package of Feyn-
	Arts 1.0.
	Package Version 2.0: no 2-Vertices
	Package Version 2.1: 2-Vertices included (new option "TwoVertices")
	Package Version 2.2: Counterterm topologies (new function
			     "CreateCTTopologies")
	Package Version 2.3: arbitrary number of edges on Vertices
			     (new option "Adjacencies")
	Package Version 2.4: re-implementation of counterterm toplogies
			     via starting topologies.
			     deleted 2-vertices feature 
*)

(* :Contents:
	Part1: definition of starting topologies
	Part2: CreateTopologies + CreateCTTopologies
	Part3: tests (for graph types) + delete-functions
	Part4: doing all the work (AddOne and its sub functions )
	Part5: comparing topologies
	(search for Part# to find the beginning of a section)
*)

FAPrint[3,"[Topology.m]"];

Begin["`Topology`"]

Options[CreateTopologies] =
 {
  Adjacencies		-> {3, 4},
  ExcludeTopologies 	-> None,
  StartingTopologies 	-> All,
  CountertermOrder	-> 0
 };

Options[CreateCTTopologies] =
 {
  StopOnError		-> True
 };

(* 
  :Part1:
	definition of starting topologies.
	The sets of starting topologies are classified according to their
	loop number "l" and counterterm order "c" by definition of 
	StartTop[l, c]. Usually its setting is a list of topologies
	that contains statements of the form "define[name] = starttop".
	The identifier "name" can be used in the option StartingTopologies
	to select a subset from the set of starting topologies. Note that
	this identifier either has to be declared in FeynArts.m or must
	live in the Global` context ("define[Global`foo] = .." ).
        If there is only one starting topology, or one always wants to
	use all of the starting topologies, the define's can be omitted.
	!There is sort of a bug in the topology generation: we can't 
	 handle 3-vertices with positive identifiers. NEVER use some-
	 thing like Vertex[3][1] in a starting topology. We need them
	 for appending the external legs. 3-vertices always negative!
*)

StartTop[0, 0] = TopologyList[
     Topology[1][   
      Propagator[External][Vertex[1][1],Vertex[3][1]],
      Propagator[External][Vertex[1][2],Vertex[3][1]],
      Propagator[External][Vertex[1][3],Vertex[3][1]]    ]
];

StartTop[0, 1] = TopologyList[
     Topology[1][   
      Propagator[External][Vertex[1][1],Vertex[1,1][1]] ]
];

StartTop[0, 2] = TopologyList[
define[Global`CT[0,2][1]] = Topology[2][
       Propagator[Internal][Vertex[1,1][-1],Vertex[1,1][-2]] ],
define[Global`CT[0,2][2]] = Topology[1][
       Propagator[External][Vertex[1][1],Vertex[1,2][1]] ]
];

StartTop[0, 3] = TopologyList[
define[Global`CT[0,3][1]] = Topology[1][
       Propagator[External][Vertex[1][1],Vertex[1,3][1]] ],
define[Global`CT[0,3][2]] = Topology[1][
       Propagator[Internal][Vertex[1,1][1],Vertex[1,2][2]] ],
define[Global`CT[0,3][3]] = Topology[2][ (* <- cf correct? *)
       Propagator[Internal][Vertex[1,1][-1],Vertex[1,1][1]],
       Propagator[Internal][Vertex[1,1][1],Vertex[1,1][-2]] ],
define[Global`CT[0,3][4]] = Topology[6][ (* <- what about this one? *)
       Propagator[Internal][Vertex[1,1][-1],Vertex[3][-1]],
       Propagator[Internal][Vertex[1,1][-2],Vertex[3][-1]],
       Propagator[Internal][Vertex[1,1][-3],Vertex[3][-1]] ]
];

StartTop[1, 0] = TopologyList[
     Topology[2][   
      Propagator[Loop[1]][ Vertex[3][1] , Vertex[3][1] ],
      Propagator[External][ Vertex[1][1] , Vertex[3][1] ]    ]
];

StartTop[1, 1] = TopologyList[
define[Global`CT[1,1][1]] =  Topology[2][   
      Propagator[Loop[1]][ Vertex[2,1][1] , Vertex[2,1][1] ]    ],
define[Global`CT[1,1][2]] =  Topology[2][   
      Propagator[Loop[1]][ Vertex[3][-1] , Vertex[3][-1] ],
      Propagator[Internal][ Vertex[1,1][1] , Vertex[3][-1] ]    ]
];

StartTop[1, 2] = TopologyList[
define[Global`CT[1,2][1]] =  Topology[2][   
      Propagator[Loop[1]][ Vertex[2,2][1] , Vertex[2,2][1] ]    ],
define[Global`CT[1,2][2]] =  Topology[2][   
      Propagator[Loop[1]][ Vertex[3][1] , Vertex[3][1] ],
      Propagator[Internal][ Vertex[1,2][1] , Vertex[3][1] ]    ],
define[Global`CT[1,2][3]] =  Topology[2][   
      Propagator[Loop[1]][ Vertex[2,1][1] , Vertex[3][1] ],
      Propagator[Loop[1]][ Vertex[2,1][1] , Vertex[3][1] ],
      Propagator[Internal][ Vertex[1,1][1] , Vertex[3][1] ]    ],
define[Global`CT[1,2][4]] =  Topology[2][   
      Propagator[Loop[1]][ Vertex[3][1] , Vertex[3][1] ],
      Propagator[Internal][ Vertex[1,1][1] , Vertex[2,1][1] ],
      Propagator[Internal][ Vertex[2,1][1] , Vertex[3][1] ]    ],
define[Global`CT[1,2][5]] =  Topology[2][   
      Propagator[Loop[1]][ Vertex[2,1][1] , Vertex[2,1][2] ],
      Propagator[Loop[1]][ Vertex[2,1][1] , Vertex[2,1][2] ]    ],
define[Global`CT[1,2][6]] =  Topology[2][   
      Propagator[Loop[1]][ Vertex[3][1] , Vertex[3][1] ],
      Propagator[Internal][ Vertex[1,1][1] , Vertex[3][2] ],
      Propagator[Internal][ Vertex[1,1][2] , Vertex[3][2] ],
      Propagator[Internal][ Vertex[3][2] , Vertex[3][1] ]    ],
define[Global`CT[1,2][7]] =  Topology[2][   
      Propagator[Loop[1]][ Vertex[3,1][1] , Vertex[3,1][1] ],
      Propagator[Internal][ Vertex[1,1][1] , Vertex[3,1][1] ]    ],
define[Global`CT[1,2][8]] =  Topology[2][   
      Propagator[Loop[1]][ Vertex[3][-1] , Vertex[3][-2] ],
      Propagator[Loop[2]][ Vertex[3][-1] , Vertex[3][-2] ],
      Propagator[Internal][ Vertex[1,1][1] , Vertex[3][-1] ],
      Propagator[Internal][ Vertex[1,1][2] , Vertex[3][-2] ]    ],
define[Global`CT[1,2][9]] =  Topology[2][   
      Propagator[Loop[1]][ Vertex[4][-1] , Vertex[4][-1] ],
      Propagator[Internal][ Vertex[1,1][1] , Vertex[4][-1] ],
      Propagator[Internal][ Vertex[1,1][2] , Vertex[4][-1] ]    ]
];

StartTop[2, 0] = TopologyList[
define[Theta] = Topology[12][  
      Propagator[Loop[1]][ Vertex[3][-2] , Vertex[3][-1] ],
      Propagator[Loop[2]][ Vertex[3][-2] , Vertex[3][-1] ],
      Propagator[Loop[3]][ Vertex[3][-2] , Vertex[3][-1] ]    ],
define[Eight] = Topology[8][   
      Propagator[Loop[1]][ Vertex[4][-1] , Vertex[4][-1] ],
      Propagator[Loop[2]][ Vertex[4][-1] , Vertex[4][-1] ]   ],
define[Bicycle] = Topology[8][   
      Propagator[Loop[1]][ Vertex[3][-1] , Vertex[3][-1] ],
      Propagator[Loop[2]][ Vertex[3][-2] , Vertex[3][-2] ],
      Propagator[Internal][ Vertex[3][-2] , Vertex[3][-1] ]   ]  
];


StartTop[2,1] = TopologyList[
define[Global`CT[2,1][1]] = Topology[6][
      Propagator[Loop[1]][  Vertex[3][-1] , Vertex[3,1][-1] ],
      Propagator[Loop[2]][  Vertex[3][-1] , Vertex[3,1][-1] ],
      Propagator[Loop[3]][  Vertex[3][-1] , Vertex[3,1][-1] ]
     ], 
define[Global`CT[2,1][2]] = Topology[4][
      Propagator[Loop[1]][  Vertex[3][-1] , Vertex[3][-2] ],
      Propagator[Loop[2]][  Vertex[3][-1] , Vertex[3][-2] ],
      Propagator[Loop[3]][  Vertex[3][-1] , Vertex[2,1][1] ],
      Propagator[Loop[4]][  Vertex[3][-2] , Vertex[2,1][1] ]
     ], 
define[Global`CT[2,1][3]] = Topology[8][
      Propagator[Loop[1]][  Vertex[3][-1] , Vertex[3][-1] ],
      Propagator[Loop[2]][  Vertex[3][-2] , Vertex[3][-2] ],
      Propagator[Internal][ Vertex[3][-1] , Vertex[3][-3] ]  ,
      Propagator[Internal][ Vertex[3][-2] , Vertex[3][-3] ]  ,
      Propagator[Internal][ Vertex[3][-3] , Vertex[1,1][1] ]  
     ], 
define[Global`CT[2,1][4]] = Topology[4][
      Propagator[Loop[1]][  Vertex[4][1] , Vertex[4][1] ],
      Propagator[Loop[2]][  Vertex[4][1] , Vertex[2,1][1] ],
      Propagator[Loop[3]][  Vertex[4][1] , Vertex[2,1][1] ]
     ], 
define[Global`CT[2,1][5]] = Topology[8][
      Propagator[Loop[1]][  Vertex[4,1][1] , Vertex[4,1][1] ],
      Propagator[Loop[2]][  Vertex[4,1][1] , Vertex[4,1][1] ]
     ], 
define[Global`CT[2,1][6]] = Topology[6][
      Propagator[Loop[1]][  Vertex[3][-1] , Vertex[4][-1] ],
      Propagator[Loop[2]][  Vertex[3][-1] , Vertex[4][-1] ],
      Propagator[Loop[3]][  Vertex[3][-1] , Vertex[4][-1] ],
      Propagator[Internal][ Vertex[4][-1] , Vertex[1,1][1] ]  
     ], 
define[Global`CT[2,1][7]] = Topology[4][
      Propagator[Loop[1]][  Vertex[3][-1] , Vertex[3][-2] ],
      Propagator[Loop[2]][  Vertex[3][-1] , Vertex[3][-2] ],
      Propagator[Loop[3]][  Vertex[3][-1] , Vertex[3][-3] ],
      Propagator[Loop[4]][  Vertex[3][-2] , Vertex[3][-3] ],
      Propagator[Internal][ Vertex[3][-3] , Vertex[1,1][1] ]  
     ], 
define[Global`CT[2,1][8]] = Topology[4][
      Propagator[Loop[1]][  Vertex[4][-1] , Vertex[4][-1] ],
      Propagator[Loop[2]][  Vertex[4][-1] , Vertex[3][-1] ],
      Propagator[Loop[3]][  Vertex[4][-1] , Vertex[3][-1] ],
      Propagator[Internal][ Vertex[3][-1] , Vertex[1,1][1] ]  
     ], 
define[Global`CT[2,1][9]] = Topology[4][
      Propagator[Loop[1]][  Vertex[3][-1] , Vertex[3][-1] ],
      Propagator[Loop[2]][  Vertex[3][-1] , Vertex[3][-2] ],
      Propagator[Loop[3]][  Vertex[3][-2] , Vertex[3][-3] ],
      Propagator[Internal][ Vertex[3][-2] , Vertex[3][-3] ]  ,
      Propagator[Internal][ Vertex[3][-3] , Vertex[1,1][1] ]  
     ], 
define[Global`CT[2,1][10]] = Topology[8][
      Propagator[Loop[1]][  Vertex[3][-1] , Vertex[3][-1] ],
      Propagator[Loop[2]][  Vertex[3][-2] , Vertex[3][-2] ],
      Propagator[Internal][ Vertex[3][-1] , Vertex[2,1][1] ]  ,
      Propagator[Internal][ Vertex[3][-2] , Vertex[2,1][1] ]  
     ], 
define[Global`CT[2,1][11]] = Topology[4][
      Propagator[Loop[1]][  Vertex[2,1][1] , Vertex[3][-1] ],
      Propagator[Loop[2]][  Vertex[2,1][1] , Vertex[3][-1] ],
      Propagator[Loop[3]][  Vertex[3][-2] , Vertex[3][-2] ],
      Propagator[Internal][ Vertex[3][-1] , Vertex[3][-2] ]  
     ], 
define[Global`CT[2,1][12]] = Topology[4][
      Propagator[Loop[1]][  Vertex[3,1][1] , Vertex[3,1][1] ],
      Propagator[Loop[1]][  Vertex[3][1] , Vertex[3][1] ],
      Propagator[Internal][ Vertex[3,1][1] , Vertex[3][1] ]  
     ], 
define[Global`CT[2,1][13]] = Topology[4][
      Propagator[Loop[1]][  Vertex[3][1] , Vertex[3][1] ],
      Propagator[Loop[2]][  Vertex[3][1] , Vertex[4][1] ],
      Propagator[Internal][ Vertex[4][1] , Vertex[4][1] ],
      Propagator[Internal][ Vertex[4][1] , Vertex[1,1][1] ]  
     ],
define[Global`CT[2,1][14]] = Topology[8][
      Propagator[Loop[1]][  Vertex[5][1] , Vertex[5][1] ],
      Propagator[Loop[2]][  Vertex[5][1] , Vertex[5][1] ],
      Propagator[Internal][ Vertex[5][1] , Vertex[1,1][1] ]
     ]
];



StartTop[3, 0] = TopologyList[ 
                (* Mercedes - star: 4x3v *)
define[Three[1]] =  Topology[ 24][ 
      Propagator[Loop[1]][ Vertex[3][-1] , Vertex[3][-2] ],
      Propagator[Loop[2]][ Vertex[3][-2] , Vertex[3][-3] ],
      Propagator[Loop[3]][ Vertex[3][-3] , Vertex[3][-1] ],
      Propagator[Loop[6]][ Vertex[3][-1] , Vertex[3][-4] ],
      Propagator[Loop[5]][ Vertex[3][-2] , Vertex[3][-4] ],
      Propagator[Loop[4]][ Vertex[3][-3] , Vertex[3][-4] ]
                               ],
                 (* 2 Circles : 4x3v *)
define[Three[2]] =  Topology[ 16][ 
      Propagator[Loop[1]][ Vertex[3][-1] , Vertex[3][-2] ],
      Propagator[Loop[2]][ Vertex[3][-2] , Vertex[3][-3] ],
      Propagator[Loop[3]][ Vertex[3][-3] , Vertex[3][-4] ],
      Propagator[Loop[4]][ Vertex[3][-4] , Vertex[3][-1] ],
      Propagator[Loop[5]][ Vertex[3][-1] , Vertex[3][-4] ],
      Propagator[Loop[6]][ Vertex[3][-2] , Vertex[3][-3] ]
                               ],
                 (* 2 Circles in touch : 2x3v + 1x4v *)
define[Three[3]] =   Topology[ 8][ 
      Propagator[Loop[1]][ Vertex[3][-1] , Vertex[3][-2] ],
      Propagator[Loop[2]][ Vertex[3][-1] , Vertex[4][-1] ],
      Propagator[Loop[3]][ Vertex[3][-1] , Vertex[4][-1] ],
      Propagator[Loop[4]][ Vertex[3][-2] , Vertex[4][-1] ],
      Propagator[Loop[5]][ Vertex[3][-2] , Vertex[4][-1] ]
                               ],
                  (* Extended Theta (1): 2x3v + 1x4v *)
define[Three[4]] = Topology[ 8][
      Propagator[Loop[1]][ Vertex[3][-1] , Vertex[3][-2] ],
      Propagator[Loop[2]][ Vertex[3][-1] , Vertex[3][-2] ],
      Propagator[Loop[3]][ Vertex[3][-1] , Vertex[4][-1] ],
      Propagator[Loop[4]][ Vertex[3][-2] , Vertex[4][-1] ],
      Propagator[Loop[5]][ Vertex[4][-1] , Vertex[4][-1] ]
                               ],
                 (* Eye of the tiger: 2x4v *)
define[Three[5]] = Topology[  48][ 
      Propagator[Loop[1]][ Vertex[4][-1] , Vertex[4][-2] ],
      Propagator[Loop[2]][ Vertex[4][-1], Vertex[4][-2] ],
      Propagator[Loop[3]][ Vertex[4][-1], Vertex[4][-2] ],
      Propagator[Loop[4]][ Vertex[4][-1], Vertex[4][-2] ]
                               ],
                  (* 3 Circles: 2x4v *)
define[Three[6]] =   Topology[ 8][
      Propagator[Loop[1]][ Vertex[4][-1] , Vertex[4][-1] ],
      Propagator[Loop[2]][ Vertex[4][-1] , Vertex[4][-2] ],
      Propagator[Loop[3]][ Vertex[4][-1] , Vertex[4][-2] ],
      Propagator[Loop[4]][ Vertex[4][-2] , Vertex[4][-2] ]
                               ],
                  (* Extended Theta (2): 1x3v + 1x5v *)
define[Three[7]] =   Topology[ 12][
      Propagator[Loop[1]][ Vertex[3][-1] , Vertex[5][-2] ],
      Propagator[Loop[2]][ Vertex[3][-1] , Vertex[5][-2] ],
      Propagator[Loop[3]][ Vertex[3][-1] , Vertex[5][-2] ],
      Propagator[Loop[4]][ Vertex[5][-2] , Vertex[5][-2] ]
                               ],
                  (* 3 leaf Glover: 1x6v *)
define[Three[8]] = Topology[48][
      Propagator[Loop[1]][ Vertex[6][-1] , Vertex[6][-1] ],
      Propagator[Loop[2]][ Vertex[6][-1] , Vertex[6][-1] ],
      Propagator[Loop[3]][ Vertex[6][-1] , Vertex[6][-1] ]
                               ],
		  (* Theta+Tadpole on Vertex *)
define[ThreeRed[1]] = Topology[12][
      Propagator[Loop[1]][ Vertex[3][-1], Vertex[4][-1] ],
      Propagator[Loop[2]][ Vertex[3][-1], Vertex[4][-1] ],
      Propagator[Loop[3]][ Vertex[3][-1], Vertex[4][-1] ],
      Propagator[Internal][ Vertex[4][-1], Vertex[3][-2] ],
      Propagator[Loop[5]][ Vertex[3][-2], Vertex[3][-2] ]
                               ],
		  (* Theta+Tadpole on Propagator *)
define[ThreeRed[2]] = Topology[8][
      Propagator[Loop[1]][ Vertex[3][-1], Vertex[3][-2] ],
      Propagator[Loop[2]][ Vertex[3][-1], Vertex[3][-2] ],
      Propagator[Loop[3]][ Vertex[3][-1], Vertex[3][-3] ],
      Propagator[Loop[4]][ Vertex[3][-2], Vertex[3][-3] ],
      Propagator[Internal][ Vertex[3][-3], Vertex[3][-4] ],
      Propagator[Loop[5]][ Vertex[3][-4], Vertex[3][-4] ]
                               ],
                  (* 3 Tadpoles *)
define[ThreeRed[3]] = Topology[8][
      Propagator[Internal][ Vertex[3][-1], Vertex[3][-2] ],
      Propagator[Internal][ Vertex[3][-1], Vertex[3][-3] ],
      Propagator[Internal][ Vertex[3][-1], Vertex[3][-4] ],
      Propagator[Loop[1]][ Vertex[3][-2], Vertex[3][-2] ],
      Propagator[Loop[2]][ Vertex[3][-3], Vertex[3][-3] ],
      Propagator[Loop[3]][ Vertex[3][-4], Vertex[3][-4] ]
                               ],
                  (* Bicycle+Tadpole on Loop *)
define[ThreeRed[4]] = Topology[8][
      Propagator[Loop[1]][ Vertex[3][-1], Vertex[3][-1] ],
      Propagator[Internal][ Vertex[3][-1], Vertex[3][-2] ],
      Propagator[Loop[2]][ Vertex[3][-2], Vertex[3][-3] ],
      Propagator[Loop[3]][ Vertex[3][-2], Vertex[3][-3] ],
      Propagator[Internal][ Vertex[3][-3], Vertex[3][-4] ],
      Propagator[Loop[4]][ Vertex[3][-4], Vertex[3][-4] ]
                               ],
                  (* Bicycle+Tadpole on Vertex *)
define[ThreeRed[5]] = Topology[8][
      Propagator[Loop[1]][ Vertex[3][-1], Vertex[3][-1] ],
      Propagator[Internal][ Vertex[3][-1], Vertex[4][-1] ],
      Propagator[Loop[2]][ Vertex[4][-1], Vertex[4][-1] ],
      Propagator[Internal][ Vertex[4][-1], Vertex[3][-2] ],
      Propagator[Loop[3]][ Vertex[3][-2], Vertex[3][-2] ]
                               ],
                  (* Eight+Tadpole on Loop *)
define[ThreeRed[6]] = Topology[8][
      Propagator[Loop[1]][ Vertex[4][-1], Vertex[4][-1] ],
      Propagator[Loop[2]][ Vertex[4][-1], Vertex[3][-1] ],
      Propagator[Loop[3]][ Vertex[4][-1], Vertex[3][-1] ],
      Propagator[Internal][ Vertex[3][-1], Vertex[3][-2] ],
      Propagator[Loop[4]][ Vertex[3][-2], Vertex[3][-2] ]
                               ],
                  (* Eight+Tadpole on Vertex => v5 *)
define[ThreeRed[7]] = Topology[16][
      Propagator[Loop[1]][ Vertex[5][-1], Vertex[5][-1] ],
      Propagator[Loop[2]][ Vertex[5][-1], Vertex[5][-1] ],
      Propagator[Internal][ Vertex[5][-1], Vertex[3][-1] ],
      Propagator[Loop[3]][ Vertex[3][-1], Vertex[3][-1] ]
                               ]
];

(* 
  :Part2:
	Recursive creation of topologies. The internal function that does
	all the work is ConstructTopologies. It saves all values once 
	found. 
	The five arguments of ConstructTopologies are:
	number of loops - counterterm order - number of external legs 
        - maximum degree of vertices - starting topology.
	To stop the recursion CreateTopologies defines ConstructTopologies
	with the minimum number of legs. Usually something like:
        ConstructTopologies[l,c,Emin,Dmax,Tname] = TopologyList[define[name]].
	ConstructTopologies always generates ALL topologies with up to #e
	edges on any vertex. Those edge numbers that are to be omitted
	(e.g. Adjacencies->{4,6}) are sorted out by CreateTopologies,
*)

(* if the recursion failed to stop:
*)
ConstructTopologies[l_Integer, c_Integer, n_Integer?Negative,
                    e_Integer, s_
                   ] := $Undefined;

(* recursive generation of topologies:
*)
ConstructTopologies[ loops_Integer, cto_Integer, ext_Integer, 
                     max_Integer, st_                         
                   ] :=
ConstructTopologies[ loops, cto, ext, max, st                 
                   ] =
    TopologyList[ Map[ AddOne[#1,ext,max]& , 
		  ConstructTopologies[loops, cto, ext-1, max, st] ] ] ;

(* define ConstructTopologies with minimal number of external legs:
*)
SetRecursionEnd[ 
  loops_Integer, countord_Integer, maxdegree_Integer ][ top_ ] :=
Set[
    ConstructTopologies[ loops, 
                         countord, 
                         Count[ top, Propagator[External][__] ],
                         maxdegree,
                         top ],
    TopologyList[ top ]
   ];


CreateTopologies::nostart = 
  "no starting topologies defined for `1` loops and counterterm order `2`.";
CreateTopologies::badstart = 
  "starting topology `1` not defined.";
CreateTopologies::badopt1 = 
  "`1` is not a list of expressions.";
CreateTopologies::badopt2 = 
  "`1` is not a positive integer.";
CreateTopologies::badopt3 = 
  "`1` is either not a list or contains something that is not a positive 
integer > 2.";
CreateTopologies::delundef = 
  "delete function \"`1`\" not defined (doing nothing).";

(* main function supporting i->o input: 
*)
CreateTopologies[l_Integer, Rule[i_Integer,o_Integer] , options___ ] := 
    Map[  Sort,
	  tt=CreateTopologies[l,i+o,options] /. 
	          Propagator[External][ Vertex[1][j_],v2_ ] :>
	          If[ j>i, 
	              Propagator[Outgoing][ Vertex[1][j],v2 ],
	              Propagator[Incoming][ Vertex[1][j],v2 ]  ] /.
          {Incoming->AAA,Outgoing->AAB}, 2 ]  /.  {AAA->Incoming,AAB->Outgoing}

CreateTopologies[l_Integer, n_Integer , options___ ] := 
    Block[{opt = ActualOptions[CreateTopologies, options],
	   delete, start, cto, adj, 
           Emax, forbV, topos, starttops, badstarttops={}},
	   delete = ExcludeTopologies/.opt;
	   start = StartingTopologies/.opt;
           cto = CountertermOrder/.opt;
	   adj = Adjacencies/.opt;
	  (* checking options: *)
	   If[ !MatchQ[ adj, _List ] ||
	       !FreeQ[ MatchQ[ #, _Integer?Positive ]& /@ adj, False ] ||
	       !FreeQ[ adj, 1 ] || !FreeQ[ adj, 2 ],
	      Message[ CreateTopologies::badopt3, adj ];
	      Return[ $Aborted ]
	     ];
	   If[ (delete =!= None) && (Head[delete] =!= List),
	      Message[ CreateTopologies::badopt1, delete ];
	      Return[$Aborted] ];
	   If[ (start =!= All) && (Head[start] =!= List),
	      Message[ CreateTopologies::badopt1, start ];
	      Return[$Aborted]];
	   If[ !MatchQ[ cto, _Integer?Positive|0 ],
	      Message[ CreateTopologies::badopt2, nrtwo ];
	      Return[$Aborted]
	     ];
         (* check whether starting topologies are defined: *)
           If[ Head[Evaluate[StartTop[l, cto]]]===StartTop,
               Message[CreateTopologies::nostart, l, cto ];
               Return[$Aborted]
             ]; 
         (* set maximum degree of vertices: *)
	   Emax = Max[adj];
	 (* generate list of "forbidden vertices": *)
	   forbV = Complement[ Table[i, {i, 3, Emax}], adj ];
         (* search starting topologies: *)
	   If[ start === All,
               starttops = StartTop[l,cto],
               starttops =  define/@ start;
             (* undefined symbols in start? *)
               badstarttops = Select[ starttops, MatchQ[ #, define[_] ]& ];
	       If[ badstarttops =!= {},
	           Message[CreateTopologies::badstart,#[[1]] 
                          ]& /@ badstarttops;
	           Return[$Aborted] 
                 ]
	     ];
         (* define the end of the recursion: *)
           SetRecursionEnd[ l, cto, Emax ] /@ starttops; 
         (* call function: *)
	   topos = TopologyList@@(
               ConstructTopologies[l,cto,n,Emax,#]& /@ starttops );
	 (* define and apply "master" delete function *)
           master[x_] := And@@( FreeQ[x, #]& /@ ( Vertex[#][_]& /@ forbV ) );
	   If[ forbV =!= {},
	       topos = Select[ topos, master ]
	     ];
           (* apply delete functions: *)
	   If[ delete =!= None,
	      If[ MemberQ[delete, AllBoxes],
		 delete = Flatten[ 
			   delete /. AllBoxes->Array[Boxes,n-3,4] ] ];
	      (* for non-sophisticated users: *)
	      delete = Union[ delete/.{Boxes[1]->Tadpoles,
				       Boxes[2]->SelfEnergies,
				       Boxes[3]->Triangles} ];
	      topos = Fold[ $ExcludeTopologies, topos, delete ] ];
	   Return[topos]
	  ];

CreateCTTopologies::abort =
  "Error in CreateTopologies for `1` loops and ct-order `2`."

CreateCTTopologies[ c_Integer, e:(Integer|Rule[_Integer,_Integer]),
                    opt___ ] :=
Block[ { co = ActualOptions[ CreateCTTopologies, CreateTopologies, opt ],
         to = ActualOptions[ CreateTopologies, CreateCTTopologies, opt ],
         tops = TopologyList[] , loop=0, stop=False},
        to = Select[ to, FreeQ[#, CountertermOrder ]& ];
        While[ (loop < c) && !stop, 
           AppendTo[ tops,
            CreateTopologies[ loop, e, CountertermOrder->(c-loop), 
                              Sequence@@ to ] ];
           If[ Last[tops]===$Aborted,
               Message[CreateCTTopologies::abort, loop, c-loop];
               If[ StopOnError /. co,
                   stop = True,
                   tops = tops /. $Aborted->
                       "-- " <> ToString[loop]  <> " loops / ct-order: " <>
                       ToString[c-loop] <> " --"
                 ] 
             ];
           loop++
          ];
        If[ stop, Return[$Aborted], Return[ tops ] ]
     ];
 
(* 
  :Part3:
	Definition of the delete function:
        For every member of the ExcludeTopologies-List there must exist
        a function $ExcludeTopologies[member][toplist] that deletes some
        topologies otherwise an error will occur. In addition we have to 
	define some functions to test topologies for special properties.
*)

(* for the application of "Fold" : 
*)
$ExcludeTopologies[t_, fun_] := $ExcludeTopologies[fun][t];

(* default: error-message 
*)
$ExcludeTopologies[trash_][top_] := (
  Message[CreateTopologies::delundef, trash]; top );

(* definition of the $ExcludeTopologies functions : 
   Most of them are defined in terms of other functions which are defined
   below.
*)
$ExcludeTopologies[Tadpoles][Global`t_] := NoTadpoles[Global`t];
$ExcludeTopologies[WFCorrections][Global`t_] := NoWFCorrections[Global`t];
$ExcludeTopologies[SelfEnergies][Global`t_] := NoSelfEnergies[Global`t];
$ExcludeTopologies[Triangles][Global`t_] := NoTriangles[Global`t];
$ExcludeTopologies[Boxes[m_]][Global`t_] := NoBoxes[Global`t,m];

(* additional definitions by hagen:
   (This are One-Liners)
*)
$ExcludeTopologies[Internal][Global`t_] := 
 Select[ Global`t, FreeQ[#, Internal]& ];

$ExcludeTopologies[Global`CTTad][Global`t_] :=
  Select[ Global`t, FreeQ[ #, Vertex[1, cto_Integer?Positive][_] ]& ];

$ExcludeTopologies[Global`ExtCTSE][Global`t_] :=
  Select[ Global`t,
          FreeQ[ #, Propagator[External][Vertex[1][_], Vertex[2,_][_]]
               ]&  ];

(* test: is the number of "Loop"-type propagators === i ? 
*)
OnLoopTest[ tt:Topology[_][___],i_Integer ] := 
 Block[{lprops},
  lprops = AtLoopPropagators[ tt ];
  Length[  Select[ lprops , 
		   !(FreeQ[#,External]&&FreeQ[#,Internal])& ]  ] === i
 ];

(* test: is there a self energy loop on an external leg ?
   there may be only one External prop, it's a vertex correction otherwise.
*)
ExternalSelfEnergy[ tt:Topology[_][___] ] := 
 Block[{lprops},
  lprops = AtLoopPropagators[ tt ];
  Length[Select[lprops, !(FreeQ[#,External]&&FreeQ[#,Internal])&]] === 2 &&
  Length[Select[lprops, !(FreeQ[#,External])&]] === 1 
 ]; 

(* test: is there a tadpole on an external leg ?
*)
ExternalTadpole[ tt:Topology[_][___] ] :=
 Block[{lprops,inprop,base},
    lprops = AtLoopPropagators[ tt ];
    inprop =  Identity @@ Select[ lprops , !(FreeQ[#,Internal])& ];
    If[ inprop==={},Return[True] ] ;    (* only non-l propagator is external *)
    base = Union[ Select[ tt, !FreeQ[#,inprop[[1]]]& ],
                   Select[ tt, !FreeQ[#,inprop[[2]]]& ]  ];
    Length[ Select[base, !FreeQ[#,External]&] ] === 1
 ];

(* selecting the loop propagators: all props of type Loop[_] and those
   that hit vertices that these are joining
*)
AtLoopPropagators[ tt:Topology[_][___] ] :=
 Block[{lvertices},
    lvertices = 
    Union[ Join @@ 
       (Select[  tt , ( FreeQ[#,Internal] && FreeQ[#,External] )&  ] /.
			              Propagator[_] -> List)
        ];
    Union @@ (Function[ z,Select[ tt , !FreeQ[#,z]& ] ] /@ lvertices)
 ];
    
(* delete functions:
*)
(* The next function is a nice example of the fact, that things usually don't
   work the way they`re supposed to.
   The first version of this function was:
        PossibleTads[ x:Topology[_][__] ] :=
                Cases[ x, Propagator[_][v_, v_] -> v ];
   This should return a list of those vertices that appear twice in a 
   propagator. This worked fine, as long as no model (!) was initialized.
   After initialization, this function returned {h1, h2, h3, h4}  !!!
   Of course, there MUST be a reason for this, but I didn't bother to
   find out.
   The new version is a less elegant construction, doing the same as the
   Cases... should have done.
*)
PossibleTads[ x:Topology[_][__] ] :=
Block[ {ret = {} },
      Do[
         If[ x[[i,1]] == x[[i,2]],
             AppendTo[ ret, x[[i,1]] ]
           ],
         {i, Length[x]}
        ];
      Return[ ret ]
     ];
RealTad[ x:Topology[_][__], v:Vertex[_][_] ] :=
     Length[ Select[ x, !FreeQ[ #, v ]& ] ] == 2;
TadpoleTest[ x:Topology[_][__] ] :=
     !FreeQ[ RealTad[ x, # ]& /@ PossibleTads[x], True ] ;

NoTadpoles[ tt:TopologyList[_] ] := 
	    Select[ tt , !TadpoleTest[#]& ];

NoSelfEnergies[ tt:TopologyList[_]  ] := 
	        Select[ tt , Not[OnLoopTest[#,2 ]]& ];
NoTriangles[ tt:TopologyList[_]  ] := 
	    Select[ tt , Not[OnLoopTest[#,3 ]]& ];
NoBoxes[ tt:TopologyList[_] , i_Integer  ] := 
	    Select[ tt , Not[OnLoopTest[#,i ]]& ];
NoWFCorrections[ tt:TopologyList[_]  ] := 
    Union[ NoTadpoles[NoSelfEnergies[ tt ]],
           Select[ Select[ tt,OnLoopTest[#,2]& ],
                  !ExternalSelfEnergy[#]&      ],
           Select[ Select[ tt,OnLoopTest[#,1]& ],
                  !ExternalTadpole[#]&         ]
         ];

(* 
  Part4:
       Doing most of the work.
*)

(* For permutations:
*)
SwapVertices[ x_ , old_ , new_ ] := x /. Thread[ old -> new ];


(* adding to one propagator 
*)
PropPlus[ Propagator[h_][f_,t_],n_Integer] :=      
 If[ !(h===External),                         
   Topology[h][ Propagator[h][f,Vertex[3][n]] , 
		Propagator[h][Vertex[3][n],t] , 
		Propagator[External][Vertex[1][n],Vertex[3][n]] ],
   Topology[h][ Propagator[h][f,Vertex[3][n]] , 
		Propagator[Internal][Vertex[3][n],t] , 
		Propagator[External][Vertex[1][n],Vertex[3][n]] ]
   ];

(* conserve and destroy information about the last addition 
*)
ConstructAdditionalInfor = 
  Topology[s_][ p1___,Topology[sp_][arg__],p2___ ] :> 
  Topology[ s sp ][p1,arg,p2];

DeleteAdditionalInfor = 
  Topology[ (s_Integer:1)(v_:Vertex) ][p__] :> 
  Topology[s][p];

(* adding propagator #n to propagator #m 
*) 
AddPropagator[ Topology[s_][ x:Propagator[___][__].. ],
	       n_Integer,m_Integer                      ] :=
   (Topology[s][x] /. 
     Topology[s][x][[m]] -> 
       PropPlus[Topology[s][x][[m]],n ]
    )/. ConstructAdditionalInfor;
   
(* adding propagator #n to {e/c}-vertex #m 
*) 
AddV[{e_Integer,c___Integer}][ 
    Topology[s_][ x:Propagator[___][__].. ], n_Integer,m_Integer ] :=
  Block[ {return,ch},
        return=
	Topology[s Vertex][x , 
            Propagator[External][Vertex[1][n], Vertex[e+1,c][n]]] /.
				  Vertex[e,c][m]:>Vertex[e+1,c][n];
        If[ m<0 , return = (Topology[s vc]) @@ return ];
        return=return/.ConstructAdditionalInfor;
        ch=Vertices[{-e,c}][ Topology[x] ];
        If[ Length[ch]>1,
           return=
	   SwapVertices[return, 
	     Vertices[{-e,c}][return],
             Table[Vertex[e,c][-i],{i,Length[Vertices[{-e,c}][return]]}] ],
           return
          ]
      ];

(* adding propagator #n to all other propagators 
*)
AddPropagator[ Topology[s_][ x:Propagator[___][__].. ] , n_Integer ] :=
       Apply[ TopologyList, 
              Array[ AddPropagator[Topology[s][ x ],n,#1]&,Length[{x}] 
		   ]
            ];

(* adding propagator #n to all vertices of type e = {ext,cto}
*)
AddV[e_][ Topology[s_][ x:Propagator[___][__].. ] , n_Integer ] :=
       Apply[ TopologyList,
              Map[ AddV[e][ Topology[s][ x ],n,#1]& , 
		   (Sequence@@#)& /@ Vertices[e][ Topology[x] ] ]
            ];

(* putting it all together:
*)
AddOne[ t:Topology[s_][__], n_Integer, e_Integer ] :=
  Block[{a,b},
	a=TopologiesCompare[ AddPropagator[ t , n ] ]   ;
        b=TopologiesCompare[ AddV[#][ t , n ] ]& /@ VsForAppend[t,e]; 
	TopologyList[ a, TopologyList@@b  ]
       ] ;

(* utility function: types of vertices (and ct's) to append to:
*)
VsForAppend[ t:Topology[s_][__], emax_Integer ] :=
Block[ {vs},
      vs = Union[ Head /@ Vertices[t] ];
      vs = Select[ vs, ( #[[1]] < emax )& ];
      vs = Select[ vs, ( ( # =!= Vertex[1] ) && ( # =!= Vertex[1,0] ) )& ]; 
      vs = vs /. Vertex->List;
      Return[vs]
     ];

(*
  Part5:
       Compare topologies (using HighEnergyPhysics`Utilities`Compare)
*)

TopologiesCompare[ tt:TopologyList[],mod___ ] := TopologyList[];

TopologiesCompare[ tt:TopologyList[__] ] :=  
    Block[{notequal,posequal},
          If[ ( Count[ tt[[1]], Propagator[Loop[_]][__] ] > 1 )||
              ( !FreeQ[ tt[[1]], Vertex[_,_][_] ] ), 
	     check = Function[ z ,(Head[ Head[z][[1]] ] === 1 )
                               ||(Head[ Head[z][[1]] ] === Symbol)
                               ||(MemberQ[ Head[z][[1]] , External ] )
                               ||(MemberQ[ Head[z][[1]] , Vertex ] )],
	     check = Function[ z ,(Head[ Head[z][[1]] ] === 1 )
                               ||(Head[ Head[z][[1]] ] === Symbol)
                               ||(MemberQ[ Head[z][[1]] , Internal ] )
                               ||(MemberQ[ Head[z][[1]] , External ] )
                               ||(MemberQ[ Head[z][[1]] , Vertex ] )]
            ];
          notequal = Select[ tt , check ];
          posequal = Select[ tt , !MemberQ[notequal,#]& ];
	  posequal = Compare[ posequal ] //.DeleteAdditionalInfor;
          notequal = notequal //.DeleteAdditionalInfor;
          Append[notequal,posequal]
         ];  

End[]; (* HighEnergyPhysics`FeynArts`Topology` *)

