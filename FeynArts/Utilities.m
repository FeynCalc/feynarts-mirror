
(* :Title: Utilities *)

(* :Authors: Hagen Eck, Sepp Kueblbeck *)

(* :Summary: Utility Routines for FeynArts`-functions *)

(* :Context: HighEnergyPhysics`FeynArts` *)

(* :Package Version 2.1 *)

(* :Mathematica Version 2.0 *)

(* :Summary:
	Collection of functions that are used for different parts of the
	FeynArts` packages.
*)

(* :Contents:
	Part1: Option handling, patterns and conversion functions
	       FreeListQ, MemberListQ
	Part2: Extract information from graphs
	Part3: FieldPoint construction
	Part4: Index handling functions
	Part5: Standard ordering
	Part6: Compare
	Part7: Alph, UCAlph, Greek, UCGreek
	(search for Part# to find beginning of a section)
*)

FAPrint[3,"[Utilities.m]"];

Begin["HighEnergyPhysics`FeynArts`Utilites`"];

(*
 * Part1:
 *        Option handling: ActualOptions
 *        Conversion functions: ToGeneric, ToClasses
 *)

ActualOptions::noopt =
"Warning: \"`2`\" is not a valid Option for symbol \"`1`\" (ignoring).";

ActualOptions[ command_Symbol, opts___Rule ] :=
    ActualOptions[ command, None, True, opts ];

ActualOptions[ command_Symbol, warn:(True|False), opts___Rule ] :=
    ActualOptions[ command, None, True, opts ];

ActualOptions[ command_Symbol, friend_Symbol, opts___Rule ] :=
    ActualOptions[ command, friend, True, opts ];

ActualOptions[ command_Symbol, friend_Symbol, warn:(True|False), 
	       opts___Rule ] :=
 Block[{ commandoptions = { FilterOptions[command, opts] },
	    keywords, otheroptions, friendoptions },
        keywords = First /@ commandoptions;
	otheroptions = Select[ {opts}, ! MemberQ[ keywords, First[#] ]& ];
	If[ friend=!=None,
	    friendoptions = First/@ {FilterOptions[friend, opts] };
	    otheroptions = 
	      Select[otheroptions, FreeQ[friendoptions, First[#]]& ]
	  ];
	If[ warn,
	    Message[ ActualOptions::noopt, command, First[#] 
		   ]& /@ otheroptions
	  ];
	Union[ commandoptions,
		 Select[ Options[command],
			 ! MemberQ[ keywords, First[#] ]& ] ]
      ];

(* Conversion Functions:
 *)

(* generic fields do NOT have any signs (attribute SelfConjugate exists
 * only only classes level:
 *)
ToGeneric[ expr_ ] := 
    expr /. { _. (f:P$Generic)[ __ ] :> f
            } /. f:(-P$Generic) :> -f;

ToClasses[ expr_ ] := 
    expr /. { s_. (f:P$Generic)[ i_Integer,  __  ] :> s f[i]  ,
              s_. (f:P$Generic)[ i_Index,  __  ] :> s f[i]     };

(* FreeListQ, MemberListQ.
 *)

(* FreeListQ[ expr_, form_List ] tests whether expr is free of all of the 
 * elements of form.
 *)
FreeListQ[ expr_, form_List, add___ ] := 
  And@@ ( FreeQ[expr, #, add]& /@ form );

(* MemberListQ[ expr_, form_List ] tests whether any of the elements of form
 * is an element of expr.
 *)
MemberListQ[ expr_, form_List, add___ ] := 
  Or@@ ( MemberQ[expr, #, add]& /@ form );


(*
  Part2:
	Extract information from a topology e.g. vertices of a special type,
	number of propagators etc. Furthermore this part contains the 
	function PSort, VSort and TSort for standard ordering.
*)
(* extract vertices (HighEnergyPhysics`FeynArts`Vertices`): 
*)

(* leave only Vertex-information: *)
StripTop[ t_Topology ] := 
	 List@@ ( (Sequence@@ Take[#1, 2])& /@ t );
(* which types of Vertices are present? *)
VertexTypes[ Topology[_][pr__] ] := VertexTypes[ Topology[pr] ];
VertexTypes[ t_Topology ] := 
  Union[ StripTop[t] /. Vertex[nrl_,___][_] :> nrl ];
(* acts only on Topology: *)
Vertices[ Topology[_][pr__] ] := Vertices[ Topology[pr] ];
Vertices[a_][ Topology[_][pr__] ] := Vertices[a][ Topology[pr] ];
(* all vertices: *)
Vertices[ t_Topology ] := Union[ StripTop[ t ] ];
(* all permutables: *)
Vertices[-1][ t_Topology ] := Select[Vertices[t], (#[[1]]<0)& ];
(* all vertices with n edges: *)
Vertices[ n_Integer?Positive ][ t_Topology ] :=
  Union[ Select[ StripTop[t], (#[[0,1]] === n )& ] ];
(* all permutable vertices with n edges: *)
Vertices[n_Integer?Negative][t_] := Select[Vertices[-n][t], (#[[1]]<0)& ];
(* possible calls {ext} and {ext,cto} *)
(* remark: ext can be negative, too -> MatchQ[_, .. should do it *)
Vertices[ {ext_} ][ t_Topology ] := Vertices[ext][t];
Vertices[ {ext_,cto_} ][ t_Topology ] :=
   Select[ Vertices[ext][t], MatchQ[ #[[0]], Vertex[_,cto] ]& ];
(* two special abbreviations: *)
NoOne[l_List] := If[ Sort[l][[1]] === 1, Drop[ l, 1 ], l ];
Vertices[I][ t_Topology ] := 
  Vertices[ {#} ][t]& /@ NoOne[ VertexTypes[t] ];
(* this is for Compare: we need internal CT's, too *)
Vertices[-I][ t_Topology ] := 
Append[
  Vertices[ {-#} ][t]& /@ NoOne[ VertexTypes[t] ],
  Select[ Vertices[-1][t], !MatchQ[#,Vertex[1][_]]& ]
 ];

(* count propagators of a special type 
   (HighEnergyPhysics`FeynArts`PropagatorCount`): 
*)
PropagatorCount[t_][Topology[_][p__]] := PropagatorCount[t][Topology[p]];
PropagatorCount[t_][top_Topology] := 
  Length[Cases[top, Propagator[t][__]]];
PropagatorCount[Loop][top_Topology] := 
  Length[Cases[top, Propagator[Loop[_]][__]]];

(* Vertex construction (FindFieldPoints)
*)
(* get the right Lorentz index 
*)
  pOne[ s_. fi_[ ind__, in1_->in2_ ] ] := s fi[ind,in1]
  pTwo[ s_. fi_[ ind__, in1_->in2_ ] ] := s fi[ind,in2]
  pOne[ x_ ] := x
  pTwo[ x_ ] := x
(* get particle which is incoming in vertex v from propagator pr 
*)
  TakeInc[ v_, pr_[ _ ,_ ,_        ] ] := 
	  Sequence[ ];
  TakeInc[ v_, pr_[ v_,_ ,p_[in__] ] ] := 
	  Sequence[ pOne[ AntiParticle[p[in]] ] ];
  TakeInc[ v_, pr_[ _ ,v_,p_[in__] ] ] := 
	  Sequence[ pTwo[ p[in] ] ];
  TakeInc[ v_, pr_[ v_,v_,p_[in__] ] ] := 
	  Sequence[ pOne[ AntiParticle[p[in]] ], pTwo[ p[in] ] ];

FindFieldPoints[ props___,i_Integer ]:=
        { FieldPoint[If[Length[props[[i,1,0]]]>1,props[[i,1,0,2]],0]] @@ ( 
            TakeInc[props[[i,1]],#]& /@ props ),
          FieldPoint[If[Length[props[[i,2,0]]]>1,props[[i,2,0,2]],0]] @@ ( 
            TakeInc[props[[i,2]],#]& /@ props ) };


(* 
 * Part3:
 *     Vertex construction. For CreateFeynAmp the fermions in a vertex 
 *     must be put in the right place:
 *     Sort propagators so that the propagator with the Vertexpoint
 *     as first argument is put on the first place, the one with
 *     the Vertexpoint as second argument is put on the second place.
 *
 *     Remark: we change the function ConstructFieldPoints such that it 
 *     returns the vertices in with generic fields sorted according to
 *     the generic couplings. This enables us, to apply Analytical-
 *     Propagator without further sorting. The fermion fields in a
 *     vertex are handled before the application of AnalyticalPropagator
 *     by using the FPositionRules.
 *)
  SortRule1[vv_] =  
   {q1___, p1:Propagator[_][_, vv, s_. F[__] ], q2___} :> { p1, q1, q2 };
  SortRule2[vv_] =  
   {q1___, p1:Propagator[_][vv, _, s_. F[__] ], q2___} :> { p1, q1, q2 };

  ConstVert[ vv_ , wl_List ] :=
    Unsorted[ vv ,
  (* NEW: *) SortGeneric[
              Flatten[ TakeInc[ vv , # ]& /@ ( Select[ wl, !FreeQ[#,vv]& ] /.
					   SortRule1[vv] /. SortRule2[vv] ),
                      1 ]
             ]
	    ];

ConstructFieldPoints[ tt:Topology[_][___] ] :=
  Block[{worklist, fps, outlist},
	worklist = Flatten[ tt/.{Topology[_]->List}];
(*
DePrint[" CFP, working with ",worklist ];
*)
        fps = Union[ Select[ Vertices[ Topology @@ worklist ],
                     !MatchQ[#, (Vertex[1][_]|Vertex[1,0][_])]& ] ];
(*
DePrint[" CFP, constructs ", InputForm[fps] ];
*)
	outlist = ConstVert[ # , worklist ]& /@ fps;
(*
Print["CFP, outlist = ",outlist];
*)
	Return[ outlist ]
       ];


(*
  Part4:
	Index Handling.
*)

(* For permutations:
*)
SwapVertices[ x_ , old_ , new_ ] := x /. Thread[ old -> new ];

(* The IndexDelta function keeps track of diagonal indices in a coupling.
   Its `mother function' Diagonal is set during the initialization for 
   every halfgeneric coupling with the function InitClassesCoupling.
   We have to protect IndexDelta from being defined like 
   IndexDelta[ i_Integer, j_Integer ] = If[ i==j, 1, 0 ], since this would
   destroy the function ProvideIndices.
*)
SetAttributes[ IndexDelta, Orderless ];
Off[SetDelayed::write];
IndexDelta[ a_, b_, c_, d_ ] := IndexDelta[ a, b, c ] IndexDelta[ c, d ];
IndexDelta[ a_, b_, c_ ] := IndexDelta[ a, b ] IndexDelta[ b, c ];
IndexDelta /: Power[ IndexDelta[a_,b_], i_Integer ] := IndexDelta[a,b];
IndexDelta[ n_?NumberQ, n_?NumberQ ] := 1;
SetAttributes[ IndexDelta, Protected ];
On[SetDelayed::write];

(* Diagonal: returns IndexDelta('s) or 1 for all Classes couplings.
   For mixed Generic/Classes couplings we set Diagonal to 1:
*)
Diagonal[ v:FieldPoint[_][__] ] := 1 /; 
		   !FreeQ[ MatchQ[ #, _. _Symbol ]& /@ v, True ];

(* Check whether expr contains indices of field. 
*)
IndexFreeQ[ expr_, field_ ] := 
  And@@ ( FreeQ[ expr, #]& /@  Indices[ field ] );
 

(*
  Part5:
	Standard ordering.
*)
(* Sort propagators: with the option AntiParticle->Identity the field 
   remains unchanged 
*)
PSort::toomuch =
"Propagator has `1` arguments; only 4 are allowed.";

PSort[ pr:Propagator[type_][f_,t_], opt___Rule ] := Sort[pr] /. {opt}; 

PSort[ pr:Propagator[type_][f_,t_,p_], opt___Rule ] :=
	If[ !OrderedQ[{f,t}],
	    Propagator[type][ t ,f ,AntiParticle[p] ],
	    If[  f===t,
		 Propagator[type][ f,f,Sort[{p,AntiParticle[p]}][[1]] ],
		 Propagator[type][ f,t,p ]
              ]
          ] /. {opt};

PSort[ pr:Propagator[type_][f_,t_,p_,r_], opt___Rule ] :=
	If[ !OrderedQ[{f,t}],
	    Propagator[type][ t ,f ,AntiParticle[p], Expand[-r] ],
	    If[  f===t,
                 ReleaseHold[
		 Propagator[type][ f,f,
                     If[ !OrderedQ[{p,AntiParticle[p]}],
                         Hold[ Sequence[ AntiParticle[p], Expand[-r] ] ],
                         Hold[ Sequence[ p, r ] ]
                       ] 
                 ]],
		 Propagator[type][ f,t,p,r ]
              ]
          ] /. {opt};

PSort[ Propagator[type_][ a_, b_, c_, d_, e__ ] ] :=
   Message[ PSort::toomuch, Length[{a,b,c,d,e}] ];

PSort[ Propagator[x__], opt___Rule ] := PSort[ Propagator[True][x], opt ];

(* Sort vertices without respect to their momentum: 
*)
VSort[ h_[ parts__ ] ] :=
 Transpose[ Sort[ Array[ { {parts}[[#]] /. sign_. f_[i_,___] :> sign f[i],
		           {parts}[[#]]
	                 }&,
	                 Length[{parts}]
                       ]
                ]
          ][[2]];

(* Sort topologies: 
*)
TSort[ t:Topology[s_][ p__ ], opt___Rule ] :=
     Sort[ PSort[ #, opt]& /@ ( t /. {
	   Incoming->a[1],Outgoing->b[1],External->c[1],Internal->d[1]} )
         ]/.{a[1]->Incoming,b[1]->Outgoing,c[1]->External,d[1]->Internal
	    }/.{opt} ;
TSort[ Topology[ p__ ], opt___Rule ] :=
     Topology@@ TSort[Topology[1][p], opt]


(* 
  Part6:
	Compare.
        The functions for comparing pure and inserted topologies. We first
	extract all sets of permutable vertices (2-, 3-, 4-vertices with
	negative numbers at the moment) and then recursively perform all the
	permutations.
*)
(* Comparable form: 
*)
CompForm[ t:Topology[___][__] ] := 
	  Topology@@Sort[ PSort[Propagator@@#1]& /@ t]  

(* Select groups of permutables:
*)
MoreThanTwo[ v_List ] :=
  Sequence@@ Select[ v, (Length[#] > 1)& ];

(* Basic permutation:
*)
VPermutation[ vlist_List ] := 
  (Head[#]/@ List@@#)& /@
  Permutations[ Head[vlist[[1]]]@@ ( (Sequence@@#)& /@ vlist ) ];

(* Recursive permutation of vertex groups: 
*)
DoPermute[ t_ ] := Flatten[ List[t] ];
DoPermute[ t_, per__ ] :=
  DoPermute[ Flatten[ SwapVertices[ {t}, {per}[[1]], # ]& /@
		      VPermutation[ {per}[[1]] ]
		    ],
	     Sequence@@ (Drop[ {per}, 1 ])
	   ];

(* Extract groups of permutable vertices and replace the topology with
   a list of equivalent topologies:
*)
(* TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST TEST *)
(*
DoCompare[ t:Topology[___][__] , tt_TopologyList ] :=
  ListCompare[ DoPermute[ t,  MoreThanTwo[{Vertices[-2][t], 
			      Vertices[-3][t], Vertices[-4][t]}] 
			],
	       tt
	      ];
*)
DoCompare[ t:Topology[___][__] , tt_TopologyList ] :=
  ListCompare[ DoPermute[t,  
                MoreThanTwo[ Vertices[-I][t] ]
             ], tt ];

(* Compare a list of equivalent topologies with a TopologyList:
*)
ListCompare[ equ_List, tt_TopologyList ] :=
    SingleListCompare[ equ, # ]& /@ (List@@tt);

(* Compare a list of equivalent topologies with one topology:
*)
SingleListCompare[ equ_List, t:Topology[___][__] ] :=
   MemberQ[ (CompForm[t]===#)& /@ (CompForm/@equ),
	    True
	  ];

(* Extract the information: 
*)
comp[ True, t:Topology[s_][x__] ] := trash;
comp[ False , t:Topology[s_][x__] ] := t ; 
comp[ n_Integer , t:Topology[s_][x__] ] := 
Block[ {new=Head[t][[1]]},
      new=If[Length[new]>1,
            Select[ new , (Head[#]===Integer)& ],
            new ];
      new=new/n;
      Topology[new]@@t  
];

(* Replace the first True in a list by the number of True's: 
*)
truesum[{a___,True,b___}]:= {a, Length[ Cases[ {a,True,b},True ] ] ,b}

(* Apply the "comp"-function: 
*)
compmap[ t:Topology[___][__] , tt_TopologyList , m_Integer ] :=
 (comp@@ #1)& /@ 
  Transpose[ { Join[ Table[False,{m-1} ] ,
                     truesum[DoCompare[t,Drop[tt,m-1]]]] , 
                List@@tt } 
	    ]; 

(* Main Compare-routine:
*)
Compare[ TopologyList[] ] := TopologyList[];
Compare[ tt:TopologyList[__] ] := 
Block[ {nn=1,topol},
      topol = tt ;
      While[ nn < Length[ topol ] ,
             topol=Select[ TopologyList@@(compmap[ topol[[nn]], topol,nn ]),  
			   (!(#===trash))& ];
             nn++
           ];
       Return[topol]
];


(* 
  :Part7:
	Alph, UCAlph, Greek, UCGreek: mappings of numbers to strings.
*)

NumberToString[ func_Symbol, head_Symbol, { i_Integer, s_String } ] :=
   Set[ func[i], head[s] ];

Alph::badnum = "`1` is not an integer of range 1..26.";
UCAlph::badnum = "`1` is not an integer of range 1..26.";
Greek::badnum = "`1` is not an integer of range 1..26.";
UCGreek::badnum = "`1` is not an integer of range 1..23.";

Alph[ n_?NumberQ ] := Message[ Alph::badnum, n ];
UCAlph[ n_?NumberQ ] := Message[ UCAlph::badnum, n ];
Greek[ n_?NumberQ ] := Message[ Greek::badnum, n ];
UCGreek[ n_?NumberQ ] := Message[ UCGreek::badnum, n ];

(* DEFAULT:
   The following four lists define the positions of greek and latin letters
   for the functions Alph, UCAlph, Greek and UCGreek. the can be changed by
   the user.
*)
NumberToString[ Alph, Identity, # ]& /@ {
{1,"a"},{2,"b"},{3,"c"},{4,"d"},{5,"e"},{6,"f"},{7,"g"},{8,"h"},{9,"i"},
{10,"j"},{11,"k"},{12,"l"},{13,"m"},{14,"n"},{15,"o"},{16,"p"},{17,"q"},
{18,"r"},{19,"s"},{20,"t"},{21,"u"},{22,"v"},{23,"w"},{24,"x"},{25,"y"},
{26,"z"}};

NumberToString[ UCAlph, Identity, # ]& /@ {
{1,"A"},{2,"B"},{3,"C"},{4,"D"},{5,"E"},{6,"F"},{7,"G"},{8,"H"},{9,"I"},
{10,"J"},{11,"K"},{12,"L"},{13,"M"},{14,"N"},{15,"O"},{16,"P"},{17,"Q"},
{18,"R"},{19,"S"},{20,"T"},{21,"U"},{22,"V"},{23,"W"},{24,"X"},{25,"Y"},
{26,"Z"}};

NumberToString[ Greek, SymbolChar, # ]& /@ {
{1,"alpha"}, {2,"beta"}, {3,"gamma"}, {4,"delta"}, {5,"chi"}, {6,"epsilon"}, 
{7,"varphi"}, {8,"eta"}, {9,"iota"}, {10,"phi"}, {11,"mu"}, {12,"nu"}, 
{13,"rho"}, {14,"sigma"}, {15,"tau"}, {16,"kappa"}, {17,"lambda"}, {18,"xi"}, 
{19,"zeta"}, {20,"pi"}, {21,"vartheta"}, {22,"upsilon"}, {23,"omega"}, 
{24,"omicron"}, {25,"psi"}, {26,"theta"} };

NumberToString[ UCGreek, SymbolChar, # ]& /@ {
{1,"Alpha"}, {2,"Beta"}, {3,"Gamma"}, {4,"Delta"}, {5,"Chi"}, {6,"Epsilon"}, 
{7,"Phi"}, {8,"Eta"}, {9,"Iota"}, {10,"Phi"}, {11,"Mu"}, {12,"Nu"}, {13,"Rho"},
{14,"Sigma"}, {15,"Tau"}, {16,"Kappa"}, {17,"Lambda"}, {18,"Xi"}, {19,"Zeta"}, 
{20,"Pi"}, {21,"Ypsilon"}, {22,"Omega"}, {23,"Psi"} };

End[]


