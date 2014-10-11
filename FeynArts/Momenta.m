
(* :Title: Momenta *)

(* :Author: Hagen Eck *)

(* :Summary: This package contains the functions needed to supply topologies
             with momenta
*)

(* :Context: HighEnergyPhysics`FeynArts` *)

(* :Package Version 0.1 *)

(* :Mathematica Version 2.0 *)

(* :Functions:
        - SupplyMomenta: add momenta to inserted topology. This function
	  returns a a list of two elements: the first one is the topology
	  with momenta appended to propagators, the second one is a list
	  of integration momenta contained in this topology.
        - InsertMomenta: for external usage. Returns the bare topology
          with momenta as third elements of the propagators.
*)

FAPrint[3, "[Momenta.m]"];

Begin["`Momenta`"];

(* decide wether external or internal:
 *)

ExternQ[ Propagator[ ty_ ][ __ ] ] := 
  MemberQ[ { External, Incoming , Outgoing } , ty ];

IncomingQ[ Propagator[ ty_ ][ __ ] ] := 
  TrueQ[ ty===Incoming ];

InternQ[ Propagator[ ty_ ][ __ ] ] := 
  FreeQ[ { External, Incoming , Outgoing } , ty ];

(* number of propagators:
*)

NrOfInc[ t:Topology[__] ] :=  Length[List@@Select[t, IncomingQ ]];
NrOfInc[ t:Topology[___][__] ] := Length[List@@Select[t, IncomingQ ]];

NrOfExt[ t:Topology[__] ] :=  Length[List@@Select[t, ExternQ ]];
NrOfExt[ t:Topology[___][__] ] := Length[List@@Select[t, ExternQ ]];

(* append external momenta:
*)
PutMomEx[ Propagator[ty_][ Vertex[1,ct_:0][n_] , to_[m_] , a___ ] ] := 
 Propagator[ty][Vertex[1,ct][n],to[m],a,FourMomentum[External,n]];

PutMomEx[ Propagator[ty_][ to_[m_] , Vertex[1,ct_:0][n_] , a___ ] ] := 
 Propagator[ty][to[m],Vertex[1,ct][n],a,-FourMomentum[External,n]];

PutMomEx[ Propagator[Incoming][ Vertex[1,ct_:0][n_], to_[m_], a___ ], Nri_ ] := 
 Propagator[Incoming][Vertex[1,ct][n],to[m],a,FourMomentum[Incoming,n]];

PutMomEx[ Propagator[Incoming][ to_[m_], Vertex[1,ct_:0][n_], a___ ], Nri_ ] := 
 Propagator[Incoming][to[m],Vertex[1,ct][n],a,-FourMomentum[Incoming,n]];

PutMomEx[ Propagator[Outgoing][ Vertex[1,ct_:0][n_], to_[m_], a___ ], Nri_ ] := 
 Propagator[Outgoing][Vertex[1,ct][n],to[m],a,-FourMomentum[Outgoing,n-Nri]];

PutMomEx[ Propagator[Outgoing][ to_[m_], Vertex[1,ct_:0][n_], a___ ], Nri_ ] := 
 Propagator[Outgoing][to[m],Vertex[1,ct][n],a,FourMomentum[Outgoing,n-Nri]];

TreatExtern[ t:Topology[__] ] := 
Block[{nri=NrOfInc[t],extli,retop},
       If[ nri===0,
           retop = PutMomEx /@ (List@@Select[ t , ExternQ ]),
           retop = PutMomEx[ # , nri ]& /@ (List@@Select[t , ExternQ ]) 
	 ];
       (* in a self-energy the second external mom is replaced by the first 
	  one (with correct sign) *)
       If[ nri===1 && NrOfExt[t]===2,
	   extli = Select[retop,ExternQ];
           retop = retop /. Last[ extli[[2]] ] :> 
				If[ Head[  Last[ extli[[2]]]  ]===Times,
				    -Last[ extli[[1]] ],  
				    Last[ extli[[1]] ]                
				  ]
         ];
       retop
      ];

(* append internal momenta:
*)
PutMomInt[ x_List ] :=  Apply[Append,#]& /@ 
		      Transpose[{x,Array[True,Length[x]]}];

TreatIntern[ t:Topology[__] ] := 
 PutMomInt[ List@@ Select[ t , InternQ ] ];

TreatSpecial[ t:Topology[__], specmom_List ] := 
        Append[ t[[ #[[1]] ]], #[[2]] ]& /@ specmom ;

(* TreatExtern, -Intern and -Special:
*)
AppendMomenta[ t:Topology[__], specmom_List ] := 
   Block[ {tUnspec},
	   tUnspec = Delete[ t, {#[[1]]}& /@ specmom ];
	   Join[ TreatSpecial[t,specmom], 
		 TreatExtern[tUnspec] , 
		 TreatIntern[tUnspec] ] /. Vertex[n_][m_] :> Vertex[n,0][m]
         ];

(* construct the "momentum vertex" :
*)
SelectMomentum[ver_, Propagator[_][ver_ , _ , ___ , m_]] := -m;
SelectMomentum[ver_, Propagator[_][ _ , ver_, ___ , m_]] := m;
SelectMomentum[ver_, Propagator[_][ver_,ver_, ___ , m_]] := {-m , m};

(* take all but external field points:
*)
MakeFieldPoints[t_List] := 
 Function[z, 
          Expand[ Flatten[ SelectMomentum[z,#1]& /@ 
			   Select[t, MemberQ[#1,z]& ] 
		         ]]
         ] /@   Select[ Vertices[Topology @@ t],
                        !MatchQ[#, (Vertex[1][_]|Vertex[1,0][_]) ]& ] ;

(* search momentum to solve equation:
*)
firstTrue[vert_List] := If[ FreeQ[vert/.List->Plus,True],
                         {},
                         Cases[ vert/.List->Plus, 
                                _.True[j_] -> True[j] ][[1]]
		       ];

(* solve equation for first momentum "True":
*)
MomentumReplacement[topo_List , i_ ] := 
    If[ firstTrue[ topo[[i]] ]==={},
(*
Print["Step ", i, ": ",topo];
Print["Vertex ",i ,": nothing to solve: ", topo[[i]] ];
*)    
       { { Global`FourVector[Incoming,1]->Global`FourVector[Incoming,1] } },
(* 
Print["Step ", i, ": ",topo];
Print["Vertex ",i ,": ",
       Solve[   Apply[ Plus,topo[[i]] ] == 0 , firstTrue[ topo[[i]] ]   
	   ]/. DebugRule
    ];
*)
       Solve[   Apply[ Plus,topo[[i]] ] == 0 , firstTrue[ topo[[i]] ]   ]
      ];

(* sum of all external momenta (=0) :
*)
momsum[ t:Topology[__] ] := 
        Plus@@ (Join[
                Cases[t, Propagator[Incoming][___,Vertex[1,0][n_],___] -> 
			 Global`FourVector[Incoming,n] ],
                Cases[t, Propagator[Outgoing][___,Vertex[1,0][n_],___] -> 
                         Global`FourVector[Outgoing, n-NrOfInc[t]] ]
               ]);

(* PutMomenta: insert momentum conservation for every vertex.
*)
PutMomenta[ t:Topology[s__][ pr__ ], momspec_List ] :=
   Topology[s]@@ PutMomenta[ Topology[pr], momspec ];

PutMomenta[ t:Topology[__], momspec_List ] := 
     TSort[ ExpandAll[
      Topology@@ Fold[ PutMomenta, 
                       AppendMomenta[t,momspec], 
		       Array[ #&,Length[ Union @@ ( Vertices[I][t] ) ] ]  
		     ] /. momsum[t]->0
     ] ];

PutMomenta[ topo_List , n_Integer?Positive ] := 
  topo /. Flatten[ MomentumReplacement[ MakeFieldPoints[topo], n ] ];

(* SupplyMomenta: momentum conservation and renumbering the internal 
   momenta:
*)
SupplyMomenta[ thetop_, momspec_List ] := 
   Block[ {tt,theTrue,theq,newq={},i=1,allq} ,
(*
Print["Topology: ", thetop];
*)
	 tt = PutMomenta[ thetop, momspec ];
(*
Print["PutMomenta = ", tt ];
*)
         (* original :  
	 theTrue = Cases[ Flatten[List@@ tt/.Propagator[_]->List], _. True[_] ];
         *)
	 theTrue = Union[ Cases[ Flatten[List@@ tt/.Propagator[_]->List], 
				 _. True[nn_] -> True[nn] ] ];
         theq = Cases[ Flatten[List@@ tt/.Propagator[_]->List], 
	       _. FourMomentum[Internal,a_] :> FourMomentum[Internal,a] ];
	 While[ Length[newq] < Length[theTrue],
                If[ FreeQ[ theq, FourMomentum[Internal,i] ],
		    AppendTo[ newq, FourMomentum[Internal,i] ],
                  ];
                i++
              ];
	 {tt /. Thread[ Rule[ theTrue , newq  ] ], Join[ newq, theq ] }
	];

(* exported function: InsertMomenta
 *)

InsertMomenta[ t:(TopologyList[__]|TopologyList[__][__]) ] := 
  TopologyList@@ (InsertMomenta /@ t);

InsertMomenta[ t:Topology[_][__] ] := InsertMomenta[ Topology@@t, {} ];

InsertMomenta[ t:Topology[_][__], m_List ] := InsertMomenta[ Topology@@t, m ];

InsertMomenta[ t_Topology ] := InsertMomenta[ t, {} ];

InsertMomenta[ t_Topology, m_List ] := 
   ReleaseHold[
   SupplyMomenta[
                 Topology@@( (Append@@#)& /@ 
                                 Transpose[{ List@@t,
                                             Array[Field,{Length[t]}] }]
                 ),
                 m
                ][[1]] /. { AntiParticle[Field[_]] :>Hold[Sequence[]],
                            Field[_]:>Hold[Sequence[]] }
                            
   ];

(* in order to Paint the momenta topologies we have to define the 
 * Appearance function:
 *)

MomAppRule =
 {
  FourMomentum[ Incoming, n_Integer ] :> "p" <> ToString[n],
  FourMomentum[ Outgoing, n_Integer ] :> "k" <> ToString[n],
  FourMomentum[ Internal, n_Integer ] :> "q" <> ToString[n],
  FourMomentum[ External, n_Integer ] :> "p" <> ToString[n]
 };

Appearance[ 0 ] :=
 {
  PropagatorLabel -> "0",
  PropagatorType -> Straight,
  ProagatorArrow -> None
 };

Appearance[ -FourMomentum[Outgoing, n_ ] ] :=
 {
  PropagatorLabel -> "k"<>ToString[n],
  PropagatorType -> Straight,
  PropagatorArrow -> Backward
 };

Appearance[  expr__  ] :=
 {
   PropagatorLabel -> ToString[ expr //. MomAppRule ],
   PropagatorType -> Straight,
   PropagatorArrow -> Forward
 } /; !FreeQ[ expr, FourMomentum ];

End[] (* HighEnergyPhysics`FeynArts`Momenta` *)
