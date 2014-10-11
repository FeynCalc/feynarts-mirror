
(* :Title: Coupling *)

(* :Author: Hagen Eck *)

(* :Summary: This package contains the functions needed to translate
	     vertices to couplings.
*)

(* :Context: HighEnergyPhysics`FeynArts` *)

(* :Package Version 0.1 *)

(* :Mathematica Version 2.0 *)

(* :Functions:
           - GtoCRules is the most important function of this package.
             It calls the function GtoCR to find the replacement rules
             for the G-functions (coupling vectors) of a coupling.
           - Coupling is an experimental function to find the Analytical-
             Coupling functions for a certain set of fields.
*)

FAPrint[3, "[Coupling.m]" ];

Begin["`Coupling`" ];

(*  AnalyticalCoupling defaults to zeroth order:
 *)
AnalyticalCoupling[ anything:(_?(!IntegerQ[#]&)).. ] := 
     AnalyticalCoupling[0][ anything ];

(* utility functions: sort coupling according to the generic or classes
 * reference order
 *)

SplitGen[ f:(s_.P$Generic) ] := { ToGeneric[f], hold[s][] };

SplitGen[ f:(s_.P$Generic[stuff__]) ] := {ToGeneric[f], hold[s][stuff]};

SortGeneric[ h_[ fields__ ] ] :=
Block[ { ref, split },

     (* find generic reference order:
      *)
       ref = Flatten[
              ReferenceOrder[Generic][[
                 Flatten[Position[ GenericFieldPoints[],
                                   FieldPoint@@ Sort[ToGeneric[{fields}]] ] ]
              ]] ];

     (* split fields into generic and "other" part:
      *)
       split = SplitGen /@ {fields};

     (* sort according to reference order:
      *)
       split = { { }, split };
       Do[ 
          split = split /. { {a___}, {b___, {ref[[xxi]], s_}, c___} } :>
                           { {a, {ref[[xxi]], s}}, {b,c} },
          {xxi, Length[ref]}
         ];

       Return[ (h@@split[[1]]) //.{ { f_, hold[s_][any__] } :> s f[any],
                               { f_, hold[s_][] } :> s f } 
             ]
 ];

SplitCla[ f:(s_.P$Generic[i_Integer, stuff___]) ] := 
          { ToClasses[f], hold[stuff] };

SortClasses[ h_[ fields__ ] ] :=
Block[ { ref, split, exp },

     (* find classes reference order:
      *)
       ref = Flatten[
              ReferenceOrder[Classes][[
                 Flatten[Position[ FieldPoints[],
                                   FieldPoint@@ Sort[ToClasses[{fields}]] ] ]
              ]] ];

     (* split fields into classes and "other" part:
      *)
       split = SplitCla /@ {fields};

     (* sort according to reference order:
      *)
       split = { { }, split };
       Do[ 
          split = split /. { {a___}, {b___, {ref[[xxi]], s_}, c___} } :>
                           { {a, {ref[[xxi]], s}}, {b,c} },
          {xxi, Length[ref]}
         ];

     (* what we would like to return is:
      *)
       exp = (h@@split[[1]]) //.{ { s_.f_, hold[any__] } :> s Append[f,any],
                                  { f_, hold[] } :> f } ;
    
       Return[exp];
 ];
                            
                            
(* Coupling:
 *  Print analytical expression of a coupling in readable form, i.e. apply
 *  AnalyticalCoupling and format the output.
 *)

Coupling::nores =
"I can't resolve the coupling `1` for kinematical index `2`.";

Coupling::cto =
"Counterterm order `2` is not defined in coupling of `1`.";

Coupling[_][ fi:(_. P$Generic).. ] := Coupling[fi];

Coupling[ fi:(_. P$Generic).. ] := 
   Coupling@@ 
    Flatten[
        ReferenceOrder[Generic][[ 
	  Flatten[Position[ GenericFieldPoints[], 
                            FieldPoint@@ Sort[ToGeneric[{fi}]] 
                          ]
                 ] 
          ]]
    ] /; Select[ {fi}, MatchQ[#,-P$Generic]& ] =!= {};

Coupling[ fi:P$Generic.. ] := 
Module[ { fields, coup },
       (* use 0 as class index *)
        fields = Table[ {fi}[[i]][0], {i, Length[{fi}]} ];
        fields = AppendFullIndices[ fields ];
        coup = AnalyticalCoupling[0]@@ Sort[fields];
	Return[ coup //. CouplingRule ];
];

Coupling[ fi:P$Particle.. ] :=  Coupling[0][ fi ];

Coupling[cto_Integer][ fi:P$Particle.. ] := 
Module[ { fields, coup, map, xmap },

      (* append momenta and kinematic indices p1,p2,.. mu1,mu2,..:
       *)
        fields = AppendFullIndices[ {fi} ];

      (* find the generic analytical expression:
       *)
        fields = SortGeneric[fields];
        coup = AnalyticalCoupling[cto]@@ fields;

      (* apply the correct G->C replacement:
       *)
        coup = coup /. GtoCRules[];

        Return[ExpandAll[coup]] 
];

AppendFullIndices[ v:{ P$Particle.. } ] :=
Block[ { momenta, parind, kinind, nr, fields },

     (* find list of momenta (p1, p2, ...)
      *)
       momenta = ToExpression /@ ( ("p"<>#)& /@
                   ( ToString /@ Table[ xxi, {xxi, Length[v]} ] ) );

     (* find list of particle indices ({i1_1, i1_2,..}, {i2_1,i2_2,..}...)
      *)
       parind = Table[ Table[ {ToString[xxi], ToString[xxj]},
                                  {xxj, Length[ParIndices[v[[xxi]]]]} ],
                           {xxi, Length[v]}  
                      ]; 
       parind =  Table[ ("i"<>#[[1]]<>"_"<>#[[2]])& /@ parind[[xxi]],
                        {xxi, Length[parind]} ];

     (* append particle indices and momenta, take care that particle indices
      * that are already present are not overwritten:
      *)
       fields =  (IndAppend@@#)& /@ Transpose[ { v, parind } ];
       fields =  (FieldAppend@@#)& /@ Transpose[{ fields, momenta }] ;

     (* append kinematical indices if not present:
      *)
       nr = 1;
       Do[
          If[ ( kinind = KinematicIndices[ v[[xxi]] ] ) =!= {},
              kinind = ToExpression /@ ( ("mu"<>#)& /@
                 (ToString /@ Table[ xxj, {xxj, nr, nr+Length[kinind]-1} ]));
              nr = nr + Length[kinind];
              fields[[xxi]] = FieldAppend[ fields[[xxi]], kinind]
            ],
          {xxi, 1, Length[v]} ]; 

       Return[ fields ] 
      ];


IndAppend[ s_. (fi:P$Generic)[n_], {ind__} ] := s fi[n, {ind}];

IndAppend[ s_. (fi:P$Generic)[n_], {} ] := s fi[n];

IndAppend[ s_. (fi:P$Generic)[n_, l_List ], {ind__} ] :=
     s fi[ n, {ind} ] /; Length[l] > Length[{ind}];

IndAppend[ s_. (fi:P$Generic)[n_, l_List ], {ind__} ] :=
     s fi[ n, l ] /; Length[l] == Length[{ind}];

IndAppend[ s_. (fi:P$Generic)[n_, l_List ], {ind__} ] :=
     s fi[ n, Join[l, Drop[{ind},Length[l]]] ] /; Length[l] < Length[{ind}];

FieldAppend[ s_. (fi:P$Generic)[n__], momki_ ] := s fi[n, momki];

KinematicIndices[ s_. (fi:P$Generic)[n__] ] := KinematicIndices[fi];

ParIndices[ s_. (fi:P$Generic)[n_Integer] ] := Indices[ fi[n] ];

CouplingRule = 
{
  G[s_][0] :> G[s]["c"],
  HighEnergyPhysics`FeynArts`Utilities`PV -> "PV",
  Global`DiracMatrix -> "gamma",
  Global`ChiralityProjector -> "om"
};

(*
 * about G -> C replacement: 
 *  The convention is to enter the Feynman rules such that the generics are 
 *  in alphabetic order.
 *  Since the introduction of the SortGeneric and SortClasses function, this
 *  convention is obsolete. Generic field couplings can be defined in any
 *  order of the fields. Two points remain:
 *  a) the coupling definitions in the classes model file have to be made
 *     with classes couplings of the same ordering as in the generic coupling
 *     definition.
 *  b) the FermionFlipRule have to match the field positions of the fermionic
 *     fields of the generic coupling (of course it is safest to keep the
 *     fermions in front for this).
 *)

(* GtoCRules: 
 *  GoesToC tries to replace the head "G" by "TheC" (remember that the 
 *  classes permutation is already resolved by applying the appropriate
 *  mapping of kinematical indices to all G-expressions). If this doesn't
 *  work, it will try the negative kinematical expression and return the
 *  corresponding classes coupling (with an additional "-"sign, of course).
 *  If both of them do not resolve TheC, it will type a message and return
 *  the expression "C[c][fields][kinpart]".
 *) 

GtoCRules[] := 
{
  G[sym_][c_][fi__][ls__] :> GoesToC[ G[sym][c][fi][ls] ]   
}; 

GoesToC[ G[sym_][c_][fi__][ls__] ] :=
Block[ { cv, fp },

     (* first we have to get rid of superfluous signs of the fields.
      * a generic field point might contain something like 
      * -V[Index[Generic,1],..] which is replaced by -V[1,..] and thus
      * cannot be resolved.
      *)
       fp = If[ SelfConjugate[#] && MatchQ[#, - P$Generic[__] ],
                - #,
                  #
              ]& /@  {fi} ;
                    
     (* find the mapping of the actual classes fields to model definition
      * of the classes coupling and extend it to all Mom's and KI's:
      *)
	map = FindMap[ fp, c ];
(*
Print[" mapping: ", map];
*)
        xmap = ExtendedMap[ map ];
(*
Print[" FFR = ",M$FermionFlipRule@@map ];
*)
     (* if this is a fermionic coupling, apply the fermion flip rule:
      *)
       If[ !FreeQ[ fp, F ],
           cv = G[sym][c][Sequence@@fp][ Sequence@@(
                               Evaluate[{ls}/.M$FermionFlipRule@@map]) ],
           cv = G[sym][c][Sequence@@fp][ ls ]
         ];
  
     (* insert the actual values of the kinematical indices:
      *) 
       cv =  cv /. xmap;

     (* just try:
      *)
       cv = cv /. G[locs_][locc_][locfi__][locls__] :> 
                  TheC[locls][ Sequence@@SortClasses[{locfi}] ];

     (* if this didn't work, it might be a G[-]:
      *)
       If[ (!FreeQ[ cv, TheC ]) && (sym === -1),
           cv = cv /. TheC[locls__][locfi__] :>
                      - TheC[Evaluate[-locls]][Sequence@@SortClasses[{locfi}]]
         ];

     (* that's all we could do, so:
      *)
       If[ !FreeQ[ cv, TheC ],
           Message[ Coupling::nores, fp, {ls} ];
           Return[ C[c][Sequence@@fp][ls] ]
         ];
 
     (* check for counterterm order:
      *)
       If[ Length[cv] < c+1,
           Message[ Coupling::cto, fp, c ];
           Return[ C[c][Sequence@@fp][ls] ],
           Return[ cv[[c+1]] ]
         ];
 ]; 

(* FindMap returns a list of position replacement rules that transform
 * a given coupling to the coupling as defined in the model, i.e. a list
 * of the form { 1->3, 2->1, 3->2 }.
 * We have to take care  in this case, because there might be more than
 * one coupling definition for certain orderings of the fields, e.g for
 * fermions C[F,-F,...] and C[-F,F,...].
 *)
FindMap[ coup_, order_ ] :=
Block[ {modelcoup, strippedcoup, gen=False, sel, fp },

     (* leave only generic or classes fields in the coupling:
      *)
       strippedcoup = Array[ coup[[#]] /. 
	        { s_. (fi:P$Generic)[i__,mom_,{__}] :> s fi[ i ],
	          s_. (fi:P$Generic)[i__,mom_]      :> s fi[ i ]  
                } /.  - (fi:P$Generic)[i__] :> If[ SelfConjugate[ fi[i] ],
				              fi[i], - fi[i] ]&,
	        Length[coup] ];

     (* if the coupling contains fields X[Index[Generic,_]], we are on the
      * generic level:
      *)
       If[ !FreeQ[ strippedcoup, Index[Generic,_] ],
           strippedcoup = ToGeneric[ strippedcoup ];
           gen = True
         ];

     (* the field point is: 
      *)
       fp = FieldPoint @@ Sort[strippedcoup];

     (* this is a list of all defined model fieldpoints of the appropriate
      * counterterm order that match the field point (should have only one 
      * entry): 
      *)
       modelcoup = 
        If[ gen,
            ReferenceOrder[Generic][[
              Flatten[Position[ GenericFieldPoints[], fp ]]
            ]],
            ReferenceOrder[Classes][[ 
	      Flatten[Position[ FieldPoints[], fp ]] 
            ]] 
          ];

     (* if the stripped coupling is a member of this list, there exists
      * a definition (C[..]) in the model file, otherwise we just use 
      * the first entry of the list 
      *)
       sel = Select[ modelcoup, (#===strippedcoup)& ];
       If[ sel==={}, 
           modelcoup=modelcoup[[1]], 
           modelcoup=sel[[1]]
         ];
       PermList[ strippedcoup, modelcoup ]
    ];

(* this function gives the map for FindMap which transforms l1 into l2: 
 *)
PermList[ l1_, l2_ ] :=
   Block[ { p, l = Length[l1], perm, i, l2x=l2 },
	  If[ l =!= Length[l2], Print[ "Unequal length of permuted lists: ",
                                       l2," <> ",l1  ] ];
          perm = {};
          Do[ p = Position[l2x, l1[[i]], 1][[1,1]]; 
              AppendTo[perm, i -> p];
              l2x[[p]] = 0,
              {i, l}
            ];
          perm

(* above code replaced by th jun 28, 96 

	  Do[ p = Position[ newl2, newl1[[i]], 1 ][[1,1]];
	      newl1 = ReplacePart[ newl1, p, i ];
	      newl2 = ReplacePart[ newl2, 0, p ],
              {i,1,l}
            ];
          Apply[ Rule, Transpose[ { Array[ #&, l ], 
				  List @@ (Identity /@ newl1) } ], {1} ]
*)
        ];

(* having found the correct mapping of the fields to the order defined in 
 * the model file, we can construct the replacement rules for all Mom's
 * and KIn's of the coupling.
 *)
ExtendedMap[ map_ ] :=
Block[ { maxKI, heads, xmap={} },
       
     (* find maximum number of kinematic indices:
      *)
       maxKI = 
        Max@@ ( Length/@ ( KinematicIndices/@ (P$Generic/.Alternatives->List)
                         ) /. KinematicIndices[_] -> {}
              );
     (* construct all possible heads (Mom, KI1, KI2, ..):
      *)
       heads = Join[{Mom}, Array[ToExpression["KI"<>ToString[#]]&, maxKI]];

     (* construct extended map for each head:
      *)
       Do[ AppendTo[ xmap,
                     Thread[ #[ map[[i]] ], Rule]& /@ heads
                   ],
           {i, Length[map]}
         ];

       Return[Flatten[xmap]]
     ];


(* this rule appends momenta Mom[n] and kinematic indices KIm[n] (m=1,2..)
   to a classes field X[i__]
*)
AMomLiRule[ n_Integer ] :=
      s_. (fi:V|S|U|F|VS|SV)[i__] :>
          If[ Length[ KinematicIndices[fi] ] === 0,
	      s fi[  i, Mom[n] ],
	      s fi[  i, Mom[n],
		      Array[ ToExpression[ "KI" <> ToString[#] ][n]&,
				    Length[ KinematicIndices[fi] ] ] 
		  ]  
            ];


(* Format-definitions for coupling symbols:
 *)

Format[ G[s_][c_][fi__][g_] ] := 
  SequenceForm[ "G", 
                ColumnForm[ { StringJoin["(", ToString[c], ")"],
                              "",
                              StringJoin@@(ToString/@ToGeneric[{fi}])},
                            Left, Center
                           ],
                "[", g , "]"
              ];

Format[ NonCommutative[ any__ ] ] :=
   SequenceForm[ "(", Dot[any], ")" ];

End[] (* "HighEnergyPhysics`FeynArts`Coupling`" *)
