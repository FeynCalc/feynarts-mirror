
(* :Title: Initialize *)

(* :Authors: Hagen Eck, Sepp Kueblbeck *)

(* :Summary: FeynArts`-functions for initialization of a model *)

(* :Context: HighEnergyPhysics`FeynArts` *)

(* :Package Version 0.1 *)

(* :Mathematica Version 2.0 *)

(* :Contents:
	Part1: (Re-)initialization of a model
	Part2: Auxiliary functions
	Part3: Initialization of Generic model 
	Part4: RestrictCurrentModel
	(search for Part# to find beginning of a section)
*)

FAPrint[3,"[Initialize.m]"];

Begin["`Initialize`"];

(*
   InitializeModel. 
	A model is defined as a string indicating the name of the
	corresponding model file and (optional) a number of restrictions
	on this model.
	This model is the $Model during a FeynArts-session.

	The functions defined for a certain model are:
	  F$AllGeneric		: all generic fields
	  F$AllClasses		: all classes of the model
	  F$AllParticles	: all particles of the model
	These lists are static for the initialized model. For InsertFields
	however, the following lists which might be changed by the function
	RestrictCurrentModel, are used:
	  F$Generic		: currently used generic fields
	  F$Classes		: currently used classes
	  F$Particles		: currently used particles
          F$AllowedFields	: all used fields
	For the construction of the couplings the following lists are
 	defined:
	  FieldPoints[0, 1, 2, ...]
	  ReferenceOrder[Generic|Classes]
	  GenericFieldPoints[]
	Furthermore, InitializeModel defines the functions
	  PossibleFields	-> fields `fitting' on a half-filled vertex
	  CheckFieldPoint	-> is this vertex in the current model?
	  AnalyticalCoupling	-> analytical expression for a coupling
	  Analytical Propagator	-> dito for propagators
	InitializeModel also sets the functions that return certain
	properties of the fields like
	  AntiParticle, Indices, SelfConjugate, Right/LeftPartner,
	  Appearance, TheMass, TheLabel, KinematicIndices etc.	
	
	A remark concerning the function CheckFieldPoint: the symbol Field-
	Point is defined to be orderless, i.e. FieldPoint[-F[4], F[3], S[3]]
	is the same as FieldPoint[F[3], -F[4], S[3]]. The fields are always
	sorted according to their generic type (F-S-V-U) and their class
	number. We have to keep in mind that this sorting of field points
	is NOT allowed when dealing with couplings: fermion fields do not
	commute!
*)

Options[ InitializeModel ] =
	{
	 GenericModel -> "Lorentz",
	 Reinitialize -> True
	};

InitializeModel::modelspec =
"\"`1`\" is not a valid Model-specification."

InitializeModel::nolist =
"\"`1`\" is not a model description list."

InitializeModel::filenotfd =
"cannot find model file for model \"`1`\"."

InitializeModel::nogenfile =
"cannot find generic model file \"`1`\"."

InitializeModel::diffmodtypes =
"different Lorentzstructures `1` are not compatible.";

InitializeModel::extensspec =
"no extension specification for `1`.";

InitializeModel::HGlabel =
"Halfgeneric field `1` has no label definition.";

InitializeModel::nohalfg =
"no halfgeneric initialization possible: no halfgeneric vertex input: `1`.";

InitializeModel::nodescr =
"Model `1` contains no class descriptions.";

InitializeModel::dblset =
"Multiple descriptions for class field `1` in model file.";

InitializeModel::norange =
"Index `1` has no IndexRange-specification.";

InitializeModel::incomp1 =
"Coupling definition in model file for `1` incompatible to generic coupling
structure. Coupling is not a vector of length `2`.";

InitializeModel::incomp2 =
"Incompatible index structure in Classes coupling: `1` is not of the 
form `2`";

InitializeModel::usethat =
"Use Options[InitializeModel] instead!";

InitializeModel::rhs1 =
"Right hand side of generic coupling `1` has G-Expressions inside the 
kinematical vector!";

InitializeModel::rhs2 =
"Right hand side of generic coupling `1` is not of the form  G . {__}.";

InitializeModel::badrestr =
"`1` is not a valid model restriction (ignoring).";

InitializeModel::nosymb =
"Your model definition contains symbols that were already assigned
values (most often \"i\" or \"j\"). I can't handle this. Please remove
those definitions and try again.\n
Assigned value is: `1`";


(* Part1: *)

(* InitializeModel needs a model description as argument and returns 
   True after having performed the initialization.
   A model description is either a single String or a list consisting
   of a String as first element and a number of model restrictions valid
   for this model.
*)

(* Status information:
*)
CurrentModel[] :=
  Which[ MatchQ[ $CurrentModel, {}],
         Print["No model initialized."],
	 MatchQ[ $CurrentModel, { _String } ],
	 Print["Current generic model: ", $CurrentModel[[1]]];
	 Print["No classes model initialized."],
	 MatchQ[ $CurrentModel, { _String, _String } ],
	 Print["Current generic model: ", $CurrentModel[[1]]];
	 Print["Current classes model: ", $CurrentModel[[2]]];
	 Print["No restrictions active."],
         MatchQ[ $CurrentModel, { _String, {_String,(__Rule)|({__Rule})}} ],
	 Print["Current generic model: ", $CurrentModel[[1]]];
	 Print["Current classes model: ", $CurrentModel[[2,1]]];
	 Print["Restrictions: \n"];
         Print[ Sequence@@ Drop[ $CurrentModel[[2]], 1 ] ],
         True,
	 Print["$CurrentModel corrupted!"]
       ];
       

(* Main function: 
*)
InitializeModel[ model:P$Model, opt___Rule ] := 
Module[ {genmod, genfile, genopt=ActualOptions[InitializeModel, opt],
         loadgen=False, loadmod=False,
         heads, message, ret=None, cat=True, newmodel},

  (* check if the right Feynman rules are loaded 
   *)
   genmod = GenericModel/.genopt;
   If[ (GenericModel/.{opt}) =!= GenericModel, loadgen = True];
     (* Option GenericModel was used *)
   If[ ($GenericModel===""), loadgen = True ];
     (* no generic model initialized, yet *)

  (* initialize generic model 
   *)
   If[ loadgen || (Reinitialize/.genopt),
    genfile = Select[ $GenericFiles[],
		      StringMatchQ[#, "*"<>genmod<> ".gen", 
				   IgnoreCase->True]& ];
    Which[ Length[ genfile ] === 0,
          Message[ InitializeModel::nogenfile, genmod <> ".gen" ];
	          Return[ $Aborted ],
          Length[ genfile ] === 1,
          genfile = genfile[[1]];
          message = " <- \"" <> genfile <> "\"",
          Length[ genfile ] > 1,
          buffer = genfile[[1]];
          Do[ If[ StringLength[ genfile[[i]] ] < StringLength[buffer],
	          buffer = genfile[[i]] ],
	      {i, 2, Length[genfile]} ];
          genfile = buffer;
          message = " <- \""<>genfile<>"\" (ambiguous!)"
         ];
    InitializeGenericModel[ genmod, genfile ];
   ];

  (* initialize classes model: 
   *)
   If[ (newmodel = ModelName[ model ]) =!= ModelName[$Model],
       loadmod =  True ];
   If[ loadmod || (Reinitialize/.genopt),
      FAPrint[2, " "];
      FAPrint[2," ... initializing "<>
                "classes model \""<>newmodel<>"\""];
      cat = Catch[ InitBasicModel[ newmodel ] ];
      Which[ (*1*) cat === $Aborted,
   	        ret = $Aborted;
   	        FAPrint[1," !! classes model \"",newmodel,
   	                  "\": initialization aborted. !!"],
   	  (*2*) cat === Global`$Stopped,
   	        FAPrint[1," !! classes model \"",newmodel,
   	                   "\": initialization stopped. !!"],
   	  (*3*) cat === True,
                   $Model = newmodel;
                   FAPrint[1," -- classes model \"",newmodel,
       			 "\" initialized --"],
                True,  FAPrint[3," InitBasicModel returned: ",cat]
          ] ;
      If[ ret===$Aborted, Return[$Aborted], FAPrint[2," "] ]
     , (*else*)
     cat = True 
    ];

   (* set allowed fields before call to RestrictCurrentModel: 
    *)
    F$AllowedFields = Union[ F$AllGeneric, F$AllClasses, F$AllParticles ];

   (* check for change in restrictions: 
    *)
    If[ loadmod || 
        ( Sort[Restrictions[model]] =!= Sort[Restrictions[$Model]]),
	$Model = model;
        loadmod = True;
        cat = RestrictCurrentModel[ Restrictions[model] ];
        If[ cat === $Aborted, 
            FAPrint[1, " ! classes model \"", newmodel,
                    "\": failed to introduce restrictions."],
            cat = True
          ]
      ];

   (* set of allowed fields in the full model. the F$Allxx are different
    * from the other lists only for user calls of RestrictCurrentModel
    * or during InsertFields. 
    *)
    F$AllGeneric = F$Generic;
    F$AllClasses = F$Classes;
    F$AllParticles = F$Particles;
    F$AllowedFields = Union[ F$AllGeneric, F$AllClasses, F$AllParticles ];

   (* update $CurrentModel: 
    *)
   If[ loadgen,
      If[ $CurrentModel === {},
          $CurrentModel = {$GenericModel},
          $CurrentModel[[1]] = $GenericModel
        ]
     ];
   If[ loadmod,
      If[ cat === True,
         If[ $CurrentModel === {$GenericModel},
             $CurrentModel = AppendTo[ $CurrentModel, model ],
             $CurrentModel[[2]] = model
           ],
         $CurrentModel = Take[ $CurrentModel, {1} ]
       ]
     ];
   Return[cat]
  ];

ModelName[ m_String ] := m;
ModelName[ {m_String, __ } ] := m;
Restrictions[ _String ] := {};
Restrictions[ {_String, any__} ] := {any};
 
(* Initialization of a complete model : InitBasicModel[ MOD ] 
 * Reads the model information file MOD.mod that contains the class 
 * definitions (M$ClassesDescription) and the coupling matrices 
 * (M$CouplingMatrices).
 *)

InitBasicModel[ MOD_String ] :=
Module[ {modelfile, buffer, message, nra, nrb, count, 
         unsortedFP, unsortedCT, defCTo, cto },

    Clear[ M$LastModelRules ];

  (* check for existence of model file: 
   *)
    modelfile = Select[ $ModelFiles[], 
 		      StringMatchQ[#, "*"<>MOD<> ".mod", IgnoreCase->True ]& ];
    Which[ Length[ modelfile ] === 0,
         Message[ InitializeModel::filenotfd, MOD <> ".mod" ]; 
         Throw[ $Aborted ],
         Length[ modelfile ] === 1,
         modelfile = modelfile[[1]];
         FAPrint[2,"  ... reading model file:"];
         message = "   <- \""<>modelfile<>"\"",
         Length[ modelfile ] > 1,
         buffer = modelfile[[1]];
         Do[ If[ StringLength[ modelfile[[i]] ] < StringLength[buffer],
        	 buffer = modelfile[[i]] ],
	    {i, 2, Length[modelfile]} ];
         modelfile = buffer;
         FAPrint[2,"  ... reading model file:"];
         message = "   <-  \""<>modelfile<>"\" (ambiguous!)"
        ];
    FAPrint[2, message];

  (* supress an error message for multi-line definitions: 
   *)
    Off[Syntax::newl];
    Get[modelfile];
    On[Syntax::newl];

  (* initialize particles: 
   *)
    If[ InitParticles[ MOD ] === $Aborted,
        Throw[ $Aborted ] ];
    nra = Length[ F$AllParticles ];
    nrb = Length[ Select[ F$AllClasses, (Head[#]=!=Times)& ] ];
    FAPrint[2,"   -> ", nra, 
	      " particle",If[nra==1,"","s"]," (incl. antiparticles) in ", nrb,
              " class",If[nrb==1,"","es"]];

  (* forming the explicit and halfgeneric vertexlists: 
   *)
    FAPrint[3,"  ... finding the vertices from the couplings."];
    unsortedFP = 
       Select[ InitClassesCoupling[#,MOD]& /@ M$CouplingMatrices,
	       #=!=None& 
             ] /. sequence->Sequence;

  (* setting the reference order of the classes couplings: 
   *)
    ReferenceOrder[Classes] = Union[(List@@#)& /@  unsortedFP ];
    If[ !FreeQ[ unsortedFP, $Aborted ], Throw[$Aborted] ];

  (* what counterterm orders are there? 
   *)
    L$CTOrders = Union[ unsortedFP /. FieldPoint[n_][fi__]:> n ];

  (* loop over counterterm orders: 
   *)
    If[ $CounterTerms,
       Do[
          FAPrint[2, "   counterterms are turned ON."];
          cto = L$CTOrders[[i]];
          unsortedCT =
            Union[(List@@#)& /@ Select[ unsortedFP, (#[[0,1]] === cto) & ] ];
          FieldPoints[ cto ] = (FieldPoint@@ #)& /@ unsortedCT;
          nra = Length[FieldPoints[cto]];
          If[ cto === 0,
             FAPrint[2,"   -> ", nra, If[nra==1," vertex"," vertices"],"."], 
             FAPrint[2,"   -> ", nra, If[nra==1," counterterm"," counterterms"],
                     " of order ",cto,"."]
            ];
          FAPrint[3,"  ... initialization."];
        (* vertex initialization: *)
          SetPossibleFields[cto][#]& /@ FieldPoints[cto];
        (* Set all full existing vertices to True, keep track of ct-order *)	
          Set[CheckFieldPoint[#], True]& /@ 
               ((FieldPoint[cto]@@#)& /@ GenericFieldPoints[]);
          Set[ CheckFieldPoint[#], True ]& /@ 
               ((FieldPoint[cto]@@#)& /@ FieldPoints[cto]),
          {i,Length[L$CTOrders]}
         ],
       FAPrint[2, "   counterterms are turned OFF."];
       cto = L$CTOrders[[1]];
       unsortedCT =
         Union[(List@@#)& /@ Select[ unsortedFP, (#[[0,1]] === cto) & ] ];
       FieldPoints[ cto ] = (FieldPoint@@ #)& /@ unsortedCT;
       nra = Length[FieldPoints[cto]];
       FAPrint[2,"   -> ", nra, If[nra==1," vertex"," vertices"],"."];
       FAPrint[3,"  ... initialization."];
       (* vertex initialization: *)
       SetPossibleFields[ 0 ][#]& /@ FieldPoints[0];
       (* Set all full existing vertices to True, keep track of ct-order *)	
       Set[ CheckFieldPoint[#], True ]& /@ 
         ((FieldPoint[0]@@#)& /@ FieldPoints[0])
      ];
      FieldPoints[] = (FieldPoint@@#)& /@ ReferenceOrder[Classes];

  (* check whether LastModelRules were given: 
   *)
    If[ !ValueQ[ M$LastModelRules ],
        FAPrint[2, " warning: no M$LastModelRules in this file."];
        M$LastModelRules = {}
      ];

    Throw[True]
];


(* Part2: *)

(* Part2.1: Particle Initialization. 
 * - construct the correct index ranges for each class (function IndexRange). 
 * - definition of a function IndexSet that returns a list of all possible 
 *   index combinations for a class (local to the function). 
 * - create the list of explicit particles of the model (F$AllParticles) and 
 *   append the antiparticles to the particle and class lists.
 *)

IndexRange[error_] := (Message[ InitializeModel::norange, error ];
		       $Aborted);

(* This one returns a list of indextypes of a particle or class.
*)
Indices[ s_. fi_[ i_Integer, __ ] ] := Indices[ fi[i] ];
Indices[_] := {};

InitParticles[ MOD_ ] :=
Block[ {Mixers,MixerPartnerList},
       (* set classes: *)
	FAPrint[3, "  ... initialize particles of ", 
		    Length[M$ClassesDescription], " Classes."];
	F$AllClasses = First /@ M$ClassesDescription;
       (* set properties of classes from their description list: *)
	Function[ z, Set[ #[[1]][ z[[1]] ], #[[2]] ]& /@ 
	             Select[ z[[2]], FreeQ[#,IndexRange]& ]  ] /@  
                 ( M$ClassesDescription /.{ 
		       Mass->TheMass,
		       PropagatorLabel -> TheLabel} ) ;
       (* scan M$ClassesDescription for InsertOnly: *)
	If[ FreeQ[#, InsertOnly ],
	   InsertOnly[#[[1]]] = All ]& /@ M$ClassesDescription;
       (* set all possible index combinations for a class: *)
	Set[ IndexSet[#], 
	    If[ Indices[#] === {},
		{},
	        Flatten[ Outer[ Index, Sequence @@ (IndexRange /@ Indices[#]) ]
		       ] /. Index -> List 
	      ]
	   ]& /@ F$AllClasses;
       (* construct particle list: *)
	F$AllParticles = 
	 Flatten[ Function[ z, If[ IndexSet[z] === {},
				   z,
				   Append[z,#]& /@ IndexSet[z] 
				 ] 
			  ] /@ F$AllClasses 
		];
      (* append AntiParticles to Particles and Classes *)
       F$AllParticles = Flatten[ If[ SelfConjugate[#], #, {#,-#} ]&  /@
		                 F$AllParticles ];
       F$AllClasses = Flatten[ If[ SelfConjugate[#], #, {#,-#} ]&  /@
		                 F$AllClasses ];
       F$Particles = F$AllParticles;
       F$Classes = F$AllClasses;
      (* for mixing of scalars and vectors *)
       If[ $SVMixing === True,
           Mixers = Cases[ F$AllParticles, _SV | _VS | (- _SV) | (- _VS) ];
           If[ SelfConjugate[#] === False,
 	       Set[ MixingPartners[ -# ],
	             Reverse[ AntiParticle /@ MixingPartners[#] ] ]
	     ]& /@ Mixers;
           (* MixingParticles = Union @@ ( MixingPartners /@ Mixers ); *)
           MixerPartnerList = Prepend[ MixingPartners[#], # ]& /@ Mixers;
	   Set[ LeftPartner[ #[[1]] ], #[[2]] ]& /@ MixerPartnerList;
	   Set[ RightPartner[ #[[1]] ], #[[3]] ]& /@ MixerPartnerList;
           AssignMixers[ #[[3]], #[[1]] ]& /@ MixerPartnerList; 
         ];
];

(* defaults for Left/Right-Partner :
 *)

LeftPartner[ particle_ ] := particle;

RightPartner[ particle_ ] := particle;

AllMixers[ _ ] := {};

AssignMixers[ right_, mixer_ ] :=
  Block[ {hold = AllMixers[right]},
	 Set[ AllMixers[right], Append[ hold, mixer ] ]
       ];
 

(* Part2.2: Miscellaneous auxiliary functions.
 * Here are the definitions of the function converting particles to
 * (one) class and a class to (a list of) particles. 
 *)

Class[ s_. fi_[n_Integer,m_,r___] ] := s fi[n,r];

Particles[ s_. fi_[n_Integer,r___] ] := 
   If[ IndexSet[ fi[n] ] === {},
       s fi[n,r],
       ( s*fi[n,#,r] )& /@ IndexSet[ fi[n] ]
     ]; 

(* Default for InsertOnly is All which means this Class can be inserted 
 * into Propagators of every type.
 *)

InsertOnly[ _ ] = All;

InsertOnly[ - p_ ] := InsertOnly[p];

(* Definition of the AntiParticle functions. 
 * AP := AP = ...   is not possible because if another model
 * with different selfconj behaviour is loaded, AP must be rebuilt. 
 *)

AntiParticle[ (fi:F|S|V|U|SV|VS)[i_Integer] ] := 
   If[ SelfConjugate[ fi[i] ], fi[i], -fi[i] ];
AntiParticle[ (fi:F|S|V|U|SV|VS)[i_Integer,j_List] ] := 
   If[ SelfConjugate[ fi[i] ], fi[i,j], -fi[i,j] ];

AntiParticle[ (fi:F|S|V|U|SV|VS)[i_Integer, mom_, any___ ] ] :=
   If[ SelfConjugate[ fi[i] ], 
        fi[i, Expand[-mom], any],
       -fi[i, Expand[-mom], any]
     ] /; Not[ FreeQ[mom, FourMomentum] ];
AntiParticle[ - (fi:F|S|V|U|SV|VS)[i_Integer, mom_, any___ ] ] :=
      fi[i, Expand[-mom], any] /; Not[ FreeQ[mom, FourMomentum] ];
 
AntiParticle[ -(fi:F|S|V|U|SV|VS)[i_Integer] ] := fi[i];
AntiParticle[ -(fi:F|S|V|U|SV|VS)[i_Integer,j_List] ] := fi[i,j];
   
AntiParticle[ 0 ] := 0;
AntiParticle[ AntiParticle[ sth_ ] ] := sth;

AntiParticle[ fi_Symbol ] := fi; 
AntiParticle[ SV ] := VS;
AntiParticle[ VS ] := SV;

AntiParticle[ sign_. (fi:F|S|V|U)[ Index[Generic,i_Integer] ] ] := 
				   - sign fi[ Index[Generic,i] ];
AntiParticle[ sign_. (SV)[ Index[Generic,i_Integer] ] ] := 
				   sign VS[ Index[Generic,i] ];
AntiParticle[ sign_. (VS)[ Index[Generic,i_Integer] ] ] := 
				   sign SV[ Index[Generic,i] ];

AntiParticle[ sign_. (fi:F|S|V|U)[ Index[Generic,i_Integer],mom_, rest___ ] ] :=
  - sign fi[ Index[Generic,i],Expand[-mom],rest ] /; 
	       Not[ FreeQ[mom,FourMomentum] ] || mom === 0;
AntiParticle[ sign_. (SV)[ Index[Generic,i_Integer],mom_, rest___ ] ] := 
  sign VS[ Index[Generic,i],Expand[-mom],rest ] /; 
	       Not[ FreeQ[mom,FourMomentum] ] || mom === 0;
AntiParticle[ sign_. (VS)[ Index[Generic,i_Integer],mom_, rest___ ] ] := 
  sign SV[ Index[Generic,i],Expand[-mom],rest ] /; 
	       Not[ FreeQ[mom,FourMomentum] ] || mom === 0;

AntiParticle[ sign_. fi_[n_Integer,m_List,mom_,rest___] ] :=
  - sign fi[n,m,Expand[-mom],rest] /; 
	       Not[ FreeQ[mom,FourMomentum] ] || mom === 0;
AntiParticle[ sign_. fi_[n_Integer,mom_,rest___] ] :=
  - sign fi[n,Expand[-mom],rest] /; 
	       Not[ FreeQ[mom,FourMomentum] ] || mom === 0;

(* Definitions of the properties of related fields:
 * There are no definitions for the masses of the particles since we want to
 * keep track of the field contents of a propagator and the mass replacement
 * rules (e.g. Mass[particle] = Mass[antiparticle]) destroy this information.
 * All those definitions are given for the function TheMass.
 *)

TheMass[ - fi_[ i_Integer, List[elm___] ] ] := 
  TheMass[ fi[ i, {elm} ] ];
TheMass[ fi_[ i_Integer, List[elm___] ] ] :=
  If[ TheMass[ fi[i] ] === 0, 0, TheMass[ fi[i] ][ elm ] ];
TheMass[ -fi_[i__Integer] ] := TheMass[ fi[i] ];
TheMass[ fi_[i_Integer] ] := Mass[ fi[i] ];
TheMass[ fi_[Index[Generic,ind_]] ] := Mass[ fi[Index[Generic,ind]] ];

SelfConjugate[ (fi:P$Generic)[i_,__] ] := SelfConjugate[ fi[i] ];
SelfConjugate[ -(fi:P$Generic)[i_,___] ] := SelfConjugate[ fi[i] ];

SelfConjugate[ _. P$Generic[ Index[Generic, _] ] ] := False;

Appearance[ s_. (fi:P$Generic)[i:(_Integer|Index[Generic,_Integer]), elm___] 
          ] := 
	 { PropagatorLabel -> PropagatorLabel[ s fi[i,elm] ],
	   PropagatorType  -> PropagatorType[ s fi[i,elm] ],
           PropagatorArrow -> PropagatorArrow[ s fi[i,elm] ]};
Appearance[ lhs_ == rhs_ ] := { Appearance[lhs], "=", Appearance[rhs] };
Appearance[ n_Integer ] := ToString[n];
(* DEFAULT *)
Appearance[F] = SymbolChar["psi"];
Appearance[S] = SymbolChar["phi"];
Appearance[U] = "u";
Appearance[V] = "A";
Appearance[Index[Generic, i_Integer]] := Alph[i];

PropagatorType[ fi_[ i:Index[Generic,_Integer] ] ] := 
	     PropagatorType[fi];
PropagatorType[ - fi_[ i:Index[Generic,_Integer] ] ] := 
	     PropagatorType[fi];
PropagatorType[ -fi_[i_Integer] ] := PropagatorType[ fi[i] ];
PropagatorType[ s_. fi_[ i_Integer, __ ] ] := PropagatorType[ s fi[i] ];

PropagatorArrow[ fi_[ i:Index[Generic,_Integer] ] ] := 
	     PropagatorArrow[fi];
PropagatorArrow[ - fi_[ i:Index[Generic,_Integer] ] ] := 
	     PropagatorArrow[fi];
PropagatorArrow[ -fi_[i_Integer] ] := PropagatorArrow[ fi[i] ] /.
   { Forward -> Backward, Backward -> Forward };
PropagatorArrow[ s_. fi_[ i_Integer, __ ] ] := PropagatorArrow[ s fi[i] ];

PropagatorLabel::undef = "Don't know the label of `1`!";

PropagatorLabel[ s_. fi_[ i:(_Integer|Index[Generic,_Integer]), j___ ] ] := 
			   TheLabel[ s fi[i,j] ];

TheLabel[ 
   (fi:F|V|S|U|SV|VS)[ i:(_Integer|Index[Generic,_Integer]), j___ ] ] := 
			   TheLabel[ s fi[i,j] ] = SetLabel[ fi[i,j] ];
TheLabel[ 
 - (fi:F|V|S|U|SV|VS)[ i:(_Integer|Index[Generic,_Integer]), j___ ] ]:=
			   TheLabel[ -fi[i,j] ] = TheLabel[ fi[i,j] ];

(* Automatic setting of the label:
 *)

TheLabel::badindx =
"Index list for class `1` (`2`) is not of the form `3`.";

SetLabel[ s_. fi_[ i:(_Integer|Index[Generic,_Integer]), j___ ] ] :=
Block[ { lab, inpos = Indices[ fi[i] ] },
  Which[
	MatchQ[ s fi[i,j], _[Index[Generic,_Integer]] ],
        lab = ComposedChar[ {Appearance[fi], Appearance[i]} ],
	MatchQ[ s fi[i,j], _[_] ],   (* undefined class *)
	Message[ PropagatorLabel::undef, s fi[i,j] ];
	lab = "?",
	MatchQ[ s fi[i,j], - _[_] ], (* antiparticle of class *)
	lab = TheLabel[ fi[i] ],
	MatchQ[ s fi[i,j], _. _[_, {__}] ], (* a particle *)
	lab = TheLabel[ fi[i] ];
        If[ Head[lab] === ComposedChar,
	    lab = IndexSpec[ #, j, fi[i] ]& /@ lab,
	    lab = IndexSpec[ lab, j, fi[i] ]
	  ],
	 True,
	 Message[ PropagatorLabel::undef, s fi[i,j] ];
	 lab = "?"
       ];
     Return[lab]
    ];

IndexSpec[ expr_List, ind_List, part_ ] := 
  IndexSpec[#, ind, part]& /@ expr;

(* specify index: expr is a string (_String,_SymbolChar) or a type of index
 * (which must be translated into a string converter (Alph, ...) with the
 * help of appearance,  ind is the list of actual indices with head Index,
 * part is the class                                     
 *)

IndexSpec[ expr:(_String|_SymbolChar), ind_List, part_ ] := expr;

IndexSpec[ expr:Index[type_], ind_List, part_ ] := ( 
  Appearance[ expr /. ( (Rule@@#)& /@ Transpose[ {Indices[part], ind} ] ) ] 
  ); 

(* Part2.3: Find lists of possible particles for insertion into a vertex 
 * with (or without) valences
 * 
 * When creating the FeynAmps, there will occur vertices like
 * GM[-F[2, 1], F[2, 1], -V[1, 1]] 
 * which will not be found because of the "-"sign before "V[1,1]".
 * It is relevant for not selfconjugate particles, but for selfconjugate 
 * ones there is only the definition without the "-"sign in the 
 * CouplingMatrices. Therefore the following definition:  
 *)

TheC[ a___, -fi_[n_,m___], b___ ] :=
	     TheC[ a, fi[n,m], b ] /; SelfConjugate[ fi[n] ]

(* Make a new definition for something:
*)

SetProperty[ property_, field___,  value_ ] := 
 SetDelayed[ property[field], value ];

DeltaSelect[ coup_ ] :=
Block[ {delta},
      If[ Head[coup] === Times,
	  delta = Select[ coup, (Head[#] === IndexDelta) & ],
	  If[ Head[coup] === IndexDelta, 
              delta = coup, 
              delta = 1
            ]
        ];
      Return[delta];
     ];

(* change pure classes field to generic template:
*)
ToGenF[ s_. fi_[ i__ ] ] :=
  If[ KinematicIndices[fi] === {},
      s fi[ i, a$mom ],
      s fi[ i, a$mom, Array[ a$kind, Length[KinematicIndices[fi]] ] ]
    ];

(* InitClassesCoupling converts a single classes coupling definition (Equal) 
 * to a function definition (SetDelayed). This function checks for 
 * compatibility of the generic and the classes coupling structure and sets 
 * the Diagonal-function for the field point.
 * Remember: the structure of the classes coupling is 
 * { {a[0], a[1], ..}, {b[0], b[1], ...}, ... } where the a, b, .. refer to
 * the kinematic vector G = {Ga, Gb, ..} and the inner lists stand for 
 * increasing order of the vertices. For a one-dimensional generic coupling
 * we need only {c[0], c[1], c[2], ...}.
 * Remark: This function formerly was called "SetTheC".
 *)

InitClassesCoupling[ Equal[ vert_, coup_ ], MOD_ ] :=
Block[{ lhs=vert, rhs=coup, genref, lencv,
        unsel, checkindices, CTstruct, CTList },

    (* find generic reference order:
     *)
      genref = GenericRef[ lhs ];

    (* if no reference order found: generic coupling not present
     *)
      If[ genref == {},
         FAPrint[1, "   ! ignoring classes coupling ", vert];
         FAPrint[1, "    (no generic coupling)."];
         Return[ sequence[] ]
        ];

(* HERE:
   maybe we should add a check for the right ordering of the classes
   fields, i.e. to refuse things like C[ F[1], V[2], -F[2] ]
*)

    (* detect length of generic coupling vector:
     *)
      lencv = Length[ CouplingVector@@genref ];

    (* first we change the rhs to standard form {{__}...}:
     *)
      If[ lencv===1 && (!MatchQ[ rhs, {{__}} ]), rhs={rhs} ];

    (* check structure of field indices in coupling:
     *)
     checkindices = 
      lhs /.{ field_[nr_, ind_List] :> { Indices[field[nr]], ind },
	        field_[nr_] :> { Indices[field[nr]], {} } };
     checkindices = If[ Length[#[[1]]]===Length[#[[2]]],
			True,
			Message[ InitializeModel::incomp2, #[[1]], #[[2]] ];
			False ]& /@ checkindices;
     If[ !FreeQ[ checkindices, False ], Return[$Aborted] ];

    (* change symbols in model file to patterns: 
     *)
      Off[RuleDelayed::rhs];
      lhs = lhs //. {a___,j_Symbol,b___} :> { a, Pattern[ j, Blank[] ], b };
      On[RuleDelayed::rhs];

    (* compare length of generic coupling with model-coupling 
     *)
      If[ lencv=!=Length[rhs],
	  Message[ InitializeModel::incomp1, lhs, lencv ];
	  Return[$Aborted] 
	];

    (* we transpose rhs so we obtain { {a[0],b[0],...}, {a[1],b[1],..},.. },
       i.e. we can check for counterterms 
     *)
      CTstruct = CTList = Transpose[rhs];

    (* write "0" if only zeroes and "i+1" if ith order is present
     *)
      Do[ CTstruct[[i]]=If[ MatchQ[CTstruct[[i]],{p:(0)..}], 0, i ],
          {i, Length[CTstruct]}
        ];

    (* set TheC:
    *)
      SetProperty[ TheC, Sequence @@ lhs, rhs ];
      Do[
         SetDelayed[ TheC[ Sequence@@((CouplingVector@@genref)[[i]])
                         ][ Sequence@@lhs ], 
                     Evaluate[rhs[[i]]]
                   ],
         {i, lencv}
        ];

    (* loop over counterterm orders to define Diagonal:
    *)
     Do[
        If[ CTstruct[[i]] =!= 0,
           If[ Head[CTList[[i]]] === List,
               unsel = Union[
                  DeltaSelect/@ Select[ Flatten[CTList[[i]]], (#=!=0)& ] ];
               If[ Length[unsel] === 1,
                   rhs = unsel[[1]],
                   rhs = 1 ],
              (*else*)
              rhs = DeltaSelect[CTList[[i]]]
             ];
           SetProperty[ Diagonal, FieldPoint[ CTstruct[[i]]-1 ] @@ #, rhs 
                      ]& /@  CPermutations[lhs]
          ],
        {i, Length[CTstruct]}
      ];

      CTstruct = Select[ CTstruct, #=!=0& ];
      Return[
             sequence@@(
              (FieldPoint[#]@@
                (vert/.{s_. (fi:F|S|V|U|VS|SV)[i_,j_]:>s fi[i]} )
              )& /@ (CTstruct-1) )
            ]
     ];

(* find the generic reference order of a field point:
*)
GenericRef[ _[ fi__ ] ] :=
  Flatten[
   ReferenceOrder[Generic][[
     Flatten[ Position[ GenericFieldPoints[],
                        FieldPoint@@ Sort[ ToGeneric[{fi}/.Pattern->Sequence/.
                                                           Blank -> Sequence
                                                    ] /. f:(-P$Generic):>-f
                                         ]
                      ]
            ]
   ]]
 ];

(* For the Diagonal-function we need fast access to Permutations:
*)
FPermutations[n_Integer] := FPermutations[n] = 
   Permutations[ Table[ i, {i, n} ] ];

CPermutations[ C[ fi__ ] ] :=
   Function[z, C@@ (Part[ {fi}, # ]& /@ z) ] /@ FPermutations[Length[{fi}]];

(* Construct PossibleFields from the vertexlists
 *) 
AssignPossibleFields[cto_][ { {fi_,vert_}, pospart_List } ] :=
  Block[{hold = PossibleFields[cto][fi,vert], holdmix},
        Set[ PossibleFields[cto][fi,vert], 
	     Union[ Join[ hold, pospart ] ] ];
	If[ fi === 0,
	    Set[ PossibleFields[cto][fi,vert],
		 Union[ Join[ hold, pospart, AllMixers[ pospart[[1]] ] ] ] ]
          ];
	If[ $SVMixing === True,
            If[ fi === S, 
	        holdmix = PossibleFields[cto][VS,vert];
                Set[ PossibleFields[cto][VS,vert], 
	             Union[ Join[ holdmix, AllMixers[ pospart[[1]] ] ] ] 
	           ];
	      ];
            If[ fi === V, 
	        holdmix = PossibleFields[cto][SV,vert];
                Set[ PossibleFields[cto][SV,vert], 
	             Union[ Join[ holdmix, AllMixers[ pospart[[1]] ] ] ] 
	           ];
	      ]
	 ]
       ];

(* call AssignPossibleFields
*)

(* ANCIENT form of AllCombinations, kept for a two-vertex so you can see
 * what AllCombList is doing:
 *
 * AllCombinations[mod_][ f1_, f2_, h1_, h2_ ] := 
 * AssignPossibleFields[mod] /@ {
 *   { { h2, FieldPoint[ h2,f1 ] } , {f2} },   (* h2 is valence => append f2 *)
 *   { { h1, FieldPoint[ h1,f2 ] } , {f1} },
 *   { { h1, FieldPoint[ h1,h2 ] } , {f1} },
 *   { { h2, FieldPoint[ h1,h2 ] } , {f2} }  };
 *
 * SetPossibleFields["Generic"][ FieldPoint[ f1:_Symbol, f2:_Symbol ] ] :=
 *  AllCombinations["Generic"][ f1, f2, 0, 0 ];
 *
 * SetPossibleFields[mod_][ FieldPoint[ f1:_. h1_[_Integer], 
 *                                      f2:_. h2_[_Integer] ] ] := 
 *   AllCombinations[mod][ f1, f2, h1, h2 ];
 *
 * end ANCIENT 
 *)

(*
AllCombinations[mod_, cto_][ f_List, h_List ] :=
  AssignPossibleFields[mod,cto] /@ AllCombList[ f, h ];
*)
AllCombinations[cto_][ f_List, h_List ] :=
  AssignPossibleFields[cto] /@ AllCombList[ f, h ];

(* Set for generic:
 *)
(* TEST : PossibleFields without odel specification *)
(*
SetPossibleFields["Generic", cto_][ f:FieldPoint[ P$Generic.. ] ] :=
  AllCombinations["Generic", cto][ List @@ f, Array[ 0&, Length[f] ] ];
*)
SetPossibleFields[ cto_][ f:FieldPoint[ P$Generic.. ] ] :=
  AllCombinations[cto][ List @@ f, Array[ 0&, Length[f] ] ];

(* Set for classes:
 *)
(*
SetPossibleFields[mod_,cto_][ f:FieldPoint[ P$Class.. ] ] := 
  AllCombinations[mod,cto][ List @@ f, 
			    List @@ ( ToGeneric[#]& /@ f )  ];
*)
SetPossibleFields[cto_][ f:FieldPoint[ P$Class.. ] ] := 
  AllCombinations[cto][ List @@ f, List @@ ( ToGeneric[#]& /@ f )  ];

(* 
 * Part3: Initialize  Generic model.
 * For smaller models (e.g. pure QCD) it has to be possible to re-define
 * the Generic Model. Since the re-definition clears the PossibleFields-
 * function, we also have to clear the $InitializedModels list.
 *)

(* TakeOut is a should-be Mathematica standard function taking out m numbers
 * out of the n integers running from 1 to n. 
 *)

TakeOut[ 1, n_Integer ] := Array[ {#}&, n];

TakeOut[ m_Integer ,n_Integer ] :=
   Join @@ Function[ z,
		     Append[ z, # ]& /@ Table[ i, {i,Last[z]+1,n} ]
		   ] /@ TakeOut[ m-1, n ];

(* this amazing function builds the SetDelayed-function for AllCombinations
 * with all combinations of valence configurations of a vertex with n legs 
 *)

FormAllCombList[ n_Integer ] := 
   Block[ { f = Array[ ToExpression[ StringJoin[ "f", ToString[#] ] ]&, n ],
            h = Array[ ToExpression[ StringJoin[ "h", ToString[#] ] ]&, n ],
	    res = {} },
	  Do[ list = TakeOut[ i1, Length[f] ];
	      Do[ v = f;
		  Do[ v = ReplacePart[ v, h[[ list[[ i2, i3 ]] ]],
					  list[[i2,i3]] ],
		      {i3,1,Length[ list[[i2]] ]} ];
		  res = 
		    Join[ res, { { h[[#]], FieldPoint@@v }, {f[[#]]} }& /@ 
			       list[[i2]] ],
		  {i2,1,Length[list]} ],
	      {i1,1,Length[f]} ];
          SetDelayed[ Evaluate[ AllCombList[ Pattern[ #, Blank[] ]& /@ f,
					     Pattern[ #, Blank[] ]& /@ h ] ],
		      Evaluate[ res ]
                    ]
        ];

GetGeneric[ s_. fi_[__] ] := fi;

InitializeGenericModel[ mod_, genfile_ ] :=
Block[ { lp, lc, gmod },

  (* no Global symbols allowed for these operations 
   *)
    $ContextPath =
      Delete[ $ContextPath, Position[$ContextPath, "Global`" ] ];
	FAPrint[2, " "];
        FAPrint[2," ... initializing "<> "generic model \""<>mod<>"\""];

  (* tabula rasa: 
   *)
    Clear[ AnalyticalPropagator ];
    Clear[ AnalyticalCoupling ];
    Clear[ ReferenceCoupling ];
    Clear[ CouplingVector ];
    Clear[ M$LastGenericRules ];
    Clear[ M$FermionFlipRule ];
    Clear[ M$TruncationRules];

  (* read generic model file: 
   *)
    FAPrint[2, "  ... reading generic file:"];
    FAPrint[2, "   <- \""<>genfile<>"\""];
    Off[Syntax::newl]; 
    Get[genfile];
    On[Syntax::newl];

    lp = M$GenericPropagators; 
    lc = M$GenericCouplings;
    gmod = $GenericModel;
   (* hold back $GenericModel *)
    $GenericModel = "";
   (* Particles and FieldPoints: *)
    F$AllGeneric = Union[ GetGeneric[ #[[1,1]] ]& /@ lp ];
    F$Generic = Union[ GetGeneric[ #[[1,1]] ]& /@ lp ];
    ReferenceOrder[Generic] = 
	 (List @@ (GetGeneric /@ #[[1]]) )& /@ lc;
    GenericFieldPoints[] = 
         (FieldPoint@@#)& /@ ReferenceOrder[Generic];
    Clear[ PossibleFields ];
    Clear[ CheckFieldPoint ];
    Clear[ AllCombList ];

  (* build the function AllCombList that forms all possible combinations 
   *)
    FormAllCombList[ # ]& /@ Union[ Length[#]& /@ GenericFieldPoints[]];

   (* the new Generic model: 
    * CheckFieldPoint must yield True except for complete generic or 
    * complete halfgeneric or complete explicit vertices which were not 
    * explicitely set to True with help of FieldPoints[...] in the 
    * initialization 
    *)
    CheckFieldPoint[ v_ ] := False /; MatchQ[ v, FieldPoint[_][P$Generic..] ];
    CheckFieldPoint[ v_ ] := False /; MatchQ[ v, FieldPoint[_][P$Class..] ];
    CheckFieldPoint[ v:FieldPoint[_][ P$Particle.. ] ] := 
       CheckFieldPoint[ Sort[ToClasses[v]] ];
    CheckFieldPoint[ _ ] := True ;
    M$FermionFlipRule[__] := {};

   (* we have to take care to have generic field points on any ct-order,
    * therefore we need this definition before the next one 
    *)
    SetDelayed[ 
      PossibleFields[_Integer?Positive][0, fp:FieldPoint[(0|P$Generic)..] ], 
      PossibleFields[0][0, fp ]
     ];
    SetDelayed[ 
      PossibleFields[_][_, FieldPoint[__] ], 
      {} 
     ];

   (* now, the `real` definitions follow: 
    *)
    SetPossibleFields[0][#]& /@ GenericFieldPoints[];
    Set[CheckFieldPoint[#], True]& /@ 
        ( (FieldPoint[0]@@#)& /@ GenericFieldPoints[]);

  (* and now we check $SVMixing: 
   *)
    FAPrint[ 2, "   $SVMixing is ", If[ $SVMixing, "ON", "OFF" ] ];
    If[ $SVMixing === True,
        LeftPartner[SV] = S;
    	LeftPartner[VS] = V;
    	RightPartner[SV] = V;
    	RightPartner[VS] = S;
    	AllMixers[V] = {SV};
    	AllMixers[S] = {VS},
    	AllMixers[V] = {};
    	AllMixers[S] = {}
      ];

  (* Propagators and Couplings 
   *)
    SetDelayed[ Evaluate[ AddFieldPattern /@ #[[1]] ],
                Evaluate[ PV[ #[[2]] ] ]               ]& /@ lp;
    InitGenericCoupling /@ lc;
    AppendTo[$ContextPath, "Global`"];
    $GenericModel = mod;
    FAPrint[ 1, " -- generic model \""<>mod<>"\" initialized --"];
   ];

(* Change field representation to patterns. We have to take care, that
 * only _Symbols can be changed to patterns i.e. after i=foo the thing
 * still works, after i=2 it won't.
 *)

Off[ RuleDelayed::rhs];

ToBlank[ i_Symbol ] := Pattern[ i, Blank[] ];

ToBlankSequence[ i_Symbol ] := Pattern[ i, BlankSequence[] ];

ToBlank[ i_ ] := (Message[InitializeModel::nosymb,FullForm[i]];
                  Interrupt[]);

ToBlankSequence[ i_ ] := (Message[InitializeModel::nosymb,FullForm[i]];
                          Interrupt[]);

AddFieldPattern[ fi_[ i_, m_ ] ] := 
      fi[ ToBlankSequence[i], ToBlank[m] ]; 

AddFieldPattern[ fi_[ i_, m_, {ki__} ] ] := 
  Append[ AddFieldPattern[ fi[i,m] ], ToBlank /@ {ki} ];

AddFieldPattern[ fi_[ i_, m_, kir_Rule ] ] := 
  Append[ AddFieldPattern[ fi[i,m] ], Map[ ToBlank, kir, {2} ] ];

AddFieldPattern[ s_ f:_[___] ] := 
  Optional[ToBlank[s]] * AddFieldPattern[ f ];

(* this rule puts Mom and KI dummies in the fields. These dummies will appear
 * as part of the Lorentz term indexing of the G's.
 *)

UMomLiRule[ n_Integer ] := 
    { s_. (fi:V|S|U|F|VS|SV)[i__,m_,{ki__}] :> 
           s fi[  i, Mom[n], Array[ ToExpression[ "KI" <> ToString[#] ][n]&,
				    Length[{ki}] ]  ],
      s_. (fi:V|S|U|F|VS|SV)[i__,m_] :> s fi[ i,Mom[n] ] };

(* transformation of the Equal's of the generic model to SetDelayed's:
 * lhs has to be changed to patterns,
 * rhs is changed from G * (g1, g2, ..) to (G[g1], G[g2], ..)*(g1, g2, ..),
 * both lhs and rhs get an additional argument: the counterterm order,
 * in addition, the folowing functions are defined:
 *    ReferenceCoupling which gives the original coupling definition of the 
 *                      generic model file 
 *    KinematicalVector which gives only the "Gamma"-part of the coupling
 *    CouplingVector which gives only the "G"-Part of the coupling
 *)

InitGenericCoupling[ (AC:AnalyticalCoupling[f___]) == rhs_ ] :=
Block[ {lhs, vec, noncom=False, ref },

      (* construct generic reference order:
       *)
        ref = {f} /.  _.(gf:P$Generic)[__] :> gf;

      (* change lhs to patterns and add ct$o_ for the counterterm order 
       *)
       lhs = 
         ACoup[ Pattern[ Global`ct$o, Blank[] ] ] @@ (AddFieldPattern /@ AC);

      (* is the whole coupling NonCommutative? 
       *)
(* HERE: does this work? *)
       If[ Head[rhs]===NonCommutative,
           rhs = rhs[[1]]; noncom = True ];

      (* we have to find coupling definitions of the form G . {anything}
       *)
       If[ !MatchQ[ rhs, G[_][__] . {__} ],
           Message[InitializeModel::rhs2, ref ]
         ];
 
      (* check for correct ordering: no G's in the second element 
       *)
       If[ !FreeQ[ rhs[[2]], G[_][__] ],
           Message[ InitializeModel::rhs1, ref ]
         ];
(*
DePrint["|- InitGenericCoupling lhs = ",lhs];
DePrint["|- InitGenericCoupling rhs = ",rhs];
*)
      (* definition of ReferenceCoupling (already with ct$o) : 
       *)
       SetDelayed[ Evaluate[lhs/.ACoup[_]:>ReferenceCoupling], 
                   Evaluate[rhs/.G[sym_]:>G[sym][Global`ct$o] ] ];
	  
      (* rhs with dummy kinematic indices:
       *)
       newmomexp = ReferenceCoupling @@ 
	         ( Array[ {f}[[#]] /. UMomLiRule[#] &,
		          Length[{f}] ] ); 
       If[ MatchQ[ newmomexp, _. G[_][_][__] ],
           newmomexp = 
             newmomexp /. factor_.*theG:G[_][_][__]:> theG . {factor}
         ]; 
(*
DePrint["|- InitGenericCoupling newmomexp = ", newmomexp ];
*)
      (* setting the CouplingVector:
       *)
       Set[ Evaluate[ CouplingVector@@ref ],
            newmomexp /.Dot->Map
          ];

      (* definition of AnalyticalCoupling:
       *)
       SetDelayed[ 
         Evaluate[ lhs /. ACoup -> AnalyticalCoupling              ], 
         Evaluate[ If[noncom,
                      PV[NonCommutative[
                         (newmomexp[[1]][#]& /@ newmomexp[[2]]). 
                          rhs[[2]]      ]    ],
                      PV[(newmomexp[[1]][#]& /@ newmomexp[[2]]).
                          rhs[[2]]      ]
                     ]         
                ] 
           ]
   ];

On[ RuleDelayed::rhs];

(* general Generic definitions:
 *)

Appearance[ Index[ Generic, i_Integer ] ] := Alph[ i ];

PropagatorLabel[V|S|F|U|VS|SV] = "";

PropagatorLabel[ _. (fi:V|S|F|U|VS|SV)[ Index[ Generic, i_Integer ] ] ] :=
  ComposedChar[ {ToString[fi], Alph[i]} ];

PropagatorType[V]  = Sine;
PropagatorType[S]  = Dashing[ {0.01, 0.01} ];
PropagatorType[F]  = Straight;
PropagatorType[U]  = Dashing[{0.003, 0.005}];
PropagatorType[SV]  = { Dashing[ {0.01, 0.01} ], Sine };
PropagatorType[VS]  = { Sine, Dashing[ {0.01, 0.01} ] };

PropagatorArrow[V|S|F|U] = None;
PropagatorArrow[SV] = Forward;
PropagatorArrow[VS] = Backward;

(*
 * :Part4:
 *	RestrictCurrentModel.
 * 	This function accepts an arbitrary number of ExcludeVertices or
 *	ExcludeParticles settings and either sets the CheckVertex-functions
 *	for the vertices to "False" or deletes the fields from the
 *	F$xxx-lists
 *      RestrictCurrentModel[] removes all restrictions from the current
 *	model
 *)

RestrictCurrentModel[] := RestrictCurrentModel[ True ][{}];

RestrictCurrentModel[ args:(_Rule|_List).. ] := 
                          RestrictCurrentModel[False][ args ];

RestrictCurrentModel[ False ][ args__ ] := CuT[ args ];
RestrictCurrentModel[ True ][  args__ ] := TuC[ args ];

CuT[ args__ ] :=
Block[ { flatlist, exclV, exclP, minusG, minusC, minusP},
      flatlist = Flatten[ {args} ];
      flatlist = 
       Select[ flatlist,
               ( If[ Not[ MatchQ[#,
                          Rule[(ExcludeParticles|ExcludeFieldPoints),_]]],
                    Message[InitializeModel::badrestr, #];
                    False,
                    True   ]
               )&
             ];
    (* exclude fields: *)
      exclP = Union@@ Cases[ flatlist, Rule[ ExcludeParticles, p_ ] -> p ];
      exclP = Union[ exclP, AntiParticle /@ exclP ];
      F$Generic = Complement[ F$Generic, exclP ];
      F$Classes = Complement[ F$Classes, exclP ];
      F$Particles = Complement[ F$Particles, exclP ];
      minusG = Length[F$AllGeneric] - Length[F$Generic];
      minusC = Length[F$AllClasses] - Length[F$Classes];
      minusP = Length[F$AllParticles] - Length[F$Particles];
       F$AllowedFields = Complement[ F$AllowedFields, exclP ];
       If[Length[exclP] > 0,
         FAPrint[2, " Excluding ", Length[exclP], " field",
                    If[Length[exclP]===1,"","s"]," for this run."]; 
         FAPrint[3, "    Generic Fields excluded: ", minusG ];
         FAPrint[3, "    Classes Fields excluded: ", minusC ];
         FAPrint[3, "  Particles Fields excluded: ", minusP ]
        ];
      
    (* exclude field points: *)
      exclV = Union@@ Cases[ flatlist, Rule[ ExcludeFieldPoints, p_] -> p ];
      If[ CheckFieldPoint[Sort[#]]===False,
           FAPrint[2, "  warning: " <> ToString[#]
                      <> " is already turned off."]
        ]& /@ exclV;
      exclV = Flatten[
        If[ MatchQ[ #, FieldPoint[__] ],
            FAPrint[2, " Excluding ", #, " at order(s) ", L$CTOrders ];
            Function[z, FieldPoint[z]@@# ]/@ L$CTOrders,
            #
          ]& /@ exclV
        ];
      If[Length[exclV] > 0 ,
         Off[RuleDelayed::rhs];
         exclV = Union[ Sort/@exclV, Sort[AntiParticle/@#]&/@ exclV
                      ] //. {a___, j_Symbol, b___}:>{a, Pattern[j,Blank[]], b}; 
         FAPrint[2, " Excluding ", Length[exclV], " field point",
                    If[Length[exclV]===1,"","s"],
                     " (incl. charge conjugate ones)." ];
         Set[ CheckFieldPoint[ Sort[#] ], False ]& /@ exclV;
         On[RuleDelayed::rhs]
        ];
    Return[ {exclV, exclP} ]
  ];

TuC[ args__ ] :=
Block[ { flatlist, inclV, inclP, cto=0, stop=False },
      flatlist = Flatten[ {args} ];
    (* remove all restrictions? *)
      If[ flatlist === {},
          inclP = 
            Complement[ Union[Join[F$AllParticles,F$AllClasses,F$AllGeneric]],
                        F$Particles, F$Classes, F$Generic ];
          inclV = {};
          While[ !stop,
                If[ MatchQ[ FieldPoints[cto], _List ],
                    AppendTo[ inclV, 
                              (FieldPoint[cto]@@#)& /@ FieldPoints[cto] ];
                    cto += 1,
                    stop = True
                  ]
               ];
          flatlist = { ExcludeFieldPoints -> Flatten[inclV],
                       ExcludeParticles -> inclP }
        ];
      flatlist = 
       Select[ flatlist,
               ( If[ Not[ MatchQ[#,
                          Rule[(ExcludeParticles|ExcludeFieldPoints),_]]],
                    Message[InitializeModel::badrestr, #];
                    False,
                    True   ]
               )&
             ];

    (* include fields: *)
      inclP = Union@@ Cases[ flatlist, Rule[ ExcludeParticles, p_ ] -> p ];
      inclP = Union[ inclP, AntiParticle /@ inclP ];
      F$Generic = 
        Union[Join[ F$Generic, Select[inclP,MatchQ[#,P$Generic]&]]];
      F$Classes = 
        Union[Join[ F$Classes, Select[inclP,MatchQ[#,P$Class]&]]];
      F$Particles = 
        Union[Join[ F$Particles, Select[inclP,MatchQ[#,P$Particle]&]]];
      F$AllowedFields = Union[Join[ F$AllowedFields, inclP ]];
      FAPrint[2, " Revived ", Length[inclP], " field",
                  If[Length[inclP]===1,"","s"],"."]; 
      
    (* include field points: *)
      inclV = Union@@ Cases[ flatlist, Rule[ ExcludeFieldPoints, p_] -> p ];
      inclV = Flatten[
        If[ MatchQ[ #, FieldPoint[__] ],
            FAPrint[2, " Excluding ", #, " at order(s) ", L$CTOrders ];
            Function[z, FieldPoint[z]@@# ]/@ L$CTOrders,
            #
          ]& /@ inclV
        ];
      If[Length[inclV] > 0 ,
         Off[RuleDelayed::rhs];
         inclV = Union[ Sort/@ inclV, Sort[AntiParticle/@#]&/@ inclV
                      ] //. {a___, j_Symbol, b___}:>{a, Pattern[j,Blank[]], b}; 
         Set[ CheckFieldPoint[ Sort[#] ], True ]& /@ inclV;
         FAPrint[2, " Revived ", Length[inclV], " field point",
                    If[Length[inclV]===1,"","s"],
                     " (incl. charge conjugate ones)." ];
         On[RuleDelayed::rhs]
        ];
    Return[ {inclV, inclP} ]
  ];

End[] (* "HighEnergyPhysics`FeynArts`Initialize`" *)
