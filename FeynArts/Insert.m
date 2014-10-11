 
(* :Title: Insert *)

(* :Authors: Hagen Eck, Sepp Kueblbeck *)

(* :Summary: 
	Insertion of fields into topologies created by CreateTopologies.
	The insertion is done in 3 levels: insertion of generic fields
	(Generic), of classes of a certain model (Classes) or of
	the members of the classes (Particles).
	Models are described in model files which are supposed to exist
	in the directory HighEnergyPhysics`FeynArts`Models`. At the 
	beginning of an insertion InsertFields calls
	HighEnergyPhysics`FeynArts`Initialize`InitializeModel`  that checks 
	whether the model is initialized or not and performs the initialization 
	if needed.
*)

(* :Context: HighEnergyPhysics`FeynArts`Insert` *)

(* :Package Version 2.1 *)

(* :Mathematica Version 2.0 *)

(* :History:
	Created Apr, 1993 from the InsertFields-package of FeynArts 1.0.
*)

(* :Remarks:
*)

(* :Contents:
	Part1: InsertFields main function
	Part2: Utility-functions for InsertFields
	Part3: TopologyInsert (insertion routine for one topology)
	Part4: TreatModel (model treatment, switch couplings on/off)
	Part5: DoInsert (nested calls of InsertFields-algorithms)
	Part6: InsertionsCompare (Utilities`Compare for inserted topologies)
        Part7: PickLevel
	(search for Part# to find the beginning of a section)
*)

FAPrint[3,"[Insert.m]"];

Begin["`Insert`"];

Options[ InsertFields ] = { Model		-> "SM",
			    GenericModel	-> "Lorentz",
			    InsertionLevel	-> Classes,
			    ExcludeParticles	-> {}, 
			    ExcludeFieldPoints	-> {},
			    Restrictions	-> {},
			    RemoveEmptyTops	-> True,
			    FieldSpecification	-> {},
			    ProcessName		-> Automatic,
			    LastSelections 	-> {}
			  } 

(* error messages:
*)

InsertFields::extnumber = 
"You cannot fit `1` -> `2` external particles onto a `3` -> `4` leg topology."
			  
InsertFields::badrestriction = 
"Restrictions `1` are not defined in the loaded model files. They have no 
effect."

InsertFields::badvertex = 
"Warning: Vertex `1` of models `2` is already turned off. "

InsertFields::badparticle = 
"Particle `1` does not live in model(s) `2`. "

InsertFields::shortspec =
"Only `1` field specifications for `2` topologies (ignoring all of them)."

InsertFields::longspec =
"`1` field specifications for `2` topologies (using the first `2` only)."

InsertFields::fspeccont =
"FieldSpecification for Field[`1`] contradictory: `2` <-> `3` (using `2`).";

InsertFields::nospec =
"Can't find any field specifications! (Aborting).";

InsertFields::extoio =
"Warning: changing External to Incoming/Outgoing."

InsertFields::genopt =
"Option \"GenericModel\" has to specify a String!";

InsertFields::modopt =
"Option \"Model\" must be a String or of the form {String, Options..}!";

InsertFields::badsel =
"Element of LastSelections (`1`) is not a proper field specification.";

InsertFields::fspeclen =
"Value of option FieldSpecification has not the same length like topology
list (using \"{}\").";

InsertFields::fspecform =
"Can't handle the value of option FieldSpecification (using \"{}\").";

(* 
  :Part1: InsertFields main function 
  	  The standard form for InsertFields calls is:
	  InsertFields[ TopologyList[...], { {Field[i]->pi,...}, ... } ],
	  where the list of field specifications is of the same length as
	  the list of topologies.
*)

(* pattern abbreviations: 
*)
TopSpec = (Topology[__]|Topology[_][__]);
IOSpec = ({___}->{___});
IOListSpec = ({IOSpec..});
FieldSpec = ({(Field[_Integer]->_)..});
FieldListSpec = ({FieldSpec..});

(* one topology -> topologylist 
*)
InsertFields[ t:TopSpec, arg__ ] :=
	InsertFields[ TopologyList[t], arg ];

(* one i/o-specification -> list 
*)
InsertFields[ t_TopologyList, io:IOSpec, opt___Rule ] :=
	InsertFields[ t, Table[ io, {Length[t]} ], opt ]

(* list of i/o-specifications -> list of fieldspecifications
*)
InsertFields[ t_TopologyList, io:IOListSpec, opt___Rule ] :=
	(Message[ InsertFields::shortspec, Length[io], Length[t] ];
	 $Aborted
	) /; Length[io]<Length[t];
InsertFields[ t_TopologyList, io:IOListSpec, opt___Rule ] :=
	(Message[ InsertFields::longspec, Length[io], Length[t] ];
	 InsertFields[ t, Take[ io, Length[t] ], opt ]
	) /; Length[io]>Length[t];
InsertFields[ t_TopologyList, io:IOListSpec, opt___Rule ] :=
	InsertFields[ If[ !FreeQ[t[[1]], External],
			 TopologyList@@( (ExToIO@@#)& /@ 
			   Transpose[{List@@t,Length[#[[1]]]&/@io}]),
			 t ],
		      (ExtToField@@#)& /@ Transpose[{List@@t, io}],
		      opt ] /; Length[io]===Length[t];

(* change rule form of externals to fieldspecification
*)
ExtToField[ tt:TopSpec, io:IOSpec ] :=
 ExtToField[ ExToIO[tt, Length[io[[1]]]], io ] /; !FreeQ[tt, External];
ExtToField[ tt:TopSpec, io:IOSpec ] :=
	If[ (PropagatorCount[Incoming][tt]===Length[io[[1]]])&&
	    (PropagatorCount[Outgoing][tt]===Length[io[[2]]]),
	    Join[ Array[ (Field[#]->io[[1,#]])&, 
			 Length[io[[1]]] ],
		  Array[ (Field[#+Length[io[[1]]]]->AntiParticle[io[[2,#]]])&, 
			 Length[io[[2]]] ] ],
	    Message[InsertFields::extnumber, Length[io[[1]]], Length[io[[2]]],
		    PropagatorCount[Incoming][tt],
		    PropagatorCount[Outgoing][tt] ];
	    $Aborted ];
		     
(* for topologies with External propagators only: 
*)
ExToIO[ tt:TopSpec, in_Integer ] :=
	(Message[InsertFields::extoio];
	 tt //. Propagator[External][Vertex[1][j_], v_] :>
	        If[ j>in, Propagator[Outgoing][Vertex[1][j],v],
	 		  Propagator[Incoming][Vertex[1][j],v] ]
	);

(* field-specification -> list of field-specifications
*)
InsertFields[ t_TopologyList, f:FieldSpec, opt___Rule ] :=
	InsertFields[ t, Table[ f, {Length[t]} ], opt ];

(* list of field-specifications too short
*)
InsertFields[ t_TopologyList, f:FieldListSpec, opt___Rule ] :=
	(Message[InsertFields::shortspec, Length[f], Length[t] ];
	 $Aborted
	) /; Length[f]<Length[t];

(* list of field-specifications too long
*)
InsertFields[ t_TopologyList, f:FieldListSpec, opt___Rule ] :=
	( Message[InsertFields::longspec, Length[f], Length[t] ];
	  InsertFields[ t, Take[ t, Length[t] ], opt ]
	) /; Length[f]>Length[t];

(* no external specification (maybe FieldSpecification is used)
*)
InsertFields[ t_TopologyList, opt___Rule ] :=
	If[ FreeQ[{opt}, FieldSpecification],
	    Message[InsertFields::nospec];
	   $Aborted,
	   InsertFields[ t, Table[{},{Length[t]}], opt ]
	  ];

(* standard call (main function)
*)
InsertFields[ t_TopologyList, f:FieldListSpec, opt___Rule ] :=
Block[ { OPT = ActualOptions[ InsertFields, opt ], ilevel,
	 nrT = Length[t], iLevel, exclVP, exclV, exclP, fspec, fields, 
	 mod, genmod, result, insli = {}, intermed, lastsel },

	(* model information: 
         *)
	 { mod, genmod } = { Model, GenericModel }/.OPT;

	(* check form of model specification: 
         *)
	 If[ !MatchQ[ genmod, _String ],
	     Message[InsertFields::genopt ];
	     Return[$Aborted]
	   ];
	 If[ !MatchQ[ mod, P$Model ],
	     Message[InsertFields::modopt];
	     Return[$Aborted]
	   ];

	(* fields with FieldSpecification: *)
         fspec = FieldSpecification/.OPT;
         fspec = Which[
           MatchQ[ fspec, FieldSpec ],
           Table[ fspec, {Length[t]} ],
           MatchQ[ fspec, FieldListSpec ],
           If[ Length[ fspec ] =!= Length[t],
               Message[InsertFields::fspeclen]; Table[{}, {Length[t]}],
               fspec
             ],
           fspec==={}, Table[{}, {Length[t]}], 
           True, Message[InsertFields::fspecform]; Table[{}, {Length[t]}]
          ];
        (* the complete field-contents: *)
	 fields = 
	  Union[ #[[2]]& /@ Flatten[ Join[ f, fspec ]]];
	(* initialize, check fields, check vertices to exclude: *)
	If[ (exclVP = TreatModel[ mod, genmod, fields, OPT ]) === $Aborted,
	   Return[$Aborted],
           exclV = exclVP[[1]]; exclP = exclVP[[2]]
	  ];

        (* include restrictions on particles level to LastSelections: *)
        OPT = OPT /. Rule[ LastSelections, a_List ] :>
            Rule[ LastSelections, 
                  Union[ Join[ a, Not /@ 
                         (Select[exclP, MatchQ[#, P$ClassMember]& ]) ]
                       ]
                ];

	(* a message: *)
	ilevel = InsertionLevel /. OPT;
	FAPrint[2, " ... starting insertion process ", 
		   If[ Head[ilevel]===List,
		      If[ Length[ilevel]>1, "on levels ", "on level " ],
		      "down to level "
		     ], ilevel ];
	(* call TopologyInsert for every Topology: *)
	Which[ ilevel===Generic, ilevel={Generic},
	       ilevel===Classes, ilevel={Generic, Classes},
	       ilevel===Particles, ilevel={Generic, Classes, Particles }
	     ];
	Do[ 
	    FAPrint[2, "  inserting Topology ", i, "/", nrT ];
	    If[$Verbose==2, Print[ " ",i ,"/", nrT ] ];
	    intermed =  
              TopologyInsert[ t[[i]], MergeFields[f[[i]], fspec[[i]]], OPT ];
	    AppendTo[ insli, intermed ];
	    If[$Verbose==1,
	     Which[ !FreeQ[ ilevel, Particles ],
	             Print["  -> ", Length[intermed[[2]]],
		           "/", Plus@@ ( Length/@ Cases[intermed[[2]],
			            Insertions[Classes][__], Infinity] ),
		           "/", Plus@@ ( Length/@ Cases[intermed[[2]],
			            Insertions[Particles][__], Infinity ] ) ],
		    !FreeQ[ ilevel, Classes ],
	             Print["  -> ", Length[intermed[[2]]],
		           "/", Plus@@ ( Length/@ Cases[intermed[[2]],
			            Insertions[Classes][__], Infinity] ) ],
		    !FreeQ[ ilevel, Generic ],
	             Print["  -> ", Length[intermed[[2]]] ]
		  ]
	      ],
	    {i, nrT}
	  ];
	(* put a ProcessName into options list if not given yet *)
        If[ (ProcessName/.OPT) === Automatic,
	   OPT = OPT /. {Automatic->AutoProcessName[t[[1]], f[[1]]]}
	  ];
	OPT = 
	 Append[ Select[ OPT, FreeQ[#,ScreenMessages]& ],
                 Process -> AutoProcessSpec[ t[[1]], f[[1]] ]
	       ];
        (* put options into Head of output *)
  	insli = (TopologyList @@ OPT) @@ insli;
	(* restore old model: *)
  	ReviveFieldPoints[ exclV, mod ];
	(* discard empty topologies: *)
  	If[ (RemoveEmptyTops/.OPT) , 
	    result = RemoveEmpty[insli] ];
	(* print summary: *)
 	PrintNumInfo[ result, Length[result], nrT, ilevel ];
	(* delete intermediate levels: *)
	If[ FreeQ[ ilevel, Generic ],
	    result = result /. Insertions[Generic][ graphs__ ]:>
	      Flatten[ Insertions[Classes]@@ ( #[[2]]& /@ {graphs} ) ]
	  ];
	If[ FreeQ[ ilevel, Classes ],
	    result = result /. Insertions[Classes][ graphs__ ]:>
	      Flatten[ Insertions[Particles]@@ ( #[[2]]& /@ {graphs} ) ]
	  ];
	result = AddGraphNumbers/@result;

       (* restore model: *)
        RestrictCurrentModel[ True ][ ExcludeFieldPoints -> exclV,
                                      ExcludeParticles -> exclP ];

	Return[result]
   ];

(*
  :Part2: Utility functions for InsertFields.
*)

(* combine field specifications from the main argument and the
   option FieldSpecification:
*)
MergeFields[ io:FieldSpec, {} ] := io;
MergeFields[ io:FieldSpec, fs:FieldSpec ] :=
Block[ {mf},
       mf = Union[ Join[ io, fs ] ];
       mf = mf //. {a___, Field[n_]->X_, b___, Field[n_]->Y_, c___
                } :> (Message[InsertFields::fspeccont, n, X, Y];
                      {a, Field[n]->X, b, c})
     ]; 

(* form a process name from the user given fields:
*)
GetProcess[ t:TopSpec, f:FieldSpec ] :=
Module[ { name, out=False, outgoing = PropagatorCount[Outgoing][t] },
	name = Table[ i, {i, PropagatorCount[Incoming][t]} ];
	If[ name==={},
	    name = Table[ i, {i, PropagatorCount[External][t]} ],
	    If[ outgoing > 0,
                name = Join[ name, {"+"},
		             Table[ i, {i, outgoing} ] + Length[name]
		           ]
	      ]
	  ];
	Do[
	   If[ name[[i]]=!="+",
	       name[[i]] = Select[ f, MatchQ[ #, Field[name[[i]]]->_ ]& ];
	       If[ name[[i]]==={}, 
		   name[[i]] = "#",
		   name[[i]] = 
		    If[out, AntiParticle[name[[i,1,2]]], name[[i,1,2]] ]
		 ],
	       out=True
	     ],
	   { i, Length[name] }
	  ];
	Return[name]
 ];

FieldToString = 
  { s_. (fi:V|S|F|U)[i_,___] :>
     StringJoin[ ToString[ s fi ]/.{"-F"->"f", "-S"->"s", 
				    "-V"->"v", "-U"->"u"},
	         ToString[ i ]
	       ],
    s_. fi_Symbol :> ToString[ s fi ]/.{"-F"->"f", "-S"->"s",
					"-V"->"v", "-U"->"u"}
	       };

AutoProcessName[ t:TopSpec, f:FieldSpec ] :=
  StringJoin@@ ( GetProcess[ t, f ]/.FieldToString );

AutoProcessSpec[ t:TopSpec, f:FieldSpec ] :=
  ToExpression[ 
    StringReplace[ 
     StringReplace[ ToString[GetProcess[ t, f ]], " "->""], 
		   ",+,"->"}->{" ] 
   ] /. GlobalIndexRules /. { True->Identity, False->Identity }

(* remove topologies with no insertions: 
*)
RemoveEmpty[ tls_ ] :=
Block[ { le=Length[tls], del=0, back=Head[tls][], new },
         FAPrint[3, " ... removing empty topologies."];
	 Do[
	    new = tls[[i]]/.Rule->rule;
	    new = ReleaseHold[ 
		  new /. Insertions[Particles][] -> Hold[Sequence[]]];
	    new = new /. rule[ any_ ] :> any;
	    new = ReleaseHold[ 
		  new /. Insertions[Classes][] -> Hold[Sequence[]]];
	    new = new /. rule[ any_ ] :> any;
	    If[ new[[2]]===Insertions[Generic][],
		del += 1,
		AppendTo[ back, new/.rule->Rule ]
	      ],
	    {i, Length[tls]}
	   ];
	 Return[back]
       ];

(* print how many generic, class and particle insertions there are:
*)
PrintNumInfo[ re_, len_, toplen_, spec_ ] :=
Block[ {ilevel, gnum = 0, cnum = 0, pnum = 0},
	Which[ spec===Generic, ilevel = {Generic},
	       spec===Classes, ilevel = {Generic, Classes},
	       spec===Particles, ilevel = {Generic, Classes, Particles},
	       True, ilevel=spec
	     ];
	FAPrint[3, " "];
	FAPrint[3, " Summary:"];
  	FAPrint[3, " -----------------------------------------"<>
		   "--------------------------"];
  	Do[ 
	   gnum = gnum + Length[ re[[i,2]] ];
	   If[ !FreeQ[ ilevel, Classes ],
	      Do[ cnum = cnum + Length[ re[[i,2,j,2]] ],
	          {j,1,Length[ re[[i,2]] ]}
                ]
	     ];
 	   If[ !FreeQ[ ilevel, Particles ],
	      Do[ 
		 Do[ pnum = pnum + Length[ re[[i,2,j,2,k,2]] ],
		      {k,1,Length[ re[[i,2,j,2]] ]}
                    ],
                  {j,1,Length[ re[[i,2]] ]}
                ]
	     ],
      	    {i,1,Length[re]}
    	  ];
	FAPrint[2, " InsertFields returns ", len, " topolog",
		   If[ len===1, "y", "ies" ], " out of ", toplen, "."];
	If[ !FreeQ[ ilevel, Generic ],
            FAPrint[1, " ==>\t",gnum, "\t Generic insertion", 
		       If[gnum===1,"","s"] ]
	  ];
	If[ !FreeQ[ ilevel, Classes ],
            FAPrint[1, " ==>\t",cnum, "\t Classes insertion", 
		       If[cnum===1,"","s"] ]
	  ];
	If[ !FreeQ[ ilevel, Particles ],
            FAPrint[1, " ==>\t",pnum, "\t Particles insertion", 
		       If[pnum===1,"","s"] ]
	  ];
	FAPrint[1, " "];
 ];

(* add numbers to the Heads of each Graph, i.e. Graph[sym]->Graph[sym, num]
*)
AddGraphNumbers[ tt:Topology[p__]->inslist_ ] :=
     Topology[p] -> RecursiveNumbering[inslist];
NumberType[ x_, t_, n_ ] := x /. Graph[s_]:>Graph[s,t==n];
RecursiveNumbering[ 
  Insertions[type_][ a:((Graph[_][__]->_)..) ] ]:= 
Insertions[type]@@ Table[ NumberType[ {a}[[i,1]], type, i ] ->
                           RecursiveNumbering[ {a}[[i,2]] ] , 
                         {i,Length[{a}]} ];
RecursiveNumbering[ 
  Insertions[type_][ a:((Graph[_][__])..) ] ]:= 
Insertions[type]@@ Array[ ( {a}[[#]]/.Graph[s_]:>Graph[s,type==#] )&,
                          Length[{a}] ];

(* 
  :Part3: TopologyInsert (main routine)
	  insert fields into 1 topology with fieldspecifications as rules, 
	  i.e. AppendFieldsToTopology, Fold InitTopology, Fold Ins1
*)
TopologyInsert[ top:TopSpec, fields:FieldSpec, opt_List ] :=
Module[ { fieldtop, starttop, inittop,
	   fieldrules,
	   aftercomp, exclv, 
	   mod, genmod, ilevel, llevel,
	   fieldlist,
	   modelname, holdoptions, want, omit,
	   i = 1, uservertices, purevertices },
	mod = Model /. opt;
	genmod = GenericModel /. opt;
	ilevel = InsertionLevel /. opt;

       (* changing level-lists to single level: 
        *)
	If[ Head[ilevel]===List, 
	    llevel = ilevel/.{Generic->1,Classes->2, Particles->3};
	    llevel = Switch[ Max[llevel], 1, Generic, 2, Classes,
			     3, Particles ],
	    llevel = ilevel
	  ];
	fieldlist = (#[[2]]& /@ fields);
	fieldtop = 
	  If[ FreeQ[ top,Field ],
              AppendFieldsToTopology[ top ],
	      top
            ];

       (* insert specified particles and provide indices: 
        *)
  	inittop = Fold[ InitTopology, { {}, fieldtop }, fields ];
	GlobalIndexRules = inittop[[1]];
	starttop = inittop[[2]] /. GlobalIndexRules;

       (* check every full vertex whether it really exists: 
        *)
  	purevertices = 
	 Select[ Union[ Flatten[ 
		  FindFieldPoints[ Drop[starttop,-1], #[[1,1]] 
			      ]& /@ ( List @@ Last[starttop][[1]] ) ] ],
                 (Length[#]>1)& ];
	uservertices = purevertices /. List@@Last[starttop][[1]];

       (* make last selections: 
        *)
        want = Flatten[ Select[ LastSelections /. opt, (Head[#]=!=Not)& ] ];
        omit = Flatten[ Select[ LastSelections /. opt, (Head[#]===Not)& 
                              ] /. Not->List ] ;
	want = Select[ CheckProperField /@ want, (Head[#]=!=trash)& ];
	omit = Select[ CheckProperField /@ omit, (Head[#]=!=trash)& ];
        want = NoSign/@want;
        omit = Union[ Join[ omit, AntiParticle/@omit ] ];
(*
Print[" TopologyInsert: want = ",  want];
Print[" TopologyInsert: omit = ",  omit];
*)
       (* check `user-defined` vertices (those that are filled from 
	* the beginning. If OK -> start insertion process  
        *)
	If[ CheckGraph[ {}, uservertices ],
	    aftercomp = 
	     DoInsert[llevel][ Last[starttop], Drop[starttop,-1], purevertices,
			       want, omit ],
      	    aftercomp = Insertions[Generic][];
            FAPrint[3, " Topology contains a filled non-existing vertex." ] 
    	  ];

       (* output: put relevant options in head of output 
        *)
	Return[ ( Topology @@ Drop[starttop,-1] -> aftercomp 
		) //. GenericIndexRule ];
 ];

(* check for proper fields:
*)
CheckProperField[ fi_ ] :=
  If[ MatchQ[ fi, P$Generic|P$Class|P$Particle ],
      fi,
      Message[InsertFields::badsel, fi ]; 
      trash[fi]
    ];

NoSign[ _. s_Symbol ] := s;
NoSign[ _. s_Symbol[ any__ ] ] := s[any];

(* provide generic Indices in Graph's:
*)
GenericIndexRule = Rule[ Field[i_Integer], fi_Symbol ] :> 
		     Rule[ Field[i], fi[ Index[Generic,i] ] ];

(* provide fixed indices for outer non-specified particles:
*)
OuterIndex[ ind_Integer, s_. fi_ ] := {True->True};

OuterIndex[ ind_Integer, s_. fi_[i_] ] := {True->True};

OuterIndex[ ind_Integer, s_. fi_[i_,list_] ] := 
	Array[ If[ MatchQ[ list[[#]], _Symbol ],
		   list[[#]] -> True[ Append[ Indices[fi[i]][[#]], ind] ],
		   True -> True 
		 ]&, Length[list]
	     ];

(* put Field[i] in Graph -- all pi were given incoming thus no AntiParticle 
   necessery 
*)
InitTopology[ { ruli_List, t:Topology[h_][props__,inshead_[gras_] ] }, 
	      Rule[ Field[i_Integer], s_. part_] ] :=
 { (* first element: fixed index rules for unspecified indices *)
   If[ MatchQ[ Head[ {props}[[i]] ], Propagator[Incoming|Outgoing] ], 
       Join[ ruli, OuterIndex[i,part] ],
       ruli
     ],
   (* second element: topology with outer fields in insertion list *)
   Topology[h][ props,
		inshead[gras/.Rule[Field[i],0] -> Rule[Field[i],s part]]
	      ]
 };

(*
 * :Part4:
 *       Model Treatment.
 *)
(* is field p member of one of the models m?
 *)
InModelQ[ m:P$Model, p:P$Generic ] := MemberQ[ F$Generic, p ];

InModelQ[ m:P$Model, p:P$Class ] := MemberQ[ F$AllClasses, p];

InModelQ[ m:P$Model, p:(-_[_Integer, ___List])] := InModelQ[ m, -p];

InModelQ[ m:P$Model, p: (fi_)[ n_Integer, i_List]] :=
	InModelQ[ m, fi[n]] && 
	Apply[ And,
  	      (MatchQ[#1[[2]], _Symbol] || Apply[MemberQ, #1] & 
	      ) /@ Transpose[ {IndexRange /@ Indices[fi[n]], i}] 
	     ] /; Length[i] === Length[Indices[fi[n]]];

InModelQ[_, _] = False;

(* complete model treatment:
 *)
TreatModel[ mod:P$Model, genmod_String, fields_List, opt_List ] :=
Block[ { init, abo = False },

      (* initialize model: *)
        If[ genmod === $GenericModel,
            init = InitializeModel[ mod, Reinitialize->False ],
  	    init = InitializeModel[ mod, GenericModel->genmod] 
          ];
        If[ init === $Aborted, Return[$Aborted] ];

      (* check if specified fields are in model(s): *)
	Do[ If[ !InModelQ[ mod, fields[[i]] ],
	       Message[ InsertFields::badparticle, 
			If[ Head[ fields[[i]] ] === If, 
			    fields[[i,2]], fields[[i]] ], 
			mod ];
	       abo = True
	      ],
	   {i, Length[fields]}
	  ];
	If[ abo, Return[$Aborted] ];

      (* apply restrictions: *)
	Return[ RestrictCurrentModel[ 
                  Restrictions/.opt,
                  Select[ opt, 
                   MatchQ[#,Rule[ExcludeFieldPoints|ExcludeParticles,_]]& ] ]
              ];
    ];

(* supply Field[i] for each propagators, for the external ones first 
*)
AppendFieldVar[ pr_[a_,b_,c___],i_Integer ] := pr[ a,b,Field[i],c ]

AppendFieldsToTopology[ Topology[p__] ] := 
  AppendFieldsToTopology[ Topology[1][p] ];
 
AppendFieldsToTopology[ tt:Topology[_][__] ] := 
 Module[{to},
     to = TSort[ PSort /@ tt ];
     to = Head[to] @@ Array[ AppendFieldVar[to[[#]],#]&,Length[to] ];
     AppendTo[ to, Insertions[
		     Graph @@ Array[ (Field[#]->0)&, Length[to] ]
				    ]
             ]
    ];

(* 
  :Part5: DoInsert-functions. Explicit insertion routines. This are the
	  InsertFields-algorithms coded for the three different insertion
	  levels Generic, Classes and Particles.
*)


(* Insert compatible particles in 1 propagator for 1 set of rules:
*)
Ins11[ vert12_ , ru_ , i_Integer, Propagator[type_] ] := 
Module[ {int, vx, newvertex, possiblevx1, possiblevx2, 
         parti, rightru, leftru, leftcto, rightcto },
	leftru = ( #[[1]] -> LeftPartner[ #[[2]] ] )& /@ (List@@ru);
	rightru = ( #[[1]] -> RightPartner[ #[[2]] ] )& /@ (List@@ru);
	parti = Field[i] /. List@@ru;   (* not inserted yet *)

      (* insert Rules (ru) into the two vertices and take care if there
       * is only one vertex in vert12 (->external) or if its a tadpole
       *)
        vx = If[ MatchQ[ vert12, {FieldPoint[_][__]} ], 
                 vert12 /. rightru, 
                 If[ vert12[[1]] === vert12[[2]], 
	             Take[vert12,1] /. rightru,
	             { vert12[[1]] /. leftru, vert12[[2]] /. rightru } ]
               ];

      (* extract counterterm orders:
       *)
        leftcto = vx[[1,0,1]];
        If[ Length[vx]===2, rightcto = vx[[2,0,1]], rightcto=leftcto ];

      (* construct the set of allowed particles for vertex 1:
       *)
	possiblevx1 = PossibleFields[leftcto][
           AntiParticle[parti], FieldPoint@@ vx[[1]] ];

      (* If: is there a second vertex or not?
       *)
   	If[ Length[vx] === 2,
	  (* yes *)
       	   possiblevx1 = AntiParticle /@ possiblevx1;

	   possiblevx2 = PossibleFields[rightcto][
              parti, FieldPoint@@ vx[[2]] ];

          (* throw out false vertices from ExcludeFieldPoints *)
           int = Select[ 
	     Intersection[ possiblevx1, possiblevx2, F$AllowedFields ],
	     (  
	        CheckFieldPoint[ Sort[(FieldPoint[leftcto]@@vert12[[1]] /. 
				  Field[i]->RightPartner[#]) /. rightru ]
			       ] 
                &&
	        CheckFieldPoint[ Sort[(FieldPoint[rightcto]@@vert12[[2]] /. 
				  Field[i]->RightPartner[#]) /. rightru ]  
                               ] 
             )&  ], 
          (* no *)	
	  (* prevent that tadpoles with non-allowed couplings are created *)
           int = Select[ 
             Intersection[ possiblevx1, F$AllowedFields ],
	     ( CheckFieldPoint[ Sort[(FieldPoint[leftcto]@@vert12[[1]] /. 
                                  Field[i]->#)/. rightru ]
	                      ] 
             )&  ] 
           ];

      (* check whether this particle is allowed on this type of propagator: 
       *)
	int = Select[ int, InsertOK[ #, type ]& ];

      (* return updated list of Rules:
       *)
	Insertions @@ 
          ( (ru /. Rule[ Field[i],_ ] -> Rule[ Field[i],# ])& /@ int )
]; 

InsertOK[ field_, type_ ] := True /; InsertOnly[field]===All;
InsertOK[ field_, Loop[_] ] := !FreeQ[ InsertOnly[field], Loop ]
InsertOK[ field_, Incoming ] := (!FreeQ[ InsertOnly[field], Incoming ])||
				(!FreeQ[ InsertOnly[field], External ]);
InsertOK[ field_, Outgoing ] := (!FreeQ[ InsertOnly[field], Outgoing ])||
				(!FreeQ[ InsertOnly[field], External ]);
InsertOK[ field_, External ] := (!FreeQ[ InsertOnly[field], External ]);
InsertOK[ field_, Internal ] := (!FreeQ[ InsertOnly[field], Internal ]);

  
(* insert the ith propagator. Select filters out external points.
*)
Ins1[ { rules_ , top_ } , i_Integer ] := 
   { 
     Flatten[ 
       Function[z,
                Ins11[ Select[ FindFieldPoints[ top,i ], (Length[#]>1)& ] ,
	               z, 
                       i, 
                       Head[top[[i]]] 
                     ]
               ] /@ rules 
     ], 
     top
   };

(* insertion on Generic level:
*)
DoInsert[Generic][ ru_, top:Topology[cf_][ props__ ], vertli_, 
		   w_List, o_List ] :=
Module[ {freesites, genrules, inserted,
	 wanted = Cases[ w, P$Generic ],
	 toomit = Cases[ o, P$Generic ] },
	(* positions of empty propagators: 
	*)	  
  	freesites = #[[1]]& /@ Position[ ru[[1]], Rule[_,0] ];
	(* strip rules to Generic rules: 
	*)
  	genrules = ru /. Rule[ a_, sign_. (b:F|V|S|U)[__] ] :> Rule[ a, b ];
	(* perform insertion:
	*)
 	inserted = Insertions[Generic] @@ 
    	 InsertionsCompare[ 
	  top,
          Fold[ Ins1, { genrules, Topology[props] },
		      freesites ][[1]] 
	  ];
	(* merge result with given insertions (ru) 
	*) 
  	inserted = inserted /. (
	  (Rule[ Field[#],_ ] :> ru[[1,#]]
	  )& /@ Complement[ Array[ #&, Length[ ru[[1]] ] ], freesites ]);
(*
DePrint["before select: ", Length[inserted]];
DePrint["  I want : ", wanted];
DePrint["  I omit : ", toomit];
*)
	If[ Length[wanted]>0,
	    inserted = Select[ inserted, MemberListQ[ #, wanted, 4 ]& ]
	  ];
	If[ Length[toomit]>0,
	    inserted = Select[ inserted, !MemberListQ[ #, toomit, 4 ]& ]
	  ];
(*
DePrint[" after select: ", Length[inserted]];
*)
  	FAPrint[3, "  -> ", Length[inserted], " Generic insertions."];
  	Return[ inserted ]
  ];

(* insertion on Classes level:
*)
DoInsert[Classes][ ru_, top:Topology[cf_][ props__ ],vertli_,
		   w_List, o_List ] :=
Module[ { freesites, clarules, genins, clains, inserted, multnum =0,
	 wanted = Cases[ w, P$Class ],
	 toomit = Cases[ o, P$Class ] },
	(* positions of empty propagators:
	   Generic fields count as free sites on the classes level;
 	   this is important if the insertion level is higher than the
	   level of the process specification.
	*)
	(* old form:
	freesites = #[[1]]& /@ Position[ ru[[1]], Rule[_,0] ];
  	*)
	freesites = #[[1]]& /@ Position[ ru[[1]], Rule[_,0|P$Generic] ];
	(* strip rules to Classes rules:
	*)
  	clarules = ru /. Rule[ a_, s_. (fi:F|S|V|U)[i_,j_] ] :> 
			  Rule[ a, s fi[i] ];
	(* find Generic insertions:
	*)
  	genins = 
	  DoInsert[Generic][ clarules, top, vertli, w, o ];

       (* find Classes insertions:
        *)
  	inserted = 
	 ( 
(*
Print[" Fold => ",
    	     Fold[ Ins1, { Insertions[ Graph @@ # ], top },
	  	         freesites ][[1]] ];
*)
           # ->  Insertions[Classes] @@ 
            InsertionsCompare[ 
	     (Topology[ (Head[#])[[1]] ]) @@ top,
    	     Fold[ Ins1, { Insertions[ Graph @@ # ], top },
	  	         freesites ][[1]]
	     ]
  	 )& /@ genins;
	(* discard empty insertions:
	*)
  	inserted = Select[ inserted, FreeQ[#,Insertions[Classes][]]& ]; 
	(* insert user entered form of externals:
	*)
  	inserted = inserted /. (
	  (Rule[ Field[#],_ ] :> ru[[1,#]]
	  )& /@ Complement[ Array[ #&, Length[ ru[[1]] ] ], freesites ]);

	(* provide explicit indices (Index[ ind ]) for Classes 
	*)

(*
Print["inserted = ",inserted];
*)
  	inserted = 
	 Function[ z, 
		   MapAt[ Function[ y,
		                    ProvideIndices[ #, vertli ]& /@ y
				  ], 
			   z, 2 ]            
		 ] /@ inserted;
(*
Print["inserted = ",inserted];
*)
  	inserted = Select[ inserted //. 
			  { False->Identity, 
			    True->Identity, 
			    Insertions[Classes][ a___, None, b___ ] :>
			     Insertions[Classes][ a, b ] },
		      FreeQ[#,Insertions[Classes][]]& ]; 
	(* loop over insertions for last selections:
	*)
(*
DePrint[" Cla wants : ", wanted];
DePrint[" Cla omits : ", toomit];
*)
  	Do[ 
        If[ Length[wanted]>0,
            inserted[[i,2]] = 
              Select[ inserted[[i,2]], 
                      MemberListQ[ # /. (x:(F|S|U|V|VS|SV))[n_,_] :> x[n], 
                                   wanted, 4 ]& 
                    ]
          ];
        If[ Length[toomit]>0,
            inserted[[i,2]] = 
              Select[ inserted[[i,2]], 
                      !MemberListQ[ # /. (x:(F|S|U|V|VS|SV))[n_,_] :> x[n], 
                                    toomit, 4 ]& 
                    ]
          ];
	    multnum += Length[ inserted[[i,2]] ],
           {i,1,Length[inserted]}
          ];

	inserted = Select[ inserted, 
                            !MatchQ[#, Graph[_][__]->Insertions[_][] ]& ];
  	FAPrint[2, "   -> ", Length[inserted], "/", multnum,
		   " Generic/Classes insertions."]; 
  	Return[inserted]
];

(* index handling:
*)
ProvideIndices[ ru:Graph[_][__Rule], vertli_ ] :=
Block[ { indexruli, rightru, deltas, new },

       (* here we must provide multiple indices. for classes fields
        * we change Field[i]->X[j] 
        *       to: Field[i]->X[j, { False[Index[type,i]], ...} ] 
        * i.e. free (dummy) indices are tagged by the head False
        *)
     	indexruli = ru /. Rule[ Field[ ind_Integer ], s_. fi_[ i_ ] ] :>
	    If[ Indices[ fi[i] ] === {},
		Rule[ Field[ind], s fi[i] ],
		Rule[ Field[ind], 
		           s fi[ i, (False[ Append[ #, ind ] ])& /@ 
					      Indices[fi[i]] ] ]
              ];

       (* HERE: not sure whether this is ok. wo simply replace the mixers 
        * by their right partners. this restricts the models to have mixing 
        * with partners of same indexrange only, which seems reasonable to me 
        *)
	rightru = ( #[[1]] -> RightPartner[ #[[2]] ] )& /@ indexruli;

(*
DePrint["map = ", ( "Diag" /@ ( vertli /. (List@@rightru) ) ) ];
DePrint["Diag = ", ( Diagonal /@ ( vertli /. (List@@rightru) ) ) ];
*)

       (* the function Diagonal returns a product of IndexDeltas or 1
        * for every field point
        *)
     	deltas = Times @@ ( Diagonal /@ ( vertli /. (List@@rightru) ) );

     	new = Nest[ ApplyDelta[False], 
                 indexruli * deltas, 
                 Count[ indexruli * deltas, IndexDelta[ False[_], False[_]] ] 
	       ];
     	new = Nest[ ApplyDelta[Number], 
	         new, 
	         Count[ new,  
		        IndexDelta[ False[_], _?NumberQ] | 
			 IndexDelta[ _?NumberQ, False[_] ] ] 
               ];
     	new = Nest[ ApplyDelta[True], 
	         new, 
	         Count[ new, 
		        IndexDelta[ True[_], False[_] ] | 
			 IndexDelta[ False[_], True[_] ] ]
	       ];
        If[ Length[new]>0,
     	    new = Nest[ ApplyDelta[Equal], 
	                new, 
	                Length[new] - 1
	              ]
          ];
(*
Print["new = ",new];
*)
     	If[ FreeQ[ new, IndexDelta ], Return[new], Return[ None ] ]
];

(* four ApplyDelta functions.
 * first dummy indices (with head False) are deltaed against one another,
 * then against numbers, then against nondummy outer indices (with head True),
 * then the nondummy against one another leaving nd1 == nd2 in the index 
 *)
ApplyDelta[_][ exp___ ] := exp;

ApplyDelta[False][ 
    Graph[s_][ r___ ] * del___ * IndexDelta[ False[a_], False[b_] ]  ] :=
                     ( Graph[s][r] * del ) /. False[b] :> False[a];
ApplyDelta[Number][ 
    Graph[s_][ r___ ] * del___ * IndexDelta[ a_?NumberQ, False[b_] ]  ] :=
                     ( Graph[s][r] * del ) /. False[b] :> a;
ApplyDelta[True][ 
    Graph[s_][ r___ ] * del___ * IndexDelta[ True[a_], False[b_] ]  ] :=
                     ( Graph[s][r] * del ) /. False[b] :> True[a];
ApplyDelta[Equal][  
    Graph[s_][ r___ ] * del___ * IndexDelta[ a_, True[b_] ]  ] := 
                     ( Graph[s][r] /. True[b] :> True[b] == a ) * del ;

(* insertion on Particles level:
*)
DoInsert[Particles][ ru_, top:Topology[cf_][ props__ ],vertli_,
                     w_List, o_List ] :=
Module[ { clains, inserted, multnum = 0, partnum =0,
	  wanted = Cases[ w, P$ClassMember ],
	  toomit = Cases[ o, P$ClassMember ] },

	(* generate classes insertions:
	*)
  	clains = 
	  DoInsert[Classes][ ru, Topology[cf][props], vertli,
			     w, o ];
(*
Print[clains];
*)
  	inserted = 
  	 Function[ z,
          z[[1]] -> ( Function[ y,
                       y -> Insertions[Particles] @@ 
                        InsertionsCompare[ 
			 (Topology[ (Head[y])[[1]] ]) @@ top,
                          Select[Flatten[FindAll/@Insertions[ Graph@@y ] ],
                                 CheckTheGraph[ #, vertli ]&
                                ]
		         ]
                       ] /@ z[[2]] ) 
	  ] /@ clains;
  	Do[Do[ 
        If[ Length[wanted]>0,
            inserted[[i,2,j,2]] = 
              Select[ inserted[[i,2,j,2]], MemberListQ[ #, wanted, 4 ]& ]
          ];
        If[ Length[toomit]>0,
            inserted[[i,2,j,2]] = 
              Select[ inserted[[i,2,j,2]], !MemberListQ[ #, toomit, 4 ]& ]
          ],
           {j,1,Length[inserted[[i,2]]]}
          ],
          {i,1,Length[inserted]}
         ];
       (* remove empty Particles insertions:
        *)
  	inserted = ( Function[ z, 
		               z[[1]] -> 
				Select[ z[[2]], 
		                        FreeQ[#,Insertions[Particles][]]&]
			     ]/@ inserted ) ;
       (* if classes have become empty now, remove them, too:
        *)
  	inserted = Select[ inserted, FreeQ[#,Insertions[Classes][]]& ]; 
       (* count classes and particles:
        *)
  	Do[ multnum += Length[ inserted[[i,2]] ];
	   Do[ partnum += Length[ inserted[[i,2,j,2]] ],
	       {j,1,Length[ inserted[[i,2]] ]}
             ],
            {i,1,Length[inserted]}
    	  ];
  	FAPrint[2,"    -> ", partnum, "/", multnum,
		  " Particles/Classes insertions."];
  	Return[inserted];
];

(* find all explicit insertions from one class insertion:
*)
FreeIndices[ Rule[ Field[ _ ], s_. fi_[ i_ ] ] ] := Sequence @@ {};

FreeIndices[ Rule[ Field[ _ ], s_. (fi:F|S|V|U|VS|SV)[ i_, li_ ] ] ] :=
 Sequence @@ Select[
   Array[ { li[[#]], True @@ ( (IndexRange /@ Indices[ fi[i] ])[[#]] ) }&, 
	  Length[li] ], 
   Not[ NumberQ[ #[[1]] ] ]& ];
  
(* keep only the smallest set of the really different indices:
*)
FindAll[ ru_ ] := 
Block[ {newru,zarray,sloties,freeindices,parins},

         newru = ru /. { in_[i_] == n_?NumberQ :> n,
			 in1_[i1__] == in2_[i2__] :> in2[i2] };

	 freeindices = Union[ FreeIndices /@ List @@ newru ];
	 If[ freeindices =!= {},
	     freeindices = Transpose[ freeindices ]
	   ];
	 If[ freeindices === {},
(*
Print["freeindices: ", freeindices];
*)
	     parins = Insertions[ newru ],
(*
Print["freeindices: ", freeindices];
*)
	     zarray = Array[ ToExpression[ "z" <> ToString[#] ]&, 
			     Length[ freeindices[[1]] ] ];
(*
Print["zarray: ", zarray];
*)
	     sloties = newru /. Thread[ Rule[ freeindices[[1]],
				              zarray ] 
                                      ];
(*
Print["sloties: ", sloties];
*)
	     newru = Outer[ Function[ Release[zarray], Release[sloties] ], 
		    	    Sequence @@ freeindices[[2]] ]; 
(*
Print["newru: ", newru];
*)
             parins = Insertions @@ Flatten[newru]
	   ];

         parins = Select[ BadGraph/@ parins, (#=!=trash)& ];
(*
Print["parins: ", parins];
*)

         Return[ parins ];
       ];

(* check whether graph contains a forbidden particle:
*)
BadGraph[ Graph[ r__Rule ] ] :=
  If[ !FreeQ[ MemberQ[ F$AllowedFields, #[[2]] ]& /@ {r}, False ],
      trash,
      Graph[r]
    ];

(* check whether all filled vertices are contained in model:
*)
TheRightRules[ rul_ ] := (#[[1]]->RightPartner[#[[2]]])& /@ (List@@rul);

CheckTheGraph[ ru_, vertli_ ] :=
Block[{thev},
     thev =  vertli /. TheRightRules[ru];
     Do[
        If[ (Sort[List@@thev[[i]] ] === 
             Sort[{-F[3,{1}], F[3,{3}], V[1]}]
            )      
           || 
           (Sort[List@@thev[[i]]]===
            Sort[{-F[3,{2}], F[3,{3}], V[1]}]
            ),
           Print[ thev[[i]], " CFP = ", CheckFieldPoint[Sort[thev[[i]]]] ]
          ];
        thev[[i]] =  CheckFieldPoint[Sort[thev[[i]]]],
        {i,Length[thev]}
       ];
      Return[And@@ thev]
 ];
 
CheckGraph[ ru_, vertli_ ] :=
   And @@ ( CheckFieldPoint[Sort[#]]& /@ ( vertli /. TheRightRules[ru] ) );

(* 
  :Part6: Compare the insertions: input: topology and rules, 
				  output: compared set of rules  
*) 

InsertionsCompare[ TopSpec, Insertions[] ] := Insertions[];

InsertionsCompare[ Topology[1][___], ins_ ] :=  ins /.{ Graph->Graph[1] };

InsertionsCompare[ top : Topology[ _Integer ][ __ ], rules_ ] := 
 Module[ { ru,inli,lili,fiinli,all,cases,pos,ar } ,

         (* prepare to find only disjoint insertions which must be compared *)

  ru     = rules /. Graph->LongInsertion ;
  inli   = Select [ top,!FreeQ[#,in]& ] ;
  lili   = Select [ top/.{inc->in,out->in},FreeQ[#,in]& ] ;
  fiinli = Array [ inli[[#,3]]&,Length[inli] ] ;
  filili = Array [ lili[[#,3]]&,Length[lili] ] ;
  all    = Array [  Join[ fiinli/.ru[[#]],
	       	          Sort[filili/.ru[[#]]] ] & , 
                    Length[ru] ] ;
  cases  = Union[  all  ] ;
  pos    = Array[  Position[ all,cases[[#]] ]&,Length[cases]  ];
  ar     = 
    Array[ 
     Function[z,
              Append[top,
                     Head[ru] @@
			Array[ rules[[ Flatten[ pos[[z]] ][[#]] ]]&,
	                       Length[ Flatten[pos[[z]]] ] 
	                     ]  
             ]      ],
          Length[pos]
	 ];

            (* compare within the disjoint sets *)

  Join  @@  ( DisjointInsertionsCompare[ # ]& /@ ar ) 
  ];

LongInsertion[ b___ ] := 
  Array[ List[b][[#,1]] -> Sort[{ List[b][[#,2]],
	                          AntiParticle[ List[b][[#,2]] ]
			       }] [[1]] &,
	 Length[List[b]] 
       ];


(* comparing disjoint subsets using Utilities`Compare: 
*)
DisjointInsertionsCompare[ to:T_[ __ ] ] := 
  Insertions @@ (ExtractInsertion /@ 
       Compare[ Apply[ TopologyList,
                       Array[ Drop[to,{-1}] /.
                                  ( Last[to][[#]] /. Graph -> List ) & ,
	                      Length[ Last[to] ]
                     ]      ] 
	      ]);

ExtractInsertion[ t:Topology[ i_Integer ][ ___ ]] :=
        Graph[i] @@ Array[ Field[#] -> t[[#,3]] &, Length[t] ];
 

(*
  :Part7:
	PickLevel for diagram lists.
*)
PickLevel[ lev_ ][ TopologyList[tops__] ] :=
   PickLevel[lev][TopologyList[][tops] ];

PickLevel[ lev_ ][ t:TopologyList[___][__] ] :=
 TopologyList@@ ( (Topology@@#)& /@
   HighEnergyPhysics`FeynArts`Graphics`InsertionsList[
     lev ][ t, "", {} ] );

End[] (* HighEnergyPhysics`FeynArts`Insert` *)

(**)
