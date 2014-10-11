 
(* :Title: Analytic *)

(* :Authors: Hagen Eck, Sepp Kueblbeck *)

(* :Summary: 
	Translation of InsertFields-output into analytic expressions
*)

(* :Context: HighEnergyPhysics`FeynArts`Analytic` *)

(* :Package Version 2.0 *)

(* :Mathematica Version 2.2 *)

(* :History:
	Created Apr, 1993 from the CreateFeynAmp-package of FeynArts 1.0
*)

(* :Contents:
	Part1: CreateFeynAmp main function
	Part2: utility functions for CreateFeynAmp
	Part3: TopToGrassman: build Grassman chains
	Part4: CreateAmplitude: insert analytical expressions
	Part5: PickLevel: create concrete amplitudes.
	(search for Part# to find the beginning of a section)
*)

FAPrint[3,"[Analytic.m]"];

Begin["`Analytic`"];

Options[ CreateFeynAmp ] = 
{
	AmplitudeLevel	-> Default,
	GaugeRules	-> { GaugeXi[_] :> 1 },
	HoldTimes	-> False,
	Momenta		-> Automatic,
(* 4-dim: *)
	PreFactor	-> -I ( 1/(2Pi)^4 )^LoopNr,
(* D-dim: *)
(*	PreFactor	-> -I ( Global`Mu^(4-Global`$D) /
				(2Pi)^Global`$D          )^LoopNr, *)
	Truncated 	-> False,
	UseModel	-> {$Default, $Default}
};

CreateFeynAmp::mom = 
"Length of momenta specification (`1`) does not match number of 
topologies (`2`).";

CreateFeynAmp::badlevel =
"InsertionLevel `1` is not contained in this insertion (ignoring).";

CreateFeynAmp::nolevel =
"Amplitude levels not contained in Insertion levels (aborting)!";

CreateFeynAmp::nomod =
"Information field of TopologyList contains no `1` model information.";

CreateFeynAmp::mtrxtr =
"Different MatrixTraceFactors inside one loop! Involved fields are `1`.
CHECK Classes MODEL!";

Propagator::nores =
"Cannot resolve Propagator of field `1`.";

(*
  :Part1: CreateFeynAmp main function: exists for Topologies and Topology-
	  Lists.
*)

(* pattern abbreviations:
*)
InsSpec = (Topology[__] -> Insertions[_][___]);
InsListSpec = (InsSpec..);

(* list version: 
*)
CreateFeynAmp[ TopologyList[ tops:InsListSpec ], opt___Rule ] :=
 CreateFeynAmp[ TopologyList[][tops], opt ];

CreateFeynAmp[ TopologyList[info___Rule ][ tops:InsListSpec ], opt___Rule ] :=
Block[ { nrT = Length[{tops}],
	OPT = ActualOptions[ CreateFeynAmp, opt ],
	ret = TopologyList[],
	mom, mod, init, genmod,
	name, longinfo, topoptions, singleoptions, tempAmp },

      (* check list of momenta specifications: 
       *)
	If[ ( mom = Momenta /. OPT ) === Automatic,
	   mom = Array[ {}&, nrT ],
	   If[ Length[ mom ] =!= nrT,
	      Message[ CreateFeynAmp::mom, Length[mom], nrT ];
	      Return[ $Aborted ]
	     ]
	  ];
	OPT = OPT /. Rule[Momenta,_] :> Rule[Momenta, mom];

      (* check whether generic model is specified:
       *)
	genmod = GenericModel/.{info};
        If[ genmod===GenericModel,
           (* no generic model in info-list *)
            Message[ CreateFeynAmp::nomod, "generic" ];
            If[ (genmod=(UseModel/.OPT)[[2]]) === $Default,
               genmod = GenericModel/.Options[InsertFields];
               FAPrint[1," Using default generic model of InsertFields: ",
                       genmod],
               FAPrint[1," Using generic model ",genmod]
              ]
          ]; 

      (* check whether classes model is specified:
       *)
	mod = Model/.{info};
        If[ mod===Model,
            Message[ CreateFeynAmp::nomod, "classes" ];
            If[ (mod=(UseModel/.OPT)[[1]]) === $Default,
               mod = Model/.Options[InsertFields];
               FAPrint[1," Using default model of InsertFields: ",mod],
               FAPrint[1," Using model ", mod]
              ]
          ]; 

      (* initialize model:
       *)
        If[ genmod =!= $GenericModel,
           (* initialize full model if no generic model initialized *)
	    init = InitializeModel[ mod, GenericModel->genmod],
           (* or just check the classes model *)
            init = InitializeModel[ mod, Reinitialize->False ]
          ];
	If[ init===$Aborted, Return[$Aborted] ];

      (* add momenta and masses to "Process": 
       *)
        longinfo = {info}/.(
	 Rule[Process, proc_ ] :>
          Rule[Process, 
	       ExtendedProcess[proc, SupplyMomenta[ {tops}[[1,1]], 
				      (Momenta/.OPT)[[1]] ][[1]] 
			      ]  
	      ] ) /. Mass -> TheMass /. TheMass -> Mass;

	(* name of process: *)
	name = ProcessName /. {info};

	(* loop over all topologies: *)
	FAPrint[ 1, " ... starting generation of amplitudes."];
	Do[
	   If[$Verbose===1,
	      Print[" ", i, "/", nrT, " (", Length[{tops}[[i,2]]], ")"] ];
	   FAPrint[2, "  generating amplitude ", i, "/", nrT, "." ];
	   singleoptions := OPT /.{ 
	     Rule[Momenta, l_ ] :> Rule[Momenta, l[[i]] ],
             Rule[UseModel, _ ] :> Rule[UseModel,({mod,genmod})] };
	   tempAmp = 
	     CreateFeynAmp[ {tops}[[i]], Sequence@@ singleoptions
	      ];
	    AppendTo[ ret, 
                     (#/.GraphName[x___]:>GraphName[name,Topology==i,x]
                     )& /@ tempAmp       ],
	    {i, nrT}
	   ];

	Return[ (FeynAmpList@@longinfo) @@ (Join@@(ret/.FeynAmpList->List)) ];
];

(* single topology version:
*)
CreateFeynAmp[ Topology[anything___][props__] -> verslist_ , opt___Rule ] :=
  FeynAmp[anything]@@ CreateFeynAmp[Tooplogy[props]->verslist, opt];

CreateFeynAmp[ Topology[props__] -> verslist_ , opt___Rule ] := 
Block[ { OPT = ActualOptions[ CreateFeynAmp, opt ], 
	 mod, genmod, init, integrMom, momTop, inslevel, amplevel, lal,
	 inittop, hold, GandM, arule, reamp = {}, ru, mtf
	},

       (* check model: 
        *)
         {mod, genmod} = UseModel/.OPT;
	 If[ mod === $Default, 
              mod = Model/.Options[InsertFields] ]; 
	 If[ genmod === $Default, 
              genmod = GenericModel/.Options[InsertFields] ]; 
	 If[ genmod =!= $GenericModel,
             init = InitializeModel[mod, GenericModel->genmod],
             init = InitializeModel[mod, Reinitialize->False ]
           ];
	 If[ init === $Aborted, Return[$Aborted] ];

	(* add momenta: 
         *)
	 {momTop, integrMom} = SupplyMomenta[ Topology[props], Momenta /. OPT ];

       (* append field indices after generic index. here we throw out the 
        * "==" but keep the set of indices containing ALL different external 
        *  indices 
        *)
         ru = verslist /. { 
	     Rule[ Field[i_Integer], fi_Symbol ] :>
	      Rule[ Field[i], fi[ Index[Generic,i] ] ] } /. { 
	     (Index[type_Symbol,i_Integer] == n_?NumberQ) :> n,
	      (Index[ty1_,i1_] == Index[ty1_,i2_]) :> Index[ty1,i1] };

       (* find level of insertion and compare with AmplitudeLevel: 
        *)
	 inslevel = GetLevel[ verslist ];
         If[ (amplevel=AmplitudeLevel/.OPT) === Default,
             amplevel = inslevel,
	     Which[ amplevel===Generic, amplevel={Generic},
		    amplevel===Classes, amplevel={Generic,Classes},
		    amplevel===Particles, amplevel={Generic,Classes,Particles}
		  ]
	   ];
	 lal = Length[amplevel];
	 If[ amplevel =!= inslevel,
            Do[
	       If[ FreeQ[ inslevel, amplevel[[i]] ],
                  Message[CreateFeynAmp::badlevel, amplevel[[i]] ];
		  amplevel[[i]] = Null
                 ],
                {i, lal}
               ]
           ];
	 If[ (amplevel=ReleaseHold[amplevel/.Null->Hold[Sequence[]]]) === {},
            Message[CreateFeynAmp::nolevel,inslevel];
	    Return[$Aborted]
           ];
	 OPT = OPT /. Rule[ AmplitudeLevel, _ ] :> 	
		Rule[ AmplitudeLevel, amplevel ];

       (* pick out levels for amplitude: 
        *)
         If[ FreeQ[ amplevel, Generic ],
 	    ru = ru /. Insertions[Generic][ graphs__ ] :>
	     Flatten[ Insertions[Classes] @@ ( #[[2]]& /@ {graphs} ) ] 
           ];
	 If[ FreeQ[ amplevel, Classes ],
	    ru = ru /. Insertions[Classes][ graphs__ ] :>
	     Flatten[ Insertions[Particles] @@ ( #[[2]]& /@ {graphs} ) ] 
           ];
	 If[ FreeQ[ amplevel, Particles ],
	    ru = ru /. Insertions[Particles][__] :> Null /. 
                       Rule[something_, Null ] :> something
           ];

       (* loop over insertions on highest level: 
        *)
	 FAPrint[2, "   ... ", Length[ru], " amplitudes",
		    Which[lal===2," (+)",lal===3," (++)",True,""],"." ];
	 Do[ 
            (* CrInit first attaches the momenta, then provides the kine-
             * matical indices according to the order of the Props (external 
             * before internal) and then forms the spinor chains.
             *) 
	     inittop = CrInit[ momTop, ru[[i]], OPT ];

            (* create generic amplitude: construct vertices, insert vertices
             * and propagators, collect denominators, find prefactor, apply
             * LastGenericRules ..
             *)
             hold = CreateAmplitude[ inittop, integrMom, OPT ];

            (* extract MatrixTraceFactors (if any):
             *)
             mtf = Cases[ hold, MatrixTraceFactor[__], Infinity ];

             hold = hold /. MatrixTraceFactor[any__] -> 1;
(* 
Print["hold = ", hold];
Print["mtf  = ",mtf];
*)
            (* if there is more than one level, the amplitude needs a set
             * of replacement rules which are constructed here (extract
             * G's and masses and apply GtoCRules). if there is only one
             * level we immediately replace the G's and masses in the generic
             * amplitude.
             * the matrix trace factor is included into the RelativeCF or
             * replaced directly by EvaluateMatrixTraceFactor.
             *)
	     If[ lal > 1,
                 GandM = Append[ FindGandM[hold] /. {-1,fi_[n__]}:>-fi[n],
                                 RelativeCF ];
	         AppendTo[ hold, 
		           GandM -> 
			    (
			     FindG[Length[amplevel]-1][ 
			       GandM /. ( Reverse /@ ( List @@ ru[[i,1]] ) ),
			       ru[[i,2]],
			       Head[ ru[[i,1]] ][[1]],
                               mtf
			      ]  /. GtoCRules[] 
			    )
		         ];
                hold = 
                    Prepend[ hold, 
                       GraphName[ Drop[Head[ru[[i,1]]], 1]/.Graph->Sequence]
                     ],
                (* else: only one level *)
		hold = hold /. GtoCRules[];
                hold = 
                    Prepend[ hold, 
                       GraphName[ Drop[Head[ru[[i]]], 1]/.Graph->Sequence]
                     ],
	       ];
             hold = hold //. M$LastModelRules;
	     reamp = Append[ reamp, hold ],
             { i, 1, Length[ru] }
           ];  

        Return[ FeynAmpList@@reamp /. Mass -> TheMass ];
];

(*
  :Part2: Utility functions: generate "long" process description
	  (ExtendedProcess), extract insertionlevel from InsertionsList
	  (GetLevel), initialize topology for CreateAmplitude (CRInit)
*)

(* add momenta and masses to "Process" :
*)
ExtendedProcess[ proc_, mtop_ ] :=
Block[{mrules,flatproc},
    mrules=Select[ List @@ ( mtop/.
		{ Propagator[Incoming][Vertex[1,0][i_Integer],_,_,mom_]:>
                                     Rule[Vertex[1,0][i],mom] ,
                  Propagator[Outgoing][Vertex[1,0][i_Integer],_,_,mom_]:>
                                     Rule[Vertex[1,0][i],-mom] } 
                           ) , FreeQ[#,Field]& ];
    header=Head[ proc[[1]] ];
    If[ Length[proc] === 1, (* for only incoming or outgoing, e.g. tadpoles *) 
        header @@ Table[ {proc[[i]],Vertex[1,0][i]/.mrules,
			       TheMass[proc[[i]]]},
                         {i,1,Length[proc]} ],
        header @@ Table[ {proc[[1,i]],Vertex[1,0][i]/.mrules,
					       TheMass[proc[[1,i]]]},
                         {i,1,Length[proc[[1]]]} ]    ->
        header @@ Table[ {proc[[2,i]],Vertex[1,0][i+Length[proc[[1]]]]/.
				 mrules, TheMass[proc[[2,i]]]},
                         {i,1,Length[proc[[2]]]} ]
      ]
   ];

(* extract insertion level:
*)
GetLevel[ Insertions[ type_ ][ Rule[ g_, deeper_ ], ___ ] ] := 
 Join[{type}, GetLevel[deeper]];
GetLevel[ Insertions[ type_ ][ __ ] ] := { type };

(* initialize topology:	
   extract sym-fac , insert version on highest level, include momenta in
   fields (MomToField), search grassman fields (TopToGrassman), provide 
   kinematic indices (AddKinematicIndex)
*)
CrInit[ Topology[ps__], Graph[sym_,num___][ru__], opt_List ] :=
  CrInit[ Topology[ps], Graph[sym, num][ru] -> {}, opt ];

CrInit[ Topology[ps__], Graph[sym_,num___][ru__] -> deeper_, opt_List ] :=
Block[ {topol}  ,
	  topol = Topology[sym][ps];

	(* construct prefactor: 
         *)
	  topol = AddPreFactor[ topol, PreFactor/.opt ] ;

	(* include momentum in field:  
         *)
	  topol = MomToField /@ (topol /. {ru});

	(* add kinematic indices: 
         *)
	  topol = AddKinematicIndex[ topol] ;

        (* build chains of Grassman fields ("dot" and "tr") 
         *)
          If[ TrueQ[ $FermionLines ],
	      topol = TopToGrassman[ topol ]
            ];

	  Return[topol];
];

(* constructing the global prefactor ...
*)
AddPreFactor[ tt:Topology[sym_][ props__ ], pf_ ] :=
   Topology[ ( pf / sym ) /. Pi->pi /. LoopNr->LoopNr[tt] /. pi->Pi
           ][ props ];   (*  ^ Mma sometimes can't handle Pi^0 *)

(* ... using Eulers relation:
*)
LoopNr[ t:Topology[_][___] ] := 
    Block[ {e, vsum} ,
	  e = Length[ Vertices[1][t] ]; (* CT-tadpoles included! *)
	  vsum = ((#[[1,0,1]]-2)*Length[#])& /@ Vertices[I][t];
	  Return[ 1/2( Plus@@vsum - e) + 1 ];
	 ];

(* include momentum in field so that AntiParticle-handling etc. is easier 
*)
MomToField[ Propagator[ty_][from_,to_,sign_. fi_[ind__],mom_] ] :=
	 Propagator[ty][ from,to,sign fi[ind,mom] ]

(* add kinematic indices:
*)
AddKinematicIndex[ top:Topology[s_][props__] ] := 
Block[{i,fieldi, retop = Topology[s][]},
	Clear[ CurrentIndex ];
	CurrentIndex[_] := 1;
	Do[ propi = top[[i]];
	    propi = GiveKinematicIndex[ propi ];
	    retop = Append[ retop,propi ],
	    {i,Length[top]}
	  ];
        Return[ retop ]
      ];

GiveKinematicIndex[ Propagator[ty_][ from_,to_,s_. fi_[ind___] ] ] :=
Block[ { ki = KinematicIndices[fi], resprop },
   If[ ki === {},
       indices = Sequence[],
       indices = 
	 If[ ty === Incoming || ty === Outgoing,
	     Array[ Index[ ki[[#]], CurrentIndex[ ki[[#]] ] ++ ]&, 
		    Length[ki] ],
	     Rule @@ Transpose[
	      Array[ { Index[ ki[[#]], CurrentIndex[ ki[[#]] ]++ ],
	               Index[ ki[[#]], CurrentIndex[ ki[[#]] ]++ ] }&, 
	 	     Length[ki] ]
              ]
           ]
     ]; 
   Return[ Propagator[ty][ from, to, s fi[ ind, indices ] ] ]
];

(* find the elements of the coupling vectors, masses, and relative
 * combinatorial factor. This depends on the remaining nesting depth, 
 * i.e. if the AmplitudeLevel contains two level specifications then 
 * there is one level of insertion left, if there are three level 
 * specifications there are 2 levels left.
 * {mtf} is the matrix trace factor and is either {} or
 * {MatrixTraceFactor[..]}
 *)

ReplaceGMCF[ listofg_, graph_, GenericCF_Integer,{} ] := 
        ( listofg /. (List @@ graph) ) /.
	   RelativeCF :> GenericCF / Head[graph][[1]] 

ReplaceGMCF[ listofg_, graph_, GenericCF_Integer,
             {mtf:MatrixTraceFactor[__]} ] := 
        ( listofg /. (List @@ graph) ) /.
	   RelativeCF :> 
             ( GenericCF / Head[graph][[1]] *
               EvaluateMatrixTraceFactor[mtf, List@@ graph] 
             )

FindG[1][ listofg_, rules_, GenericCF_Integer, {mtf___} ] :=
    Function[ z,
              ReplaceGMCF[ listofg,z,GenericCF,{mtf} ] 
            ]   /@  rules 

FindG[2][ listofg_, rules_, GenericCF_Integer, {mtf___} ] :=
    Function[ z,
              ReplaceGMCF[ listofg,z[[1]],GenericCF,{mtf} ] -> 
                        ( ReplaceGMCF[ listofg,#,GenericCF,{mtf} ]& /@ z[[2]] ) 
            ]   /@  rules 

FindGandM[amp_] :=
Block[ { a = amp/.Mass->TheMass/.TheMass->Mass },
       a =  Union[ Flatten[
	     Cases[ a, #[___], Infinity ]& /@ 
		     {G[_][_][__], Mass} 
	     ]];
	Return[a];
     ];

(* EvaluateMatrixTraceFactor: tries to replace all classes and particles
 * fields to find correct trace factor.
 *)
EvaluateMatrixTraceFactor[ any_, ru_ ] :=
Block[ {mtf, act, ret = 1 },
       
      (* firstly, we have ro re-replace the F[Index[Generic,i]] by Field[i]
       * to make the field replacement rules match:
       *)
       mtf = any /. P$Generic[Index[Generic,n_Integer]] :> Field[n] /. ru;
      
      (* now, we convert all fields to classes level without signs:
       *)
       mtf = ToClasses[mtf] /. - fi_[i_Integer] :> fi[i] ;

      (* insert trace factors, give factor 1 for non-defined fields:
       *)
       Do[ act =  Union[ ( MatrixTraceFactor/@ mtf[[i]]
                         ) /. MatrixTraceFactor[_] -> 1
                       ];
           If[ Length[act] > 1,
               Message[ CreateFeynAmp::mtrxtr, Union[mtf[[i]]] ];
               act = {MatrixTraceFactor[ Or@@act ]}
             ];
           ret *= act[[1]];
          ,
          {i,Length[mtf]} 
         ];
       Return[ret]
     ];

(*
  :Part3: 
	 TopToGrassman
 	 form spinor chains (also for SUSY) and determine fermion 
	 statistics factor
*)

(* first step : building chains of grassman fields. 
   We use two different Heads for the Grassman chains: gmE and gmI for 
   external and internal chains respectively.
*)

(* patterns for Grassman particles:
*)
gmP = _. (F|U)[__];
gmF = _. F[__];
gmU = _. U[__];

(* patterns for internal and external Grassman propagators:
*)
extgmProp = Propagator[(External|Incoming|Outgoing)][x___, _. (U|F)[__] ];
intgmProp = Propagator[(Internal|Loop[_])][x___, _. (U|F)[__] ];

(* take an arbitrary Grassman propagator to start with:
*)
GrassmanStart = 
{
 Topology[s_][pa___, pg:extgmProp, pb___] :> Topology[s][ pa, pb, gmE[pg] ],
 Topology[s_][pa___, pg:intgmProp, pb___] :> Topology[s][ pa, pb, gmI[pg] ]
};

(* append the other propagators:
*)
GrassmanRules = 
{
 (* for F's: 
 *)
 Topology[s_][ pa___, Propagator[ty_][toz_, toy_, gm1:gmF ], pb___, 
     (h:gmE|gmI)[ qa___, Propagator[tz_][frz_, toz_, gm2:gmF ] ] ] :>
   Topology[s][ pa, pb, 
     h[ qa, Propagator[tz][frz,toz,gm2], Propagator[ty][toz,toy,gm1]]], 
 (**)
 Topology[s_][ pa___, Propagator[ty_][fry_, toz_, gm1:gmF ], pb___, 
     (h:gmE|gmI)[ qa___, Propagator[tz_][frz_, toz_, gm2:gmF ] ] ] :>
   Topology[s][ pa, pb, 
     h[ qa, Propagator[tz][frz,toz,gm2], 
	    Propagator[ty][toz,fry,AntiParticle[gm1]]]],             
 (**)
 Topology[s_][ pa___, Propagator[ty_][fry_, frz_, gm1:gmF ], pb___, 
     (h:gmE|gmI)[ Propagator[tz_][frz_, toz_, gm2:gmF ], qa___ ] ] :>
   Topology[s][ pa, pb, 
     h[ Propagator[ty][fry,frz,gm1], Propagator[tz][frz,toz,gm2], qa]], 
 (**)
 Topology[s_][ pa___, Propagator[ty_][frz_, toy_, gm1:gmF ], pb___, 
     (h:gmE|gmI)[ Propagator[tz_][frz_, toz_, gm2:gmF ], qa___ ] ] :>
   Topology[s][ pa, pb, 
     h[ Propagator[ty][toy,frz,AntiParticle[gm1]], 
	Propagator[tz][frz,toz,gm2], qa]] ,             
 (* 
   for U's: 
 *)
 Topology[s_][ pa___, Propagator[ty_][toz_, toy_, gm1:gmU ], pb___, 
     (h:gmE|gmI)[ qa___, Propagator[tz_][frz_, toz_, gm2:gmU ] ] ] :>
   Topology[s][ pa, pb, 
     h[ qa, Propagator[tz][frz,toz,gm2], Propagator[ty][toz,toy,gm1]]], 
 Topology[s_][ pa___, Propagator[ty_][fry_, toz_, gm1:gmU ], pb___, 
     (h:gmE|gmI)[ qa___, Propagator[tz_][frz_, toz_, gm2:gmU ] ] ] :>
   Topology[s][ pa, pb, 
     h[ qa, Propagator[tz][frz,toz,gm2], 
	    Propagator[ty][toz,fry,AntiParticle[gm1]]]],             
 Topology[s_][ pa___, Propagator[ty_][fry_, frz_, gm1:gmU ], pb___, 
     (h:gmE|gmI)[ Propagator[tz_][frz_, toz_, gm2:gmU ], qa___ ] ] :>
   Topology[s][ pa, pb, 
     h[ Propagator[ty][fry,frz,gm1], Propagator[tz][frz,toz,gm2], qa]], 
 Topology[s_][ pa___, Propagator[ty_][frz_, toy_, gm1:gmU ], pb___, 
     (h:gmE|gmI)[ Propagator[tz_][frz_, toz_, gm2:gmU ], qa___ ] ] :>
   Topology[s][ pa, pb, 
     h[ Propagator[ty][toy,frz,AntiParticle[gm1]], 
	Propagator[tz][frz,toz,gm2], qa]]             
};

(* put them together:
*)
Grassman = 
{
 Topology[s_][p___] :> 
 ((Topology[s][p] /. GrassmanStart)//. GrassmanRules)
};

(* second step : correcting the combinatorial factor (1), add -1 for 
   every internal grassman chain 
*)

Scorrect1[ tt:Topology[s_][args___] ]:= 
  Topology[ (-1)^(Count[tt,gmI[__]]) s ][args];

Scorrect2[ tt:Topology[s_][args___] ]:= 
  Topology[ (-1)^(Count[tt,dot[__]]) s ][args];

(* third step : building correct fermion chains 
   external/internal chains of Ghosts are thrown away (need them only for
   determination of the sign), external fermion chains get Head "dot" and
   internal chains Head "tr", chains starting with a `positive` fermion
   are reversed (to have the standard ordering for Dirac-fermions, though
   it doesn't matter for the Feynman rules).
*)

PropReverse[ Propagator[ty_][ fr_,to_,fi_ ] ] := 
  Propagator[ty][to, fr, AntiParticle[fi] ];

ToFermionLines = 
{
(* no chains for Ghosts: *)
 Topology[s_][ pa___, 
              (gmE|gmI)[Propagator[ty_][fr_ ,to_ , ghost:gmU ],pb___ ], 
               pc___ ] :>
  Topology[s][ Propagator[ty][fr,to,ghost], pb, pa, pc],
(* these chains are correct: *)
 Topology[s_][ pa___, 
               gmE[ Propagator[ty_][ fr_, to_,-F[in__] ], pb___], 
               pc___ ] :>
  Topology[s][ pa, pc, dot[Propagator[ty][fr,to,-F[in]], pb] ] ,
 Topology[s_][ pa___, 
               gmI[ Propagator[ty_][ fr_, to_,-F[in__] ], pb___], 
               pc___ ] :>
  Topology[s][ pa, pc, tr[Propagator[ty][fr,to,-F[in]], pb] ] ,
(* Reverse propagator for Diracfermions in wrong direction (i.e. if they do
   not carry a minus sign). Majos have no sign.  *)
 Topology[s_][ pa___, 
               the:gmE[ Propagator[ty_][ fr_ ,to_, F[in__] ], pb___], 
               pc___ ] :>
  Topology[s][ pa , pc , dot@@ (Reverse[ PropReverse/@ the ])  ],
 Topology[s_][ pa___, 
               the:gmI[ Propagator[ty_][ fr_ ,to_, F[in__] ], pb___], 
               pc___ ] :>
  Topology[s][ pa , pc , tr@@ (Reverse[ PropReverse/@ the ])  ]
};

(* HERE: *)
Totr[ anything__ ] :=
  tr[ Sequence@@
      Union[ Cases[{anything}, Propagator[a_,b_,field_] :> field
                  ] /. -F[c__] :> F[c]
           ]
    ][ anything ];

(* fourth step : extracting the extern fermion numbers 
   the amplitude now contains Propagator's, dot's and tr's.
*)

ExtractExt[ Propagator[_][___] ] := {};

ExtractExt[ dot[ Propagator[_][Vertex[1,0][a_],__], ___,
		 Propagator[_][_,Vertex[1,0][b_],__] ] ] := {a,b};

ExtractExt[ tr[ ___ ] ] := {};

ExtractPermutables[ tt:Topology[_][__] ] := Join@@ ( ExtractExt/@ tt ) ;

(*  fifth step : check relative sign of extern fermions 
*)
PermutationSign[ p_List ] := 
   Block[ {l=p,i,j,h,len,exp=0} ,
          len=Length[ p ] ;
          For[ j=1 , j<=len, j++ , 
          For[ i=1 , i<len , i++ ,
               If[ l[[i]] > l[[i+1]] ,
                   h=l[[i]] ; l[[i]]=l[[i+1]] ; l[[i+1]]=h ; exp++ ]
             ] ] ;
          Return[ (-1)^(exp+len/2) ]
        ];

(* putting everything together:
*) 

TopToGrassman[ tt:Topology[ s_ ][ pr__ ] ] :=
Block[ {fermfac, gmtop},

       (* constructing the chains gmE/gmI: 
        *)
	 gmtop = tt //.Grassman;

       (* throw away fermion lines if $FermionLines=False:
        *)
         If[ Not[ $FermionLines ],
             gmtop = gmtop //. { 
                 (gmI|gmE)[ferm__]:>seq[ferm] /; Not[FreeQ[{ferm}, _.F[__]]]
                } /. seq->Sequence
           ];

       (* give (-1) for every Grassman chain: 
        *)
	  gmtop = Scorrect1[ gmtop ];

       (* no Heads for ghosts, dot/tr-Heads for fermions: 
        *)
	  gmtop = gmtop//.ToFermionLines;

       (* give (-1) for dot's (= external fermion lines) to compensate 
        * for Scorrect1: 
        *)
	  gmtop = Scorrect2[ gmtop ];

        (* relative sign of amplitudes with external fermions:
         *)
          fermfac = PermutationSign[ ExtractPermutables[gmtop] ] ;
          gmtop = gmtop/. Topology[ ss_ ]->Topology[ ss fermfac ] ;

          Return[ gmtop ];
        ];
	
(*
 * :Part4: 
 *      CreateAmplitude.
 *	Insert analytic expressions into a topology. The topology contains
 *	propagators of the form Propagator[ from, to, field ] and 
 *	SpinorChains collecting propagators containing F's or U's ordered
 *	against the fermion flow. The fields contain the momentum they carry
 *      and the kinematical indices as last two elements. Whether field
 *      indices are present or not depends on the level of insertion.
 *)

CreateAmplitude[ acTop:Topology[ sym_ ][ ps__ ] , theq_List , opt_ ] :=
Block[ { theamp,grules,trules,dummy1,dummy2 },

    (* create rules for the gauge parameters and truncation: 
     *)
      grules = GaugeRules/.opt;
      If[ Head[grules]=!=List, grules={grules} ];
      trules = M$TruncationRules;
      If[ Head[trules]=!=List, trules={trules} ];

      theamp = acTop;

    (* construct MatrixTraceFactors:
     *)
      theamp = IsolateMatrixTraceFactor[ theamp ];

    (* construct vertices (Utilities`) and put'em in the right places: 
     *)
      theamp = VertexHandling[ theamp ];

    (* Vertex->AnalyticalCoupling: 
     *)
      theamp = VertInsert[ theamp ];

    (* Propagator->AnalyticalPropagator: 
     *)
      theamp = PropInsert[ theamp ] /.grules ;

    (* split theamp -> scalars*spinorchains, Head-> FeynAmp, insert Mult: 
     *)
      theamp = FeynMultiplication[ theamp ];

    (* collect the PropagatorDenominators: 
      *)
      theamp = CollectPD[ theamp ];

    (* expand some arguments: 
     *)
      theamp = theamp/.{Global`FourVector->lv, PropagatorDenominator->df,
			FeynAmpDenominator->dn};

    (* truncate external wave functions: 
     *)
      If[ Truncated/.opt , theamp = theamp/.trules ];

    (* prepend integration momenta: 
     *)
      theamp = Prepend[ theamp, List@@theq ]; 

    (* treat dummy indices of QCD: 
     *)
      theamp = theamp /. DummyRule    ;

    (* styling of output, e.g. remove dot|tr : 
     *)
      theamp =  theamp//.StylingRules;   

    (* perform multiplication if not switched off: 
     *)
      If[ (HoldTimes/.opt) === False,
          theamp = MultToTimes[theamp]
        ];

      Return[ theamp//.M$LastGenericRules ]
];

(* CreateAmplitude - functions :
*)

(* IsolateMatrixTraceFactor: searches for 'tr' in the amplitude and
 * inserts MatrixTracefactor[{fields1}, {fields2}, ..], where fieldsX
 * is the list of fields of traceX.
 * We have to do this since it is not possible to find the MatrixTraceFactor
 * on generic level.
 * from the tr's we take the propagator's third element (field) and drop its
 * last element (momentum).
 *)
IsolateMatrixTraceFactor[ Topology[ s_ ][ elements__ ] ] :=
  ReleaseHold[
  Topology[s * MatrixTraceFactor@@(
               Cases[ {elements}, 
                      tr[props__] :> 
                      (Drop[ (#[[3]]/.-F[any__]:>F[any]), -1 ]& /@ {props}) 
                    ]
              ) /. MatrixTraceFactor[] -> Hold[Sequence[]]
           ][elements]
  ] 

(* VertexHandling: construct all vertices of a topology, put vertices along
 * spinor chains into the correct positions. 
 *)
VertexHandling[ Topology[ s_ ][ elements__ ] ] :=
  ( Topology[s][ Union[ ConstructFieldPoints[ 
		       Topology[s][elements] /. {dot->List, tr->List} ] ],
		elements 
	      ] //.VPositionRules ) //.FPositionRules;

VPositionRules = {
 Topology[s_][ { listofvert__ } , otherstuff__ 
             ] :> 
      Topology[s][ listofvert , otherstuff ] ,
 Topology[s_][ a___, Unsorted[ thev_ , vv_List ], b___ ,
               (h:(dot|tr))[ pa___,p:Propagator[_][_,thev_,__], pb___ ],
	       c___ 
	     ] :>
      Topology[s][ a , b , h[pa,p,Unsorted[thev,vv],pb] , c ]
   };

FPositionRules = {
   (h:(dot|tr))[ pa___, 
          p:Propagator[_][_,_, thef_ ],
          Unsorted[v_, {any_,thef_,more__}], 
                 pb___ ] :>
   h[ pa, p, Unsorted[ v, {thef, any, more}], pb ],

  (* this one's more general: if the fermions carry kinematical indices,
   * the fields in the propagator and the vertex are not equal
   *)
   (h:(dot|tr))[ pa___, 
          p:Propagator[_][_,_, s_. F[ index_ , pstuff___ ] ],
         Unsorted[v_, {any_, s_. F[ index_, vstuff___],more__}], 
                 pb___ ] :>
   h[ pa, p, Unsorted[ v, {s F[index,vstuff], any, more}], pb ]
};

(* VertInsert: replace Vertex by AnalyticalCoupling: automatic insertion 
 * of the couplings. Vertices are in generic standard ordering from the
 * function ConstructFieldPoints.
 *)
VertInsert[ tt:Topology[s_][elem__] ] :=
  tt /.  Unsorted[Vertex[_,c_][_],vv_List ,___]   :> 
           AnalyticalCoupling[c] @@ vv ;

(* PropInsert: replace Propagator by AnalyticalPropagator: automatic insertion 
 * of the propagators.
 *)
PropInsert[ tt:Topology[s_][elem__] ] := 
  tt /. Propagator[ty_][ff__] :> PropOrder[Propagator[ty][ff]] ;

(* PropOrder:
 * In ancient FeynArts, this function was also used to find the correct
 * direction of the Internal fermion propagators. 
 * This should not be necessary anymore. (See defnitions of the propagators 
 * in the Mayo-paper)
 *)
PropOrder[ Propagator[type_][from_, to_, part_ ] ] :=
Block[ { newtype, retexpr },

     (* first try:
      *)
       retexpr = AnalyticalPropagator[type][part];

     (* replace Loop-propagators:
      *)
       newtype = type /. {Loop[_] :>Internal};
       If[ !( Head[ retexpr ] === PV ) ,
           retexpr =  AnalyticalPropagator[ newtype ][ part ] 
         ];

     (* replace external proapagators:
      *)
       newtype = newtype /. {Incoming->External,Outgoing->External};
       If[ !( Head[ retexpr ] === PV ) ,
           retexpr =  AnalyticalPropagator[ newtype ][ part ] 
         ];

     (* check expression:
      *)
       If[ !( Head[ retexpr ] === PV ),
          Message[ Propagator::nores, part ];
          retexpr = Propagator[part]
         ];
 
      Return[retexpr]
];

(* FeynMultiplication. 
 * Find scalars and spinorchains, i.e. translate PV and dot|tr to 
 * "FeynAmp[ scalars * spinorchains ].
 *)

SpCrule0 =   (* initialization *)
 {
 (* get rid of one-dimensional coupling vectors:
  *)
  (g:(G[_][_][__][])).{d_} :> g*d,
 (* collect NonCommutatives from the vector multiplication:
  *) 
  a___ + s1_. NonCommutative[g1__] + s2_. NonCommutative[g2__] + b___ :>
   a + NonCommutative[ s1 Dot[g1] + s2 Dot[g2] ] + b
 };

SpCrule1 =   (* separation inside dot|tr *)
 { 
  (h:(dot|tr))[ PV[ NonCommutative[ gp1__ ]  sc1_. ] ,
                PV[ NonCommutative[ gp2__ ]  sc2_. ] , sth___ ] :>
    h[ PV[ NonCommutative[ gp1 , gp2 ]  Mult[sc1,sc2] ] , sth ],
  (h:(dot|tr))[ PV[ sc1_ ] , 
                PV[ NonCommutative[ gp2__ ]  sc2_. ] , sth___ ] :>
    h[ PV[ NonCommutative[ gp2 ]  Mult[sc1, sc2] ] , sth ],
  (h:(dot|tr))[ PV[ NonCommutative[ gp1__ ]  sc1_. ] , 
                PV[  sc2_ ] , sth___ ] :>
    h[ PV[ NonCommutative[ gp1 ]  Mult[sc1,sc2] ] , sth ]
  };

SpCrule2 =   (* throw out scalars from dot|tr *)
 {
  (h:(dot|tr))[ PV[ NonCommutative[ sth___ ]  scal_. ] ] :> 
    PV[ Mult[ h[ sth ], scal] ]
  };

Multrule =   (* multiply all *)
 {
  Topology[sym_][ pat:PV[ _ ].. ]:>
     FeynAmp[ Mult[ sym , (Mult[pat]/.PV->Mult) ] ]
  };
	   
FeynMultiplication[ tt:Topology[_][__] ] := 
   tt //.SpCrule0//.SpCrule1//.SpCrule2/.Multrule;

(* collect PropagatorDenominators: 
 *)
AllnonPD[ l_ ]:=
  Select[ l[[1]], (FreeQ[#,PropagatorDenominator] || FreeQ[#,Internal])& ];

AllPD[ l_ ] := 
  ( Expand[#,PropagatorDenominator]& [ 
      Select[ l[[1]], 
	      !(FreeQ[#,PropagatorDenominator] || FreeQ[#,Internal])&
	    ] 
      ] /.rule1 /.rule2 
  ) //.rule3 ;

rule1 = f_. PropagatorDenominator[a__] :> FeynAmpDenominator[ f prden[a] ];
rule2 = f__ PropagatorDenominator[a__] :> FeynAmpDenominator[ f prden[a] ];
rule3 = Mult[ FeynAmpDenominator[ a___], FeynAmpDenominator[ b___ ] ] :>
          FeynAmpDenominator[ a , b ];

CollectPD[ x_ ] :=
If[ FreeQ[ x, PropagatorDenominator],
   x,
   FeynAmp[ Mult[ AllnonPD[x], 
		  AllPD[x] /. prden -> PropagatorDenominator ] ]
  ];

(* Expand FeynAmpDenominator, PropagatorDenominator and Global`FourVector 
 * arguments: 
 *)
lv[ mom_ , args___ ] := Global`FourVector[ Expand[mom] , args ];

df[ mom_, mass_ ] := 
  If[ MemberQ[ mom , -FourMomentum[Internal,1] ] ||
        Head[ First[ mom ] ]===Times ||
        MatchQ[ mom, -FourMomentum[Internal,_Integer] ] ,
     PropagatorDenominator[ Expand[-mom] , mass ],
     PropagatorDenominator[ Expand[mom]  , mass ]
    ]/;!(mom===0);

df[ 0 , mass_ ] := PropagatorDenominator[ 0, mass ];

dn[ allfactors___ ] := Sort[ FeynAmpDenominator[ allfactors ] ];

(* If a dummy index appears in the FR for QCD: substitute it by approprate
   index gi[n].
*)
DummyRule =  
 FeynAmp[a___] :> If[ FreeQ[{a},dummy], FeynAmp[a], DummyTreat[FeynAmp[a]] ];

DummyTreat[ FeynAmp[amp__] ] := 
  DummyTreat[ FeynAmp[amp],
	      Last[ Sort[ Cases[{amp}, gi[_], Infinity] ] ][[1]] 
	    ];

DummyTreat[ FeynAmp[amp__], high_Integer ] :=
Block[ {newamp,thisdummypair,thisgipair},
      If[ FreeQ[ {amp},dummy ],
         Return[ FeynAmp[amp] ],
         newamp=FeynAmp[amp] /. 
	  ( SU3F[a___, dummy, b___] * SU3F[c___, dummy, d___] :>
          Prod[SU3F[a, dummy, b], SU3F[c, dummy, d]] );
         thisdummypair = Cases[newamp, Prod[___], Infinity][[1]];
         thisgipair = thisdummypair /. dummy->gi[high+1];
         DummyTreat[ (newamp/.thisdummypair:>thisgipair)/.Prod->Times, 
		     high+1 
		   ]
        ] 
  ];

(* perform multiplication of Feynman rules: 
*)
MultToTimes[ FeynAmp[a___] ] := 
  Simplify[ FeynAmp[a] /. MTRule1 //. MTRule2 ];

MTRule1 = Mult -> Times;

MTRule2 = 
 FeynAmpDenominator[ a___, f__ PropagatorDenominator[g__], c___ ] :>
  Times[f] FeynAmpDenominator[ a, PropagatorDenominator[g], c ];

(* styling of the amplitude:
 *)
StylingRules = {

 (* internal momenta to beginning of denominator 
  *)
    FeynAmpDenominator[ a___PropagatorDenominator, 
               PropagatorDenominator[FourMomentum[Internal,1],mass_], 
               b___PropagatorDenominator ] :>
     FeynAmpDenominator[ PropagatorDenominator[
			   FourMomentum[Internal,1],mass ], a, b ],

 (* make q1 positive in all denominators 
  *)
    PropagatorDenominator[ Plus[ a___, 
	       - FourMomentum[Internal,1] , b___ ], mass_ ] :>
     PropagatorDenominator[ 
	     Expand[ - Plus[-FourMomentum[Internal,1],a,b] ] , mass ],

 (* fermion chains: 
  *)
    dot -> FermionChain,
    tr -> MatrixTrace,
    MatrixTrace[Dot[]]:> 1,

 (* list of integration momenta -> Integral 
  *)
    FeynAmp[n___,List[  mom___ ],amp_] :> FeynAmp[ n, Integral[mom], amp ]

};

(*
  :Part5:
	PickLevel. Creates amplitudes on different levels from generic
	amplitudes. For each of the levels we pick out, we add a running
        number. This is just for convenience, the running numbers are not
        unique like the numbers from the insertion process.
*)

PickLevel::noclass = 
"Amplitudes have no Classes insertions (only Generic).";

PickLevel::nopart = 
"Amplitudes have no Particles insertions (only Classes).";

(* this adds the running number as Number==n to the GraphName:
*)
RunningNumber[ fa_, type_ ] :=
  Head[fa]@@ Array[ (fa[[#]]/.GraphName[any___]:>GraphName[any,type==#]
                    )&, Length[fa]
                  ];

PickLevel[Generic][ FeynAmpList[h___][amps___] ] :=
  RunningNumber[ Drop[#,-1]& /@ FeynAmpList[h][amps], Number ];

PickLevel[Classes][ FeynAmpList[h___][amps___] ] :=
Block[ {rulelist, zwi, result},
 
    (* check whether classes level is present:
     *)
      If[ FreeQ[ {amps}, Classes ], 
          Message[ PickLevel::noclass ];
          Return[$Aborted]
        ];

    (* huge pure function for picking the levels:
     *)
      result = Function[ z,
         (* next-to-last element is the analytic amplitude (head Times) *)
	  zwi = ReplacePart[ z, Prepend[ z[[-2]], RelativeCF ], -2 ];  
         (* pickout the Insertions[Classes]-list from the last element: *)
	  rulelist = Thread[ Rule[ zwi[[-1,1]], # ] ]& /@ 
		If[ FreeQ[ {amps}, Particles ],
		    zwi[[-1,2]],
		    First /@ zwi[[-1,2]]
		  ]; 
          (* now apply all rules, add Classes-numbering and turn it into
             a Sequence (List stems from rulelist) *)
	    Sequence @@ 
             RunningNumber[ (Append[Drop[zwi,-2], zwi[[-2]] /. # ]
                            )& /@ rulelist,
                            Classes
                          ] 
          ] /@ FeynAmpList[h][amps];

  RunningNumber[ result, Number ]
];

PickLevel[Particles][ FeynAmpList[h___][amps___] ] :=
Block[ {rulefrom, ruleto, ampl, rules, result},
    
    (* check whether Particles level is present:
     *)
      If[ FreeQ[ {amps}, Particles ], 
          Message[ PickLevel::nopart ];
          Return[$Aborted]
        ];
     
    (* apply a huge pure function to the amplitude list:
     *)
      result = Function[ y,
         (* separation of amplitude and rules: *)
          ampl = Drop[ y, -1 ];
          rulefrom = y[[-1,1]];
          ruleto = y[[-1,2]]; 
         (* again, we first multiply the amplitude by RelativeCF: *)
          ampl = ReplacePart[ampl, Prepend[ampl[[-1]], RelativeCF], -1];
         (* now we make lists of amplitudes and rules *)
          If[ Head[ruleto]===Insertions[Classes],
              ampl = RunningNumber[ Table[ampl, {Length[ruleto]}], Classes];
              rules = 
                 List@@(
                 SecondThread[Rule[rulefrom, #]]& /@ (
                 ruleto/. Rule[_,Insertions[Particles][ins___]]:>{ins})),
              ampl = {ampl};
              rules = { Thread[Rule[rulefrom, #]]& /@ (List@@ruleto) }
            ];
         (* apply rules for every classes insertion: *)
          Sequence@@ 
            Flatten[ Array[RunningNumber[ampl[[#]]/.rules[[#]], Particles]&,
                           Length[ampl] ]
                   ]
         ] /@ FeynAmpList[h][amps]; 

  Return[ RunningNumber[ result, Number ] ]
];

(* SecondThread: go one level deeper in ``threading'':
*)
SecondThread[ f_[ a_List, b_List ] ] := Thread[ f[a, #] ]& /@ b;

End[] (* HighEnergyPhysics`FeynArts`Analytic` *)

(**)
