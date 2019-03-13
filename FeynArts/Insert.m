(*
	Insert.m
		Insertion of fields into topologies created by 
		CreateTopologies.
		last modified 7 Mar 19 th

The insertion is done in 3 levels: insertion of generic fields (Generic),
of classes of a certain model (Classes) or of the members of the classes
(Particles).

Models are described in model files which are supposed to exist somewhere
on the $ModelPath.  At the beginning of an insertion InsertFields calls
InitializeModel that checks whether the model is initialized or not and
performs the initialization if needed.

*)

Begin["`Insert`"]

Options[InsertFields] = {
  Model -> "SM",
  GenericModel -> "Lorentz",
  InsertionLevel -> Classes,
  ExcludeParticles -> {}, 
  ExcludeFieldPoints -> {},
  Restrictions -> {},
  LastSelections -> {}
} 

InsertFields::syntax =
"The syntax of InsertFields is InsertFields[tops, in -> out, options]."

InsertFields::extnumber = 
"You cannot fit `` -> `` external particles onto a `` -> `` leg \
topology."
			  
InsertFields::badparticle = 
"Particle `` does not live in model ``."

InsertFields::badsel =
"Element `` of LastSelections is not a proper field specification."


InsertFields[top:P$Topology, args__] := InsertFields[TopologyList[top], args]

InsertFields[tops:(TopologyList | _TopologyList)[___],
  initial_ -> final_, options___Rule] :=
Block[ {proc, ilevel, excl, last, omit, need, 
res, ninc, nout, pinc, pout, fields, topnr = 0, fieldnr = 0,
opt = ActualOptions[InsertFields, options]},

  If[ (ilevel = ResolveLevel[InsertionLevel /. opt]) === $Failed,
    Return[$Failed] ];

  If[ InitializeModel[Model /. opt,
        GenericModel -> (GenericModel /. opt),
        Reinitialize -> False] =!= True,
    Return[$Failed] ];

  proc = Flatten[{initial}] -> Flatten[{final}] /. _Integer p_Symbol :> p;

  If[ Or@@ (If[ InModelQ[#], False,
    Message[InsertFields::badparticle, #, $Model]; True ]&)/@
      Join@@ proc,
    Return[$Failed] ];

  proc = Map[AppendIndex[Field[++fieldnr] -> #][[2]]&, proc, {2}];

  excl = RestrictCurrentModel[Restrictions /. opt,
    Cases[opt, _[ExcludeFieldPoints | ExcludeParticles, _]]];
  last = Union[Flatten[{LastSelections /. opt}, 1]] /.
    (f:P$Generic)[i_, {j__}] -> f[i, {j, ___}];

  If[ Length[res = tops] > 0,
    ninc = Length @ proc[[1]];
    nout = Length @ proc[[2]];
    res = res /. Propagator[External][Vertex[1][j_], v2__] :>
      Propagator[If[j > ninc, Outgoing, Incoming]][Vertex[1][j], v2];

    pinc = Count[res[[1]], Incoming, Infinity, Heads -> True];
    pout = Count[res[[1]], Outgoing, Infinity, Heads -> True];
    If[ ninc =!= pinc || nout =!= pout,
      Message[InsertFields::extnumber, ninc, nout, pinc, pout];
      Return[$Failed] ];

    omit = Union[CheckProperField/@ Flatten[{
      Cases[last, !p_ :> {p, AntiParticle[p]}],
      Cases[ExcludeParticles /. excl, _. P$Generic[_, _]] }]];
    need = CheckProperField/@ Complement[last /. !p_ :> p, omit];

    FAPrint[2, ""];
    FAPrint[2, "inserting at level(s) ", ilevel];

    fields = MapIndexed[Field@@ #2 -> #1 &,
      Join[ proc[[1]], AntiParticle/@ proc[[2]] ]];
    ninc = Length[fields];
    res = TopologyInsert[Last[ilevel]]/@ res
  ];

  RestrictCurrentModel[];

  FAPrint[1, "in total: ", Statistics[res, ilevel, " insertion"]];

  PickLevel[ilevel][TopologyList[
    Process -> proc,
    Model -> $Model,
    GenericModel -> $GenericModel,
    InsertionLevel -> ilevel,
    Sequence@@ excl,
    LastSelections -> last ]@@ res] /. Insertions -> NumberInsertions
]

_InsertFields := (Message[InsertFields::syntax]; $Failed)


CheckProperField[fi_Alternatives] := CheckProperField/@ fi

CheckProperField[fi:P$Generic | _. P$Generic[__]] := fi

CheckProperField[fi_] := (Message[InsertFields::badsel, fi]; Seq[])


ValidIndexQ[{i_, j___}, {fi_, f___}] :=
  If[ IntegerQ[i] && !MemberQ[IndexRange[fi], i], False, ValidIndexQ[{j}, {f}] ]

ValidIndexQ[{}, _] = True

_ValidIndexQ = False


InModelQ[s_. fi_[i_Integer, j_List]] := InModelQ[s fi[i]] &&
  Block[{NoUnfold = Identity}, ValidIndexQ[j, Indices[fi[i]]]]

InModelQ[p_] := MemberQ[F$AllowedFields, p]


FieldToString[fi_[n_, ___]] := ToString[fi] <> ToString[n]

FieldToString[-fi_[n_, ___]] := ToLowerCase[ToString[fi]] <> ToString[n]

FieldToString[_] = "_"


(* add numbers to the Heads of each FeynmanGraph, i.e.
   FeynmanGraph[sym] -> FeynmanGraph[sym, lev == num] *)

AddNumbering[FeynmanGraph[s_, ___][args__] -> ins_, n_] :=
  FeynmanGraph[s, n][args] -> ins

AddNumbering[FeynmanGraph[s_, ___][args__], n_] :=
  FeynmanGraph[s, n][args]

NumberInsertions[lev_][gr___] :=
  Insertions[lev]@@ MapIndexed[AddNumbering[ #1, lev == #2[[1]] ]&, {gr}]


(* TopologyInsert: insert fields into one topology *)

(t_TopologyInsert)[Topology[prop__] -> ins_] := t[Topology[1][prop] -> ins]

(t_TopologyInsert)[Topology[prop__]] := t[Topology[1][prop]]

(* start from scratch if someone wants to re-insert Generic level *)

TopologyInsert[Generic][top_ -> Insertions[_][__]] := TopologyInsert[Generic][top]

(* allow insertion on top of old insertions only if external particles
   are the same: *)

(t_TopologyInsert)[top_ -> Insertions[_][ins_, ___]] := t[top] /;
  fields =!= List@@ Take[ins, ninc]

(* append Insertions template: *)

ReplaceFieldNo[prop_[from_, to_, ___], {n_}] := prop[from, to, Field[n]]

(t_TopologyInsert)[top:Topology[_][__]] :=
  t[ MapIndexed[ReplaceFieldNo, top] -> {Level[{fields,
    Table[Field[n] -> 0, {n, ninc + 1, Length[top]}]}, {2}, FeynmanGraph]} ]

TopologyInsert[lev_][top:Topology[_][__] -> ins_] :=
Block[ {vertli, fpoints, res, topol = top},
  fpoints = Map[VertexFields[top],
    DeleteCases[Take[#, 2], Vertex[1][_]]&/@ top, {2}];
  vertli = Union[Flatten[Apply[List, fpoints, {0, 1}]]];

	(* if the field points exist with the external particles
	   inserted, start insertion process *)
  res = If[ VectorQ[vertli /. ToClasses[fields], CheckFP],
    Catch[DoInsert[lev][ins]],
  (* else *)
    Insertions[Generic][] ];
  FAPrint[2, "> Top. ", ++topnr, ": ",
    Statistics[top -> res, ilevel, " insertion"]];
  top -> res
]

TopologyInsert[_][other_] := other


(* extract field points *)

VertexFields[top_][v_] := Level[TakeInc[v]/@ top, {2}, FieldPoint[CTO[v]]]

CTO[Vertex[_, c_][__]] := c

CTO[_] = 0


FieldPoints[gr_:{}, top:P$Topology, ___] := Sort/@
  (VertexFields[AddFieldNo[top]]/@ Vertices[top] /. List@@ gr)


(* get particle which is incoming in vertex v from propagator pr *)

TakeInc[v_][_[v_, v_, f_, ___]] := {IncField[f], OutField[f]}

TakeInc[v_][_[v_, _, f_, ___]] := {IncField[f]}

TakeInc[v_][_[_, v_, f_, ___]] := {OutField[f]}

TakeInc[_][_] = {}


IncField[s_. fi_[ind__, fr_ -> _]] := AntiParticle[s fi[ind, fr]]

IncField[fi_] := AntiParticle[fi]

OutField[s_. fi_[ind__, _ -> to_]] := s fi[ind, to]

OutField[fi_] := fi


RightPartner[fi_] := MixingPartners[fi][[-1]] /; FreeQ[fi, Field]


ParticleLookup[fp_, Mix[L_,R_]] := PLookup[fp, Mix[L,R], L, R]

ParticleLookup[fp_, Rev[L_,R_]] := PLookup[fp, Mix[L,R], R, L]

ParticleLookup[{fp_, r___}, p_] := (lallowed = PLookup[fp, p, p]) /;
  fp === r

ParticleLookup[fp_, p_] := PLookup[fp, p, AntiParticle[p], p]

PLookup[{fpl_, fpr_}, mp_, ap_, p_] := Intersection[
  lallowed = AntiParticle/@ PLookup[fpl, mp, ap],
  rallowed = PLookup[fpr, mp, p] ]

PLookup[fp_, mp_, p_] := Flatten[ Compatibles[mp]/@
  FieldLookup[ fp[[0,1]] ][p, FieldPoint@@ (fp /. _Field -> p)] ]


FieldLookup[cto_?NonNegative] := PossibleFields[cto]

FieldLookup[cto_][0, fp_] := PossibleFields[-cto][0, fp]

FieldLookup[_][p_, fp_] :=
Block[ {n = Position[fp, p, {1}, 1]},
  Select[
    Select[F$Classes, !FreeQ[#, p]&],
    VFAllowed[ReplacePart[fp, #, n]]& ]
]


Attributes[CheckFun] = {HoldFirst}

CheckFun[vf_, p:P$Generic] := ToGeneric[#] === p && vf &

CheckFun[vf_, _] := vf &


CheckFP[fp:FieldPoint[_?NonNegative][__]] :=
  CheckFieldPoint[Sort[fp]]

CheckFP[fp_] :=
  !FreeQ[fp, Field] ||
  ( MemberQ[FieldPointList[Generic], FieldPoint@@ ToGeneric[fp] /. 0 -> _] &&
    VFAllowed[fp] )


VFAllowed[fp_] := True /; MemberQ[fp, 0 | P$Generic]

VFAllowed[fp_] :=
  ViolatesQ@@ Flatten[QuantumNumbers/@ List@@ fp] =!= True


(* Insert compatible particles in 1 propagator for 1 set of rules: *)

Ins11[vert12_, ru_, i_] := 
Block[ {vx, p = ru[[i,2]], lallowed, rallowed, allowed, prop},
  vx = Map[RightPartner, List@@ vert12 /. Delete[List@@ ru, i], {2}];
  allowed = Select[
    Intersection[ParticleLookup[vx, p], F$AllowedFields],
    CheckFun[VectorQ[vx /. Field[i] -> #, CheckFP], $GenericMixing && p] ];

  prop = ResolveType @ vert12[[0,1]];

  If[ TrueQ[$FADebug],
    vx = vert12 /. List@@ ru;
    Print["Ins11: inserting field #", i, " (", p, ") on ", prop];
    Print["Ins11: fields     = ", List@@ ru];
    Print["Ins11: vert12     = ", vert12];
    Print["Ins11: L-vertex   = ", vx[[1]] -> RightPartner/@ vx[[1]] ];
    Print["Ins11: R-vertex   = ", vx[[-1]] -> RightPartner/@ vx[[-1]] ];
    Print["Ins11: L-allowed  = ", lallowed];
    Print["Ins11: R-allowed  = ", rallowed];
    Print["Ins11: allowed    = ", allowed];
    Print[""];
  ];

  ReplacePart[ru, #, {i, 2}]&/@
    Select[allowed, !FreeQ[InsertOnly[#], prop]&]
]


(* insert the ith propagator: *)

Ins1[ru_, i_] :=
Block[ {ins},
  ins = Flatten[Ins11[fpoints[[i]], #, i]&/@ ru];
  If[ Length[ins] === 0, Throw[ins] ];
  ins
]


Need[] = Sequence[]

Need[fi__] := Select[#, ContainsQ[Drop[#, ninc], {fi}]&]&

Omit[] = Sequence[]

Omit[fi__] := Select[#, FreeQ[Drop[#, ninc], Alternatives[fi]]&]&


(* insertion at generic level: *)

DoInsert[Generic][ins_] :=
Block[ {freesites, theins, filter},
  freesites = Flatten[Position[ins[[1]], _ -> 0, 1]];

  filter = Composition[
    Need@@ (Cases[need, P$Generic | _[P$Generic..]] /.
      Mix[fi__] :> Seq[Mix[fi], Rev[fi]]),
    Omit@@ (Cases[omit, P$Generic | _[P$Generic..]] /.
      Mix[fi__] :> Seq[Mix[fi], Rev[fi]]) ];

  theins = Insertions[Generic]@@ InsertionsCompare[
    topol,
    filter @ Catch[Fold[Ins1, ToGeneric[ins], freesites]] ];

		(* restore unstripped rules *)
  theins = theins /.
    Cases[ins[[1]], ru:(fi_ -> p_ /; p =!= 0) -> ((fi -> _) -> ru) ];

  If[ Length[theins] === 0, Throw[theins] ];
  theins
]


(* insertion at classes level: *)

DoInsert[Classes][ins_] :=
Block[ {theins, rul, freesites, filter, pfilter},
  theins = ToClasses[ rul = ins /.
    (x_ -> Insertions[Classes | Particles][___]) -> x ];

	(* Generic fields count as free sites at classes level *)
  freesites = Flatten[Position[theins[[1]], _ -> _?AtomQ, 1]];

  If[ Head[theins] =!= Insertions[Generic],
    theins = DoInsert[Generic][theins]];

  filter = Composition[
    Need@@ Cases[need, _. P$Generic[_] | _[(_. P$Generic[_])..]],
    Omit@@ Cases[omit, _. P$Generic[_] | _[(_. P$Generic[_])..]] ];
  pfilter = If[ Length[$ExcludedParticleFPs] === 0, Identity,
    Select[#, !ExcludedQ[vertli /. List@@ #]&]& ];

  theins = (# -> Insertions[Classes]@@ InsertionsCompare[
    Topology[ #[[0,1]] ]@@ topol,
    filter @ Catch[Fold[Ins1, {FeynmanGraph@@ #}, freesites]] ]
  )&/@ theins;

		(* restore unstripped rules *)
  theins = theins /.
    Cases[rul[[1]], ru:(fi_ -> _?(!AtomQ[#]&)) -> ((fi -> _) -> ru)] /.
    cins:Insertions[Classes][__] :> pfilter[ProvideIndices/@ cins];

  theins = Select[theins, Length[ #[[2]] ] =!= 0 &];
  If[ Length[theins] === 0, Throw[theins] ];
  theins
]


Attributes[IndexDelta] = {Orderless}

IndexDelta[n_, n_] := 1

	(* Caveat: do not discard the n on n_Integer, or else
	   the IndexDelta[n_, n_] rule will apply! *)
IndexDelta[n_Integer, _Integer] = 0

Conjugate[IndexDelta[ind__]] ^:= IndexDelta[ind]

IndexDelta[ind__]^_?Positive ^:= IndexDelta[ind]

ext/: IndexDelta[ext[_], ext[_]] = 1


AppendIndex[Field[n_] -> s_. fi:_[_]] :=
  (Field[n] -> s Append[fi, Append[#, n]&/@ Indices[fi]]) /;
  Indices[fi] =!= {}

AppendIndex[Field[n_] -> s_. fi_[i_, j_List]] :=
  (Field[n] -> s fi[i, Join[ j,
    Append[#, n]&/@ Drop[Indices[fi[i]], Length[j]] ]]) /;
  Length[j] < Length[Indices[fi[i]]]

AppendIndex[other_] := other


ProvideIndices[ru_] :=
Block[ {indexru, extind, vert},
  indexru = AppendIndex/@ ru;
  extind = Alternatives@@ DeleteCases[
    Union@@ Cases[Take[indexru, ninc], _List, Infinity],
    _Integer ];
  vert = VSort/@ Flatten[vertli /. Distribute[
    Thread[MapAt[MixingPartners, #, 2]]&/@ List@@ indexru,
    List ]];
  EvaluateDelta[indexru,
    Union[Flatten[CouplingDeltas/@ vert]] /. e:extind :> ext[e]]
]


EvaluateDelta[expr_, {}] :=
Block[ {c},
  c[_] = ninc;
  c[t_, n_] := c[t, n] = ++c[t];
  expr /. Index[t_, n_] :> Index[t, c[t, n]] /; n > ninc
]

	(* For unequal integers: *)
EvaluateDelta[_, {0, ___}] = Sequence[]

EvaluateDelta[expr_, {IndexDelta[_ext, _Integer], rest___}] :=
  EvaluateDelta[expr, {rest}]

EvaluateDelta[expr_, {IndexDelta[ind1_ext, ind2_], rest___}] :=
  EvaluateDelta[expr /. ind2 -> ind1[[1]], {rest} /. ind2 -> ind1]

EvaluateDelta[expr_, {IndexDelta[ind1_Integer, ind2_], rest___}] :=
  EvaluateDelta[expr /. ind2 -> ind1, {rest} /. ind2 -> ind1]

EvaluateDelta[expr_, {IndexDelta[ind1_, ind2_Index], rest___}] :=
  EvaluateDelta[expr /. ind2 -> ind1, {rest} /. ind2 -> ind1]

EvaluateDelta[expr_, {IndexDelta[ind1_, ind2_], rest___}] :=
  EvaluateDelta[expr /. ind2 -> ind1, {rest} /. ind2 -> ind1]

EvaluateDelta[expr_, {_, rest___}] := 
  EvaluateDelta[expr, {rest}]


(* insertion at particles level: *)

DoInsert[Particles][ins_] :=
Block[ {theins, filter},
  theins = ins /. (x_ -> Insertions[Particles][__]) :> x;
  If[ FreeQ[theins, Insertions[Classes]],
    theins = DoInsert[Classes][theins] ];

  filter = Composition[
    If[ Length[$ExcludedParticleFPs] === 0,
      Identity,
      Select[#, !ExcludedQ[vertli /. List@@ #]&]& ],
    Need@@ Cases[need, _. P$Generic[_, _] | _[(_. P$Generic[_, _])..]],
    Omit@@ Cases[omit, _. P$Generic[_, _] | _[(_. P$Generic[_, _])..]] ];

  theins /. cins:Insertions[Classes][__] :> IndexVariations/@ cins
]


IndexVariations[gr:FeynmanGraph[s_, ___][ru__]] :=
Block[ {ins, li, NoUnfold},
  _NoUnfold = {};
  ins = DeleteCases[
    Thread[# -> IndexRange[Take[#, 1]]]&/@ 
      Union[Cases[{ru}, Index[__], Infinity]], {} ];
  If[ Length[ins] =!= 0,
    ins = Flatten[Outer[li, Sequence@@ ins]] /. li -> List ];
  If[ Length[ins] === 0,
    Return[gr -> filter[Insertions[Particles][gr]]] ];
  ins = InsertionsCompare[Topology[s]@@ topol, filter[{ru} /. ins]];
  gr -> Insertions[Particles]@@ ins
]


InsertionsCompare[_, {}] = {}

InsertionsCompare[top_, ins_] :=
Block[ {f, cmp, fno},
	(* only attempt to compare disjoint insertions *)
  _f = 4711;
  Cases[top, Propagator[t_][_, _, fi_] :> (f[t] *= fi)];
  _f = {};
  cmp = {{f[Internal]}, Cases[DownValues[f], _[_[_[_Loop]], fi_] :> fi]};
  cmp = Apply[Plus, DeleteCases[cmp /. List@@@ ins, _Integer, {4}], {2}];
  cmp = Apply[List, ins[[ Flatten[Position[cmp, #, 1]] ]], {0, 1}]&/@ 
    Union[cmp];
  cmp = TopologyList@@ (Compare[TopologyList@@ (top /. #)]&)/@ cmp;
  fno = List@@ Last/@ top;
  (FeynmanGraph[ #[[0,1]] ]@@ Thread[fno -> Last/@ List@@ #]&)/@ cmp
]

End[]

