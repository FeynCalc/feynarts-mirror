(*
	WriteTeXFile.m
		writes out the couplings of a FeynArts
		model file in TeX form
		last modified 2 Sep 19 th

	Usage:	WriteTeXFile["model"]
*)


BeginPackage["WriteTeXFile`", "FeynArts`"]

WriteTeXFile::usage =
"WriteTeXFile[\"model.mod\"] writes the Feynman rules in model.mod in
TeX format to model.tex."

TeXFile::usage =
"TeXFile is an option of WriteTeXFile.  It specifies the output filename
to use.  If set to Automatic, the name of the model file with extension
\".tex\" is used."

PreFunction::usage =
"PreFunction is an option of WriteTeXFile.  It specifies a function to be
applied to each component of the coupling vector before formatting."

MaxLeaf::usage =
"MaxLeaf is an option of WriteTeXFile.  It specifies the maximum
permissible leaf count per line."

TeX::usage =
"TeX[s] indicates that s is TeX code that is written unmodified to the
output file."

Sym::usage =
"Sym[s, sub, sup] prints as symbol s with subscript sub and superscript
sup, where sub and sup are optional."

delta::usage =
"delta[sym] outputs \"delta sym\"."

SymRules::usage =
"SymRules turns common symbols into their printed form."

ModelSymRules::usage =
"ModelSymRules turns model symbols into their printed form."

WidthRules::usage =
"WidthRules turns common symbols into a form that better approximates their
width.  This is used for computing line breaks."

ModelWidthRules::usage =
"ModelWidthRules turns model symbols into a form that better approximates
their width.  This is used for computing line breaks."

ConjSym::usage =
"ConjSym[f] specifies a conjugation symbol for field f.  Choices are:\n
  ConjSym[f] = \"-\"\n
  -- f prints as f^-, anti-f prints as f^+,\n
  ConjSym[f] = NoDagger\n
  -- f prints as f, anti-f prints as f^\\dagger,\n
  ConjSym[f] = Null\n
  -- f prints as f, anti-f prints as \\bar f."

NoDagger::usage =
"NoDagger is a symbol used with ConjSym to indicate printing of the
conjugate field with a dagger."

Class::usage =
"Class associates each field a class which is used to group the
couplings."

IndexLetter::usage =
"IndexLetter[i] gives the letter with which indices of type i will be
abbreviated."

AAA::usage =
"AAA[x] serves to move x to the front of a product.  The AAA is
not printed."

BracketForm::usage =
"BracketForm[expr] isolates expr from the rest of the expression it
is embedded in and is turned into HoldForm after formatting."


Begin["`Private`"]

template = ReadList[
  System`Private`FindFile[$Input] <> ".tex",
  Record, RecordSeparators -> {} ][[1]]

System`Convert`TeXFormDump`maketex[RowBox[{"TeX", _, arg_String, _}]] :=
  ToExpression[arg]

Format[TeX[s_], OutputForm] := s

Format[BracketForm[expr_], OutputForm] := HoldForm[expr]

TeXEnv[name_, vspace_][args__] := SequenceForm@@ Flatten[{
  TeX["\n\\begin{" <> name <> "}\n"],
  Riffle[{args}, TeX[vspace]],
  TeX["\n\\end{" <> name <> "}"] }]

delta[x_] := SequenceForm["\[Delta]", x]


Sym[s_, sub_:{}, sup_:{}] :=
  sym[Flatten[{s}], MakeList[sub], MakeList[sup]]

MakeList[Null] = {}

MakeList[{}] = {}

MakeList[s_Symbol] = {s}

MakeList[i__] := Riffle[DeleteCases[Flatten[{i}], Null], ","]

TeXsym[s_, sub_, sup_] := Subsuperscript@@ SequenceForm@@@
  Map[ToString, {s, sub, sup /. NoDagger :> Sequence[] /. {a__, ","} -> {a}}, {-1}]

Subscript[s_, _[]] := s

Subsuperscript[s_, sub_, _[]] := Subscript[s, sub]

Subsuperscript[s_, _[], sup_] := Superscript[s, sup]

Subsuperscript[s_, sub_, sup_] := Superscript[Subscript[s, sub], sup]


sym/: sym[x__, {sup___}]^n_Integer?Positive := sym[x, {sup, n}];
sym/: sym[x__, {sup___}]^n_Integer := 1/sym[x, {sup, -n}] /; n < -1

sym/: Conjugate[sym[x__, {sup___}]] := sym[x, {sup, "*"}]

sym/: OverBar[sym[x__, {sup___, TeX["-"]}]] := sym[x, {sup, TeX["+"]}];
sym/: OverBar[sym[x__, {sup___, TeX["+"]}]] := sym[x, {sup, TeX["-"]}]

sym/: OverBar[sym[x__, {sup___, TeX["--"]}]] := sym[x, {sup, TeX["++"]}];
sym/: OverBar[sym[x__, {sup___, TeX["++"]}]] := sym[x, {sup, TeX["--"]}]

sym/: OverBar[sym[x__, {sup___, NoDagger}]] := sym[x, {sup, "\[Dagger]"}];
sym/: OverBar[sym[x__, {sup___, "\[Dagger]"}]] := sym[x, {sup, NoDagger}]

sym/: OverBar[sym[x_, subp__]] := sym[OverBar/@ x, subp]


Attributes[MakeFieldRules] = {Listable}

MakeFieldRules[f_ == desc_] :=
Block[ {Index = Identity, i = Indices /. desc, j, Sym},
  If[ Length[i] > 0,
    SetDelayed@@ (Hold@@ {
      IndexRules[_. Append[f, Pattern[#, Blank[]]&/@ i], {n_}],
      MapThread[#1 :> #2 <> ToString[n] &, {i, IndexLetter/@ i}]
    } /. RuleDelayed -> Rule) ];
  RuleDelayed@@ {
    If[Length[i] === 0, f, Append[f, j___]],
    ToSym[PropagatorLabel /. desc /. Thread[i -> Array[iMap[j], Length[i]]],
      AddConj[SelfConjugate /. desc, ConjSym[f]]] }
]

IndexLetter[i_] := ToLowerCase[StringTake[ToString[i], 1]]

iMap[][___] = iMap[{}][___] = {}

iMap[j_List][n_] := j[[n]] /. Null -> {}


TeXStr[s_String] := TeX[s]

TeXStr[other_] := other


Attributes[ToSym] = {Listable}

ToSym[ComposedChar[s_, sub_, sup_, "\\tilde"], h_] :=
  h[OverTilde[TeXStr[s]], TeXStr[sub], TeXStr[sup]]

ToSym[ComposedChar[s_, sub_, sup_, bar_], h_] :=
  h[{TeX[bar <> "{"], TeXStr[s], TeX["}"]}, TeXStr[sub], TeXStr[sup]]

ToSym[ComposedChar[s_, sub_:Null, sup_:Null], h_] :=
  h[TeX[s], TeXStr[sub], TeXStr[sup]]

ToSym[other_, h_] := h[TeX[other], Null, Null]


AddConj[True, _] = AddConj[False, Null] = Sym

AddConj[False, c_][s_, sub_, sup_] := Sym[s, sub, {sup, c}]


ToBar[-f_] := OverBar[f]

ToBar[f_] := f


MakeSum[] = sum

MakeSum[{v1_, r__}, a___, {v2_, r__}, b___] :=
  MakeSum[{{v1, v2}, r}, a, b]

MakeSum[{var_, from_:1, to_}, a___][b___, expr_] := MakeSum[a][b,
  sym[{TeX["\\sum\\limits"]}, {MakeList[var], "=", from} //Flatten, to],
  expr]


sum[s__, expr_] := -sum[s, -expr] /; MinusInFrontQ[expr]

sum[s__, ZPlusB[a_, b__]] := -sum[s, ZPlusB@@ -{a, b}] /; 
  MinusInFrontQ[a]


widthRules := widthRules = Join[ModelWidthRules, WidthRules]

symRules := symRules = Join[ModelSymRules, SymRules]


(* splitting up long expressions *)

SmallEnough[expr__] := LeafCount[{expr} /. widthRules] < maxleaf


Attributes[SplitLongPlus] = {HoldAll}

SplitLongPlus[x__] := Plus[x] /; SmallEnough[x]

SplitLongPlus[x__] :=
Block[ {cb, maxleaf = maxleaf - 5},
  ZPlusB@@ Flatten[ Operate[Coalesce, Plus[x]] ]
]


Coalesce[h_][a_, b_, r___] := Coalesce[h][{a, b}, r] /; SmallEnough[a, b]

Coalesce[h_][a_, r___] := cb[ h@@ Flatten[{a}], Coalesce[h][r] ]

Coalesce[_][] = Sequence[]


(* ordering inside a product *)

MinusInFrontQ[p_Plus] := MinusInFrontQ @ First[p]

MinusInFrontQ[_?Negative _.] = True

MinusInFrontQ[_] = False


TimesS[r__] :=
  If[MinusInFrontQ[#], TimesO[-1, -#], TimesO[1, #]]& @ Times[r]

TimesO[x_, r_. p_Plus] := If[ MinusInFrontQ[p],
  TimesO[-x ZPlusA@@ -p, r],
  TimesO[x ZPlusA@@ p, r] ]

TimesO[x_, r_. p_Plus^n_?Positive] := If[ MinusInFrontQ[p],
  TimesO[x (-1)^n (-p)^n, r],
  TimesO[x p^n, r] ]

TimesO[x_, r_. p_Plus^n_?Negative] :=
  TimesO[x (-1)^n, r (-p)^n] /; MinusInFrontQ[p]

TimesO[x_, r_. ZPlusB[a_, b__]] := If[ MinusInFrontQ[a],
  TimesO[-x ZPlusB@@ -{a, b}, r],
  TimesO[x ZPlusB[a, b], r] ]

TimesO[x_, r_. t_sum] := TimesO[x t, r]

TimesO[x_, r_] := If[Denominator[r] =!= 1, x HoldForm[r], x r] /. ZPlusA -> Plus


HoldTimes[t_Times] := HoldTimes@@ t


TeXZPlusB[a__] := TeXEnv["PlusB", "\\\\\n"]@@ AddSigns[a]

AddSigns[a_, b___] := Partition[Flatten @ {a,
  If[MinusInFrontQ[#], {TeX["\,-"], -#}, {TeX["\,+"], #}]&/@ {b}, 
  ""}, 2]


CVcomp[t_Times] := Flatten[Replace[List@@ t (*Cases[t, Except[_Plus]]*), {
  n_?Negative :> {-1, -n},
  Complex[0, n_?Negative] :> {-1, -n, I},
  Complex[0, n_] :> {n, I},
  p_Plus?MinusInFrontQ :> {-1, -p}
}, 1]]

CVcomp[0] := Sequence[]

_CVcomp = {}


ToCoupVec[{c_}] := CoupVec[1][c]

ToCoupVec[cv_List] :=
  (CoupVec[#]@@ (cv/# /. p_Plus/q_Plus :> -1 /; p + q == 0))&[
    Times@@ Intersection@@ CVcomp/@ cv ]


TeXCoupVec[1][args__] := TeXEnv["CoupVec", "\n\\Next\n"]@@
  HoldForm/@ {args}
(*  Replace[HoldForm/@ {args}, HoldForm[x_] :> x, {2, Infinity}]*)

TeXCoupVec[pre_][args__] := SequenceForm[HoldForm[pre], TeXCoupVec[1][args]]


(* sorting into classes of couplings *)

ToTeX[c_, lhs_, rhs_, n_] :=
Block[ {cl, cr},
  WriteString["stdout", ToString[n], "\r"];
  {cl, cr} = {ToBar/@ lhs, Global`CVRAW[n] = ToCoupVec[Global`CVORIG[n] = prefunc/@ rhs]} /.
    Join@@ MapIndexed[IndexRules, lhs] /.
    Plus -> SplitLongPlus //.
    symRules /.
    FieldRules /.
    { Conjugate[(t:Times)[a__]] :> Conjugate[Sym[{"(", a, ")"}]],
      Conjugate[x_] :> Conjugate[Sym[x]] } //.
    IndexSum[expr_, i__] :> MakeSum[i][expr] /.
    sym -> TeXsym /.
    BracketForm[p_Plus] :> -BracketForm[-p] /; MinusInFrontQ[p] /.
    Times -> TimesS /.
    ZPlusB :> TeXZPlusB //.
    { sum[a___, p_Plus] :> SequenceForm[a, TeX["\\left("], p, TeX["\\right)"]],
      sum[a__] :> SequenceForm[a] };
  Global`CV[n] = cr = cr /.
    {p_Plus :> p, -x_ :> SequenceForm["-", x],
      i_Integer?Negative x_ :> SequenceForm["-", -i x]} /.
    CoupVec -> TeXCoupVec /.
    BracketForm -> HoldForm /.
    AAA[x_] :> x //.
    { SequenceForm[s_SequenceForm] :> s,
      SequenceForm[x_] :> x,
      HoldForm[h_HoldForm] :> h };
  If[ MemberQ[Global`$Debug, n], Interrupt[] ];
  class[c] = {class[c], "$",
    ToString[TeX["\\Coup{" <> ToString[n] <> "}"]@@ cl == cr, TeXForm],
    "$\n\n\\bigskip\n\n"}
]


Plural[p_Plus] := Plural/@ List@@ p

Plural[_[s_String]] := {" -- ", s}

Plural[n_ _[s_]] := {" -- ", ToString[n], " ", s} /;
  StringTake[s, -1] === "s"

Plural[n_ _[s_]] := {" -- ", ToString[n], " ", s, "s"}


AddCoup[lhs_ == rhs:{__List}, {n_}] :=
Block[ {cv = Transpose[rhs], name, h1, h2},
  If[ cto >= 0 && cto < Length[cv] && !MatchQ[cv = cv[[cto + 1]], {(0)...}],
    name = Class/@ lhs;
    h1 = ToString[Head[#]]&/@ List@@ name;
    h2 = Rest[Flatten[Plural[Plus@@ name]]];
    ToTeX[{"\\Class{", h1, "}{", h2, "}\n"}, lhs, cv, n] ]
]

AddCoup[c_, _] := (Message[WriteTeXFile::badcoup, c]; {})


WriteTeXFile::badcoup = "Warning: `` is not recognized as a coupling."

Options[WriteTeXFile] = {
  ModelEdit :> Null,
  TeXFile -> Automatic,
  PreFunction -> Identity,
  MaxLeaf -> 60,
  CTOrder -> 0 }

WriteTeXFile[model_, opt___?OptionQ] :=
Block[ {texfile, maxleaf, cto,
mod, FieldRules, IndexRules, class, couplings, hh},

  {texfile, prefunc, maxleaf, cto} = {TeXFile, PreFunction, MaxLeaf, CTOrder} /.
    {opt} /. Options[WriteTeXFile];

  Check[ mod = LoadModel[model], Abort[] ];
  ModelEdit /. {opt} /. Options[WriteTeXFile];

  FieldRules = MakeFieldRules[M$ClassesDescription];
  _IndexRules = {};

  _class = {};
  MapIndexed[AddCoup, M$CouplingMatrices];
  _class =.;

  couplings = StringReplace[
    StringJoin[{#1[[1, 1]], #2}&@@@ DownValues[class]],
    {"\n \n" -> "\n", "{}^" -> "^"} ];

  If[ texfile === Automatic, texfile = mod <> ".tex" ];
  If[ cto > 0,
    mod = mod <> " (" <> ToString[cto] <> "-loop counter terms)"];

  hh = OpenWrite[texfile];
  WriteString[hh, StringReplace[template,
    {"COUPLINGS" -> couplings, "MODEL" -> ToString[mod]}]];
  Close[hh]
]

End[]

EndPackage[]


(* here come the model-dependent things *)

Class[_. _S] = S["Scalar"];
Class[_. _F] = S["Fermion"];
Class[_. _SV] = SV["Scalar--Vector"];
Class[_. _V] = V["Gauge Boson"];
Class[_. _U] = U["Ghost"]

(* we want i and coupling constants to print in front,
   thus the silly name "AAA" *)

AAA/: AAA[x_]^n_ := AAA[x^n]

ConjSym[_F] = ConjSym[_U] = Null

_ConjSym = TeX["-"]


SinSym = TeX["s"];
CosSym = TeX["c"];
TanSym = TeX["t"];
BosonMassSym = TeX["M"];
FermionMassSym = TeX["m"];
USym = TeX["U"];
VSym = TeX["V"];
WSym = TeX["W"];
ZSym = TeX["Z"];

SymRules = {
  $HKSign -> 1 (* "(-)" *),
  Complex[re_, im_] :> re + im AAA["i"],
  EL :> AAA[Sym[TeX["e"]]],
  GS :> AAA[Sym[TeX["g"], "s"]],
  SW :> Sym[SinSym, "W"],
  CW :> Sym[CosSym, "W"],
  MW :> Sym[BosonMassSym, "W"],
  MZ :> Sym[BosonMassSym, "Z"],
  MH :> Sym[BosonMassSym, "H"],
  MLE[j_] :> Sym[FermionMassSym, F[2, {j}]],
  MQU[j_] :> Sym[FermionMassSym, F[3, {j}]],
  MQD[j_] :> Sym[FermionMassSym, F[4, {j}]],
  Mass[f_] :> Sym[FermionMassSym, f],
  CKM[g__] :> Sym["CKM", {g}],
  SUNTSum[c1_, c2_, c3_, c4_] :> Sym[{SUNT[x, c1, c2], SUNT[x, c3, c4]}],
  SUNT[g_, c1_, c2_] :> Sym[TeX["T"], {c1, c2}, g],
  SUNT[g__, c1_, c2_] :> Sym[{"(", Sym[TeX["T"], Null, #]&/@ {g}, ")"}, {c1, c2}],
  SUNF[g1_, g2_, g3_, g4_] :> Sym[{SUNF[g1, g2, "x"], SUNF["x", g3, g4]}],
  SUNF[g1_, g2_, g3_] :> Sym[TeX["f"], Null, {g1, g2, g3}],
  d_IndexDelta :> 1 /; !FreeQ[d, "c1"|"c2"|"c3"|"c4"],
  IndexDelta[n_Integer, i_] :> Sym["\[Delta]", {i, n}],
  IndexDelta[i__] :> Sym["\[Delta]", {i}],
  GaugeXi[v_] :> Sym["\[Xi]", v],
  dZe1 :> Sym[delta["Z"], "e"],
  dMHsq1 :> Sym[delta[BosonMassSym], "H", "2"],
  dMWsq1 :> Sym[delta[BosonMassSym], "W", "2"],
  dMZsq1 :> Sym[delta[BosonMassSym], "Z", "2"],
  dMf1[t_, j_] :> Sym[delta[FermionMassSym], j, F[t, {g}]],
  dMf1[t_, j_] :> Sym[delta[FermionMassSym], j, F[t, {g}]],
  dSW1 :> Sym[delta[SinSym], "W"],
  dCW1 :> Sym[delta[CosSym], "W"],
  dZH1 :> Sym[delta[ZSym], "H"],
  dZW1 :> Sym[delta[ZSym], "W"],
  dZbarW1 :> Sym[delta[OverBar[ZSym]], "W"],
  dZAA1 :> Sym[delta[ZSym], "\[Gamma]\[Gamma]"],
  dZZA1 :> Sym[delta[ZSym], "Z\[Gamma]"],
  dZAZ1 :> Sym[delta[ZSym], "\[Gamma]Z"],
  dZZZ1 :> Sym[delta[ZSym], "ZZ"],
  dUW1 :> Sym[delta[USym], "W"],
  dUAA1 :> Sym[delta[USym], "\[Gamma]\[Gamma]"],
  dUZA1 :> Sym[delta[USym], "Z\[Gamma]"],
  dUAZ1 :> Sym[delta[USym], "\[Gamma]Z"],
  dUZZ1 :> Sym[delta[USym], "ZZ"],
  dZG01 :> Sym[delta[ZSym], Sym["G", Null, "0"]],
  dZGp1 :> Sym[delta[ZSym], "G"],
  dZfL1[t_, j1_, j2_] :> Sym[delta[ZSym], {j1, j2}, {F[t], "L"}],
  dZbarfL1[t_, j1_, j2_] :> Sym[delta[OverBar[ZSym]], {j1, j2}, {F[t], "L"}],
  dZfR1[t_, j1_, j2_] :> Sym[delta[ZSym], {j1, j2}, {F[t], "R"}],
  dZbarfR1[t_, j1_, j2_] :> Sym[delta[OverBar[ZSym]], {j1, j2}, {F[t], "R"}],
  dCKM1[j1_, j2_] :> Sym[delta["CKM"], {j1, j2}],
  dTH1 :> Sym[delta["T"], "H"]
}

ModelSymRules = {}


WidthRules = {
  _Mass -> "m"[1],
  h_[__][i__] :> h[i, 1],  (* upper indices of e.g. USf don't count *)
  (d:dZfL1|dZfR1)[t_, j1_, j2_] :> d[j1, j2],
  -1 -> +1,  (* a - b counts same as a + b *)
  Conjugate -> Identity,  (* USf^* counts as USf *)
  Power -> (If[IntegerQ[#2], #1, #1^#2]&)  (* M_W^2 counts as M_W *)
}

ModelWidthRules = {}

Null

