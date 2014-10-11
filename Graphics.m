 
(* :Title: Graphics *)

(* :Authors: Hagen Eck, Sepp Kueblbeck *)

(* :Summary: Graphics Primitives for HighEnergyPhysics *)

(* :Context: HighEnergyPhysics`Graphics` *)

(* :Package Version 1.0 *)

(* :Mathematica Version 2.0 *)

(* :Remark: 
	The problem of these graphics routines is that they are
	considerably slow. The time consuming part is the production
	of graphics primitives for sine-waves and cycles (in the 
	function PropagatorPrimitives for Head===Special). If there
	is any good solution we should adapt it.
*)

(* :Contents:
	Part1: utilities 
	Part2: ComposedChar, SymbolChar
	Part3: geometry
	Part4: graphics primitives
	Part5: labels, arrows, endpoints
	Part6: PropagatorGraphics
	(search for Part# to find beginning of a section)
*)

(* :Hint:
	Not all of the settings we use in drawing diagrams can be changed
	by the user via Options (e.g. the default spacing between a pro-
	pagator line and its label). To change them just search for the
	string DEFAULT in this file.
*)

BeginPackage["HighEnergyPhysics`Graphics`"];

(*
  The basic function for the construction of graphics primitives is
  PropagatorGraphics. Its arguments are the coordinates of the endpoints.
  The appearance of the propagator is determined by various options.
*)
PropagatorGraphics::usage =
"PropagatorGraphics[{x1, y1}, {x2, y2}] returns a Graphics object with graphic 
primitives for a propagator joining the points {x1,y1} and {x2,y2}.
PropagatorGraphics[{x1,y1}, {x2,y2}, f] applies the Appearance-function on
the Symbol f to determine the PropagatorGraphics-Options.";

PropagatorLabel::usage =
"PropagatorLabel is an option for PropagatorGraphics that determines the
label of the propagator written near the center of the line.";

PropagatorType::usage =
"PropagatorType is an option for PropagatorGraphics. Possible values are
Straight (default), Dashing[n,m], Sine and Cycles. Also possible is a list
combining two of these types (for mixing propagators).";

PropagatorArrow::usage =
"PropagatorArrow is an option for PropagatorGraphics. Possible values are
None, Forward and Backward.";

Propagator3rdPoint::usage =
"Propagator3rdPoint is an option for PropagatorGraphics. The propagator joins
the two endpoints via the given point."

PropagatorAmplitude::usage =
"PropagatorAmplitude is an option for PropagatorGraphics that determines 
the size of the amplitudes of Propagators with PropagatorType->Sine or
PropagatorType->Cycles.";

PropagatorArrowLength::usage =
"PropagatorArrowLength is an option for PropagatorGraphics.";

PropagatorEndpoints::usage =
"PropagatorEndpoints is an option for PropagatorGraphics. Possible values are 
None, {n}, or {n,m} where n and m can be one of None, Vertex, CounterTerm
or a Graphics-object (Head \"Graphics\").";

PropagatorHeight::usage =
"PropagatorHeight is an option for PropagatorGraphics that determines the
curvature of the Propagator.";

PropagatorLabelPosition::usage =
"PropagatorLabelPosition is an option for PropagatorGraphics.";

PropagatorDLabelPosition::usage =
"PropagatorDLabelPosition is an option for PropagatorGraphics.";

PropagatorPeriods::usage =
"PropagatorPeriods is an option for PropagatorGraphics that determines 
the number of periods (per length of line) of Propagators with 
PropagatorType->Sine or PropagatorType->Cycles.";

PropagatorPhase::usage =
"PropagatorPhase is an option for PropagatorGraphics that determines the
phase shift for propagators with PropagatorType->Cycles.";

PropagatorScaling::usage = 
"PropagatorScaling is an option for PropagatorGraphics. Its setting should 
be the overall size of the graph that is to be drawn. Usually it is determined
automatically from TopologyGraphics."

PropagatorThickness::usage =
"PropagatorThickness is an option for PropagatorGraphics.";

(*
  Possible values of PropagatorGraphics-Options:
*)

Backward::usage =
"Backward is a possible value of the Option PropagatorArrow.";

Cycles::usage =
"Cycles is a possible value of the Option PropagatorType.";

Forward::usage =
"Forward is a possible value of the Option PropagatorArrow.";

Sine::usage =
"Sine is a possible value of the Option PropagatorType.";

Straight::usage =
"Straight is a possible value of the Option PropagatorType.";

$Default::usage =
"$Default is a possible value of various Options.";

Size::usage =
"Size is a symbol of HighEnergyPhysics`Graphics`.";

CounterTerm::usage =
"CounterTerm is a possible value of the option PropagatorEndpoints.";

(* The function Appearance collects the PropagatorGraphics-Options for a 
   symbol f (usually a field) in a list.
*)
Appearance::usage =
"Appearance[ f ] gives the Graphics directives for Symbol f."

(* ComposedChar: print characters with sub- and superscripts.
*)
ComposedChar::usage =
"ComposedChar[list, coords] is a Graphics primitive of 
HighEnergyPhysics`Graphics` to print indexed characters. list can contain up 
to 4 expressions that correspond to {text, subscript, superscript, tilde}."

ComposedCharOffset::usage =
"ComposedCharOffset is an Option of ComposedChar."

MultiScriptOffset::usage =
"MultiScriptOffset is an Option of ComposedChar."

(* MakeLabel: graphics primitives for PropagatorGraphics-Labels
*)
MakeLabel::usage =
"MakeLabel[ g, l, p ] constructs graphics primitives for labelling propagators.
Its arguments are g: {pM, pC, radius, angle} (propagator geometry), l: the label
and p: the position (can both be ComposedChar)."

(* Print greek charakters:
*)
SymbolChar::usage =
"SymbolChar[s] is a notation for a symbol of the Postscript Symbol-font
named s. The mapping of characters to names is defined in the file
HighEnergyPhysics/Graphics/STable.m."

(* Miscellaneous stuff:
*)
$ScreenTextFont::usage =
"$ScreenTextFont is a symbol of HighEnergyPhysics`Graphics`."

$ScreenSymbolFont::usage =
"$ScreenSymbolFont is a symbol of HighEnergyPhysics`Graphics`."

$PrinterTextFont::usage =
"$PrinterTextFont is a symbol of HighEnergyPhysics`Graphics`."

$PrinterSymbolFont::usage =
"$PrinterSymbolFont is a symbol of HighEnergyPhysics`Graphics`."

$FSize::usage =
"$FSize is a Symbol of HighEnergyPhysics`Graphics`."

$FontSize::usage =
"$FontSize is a symbol of HighEnergyPhysics`Graphics`."

$SmallFontSize::usage =
"$SmallFontSize is a symbol of HighEnergyPhysics`Graphics`."

$TextFont::usage =
"$TextFont is a Symbol of HighEnergyPhysics`Graphics`. For displaying an
object containing this symbol it has to be replaced by either a well defined 
font (e.g. \"Courier\") or by the Symbol None.";

$SymbolFont::usage =
"$SymbolFont is a Symbol of HighEnergyPhysics`Graphics`. For displaying an
object containing this symbol it has to be replaced by either a well defined 
font (e.g. \"Symbol\") or by the Symbol None.";

$SFont::usage =
"$SFont is a Symbol of HighEnergyPhysics`Graphics`. In FontForm-specifications
it serves as a remainder that a symbol is to be printed in $SymbolFont."

(* if FeynArts calls this package, then Global`$FeynArts=True. In this case 
   we have to assure that the topological FeynArts-symbols are in the proper
   context. Otherwise, we read in the usages from Graphics/Symbols.m thus
   putting the symbols into the context HighEnergyPhysics`Graphics`.
*)
If[ TrueQ[ Global`$FeynArts ],
   TopologyList	= HighEnergyPhysics`FeynArts`TopologyList;
   Topology	= HighEnergyPhysics`FeynArts`Topology;
   Propagator	= HighEnergyPhysics`FeynArts`Propagator;
   Vertex    	= HighEnergyPhysics`FeynArts`Vertex;
   Incoming	= HighEnergyPhysics`FeynArts`Incoming;
   Outgoing	= HighEnergyPhysics`FeynArts`Outgoing;
   External	= HighEnergyPhysics`FeynArts`External;
   Internal	= HighEnergyPhysics`FeynArts`Internal;
   Loop		= HighEnergyPhysics`FeynArts`Loop,
   (* else *)
   GetContext["HighEnergyPhysics`Graphics`Symbols`"]
  ]

Begin["`Private`"]

(* 
   In the definition of the `Graphics-Constants' the font-specification
   "None" is only useful for Greek letters i.e. a *SymbolFont.
*)
(* DEFAULT *)
$ScreenTextFont = "Courier";
$ScreenSymbolFont = None;
$PrinterTextFont = "Helvetica";
$PrinterSymbolFont = "Symbol";
$FontSize = 10;
$SmallFontSize = 8;

(* here comes the list of HighEnergyPhysics`Graphics` - primitives together
   with their replacement rules in the functions Show and Display:
   (the symbol $FSize stands for a fontsize that isn't specified yet)
*)
HEPPrimitives = 
 { (* Symbol *)		(* Show *)		(* Display *)
   SymbolChar   :> { 	SpecifySymbolChar, 	SpecifySymbolChar 	},
   ComposedChar :> { 	UncomposeChar, 		UncomposeChar		},
   $TextFont    :> { 	$ScreenTextFont, 	$PrinterTextFont	},
   $SymbolFont  :> { 	$ScreenSymbolFont, 	$PrinterSymbolFont	},
   $FSize       :> { 	$FontSize, 		$FontSize		}
 };

	(** :Part1: utilities **)

(* Due to our lack of creativity we start this part with a nice example
   of plagiarism:
*)
FilterOptions[ command_Symbol, opts___ ] :=
 Block[ {keywords = First /@ Options[command]},
        Sequence @@ Select[ {opts}, MemberQ[keywords, First[#]]& ]
 ];	(* stolen from R.Maeder *)
 
(* Return a SetOptions-output (list of current options) without setting 
   options:
*)
ActualOptions[ command_Symbol, opts___ ] :=
 Block[ {commandoptions = { FilterOptions[command, opts] },
         keywords },
        keywords = First /@ commandoptions;
        Union[ commandoptions,
	       Select[ Options[command], !MemberQ[keywords, First[#]]& ]]
 ];

(* Some shorthand notations:
*)
nPi = N[Pi,10];				(* a number *)
numberQ[ x_ ] := NumberQ[N[x]];		(* a query *)
point = { _?numberQ, _?numberQ }; 	(* a pattern *)

(* Resolving HighEnergyPhysics`Graphics`-symbols and throw away superfluous 
   SymbolFont-heads.
*)
Unprotect[Show, Display];

Show[ x__ ] := Show@@ (
  {x} //. (#[[1]]&/@(Thread/@HEPPrimitives)) /. {$SFont->Identity}
     ) /; Or@@ ( Not[FreeQ[{x},#]]& /@ (#[[1]]&/@HEPPrimitives) );

Display[ channel_String, x__ ] := (Display[channel,#]&)@@ (
  {x} //. (#[[2]]&/@(Thread/@HEPPrimitives)) /. {$SFont->Identity}
     ) /; Or@@ ( Not[FreeQ[{x},#]]& /@ (#[[1]]&/@HEPPrimitives) );

Protect[Show, Display];

	(** :Part2: SymbolChar, ComposedChar **)

(* SymbolChar: define strings as equivalent to Postscript symbol font
   characters.
*)
GetContext[ "HighEnergyPhysics`Graphics`STable`"];

(* Prevent SymbolChar from being ToString-ed:
*)
ToString[ SymbolChar[ any_ ] ] ^:= SymbolChar[any];

(* Specify symbols:
   map definition of SpecifySymbolChar and SubstitSymbolChar on
   STable.
*)
( Set[ SpecifySymbolChar[#[[1]]], 
       FontForm[ $SFont[#[[2]]], {$SymbolFont, $FSize} ] ];
  Set[ SubstitSymbolChar[#[[2]]], #[[3]] ]
)& /@ STable;

(* additional FontForm-definition for font===None:
*)
Unprotect[FontForm];
FontForm[ $SFont[s_String], {None, size_} ] :=
  FontForm[ SubstitSymbolChar[s], {$DefaultFont[[1]], size} ];
Protect[FontForm];

(* ComposedChar treatment:
*)
(* DEFAULT *)
Options[ ComposedChar ] =
{
   ComposedCharOffset :> { {   0, -.03}*$FSize, 	(* subscript 	*)
                           {   0,  .02}*$FSize,	 	(* superscript  *)
                           {   0,  .04}*$FSize },	(* tilde	*)
   MultiScriptOffset :> .08*$FSize
};

ComposedChar::toomuch =
"Too many arguments (max. 4 are allowed).";
ComposedChar::badarg =
"Arguments `1` are not of the form [ List, List, Rule(s) ].";
ComposedChar::badpos =
"First position argument can not be Null.";

TreatCompChar[ x___ ] := ( Message[ComposedChar::badarg, x]; {} );
TreatCompChar[ l_List, {x_?numberQ, y_?numberQ}, o___Rule ] :=
  TreatCompChar[ l, {{x,y}}, o ];
TreatCompChar[ s_List, l_List, o___Rule ] :=
Block[ { opt = ActualOptions[ ComposedChar, o ], ls = s, ll = l, optll,
	 newtext, backtext={}, offs },
     optll = (ComposedCharOffset/.opt)/.$FSize->$FontSize;
     If[ Length[optll] < 3,
	 optll = Join[ optll, Table[Null,{ 3 - Length[optll]}] ] ];
     If[ Length[ls] > 4, 
	 Message[ ComposedChar::toomuch ]; Return[{}]  ];
     If[ Length[ls] < Length[ll], ll = Take[ ll, Length[ls] ] ];
     If[ Length[ll] < Length[ls], 
	  ll = Join[ ll, Table[Null,{Length[ls]-Length[ll]} ] ] ];
     Do[ If[ (i===1)&&(ll[[1]]===Null),
	     Message[ ComposedChar::badpos ]; Return[{}] 
	   ];
	 If[ (i===2)||(i==3), offs={-1,0}, offs={1,0} ];
	 If[ ll[[i]]===Null,
	     ll[[i]] = ll[[1]] + (optll[[i-1]]/.Null->(
			  ComposedCharOffset/.Options[ComposedChar])[[i-1]]) ,
	     ll[[i]] = ll[[1]] + If[i===1,0,ll[[i]]] 
	   ];
	 newtext = { ls[[i]]/.Null->"", ll[[i]]} ;
	 If[ Head[newtext[[1]]] === List,
	     newtext[[2]] = 
	      Table[ newtext[[2]]+{(i-1)*
		     ((MultiScriptOffset/.opt)/.$FSize->$FontSize), 0}, 
		    {i,Length[newtext[[1]]]}];
	     newtext = Transpose[newtext],
	     newtext = {newtext}
	   ];
	 newtext = SpecifyFont /@ newtext;
	 newtext = newtext /. $FSize:>If[i===1,$FontSize,$SmallFontSize];
	 backtext = Join[ backtext, Append[#,offs]& /@ newtext ],
	 {i, 1, Length[ls]}
	];
      Return[ backtext ]
     ];

SpecifyFont[ { text_, coord_ } ] :=
     { text /. s_String :> FontForm[s,{$TextFont,$FSize}] //. 
		$SFont[FontForm[i_,_]] :> $SFont[i]  ,
       coord };

(* Uncompose ComposedChar:
*)
UncomposeChar[ char_List, pos_List, opt___Rule ] := 
  (Text@@#)& /@ TreatCompChar[char, pos, opt ] ;

	(** :Part3: geometry **)

(* orientation of a line in the range 0..2Pi : 
*)
Orientation[{xa_?numberQ, ya_?numberQ}, {xb_?numberQ, yb_?numberQ}] :=
  FixRange[ N[ Sign[ yb-ya ] Pi/2 , 10 ] ]/; Abs[xa-xb]<10^-5 ;

Orientation[{xa_?numberQ, ya_?numberQ}, {xb_?numberQ, yb_?numberQ}] :=
  FixRange[ N[ (Pi/2 - Sign[xb-xa] Pi/2), 10 ] ] /; Abs[ya-yb]<10^-5 ;

Orientation[{xa_?numberQ, ya_?numberQ}, {xb_?numberQ, yb_?numberQ}] :=
  FixRange[ N[ ArcTan[ (yb-ya)/(xb-xa) ]
             + (1-Sign[xb-xa]) Sign[yb-ya] Pi/2 , 10 ] ];

FixRange[ x_ ] := FixRange[ x - 2nPi ] /; x >= 2 nPi ;
FixRange[ x_ ] := FixRange[ x + 2nPi ] /; x < 0 ;
FixRange[ x_ ] := x;

(* Polar Coordinates of a vector defined by two points: 
*)
PolarR[ {xa_?numberQ, ya_?numberQ}, {xb_?numberQ, yb_?numberQ} ] :=
    N[ Sqrt[ (xa-xb)^2 + (ya-yb)^2 ], 10 ];

PolarPhi[ {xa_?numberQ, ya_?numberQ}, {xb_?numberQ, yb_?numberQ} ] :=
   0 /; {xa, ya}==={xb, yb} ;

PolarPhi[ {xa_?numberQ, ya_?numberQ}, {xb_?numberQ, yb_?numberQ} ] :=
   Orientation[ {xa, ya}, {xb, yb} ];

(* (smaller) angle between two vectors:
*)
Angle[ a:point, b:point ] := 
 ArcCos[ (Plus@@ (a b))/Sqrt[Plus@@(a^2) Plus@@(b^2)] ];

(* All propagators are represented as a part of a circle. The task of our 
   basic geometry function is to find {pM, pC, radius, angle} where the angle 
   can be < 0 to keep track of the propagator direction (pM:center, 
   pC:propagatorcenter).
*)
PropagatorGeometry[ ___ ] := 
 ( Print["error: PropagatorGeometry (bad arguments)"]; Return[] );

PropagatorGeometry[ pA:point, pB:point, and:(_?numberQ|point|None) ] :=
 If[ N[pA] === N[pB],
     tadpole[ pA, and ],
     propagator[ pA, pB, PolarR[pA, pB], PolarPhi[pA, pB], and ]
   ];
	    
tadpole[ P_, None ] := tadpole[ P, 3 ];

tadpole[ P_, height_?numberQ ] :=
 { P + {0, height/2}, P + {0, height}, height/2, 2nPi };

tadpole[ P_, third:point ] :=
 { (P + third)/2, third, PolarR[P, third]/2, 2nPi };

propagator[ pA_, pB_, rAB_, phiAB_, None ] :=
 Block[{ phiMC = phiAB-nPi/2., rad = 100*rAB, pC = (pA+pB)/2., alpha },
       alpha = N[ 2*ArcSin[ rAB/2/rad ] ];
       Return[{pC - {Cos[phiMC],Sin[phiMC]}*rad, pC, rad, alpha}]
      ];

propagator[ pA_, pB_, rAB_, phiAB_, height_?numberQ ] :=
 propagator[ pA, pB, rAB, phiAB,
	     N[(pA+pB)/2 + height {Cos[phiAB+nPi/2], Sin[phiAB+nPi/2]} ]
	   ];

propagator[ pA_, pB_, rAB_, phiAB_, third:point ] :=
 Block[ { b = pB - pA, c = third - pA, 
	  a1, a2, b1, b2, c1, c2,
	  pM, pC, r, angle},
	a1 = c[[1]];                  a2 = b[[1]];
	b1 = c[[2]];                  b2 = b[[2]];
	c1 = -1/2(c[[1]]^2+c[[2]]^2); c2 = -1/2(b[[1]]^2+b[[2]]^2);
	pM = { Det[{{b1,c1},{b2,c2}}]/Det[{{a1,b1},{a2,b2}}] + pA[[1]],
	       Det[{{c1,a1},{c2,a2}}]/Det[{{a1,b1},{a2,b2}}] + pA[[2]] };
	r = PolarR[ pA, pM ];
	angle =  ( 2 nPi - 2 Angle[ pB-third, pA-third ] ) *
		Sign[ c[[1]] b[[2]] - c[[2]] b[[1]] ] ;
	pC = pM + r * { Cos[ PolarPhi[pM,pA] + angle/2 ],
			Sin[ PolarPhi[pM,pA] + angle/2 ] };
	Return[ N/@ {pM, pC, r, angle} ];
 ];

	(** :Part4: graphics primitives **)

(* DEFAULT *)
(* Special: Special[ amplitude, phase, periods, plotpoints ] 
*)
primitivetype[ _ ] = $Unknown;
primitivetype[ Straight ] = Circle[];
primitivetype[ Dashing[n_,m_] ] = Circle[Dashing[{n, m}]];
primitivetype[ Dashing[{n_,m_}] ] = Circle[Dashing[{n, m}]];
primitivetype[ Sine ] = Special[.3, 0., 50, 10];
primitivetype[ Cycles ] = Special[.5 , .2, 80, 10];

(* The PropagatorPrimitives we create here construct the propagator as
   part of a circle. For PropagatorTypes->Sine we include a variation of
   the radius of the circle and for PropagatorType->Cycles an additional
   variation of the polar angle (PhaseShift). Straight lines are part of
   a circle with huge radius.
*)
(* there might be two types of lines:
*)
PropagatorPrimitives[ {typeA_, typeB_}, geom_List, scale_, opt___Rule ] :=
{
 PropagatorPrimitives[ typeA,  HalfGeometry[geom, -1], scale, 
  Sequence@@( {opt}/.Rule[PropagatorArrow,_]->Rule[PropagatorArrow,None] ) ],
 PropagatorPrimitives[ typeB,  HalfGeometry[geom, +1], scale, 
  Sequence@@( {opt}/.Rule[PropagatorArrow,_]->Rule[PropagatorArrow,None] ) ]
};
HalfGeometry[ { {pMx_, pMy_}, {pCx_, pCy_}, r_, a_ }, sign_ ] :=
Block[ { phi = PolarPhi[ {pMx, pMy}, {pCx, pCy} ] },
	{ {pMx, pMy},
	  {pMx, pMy} + r * { Cos[ phi + sign*a/4 ], Sin[ phi + sign*a/4 ] },
	  r,
	  a/2
	}
     ];

PropagatorPrimitives[ type_, geom_List, scale_, opt___Rule ] :=
 Block[ { ptype = primitivetype[type], phi, thickness,
	  amplitude, phase, periods, plotpoints, arrow, angles },
	If[ ptype === $Unknown, 
	   Message[PropagatorGraphics::unknown, type]; Return[$Aborted]];
(*
Print[" Center: ",N[geom[[1]],2],"\tPoint: ",N[geom[[2]],2]];
Print[" Radius: ",N[geom[[3]],2],"\tAngle: ",N[geom[[4]]/Pi*180,2]];
*)
	arrow = PropagatorArrow/.{opt};
	Which[ arrow === None, arrow = 0,
	       (arrow === Forward) || (arrow===Backward) , arrow = 1 ];
	(* damp more for type=Cycles *)
	If[ type === Cycles, arrow *= 3 ];
	phi = PolarPhi[geom[[1]],geom[[2]]];
	thickness = PropagatorThickness/.{opt};
	If[ thickness === $Default, thickness = Thickness[0.001],
				    thickness = Thickness[thickness] ];
	If[ Head[ ptype ] === Circle,
	   angles = { phi - Abs[geom[[4]]/2], phi + Abs[geom[[4]]/2]};
	   Return[ { thickness,
		     ptype/.Circle->Sequence,
	             FCircle[ geom[[1]], geom[[3]], angles ] }]
	  ];
	If[ Head[ ptype ] === Special,
	   amplitude = PropagatorAmplitude/.{opt}/.$Default->ptype[[1]];
	   amplitude *= Abs[scale/20];
	   phase = PropagatorPhase/.{opt}/.$Default->ptype[[2]];
	   periods = PropagatorPeriods/.{opt}/.$Default->ptype[[3]];
	   periods *= Abs[((geom[[4]] / (2 nPi)) geom[[3]] / scale)];
	   periods = Round[periods];
	   plotpoints = PlotPoints/.{opt}/.$Default->ptype[[4]];
	   Return[ 
	    Prepend[
	     SpecialPrimitives[amplitude,phase,periods,plotpoints,geom[[1]], 
			       phi,geom[[3]],geom[[4]],arrow ],
	     thickness ]  ]
	  ];
  ];

(* there is a bug in the Mathematica Circle-routine so we have to write
   our own :-|
*)
If[ $VersionNumber >= 2.1 ,

FCircle[ {x_, y_ }, r_, ang___List ] :=
Block[{angles},
      angles = Table[ ang[[1]] + (i/20)*(ang[[2]]-ang[[1]]), {i,0,20}];
      Return[ Line@ ( ({ x + r Cos[#], y + r Sin[#]})& /@ angles ) ]
      ],

(* for the older versions we use the built-in function 
*)
FCircle[ {x_, y_ }, r_, ang___List ] := Circle[{x,y}, r, ang ]

]; (* end_If (V2.1) *)

(* this procedure needs most of the computing time: for a "Circle"-type
   propagator the average time is 0.16s for a "Special"-type it is 1.89s
   with 20 PlotPoints and 1.06 with 10 PlotPoints
*)
SpecialPrimitives[ am_, ph_, pe_, pp_, M_, phi_, r_, al_, ar_ ] :=
 Block[ { nr, inc, amg, rad, phas = ph*Abs[al]/pe },
 	nr = Floor[ pp * pe ];
        inc = Array[((#-1)/nr*al/2.)&, nr+1 ];
        ang[1] = Array[(phi-inc[[#]]-(phas*
		      Sin[(#-1)/nr*pe*2*nPi]))&, nr+1 ];
        ang[2] = Array[(phi+inc[[#]]+(phas*
		      Sin[(#-1)/nr*pe*2*nPi]))&, nr+1 ];
        rad = Array[(r+am*
		      Cos[(#-1)/nr*pe*2*nPi])&, nr+1 ];
    	rad = Join[ Drop[rad, -Round[pp/3.] ],
	            SmoothRadius[ r, Take[ rad, -Round[pp/3.]],1]];
        If[ ar =!= 0,
    	   rad = Join[ Reverse[ SmoothRadius[ r,
	       	                Reverse[ Take[ rad, Round[ar*pp/3.]]],0]],
		       Drop[rad, Round[ar*pp/3.] ] ]
	  ];
        Return[ { Line[ Array[ M + rad[[#]]*
	                {Cos[ang[1][[#]]],Sin[ang[1][[#]]]}&, nr+1]],
  		  Line[ Array[ M + rad[[#]]*
	                {Cos[ang[2][[#]]],Sin[ang[2][[#]]]}&, nr+1]] }
	      ];
 ];

(* At the ends of the propagator and for propagators with arrows we damp
   the sine waves and the wiggles down to the circle radius.
*)
SmoothRadius[ rad_, rval_List, pow_Integer ]:=
 Block[ {n=Length[rval]},
       Table[ rval[[i]]+Sign[rad-rval[[i]]] * Abs[(rad-rval[[i]])(i/n)^pow], 
	     {i,n} ]
      ];

	(** :Part5: labels, arrows, endpoints **)

PropagatorAcessoires[pA:point, pB:point, geom_List, OP__Rule] :=
 Block[ { aces={}},
	If[ MatchQ[ PropagatorLabel/.{OP}, {_, _} ],
	   AppendTo[ aces,
		     MakeDLabel[HalfGeometry[geom, -1], HalfGeometry[geom, +1],
				PropagatorLabel/.{OP},
				PropagatorDLabelPosition/.{OP} ]],
	   AppendTo[ aces,
		     MakeLabel[geom,
			       PropagatorLabel/.{OP},
			       PropagatorLabelPosition/.{OP} ]]
	  ];
	AppendTo[ aces,
		  EndPoints[pA, pB, 
			    PropagatorEndpoints/.{OP},
			    PointSize/.{OP}           ]];
	Return[aces];
 ];

(* these are the graphics primitives for the vertices or counterterms:
*)
EndPoints[ A_, B_, None, size_] = {};
EndPoints[ A_, B_, type_, size_] := EndPoints[A, B, {type, type}, size];
EndPoints[ A_, B_, {type_}, size_] := EndPoints[A, B, {type, type}, size];
EndPoints[ A_, B_, {tA_, tB_}, size_ ] :=
  { EndPoints[A, tA, size], EndPoints[B, tB, size] }
EndPoints[ point_, type_, size_ ] :=
  Which[ type===None, {},
	 type===Vertex,
	  {PointSize[size],Point[point]},
	 type===CounterTerm, 
	  {Thickness[size/3.],
	   Line[ {point-15 size SL, point+15 size SL} ],
	   Line[ {point-15 size BS, point+15 size BS} ] },
	 MatchQ[ type, Graphics[_] ],
	  List@@(Release[type/.Point->point/.Size->size]),
	 True, Message[ PropagatorGraphics::noendp, type ]; {} ]

(* DEFAULT *)
BS = { -1.8, 1.8 };
SL = {  1.8, 1.8 };
(* counterterms of higher order:
*)
CounterTerm[1] := 
  Graphics[ {Thickness[ Size/3. ],
             Line[ { Hold[Point] - 15 Size* Hold[SL],
                     Hold[Point] + 15 Size* Hold[SL]  }],
             Line[ { Hold[Point] - 15 Size* Hold[BS],
                     Hold[Point] + 15 Size* Hold[BS]  }]
             } ];
CounterTerm[2] := 
  Graphics[ {Thickness[ Size/3. ],
             Line[ { Hold[Point] - 15 Size* Hold[SL],
                     Hold[Point] + 15 Size* Hold[SL]  }],
             Line[ { Hold[Point] - 15 Size* Hold[BS],
                     Hold[Point] + 15 Size* Hold[BS]  }],
             Circle[ Point, 60 Size ]
             } ];
CounterTerm[3] := 
  Graphics[ {Thickness[ Size/3. ],
             Line[ { Hold[Point] - 15 Size* Hold[SL],
                     Hold[Point] + 15 Size* Hold[SL]  }],
             Line[ { Hold[Point] - 15 Size* Hold[BS],
                     Hold[Point] + 15 Size* Hold[BS]  }],
             Circle[ Point, 60 Size ],
             Circle[ Point, 90 Size ]
             } ];


(* Construction of the label:
*)
(* DEFAULT *)
LabelDistance = 1.15;  (* from propagator center *)

(* the label for mixing-propagators:
*)
MakeDLabel[ geomA_, geomB_, {labelA_, labelB_}, {posA_, posB_} ] :=
  { MakeLabel[ geomA, labelA, posA ],
    MakeLabel[ geomB, labelB, posB ] };

(* and all the other possibilities (at least most of them):
*)
MakeLabel[ g_List, None, _ ] := {};
MakeLabel[ g_List, ComposedChar[ l_List ], Automatic ] :=
   ComposedChar[  l /. Null->{""}, 
		 { g[[2]] +
		  LabelDistance * { Cos[PolarPhi[g[[1]],g[[2]]]],
				    Sin[PolarPhi[g[[1]],g[[2]]]]} } ];
MakeLabel[ g_List, ComposedChar[ l_List, p:point ], Automatic ] :=
   ComposedChar[  l /. Null->{""}, {p} ];
MakeLabel[ g_List, ComposedChar[ l_List, {Null,p__} ], q:point ] :=
   ComposedChar[  l /. Null->{""}, {q,p} ];
MakeLabel[ g_List, ComposedChar[ l_List, {Null,p__} ], 
		   ComposedChar[ { q:point, ___}    ]  ] :=
   ComposedChar[  l /. Null->{""}, {q,p} ];
MakeLabel[ g_List, ComposedChar[ l_List, {Null,p__} ], Automatic ] :=
   ComposedChar[  l /. Null->{""}, 
                 {g[[2]] +
		  LabelDistance * { Cos[PolarPhi[g[[1]],g[[2]]]],
	          Sin[PolarPhi[g[[1]],g[[2]]]]} ,p } ];
MakeLabel[ g_List, ComposedChar[ l_List, p_List ], Automatic ] :=
   ComposedChar[  l /. Null->{""}, p ];
MakeLabel[ g_List, l_, Automatic ] :=
   ComposedChar[ { l }, 
	         {g[[2]] + 
		 LabelDistance * { Cos[PolarPhi[g[[1]],g[[2]]]],
		                   Sin[PolarPhi[g[[1]],g[[2]]]]} } ];
MakeLabel[ g_List, l:(_String|_SymbolChar), any_ ] :=
    MakeLabel[ g, ComposedChar[{l}], any ];
MakeLabel[ g_List, l_List, any_ ] :=
    MakeLabel[ g, ComposedChar[l], any ];
MakeLabel[ g_List, ComposedChar[ l_List ], p:point ] :=
    ComposedChar[  l /. Null->{""}, { p } ];
MakeLabel[ g_List, ComposedChar[ l_List ], ComposedChar[ p_List ] ] :=
    MakeLabel[ g, ComposedChar[l, p], Automatic ];
MakeLabel[ g_List, ComposedChar[ l_List, p:point ], _ ] :=
    ComposedChar[  l /. Null->{""}, {p} ];
MakeLabel[ g_List, ComposedChar[ l_List, p_List ], _ ] :=
    ComposedChar[  l /. Null->{""}, p ];

(* The arrow is just a polygon:
*)
(* DEFAULT *)
ArrowLength = 0.05;        (* length as fraction of Scale, contained in "sc" *)
ArrowShape = {nPi/5, 1.4}; (* angle of arrow and length of "wings" *)

Arrow[None, a:point, b:point, _List, _ ] := {};
Arrow[ Forward, a:point, b:point, geom_List, sc_ ] :=
  Arrow[ geom[[1]], geom[[2]], geom[[3]], sc/geom[[3]]*Sign[geom[[4]]] ];
Arrow[ Backward, a:point, b:point, geom_List, sc_ ] :=
  Arrow[ geom[[1]], geom[[2]], geom[[3]], -sc/geom[[3]]*Sign[geom[[4]]] ];
Arrow[ M_, C_, r_, al_ ] :=
  Arrow[ M + r { Cos[PolarPhi[M,C]+al/2], Sin[PolarPhi[M,C]+al/2]},
         M + r { Cos[PolarPhi[M,C]-al/2], Sin[PolarPhi[M,C]-al/2]} ];
Arrow[ P_, E_ ] :=
  Block[ { phiPE=PolarPhi[P,E], psi=ArrowShape[[1]]/2,
	   l=PolarR[P,E], x=ArrowShape[[2]] },
        Return[ { Polygon[ { P, P + l*x*{Cos[phiPE+psi],Sin[phiPE+psi]},
			     E, P + l*x*{Cos[phiPE-psi],Sin[phiPE-psi]} 
			   } ] } ];
       ];

	(** :Part6: PropagatorGraphics **)

Options[PropagatorGraphics] = 
       { 
	 PropagatorType -> Straight,

	 PointSize -> .015,
	 PropagatorThickness -> $Default,

	 Propagator3rdPoint -> None,
	 PropagatorHeight -> None,

	 PropagatorAmplitude -> $Default,
	 PropagatorPhase -> $Default,
	 PropagatorPeriods -> $Default,
	 PlotPoints -> $Default,

	 PropagatorArrow -> None,
	 PropagatorArrowLength -> Automatic,
	 PropagatorEndpoints -> None,
	 PropagatorLabel -> None,
	 PropagatorLabelPosition -> Automatic,
	 PropagatorDLabelPosition -> {Automatic, Automatic},

	 PropagatorScaling -> Automatic
       };

PropagatorGraphics::unknown =
" Baby, this is not a type I know of : `1`.";

PropagatorGraphics::undef =
" Oops! I don't know this Symbol: `1` (at least not in this place).";

PropagatorGraphics::noapp =
" No Appearance definition found for Symbol `1`.";

PropagatorGraphics::dbldef =
" Propagator3rdPoint and PropagatorHeight can not be specified simultaneously
(using Propagator3rdPoint).";

PropagatorGraphics::noendp =
" I can't handle endpoints of type \"`1`\" (use None).";

PropagatorGraphics[pA:point, pB:point, field_, op___Rule ] :=
  If[ ValueQ[ Appearance[field] ],
     PropagatorGraphics[pA, pB, Sequence@@ Appearance[field], op ],
     Message[ PropagatorGraphics::noapp, field ];
     PropagatorGraphics[ a, b, op ]
    ] /; Head[field] =!= Rule ;

PropagatorGraphics[pA:point, pB:point, op___Rule ] :=
 Block[ { OP = ActualOptions[PropagatorGraphics, op], 
	  h, t,
	  geom, scale, arrlen },
	{h, t} = {PropagatorHeight, Propagator3rdPoint}/.OP;
	If[ h===None, geom = PropagatorGeometry[pA, pB, t],
	    If[ t=!=None, Message[PropagatorGraphics::dbldef];
		geom = PropagatorGeometry[pA, pB, t] ,
		geom = PropagatorGeometry[pA, pB, h]
	      ]
	  ];
	If[ (scale = PropagatorScaling/.OP) === Automatic,
	   scale = PolarR[ pA, pB ] ];
	If[ (arrlen = PropagatorArrowLength/.OP) === Automatic,
	   arrlen = ArrowLength*scale ];
	lineprim = PropagatorPrimitives[ PropagatorType/.OP, 
			                 geom, scale, Sequence@@ OP ];
	AppendTo[lineprim, PropagatorAcessoires[pA, pB, geom, Sequence@@OP]];
	AppendTo[lineprim, Arrow[PropagatorArrow/.OP, pA, pB, geom, arrlen]];
	Return[ Graphics[{lineprim}] ]
 ];

End[]

EndPackage[] (* HighEnergyPhysics`Graphics` *)

If[ (!MemberQ[$CommandLine, "-noprompt"])&&(!TrueQ[Global`$FeynArts]) ,
    Print[" -- HEP Graphics loaded --"]
  ]

