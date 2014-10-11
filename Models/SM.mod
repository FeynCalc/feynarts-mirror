
(* :Title: SM.mod *)

(* :Authors: Hagen Eck, Sepp Kueblbeck *)

(* :Last Change: 27.03.95 *)

(* :17.10.95  Stefan Bauberger:
    Some corrections and addition of all one-loop counterterms
    according to A. Denner. The gauge-fixing terms are assumed not to
    be renormalized. In addtions to the convention of A. Denner
    field renormalization for the phi- and chi-fields is assumed.
    The counterterms which are connected with quark mixing are not yet
    well tested.
*)

(* :Summary: Model file for HighEnergyPhysics`FeynArts. 
	     model:
	     => SM - model <=
	     ----------------
    
    Reference: Ansgar Denner, "Techniques for the calculation of
               electroweak radiative corrections at one-loop level
               and results for W-physics at LEP200", Fortschr. d.
               Physik, 41 (1993) 4
*)

(* :Context: HighEnergyPhysics`FeynArts` *)

(* :Package Version 1.1 *)

(* :Mathematica Version 2.0 *)

(* :Requirements: FeynArts 2.0 *)

(*
    This file introduces the following symbols:

	EL:		charge of electron (Thomson-limes)
	CW,SW:		cosine and sine of Weinberg angle

	FQ:		quark mixing matrix (tagged by symbol "Matrix")

	MW, MZ, MH:	masses of W-, Z-, phys.Higgs bosons
	ME, MM, ML:	masses of leptons (e, mu, tau)
	MU, MC, MT:	masses of u-type quarks (up, charm, top)
	MD, MS, MB:	masses of d-type quarks (down, strange, bottom)
	MLE, MQU, MQD:	classes mass parameters (leptons, u-type, d-type)

*)

(* :Description: 
	This file contains the definition of a model for FeynArts.
	- All particles are arranged in classes. For single particle
	  model definitions each particle lives in its own class 
	  (this resembles the FeynArts 1.0 point of view).
	- For each class the common SelfConjugate behaviour and the
	  IndexRange MUST be present in the definitions.
	- IMPORTANT: The coupling matrices for fermionic couplings
          have to match the coupling definition in the generic file.
          Lorentz.gen: F-F-S, F-F-V have fermion number flow from
          right to left (this matches the FMRs of the reference).
*)

(* For the SM and related models we define some restrictions  and
   abbreviations in a file:
*)
<<"HighEnergyPhysics/Models/SMtype.abb";

IndexRange[ Index[ Generation ] ] = { 1, 2, 3 };
Appearance[ Index[ Generation, i_Integer ] ] := Alph[ i+8 ];

MaxGenerationIndex = 3;


(*
FermionChain[x1___,0,x2___] := 0;
FermionChain[x1___,1,x2___] := FermionChain[x1,x2];
FermionChain[x1_] := x1;
*)
dZfL1[1,{j1_Integer},{j2_Integer}] := If[j1==j2,dZfL1[1,j1],0];
dZfL1[2,{j1_Integer},{j2_Integer}] := If[j1==j2,dZfL1[2,j1],0];
dZfR1[1,{j1_Integer},{j2_Integer}] := If[j1==j2,dZfR1[1,j1],0];
dZfR1[2,{j1_Integer},{j2_Integer}] := If[j1==j2,dZfR1[2,j1],0];
dZfL1[i1_,{j1_Integer}] := dZfL1[i1,j1];
dZfR1[i1_,{j1_Integer}] := dZfR1[i1,j1];
dMf1[i1_,{j1_Integer}] := dMf1[i1,j1];


M$ClassesDescription =
{
  (* Leptons (neutrino): I_3 = +1/2, Q = 0 *)
 F[1]  == { SelfConjugate -> False, 
	      Indices -> {Index[Generation]},
	      Mass -> 0, 
	      PropagatorLabel -> ComposedChar[ {SymbolChar["nu"],
					        Index[Generation]} ], 
	      PropagatorType -> Straight, 
	      PropagatorArrow -> Forward }, 

  (* Leptons (electron): I_3 = -1/2, Q = -1 *)
 F[2]  == { SelfConjugate -> False, 
	      Indices -> {Index[Generation]},
	      Mass -> MLE,
	      PropagatorLabel -> ComposedChar[ {"e", Index[Generation] } ],
	      PropagatorType -> Straight, 
	      PropagatorArrow -> Forward }, 

  (* Quarks (u): I_3 = +1/2, Q = +2/3 *)
 F[3]  == { SelfConjugate -> False, 
	      Indices -> {Index[Generation]},
	      Mass -> MQU,
              MatrixTraceFactor -> 3,
	      PropagatorLabel -> ComposedChar[ {"u", Index[Generation] } ],
	      PropagatorType -> Straight, 
	      PropagatorArrow -> Forward }, 

  (* Quarks (d): I_3 = -1/2, Q = -1/3 *) 
 F[4]  == { SelfConjugate -> False, 
	      Indices -> {Index[Generation]},
	      Mass -> MQD,
              MatrixTraceFactor -> 3,
	      PropagatorLabel -> ComposedChar[ {"d", Index[Generation] } ],
	      PropagatorType -> Straight, 
	      PropagatorArrow -> Forward }, 

  (* Gaugebosons: Q = 0 *)
 V[1]  == { SelfConjugate -> True, 
	      Indices -> {},
	      Mass -> 0, 
	      PropagatorLabel -> SymbolChar["gamma"], 
	      PropagatorType -> Sine, 
	      PropagatorArrow -> None },
   
 V[2]  == { SelfConjugate -> True, 
	      Indices -> {},
	      Mass -> MZ, 
	      PropagatorLabel -> "Z", 
	      PropagatorType -> Sine, 
	      PropagatorArrow -> None },
   
  (* Gaugebosons: Q = -1 *)
 V[3]  == { SelfConjugate -> False, 
	      Indices -> {},
	      Mass -> MW, 
	      PropagatorLabel -> "W", 
	      PropagatorType -> Sine, 
	      PropagatorArrow -> Forward },

  (* mixing Higgs gauge bosons: Q = 0 *) 
 SV[2]  == { SelfConjugate -> False, 
	      Indices -> {},
	      Mass -> MZ, 
	      MixingPartners -> {S[2],V[2]},
	      PropagatorLabel -> {SymbolChar["chi"], "Z"}, 
	      PropagatorType -> { Dashing[0.01, 0.01], Sine }, 
	      PropagatorArrow -> None },

  (* mixing Higgs gauge bosons: charged *) 
 SV[3]  == { SelfConjugate -> False, 
	      Indices -> {},
	      Mass -> MW, 
	      MixingPartners -> {S[3],V[3]},
	      PropagatorLabel -> {SymbolChar["phi"], "W"}, 
	      PropagatorType -> { Dashing[0.01, 0.01], Sine }, 
	      PropagatorArrow -> Forward },

 VS[3]  == { SelfConjugate -> False, 
	      Indices -> {},
	      Mass -> MW, 
	      MixingPartners -> {V[3],S[3]},
	      PropagatorLabel -> {"W", SymbolChar["phi"]}, 
	      PropagatorType -> { Sine, Dashing[0.01, 0.01]}, 
	      PropagatorArrow -> Forward },

  (* physical Higgs: Q = 0 *) 
 S[1]  == { SelfConjugate -> True, 
	      Indices -> {},
	      Mass -> MH, 
	      PropagatorLabel -> "H", 
	      PropagatorType -> Dashing[0.01, 0.01], 
	      PropagatorArrow -> None },

  (* unphysical Higgs: neutral *) 
 S[2]  == { SelfConjugate -> True, 
	      Indices -> {},
	      Mass -> MZ, 
	      PropagatorLabel -> SymbolChar["chi"], 
	      PropagatorType -> Dashing[0.01, 0.01], 
	      PropagatorArrow -> None },

  (* unphysical Higgs: Q = -1 *)  
 S[3]  == { SelfConjugate -> False, 
	      Indices -> {},
	      Mass -> MW, 
	      PropagatorLabel -> SymbolChar["phi"], 
	      PropagatorType -> Dashing[0.01, 0.01], 
	      PropagatorArrow -> Forward },

  (* Ghosts: neutral *) 
 U[1]  == { SelfConjugate -> False, 
	      Indices -> {},
	      Mass -> 0, 
	      PropagatorLabel -> ComposedChar[{"u", ,SymbolChar["gamma"]}], 
	      PropagatorType -> Dashing[0.003,0.005], 
	      PropagatorArrow -> Forward },
 U[2]  == { SelfConjugate -> False, 
	      Indices -> {},
	      Mass -> MZ, 
	      PropagatorLabel -> ComposedChar[{"u", ,"Z"}], 
	      PropagatorType -> Dashing[0.003,0.005], 
	      PropagatorArrow -> Forward },
  (* Ghosts: charged *) 
 U[3]  == { SelfConjugate -> False, 
	      Indices -> {},
	      Mass -> MW, 
	      PropagatorLabel -> ComposedChar[{"u", ,"-"}], 
	      PropagatorType -> Dashing[0.003,0.005], 
	      PropagatorArrow -> Forward },
 U[4]  == { SelfConjugate -> False, 
	      Indices -> {},
	      Mass -> MW, 
	      PropagatorLabel -> ComposedChar[{"u", ,"+"}], 
	      PropagatorType -> Dashing[0.003,0.005], 
	      PropagatorArrow -> Forward }
};

TheMass[ F[2,{1}] ] = ME; TheMass[ F[2,{2}] ] = MM; TheMass[ F[2,{3}] ] = ML;
TheMass[ F[3,{1}] ] = MU; TheMass[ F[3,{2}] ] = MC; TheMass[ F[3,{3}] ] = MT;
TheMass[ F[4,{1}] ] = MD; TheMass[ F[4,{2}] ] = MS; TheMass[ F[4,{3}] ] = MB;
TheLabel[ F[1,{1}] ] = ComposedChar[{SymbolChar["nu"],"e"}]; 
TheLabel[ F[1,{2}] ] = ComposedChar[{SymbolChar["nu"],SymbolChar["mu"]}]; 
TheLabel[ F[1,{3}] ] = ComposedChar[{SymbolChar["nu"],SymbolChar["tau"]}]; 
TheLabel[ F[2,{1}] ] = "e"; 
TheLabel[ F[2,{2}] ] = SymbolChar["mu"]; 
TheLabel[ F[2,{3}] ] = SymbolChar["tau"];
TheLabel[ F[3,{1}] ] = "u"; 
TheLabel[ F[3,{2}] ] = "c";
TheLabel[ F[3,{3}] ] = "t";
TheLabel[ F[4,{1}] ] = "d"; 
TheLabel[ F[4,{2}] ] = "s";
TheLabel[ F[4,{3}] ] = "b";

M$CouplingMatrices =
{
(* V-V :     G(+) * { - g[mu,nu]*mom^2,
                      g[mu,nu],
                      - mom[mu]*mom[nu]  }
*)
  C[ -V[3],  V[3] ] ==
    { { 0, I * dZW1,
           I * dZW2     },
      { 0, I * (MW^2*dZW1+dMWsq1),
           I * (MW^2*dZW2+dMWsq2+dMWsq1*dZW1)   },
      { 0, - I * dZW1,
           - I * dZW2 } },

  C[ V[2],  V[2] ] ==
    { { 0, I * dZZZ1 },
      { 0, I * (MZ^2*dZZZ1+dMZsq1) },
      { 0, - I * dZZZ1 } },

  C[ V[1],  V[1] ] ==
    { { 0, I * dZAA1 },
      { 0, 0 },
      { 0, - I * dZAA1 } },

  C[ V[1],  V[2] ] ==
    { { 0, I * (dZAZ1+dZZA1)/2 },
      { 0, I * MZ^2*dZZA1/2 },
      { 0, - I * (dZAZ1+dZZA1)/2 } },

(* S-V :     G(+) * { mom1[mu],
                      mom2[mu] }
*)

  C[ S[3],  -V[3] ] ==
    { { 0, - (1/2)* I*MW*(dZW1+dZphi1+dMWsq1/MW^2)/2 },
      { 0, (1/2)* I*MW*(dZW1+dZphi1+dMWsq1/MW^2)/2 }},

  C[ -S[3],  V[3] ] ==
    { { 0, (1/2)* I*MW*(dZW1+dZphi+dMWsq1/MW^2)/2 },
      { 0, - (1/2)* I*MW*(dZW1+dZphi+dMWsq1/MW^2)/2 }},

  C[ S[2],  V[2] ] ==
    { { 0, (1/2)* MZ * (dZZZ1+dZchi1+dMZsq1/MZ^2)/2 },
      { 0, - (1/2)* MZ * (dZZZ1+dZchi1+dMZsq1/MZ^2)/2 }},

  C[ S[2],  V[1] ] ==
    { { 0, (1/2)* MZ * dZZA1/2 },
      { 0, - (1/2)* MZ * dZZA1/2 }},

(* S-S :     G(+) * { - mom^2, 1 }
*)

  C[ S[1],  S[1] ] ==
    { { 0, - I * dZH1 },
      { 0, - I * (dMHsq1 + MH^2 * dZH1 ) } },

  C[ S[2],  S[2] ] ==
    { { 0, - I * dZchi1 },
      { 0, I * EL/(2*MW*SW) dTad1} },

  C[ S[3], -S[3] ] ==
    { { 0, - I * dZphi1 },
      { 0, I * EL/(2*MW*SW) dTad1 } },

(* F-F :     G(+) * {  slash[mom1] omega[-],
                       slash[mom2] omega[+],
                       omega[-],
                       omega[+]             }
*)

  C[ F[3,{j1}], -F[3,{j2}] ] ==
    { { 0,  I * (1/2) * (dZfL1[3,{j1},{j2}]+Conjugate[dZfL1[3,{j1},{j2}]]) },
      { 0, -I * (1/2) * (dZfR1[3,{j1},{j2}]+Conjugate[dZfR1[3,{j1},{j2}]]) },
      { 0, -I * (1/2) * (  Mass[F[3,{j1}]]*dZfL1[3,{j1},{j2}]
                         + Mass[F[3,{j2}]]*Conjugate[dZfR1[3,{j1},{j2}]]
                         + 2* IndexDelta[j1,j2]
                            *dMf1[3,{j1}] ) },
      { 0, -I * (1/2) * (  Mass[F[3,{j1}]]*dZfR1[3,{j1},{j2}]
                         + Mass[F[3,{j2}]]*Conjugate[dZfL1[3,{j1},{j2}]]
                         + 2* IndexDelta[j1,j2]
                            *dMf1[3,{j1}] ) } },

  C[ F[4,{j1}], -F[4,{j2}] ] ==
    { { 0,  I * (1/2) * (dZfL1[4,{j1},{j2}]+Conjugate[dZfL1[4,{j1},{j2}]]) },
      { 0, -I * (1/2) * (dZfR1[4,{j1},{j2}]+Conjugate[dZfR1[4,{j1},{j2}]]) },
      { 0, -I * (1/2) * (  Mass[F[4,{j1}]]*dZfL1[4,{j1},{j2}]
                         + Mass[F[4,{j2}]]*Conjugate[dZfR1[4,{j1},{j2}]]
                         + 2* IndexDelta[j1,j2]
                            *dMf1[4,{j1}] ) },
      { 0, -I * (1/2) * (  Mass[F[4,{j1}]]*dZfR1[4,{j1},{j2}]
                         + Mass[F[4,{j2}]]*Conjugate[dZfL1[4,{j1},{j2}]]
                         + 2* IndexDelta[j1,j2]
                            *dMf1[4,{j1}] ) } },

  C[ F[1,{j1}], -F[1,{j2}] ] ==
    { { 0,  I * (1/2) * (dZfL1[1,{j1},{j2}]+Conjugate[dZfL1[1,{j1},{j2}]]) },
      { 0, -I * (1/2) * (dZfR1[1,{j1},{j2}]+Conjugate[dZfR1[1,{j1},{j2}]]) },
      { 0, 0 },
      { 0, 0 } },

  C[ F[2,{j1}], -F[2,{j2}] ] ==
    { { 0,  I * (1/2) * (dZfL1[2,{j1},{j2}]+Conjugate[dZfL1[2,{j1},{j2}]]) },
      { 0, -I * (1/2) * (dZfR1[2,{j1},{j2}]+Conjugate[dZfR1[2,{j1},{j2}]]) },
      { 0, -I * (1/2) * (  Mass[F[2,{j1}]]*dZfL1[2,{j1},{j2}]
                         + Mass[F[2,{j2}]]*Conjugate[dZfR1[2,{j1},{j2}]]
                         + 2* IndexDelta[j1,j2]
                            *dMf1[2,{j1}] ) },
      { 0, -I * (1/2) * (  Mass[F[2,{j1}]]*dZfR1[2,{j1},{j2}]
                         + Mass[F[2,{j2}]]*Conjugate[dZfL1[2,{j1},{j2}]]
                         + 2 * IndexDelta[j1,j2]
                             *dMf1[2,{j1}] ) } },


(* V-V-V-V:   G(+) * { g[mu1,mu2] g[mu3,mu4],
                       g[mu1,mu4] g[mu2,mu3],
                       g[mu1,mu3] g[mu2,mu4]  }
*) 

  C[ -V[3],  -V[3],  V[3],  V[3]] == 
   { { I EL^2 * (2/SW^2),
       I EL^2 * (2/SW^2) * ( 2*dZe1 - 2*dSW1/SW + 2*dZW1 ) }, 
     { I EL^2 * (-1/SW^2),
       I EL^2 * (-1/SW^2) * ( 2*dZe1 - 2*dSW1/SW + 2*dZW1 ) },
     { I EL^2 * (-1/SW^2), 
       I EL^2 * (-1/SW^2) * ( 2*dZe1 - 2*dSW1/SW + 2*dZW1 ) } },

  C[ -V[3],   V[3],  V[2],  V[2]] == 
   { { I EL^2 * (-2 CW^2/SW^2),
       I EL^2 * (-2 CW^2/SW^2) * ( 2*dZe1 - 2*dSW1/(SW*CW^2) + dZW1 + dZZZ1
                                  - dZAZ1*SW/CW )  }, 
     { I EL^2 * ( 1 CW^2/SW^2),
       I EL^2 * ( 1 CW^2/SW^2) * ( 2*dZe1 - 2*dSW1/(SW*CW^2) + dZW1 + dZZZ1
                                  - dZAZ1*SW/CW )  }, 
     { I EL^2 * ( 1 CW^2/SW^2), 
       I EL^2 * ( 1 CW^2/SW^2) * ( 2*dZe1 - 2*dSW1/(SW*CW^2) + dZW1 + dZZZ1
                                  - dZAZ1*SW/CW )  } },
 
  C[ -V[3],   V[3],  V[1],  V[2]] == 
   { { I EL^2 * ( 2 CW/SW),
       I EL^2 * ( 2 CW/SW) * ( 2*dZe1 - dSW1/(SW*CW^2) + dZW1 + dZZZ1/2
                              + dZAA1/2 - SW/CW * dZAZ1/2 
                              - CW/SW * dZZA1/2 ) },
     { I EL^2 * (-1 CW/SW), 
       I EL^2 * (-1 CW/SW) * ( 2*dZe1 - dSW1/(SW*CW^2) + dZW1 + dZZZ1/2
                              + dZAA1/2 - SW/CW * dZAZ1/2 
                              - CW/SW * dZZA1/2 ) },
     { I EL^2 * (-1 CW/SW), 
       I EL^2 * (-1 CW/SW) * ( 2*dZe1 - dSW1/(SW*CW^2) + dZW1 + dZZZ1/2
                              + dZAA1/2 - SW/CW * dZAZ1/2 
                              - CW/SW * dZZA1/2 ) } },

  C[ -V[3],   V[3],  V[1],  V[1]] == 
   { { I EL^2 * (-2),
       I EL^2 * (-2) * (2*dZe1 + dZW1 + dZAA1 - CW/SW * dZZA1 ) }, 
     { I EL^2 * ( 1), 
       I EL^2 * ( 1) * (2*dZe1 + dZW1 + dZAA1 - CW/SW * dZZA1 ) }, 
     { I EL^2 * ( 1), 
       I EL^2 * ( 1) * (2*dZe1 + dZW1 + dZAA1 - CW/SW * dZZA1 ) } }, 

(* V-V-V:     G(-) * ( g[mu1,mu2] (p2-p1)_mu3 +
                       g[mu2,mu3] (p3-p2)_mu1 +
                       g[mu3,mu1] (p1-p3)_mu2  ) 
*)
  C[ V[1], -V[3], V[3]] ==
   { -I EL ,
     -I EL * ( dZe1 + dZW1 + dZAA1/2 - CW/SW * dZZA1/2 ) },

(* sign corrected 26feb96 th *)
  C[ V[2], -V[3], V[3]] ==
   { I EL * (CW/SW), 
     I EL * (CW/SW) * (  dZe1 - 1/CW^2 dSW1/SW + dZW1 + dZZZ1/2 
                       - (SW/CW) * dZAZ1/2 ) },

(* S-S-S-S: G(+) * 1
*)
  C[S[1], S[1], S[2], S[2]] == 
    {(-I/4*EL^2*Mass[S[1]]^2)/(SW^2*Mass[V[3]]^2),
     (-I/4*EL^2*Mass[S[1]]^2)/(SW^2*Mass[V[3]]^2)
         * (  2*dZe1 - 2*dSW1/SW + dMHsq1/MH^2 + EL/(2*SW*MW*MH^2) * dTad1
            - dMWsq/MW^2 + dZH1 + dZchi1 ) }, 

  C[S[3], S[3], -S[3], -S[3]] == 
    {(-I/2*EL^2*Mass[S[1]]^2)/(SW^2*Mass[V[3]]^2),
     (-I/2*EL^2*Mass[S[1]]^2)/(SW^2*Mass[V[3]]^2)
         * (  2*dZe1 - 2*dSW1/SW + dMHsq1/MH^2 + EL/(2*SW*MW*MH^2) * dTad1
            - dMWsq1/MW^2 + 2*dZphi1 )}, 

  C[S[3], S[1], S[1], -S[3]] == 
    {(-I/4*EL^2*Mass[S[1]]^2)/(SW^2*Mass[V[3]]^2), 
     (-I/4*EL^2*Mass[S[1]]^2)/(SW^2*Mass[V[3]]^2)
         * (  2*dZe1 - 2*dSW1/SW + dMHsq1/MH^2 + EL/(2*SW*MW*MH^2) * dTad1
            - dMWsq1/MW^2 + dZH1 + dZphi1 )}, 

  C[S[3], S[2], S[2], -S[3]] == 
    {(-I/4*EL^2*Mass[S[1]]^2)/(SW^2*Mass[V[3]]^2), 
     (-I/4*EL^2*Mass[S[1]]^2)/(SW^2*Mass[V[3]]^2)
         * (  2*dZe1 - 2*dSW1/SW + dMHsq1/MH^2 + EL/(2*SW*MW*MH^2) * dTad1
            - dMWsq1/MW^2 + dZchi1 + dZphi1 )}, 

  C[S[1], S[1], S[1], S[1]] == 
    {((-3*I)/4*EL^2*Mass[S[1]]^2)/(SW^2*Mass[V[3]]^2),
     ((-3*I)/4*EL^2*Mass[S[1]]^2)/(SW^2*Mass[V[3]]^2)
         * (  2*dZe1 - 2*dSW1/SW + dMHsq1/MH^2 + EL/(2*SW*MW*MH^2) * dTad1
            - dMWsq1/MW^2 + 2*dZH1 )}, 

  C[S[2], S[2], S[2], S[2]] ==
    {((-3*I)/4*EL^2*Mass[S[1]]^2)/(SW^2*Mass[V[3]]^2),
     ((-3*I)/4*EL^2*Mass[S[1]]^2)/(SW^2*Mass[V[3]]^2)
         * (  2*dZe1 - 2*dSW1/SW + dMHsq1/MH^2 + EL/(2*SW*MW*MH^2) * dTad1
            - dMWsq1/MW^2 + 2*dZchi1 )}, 

(* S-S-S:   G(+) * 1
*)
  C[ S[1],  S[1],  S[1]] == 
    { I EL * (-3/(2*SW)) Mass[S[1]]^2/Mass[V[3]], 
      I EL * (-3/(2*SW)) Mass[S[1]]^2/Mass[V[3]] 
         * (  dZe1 - dSW1/SW + dMHsq1/MH^2 + EL/(2*SW*MW*MH^2) * dTad1
            - dMWsq1/(2*MW^2) + (3/2) dZH1 ) },
 
  C[ S[1],  S[2],  S[2]] == 
    { I EL * (-1/(2*SW)) Mass[S[1]]^2/Mass[V[3]], 
      I EL * (-1/(2*SW)) Mass[S[1]]^2/Mass[V[3]]
         * (  dZe1 - dSW1/SW + dMHsq1/MH^2 + EL/(2*SW*MW*MH^2) * dTad1
            - dMWsq1/(2*MW^2) + dZH1/2 + dZchi1 ) },

  C[ S[3],  S[1], -S[3]] == 
    { I EL * (-1/(2*SW)) Mass[S[1]]^2/Mass[V[3]], 
      I EL * (-1/(2*SW)) Mass[S[1]]^2/Mass[V[3]]
         * (  dZe1 - dSW1/SW + dMHsq1/MH^2 + EL/(2*SW*MW*MH^2) * dTad1
            - dMWsq1/(2*MW^2) + dZH1/2 + dZphi1 ) },

(* S-S-V-V:  G(+) * ( g[mu3,mu4] )
*)

  C[S[1],  S[1], V[3], -V[3]] ==
    {(I/2*EL^2)/SW^2, 
     (I/2*EL^2)/SW^2 * ( 2*dZe1 - 2*dSW1/SW + dZW1 + dZH1 )}, 

  C[S[2],  S[2], V[3], -V[3]] == 
    {(I/2*EL^2)/SW^2, 
     (I/2*EL^2)/SW^2 * ( 2*dZe1 - 2*dSW1/SW + dZW1 + dZchi1 )}, 

  C[S[3],  -S[3], V[3], -V[3]] == 
    {(I/2*EL^2)/SW^2, 
     (I/2*EL^2)/SW^2 * ( 2*dZe1 - 2*dSW1/SW + dZW1 + dZphi1 )}, 

  C[S[3], -S[3], V[2],  V[2]] ==
    {(I/2*EL^2*(CW^2 - SW^2)^2)/(CW^2*SW^2), 
     (I/2*EL^2*(CW^2 - SW^2)^2)/(CW^2*SW^2) 
       * (  2*dZe1 + 2/(SW*CW^2*(SW^2-CW^2)) dSW1 + dZZZ1 + dZphi1
          + 2*SW*CW/(SW^2-CW^2) dZAZ1 )  }, 

  C[S[3], -S[3], V[1],  V[2]] ==
    {(I*EL^2*(-CW^2 + SW^2))/(CW*SW), 
     (I*EL^2*(-CW^2 + SW^2))/(CW*SW)
       * (  2*dZe1 + 1/(CW^2*(SW^2-CW^2)) dSW1/SW + dZZZ1/2 + dZAA1/2
          + dZphi1 + (SW^2-CW^2)/(2*SW*CW) dZZA1/2
          + SW*CW/(SW^2-CW^2) dZAZ1 ) },  

  C[S[3], -S[3], V[1],  V[1]] ==
    {2*I*EL^2, 
     2*I*EL^2 * (2*dZe1 + dZAA1 + dZphi1 + (SW^2-CW^2)/(2*SW*CW) dZZA1) },

  C[S[1],  S[1], V[2],  V[2]] ==
    {(I/2*EL^2)/(CW^2*SW^2), 
     (I/2*EL^2)/(CW^2*SW^2)
       * (2*dZe1 + 2*(SW^2-CW^2)/CW^2 dSW1/SW + dZZZ1 + dZH1 ) },

  C[S[2],  S[2], V[2],  V[2]] ==
    {(I/2*EL^2)/(CW^2*SW^2), 
     (I/2*EL^2)/(CW^2*SW^2) 
       * (2*dZe1 + 2*(SW^2-CW^2)/CW^2 dSW1/SW + dZZZ1 + dZchi1 ) },

  C[S[1],  S[1], V[1],  V[2]] ==
    {0, (I/2*EL^2)/(CW^2*SW^2) dZZA1/2 },

  C[S[2],  S[2], V[1],  V[2]] ==
    {0, (I/2*EL^2)/(CW^2*SW^2) dZZA1/2 },

  C[S[1], -S[3], V[3],  V[2]] ==
    {(-I/2*EL^2)/CW, 
     (-I/2*EL^2)/CW * (  2*dZe1 - dCW1/CW + dZW1/2 + dZH1/2 + dZphi1/2
                       + dZZZ1/2 + CW/SW dZAZ1/2 ) },

  C[S[1], S[3], -V[3],  V[2]] ==
    {(-I/2*EL^2)/CW, 
     (-I/2*EL^2)/CW * (  2*dZe1 - dCW1/CW + dZW1/2 + dZH1/2 + dZphi1/2
                       + dZZZ1/2 + CW/SW dZAZ1/2 ) },

  C[S[1], S[3], -V[3],  V[1]] ==
    {(-I/2*EL^2)/SW, 
     (-I/2*EL^2)/SW * (  2*dZe1 - dSW1/SW + dZW1/2 + dZH1/2 + dZphi1/2
                       + dZAA1/2 + SW/CW dZZA1/2 ) },

  C[S[1], -S[3], V[3],  V[1]] ==
    {(-I/2*EL^2)/SW, 
     (-I/2*EL^2)/SW * (  2*dZe1 - dSW1/SW + dZW1/2 + dZH1/2 + dZphi1/2
                       + dZAA1/2 + SW/CW dZZA1/2 ) },

  C[S[3],  S[2], V[2], -V[3]] ==
    { EL^2/(2*CW), 
      EL^2/(2*CW) * ( 2*dZe1 - dCW1/CW + dZW1/2 + dZZZ1/2 + dZphi1/2 + dZchi1/2
                     + (CW/SW) dZAZ1/2 ) }, 

  C[-S[3],  S[2], V[2], V[3]] ==
    {-EL^2/(2*CW), 
     -EL^2/(2*CW) * ( 2*dZe1 - dCW1/CW + dZW1/2 + dZZZ1/2 + dZphi1/2 + dZchi1/2
                     + (CW/SW) dZAZ1/2 ) }, 

  C[S[3],  S[2], V[1], -V[3]] ==
    { EL^2/(2*SW),
      EL^2/(2*SW) * ( 2*dZe1 - dSW1/SW + dZW1/2 + dZAA1/2 + dZphi1/2 + dZchi1/2
                     + (SW/CW) dZZA1/2 ) },


  C[-S[3],  S[2], V[1], V[3]] ==
    {-EL^2/(2*SW),
     -EL^2/(2*SW) * ( 2*dZe1 - dSW1/SW + dZW1/2 + dZAA1/2 + dZphi1/2 + dZchi1/2
                     + (SW/CW) dZZA1/2 ) },

(* S-S-V:    G(-) * ( (p1-p2)_mu3 )
*)
  C[ S[2],  S[1],  V[1]] ==
    {0, EL/(2*CW*SW) dZZA1/2 }, 

  C[ S[2],  S[1],  V[2]] ==
    { EL/(2*CW*SW), 
      EL/(2*CW*SW) (  dZe1 + (SW^2-CW^2)/CW^2 dSW1/SW + dZH1/2 + dZZZ1/2
                    + dZchi1/2 ) },
          
  C[-S[3],  S[3],  V[1]] ==
    { -I EL, 
      -I EL (dZe1 + dZAA1/2 + dZphi1 + (SW^2-CW^2)/(2*SW*CW) dZZA1/2 ) },

  C[-S[3],  S[3],  V[2]] ==
    { -I EL * (SW^2 - CW^2)/(2*CW*SW),
      -I EL * (SW^2 - CW^2)/(2*CW*SW)
        * (  dZe1 + 1/((SW^2-CW^2)*CW^2) dSW1/SW + dZZZ1/2 + dZphi1
           + 2*SW*CW/(SW^2-CW^2) dZAZ1/2 ) },

  C[ S[3],  S[1], -V[3]] ==
    { -I EL/(2*SW) ,
      -I EL/(2*SW) (dZe1 - dSW1/SW + dZW1/2 + dZH1/2 + dZphi1/2) },

  C[ -S[3],  S[1], V[3]] ==
    { I EL/(2*SW) ,
      I EL/(2*SW) (dZe1 - dSW1/SW + dZW1/2 + dZH1/2 + dZphi1/2) },

  C[ S[3],  S[2], -V[3]] ==
    { EL/(2*SW), 
      EL/(2*SW) (dZe1 - dSW1/SW + dZW1/2 + dZphi1/2 + dZchi1/2 ) },

  C[-S[3],  S[2], V[3]] ==
    { EL/(2*SW), 
      EL/(2*SW) (dZe1 - dSW1/SW + dZW1/2 + dZphi1/2 + dZchi1/2 ) },

(* S-V-V:    G(+) * ( g[mu2,mu3] )
*)
  C[ S[1], -V[3],  V[3]] ==
    { I EL * Mass[V[3]] * (1/SW),
      I EL * Mass[V[3]] * (1/SW)
        * (dZe1 - dSW1/SW + dMWsq1/(2*MW^2) + dZH1/2 + dZW1 ) },

  C[ S[1],  V[2],  V[2]] ==
    { I EL * Mass[V[3]]/(SW*CW^2), 
      I EL * Mass[V[3]]/(SW*CW^2) 
        * (  dZe1 + (2*SW^2-CW^2)/CW^2 dSW1/SW + dMWsq1/(2*MW^2)
           + dZH1/2 + dZZZ1 ) },

  C[ S[1],  V[2],  V[1]] ==
    {0, I EL * Mass[V[3]]/(SW*CW^2) dZZA1/2 },

  C[-S[3],  V[3],  V[2]] ==
    { -I EL * Mass[V[3]] * (SW/CW), 
      -I EL * Mass[V[3]] * (SW/CW)
        * (  dZe1 + 1/CW^2 * dSW1/SW + dMWsq1/(2*MW^2) + dZW1/2 + dZZZ1/2
           + dZphi1/2 + CW/SW * dZAZ1/2 ) },

  C[S[3], -V[3],  V[2]] ==
    { -I EL * Mass[V[3]] * (SW/CW), 
      -I EL * Mass[V[3]] * (SW/CW)
        * (  dZe1 + 1/CW^2 * dSW1/SW + dMWsq1/(2*MW^2) + dZW1/2 + dZZZ1/2
           + dZphi1/2 + CW/SW * dZAZ1/2 ) },

  C[-S[3],  V[3],  V[1]] ==
    { -I EL * Mass[V[3]],
      -I EL * Mass[V[3]]
       * (  dZe1 + dMWsq1/(2*MW^2) + dZW1/2 + dZAA1/2 + dZphi1/2
          + SW/CW * dZZA1/2 ) },

  C[ S[3], -V[3],  V[1]] ==
    { -I EL * Mass[V[3]],
      -I EL * Mass[V[3]]
       * (  dZe1 + dMWsq1/(2*MW^2) + dZW1/2 + dZAA1/2 + dZphi1/2
          + SW/CW * dZZA1/2 ) },

(* F-F-V:    G(-) * { gamma[mu3] omega[-],
                      gamma[mu3] omega[+]  }
*)
(*
 gn-  = 1/(2*SW*CW)                    gn+ = 0 
 ge-  = (-1/2+SW^2)/(SW*CW)           ge+ = (SW/CW) ; 
 gu-  = (1/2 - 2/3 SW^2)/(SW*CW)      gu+ = (-2/3 * SW/CW) ; 
 gd-  = (-1/2 + 1/3 SW^2)/(SW*CW)     gd+ = (1/3 * SW/CW) ; 
 dgn- = 1/(2*SW*CW) (dZe1+(SW^2-CW^2)/CW^2 dSW1/SW)     dgn+=0
 dge- = (-1)/(2*SW*CW) (dZe1+ (SW^2-CW^2)/CW^2 dSW1/SW)
          +  (SW/CW) (dZe1+1/CW^2 dSW1/SW)
 dge+ = (SW/CW) (dZe1+1/CW^2 dSW1/SW)
 dgu- = 1/(2*SW*CW) (dZe1+(SW^2-CW^2)/CW^2 dSW1/SW)
          + (-2*SW)/(3*CW) (dZe1+1/CW^2 dSW1/SW)
 dgu+ = (-2*SW)/(3*CW) (dZe1+1/CW^2 dSW1/SW)
 dgd- = (-1)/(2*SW*CW) (dZe1+(SW^2-CW^2)/CW^2 dSW1/SW)
          + SW/(3*CW) (dZe1+1/CW^2 dSW1/SW)
 dgd+ = SW/(3*CW) (dZe1+1/CW^2 dSW1/SW)
*)

  C[ -F[1, {j1}], F[1, {j2}], V[1]] == 
   { { 0,
       I EL IndexDelta[j1,j2] 1/(2*SW*CW) dZZA1/2 }, 
     { 0,
       0 } },

  C[ -F[2, {j1}], F[2, {j2}], V[1]] == 
   { { I EL * IndexDelta[j1, j2],
       I EL IndexDelta[j1, j2] 
         * (  dZe1 + dZAA1/2
            + (dZfL1[2,{j1},{j2}] + Conjugate[dZfL1[2,{j1},{j2}]])/2
            + (-1/2+SW^2)/(SW*CW) * dZZA1/2 ) }, 
     { I EL * IndexDelta[j1, j2],
       I EL IndexDelta[j1, j2]
         * (  dZe1 + dZAA1/2
            + (dZfR1[2,{j1},{j2}] + Conjugate[dZfR1[2,{j1},{j2}]])/2
            + IndexDelta[j1,j2] *  (SW/CW) * dZZA1/2 ) } },

  C[ -F[3, {j1}], F[3, {j2}], V[1]] == 
   { { I EL * (-2/3) * IndexDelta[j1, j2],
       I EL 
         * (  (-2/3) * ( IndexDelta[j1, j2] * (dZe1 + dZAA1/2)
            + (dZfL1[3,{j1},{j2}] + Conjugate[dZfL1[3,{j1},{j2}]])/2 )
            + IndexDelta[j1,j2] * (1/2 - 2/3 SW^2)/(SW*CW) * dZZA1/2 ) }, 
     { I EL * (-2/3) * IndexDelta[j1, j2],
       I EL 
         * (  (-2/3) * ( IndexDelta[j1, j2] * (dZe1 + dZAA1/2)
            + (dZfR1[3,{j1},{j2}] + Conjugate[dZfR1[3,{j1},{j2}]])/2 )
            + IndexDelta[j1,j2] * (-2/3 * SW/CW) * dZZA1/2 ) } },

  C[ -F[4, {j1}], F[4, {j2}], V[1]] == 
   { { I EL * (1/3) * IndexDelta[j1, j2],
       I EL 
         * (  (1/3) * ( IndexDelta[j1, j2] * (dZe1 + dZAA1/2)
            + (dZfL1[4,{j1},{j2}] + Conjugate[dZfL1[4,{j1},{j2}]])/2 )
            + IndexDelta[j1,j2] * (-1/2 + 1/3 SW^2)/(SW*CW) * dZZA1/2 ) }, 
     { I EL * (1/3) * IndexDelta[j1, j2],
       I EL 
         * (  (1/3) * ( IndexDelta[j1, j2] * (dZe1 + dZAA1/2)
            + (dZfR1[4,{j1},{j2}] + Conjugate[dZfR1[4,{j1},{j2}]])/2 )
            + IndexDelta[j1,j2] * (1/3 * SW/CW) * dZZA1/2 ) } },

  C[ -F[1, {j1}], F[1, {j2}], V[2]] == 
   {{ I EL * 1/(2*SW*CW) * IndexDelta[j1, j2],
      I EL IndexDelta[j1,j2]
       * (  1/(2*SW*CW)
            * (  dZZZ1/2
               + (dZfL1[1,{j1},{j2}] + Conjugate[dZfL1[1,{j1},{j2}]])/2)
          + 1/(2*SW*CW) (dZe1+(SW^2-CW^2)/CW^2 dSW1/SW) )  },
    { 0,
      0 }},

  C[ -F[2, {j1}], F[2, {j2}], V[2]] ==
   {{ I EL (-1/2+SW^2)/(SW*CW) IndexDelta[j1,j2],
      I EL IndexDelta[j1,j2]
       *( (-1/2+SW^2)/(SW*CW) 
           *(  dZZZ1/2
             + (dZfL1[2,{j1},{j2}] + Conjugate[dZfL1[2,{j1},{j2}]])/2
             + (-1)/(2*SW*CW) (dZe1 + (SW^2-CW^2)/CW^2 dSW1/SW)
              + (SW/CW) (dZe1+1/CW^2 dSW1/SW) ) 
         + dZAZ1/2 )  },
    { I EL (SW/CW) IndexDelta[j1,j2],
      I EL IndexDelta[j1,j2]
       *(  (SW/CW)
           *( dZZZ1/2
             + (dZfR1[2,{j1},{j2}] + Conjugate[dZfR1[2,{j1},{j2}]])/2) 
         +  (SW/CW) (dZe1+1/CW^2 dSW1/SW)
         + dZAZ1/2  ) } },

  C[ -F[3, {j1}], F[3, {j2}], V[2]] ==
   {{ I EL (1/2 - 2/3 SW^2)/(SW*CW) IndexDelta[j1,j2],
      I EL
       *(  (1/2 - 2/3 SW^2)/(SW*CW)
           *(  IndexDelta[j1, j2] dZZZ1/2
             + (dZfL1[3,{j1},{j2}] + Conjugate[dZfL1[3,{j1},{j2}]])/2) 
         + IndexDelta[j1,j2]
           * (  1/(2*SW*CW) (dZe1+(SW^2-CW^2)/CW^2 dSW1/SW)
              + (-2*SW)/(3*CW) (dZe1+1/CW^2 dSW1/SW)
              - (2/3) dZAZ1/2 ) ) },
    { I EL (-2/3 * SW/CW) IndexDelta[j1,j2],
      I EL
       *(  (-2/3 * SW/CW)
           *(  IndexDelta[j1, j2] dZZZ1/2
             + (dZfR1[3,{j1},{j2}] + Conjugate[dZfR1[3,{j1},{j2}]])/2) 
         + IndexDelta[j1,j2]
           * ( (-2*SW)/(3*CW) (dZe1+1/CW^2 dSW1/SW)
              - (2/3) dZAZ1/2 ) ) } },

  C[ -F[4, {j1}], F[4, {j2}], V[2]] ==
   {{ I EL (-1/2 + 1/3 SW^2)/(SW*CW) IndexDelta[j1,j2],
      I EL
       *(  (-1/2 + 1/3 SW^2)/(SW*CW)
           *(  IndexDelta[j1, j2] dZZZ1/2
             + (dZfL1[4,{j1},{j2}] + Conjugate[dZfL1[4,{j1},{j2}]])/2) 
         + IndexDelta[j1,j2]
           * ( (-1)/(2*SW*CW) (dZe1+(SW^2-CW^2)/CW^2 dSW1/SW)
              + SW/(3*CW) (dZe1+1/CW^2 dSW1/SW)
              + (1/3) dZAZ1/2 ) ) },
    { I EL (1/3 * SW/CW) IndexDelta[j1,j2],
      I EL
       *(  (1/3 * SW/CW)
           *(  IndexDelta[j1, j2] dZZZ1/2
             + (dZfR1[4,{j1},{j2}] + Conjugate[dZfR1[4,{j1},{j2}]])/2) 
         + IndexDelta[j1,j2]
           * ( SW/(3*CW) (dZe1+1/CW^2 dSW1/SW)
              + (1/3) dZAZ1/2 ) ) } },

  C[ -F[1, {j1}], F[2, {j2}], -V[3]] == 
   {{ I EL 1/(2^(1/2)*SW) IndexDelta[j1, j2], 
      I EL 1/(2^(1/2)*SW) IndexDelta[j1, j2]
        * (  dZe1 - dSW1/SW + dZW1/2
           + (Conjugate[dZfL1[1,{j1},{j1}]] + dZfL1[2,{j2},{j2}])/2 ) },
    {0,0}},

  C[ -F[2, {j1}], F[1, {j2}],  V[3]] == 
   {{ I EL 1/(2^(1/2)*SW) IndexDelta[j1, j2], 
      I EL 1/(2^(1/2)*SW) IndexDelta[j1, j2]
        * (  dZe1 - dSW1/SW + dZW1/2
           + (Conjugate[dZfL1[2,{j1},{j1}]] + dZfL1[1,{j2},{j2}])/2 ) },
    {0,0}},

  C[ -F[3, {j1}], F[4, {j2}], -V[3]] == 
   {{ I EL 1/(2^(1/2)*SW) Matrix[FQ][j1,j2] , 
      I EL 1/(2^(1/2)*SW)
       *(  Matrix[FQ][j1,j2] (dZe1 - dSW1/SW + dZW1/2)
         + Matrix[dFQ][j1,j2]
         + Sum[ Matrix[FQ][k,j2]*Conjugate[dZfL1[3,{j1},{k}]]
               +Matrix[FQ][j1,k]*dZfL1[4,{k},{j2}],
               {k,MaxGenerationIndex}]/2 ) }, 
      {0,0}  },

  C[ -F[4, {j2}], F[3, {j1}], V[3]] == 
   {{ I EL 1/(2^(1/2)*SW) Conjugate[Matrix[FQ][j2,j1]] ,
      I EL 1/(2^(1/2)*SW)
       *(  Conjugate[Matrix[FQ][j2,j1]] (dZe1 - dSW1/SW + dZW1/2)
         + Conjugate[Matrix[dFQ][j2,j1]]
         + Sum[ Conjugate[Matrix[FQ][k,j1]]*Conjugate[dZfL1[4,{j2},{k}]]
               +Conjugate[Matrix[FQ][j2,k]]*dZfL1[3,{k},{j1}],
               {k,MaxGenerationIndex}]/2 ) }, 
      {0,0}  },


(* F-F-S:    G(+) * { omega[-],
                      omega[+] }
*)

  C[ -F[2, {j1}], F[2, {j2}], S[1]] == 
   {{ I EL (-1/(2*SW)) Mass[F[2, {j1}]]/Mass[V[3]] IndexDelta[j1, j2],
      I EL (-1/(2*SW)) Mass[F[2, {j1}]]/Mass[V[3]] IndexDelta[j1, j2]
       * (  dZe1 - dSW1/SW + dMf1[2,{j1}]/Mass[F[2,{j1}]]
          - dMWsq1/(2*MW^2) + dZH1/2 
          + (dZfL1[2,{j1}] + Conjugate[dZfR1[2,{j1}]])/2 ) },
    { I EL (-1/(2*SW)) Mass[F[2, {j1}]]/Mass[V[3]] IndexDelta[j1, j2],
      I EL (-1/(2*SW)) Mass[F[2, {j1}]]/Mass[V[3]] IndexDelta[j1, j2]
       * (  dZe1 - dSW1/SW + dMf1[2,{j1}]/Mass[F[2,{j1}]]
          - dMWsq1/(2*MW^2) + dZH1/2 
          + (dZfR1[2,{j1}] + Conjugate[dZfL1[2,{j1}]])/2 ) } },

  C[ -F[3, {j1}], F[3, {j2}], S[1]] == 
   {{ I EL (-1/(2*SW)) Mass[F[3, {j1}]]/Mass[V[3]] IndexDelta[j1, j2],
      I EL (-1/(2*SW)) Mass[F[3, {j1}]]/Mass[V[3]]
       *(  IndexDelta[j1, j2] 
           * (  dZe1 - dSW1/SW + dMf1[3,{j1}]/Mass[F[3,{j1}]]
              - dMWsq1/(2*MW^2) + dZH1/2 )
         + dZfL1[3,{j1},{j2}]/2 )
     + I EL (-1/(2*SW)) Mass[F[3, {j2}]]/Mass[V[3]]
       * Conjugate[dZfR1[3,{j1},{j2}]]/2  },
    { I EL (-1/(2*SW)) Mass[F[3, {j1}]]/Mass[V[3]] IndexDelta[j1, j2],
      I EL (-1/(2*SW)) Mass[F[3, {j1}]]/Mass[V[3]]
       *(  IndexDelta[j1, j2] 
           * (  dZe1 - dSW1/SW + dMf1[3,{j1}]/Mass[F[3,{j1}]]
              - dMWsq1/(2*MW^2) + dZH1/2 )
         + dZfR1[3,{j1},{j2}]/2 )
     + I EL (-1/(2*SW)) Mass[F[3, {j2}]]/Mass[V[3]]
       * Conjugate[dZfL1[3,{j1},{j2}]]/2  } },

  C[ -F[4, {j1}], F[4, {j2}], S[1]] == 
   {{ I EL (-1/(2*SW)) Mass[F[4, {j1}]]/Mass[V[3]] IndexDelta[j1, j2],
      I EL (-1/(2*SW)) Mass[F[4, {j1}]]/Mass[V[3]]
       *(  IndexDelta[j1, j2] 
           * (  dZe1 - dSW1/SW + dMf1[4,{j1}]/Mass[F[4,{j1}]]
              - dMWsq1/(2*MW^2) + dZH1/2 )
         + dZfL1[4,{j1},{j2}]/2 )
     + I EL (-1/(2*SW)) Mass[F[4, {j2}]]/Mass[V[3]]
       * Conjugate[dZfR1[4,{j1},{j2}]]/2  },
    { I EL (-1/(2*SW)) Mass[F[4, {j1}]]/Mass[V[3]] IndexDelta[j1, j2],
      I EL (-1/(2*SW)) Mass[F[4, {j1}]]/Mass[V[3]]
       *(  IndexDelta[j1, j2] 
           * (  dZe1 - dSW1/SW + dMf1[4,{j1}]/Mass[F[4,{j1}]]
              - dMWsq1/(2*MW^2) + dZH1/2 )
         + dZfR1[4,{j1},{j2}]/2 )
     + I EL (-1/(2*SW)) Mass[F[4, {j2}]]/Mass[V[3]]
       * Conjugate[dZfL1[4,{j1},{j2}]]/2  } },

  C[ -F[2, {j1}], F[2, {j2}], S[2]] == 
   {{-EL 1/(2*SW) Mass[F[2, {j1}]]/Mass[V[3]] IndexDelta[j1,j2],
     -EL 1/(2*SW) Mass[F[2, {j1}]]/Mass[V[3]] IndexDelta[j1,j2]
       * (  dZe1 - dSW1/SW + dMf1[2,{j1}]/Mass[F[2,{j1}]]
          - dMWsq1/(2*MW^2) + dZchi1/2
          + (dZfL1[2,{j1}] + Conjugate[dZfR1[2,{j1}]])/2 ) },
    { EL 1/(2*SW) Mass[F[2, {j1}]]/Mass[V[3]] IndexDelta[j1,j2],
      EL 1/(2*SW) Mass[F[2, {j1}]]/Mass[V[3]] IndexDelta[j1,j2]
       * (  dZe1 - dSW1/SW + dMf1[2,{j1}]/Mass[F[2,{j1}]]
          - dMWsq1/(2*MW^2) + dZchi1/2
          + (dZfR1[2,{j1}] + Conjugate[dZfL1[2,{j1}]])/2 ) } },

  C[ -F[3, {j1}], F[3, {j2}], S[2]] == 
   {{+EL 1/(2*SW) Mass[F[3, {j1}]]/Mass[V[3]] IndexDelta[j1,j2],
     +EL 1/(2*SW) Mass[F[3, {j1}]]/Mass[V[3]] IndexDelta[j1,j2]
       * (  dZe1 - dSW1/SW + dMf1[3,{j1}]/Mass[F[3,{j1}]]
          - dMWsq1/(2*MW^2) + dZchi1/2 )
     +EL 1/(2*SW) Mass[F[3, {j1}]]/Mass[V[3]]
       * dZfL1[3,{j1}]/2
     +EL 1/(2*SW) Mass[F[3, {j2}]]/Mass[V[3]]
       * Conjugate[dZfR1[3,{j1}]]/2  },
    {-EL 1/(2*SW) Mass[F[3, {j1}]]/Mass[V[3]] IndexDelta[j1,j2],
     -EL 1/(2*SW) Mass[F[3, {j1}]]/Mass[V[3]] IndexDelta[j1,j2]
       * (  dZe1 - dSW1/SW + dMf1[3,{j1}]/Mass[F[3,{j1}]]
          - dMWsq1/(2*MW^2) + dZchi1/2 )
     -EL 1/(2*SW) Mass[F[3, {j1}]]/Mass[V[3]]
       * dZfR1[3,{j1}]/2
     -EL 1/(2*SW) Mass[F[3, {j2}]]/Mass[V[3]]
       * Conjugate[dZfL1[3,{j1}]]/2  } },

  C[ -F[4, {j1}], F[4, {j2}], S[2]] == 
   {{-EL 1/(2*SW) Mass[F[4, {j1}]]/Mass[V[3]] IndexDelta[j1,j2],
     -EL 1/(2*SW) Mass[F[4, {j1}]]/Mass[V[3]] IndexDelta[j1,j2]
       * (  dZe1 - dSW1/SW + dMf1[4,{j1}]/Mass[F[4,{j1}]]
          - dMWsq1/(2*MW^2) + dZchi1/2 )
     -EL 1/(2*SW) Mass[F[4, {j1}]]/Mass[V[3]]
       * dZfL1[4,{j1}]/2
     -EL 1/(2*SW) Mass[F[4, {j2}]]/Mass[V[3]]
       * Conjugate[dZfR1[4,{j1}]]/2  },
    {+EL 1/(2*SW) Mass[F[4, {j1}]]/Mass[V[3]] IndexDelta[j1,j2],
     +EL 1/(2*SW) Mass[F[4, {j1}]]/Mass[V[3]] IndexDelta[j1,j2]
       * (  dZe1 - dSW1/SW + dMf1[4,{j1}]/Mass[F[4,{j1}]]
          - dMWsq1/(2*MW^2) + dZchi1/2 )
     +EL 1/(2*SW) Mass[F[4, {j1}]]/Mass[V[3]]
       * dZfR1[4,{j1}]/2
     +EL 1/(2*SW) Mass[F[4, {j2}]]/Mass[V[3]]
       * Conjugate[dZfL1[4,{j1}]]/2  } },

  C[ -F[3, {j1}], F[4, {j2}], -S[3]] == 
   {{ I EL 1/(2^(1/2)*SW) Mass[F[3, {j1}]]/Mass[V[3]] Matrix[FQ][j1,j2] ,
      I EL 1/(2^(1/2)*SW) Mass[F[3, {j1}]]/Mass[V[3]] Matrix[FQ][j1,j2] 
       * (  dZe1 - dSW1/SW + dMf1[3,{j1}]/Mass[F[3, {j1}]]
          - dMWsq1/(2*MW^2) + dZphi1/2)
     +I EL 1/(2^(1/2)*SW) Mass[F[3, {j1}]]/Mass[V[3]] Matrix[dFQ][j1,j2]
     +I EL 1/(2^(1/2)*SW) 1/Mass[V[3]] 
       * Sum[ Mass[F[3, {j1}]] Matrix[FQ][j1,k] dZfL1[4,{k},{j2}]
             +Mass[F[3, {k }]] Matrix[FQ][k,j2] Conjugate[dZfR1[3,{j1},{k}]],
             {k,MaxGenerationIndex}]/2 },
    {-I EL 1/(2^(1/2)*SW) Mass[F[4, {j2}]]/Mass[V[3]] Matrix[FQ][j1,j2] ,
     -I EL 1/(2^(1/2)*SW) Mass[F[4, {j1}]]/Mass[V[3]] Matrix[FQ][j1,j2] 
       * (  dZe1 - dSW1/SW + dMf1[4,{j2}]/Mass[F[4, {j2}]]
          - dMWsq1/(2*MW^2) + dZphi1/2)
     -I EL 1/(2^(1/2)*SW) Mass[F[4, {j2}]]/Mass[V[3]] Matrix[dFQ][j1,j2]
     -I EL 1/(2^(1/2)*SW) 1/Mass[V[3]] 
       * Sum[ Mass[F[4, {k }]] Matrix[FQ][j1,k] dZfR1[4,{k},{j2}]
             +Mass[F[4, {j2}]] Matrix[FQ][k,j2] Conjugate[dZfL1[3,{j1},{k}]],
             {k,MaxGenerationIndex}]/2 } },

  C[ -F[4, {j2}], F[3, {j1}], S[3]] == 
   {{-I EL 1/(2^(1/2)*SW) Mass[F[4, {j2}]]/Mass[V[3]]
                             * Conjugate[Matrix[FQ][j2,j1]] ,
     -I EL 1/(2^(1/2)*SW) Mass[F[4, {j2}]]/Mass[V[3]]
                             * Conjugate[Matrix[FQ][j2,j1]] 
       * (  dZe1 - dSW1/SW + dMf1[4,{j2}]/Mass[F[4, {j2}]]
          - dMWsq1/(2*MW^2) + dZphi1/2)
     -I EL 1/(2^(1/2)*SW) Mass[F[4, {j2}]]/Mass[V[3]]
                             * Conjugate[Matrix[dFQ][j2,j1]]
     -I EL 1/(2^(1/2)*SW) 1/Mass[V[3]] 
       * Sum[  Mass[F[4, {j2}]] Conjugate[Matrix[FQ][j2,k]]
                           * dZfL1[3,{k},{j1}]
             + Mass[F[4, {k }]] Conjugate[Matrix[FQ][k,j1]]
                           * Conjugate[dZfR1[4,{j2},{k}]],
             {k,MaxGenerationIndex}]/2 },
    { I EL 1/(2^(1/2)*SW) Mass[F[3, {j1}]]/Mass[V[3]]
                             * Conjugate[Matrix[FQ][j2,j1]] ,
      I EL 1/(2^(1/2)*SW) Mass[F[3, {j1}]]/Mass[V[3]]
                             * Conjugate[Matrix[FQ][j2,j1]] 
       * (  dZe1 - dSW1/SW + dMf1[3,{j1}]/Mass[F[3, {j1}]]
          - dMWsq1/(2*MW^2) + dZphi1/2)
     +I EL 1/(2^(1/2)*SW) Mass[F[3, {j1}]]/Mass[V[3]]
                             * Conjugate[Matrix[dFQ][j2,j1]]
     +I EL 1/(2^(1/2)*SW) 1/Mass[V[3]] 
       * Sum[  Mass[F[3, {k }]] Conjugate[Matrix[FQ][j2,k]]
                           * dZfR1[3,{k},{j1}]
             + Mass[F[3, {j1}]] Conjugate[Matrix[FQ][k,j1]]
                           * Conjugate[dZfL1[4,{j2},{k}]],
             {k,MaxGenerationIndex}]/2 } },

  C[ -F[1, {j1}], F[2, {j2}], -S[3]] == 
   {{0,0}, 
    {-I*EL 1/(2^(1/2)*SW) Mass[F[2,{j2}]]/Mass[V[3]] IndexDelta[j1, j2],
     -I*EL 1/(2^(1/2)*SW) Mass[F[2,{j2}]]/Mass[V[3]] IndexDelta[j1, j2]
       * (  dZe1 - dSW1/SW + dMf1[2,{j2}]/Mass[F[2,{j2}]]
          - dMWsq1/(2*MW^2) + dZphi1/2 
          + Conjugate[dZfL1[1,{j1}]]/2 + dZfR1[2,{j2}] ) }},

  C[ -F[2, {j2}], F[1, {j1}], S[3]] == 
   {{-I*EL 1/(2^(1/2)*SW) Mass[F[2,{j2}]]/Mass[V[3]] IndexDelta[j1, j2],
     -I*EL 1/(2^(1/2)*SW) Mass[F[2,{j2}]]/Mass[V[3]] IndexDelta[j1, j2]
       * (  dZe1 - dSW1/SW + dMf1[2,{j2}]/Mass[F[2,{j2}]]
          - dMWsq1/(2*MW^2) + dZphi1/2 
          + Conjugate[dZfR1[2,{j2}]]/2 + dZfL1[1,{j1}] ) },
    {0,0} },

(* S-U-U:  G(+) * 1
*)

  C[ S[1], -U[2], U[2]] == 
    { (-I/2*EL*Mass[V[3]])/(CW^2*SW*GaugeXi[Z]),
      (-I/2*EL*Mass[V[3]])/(CW^2*SW*GaugeXi[Z])
       * (dZe1 + (2*SW^2-CW^2)/CW^2 dSW1/SW + dMWsq1/(2*MW^2) + dZH1/2) }, 

  C[S[1], -U[4], U[4]] == 
    { (-I/2*EL*Mass[V[3]])/(SW*GaugeXi[W]),
      (-I/2*EL*Mass[V[3]])/(SW*GaugeXi[W])
       * (dZe1 - dSW1/SW + dMWsq1/(2*MW^2) + dZH1/2) },

  C[ S[1], -U[3], U[3]] == 
    { (-I/2*EL*Mass[V[3]])/(SW*GaugeXi[W]),
      (-I/2*EL*Mass[V[3]])/(SW*GaugeXi[W])
       * (dZe1 - dSW1/SW + dMWsq1/(2*MW^2) + dZH1/2) },

  C[ S[2], -U[4], U[4]] ==
    { (EL*Mass[V[3]])/(2*SW*GaugeXi[W]),
      (EL*Mass[V[3]])/(2*SW*GaugeXi[W])
       * (dZe1 - dSW1/SW + dMWsq1/(2*MW^2) + dZchi1/2) },

  C[ S[2], -U[3], U[3]] == 
    { -(EL*Mass[V[3]])/(2*SW*GaugeXi[W]),
      -(EL*Mass[V[3]])/(2*SW*GaugeXi[W])
        * (dZe1 - dSW1/SW + dMWsq1/(2*MW^2) + dZchi1/2) },

  C[ -S[3], -U[2], U[3]] == 
    { (I/2*EL*Mass[V[3]])/(CW*SW*GaugeXi[Z]),
      (I/2*EL*Mass[V[3]])/(CW*SW*GaugeXi[Z])
       *(dZe1 + (SW^2-CW^2)/CW^2 dSW1/SW + dMWsq1/(2*MW^2) + dZphi1/2) },

  C[ S[3], -U[2], U[4]] == 
    { (I/2*EL*Mass[V[3]])/(CW*SW*GaugeXi[Z]),
      (I/2*EL*Mass[V[3]])/(CW*SW*GaugeXi[Z])
       *(dZe1 + (SW^2-CW^2)/CW^2 dSW1/SW + dMWsq1/(2*MW^2) + dZphi1/2) },

  C[ -S[3], -U[4], U[2]] == 
    { (I/2*EL*(-CW^2 + SW^2)*Mass[V[3]])/ (CW*SW*GaugeXi[W]),
      (I/2*EL*(-CW^2 + SW^2)*Mass[V[3]])/ (CW*SW*GaugeXi[W])
       *(dZe1 + 1/((SW^2-CW^2)*CW^2) dSW1/SW + dMWsq1/(2*MW^2) + dZphi1/2) },
          
  C[ S[3], -U[3], U[2]] == 
    { (I/2*EL*(-CW^2 + SW^2)*Mass[V[3]])/ (CW*SW*GaugeXi[W]),
      (I/2*EL*(-CW^2 + SW^2)*Mass[V[3]])/ (CW*SW*GaugeXi[W])
       *(dZe1 + 1/((SW^2-CW^2)*CW^2) dSW1/SW + dMWsq1/(2*MW^2) + dZphi1/2) },

  C[ -S[3], -U[4], U[1]] == 
    { I*EL*Mass[V[3]]/GaugeXi[W],
      I*EL*Mass[V[3]]/GaugeXi[W] (dZe1 + dMWsq1/(2*MW^2) + dZphi1/2) },

  C[ S[3], -U[3], U[1]] == 
    { I*EL*Mass[V[3]]/GaugeXi[W],
      I*EL*Mass[V[3]]/GaugeXi[W] (dZe1 + dMWsq1/(2*MW^2) + dZphi1/2) },


(* U-U-V: G(+) * { p1_mu3,
                   p2_mu3 }
*)
  C[ -U[4], U[4],  V[1]] ==
    {{I*EL, I*EL (dZe1 + dZAA1/2 - (CW/SW) dZZA1/2) },
     {0,0}}, 

  C[ -U[3], U[3],  V[1]] ==
    {{-I*EL, -I*EL (dZe1 + dZAA1/2 - (CW/SW) dZZA1/2) },
     {0,0}}, 

  C[ -U[4], U[4],  V[2]] ==
    {{-I*EL CW/SW, -I*EL CW/SW (dZe1 - 1/CW^2 dSW1/SW + dZZZ1/2)
                   + I*EL dZAZ1/2 },
     {0,0}}, 

  C[ -U[3], U[3],  V[2]] ==
    {{I*EL CW/SW, I*EL CW/SW (dZe1 - 1/CW^2 dSW1/SW + dZZZ1/2 )
                  - I*EL dZAZ1/2 },
     {0,0}}, 

  C[ -U[4], U[2], -V[3]] ==
    {{ I*EL CW/SW , I*EL CW/SW (dZe1 - 1/CW^2 dSW1/SW + dZW1/2) },
     {0,0}},

  C[ -U[3], U[2], V[3]] ==
    {{ -I*EL CW/SW , -I*EL CW/SW (dZe1 - 1/CW^2 dSW1/SW + dZW1/2) },
     {0,0}},

  C[ -U[2], U[3], -V[3]] ==
    {{ -I*EL CW/SW, -I*EL CW/SW (dZe1 - 1/CW^2 dSW1/SW + dZW1/2) }, 
     {0,0}}, 

  C[ -U[2], U[4], V[3]] ==
    {{ I*EL CW/SW, I*EL CW/SW (dZe1 - 1/CW^2 dSW1/SW + dZW1/2) }, 
     {0,0}}, 

  C[ -U[4], U[1], -V[3]] ==
    {{ -I*EL , -I*EL (dZe1 + dZW1/2) },
     { 0, 0}}, 

  C[ -U[3], U[1], V[3]] ==
    {{ I*EL , I*EL (dZe1 + dZW1/2) },
     { 0, 0}},

  C[ -U[1], U[3], -V[3]] ==
    {{ I*EL , I*EL (dZe1 + dZW1/2) },
     { 0, 0}},

  C[ -U[1], U[4], V[3]] ==
    {{ -I*EL , -I*EL (dZe1 + dZW1/2) },
     { 0, 0}}

};

(* M$LastModelRules: possibility to apply some transformation rules to
   the amplitudes (e.g. to change contexts):
*)
M$LastModelRules =
{
};

(**)
