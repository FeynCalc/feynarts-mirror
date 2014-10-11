
(* :Title: QED.mod *)

(* :Authors: Hagen Eck, Sepp Kueblbeck *)

(* :Summary: Model file for HighEnergyPhysics`FeynArts. 
	     model:
	     => Quantum Electrodynamics with leptons only <=
	     -----------------------------------------------
*)

(* :Context: HighEnergyPhysics`FeynArts` *)

(* :Package Version 0.1 *)

(* :Mathematica Version 2.0 *)

(* :Requirements: FeynArts 2.0 *)

(* This line indicates the type of classes model that is loaded (should
   be the basename of this file and has to match the identifier of
   ClassesDescription and CouplingMatrices):
*)


(* Class descriptions:
*)

IndexRange[ Index[ Generation ] ] = { 1, 2, 3 };
Appearance[ Index[ Generation, i_Integer ] ] := Alph[ i+8 ];

M$ClassesDescription =
{
  (* Leptons (e, mu, tau) *)
 F[1]  == { SelfConjugate -> False, 
              Indices -> { Index[Generation] },
	      Mass -> MLE,
	      PropagatorLabel -> ComposedChar[ {"e", Index[Generation]} ], 
	      PropagatorType -> Straight, 
	      PropagatorArrow -> Forward }, 

  (* Gaugebosons: Q = 0 *)
 V[1]  == { SelfConjugate -> True, 
	      Mass -> 0, 
	      PropagatorLabel -> SymbolChar["gamma"], 
	      PropagatorType -> Sine, 
	      PropagatorArrow -> None }
}

(* Definition of masses and labels for explicit particles: 
*)
TheMass[ F[1,{1}] ] = ME;	TheLabel[ F[1,{1}] ] = "e";
TheMass[ F[1,{2}] ] = MM;	TheLabel[ F[1,{2}] ] = SymbolChar["mu"];
TheMass[ F[1,{3}] ] = ML;	TheLabel[ F[1,{3}] ] = SymbolChar["tau"];

(* Definition of the QED-coupling(s):
 *)

M$CouplingMatrices =
{
(* F-F : G(+) * { slash[ p1 ] * omega[-],
                  slash[ p2 ] * omega[+],
                  omega[-],
                  omega[+]               }
*)
(*
  C[ F[1,{j1}], -F[1,{j2}] ]
       == IndexDelta[j1,j2] * { { 0, CL }, { 0, CR },
	                        { 0, CSM }, { 0, CSP } },
*)
(* V-V : G(+) * { g[mu,nu] p1 p2,
                  p1_mu p2_nu,
                  g[mu,nu]        }
*)
(*
  C[ V[1], V[1] ]
       == { { 0, -I CAA }, 
	    { 0,  0 } ,
            { 0,  0 }        },
*)

 (* F-F-V : G(+) * { gamma[mu] omega[-],
                     gamma[mu] omega[+]  }
*)
  C[ F[1,{j1}], -F[1,{j2}], V[1] ]
       == IndexDelta[j1,j2] * { { I*EL, I*EL*CL }, 
	                        { I*EL, I*EL*CR } }
};

(* LastModelRules: applied at the and of the amplitude generation.
*)
M$LastModelRules =
 {
 };
(**)
