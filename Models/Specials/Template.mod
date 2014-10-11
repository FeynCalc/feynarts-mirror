
(* :Title: Template.mod *)

(* :Author:  *)

(* :Summary: Model file template for HighEnergyPhysics`FeynArts. 
*)

(* :Context: HighEnergyPhysics`FeynArts` *)

(* :Mathematica Version 2.0 *)

(* :Requirements: FeynArts 2.0 *)

(* :Description: 
	This file contains the definition of a model for FeynArts.
	- All particles are arranged in classes. For single particle
	  model definitions each particle lives in its own class 
	  (this resembles the FeynArts 1.0 point of view).
	- For each class the common SelfConjugate behaviour and the
	  Indices MUST be present in the definitions.
	- For each Indices entry in a class (choice of an index name)
	  a IndexRange definition must be given.
	- IMPORTANT: The coupling matrices for the following couplings
	  have to be defined in the same order like in the Generic model
	  (containing the general analytical expressions for the vertices):
	  F-F-S, F-F-V, U-U-V, V-V-V-V. These vertices are NOT ORDERLESS!
        - IndexDelta's must be global over the whole vertex in order
	  to be considered in the particle indices of the graphs
          (this means the IndexDelta must be written before the whole
	  coupling, so the coupling must be diagonal in all generic subparts
	  and in all counterterms.)
*)

(* Class descriptions:
*)

IndexRange[ Index[ Generation ] ] = { 1, 2, 3 };
Appearance[ Index[ Generation, i_Integer ] ] := Alph[ i+8 ];


IndexDelta[ Index[Generation,i_], Index[Generation,i_] ] := 1

ClassesDescription["QE"] =
{
  (* Leptons (e, mu, tau) *)
 F[2]  == { SelfConjugate -> False, 
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
TheMass[ F[2,{1}] ] = ME;	TheLabel[ F[2,{1}] ] = "e";
TheMass[ F[2,{2}] ] = MM;	TheLabel[ F[2,{2}] ] = SymbolChar["mu"];
TheMass[ F[2,{3}] ] = ML;	TheLabel[ F[2,{3}] ] = SymbolChar["tau"];

(* Couplings:
*)
CouplingMatrices["QE"] =
{
  C[ F[2,{j1}], -F[2,{j2}] ]
       == IndexDelta[j1,j2] * { { 0, CL }, { 0, CR },
	                        { 0, CSM }, { 0, CSP } },
  C[ V[1], V[1] ]
       == { { 0, -I CAA }, 
	    { 0,  0 }     },

   (** f - f - V **)
  C[ F[2,{j1}], -F[2,{j2}], V[1] ]
       == IndexDelta[j1,j2] * { List[ I*EL, I*EL*CL ], 
	                        List[ I*EL, I*EL*CR ] }
}

(* After the generation of the amplitude a model-dependend set of rules 
   can be applied to the analytic expression. This set of rules is defined
   here. 
*)
LastModelRules =
 {
 };
(**)
