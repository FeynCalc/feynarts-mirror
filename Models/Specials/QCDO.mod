
(* :Title: QCDO.mod *)

(* :Description:
	Model file for FeynArts.
	********* generic QCDO ***********
*)

(* :Authors: R.Mertig, H.Eck
*)

LastModelRules["QCDO"] =
 {
 };

ClassesDescription["QCDO"] =
{
  (* quarks *)
 F[1]  == { SelfConjugate -> False, 
              Indices -> {},
	      Mass -> Mq,
	      PropagatorLabel -> "", 
	      PropagatorType -> Straight, 
	      PropagatorArrow -> Forward }, 

  (* gluons *)
 V[1]  == { SelfConjugate -> True, 
	      Indices -> {},
	      Mass -> 0, 
	      PropagatorLabel -> "", 
	      PropagatorType -> Cycles, 
	      PropagatorArrow -> None },

  (* ghosts *)
 U[1]  == { SelfConjugate -> False, 
	      Indices -> {},
	      Mass -> 0, 
	      PropagatorLabel -> "", 
	      PropagatorType -> Dashing[0.003,0.005],
	      PropagatorArrow -> Forward }
}

(* Couplings:
*)
CouplingMatrices["QCDO"] =
{
  C[ F[1], -F[1], V[1] ]
       == {{1}},
  C[ F[1], -F[1], V[1], V[1] ]
       == {{1}},
  C[ F[1], -F[1], V[1], V[1], V[1] ]
       == {{1}},
  C[ U[1], -U[1], V[1] ]
       == {{1}},
  C[ V[1], V[1], V[1] ]
       == {{1}},
  C[ V[1], V[1], V[1], V[1] ]
       == {{1}}
};

(**)
