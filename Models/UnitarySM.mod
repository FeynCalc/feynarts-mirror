(*
	UnitarySM.mod
		SM.mod in unitary gauge
		last modified 15 Jan 07 by Thomas Hahn
*)


ReadModelFile["SM.mod"]

M$CouplingMatrices = Select[M$CouplingMatrices,
  FreeQ[#[[1]], U | S[-3 | 2 | 3]]&]

AppendTo[M$LastModelRules, GaugePart -> gaugePart]

gaugePart[_, 0, _] = 0

gaugePart[0, Infinity, _] = 0		(* this is for unitary gauge *)

gaugePart[m_, Infinity, _] := 1/m^2	(* this is for unitary gauge *)

gaugePart[m_, xi_, p_] := (1 - xi) PropagatorDenominator[p, Sqrt[xi] m]

