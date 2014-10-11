
(* FeynArts startup file
*)

(* filter for counterterm tadpoles:
*)
$ExcludeTopologies[CTTadpoles][ t_ ] :=
  Select[ t, FreeQ[#, Vertex[1, _Integer?Positive][_] ]& ];

(* path to model files:
*)
(*
$ModelPath = { "/users/hagen/develop/Models" };
*)

Format[ Continuation[_] ] := "    ";
