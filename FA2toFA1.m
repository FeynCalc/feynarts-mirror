FA1fourvector[a_,b_]:=FourVector[a /. Momentum -> Identity,b];

FA1scalarproduct[a__]:=ScalarProduct[a] /. Momentum -> Identity;

FA1fourmomentum[Incoming,i_Integer]:=
     Momentum[ToExpression["p"<>ToString[i]]];
FA1fourmomentum[Outgoing,i_Integer]:=
     Momentum[ToExpression["k"<>ToString[i]]];
FA1fourmomentum[Internal,i_Integer]:=
     Momentum[ToExpression["q"<>ToString[i]]];

Unprotect[Conjugate];
Conjugate[FA1polarizationvector][_,p_,li_]:=
  Conjugate[PolarizationVector[p /. Momentum -> Identity,li]];
Protect[Conjugate];
FA1polarizationvector[_,p_,li_]:=
  PolarizationVector[p /. Momentum -> Identity,li];
FA1polarizationvector[a_,b_]:=PolarizationVector[a,b];

FA1DiracSpinor[p_,m_,c___] := Spinor[p /. Momentum -> Identity, m];

FA1index[Lorentz,a_]:=ToExpression["li"<>ToString[a]];
FA1index[a_,b_]:=Index[a,b];

FA1feynamp[a_,c_]:=FeynAmp[a,c];
FA1feynamp[a_,Integral[],c_]:=FeynAmp[a,c];	(* tree graphs *)
FA1feynamp[a_,b_,c_]:=FeynAmp[a,b /. Momentum -> Identity /.
  Integral -> Identity,c];

FA1propagatordenominator[p_,m_]:=
  PropagatorDenominator[p /. Momentum -> Identity, m];

FA1DiracSlash[x__] := DiracSlash[x /. Momentum->Identity];

FA2toFA1[expr_]:= expr //.
  FourMomentum -> FA1fourmomentum //.
  FourVector -> FA1fourvector //.
  ScalarProduct -> FA1scalarproduct //.
  PolarizationVector -> FA1polarizationvector //.
  DiracSpinor -> FA1DiracSpinor //.
  Index -> FA1index //.
  PropagatorDenominator -> FA1propagatordenominator //.
  FeynAmp -> FA1feynamp /.
  MatrixTrace -> DiracTrace /.
  DiracSlash -> FA1DiracSlash /.
  MLE[__] -> ME /. MQU[__] -> MU /. MQD[__] -> MD /.
  IndexDelta[Index[Generation,_],Index[Generation,_]] -> 1 /.
  (Number == i_) :> ToExpression["N"<>ToString[i]] /.  (* cosmetic changes *)
  (Topology == j_) :> ToExpression["T"<>ToString[j]] /.
  (Generic == _) -> 0 /.
  (Classes == _) -> 0 /.
  (Particles == _) -> 0 //.
  GraphName[a___,0,b___] -> GraphName[a,b] /.
  GraphName[a_,b_,c_] -> GraphName[a,b,I1,c];

FA1pl2li[p_]:=List@@ p /; Head[p]===Plus;
FA1pl2li[o_]:=List[o];

SupplyColourFactor[FeynAmp[gn_,q_,amp_]]:=
  FeynAmp[gn,q,SupplyColourFactor[amp]];

SupplyColourFactor[expr_]:=
  expr*3^Length[Cases[Cases[expr,
    PropagatorDenominator[a_,MU|MD] -> FA1pl2li[a],
    Infinity],_[Internal,a_] -> a,Infinity]//Union];

