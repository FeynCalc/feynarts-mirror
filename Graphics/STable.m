
(* :File: HighEnergyPhysics/Graphics/STable.m *)

(* :Contents:
   List of charakter mappings: {symbolstring, ps-symbol, substitute}
   The symbol charakters are put in their FontForm through SymbolChar->
   SpecifySymbolChar. The FontForm contains a $SFont-Symbol as a reminder 
   for the case of $SymbolFont=None.
   Since these mappings depend on the PostScript Symbol font the first two
   entries of each mapping may not be changed. The third entry of every 
   mapping is the substitution string for a symbol character if no Symbol 
   font is present.
*)

STable =
{
 {"alpha", "a", "al"}, 
 {"beta", "b", "b"}, 
 {"gamma","g","g"},
 {"chi","c","x"}, 
 {"delta", "d", "de"}, 
 {"epsilon", "e", "eps"},
 {"varphi","f","fi"}, 
 {"eta","h","eta"},
 {"iota","i","i"},
 {"phi","j","fi"}, 
 {"kappa","k","ka"},
 {"lambda","l","la"},
 {"mu","m","mu"},
 {"nu","n","nu"},
 {"pi","p","pi"},
 {"vartheta","q","th"},
 {"rho","r","ro"},
 {"sigma","s","si"},
 {"tau","t","ta"},
 {"ypsilon","u","yp"},
 {"omega","w","om"},
 {"omicron","w","o"},
 {"xi","x","xi"},
 {"psi","y","ps"},
 {"zeta","z","ze"},
 {"Alpha","A","Al"},
 {"Beta","B","Be"},
 {"Chi","C","X"},
 {"Delta","D","De"},
 {"Gamma","G","Ga"},
 {"theta","J","th"},
 {"Lambda","L","La"},
 {"Pi","P","Pi"},
 {"Phi","F","Ph"},
 {"Theta","Q","Th"},
 {"Sigma","S","Si"},
 {"Rho","R","Ro"},
 {"Tau","T","Ta"},
 {"Ypsilon","U","Yp"},
 {"Omega","W","Om"},
 {"Xi","X","Xi"},
 {"Psi","Y","Ps"},
 {"Zeta","Z","Ze"}
};
(**)
