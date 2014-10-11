
(* :Title: FeynArts *)

(* :Author: Hagen Eck, Sepp Kueblbeck *)

(* :Summary: Generation of Feynman Graphs and Amplitudes *)

(* :Context: HighEnergyPhysics`FeynArts` *)

(* :Package Version 2.0 *)

(* :Mathematica Version 2.0 *)

(* :History:
	Version 1.0 written 1991 by Hagen Eck and Sepp Kueblbeck.
	Version 2.0 collects all FeynArts-functions in one single
	Context and seperates Graphics routines for "standalone"
	usage.
*)

(* :Table of Contents:
	Part1:	utility functions and symbols (FeynArts/Utility.m)
	Part2:	topological symbols + generation of topologies 
		(Feynarts/Topology.m)
	Part3:	field symbols + insertion functions (FeynArts/Insert.m)
	Part4:	symbols for model definition (Models/*.mod)
	Part5:	couplings + creation of amplitudes (FeynArts/Analytic.m)
	Part6:	draw lists of topologies (FeynArts/Graphics.m)
	Part7:	miscellaneous symbols
	Part8:	Pattern
*)

(* a global symbol to indicate that FeynArts is loaded:
*)
$FeynArts = True;

BeginPackage["HighEnergyPhysics`FeynArts`",
	     {"HighEnergyPhysics`Graphics`",
              "Utilities`FilterOptions`"} ]

	(** Part1: utility functions and symbols (Utility.m) **)

(* option handling:
*)

ActualOptions::usage =
"ActualOptions[symbol, options...] returns a list of options of symbol
with the valid options for symbol replaced by the actual values."

(* general utility functions:
*)

FreeListQ::usage =
"FreeListQ[ expr_, form_List ] tests whether expr is free of all of the 
elements of form.";

MemberListQ::usage =
"MemberListQ[ expr_, form_List ] tests whether any of the elements of form
is an element of expr.";

(* extract information from topologies:
*)

VertexTypes::usage =
"VertexTypes[t] gives the numbers of edges that occur at the vertices of
t as a list of integers.";

Vertices::usage =
"Vertices[t] gives a list of all vertices of topology t.\n 
Vertices[n][t] gives all n-points and Vertices[-n][t] gives all n-points 
with negative index numbers (permutables!). n can be a list of integers, 
too."

PropagatorCount::usage =
"PropagatorCount[t][top] gives the number of propagators of type t in topology
top."

FindFieldPoints::usage = 
"FindFieldPoints[ t,  n ] constructs the vertices at the end of the nth 
propagator of topology t."

ConstructFieldPoints::usage = 
"ConstructFieldPoints[ t ] constructs a list of vertices from topology t.
Every element of this list is of the form Unsorted[ v, {fi..} ] where v is
a vertex name and {fi..} the list of incoming fields of this vertex.";

Unsorted::usage =
"Unsorted[v, l_List ] is an element of the list returned by 
ConstructFieldPoints. v is a vertex name and l is the list of incoming 
fields of that vertex.";

ReferenceCoupling::usage =
"ReferenceCoupling[ coup ] is a function that distributes the momenta and 
kinematic indices of a (vector-, or G[-]-) coupling according to the 
appropriate generic coupling. Stated otherwise: it just reproduces the 
analytical expression for the coupling exactly as defined in the generic 
model file."

CouplingVector::usage =
"CouplingVector[ f1, f2, .. ] gives the coupling vector of the generic
fields f1, f2, .. in an unevaluated form.";

Mom::usage = 
"Mom[n] is a momentum variable. It is part of a Lorentz structure which 
is an index that marks the generic coupling constants."

KI::usage = 
"KI[n] is a kinematic index variable. It is part of a Lorentz structure which 
is an index that marks the generic coupling constants."

(* standard ordering:
*)

PSort::usage =
"PSort[p] sorts the first two elements of a Propagator p."

VSort::usage =
"VSort[v[p1,p2,...]] sorts the elements of a Vertex v only looking
at the first index of p1. This is done because if the momentum
is a sum or a product the Sort may give different results than
VSort."

TSort::usage =
"TSort[t] sorts the elements of the Topology t into canonical order."

SortGeneric::usage =
"SortGeneric[ coup ] sorts a list of fields coup according to the
generic reference order of the current model."

SortClasses::usage =
"SortClasses[ coup ] sorts a list of fields coup according to the reference
order of the current model."

Compare::usage =
"Compare[t] is the `pure' compare function to eliminate equivalent 
topologies of a TopologyList t. "

(* model initialization:
*)

InitializeModel::usage =
"InitializeModel[MOD] initializes the classes model for model description
MOD. A valid model description is either a single string giving the base
name of the classes model file, or a list with a single string as first
element and model restrictions (Rules) for this model as additional
elements. The generic model to initialize can be set with the option
GenericModel."

Reinitialize::usage =
"Reinitialize is an option for InitializeModel. InitializeModel will perform
the initialization of a model that was already initialized (i.e. that equals
$Model) only if Reinitialize is set to True (=default)."

RestrictCurrentModel::usage =
"RestrictCurrentModel[ args ] applies a number of ExcludeVertices and
ExcludeParticles restrictions to the current model. The restrictions may
be given as several arguments or as (nested) lists.\n
RestrictCurrentModel[] removes all currently active restrictions. \n
Note that this doesn't affect the restrictions that were activated when
initializing the model via {\"model\", restrictions}.";

(* model dependend functions (defined by InitializeModel):
*)

PossibleFields::usage =
"PossibleFields[cto][t, fp] gives a list of all fields of generic
type t of the current model that are compatible with the given valence 
structure of a field point fp with counterterm order cto."

CheckFieldPoint::usage = 
"CheckFieldPoint[mod] returns False if it contains a complete in mod and Generic
nonexisting vertex and True otherwise."

AntiParticle::usage =
"AntiParticle[f] is the anti particle of f (what else?)."

TheMass::usage =
"TheMass[p] gives the value of the mass of particle p (if specified in the
model file)."

TheLabel::usage = 
"TheLabel[p] gives the PropagatorLabel of particle p (if specified in the 
model file)."

Indices::usage =
"Indices[c] gives a list of index names of class c."

IndexRange::usage =
"IndexRange[i] gives a list of possible values of index i."

Diagonal::usage =
"Diagonal[v] returns a list of IndexDeltas for a vertex v."

ReferenceOrder::usage =
"ReferenceOrder[x] gives a list of all field points of the current model in 
(unsorted) list form. x can be one of the identifiers Generic or Classes."

FieldPoints::usage =
"FieldPoints[n] gives a list of all field points of order n of the current 
model.\n
FieldPoints[] gives a list of all field points of the current model."

GenericFieldPoints::usage
"GenericFieldPoints[] gives a list of all generic couplings of the current
model."

M$GenericPropagators::usage =
"M$GenericPropagators is the list of propagators and their analytic 
expressions which are allowed in the current generic model 
$GenericModel."

M$GenericCouplings::usage =
"M$GenericCouplings is the list of couplings and their analytic 
expressions which are allowed in the current generic model 
$GenericModel."

ToGeneric::usage =
"ToGeneric[expr] returns expr with all classes and particle fields replaced
by their generic fields. Keep in mind that this procedure removes the signs
of the fields."

ToClasses::usage =
"ToClasses[expr] returns expr with all particle fields replaced by their 
classes fields." ;

	(** Part2: topological symbols + generation of topologies
	    (FeynArts/Topology.m) **)

Topology::usage =
"Topology is the head of a topology data structure.
Topology[s] is the head of a topology with combinatorial factor 1/s."

TopologyList::usage =
"TopologyList is the head of a list of topologies."

Propagator::usage =
"Propagator[ v1, v2 ] is a (undirected) propagator joining the two
vertices v1 and v2.\n
Propagator[ v1, v2, f ] is a (directed) propagator transporting field
f from vertex v1 to v2.\n
Propagator[t][...] is the representation of a propagator of type t.
Possible types are: Incoming, Outgoing, External, Internal and Loop[n]."

Incoming::usage =
"Incoming is a Symbol to denote a type of Propagator."

Outgoing::usage =
"Outgoing is a Symbol to denote a type of Propagator."

External::usage =
"External is a Symbol to denote a type of Propagator."

Internal::usage =
"Internal is a Symbol to denote a type of Propagator."

Loop::usage =
"Loop[n] is a Symbol to denote a type of Propagator."

FieldPoint::usage =
"FieldPoint[n][f1, f2, ...] is the representation of a fieldpoint 
of order n with incoming fields fi in a Feynman diagram"

Vertex::usage =
"Vertex[e][n] is the representation of a vertex with e propagators
in a topology.\n
Vertex[e,o][n] is the representation of a vertex of order o in a
topology."

(* generation of topologies:
*)
CreateTopologies::usage =
"CreateTopologies[ l, i -> o, options ] returns a TopologyList of 
Topologies with i incoming and o outgoing legs and l loops (for the 
momentarily entered starting topologies l=0,1,2). "

ExcludeTopologies::usage =
"ExcludeTopologies is an option of CreateTopologies. Its setting is a list
that can contain any of the symbols: Tadpoles, SelfEnergies, WFCorrections, 
Triangles, AllBoxes, Theta, Eight, Bicycle."

(* remark: for every ExcludeTopologies-option a corresponding function
   $ExcludeTopologies[option][topologylist] has to be defined.
*)

$ExcludeTopologies::usage =
"$ExcludeTopologies[type][top] gives topologylist top without
topologies of a certain type. This function can be defined for 
type to be used as possible value of the option ExcludeTopologies."

(* already defined ExcludeTopologies values:
*)

Tadpoles::usage = 
"Tadpoles is a possible value of the option ExcludeTopologies."

SelfEnergies::usage = 
"SelfEnergies is a possible value of the option ExcludeTopologies."

WFCorrections::usage = 
"WFCorrections is a possible value of the option ExcludeTopologies."

Triangles::usage = 
"Triangles is a possible value of the option ExcludeTopologies."

AllBoxes::usage = 
"AllBoxes is a possible value of the option ExcludeTopologies."

Boxes::usage = 
"Boxes[n] is a possible value of the option ExcludeTopologies."

StartingTopologies::usage =
"StartingTopologies is an option of CreateTopologies. It selects the
starting topologies out of a given set of topologies for #loops > 1."

StartTop::usage =
"StartTop[ l, o ] is the list of starting topologies for topologies with
l loops and counterterm order o. It is defined in the file Topology.m."

(* remark: for every StartingTopology-Option the corresponding topologies
   have to be defined in Topology.m
*)
Theta::usage = 
"Theta is a possible value of the option StartingTopologies (2loop)."

Eight::usage =
"Eight is a possible value of the option StartingTopologies (2loop)."

Bicycle::usage =
"Bicycle is a possible value of the option StartingTopologies (2loop)."

Three::usage =
"Three[n] is a possible value of the option StartingTopologies (3loop,
irreducible, n=1..8)"

ThreeRed::usage =
"Three[n] is a possible value of the option StartingTopologies (3loop,
reducible, n=1..7)"

CountertermOrder::usage =
"CountertermOrder is an option of CreateTopologies. It selects a
set of starting topologies with a certain counterterm order."

Adjacencies::usage =
"Adjacencies is an option for CreateTopologies. Its setting is a
list {e1, e2, ...} that contains integers (ei>2) that are the possible 
numbers of propagators ending at one vertex. The case of e=1 is always
possible (external fields) and e=2 is controlled via the option
TwoVertices."

CreateCTTopologies::usage =
"CreateCTTopologies[ c, i -> o] returns a TopologyList of counterterm
topologies of order c."

StopOnError::usage =
"StopOnError is an option for CreateCTTopologies."

	(** Part3: field symbols, insertion functions 
	    (FeynArts/Insert.m) **)

(* field symbols:
*)
Field::usage =
"Field[n] is the general specification of the nth field in a Feynman graph."

F::usage =
"F is a fermion field."

S::usage =
"S is a scalar field."

SV::usage =
"SV is a mixed scalar vector field. Like for ordinary fields different mixing
fields must be called different in different model files." 

VS::usage =
"VS is a mixed scalar vector field. Like for ordinary fields different mixing
fields must be called different in different model files."

U::usage =
"U is a ghost field (Grassman valued scalar field)."

V::usage =
"V is a vector boson field."

(* insertion functions:
*)
InsertFields::usage =
"InsertFields[ top, fi ] constructs all Feynman-diagrams of a Topology or
a TopologyList top with a number of fields given in fi. fi either specifies
external fields in the form {in1, in2,..}->{out1, out2,..} or arbitrary
fields of the topology by the option FieldSpecification."

FieldSpecification::usage =
"FieldSpecification is an option for InsertFields. For a Topology its setting
has the form {Field[n1]->fi1, Field[n2]->fi2, ...}, for a TopologyList with
length n a list of n such objects. The integers n1, n2, .. are the positions
of the propagators in the standard ordering of the topology."

ExcludeParticles::usage =
"ExcludeParticles is an option of InsertFields. Exclusion of a particle
always implies exclusion of its antipartitcle."

ExcludeFieldPoints::usage =
"ExcludeFieldPoints is an option of InsertFields. Exclusion of a vertex always
implies exclusion of the corresponding charge conjugate vertex.
It is only legitimate to exclude Class vertices or Particle vertices
(which may have indices), but no admixtures."

Restrictions::usage =
"Restrictions is an option of InsertFields. It contains shorthands to exclude
vertices or particles defined in the corresponding model file."

RemoveEmptyTops::usage =
"RemoveEmptyTops is an Option for InsertFields. The result of InsertFields
will contain no Topologies without possible insertions."

LastSelections::usage =
"LastSelections is an Option for InsertFields. It selects the Insertions 
that do/do not contain the given Symbols."

Insertions::usage =
"Insertions is the head of an insertion list. Usually it is the last element
of an inserted topology returned by InsertFields."

Graph::usage =
"Graph is the Head of a list of Field-replacement rules. Usually it is an
element of an insertions list returned by InsertFields."

(* to visualize distribution of momenta:
*)
InsertMomenta::usage =
"InsertMomenta[t] returns the topology or topology list t with momenta
appended to the propagators as in CreateFeynAmp. \n
InsertMomenta[t, m] uses the list m for insertion of momenta as done
by the Option Momenta of CreateFeynAmp."

(* for internal usage mainly:
 *)
SupplyMomenta::usage =
"SupplyMomenta[t] returns a list { tm, q } where tm is topology t with
momenta appended to the propagators and q is the list of integration
momenta of the corresponding amplitude."

(* model and process information for InsertFields and CreateFeynAmp 
*)
InsertionLevel::usage =
"InsertionLevel is an option for InsertFields and CreateFeynAmp. Possible
settings are Generic, Classes (=default) or Particles meaning \"down to\" 
this level. To pick certain levels put them in a list (Mathematica level
specification). CreateFeynAmp uses the level of InsertFields as default."

Generic::usage =
"Generic is a symbol to denote the generic (general field types) level of
insertion."

Classes::usage =
"Classes is a symbol to denote the classes (multiplets) level of insertion.
Classes[\"mod\"] is the list of Classes of model mod."

Particles::usage =
"Particles is a symbol to denote the particles (members of classes) level
of insertion."

Model::usage =
"Model is an option for InsertFields. The Model-specification is a list of
strings that are to be used simultaneously in this generation process (this 
have to be compatible models). FeynArts reads the model information from
the files ../Models/FOO.mod where \"FOO\" is one of the model strings."

GenericModel::usage =
"GenericModel is an option for InsertFields and InitializeModel. 
Its setting is a string \"GEN\", denoting the generic model file 
../Models/GEN.gen to be loaded. Default for generic models is \"Lorentz\"."

$ModelFiles::usage =
"$ModelFiles[] is a list of files matching \"*.mod\" which are assumed to
contain model information."

$GenericFiles::usage =
"$GenericFiles[] is a list of files matching \"*.gen\" which are assumed to
contain generic model information."

ProcessName::usage =
"ProcessName is an option for InsertFields and carries the information of
the user-defined name for a special generation process."

Process::usage =
"Process is a Symbol to carry the information about external fields in a
special generation process."

GraphName::usage =
"GraphName is the Head of a data structure that identifies a single
graph in a process. Usually it is an element of a FeynAmp-expression."

	(* Part4: Symbols for model definition
	   (FeynArts/Models/*.mod *)

AnalyticalCoupling::usage =
"AnalyticalCoupling[vertex] gives the analytical expression for a
vertex defined in a generic model file."

AnalyticalPropagator::usage =
"AnalyticalPropagator[type][field] gives the analytical expression for a
propagator defined in a generic model file."

M$ClassesDescription::usage =
"M$ClassesDescription contains the classes of the current model and their
properties. They are defined in the files ./Models/x.MOD ."

M$CouplingMatrices::usage =
"M$CouplingMatrices contains the explicit coupling matrices of the current
model. It is defined in the files ./Models/x.MOD ."

KinematicIndices::usage =
"KinematicIndices[ X ] gives the list of kinematic indices of generic field
X. It is defined in a generic model file. Example: Models/Weyl.gen contains
te definition KinematicIndices[F] = {WeylIndex}. ";

MatrixTraceFactor::usage =
"MatrixTraceFactor -> n is an optional entry in the class definition
of fermions. \n
CreateFeynAmp will multiply any MatrixTrace containing at least one of
the classes fields of that entry with n. Note, that every loop is
multiplied by a trace factor only once. Typical usage of this symbol is 
MatrixTraceFactor -> 3 for quarks.\n
In the classes or particles amplitude this factor multiplies the
symbol RelativeCF."

SelfConjugate::usage =
"SelfConjugate -> True|False is an entry in the M$ClassesDescription list.
SelfConjugate[p] returns True or False for a class or a particle depending
on the setting in M$ClassesDescription."

InsertOnly::usage =
"InsertOnly is an entry in ClassesDescription and a function returning the
list of allowed propagator types for a Class."

PolarizationVector::usage =
"PolarizationVector[ p_, m_, l_ ] is the polarization vector of particle p
with momentum m and Lorentz index l."

$FermionLines::usage =
"$FermionLines is a FeynArts system constant that can take the values True
or False indicating whether CreateFeynAmp should collect fermion fields F 
in lines or not. The fermion flow of a line is defined to be opposite to
the line. The fermionic couplings are inserted into the lines at the right
positions (between the corresponding propagators) and the NonCommutative
parts of the couplings and propagators appear in FermionChain- (for
external fermions) or MatrixTrace-objects (for internal loops).\n
Fermion line construction has to be turned off for generic models that
contain couplings of four or more fermions (there is no possibility to
break a fermion 4-vertex in two in order to insert that vertex into the
two fermion lines that are constructed through the vertex). In that case the 
fermions should carry a kinematical (e.g. Dirac-) index.\n
Note, that if $FermionLines=False the classes option MatrixTraceFactor won't 
have any effect on fermionic classes and that there are no additional signs 
for odd permutations of external fermions."

Mass::usage =
"Mass[p] denotes the mass of particle p. It is just a symbol carrying
no further information. FeynArts defines the function TheMass that returns
the explicit `value' of the mass."

$SVMixing::usage =
"$SVMixing is a FeynArts system constant that can take the values True 
or False indicating whether mixing of scalar fields and vector bosons 
is or is not allowed. Usually it is set in the file Setup.m."

$CounterTerms::usage =
"$CounterTerms is a FeynArts system constant. If its value is False FeynArts 
won't initialize counterterm couplings during InitializeModel and
InsertFields will not use any higher order couplings. Usually $Counterterms
is set in the file Setup.m. Note that if you want to turn on the counterterm 
couplings during a FeynArts session where $CounterTerms was set to False at
startup, you have to re-initialize your model with InitializeModel.";

MixingPartners::usage =
"MixingPartners specifies the partners in a mixing propagator."

LeftPartner::usage =
"LeftPartner[mixer] gives the left hand side part of the mixing propagator 
mixer running from left to right."

RightPartner::usage =
"RightPartner[mixer] gives the right hand side part of the mixing propagator 
mixer running from left to right."

AllMixers::usage =
"AllMixers[part] gives all mixers the right side of which is the particle part."

Index::usage =
"Index is the Head of an index name (i.e. Index[Generation])."

IndexDelta::usage =
"IndexDelta[ i1, i2 ] is a symbol in the definition of a Classes-coupling
that indicates that the coupling is diagonal in the indices i1 and i2.";

TheC::usage =
"TheC gives the values for the coupling strengths."

C::usage =
"C is a symbol to denote the coupling strength formally (see TheC)."

G::usage =
"G[sym][c][fi...][expr] is a generic coupling matrix of counterterm order c
with fields fi specified by the kinematical expression expr. If sym = 1,
then G is symmetric, if sym = -1 G then G is antisymmetric."

PV::usage =
"PV is the head of a general analytical expression in FeynArts. The letters
stand for Propagator/Vertex."

M$TruncationRules::usage =
"M$TruncationRules is a set of rules that is applied by CreateFeynAmp if
Truncated->True. It is defined in the generic model file."

M$FermionFlipRule::usage =
"M$FermionFlipRule[ map ] is a function defined in the generic model file that
returns a list of rules that are applied to a fermionic coupling for a 
certain mapping map. Exchanging the first and the second fermion in
a coupling could be handled by FermionFlipRule[1->2,2->1,__]."

M$LastModelRules::usage =
"M$LastModelRules is a set of rules that is applied by CreateFeynAmp 
before it returns the amplitudes and before the M$LastGenericRules are applied. 
It is defined in the model file and can contain mappings of symbols to certain 
Contexts or to special symbols of other packages."

M$LastGenericRules::usage =
"M$LastGenericRules is a set of rules that is applied by CreateFeynAmp before 
it returns the amplitudes and after the LastModelRules are applied. It is 
defined in the generic model file and can contain mappings of symbols to 
certain Contexts or to special symbols of other packages."

	(** Part5: couplings, creation of amplitudes 
	    (FeynArts/Analytic.m) **)

CreateFeynAmp::usage =
"CreateFeynAmp[ t ] creates a list of Feynman amplitudes (Head: FeynAmpList)
of all insertions of Topology or TopologyList t."

AmplitudeLevel::usage =
"AmplitudeLevel is an option for CreateFeynAmp."

GaugeRules::usage =
"GaugeRules is an option of CreateFeynAmp." 

PreFactor::usage =
"PreFactor is an option of CreateFeynAmp."

LoopNr::usage =
"LoopNr is a symbol of FeynArts that can be used in the option PreFactor of
CreateFeynAmp.";

UseModel::usage =
"UseModel is an option of CreateFeynAmp. It applies only, if there is no
Model or GenericModel entry in the Head of a TopologyList or a Topology.
Its setting is a list {MOD, genMOD} where MOD is a list of model names
(strings) and genMOD a string.";

Truncated::usage =
"Truncated is an option of CreateFeynAmp that determines whether to 
apply the TruncationRules of the current generic model (default=False)."
 
HoldTimes::usage =
"HoldTimes is an option of CreateFeynAmp that determines whether to 
multiply Feynman rules using Mult (which does not perform the multiplication)
or Times."

Momenta::usage =
"Momenta is an option for CreateFeynAmp. E.g. { 1 -> q[1], 7 -> q[2] } sets 
the momentum of propagator 1 equal to q[1] and the momentum of propagator 7 
equal to q[2].  Use this option with care. If you set the momenta inproperly 
FeynArts cannot solve the equations for momentum conservation."

PickLevel::usage =
"PickLevel[ lev ][ amp ] picks out the amplitudes of level lev from the list
of generic amplitudes amp. The levels can be Generic, Classes or Particles.\n
PickLevel[ lev ][ t ] picks out the diagrams of level lev from topology list
t. The levels can be combined to lists, too. The symbol All can be used as
an abbreviation for {Generic, Classes, Particles}."

Coupling::usage =
"Coupling[ f1, f2, ... ] returns the analytical expression for the coupling
of fields f1, f2, ... in the current model in readable form. The fields may
be of any level. If momenta and kinematic indices are not given explicitely
FeynArts uses the symbols p1, p2, ... and mu1, mu2, ... . The rules of
CouplingRule are applied to the result. \n
On the generic level, the \"indices\" of the elements of the G-vector
are expressions that may contain momenta and kinematic indices. Those are
represented by Mom[n] and KIx[n], where n gives the position of the
corresponding field in the original generic coupling and x distinguishes
the kinematic indeces of a field. E.g. Mom[3] is the momentum of
the 3rd field of the generic coupling definition and KI2[1] is the second
kinematic index of the first field.";

CouplingRule::usage =
"CouplingRule give replacement rules for the symbols of a coupling for the
output of the function Coupling.";

GtoCRules::usage =
"GtoCRules[] generates a list of replacement rules for the coupling
vectors (with head G) of an expression. This makes sense only if it 
is applied to an expression as in: expr /. GtoCRules[]."

(* output symbols:
*)
FeynAmp::usage =
"FeynAmp is the Head of a data structure that represents the analytical
expression that belongs to one single Feynman graph. Its members are:
graph identification, list of integration momenta, analytical expression of
the amplitude (usually in a nested form)."

FeynAmpList::usage =
"FeynAmpList is the Head of a collection of Feynman amplitudes (all of
them with Head FeynAmp) that is returned by the function CreateFeynAmp."

Integral::usage =
"Integral[q___] is a member of a FeynAmp data structure and contains the
list of integration momenta of the amplitude (no elements for tree graphs)."

PropagatorDenominator::usage =
"PropagatorDenominator[ p, m ] stands for the expression 1/(p^2-m^2)
that is contained in internal propagators of a Feynman graph."

FeynAmpDenominator::usage =
"FeynAmpDenominator[ d1, d2, .. ] collects PropagatorDenominators d1, d2, ...
of a Feynmangraph."

GaugeXi::usage =
"GaugeXi[s_Symbol] is a gauge parameter with index s."

RelativeCF::usage =
"RelativeCF is the relative combinatorial factor of a graph with respect
to the generic graph it was created from. This symbol appears in generic
amplitudes and its value is defined for every Classes or Particles insertion.
RelativeCF also contains additional MatrixTraceFactors (if any)."

	(** Part6: draw lists of topologies (FeynArts/Graphics.m) **)

(* TopologyGraphics function and Options:
*)
TopologyGraphics::usage =
"TopologyGraphics[ t ] returns a graphics object for Topology t. The 
information about the coordinates of the vertices of the topology is obtained
from the directory specified by the option TopologyDataDir."

AutoShape::usage =
"AutoShape is an option for TopologyGraphics and Paint."

GraphFrame::usage =
"GraphFrame is an option for TopologyGraphics."

GraphLabel::usage =
"GraphLabel is an option for TopologyGraphics and Paint."

GraphLabelPosition::usage =
"GraphLabelPosition is an Option for TopologyGraphics."

ShowGrid::usage =
"ShowGrid is an option for TopologyGraphics. ShowGrid -> True draws a grid
of size 20x20 as background. This option is used by TopologyShaping."

FlipProps::usage =
"FlipProps is an option for TopologyGraphics and give a list of
Propagator numbers that are to be reversed before drawing. This option
is usually set by Paint for the Outgoing Propagators.";

ShowPointNames::usage =
"ShowPointNames is an options for TopologyGraphics. ShowPointNames->True
writes a String \"n/m\" near every Vertex[n][m]."

StandardEndpoints::usage =
"StandardEndpoints is an option for TopologyGraphics. If its setting is
True and TopologyGraphics finds a GraphInfo function without any Endpoint
specifications then it sets vertex dots at internal points and no dots 
at external points."

TopologyDataDir::usage =
"TopologyDataDir is an option for TopologyGraphics and Shape.
Usually its setting is $TopologyDataDir. Its settings have to be Context
specifications."

(* topologycal information:
*)
GraphInfo::usage =
"GraphInfo[t, dir] returns the topological information for topology t (if 
possible). This information is stored in the function GraphPoints in a 
file in directory dir.";

(* the editor:
*)
Shape::usage =
"Shape[t] is a simple interactive editor for (bare or inserted) Topologies.";

(* Paint and its options:
*)
Paint::usage = 
"Paint[toplist] draws a list of inserted or bare topologies.";

$DefaultLabel::usage =
"$DefaultLabel gives the default graph label for Paint. Usually it is a
ComposedChar-construct. $DefaultLabel can contain the following symbols
which are replaced by their actual values: ProcessName (if Paint-ing a
TopologyList containing this information), Topology, Generic, Classes,
Particles, Number.";

Destination::usage =
"Destination is an option for Paint. Possible values are Screen (default),
File or All."

DisplayMode::usage =
"DisplayMode is an option for Paint. Possible values are Page, Graph and All."

GraphSpacing::usage =
"GraphSpacing is an option for Paint."

ColumnsXRows::usage =
"ColumnsXRows (\"columns times rows\") is an Option for Paint. Its setting
can be a pair of numbers {m, n} or a single integer number n which stands
for {n, n}.";

FileBaseName::usage =
"FileBaseName is an option for Paint. It can contain the symbol ProcessName.";

FontScaling::usage =
"FontScaling is an option for Paint."

Screen::usage =
"Screen is a possible value of the option Destination."

Page::usage =
"Page is a possible value of the option DisplayMode."

DoNotStop::usage =
"DoNotStop is an option for Paint. Set to True, Paint replaces all occurences
of $Aborted by an (nearly) empty place and goes on drawing topologies."

PaintLevel::usage =
"PaintLevel is an option for Paint."

	(** Part7: miscellaneous symbols **)

(* special algebraical symbols:
*)
FourMomentum::usage =
"FourMomentum[ s_Symbol, n_Integer ] is the nth momentum of type s.
Allowed types are Incoming, Outgoing, External and Internal."

Flip::usage =
"Flip is the head of a flipped vertex i.e. a vertex with reversed
orientation of fermion flow."

Mult::usage =
"Mult is the FeynArts symbol for a not executed multiplication (Hold[Times]).
It is useful for backtracing factors in Feynman rules. It is switched on by
HoldTimes -> True. (Default ist HoldTimes -> False.)"

NonCommutative::usage =
"NonCommutative is the head of a list of noncommuting objects in
a Feynman rule."

MatrixTrace::usage =
"MatrixTrace is the head of a trace of noncommuting objects (i.e. of 
Symbols with Head NonCommutative in the Feynman rules) in closed 
fermion loops."

FermionChain::usage =
"FermionChain is the head of a trace of noncommuting objects (i.e. of 
Symbols with Head NonCommutative in the Feynman rules) in open 
fermion chains."

(* number to string conversion:
*)
Alph::usage =
"Alph[n] gives the nth lowercase letter (n=1..26)."

UCAlph::usage =
"UCAlph[n] gives the nth uppercase letter (n=1..26)."

Greek::usage =
"Greek[n] gives the nth lowercase greek letter as a SymbolChar (n=1..23)."

UCGreek::usage =
"UCGreek[n] gives the nth uppercase greek letter as a SymbolChar (n=1..23)."

(* i/o-symbols:
*)
$Verbose::usage =
"$Verbose is a integer number that determines the extent of run-time 
messages in FeynArts. It ranges from 0 (no messages) to 3 (debug level)."

FAPrint::usage =
"FAPrint[ l_Integer, s_String ] prints String s if printing level l is
smaller or equal than $Verbose."

FeynArtsForm::usage =
"FeynArtsForm[ expr ] defines the FeynArts output form of expr. It can be
used as a value for $PrePrint. If set it gives a feyn output form.";

(* FeynArts system constants:
*)

$NVertices::usage =
"$NVertices is a list of integers denoting the field point types currently
allowed in FeynArts."

$ModelPath::usage =
"$ModelPath gives a list of directories that are assumed to contain model
files.";

(* -> OBSOLETE *)
$InitializedModels::usage =
"$InitializedModels is a list of all models that were initialized during the
session."

(* -> OBSOLETE ( replaced by $GenericModel ) *)
$CurrentGenericModel::usage =
"$InitializedGenericModel is the currently initialized generic model."

(* -> OBSOLETE ( replaced by $Model ) *)
$ModelName::usage =
"$ModelName is the name of the currently used classes model. It is also 
the identifier of the corresponding ClassesDescription and CouplingMatrices 
lists."

$Model::usage =
"$Model is the name of the currently used classes model. It is also 
the identifier of the corresponding ClassesDescription, CouplingMatrices,
and LastModelRules lists."

$GenericModel::usage =
"$InitializedGenericModel is the currently initialized generic model." 

$CurrentModel::usage =
"$CurrentModel is a list that stores information about the currently used
Model."

CurrentModel::usage =
"CurrentModel[] prints status information about the currently used model."

$TopologyDataDir::usage =
"$TopologyDataDir is a symbol that denotes the directory containing 
information about topology coordinates."

F$Generic::usage =
"F$Generic gives the list of generic fields of the current model.
Its content may chenge with every call of RestrictCurrentModel."

F$Classes::usage =
"F$Classes gives the list of classes fields of the current model.
Its content may chenge with every call of RestrictCurrentModel."

F$Particles::usage =
"F$Particles gives the list of particles fields of the current model.
Its content may chenge with every call of RestrictCurrentModel."

F$AllGeneric::usage =
"F$AllGeneric gives the list of generic fields of the current model."

F$AllClasses::usage =
"F$AllClasses gives the list of classes fields of the current model."

F$AllParticles::usage =
"F$AllParticles gives the list of particles fields of the current model."

F$AllowedFields::usage =
"F$AllwedFields is the list of all fields on all three levels that are
in the current model. This list is affected by calls of the function
RestrictCurrentModel."

L$CTOrders::usage =
"L$CTOrders is the list of counterterm orders of the current model.";

(* a flag:
*)
$Undefined::usage =
"$Undefined is a symbol of FeynArts.";

(* for debugging:
*)
DebugLevel=0;
FieldRules = {
s_. (gen:F|S|V|U|VS|SV)[n_Integer,___] :> 
     SequenceForm[If[s===-1,"-",""],gen, n],
s_. (gen:F|S|V|U|VS|SV)[Index[Generic,n_Integer],___] :> 
     SequenceForm[If[s===-1,"-",""],gen,"_", n]
};
DebugRule ={
  Propagator[any_] :> "P",
  Vertex[e_][n_] :> SequenceForm["v",e,"|",n],
  Vertex[e_,o_][n_] :> SequenceForm["c",o,"|",e,"|",n],
  HighEnergyPhysics`FeynArts`Analytic`gmE->"gmE", 
  HighEnergyPhysics`FeynArts`Analytic`gmI->"gmI",
  HighEnergyPhysics`FeynArts`Utilities`PV -> "PV",
  Propagator[ty_][ fr_, to_, part:(_. _[___]) ] :>
    ty[fr[[1]],to[[1]],part /. FieldRules ],
  HighEnergyPhysics`FeynArts`Analytic`tr -> "tr",
  HighEnergyPhysics`FeynArts`Analytic`dot -> "dot",
  Literal[HighEnergyPhysics`FeynArts`Analytic`hold] -> "hold",
  Index[Generic,i_Integer]:>"I"[i],
  Unsorted[ v_, li_ ] :> 
   (SequenceForm["v",v[[1]]])[  li /. FieldRules ],
  Global`FourVector[ External, n_ ] :> "x"[n],
  Global`FourVector[ Incoming, n_ ] :> "p"[n],
  Global`FourVector[ Outgoing, n_ ] :> "k"[n],
  Global`FourVector[ Internal, n_ ] :> "q"[n],
  FourMomentum[ External, n_ ] :> "x"[n],
  FourMomentum[ Incoming, n_ ] :> "p"[n],
  FourMomentum[ Outgoing, n_ ] :> "k"[n],
  FourMomentum[ Internal, n_ ] :> "q"[n],
  PropagatorDenominator->"Denom",
  Global`ChiralityProjector->"om",
  NonCommutative->"NC",
  Global`GluonPropagator->"gluon",
  Global`QuarkPropagator->"quark",
  Global`GhostPropagator->"ghost",
  Global`QuarkGluonVertex->"qqg",
  Global`GluonVertex->"gg",
  Global`GluonGhostVertex->"uug",
  Index[Lorentz,i_Integer]:>SequenceForm["li",i],
  Index[Color,i_Integer]:>SequenceForm["ci",i],
  HighEnergyPhysics`FeynArts`Utilities`ct$o -> "ct",
  HighEnergyPhysics`FeynArts`Utilities`PV -> "PV",
  HighEnergyPhysics`FeynArts`Utilities`ACoup -> "AC"
};
DePrint[s_String, expr__ ] :=
Block[{a=DebugLevel},
     If[ DebugLevel>0, Print[s, Sequence@@ ({expr}/.DebugRule) ] ];
     If[ DebugLevel>1,
         While[ (a=InputString["-------------- ? "])===EndOfFile ] ];
     Which[ a==="0", DebugLevel=0,
            a==="1", DebugLevel=1
          ]
     ];

FATime = SessionTime[];
StartTimer[] := (FATime = SessionTime[]; WriteString["stdout","\n"]);
TimeDiff[] := Block[{thetime=FATime}, WriteString["stdout", 
                    ToString[ N[ (FATime=SessionTime[])-thetime, 4 ] ] ] ];

	(** Part8: Patterns **)

P$Model::usage =
"P$Model is the pattern for a valid model description."

P$Generic::usage =
"P$Generic is the pattern for generic fields."

P$Class::usage =
"P$Class is the pattern for fields on the classes level."

P$ClassMember::usage =
"P$ClassMember is the pattern for fields on the particles level."

P$Particle::usage =
"P$Particle is the pattern for fields on the particles level."

Begin["`Private`"]

	(** FeynArts symbols' Attributes **)

(* Sometimes, Mathematica can't find the function SetAttributes, and
   uses HighEnergyPhysics`FeynArts`Private`SetAttributes instead
   ( even System`SetAttributes` doesn't work ! )
*)

If[ Context[SetAttributes] =!= "System`",
    Print["  -- ALERT! --"];
    Print["I can't find the function System`SetAttributes."];
    Print["(maybe you're under DEC OSF/1 and this is \"init.m\""];
    Print["Please load FeynArts from the Mathematica prompt."];
    Print[" "];
    Print[" -- FeynArts won't work --"];
    Abort[]
  ];

SetAttributes[FieldPoint,Orderless]; 

SetAttributes[TopologyList, Flat];

SetAttributes[Mult, Flat];

SetAttributes[$NVertices, Protected];

Mult[ a___, 1, b__ ] := Mult[ a, b ];
Mult[ a__, 1, b___ ] := Mult[ a, b ];

	(** FeynArts Output format **)

FeynArtsForm[ a___ ] := a;

	(** FAPrint **)

FAPrint::nolevel = "printing level `1` is not an integer of range 0..3.";

FAPrint[ l_Integer, s__ ] :=
  If[ l <= $Verbose, Print[ s ] ] /; 0 <= l <= 3;

FAPrint[ l_, s__ ] := Message[ FAPrint::nolevel, l ];

	(** setting FA-constants **)

$Verbose = 2;

Unprotect[$NVertices]; $NVertices = {2, 3, 4}; Protect[$NVertices];

(* under Windows, Mathematica can't change to directories if there
   is a "\\" at the end of it.
*)
If[ StringMatchQ[$System, "*Windows*"],
    $Path = Union[ Join[ $Path,
                        If[ StringMatchQ[ #, "*\\" ], 
                            StringDrop[ #, -1 ], 
                            # ]& /@ $Path             ] ]
  ];

(* constructing the path to the model files:
*)
$ModelPath = Union[ 
   FileNames[ {"*HighEner*"}, $Path /. "." -> Directory[] ] ];

$ModelPath = ( # <> $PathnameSeparator <> "Models" )& /@ $ModelPath;

$ModelFiles[] := Block[{l},
			Off[Filenames::cdir];
		        l = FileNames[{"*.mod"}, $ModelPath ];
			On[Filenames::cdir];
			Return[l]
		      ];
$GenericFiles[] := Block[{l},
			Off[Filenames::cdir];
		        l = FileNames[{"*.gen"}, $ModelPath ];
			On[Filenames::cdir];
			Return[l]
		      ];

If[ $ModelFiles[] === {},
    FAPrint[1, "Can't find any model files!"] ];
If[ $GenericFiles[] === {},
    FAPrint[1, "Can't find any generic files!"] ];
    
$SVMixing = False;

$CounterTerms = True;

$FermionLines = True;

$InitializedModels = {};

$FeynmanRulesType = "";

$GenericModel = "";

$Model = "";

$CurrentModel = {};

$PrePrint = FeynArtsForm;

$TopologyDataDir = "HighEnergyPhysics`Graphics`DataDir`";

(* Setting the values of the FeynArts-patterns:
*)

(* a model is either a string or a list containing the model string and
   some (hopefully) valid model restrictions as Rules
*)
P$Model = (_String)|({ _String, (__Rule)|({__Rule}) });

(* the field patterns. keep in mind that these patterns do not match 
   fields containing momenta or kinematical indices.
*)
P$Generic  = (F|S|V|U|VS|SV);
P$Class    = _. P$Generic[_Integer];
P$ClassMember = _. P$Generic[_Integer, {__}];
P$Particle = (_. P$Generic[_Integer, {__}])|P$Class;

End[] (* HighEnergyPhysics`FeynArts`Private` *)

	(** read function definitions: **)

Get["HighEnergyPhysics`FeynArts`Utilities`"];
Get["HighEnergyPhysics`FeynArts`Initialize`"];
Get["HighEnergyPhysics`FeynArts`Coupling`"];
Get["HighEnergyPhysics`FeynArts`Momenta`"];
Get["HighEnergyPhysics`FeynArts`Topology`"];
Get["HighEnergyPhysics`FeynArts`Insert`"];
Get["HighEnergyPhysics`FeynArts`Analytic`"];
Get["HighEnergyPhysics`FeynArts`Graphics`"];

EndPackage[] (* HighEnergyPhysics`FeynArts` *)

If[ Length[ FileNames[ "SetUp.m", $Path, 2 ] ] > 0,
    Get["HighEnergyPhysics`SetUp`"]
  ];

FAPrint[1," -- FeynArts 2.1 loaded --"];

Null
