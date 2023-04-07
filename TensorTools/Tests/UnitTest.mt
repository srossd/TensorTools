(* Wolfram Language Test file *)

Needs["TensorTools`"];

(* Tensor *)

Test[
	IndexData[SpaceTime] = Index[4, "Greek", 12];
	IndexData[Spinor] = Index[2, "Greek", 1];
	IndexData[DottedSpinor] = Index[2, "Greek", 1, OverDot];
	IndexData[SO3] = Index[3, "Latin", 1];
	
	Table[DisplayName[idxtype, i], {idxtype, {SpaceTime, Spinor, SO3}}, {i, 5}],
	
	{{"\[Mu]", "\[Nu]", "\[Xi]", "\[Omicron]", "\[Pi]"}, {"\[Alpha]", "\[Beta]", "\[Gamma]", "\[Delta]", "\[CurlyEpsilon]"}, {"a", "b", "c", "d", "e"}}
	,
	TestID->"UnitTest-20230307-H1F0M4"
]

Test[
	tensorA = Tensor[{{"A", Raised[SpaceTime], Lowered[Spinor], Lowered[DottedSpinor]}}];
	Normal@Components[tensorA],
	
	{{{Component["A"][1, 1, 1], 
   Component["A"][1, 1, 2]}, {Component["A"][1, 2, 1], 
   Component["A"][1, 2, 2]}}, {{Component["A"][2, 1, 1], 
   Component["A"][2, 1, 2]}, {Component["A"][2, 2, 1], 
   Component["A"][2, 2, 2]}}, {{Component["A"][3, 1, 1], 
   Component["A"][3, 1, 2]}, {Component["A"][3, 2, 1], 
   Component["A"][3, 2, 2]}}, {{Component["A"][4, 1, 1], 
   Component["A"][4, 1, 2]}, {Component["A"][4, 2, 1], 
   Component["A"][4, 2, 2]}}}
	,
	TestID->"UnitTest-20230307-K4N6G6"
]

Test[
	tensorB = Tensor[{{"B", Lowered[Spinor], Lowered[Spinor], Lowered[DottedSpinor]}}];
	DeclareTensorSymmetry["B", Symmetric[{1, 2}]];
	Normal@Components[tensorB],
	
	{{{Component["B"][1, 1, 1], 
   Component["B"][1, 1, 2]}, {Component["B"][1, 2, 1], 
   Component["B"][1, 2, 2]}}, {{Component["B"][1, 2, 1], 
   Component["B"][1, 2, 2]}, {Component["B"][2, 2, 1], 
   Component["B"][2, 2, 2]}}}
	,
	TestID->"UnitTest-20230307-J4Q8I6"
]

Test[
	TensorProduct[tensorA, tensorB],

	Tensor[{{"A", Raised[SpaceTime], Lowered[Spinor], Lowered[DottedSpinor]}, {"B", Lowered[Spinor], Lowered[Spinor], Lowered[DottedSpinor]}}]
	,
	TestID->"UnitTest-20230307-J6X3M9"
]

Test[
	{Symbolic[tensorA], Symbolic[tensorB]},
	
	{{{"A", Raised[SpaceTime], Lowered[Spinor], Lowered[DottedSpinor]}}, {{"B", Lowered[Spinor], Lowered[Spinor], Lowered[DottedSpinor]}}}
	,
	TestID->"UnitTest-20230307-I7N9R4"
]

Test[
	{Indices[tensorA], Indices[tensorB]},
	
	{{Raised[SpaceTime], Lowered[Spinor], Lowered[DottedSpinor]}, {Lowered[Spinor], Lowered[Spinor], Lowered[DottedSpinor]}}
	,
	TestID->"UnitTest-20230307-C4G7L4"
]

Test[
	{TensorPermutation[tensorA], TensorPermutation[tensorB]},
	
	{{1, 2, 3}, {1, 2, 3}}
	,
	TestID->"UnitTest-20230307-O4P6E2"
]

Test[
	{ContractedPairs[tensorA], ContractedPairs[tensorB]},
	
	{{}, {}}
	,
	TestID->"UnitTest-20230307-G5E5H0"
]

Test[
	Normal@CanonicallyOrderedComponents[tensorA],
	
	{{{Component["A"][1, 1, 1], Component["A"][2, 1, 1], 
   Component["A"][3, 1, 1], 
   Component["A"][4, 1, 1]}, {Component["A"][1, 2, 1], 
   Component["A"][2, 2, 1], Component["A"][3, 2, 1], 
   Component["A"][4, 2, 1]}}, {{Component["A"][1, 1, 2], 
   Component["A"][2, 1, 2], Component["A"][3, 1, 2], 
   Component["A"][4, 1, 2]}, {Component["A"][1, 2, 2], 
   Component["A"][2, 2, 2], Component["A"][3, 2, 2], 
   Component["A"][4, 2, 2]}}}
	,
	TestID->"UnitTest-20230307-Z0N6J0"
]

(* TensorPermute *)

Test[
	TensorPermute[TensorPermute[tensorB, {2, 1, 3}], {2, 1, 3}],
	
	tensorB
	,
	TestID->"UnitTest-20230307-J5J6G2"
]

(* Contract *)

Test[
	eps = Tensor[{{"\[Epsilon]", Raised[SO3], Raised[SO3], Raised[SO3]}}];
	BuildTensor[{"\[Epsilon]", Raised[SO3], Raised[SO3], Raised[SO3]}] = SparseArray@LeviCivitaTensor[3];
	
	Components@Contract[TensorProduct[eps, eps, eps, eps], {{1, 4}, {2, 7}, {3, 10}, {5, 8}, {9, 11}, {12, 6}}],
	
	-6
	,
	TestID->"UnitTest-20230307-F3I0F8"
]

Test[
	TensorPermute[Contract[TensorProduct[eps, eps], {{3, 4}}], {1, 2, 4, 3}],
	
	Contract[TensorPermute[Tensor[{{"\[Epsilon]", Raised[SO3], Raised[SO3], Raised[SO3]}, {"\[Epsilon]", Raised[SO3], Raised[SO3], Raised[SO3]}}], {1, 2, 3, 4, 6, 5}], {{3, 4}}]
	,
	TestID->"UnitTest-20230307-W6Q0N2"
]

Test[
	TensorPermute[Contract[TensorProduct[eps, eps], {{3, 4}}], {4, 3, 2, 1}],
	
	Contract[TensorPermute[Tensor[{{"\[Epsilon]", Raised[SO3], Raised[SO3], Raised[SO3]}, {"\[Epsilon]", Raised[SO3], Raised[SO3], Raised[SO3]}}], {6, 5, 3, 4, 2, 1}], {{3, 4}}]
	,
	TestID->"UnitTest-20230307-P0H8U5"
]

(* SwapFactors *)

Test[
	SwapFactors[TensorProduct[tensorA, tensorB], 1, 2],
	
	TensorPermute[
	 Tensor[{{"B", Lowered[Spinor], Lowered[Spinor], 
	    Lowered[DottedSpinor]}, {"A", Raised[SpaceTime], Lowered[Spinor], 
	    Lowered[DottedSpinor]}}], {2, 5, 6, 4, 1, 3}]
	,
	TestID->"UnitTest-20230307-T5U8Q0"
]

Test[
	SwapFactors[TensorProduct[eps, tensorA, eps, tensorB], {4, 3, 2, 1}],
	
	TensorPermute[Tensor[{{"B", Lowered[Spinor], Lowered[Spinor], 
    Lowered[DottedSpinor]}, {"\[Epsilon]", Raised[SO3], Raised[SO3], 
    Raised[SO3]}, {"A", Raised[SpaceTime], Lowered[Spinor], 
    Lowered[DottedSpinor]}, {"\[Epsilon]", Raised[SO3], Raised[SO3], 
    Raised[SO3]}}], {2, 8, 9, 10, 11, 12, 7, 1, 3, 4, 5, 6}]
	,
	TestID->"UnitTest-20230307-Z1D5V4"
]

(* SwapIn *)

Test[
	tensorC = Tensor[{{"C", Lowered[Spinor]}}];
	tensorD = 
	  Tensor[{{"D", Lowered[Spinor], Lowered[Spinor], Lowered[Spinor], 
	     Lowered[DottedSpinor]}}];
	SwapIn[TensorProduct[tensorC, tensorB], {1, 2}, tensorD],
	
	Tensor[{{"D", Lowered[Spinor], Lowered[Spinor], Lowered[Spinor], 
   Lowered[DottedSpinor]}}]
	,
	TestID->"UnitTest-20230307-J5Q4K9"
]

Test[
	SwapIn[TensorPermute[TensorProduct[tensorC, tensorB], {2, 1, 3, 4}], {1, 2}, tensorD],
  
  	TensorPermute[Tensor[{{"D", Lowered[Spinor], Lowered[Spinor], Lowered[Spinor], Lowered[DottedSpinor]}}], {2, 1, 3, 4}]
	,
	TestID->"UnitTest-20230307-L1Y8J0"
]

(* NormalOrder *)

Test[
	DeclareAnnihilator["B"];
	tensorB = Tensor[{{"B", Lowered[SO3], Lowered[SO3]}}];
	tensorBd = Tensor[{{"\!\(\*SuperscriptBox[\(B\), \(\[Dagger]\)]\)", Lowered[SO3], Lowered[SO3]}}];
	tensor\[Delta] = Tensor[{{"\[Delta]", Lowered[SO3], Lowered[SO3]}}];
	
	Anticommutator[tensorB, tensorBd] := TensorPermute[TensorProduct[tensor\[Delta], tensor\[Delta]], {1, 4, 2, 3}];
	Anticommutator[tensorB, tensor\[Delta]] := 0;
	state = Contract[TensorProduct[tensorBd, tensorBd, tensorBd], {{2, 3}, {4, 5}, {6, 1}}];
	
	NormalOrder[TensorProduct[tensorB, state]],
	
	-Contract[
   TensorPermute[
    Tensor[{{"\!\(\*SuperscriptBox[\(B\), \(\[Dagger]\)]\)", 
       Lowered[SO3], 
       Lowered[SO3]}, {"\!\(\*SuperscriptBox[\(B\), \(\[Dagger]\)]\)",
        Lowered[SO3], 
       Lowered[SO3]}, {"\!\(\*SuperscriptBox[\(B\), \(\[Dagger]\)]\)",
        Lowered[SO3], Lowered[SO3]}, {"B", Lowered[SO3], 
       Lowered[SO3]}}], {3, 4, 5, 6, 7, 8, 1, 2}], {{4, 5}, {6, 
     7}, {8, 3}}] + 
 Contract[
  TensorPermute[
   Tensor[{{"\!\(\*SuperscriptBox[\(B\), \(\[Dagger]\)]\)", 
      Lowered[SO3], 
      Lowered[SO3]}, {"\!\(\*SuperscriptBox[\(B\), \(\[Dagger]\)]\)", 
      Lowered[SO3], Lowered[SO3]}, {"\[Delta]", Lowered[SO3], 
      Lowered[SO3]}, {"\[Delta]", Lowered[SO3], Lowered[SO3]}}], {3, 
    4, 5, 6, 1, 8, 2, 7}], {{4, 5}, {6, 7}, {8, 3}}] - 
 Contract[
  TensorPermute[
   Tensor[{{"\!\(\*SuperscriptBox[\(B\), \(\[Dagger]\)]\)", 
      Lowered[SO3], Lowered[SO3]}, {"\[Delta]", Lowered[SO3], 
      Lowered[SO3]}, {"\[Delta]", Lowered[SO3], 
      Lowered[SO3]}, {"\!\(\*SuperscriptBox[\(B\), \(\[Dagger]\)]\)", 
      Lowered[SO3], Lowered[SO3]}}], {3, 4, 1, 6, 2, 5, 7, 8}], {{4, 
    5}, {6, 7}, {8, 3}}] + 
 Contract[
  TensorPermute[
   Tensor[{{"\[Delta]", Lowered[SO3], Lowered[SO3]}, {"\[Delta]", 
      Lowered[SO3], 
      Lowered[SO3]}, {"\!\(\*SuperscriptBox[\(B\), \(\[Dagger]\)]\)", 
      Lowered[SO3], 
      Lowered[SO3]}, {"\!\(\*SuperscriptBox[\(B\), \(\[Dagger]\)]\)", 
      Lowered[SO3], Lowered[SO3]}}], {1, 4, 2, 3, 5, 6, 7, 8}], {{4, 
    5}, {6, 7}, {8, 3}}]
	,
	TestID->"UnitTest-20230307-D4B9V2"
]

Test[
	conj = Contract[TensorProduct[tensorB, tensorB, tensorB], {{2, 3}, {4, 5}, {6, 1}}];
	BuildTensor[{"\[Delta]", Lowered[SO3], Lowered[SO3]}] := SparseArray[IdentityMatrix[3]];
	
	CanonicallyOrderedComponents[NormalOrder[TensorProduct[conj, state], "VEV" -> True]],
	
	72
	,
	TestID->"UnitTest-20230307-O0H7S7"
];
	