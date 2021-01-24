BeginPackage["RichardsonExtrapolation`"];

Unprotect["RichardsonExtrapolateion`*"];
ClearAll["RichardsonExtrapolation`*", "RichardsonExtrapolation`Private`*"];

RichardsonExtrapolate::usage = "RichardsonExtrapolate[series, orderN] find the limit of the series by using Richardson extrapolation with the last orderN elements of the series."


Begin["`Private`"];

RichardsonExtrapolate[series_, orderN_] := Block[{n},
	n = Length@series;
	If[orderN>n, Print["orderN is "<>ToString[orderN]<>", which larger than the length of the series "<>ToString[n]]];
	Sum[series[[n-orderN+k]] (n - orderN + k)^orderN (-1)^(k+orderN) / (k! (orderN - k)! ), {k, 0, orderN}]
];

Scan[SetAttributes[#, {Protected, ReadProtected}]&,
     Select[Symbol /@ Names["RichardsonExtrapolation`*"], Head[#] === Symbol &]];

End[];
EndPackage[];
