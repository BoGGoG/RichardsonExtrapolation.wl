RichardsonExtrapolate[series_, orderN_] := Block[{n},
	n = Length@series;
	If[orderN>n, Print["orderN is "<>ToString[orderN]<>", which larger than the length of the series "<>ToString[n]]];
	Sum[series[[n-orderN+k]] (n - orderN + k)^orderN (-1)^(k+orderN) / (k! (orderN - k)! ), {k, 0, orderN}]
];
