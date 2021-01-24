Get["RichardsonExtrapolation`"]

testSeries = Map[3 # / (1 + #)&, Range[1,30]]

RichardsonExtrapolate[testSeries, 5] //N
