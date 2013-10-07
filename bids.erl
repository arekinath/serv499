-module(bids).

-export([valid/1, points/1, greater/2]).

valid(B) ->
	points(B) > 0.

points({Suit, N}) ->
	proplists:get_value(Suit,
		[{"S", 0}, {"C", 10}, {"D", 20}, {"H", 30}], -300) +
	proplists:get_value(N,
		[{4, 20}, {5, 70}, {6, 120},
		 {7, 170}, {8, 220}, {9, 270}], -50).

greater(_B, none) -> true;
greater(none, _B) -> false;
greater(B1, B2) ->
	(points(B1) > points(B2)).
