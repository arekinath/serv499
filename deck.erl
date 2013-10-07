-module(deck).

-export([generate/0, deal/1, rank_lte/2]).

generate() ->
	Suits = ["S", "D", "C", "H"],
	Ranks = ["A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2"],
	Combos = [{R, S} || R <- Ranks, S <- Suits],
	[V || {_, V} <-
		lists:sort(
			[{crypto:rand_uniform(1,length(Combos)), V} || V <- Combos])].

rank_lte(R1, R2) ->
	Ranks = ["A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2"],
	RankLookup = lists:zip(lists:reverse(Ranks), lists:seq(1, length(Ranks))),
	(proplists:get_value(R1, RankLookup) =< proplists:get_value(R2, RankLookup)).

deal([Hand | HRest], [Next | Rest]) when length(Hand) < 13 ->
	deal([[Next | Hand] | HRest], Rest);
deal(Hands, Deck) when length(Hands) < 4 ->
	deal([[] | Hands], Deck);
deal(Hands, Deck) -> {Hands, Deck}.

deal(Deck) -> deal([], Deck).
