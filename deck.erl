%% serv499 for erlang
%%
%% Copyright (c) 2013, Alex Wilson (arekinath)
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%%  * Redistributions of source code must retain the above copyright notice,
%%    this list of conditions and the following disclaimer.
%%  * Redistributions in binary form must reproduce the above copyright notice,
%%    this list of conditions and the following disclaimer in the documentation
%%    and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED  TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR  BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.
%%

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
