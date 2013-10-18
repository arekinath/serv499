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

-module(game_fsm).

-behaviour(gen_fsm).

-export([start_link/1, join/3, disconnect/2, play/3, pass/2, chat/3]).
-export([init/1, handle_sync_event/4, handle_event/3, handle_info/3, terminate/3]).
-export([wait_players/3, wait_players/2]).
-export([deal/2]).
-export([bid/2]).
-export([lead/2]).
-export([trick/2]).
-export([score/2]).
-export([abort/2]).

%% public API

start_link(Name) ->
	gen_fsm:start_link(?MODULE, [Name], []).

join(Game, Pid, Name) ->
	gen_fsm:sync_send_all_state_event(Game, {new_player, Pid, Name}).

disconnect(Game, Pid) ->
	gen_fsm:send_all_state_event(Game, {disconnect, Pid}).

play(Game, Pid, CardOrBid) ->
	gen_fsm:send_event(Game, {play, Pid, CardOrBid}).

pass(Game, Pid) ->
	gen_fsm:send_event(Game, {pass, Pid}).

chat(Game, Pid, Data) ->
	gen_fsm:send_all_state_event(Game, {chat, Pid, Data}).

%% internal stuff

-record(state, {
	players=[], dead=[], name, hands=[], bidding=[], bid,
	pnames=[], teams=[], discon, trick=[], bidwin, mrefs=[]
	}).

-record(team, {players, tricks=0, score=0}).

rotate_until(P, [Next | Players]) when P == Next ->
	[P | Players];
rotate_until(P, [Next | Players]) ->
	rotate_until(P, Players ++ [Next]).

init([Name]) ->
	% we start up waiting for players
	io:format("[~p] game waiting for players\n", [Name]),
	{ok, wait_players, #state{name = Name}}.

terminate(_, _State, #state{name = Name}) ->
	ok.

wait_players({new_player, Pid, Name}, _From, S = #state{players = P, pnames = PN, name = N, mrefs = MR}) ->
	P2 = [Pid | P],
	PN2 = [{Pid, Name} | PN],
	MRef = monitor(process, Pid),
	MR2 = [{Pid, MRef} | MR],
	lists:foreach(fun(Player) ->
		Player ! {joined, Name}
	end, P),
	if length(P2) == 4 ->
		% sort the players by name
		PN3 = lists:keysort(2, PN2),
		P3 = [PPid || {PPid,_} <- PN3],

		% separate into teams
		[Pl1,Pl2,Pl3,Pl4] = PN3,
		Teams = [#team{players = [Pl1, Pl3]}, #team{players = [Pl2, Pl4]}],
		TeamNums = lists:zip(lists:seq(1, length(Teams)), [ [NName || {_,NName} <- T#team.players] || T <- Teams ]),

		% tell everyone about it
		lists:foreach(fun(PPid) ->
			PPid ! {start_game, N, TeamNums}
		end, P3),

		% and now let's move to deal some cards
		{reply, ok, deal, S#state{players = P3, pnames = PN3, teams = Teams, mrefs = MR2}, 0};
	true ->
		{reply, ok, wait_players, S#state{players = P2, pnames = PN2, mrefs = MR2}}
	end.

wait_players({disconnect, Pid}, S = #state{players = P, pnames = PN, mrefs = MR}) ->
	P2 = P -- [Pid],
	PN2 = lists:keydelete(Pid, 1, PN),
	[demonitor(R) || {PPid,R} <- MR, PPid =:= Pid],
	MR2 = lists:keydelete(Pid, 1, MR),
	Name = proplists:get_value(Pid, PN),
	lists:foreach(fun(Player) ->
		Player ! {disconnected, Name}
	end, P),
	case length(P2) of
		0 ->
			{stop, normal, S};
		_ ->
			{next_state, wait_players, S#state{players = P -- [Pid], pnames = PN2, mrefs = MR2}}
	end.

% once we have our players, we can deal out the hands
% we also return here later to deal new hands from the deck
deal(timeout, S = #state{players = P, teams = Ts, name = Nm}) ->
	io:format("[~p] dealing cards\n", [Nm]),
	Ts1 = [T#team{tricks = 0} || T <- Ts],

	% generate and shuffle the deck
	Deck = deck:generate(),

	% deal out some hands
	{Dealt, _} = deck:deal(Deck),
	Hands = lists:zip(P, Dealt),
	lists:foreach(fun({Player, Hand}) ->
		Player ! {hand, Hand}
	end, Hands),
	% continue on to bidding
	{next_state, bid, S#state{hands = Hands, bidding = P, teams = Ts1, bid = none}, 0}.

% if only one player is left bidding, they've won
bid(timeout, S = #state{bidding = [P], players = Ps, bid = B, name = Nm, pnames = PN}) ->
	% tell everyone the winning bid
	lists:foreach(fun(Player) ->
		Player ! {winning_bid, B}
	end, Ps),
	Name = proplists:get_value(P, PN),
	io:format("[~p] bidding won by ~p (~p)\n", [Nm, Name, B]),
	% and let the player who won take the lead
	{next_state, lead, S#state{players = rotate_until(P,Ps), bidwin = P}, 0};

% ask the next player for their bid
bid(timeout, S = #state{bidding = [P | _Rest], bid = B, dead = D}) ->
	case lists:member(P, D) of
		true ->
			% this player already disconnected
			{next_state, abort, S#state{discon = P}, 0};
		_ ->
			case B of
				none -> P ! first_bid;
				_ -> P ! {bid, B}
			end,
			{next_state, bid, S}
	end;

% if someone disconnects during their turn, abort right away
bid({disconnect, P}, S = #state{bidding = [P | _Rest]}) ->
	{next_state, abort, S#state{discon = P}, 0};

bid({play, P, B1S}, S = #state{bidding = [P | Rest], bid = B, players = Ps, pnames = PN}) ->
	{N, Suit} = B1S,
	B1 = {catch (list_to_integer(N)), Suit},
	case bids:valid(B1) of
		true ->
			case bids:greater(B1, B) of
				true ->
					Name = proplists:get_value(P, PN),
					lists:foreach(fun(Player) ->
						Player ! {bid_info, Name, B1}
					end, Ps -- [P]),
					{next_state, bid, S#state{bidding = Rest ++ [P], bid = B1}, 0};
				_ ->
					P ! reject,
					{next_state, bid, S}
			end;
		_ ->
			P ! reject,
			{next_state, bid, S}
	end;

bid({pass, P}, S = #state{bidding = [P | Rest], pnames = PN, players = Ps}) ->
	% players who pass are out of the bidding -- don't put them back on the list
	Name = proplists:get_value(P, PN),
	lists:foreach(fun(Player) ->
		Player ! {bid_info, Name, pass}
	end, Ps -- [P]),
	{next_state, bid, S#state{bidding = Rest}, 0}.

% ask a player to lead a card
lead(timeout, S = #state{players = [P | _Rest], dead = D}) ->
	case lists:member(P, D) of
		true ->
			{next_state, abort, S#state{discon = P}, 0};
		_ ->
			P ! lead,
			{next_state, trick, S#state{trick = []}}
	end;

lead({disconnect, P}, S = #state{players = [P | _Rest]}) ->
	{next_state, abort, S#state{discon = P}, 0}.

% if everyone's played in this trick, move on to scoring
trick(timeout, S = #state{players = [], pnames = PN}) ->
	P = [Pid || {Pid, _Name} <- PN],
	{next_state, score, S#state{players = P}, 0};

% ask a player to follow the lead
trick(timeout, S = #state{players = [P | _Rest], trick = T, dead = D}) ->
	case lists:member(P, D) of
		true ->
			{next_state, abort, S#state{discon = P}, 0};
		_ ->
			{_, {_, Suit}} = lists:last(T),
			P ! {follow, Suit},
			{next_state, trick, S}
	end;

trick({play, P, C}, S = #state{players = [P | Rest], hands = H, trick = T, pnames = PN}) ->
	Hand = proplists:get_value(P, H),
	case lists:member(C, Hand) of
		true ->
			{_Rank,Suit} = C,
			FollowSuit = case T of [] -> none; _ -> element(2, element(2, lists:last(T))) end,
			Follows = [{R,FS} || {R,FS} <- Hand, (FS =:= FollowSuit)],
			% check for a valid play
			Valid = if
				% if this is the leading play
				length(T) == 0 -> true;
				% if it follows suit
				Suit =:= FollowSuit -> true;
				% if the player can't follow suit
				length(Follows) == 0 -> true;
				% everything else is bad
				true -> false
			end,
			if Valid ->
				T2 = [{P, C} | T],
				P ! accept,
				Name = proplists:get_value(P, PN),
				lists:foreach(fun({PlPid, _}) ->
					if PlPid =/= P ->
						PlPid ! {play_info, Name, C};
					true -> ok
					end
				end, PN),
				% remove the card from the player's hand
				Hand1 = Hand -- [C],
				H1 = lists:keystore(P, 1, H, {P, Hand1}),
				% continue with the remaining players for this trick
				{next_state, trick, S#state{players = Rest, trick = T2, hands = H1}, 0};
			true ->
				P ! reject,
				{next_state, trick, S}
			end;
		_ ->
			P ! reject,
			{next_state, trick, S}
	end;

trick({disconnect, P}, S = #state{players = [P | _Rest]}) ->
	{next_state, abort, S#state{discon = P}, 0}.

score(timeout, S = #state{hands = Hands, trick = T, bid = Bid, bidwin = BidWinner, teams = Teams, pnames = PN, players = Players, name = Nm}) ->
	{BidTricks, TrumpSuit} = Bid,
	{_, {_, LeadSuit}} = lists:last(T),
	Lte = fun({_, {R1,S1}}, {_, {R2,S2}}) ->
		if
			% trump suit always wins
			(S1 =:= TrumpSuit) andalso (S2 =/= TrumpSuit) -> false;
			(S1 =/= TrumpSuit) andalso (S2 =:= TrumpSuit) -> true;
			% lead suit always beats non-lead
			(S1 =:= LeadSuit) andalso (S2 =/= LeadSuit) -> false;
			(S1 =/= LeadSuit) andalso (S2 =:= LeadSuit) -> true;
			% otherwise we must have two of the same suit
			true -> deck:rank_lte(R1, R2)
		end
	end,

	% figure out the winner of the trick and adjust #tricks
	Ts = lists:sort(Lte, T),
	{Winner, _WinCard} = lists:last(Ts),
	Teams1 = lists:map(fun(Team = #team{players = Ps, tricks = Tr}) ->
		Pids = [Pid || {Pid, _} <- Ps],
		case lists:member(Winner, Pids) of
			true -> Team#team{tricks = Tr + 1};
			_ -> Team
		end
	end, Teams),

	% tell everyone
	WinnerName = proplists:get_value(Winner, PN),
	lists:foreach(fun(Player) ->
		Player ! {trick_won, WinnerName}
	end, Players),

	% if anyone has run out of cards, this hand is over
	EmptyHand = lists:any(fun({_P, H}) -> H =:= [] end, Hands),
	if EmptyHand ->
		Teams2 = lists:map(fun(Team = #team{players = Ps, tricks = Tr, score = Sc}) ->
			Pids = [Pid || {Pid, _} <- Ps],
			case lists:member(BidWinner, Pids) of
				true ->
					if Tr >= BidTricks ->
						Team#team{score = Sc + bids:points(Bid)};
					true ->
						Team#team{score = Sc - bids:points(Bid)}
					end;
				_ -> Team
			end
		end, Teams1),

		% tell everyone the new scores
		Scores = lists:zip(lists:seq(1,length(Teams2)), lists:map(fun(#team{score=Sc}) -> Sc end, Teams2)),
		io:format("[~p] hand is over, new scores ~p\n", [Nm, Scores]),
		lists:foreach(fun(Player) ->
			Player ! {scores, Scores}
		end, Players),

		AnyWinners = lists:any(fun(#team{score=Sc}) -> Sc > 499 end, Teams2),
		AnyLosers = lists:any(fun(#team{score=Sc}) -> Sc < -499 end, Teams2),

		% has anybody won or lost yet?
		if AnyWinners orelse AnyLosers ->
			% thanks for playing, kids
			io:format("[~p] game over!\n", [Nm]),
			lists:foreach(fun(Player) ->
				Player ! game_over
			end, Players),
			{stop, normal, S#state{teams = Teams2}};
		true ->
			% nope, deal out a new hand
			{next_state, deal, S#state{teams = Teams2}, 0}
		end;

	true ->
		% otherwise, continue with the next trick
		{next_state, lead, S#state{teams = Teams1, players = rotate_until(Winner, Players)}, 0}
	end.


% handle stopping after disconnection
abort(timeout, S = #state{discon = P, dead = D, pnames = PN}) ->
	Name = proplists:get_value(P, PN),
	Ps = [Pid || {Pid, _} <- PN],
	lists:foreach(fun(Player) ->
		Player ! {disconnected, Name}
	end, Ps -- D -- [P]),
	{stop, normal, S}.

handle_info({'DOWN', _MRef, process, Pid, _Why}, State, S) ->
	handle_event({disconnect, Pid}, State, S).

handle_sync_event(Ev = {new_player, _Pid, _Name}, From, wait_players, S) ->
	wait_players(Ev, From, S);
handle_sync_event({new_player, _Pid, _Name}, _From, State, S) ->
	{reply, {error, full}, State, S}.

handle_event({chat, From, Data}, State, S = #state{pnames = PN}) ->
	Targets = [Pid || {Pid, _} <- PN, Pid =/= From],
	FromName = proplists:get_value(From, PN),
	lists:foreach(fun(Player) ->
		Player ! {chat, FromName, Data}
	end, Targets),
	{next_state, State, S};
handle_event(Ev = {disconnect, _Pid}, wait_players, S) ->
	wait_players(Ev, S);
handle_event(Ev = {disconnect, P}, bid, S = #state{bidding = [P | _]}) ->
	bid(Ev, S);
handle_event(Ev = {disconnect, P}, lead, S = #state{players = [P | _]}) ->
	lead(Ev, S);
handle_event(Ev = {disconnect, P}, trick, S = #state{players = [P | _]}) ->
	trick(Ev, S);
handle_event({disconnect, Pid}, State, S = #state{dead = D}) ->
	{next_state, State, S#state{dead = [Pid | D]}}.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

plexer(Tag, Parent) ->
	receive M -> Parent ! {Tag, M}, plexer(Tag, Parent) end.

spawn_players() ->
	eatall(),
	Parent = self(),
	[spawn(fun() -> plexer(p1, Parent) end),
	 spawn(fun() -> plexer(p2, Parent) end),
	 spawn(fun() -> plexer(p3, Parent) end),
	 spawn(fun() -> plexer(p4, Parent) end)].

spawn_join_players(G) ->
	Ps = spawn_players(),
	[ok = game_fsm:join(G,P,"p" ++ integer_to_list(N)) || {P,N} <- lists:zip(Ps, lists:seq(1, length(Ps)))],
	Ps.

expect([]) -> ok;
expect([Msg | Rest]) ->
	receive Msg -> expect(Rest)
	after 1000 -> error(timeout) end.

eatall() ->
	receive _M -> eatall() after 100 -> ok end.

join_discon_test() ->
	[P1,P2,P3,P4] = spawn_players(),
	Me = self(),
	P5 = spawn(fun() -> plexer(p5, Me) end),

	{ok, G} = game_fsm:start_link("game"),
	ok = game_fsm:join(G, P1, "p1"),
	ok = game_fsm:join(G, P2, "p2"),
	ok = game_fsm:join(G, P3, "p3"),
	ok = game_fsm:disconnect(G, P1),
	ok = game_fsm:join(G, P4, "p4"),
	ok = game_fsm:join(G, P5, "p5"),

	Teams = [{1, ["p2","p4"]}, {2, ["p3", "p5"]}],
	expect([
		{p2, {start_game, "game", Teams}},
		{p3, {start_game, "game", Teams}},
		{p4, {start_game, "game", Teams}},
		{p5, {start_game, "game", Teams}},
		{p2, first_bid}
		]),

	{error, full} = game_fsm:join(G, P1, "p1").

deal_test() ->
	{ok, G} = game_fsm:start_link("game"),
	_ = spawn_join_players(G),
	receive {p1, {hand, List}} when length(List) == 13 -> ok
		after 1000 -> error(timeout) end,
	receive {p2, {hand, List2}} when length(List2) == 13 -> ok
		after 1000 -> error(timeout) end.

bid_test() ->
	{ok, G} = game_fsm:start_link("game"),
	[P1,P2,_P3,_P4] = spawn_join_players(G),
	receive {p1, first_bid} -> ok
		after 1000 -> error(timeout) end,
	game_fsm:play(G, P1, {"C", "1"}),
	receive {p2, {bid_info, "p1", {"C", 1}}} -> error(accepted_bad_bid)
		after 500 -> ok end,
	game_fsm:play(G, P1, {"C", "4"}),
	receive {p1, {bid_info, _, _}} -> error(bid_info_not_origin)
		after 500 -> ok end,
	receive {p3, {bid_info, "p1", {"C", 4}}} -> ok
		after 1000 -> error(timeout) end,
	receive {p2, {bid, {"C", 4}}} -> ok
		after 1000 -> error(timeout) end,
	game_fsm:play(G, P2, {"C", "5"}),
	receive {p1, {bid_info, "p2", {"C", 5}}} -> ok
		after 1000 -> error(timeout) end.

bid_pass_test() ->
	{ok, G} = game_fsm:start_link("game"),
	[P1,P2,P3,P4] = spawn_join_players(G),
	receive {p1, first_bid} -> ok
		after 1000 -> error(timeout) end,
	game_fsm:play(G, P1, {"C", "4"}),
	receive {p1, accept} -> ok
		after 1000 -> error(timeout) end,
	eatall(),
	game_fsm:pass(G, P2),
	receive {p1, {bid_info, "p2", pass}} -> ok
		after 1000 -> error(timeout) end,
	receive {p3, {bid_info, "p2", pass}} -> ok
		after 1000 -> error(timeout) end,
	eatall(),
	game_fsm:pass(G, P3),
	receive {p4, {bid, {"C", 4}}} -> ok
		after 1000 -> error(timeout) end,
	game_fsm:play(G, P4, {"C", "5"}),
	receive {p4, accept} -> ok
		after 1000 -> error(timeout) end,
	receive {p1, {bid, {"C", 5}}} -> ok
		after 1000 -> error(timeout) end,
	eatall(),
	game_fsm:play(G, P1, {"C", "6"}),
	receive {p4, {bid, {"C", 6}}} -> ok
		after 1000 -> error(timeout) end.

bid_disconnect_test() ->
	{ok, G} = game_fsm:start_link("game"),
	[P1,P2,P3,P4] = spawn_join_players(G),
	receive {p1, first_bid} -> ok
		after 1000 -> error(timeout) end,
	game_fsm:disconnect(G, P4),
	game_fsm:play(G, P1, {"C", "4"}),
	receive {p1, accept} -> ok
		after 1000 -> error(timeout) end,
	eatall(),
	game_fsm:pass(G, P2),
	receive {p1, {bid_info, "p2", pass}} -> ok
		after 1000 -> error(timeout) end,
	receive {p3, {bid_info, "p2", pass}} -> ok
		after 1000 -> error(timeout) end,
	eatall(),
	game_fsm:pass(G, P3),
	receive {p1, {disconnected, "p4"}} -> ok
		after 1000 -> error(timeout) end,
	receive {p2, {disconnected, "p4"}} -> ok
		after 1000 -> error(timeout) end.

lead_test() ->
	{ok, G} = game_fsm:start_link("game"),
	[P1,P2,P3,P4] = spawn_join_players(G),
	receive {p2, {hand, P2Hand}} -> ok end,
	receive {p3, {hand, P3Hand}} -> ok end,
	receive {p1, first_bid} -> ok
		after 1000 -> error(timeout) end,
	game_fsm:play(G, P1, {"C", "4"}),
	game_fsm:play(G, P2, {"C", "5"}),
	game_fsm:pass(G, P3),
	game_fsm:pass(G, P4),
	game_fsm:pass(G, P1),
	receive {p2, lead} -> ok
		after 1000 -> error(timeout) end,
	eatall(),
	Card = lists:last(P2Hand),
	game_fsm:play(G, P2, Card),
	receive {p2, accept} -> ok
		after 1000 -> error(timeout) end,
	receive {p1, {play_info, "p2", Card}} -> ok
		after 1000 -> error(timeout) end,
	receive {p3, {follow, Suit}} -> ok end,
	eatall(),
	SuitCards = [X || X = {_R,S} <- P3Hand, S =:= Suit],
	P3Card = case SuitCards of [] -> lists:last(P3Hand); _ -> lists:last(SuitCards) end,
	game_fsm:play(G, P3, P3Card),
	io:format("playing ~p to follow ~p\n", [P3Card, Suit]),
	receive {p3, accept} -> ok
		after 1000 -> error(timeout) end,
	receive {p4, {follow, Suit}} -> ok
		after 1000 -> error(timeout) end.

-endif.
