-module(player).
-behaviour(gen_fsm).

-export([start_link/1, init/1, handle_info/3, terminate/3]).
-export([accept/2, get_name/2, get_game/2, before_start/2]).
-export([wait_play/2, play/2]).

start_link(ListenSock) ->
	gen_fsm:start_link(?MODULE, [ListenSock], []).

-record(state, {lsock, sock, line, name, game}).

iol_join([], _) -> [];
iol_join([Next], _Sep) -> [Next];
iol_join([Next | Rest], Sep) -> [Next, Sep | iol_join(Rest, Sep)].

init([ListenSock]) ->
	{ok, accept, #state{lsock = ListenSock, line = <<>>}, 0}.

terminate(_Reason, _State, _S) -> ok.

accept(timeout, S = #state{lsock = ListenSock}) ->
	{ok, Sock} = gen_tcp:accept(ListenSock),
	% start our replacement in the pool
	player_sup:start_player(),
	% send welcome
	inet:setopts(Sock, [{active, true}]),
	gen_tcp:send(Sock, <<"MHi there! This is arekinath's 2310 499serv\n">>),
	{next_state, get_name, S#state{sock = Sock}}.

get_name({line, Name}, S = #state{}) ->
	{next_state, get_game, S#state{name = Name}};

get_name(disconnect, S) ->
	{stop, normal, S}.

get_game({line, GameName}, S = #state{sock = Sock, name = Name}) ->
	{ok, Game} = game_index:get(GameName),
	case game_fsm:join(Game, self(), Name) of
		ok ->
			gen_tcp:send(Sock, <<"MJoined game ", GameName/binary, "\n">>),
			{next_state, before_start, S#state{game = Game}};
		_ ->
			gen_tcp:send(Sock, <<"MSorry, that game is full\n">>),
			gen_tcp:close(Sock),
			{stop, normal, S}
	end;

get_game(disconnect, S) ->
	{stop, normal, S}.

before_start({start_game, _N, Teams}, S = #state{sock = Sock}) ->
	lists:foreach(fun({TN, Names}) ->
		gen_tcp:send(Sock, [<<"MTeam">>, integer_to_list(TN), <<": ">>, iol_join(Names, <<", ">>), <<"\n">>])
	end, Teams),
	{next_state, wait_play, S};

before_start({joined, Player}, S = #state{sock = Sock}) ->
	gen_tcp:send(Sock, [<<"M">>, Player, <<" has joined\n">>]),
	{next_state, before_start, S};

before_start({disconnected, Player}, S = #state{sock = Sock}) ->
	gen_tcp:send(Sock, [<<"M">>, Player, <<" has left\n">>]),
	{next_state, before_start, S};

before_start({line, <<"M", Rest/binary>>}, S = #state{game = Game}) ->
	game_fsm:chat(Game, self(), Rest),
	{next_state, before_start, S}.

wait_play({hand, Hand}, S = #state{sock = Sock}) ->
	HandParts = [[R,Su] || {R,Su} <- Hand],
	gen_tcp:send(Sock, [<<"H">>, HandParts, <<"\n">>]),
	{next_state, wait_play, S};

wait_play({scores, Scores}, S = #state{sock = Sock}) ->
	Part = iol_join(lists:map(fun({TN, Score}) ->
		[<<"Team ">>, integer_to_list(TN), <<"=">>, integer_to_list(Score)]
	end, Scores), ", "),
	gen_tcp:send(Sock, [<<"M">>, Part, <<"\n">>]),
	{next_state, wait_play, S};

wait_play(first_bid, S = #state{sock = Sock}) ->
	gen_tcp:send(Sock, <<"B\n">>),
	{next_state, play, S};

wait_play({bid, LastBid}, S = #state{sock = Sock}) ->
	{N,Su} = LastBid,
	gen_tcp:send(Sock, [<<"B">>, integer_to_list(N), Su, <<"\n">>]),
	{next_state, play, S};

wait_play(lead, S = #state{sock = Sock}) ->
	gen_tcp:send(Sock, <<"L\n">>),
	{next_state, play, S};

wait_play({follow, Suit}, S = #state{sock = Sock}) ->
	gen_tcp:send(Sock, [<<"P">>, Suit, <<"\n">>]),
	{next_state, play, S};

wait_play({line, <<"M", Rest/binary>>}, S = #state{game = Game}) ->
	game_fsm:chat(Game, self(), Rest),
	{next_state, wait_play, S}.

play({line, <<"M", Rest/binary>>}, S = #state{game = Game}) ->
	game_fsm:chat(Game, self(), Rest),
	{next_state, play, S};

play({line, <<"PP">>}, S = #state{game = Game}) ->
	game_fsm:pass(Game, self()),
	{next_state, wait_play, S};

play({line, CardOrBid}, S = #state{game = Game}) when byte_size(CardOrBid) == 2 ->
	[A,B] = binary_to_list(CardOrBid),
	game_fsm:play(Game, self(), {[A], [B]}),
	{next_state, play, S};

play(accept, S = #state{sock = Sock}) ->
	gen_tcp:send(Sock, <<"A\n">>),
	{next_state, wait_play, S};

play(reject, S = #state{sock = Sock}) ->
	gen_tcp:send(Sock, <<"MI'm sorry Dave, I can't do that\n">>),
	{next_state, play, S}.

handle_info({tcp, Sock, Data}, State, S = #state{sock = Sock, line = L}) ->
	Line = <<L/binary, Data/binary>>,
	case binary:last(Data) of
		$\n ->
			ChompLine = binary:part(Line, 0, byte_size(Line)-1),
			?MODULE:State({line, ChompLine}, S#state{line = <<>>});
		_ ->
			{next_state, State, S#state{line = Line}}
	end;
handle_info({tcp_closed, Sock}, State, S = #state{sock = Sock}) ->
	?MODULE:State(disconnect, S);

handle_info({bid_info, P, pass}, _, S = #state{sock = Sock}) ->
	gen_tcp:send(Sock, [<<"M">>, P, <<" passes\n">>]),
	{next_state, wait_play, S};
handle_info({bid_info, P, {N,Su}}, _, S = #state{sock = Sock}) ->
	gen_tcp:send(Sock, [<<"M">>, P, <<" bids ">>, integer_to_list(N), Su, <<"\n">>]),
	{next_state, wait_play, S};
handle_info({play_info, P, {R,Su}}, _, S = #state{sock = Sock}) ->
	gen_tcp:send(Sock, [<<"M">>, P, <<" plays ">>, R, Su, <<"\n">>]),
	{next_state, wait_play, S};
handle_info({winning_bid, {N,Su}}, _, S = #state{sock = Sock}) ->
	gen_tcp:send(Sock, [<<"T">>, integer_to_list(N), Su, <<"\n">>]),
	{next_state, wait_play, S};
handle_info({trick_won, P}, _, S = #state{sock = Sock}) ->
	gen_tcp:send(Sock, [<<"MTrick won by ">>, P, <<"\n">>]),
	{next_state, wait_play, S};

handle_info({chat, Who, Data}, State, S = #state{sock = Sock}) ->
	gen_tcp:send(Sock, [<<"M<">>, Who, <<"> ">>, Data, <<"\n">>]),
	{next_state, State, S};

handle_info(game_over, _, S = #state{sock = Sock}) ->
	gen_tcp:send(Sock, <<"O\n">>),
	gen_tcp:close(Sock),
	{stop, normal, S};
handle_info({disconnected, Who}, St, S = #state{sock = Sock}) when St =/= before_start ->
	gen_tcp:send(Sock, [<<"M">>, Who, <<" disconnected early\n">>]),
	gen_tcp:send(Sock, <<"O\n">>),
	gen_tcp:close(Sock),
	{stop, normal, S};

handle_info(Msg, State, S) ->
	?MODULE:State(Msg, S).
