-module(player_sup).

-behaviour(supervisor).
-export([start_link/1, init/1, start_player/0]).

start_link(Port) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [Port]).

start_player() ->
	supervisor:start_child(?MODULE, []).

initial_listeners() ->
	[start_player() || _ <- lists:seq(1,20)],
	ok.

init([Port]) ->
	{ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active,once}, {packet,line}, {reuseaddr, true}]),
	spawn_link(fun initial_listeners/0),
	Player = {undefined,
		{player, start_link, [ListenSocket]},
		temporary, 1000, worker, [player]},
	{ok, {{simple_one_for_one, 60, 600}, [Player]}}.
