-module(game_sup).

-behaviour(supervisor).
-export([start_link/0, init/1, start_game/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_game(Name) ->
	supervisor:start_child(?MODULE, [Name]).

init([]) ->
	Game = {undefined,
		{game_fsm, start_link, []},
		permanent, 5000, worker, [game_fsm]},
	{ok, {{simple_one_for_one, 60, 60}, [Game]}}.
