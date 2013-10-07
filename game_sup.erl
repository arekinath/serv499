-module(game_sup).

-behaviour(supervisor).
-export([start_link/0, init/1, get/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

get(Name) ->
	case ets:lookup(games, Name) of
		[{Name, Ref}] -> Ref;
		[] ->
			{ok, Ref} = supervisor:start_child(?MODULE, [Name]),
			ets:insert(games, {Name, Ref}),
			Ref
	end.

init([]) ->
	ets:new(games, [set, named_table, public, {read_concurrency, true}]),
	Game = {undefined,
		{game_fsm, start_link, []},
		permanent, 5000, worker, [game_fsm]},
	{ok, {{simple_one_for_one, 60, 60}, [Game]}}.
