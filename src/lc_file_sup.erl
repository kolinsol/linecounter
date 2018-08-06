-module(lc_file_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(FileName) ->
    supervisor:start_child(?SERVER, [FileName]).

init([]) ->
    Element = {lc_file_worker, {lc_file_worker, start_link, []},
               temporary, brutal_kill, worker, [lc_file_worker]},
    Children = [Element],
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.
