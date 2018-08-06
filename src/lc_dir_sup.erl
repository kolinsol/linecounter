-module(lc_dir_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/2]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(DirName, RefreshInterval) ->
    supervisor:start_child(?SERVER, [DirName, RefreshInterval]).

init([]) ->
    Element = {lc_dir_worker, {lc_dir_worker, start_link, []},
               temporary, brutal_kill, worker, [lc_dir_worker]},
    Children = [Element],
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.

