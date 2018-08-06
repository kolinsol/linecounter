-module(lc_sup).

-behaviour(supervisor).

-export([init/1]).

-export([start_link/0]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    DirSup = {lc_dir_sup, {lc_dir_sup, start_link, []},
              permanent, 2000, supervisor, [lc_dir_sup]},
    Server = {lc_server, {lc_server, start_link, []},
              permanent, 2000, worker, [lc_server]},
    Children = [Server, DirSup],
    RestartStrategy = {one_for_one, 4, 3600},
    {ok, {RestartStrategy, Children}}.
