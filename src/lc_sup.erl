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
    FileStore = {lc_file_store, {lc_file_store, start_link, []},
              permanent, 2000, worker, [lc_file_store]},
    Children = [FileStore, DirSup],
    RestartStrategy = {one_for_one, 4, 3600},
    {ok, {RestartStrategy, Children}}.
