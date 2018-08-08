-module(linecounter).

-export([analyze/1]).

analyze(DirName) ->
    case lc_dir_store:lookup(DirName) of
        {ok, _Pid} ->
            already_inited;
        {error, _} ->
            {ok, Pid} = lc_dir_worker:create(DirName),
            lc_dir_store:insert(DirName, Pid)
    end.
