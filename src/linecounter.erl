-module(linecounter).

-export([analyze/1]).

analyze(Dir) ->
    lc_dir_worker:create(Dir).
