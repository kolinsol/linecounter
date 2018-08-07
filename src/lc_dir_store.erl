-module(lc_dir_store).

-export([init/0, insert/2,
         delete/1, lookup/1]).

-define(STORE, "~/.linecounter").
-define(TABLE_NAME, ?MODULE).

init() ->
    ets:new(?TABLE_NAME, [public, named_table]),
    ok.

insert(DirName, Pid) ->
    ets:insert(?TABLE_NAME, {DirName, Pid}).

lookup(DirName) ->
    case ets:lookup(?TABLE_NAME, DirName) of
         [{DirName, Pid}] -> {DirName, Pid};
         [] -> {error, not_found}
    end.

delete(Pid) ->
    ets:match_delete(?TABLE_NAME, {'_', Pid}).
