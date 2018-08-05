-module(lc_server).
-behaviour(gen_server).

%% DEBUG
-export([state/0]).

-export([start_link/1, start_link/0, stop/0,
         change_dir/1]).

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_DIR, "~/.linecounter").

-record(state, {dir_name}).

start_link(Dir) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Dir], []).

start_link() ->
    start_link(?DEFAULT_DIR).

stop() ->
    gen_server:cast(?SERVER, stop).

change_dir(NewDir) ->
    gen_server:cast(?SERVER, {change_dir, NewDir}).

%% DEBUG
state() ->
    gen_server:call(?SERVER, state).

init([Dir]) -> {ok, #state{dir_name=Dir}}.

%% DEBUG
handle_call(state, _From, State) ->
    {reply, State, State}.

handle_cast({change_dir, NewDir}, State) ->
    {noreply, State#state{dir_name=NewDir}};
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
