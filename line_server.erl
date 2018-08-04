-module(line_server).
-behaviour(gen_server).

-export([start_link/1, start_link/0, stop/0]).

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

init([Dir]) -> {ok, #state{dir_name=Dir}}.

handle_call(_Msg, _From, State) -> {ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
