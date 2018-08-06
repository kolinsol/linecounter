-module(sc_element).

-behaviour(gen_server).

-export([start_link/2, create/2, create/1]).

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_REFRESH_INTERVAL, 60 * 1000).

-record(state, {refresh_interval, dir_name}).

start_link(DirName, RefreshInterval) ->
    gen_server:start_link(?MODULE, [DirName, RefreshInterval], []).

create(DirName, RefreshInterval) ->
    sc_sup:start_child(DirName, RefreshInterval).

create(DirName) ->
    create(DirName, ?DEFAULT_REFRESH_INTERVAL).

init([RefreshInterval]) ->
    {ok, #state{refresh_interval = RefreshInterval}, RefreshInterval}.

handle_call(_Msg, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    DirName = State#state.dir_name,
    io:format("refreshing dir ~p~n", [DirName]),
    Timeout = State#state.refresh_interval,
    {ok, State, Timeout}.

terminate(_Reason, _State) ->
    lc_dir_store:delete(self()),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
