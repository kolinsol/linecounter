-module(lc_file_store).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-include("../include/records.hrl").

-record(state, {files = [],
                total_size = 0,
                total_lines = 9}).

init([]) -> {ok, #state{}}.

handle_call(_Msg, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({add_file, File}, State) ->
    File = #file_info{size = Size, lines = Lines},
    State = #state{files = CurrentFiles,
                   total_size = CurrentSize,
                   total_lines = CurrentLines},
    {noreply, #state{files = [File|CurrentFiles],
                     total_size = CurrentSize + Size,
                     total_lines = CurrentLines + Lines}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
