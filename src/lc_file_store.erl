-module(lc_file_store).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-include("../include/records.hrl").

-record(state, {files = [],
                total_size = 0,
                total_line_number = 0,
                total_line_lengths = []}).

-export([start_link/0]).

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) -> {ok, #state{}}.

handle_call(_Msg, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({add_file,
             File = #file_info{size = Size,
                               line_number = FileLineNumber,
                               line_lengths = FileLineLengths}},
            #state{files = CurrentFiles,
                   total_size = CurrentSize,
                   total_line_number = CurrentLineNumber,
                   total_line_lengths = CurrentLineLengths}) ->
    {noreply, #state{files = [File|CurrentFiles],
                     total_size = CurrentSize + Size,
                     total_line_number = CurrentLineNumber + FileLineNumber,
                     total_line_lengths = CurrentLineLengths ++ FileLineLengths}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
