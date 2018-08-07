-module(lc_dir_worker).

-behaviour(gen_server).

-export([start_link/2, create/2, create/1]).

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_REFRESH_INTERVAL, 60 * 1000).

-record(state, {dir_name, refresh_interval, start_time,
                inner_dirs = [], inner_files = []}).

start_link(DirName, RefreshInterval) ->
    gen_server:start_link(?MODULE, [DirName, RefreshInterval], []).

create(DirName, RefreshInterval) ->
    lc_dir_sup:start_child(DirName, RefreshInterval).

create(DirName) ->
    create(DirName, ?DEFAULT_REFRESH_INTERVAL).

init([DirName, RefreshInterval]) ->
    Now = calendar:local_time(),
    StartTime = calendar:datetime_to_gregorian_seconds(Now),
    {InnerFileList, InnerDirList} = lc_file_util:categorize(DirName),
    ProcessedFiles = lists:map(fun lc_file_util:process_file/1,
                               InnerFileList),
    {ok,
     #state{dir_name = DirName,
            inner_dirs = InnerDirList,
            inner_files = ProcessedFiles,
            refresh_interval = RefreshInterval,
            start_time = StartTime},
     time_left(StartTime, RefreshInterval)}.

time_left(_StartTime, infinity) -> infinity;
time_left(StartTime, LeaseTime) ->
    Now = calendar:local_time(),
    CurrentTime = calendar:datetime_to_gregorian_seconds(Now),
    TimeElapsed = CurrentTime - StartTime,
    case LeaseTime - TimeElapsed of
        Interval when Interval =< 0 -> 0;
        Interval -> Interval * 1000
    end.

handle_call(_Msg, _From, State) ->
	#state{refresh_interval = RefreshInterval, start_time = StartTime} = State,
    TimeLeft = time_left(StartTime, RefreshInterval),
    Reply = ok,
    {reply, Reply, State, TimeLeft}.

handle_cast(_Msg, State) ->
	#state{refresh_interval = RefreshInterval, start_time = StartTime} = State,
    TimeLeft = time_left(StartTime, RefreshInterval),
    {noreply, State, TimeLeft}.

handle_info(timeout, State) ->
    Now = calendar:local_time(),
    NewStartTime = calendar:datetime_to_gregorian_seconds(Now),
    Timeout = State#state.refresh_interval,
    {noreply, State#state{start_time = NewStartTime}, time_left(NewStartTime, Timeout)}.

terminate(_Reason, _State) ->
    lc_dir_store:delete(self()),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
