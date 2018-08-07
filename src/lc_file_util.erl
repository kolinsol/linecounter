-module(lc_file_util).
-export([list_dir_recursive/1, categorize/1, process_file/1]).

-include("../include/records.hrl").

list_dir_recursive(DirName) ->
    DeepFileList = do_list_dir_recursive(DirName),
    lists:map(fun({ok, FileName}) -> FileName end,
              lists:flatten(DeepFileList)).

categorize(DirName) ->
    {ok, FileNames} = file:list_dir(DirName),
    ExtendedFileNames = lists:map(
        fun(FileName) -> add_basename(DirName, FileName) end,
        FileNames
    ),
    Files = [FileName || FileName <- ExtendedFileNames,
                         not filelib:is_dir(FileName)],
    Dirs = [FileName || FileName <- ExtendedFileNames,
                        filelib:is_dir(FileName)],
    {Files, Dirs}.

process_file(FileName) ->
    Size = get_size(FileName),
    Lines = count_lines(FileName),
    Type = get_filetype(FileName),
    LastModified = last_modified(FileName),
    #file_info{name=FileName,
               size=Size,
               lines=Lines,
               type=Type,
               last_modified=LastModified}.

count_lines(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    do_count_lines(Device, 0).

get_filetype(FileName) -> 
    case filename:extension(FileName) of
        [] -> "no filetype";
        FileType -> FileType
    end.

get_size(FileName) -> filelib:file_size(FileName).

last_modified(FileName) -> filelib:last_modified(FileName).

do_list_dir_recursive(BaseName) ->
    {ok, FileList} = file:list_dir(BaseName),
    ExtendedFileList = lists:map(fun(FileName) ->
                                     add_basename(BaseName, FileName)
                                 end, FileList),
    lists:map(fun (FileName) ->
                  case filelib:is_dir(FileName) of
                      true -> do_list_dir_recursive(FileName);
                      false -> {ok, process_file(FileName)}
                  end
              end,
              ExtendedFileList).

do_count_lines(Device, Result) ->
    case io:get_line(Device, "") of
        eof -> file:close(Device), Result;
        _Line -> do_count_lines(Device, Result + 1)
    end.

add_basename(BaseName, FileName) -> BaseName ++ "/" ++ FileName.
