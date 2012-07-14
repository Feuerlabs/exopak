%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2012 Feuerlabs, Inc. All rights reserved.
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @doc
%%%    manage library archives
%%% @end
%%% Created : 21 May 2012 by Tony Rogvall <tony@rogvall.se>

-module(erl_ar).

-compile(export_all).

-import(lists, [reverse/1]).
-export([open/1, open/2, close/1]).
-export([add/3, table/1, t/1, extract/2, create/2]).
-export([read_global_header/1, write_global_header/1]).
-export([read_file_header/1, write_file_header/2]).
-export([read_fd/2, skip_fd/2]).

-define(GLOBAL_HEADER, <<"!<arch>\n">>).
-define(FILE_MAGIC, "`\n").

-include_lib("kernel/include/file.hrl").

%% calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}).
-define(GREGORIAN_UNIX_BIRTH, 62167219200). 

-record(file_header,
	{
	  file_name,  %% 16 bytes - ASCII
	  file_mtime, %% 12 bytes - Decimal
	  owner_id,   %% 6 bytes  - Decimal
	  group_id,   %% 6 bytes  - Decimal
	  file_mode,  %% 8 bytes  - Octal
	  file_size   %% 10 bytes - Decimal 
	  %% FILE_MAGIC - 2 bytes
	}).

-type device() :: pid().

open(File) ->
    open(File, [read]).

open(File, Opts) ->
    file:open(File, Opts++[binary]).

close(Fd) ->
    file:close(Fd).

-spec read_global_header(Fd::device()) ->
				{ok,Header::binary()} | {error,Reason::term()}.

read_global_header(Fd) ->
    case file:read(Fd, 8) of
	{ok, ?GLOBAL_HEADER} ->
	    {ok, ?GLOBAL_HEADER};
	{ok, _} ->
	    {error, bad_header};
	Error ->
	    Error
    end.

-spec(write_global_header(Fd::device()) ->
	     ok | {error,Reason::term()}).

write_global_header(Fd) ->
    file:write(Fd, ?GLOBAL_HEADER).

-spec read_file_header(Fd::device()) -> 
			      {ok,#file_header{}} | {error,Reason::term()}.

read_file_header(Fd) ->
    case file:read(Fd, 60) of
	{ok, <<BFileName:16/binary,
	       BFileMTime:12/binary,
	       BOwnerID:6/binary,
	       BGroupID:6/binary,
	       BFileMode:8/binary,
	       BFileSize:10/binary,
	       ?FILE_MAGIC>>} ->
	    FileName = binary_to_list(trim_fname(BFileName)),
	    FileMTime = binary_to_integer(trim(BFileMTime), 10),
	    OwnerId  = binary_to_integer(trim(BOwnerID), 10),
	    GroupId  = binary_to_integer(trim(BGroupID), 10),
	    FileMode = binary_to_integer(trim(BFileMode), 8),
	    FileSize = binary_to_integer(trim(BFileSize), 10),
	    case FileName of
		"/" -> %% GNU lookup table name
		    {ok,#file_header {
		       file_name = "/", file_mtime  = FileMTime, 
		       owner_id  = OwnerId, group_id  = GroupId,
		       file_mode = FileMode, file_size = FileSize
		      }};
		"#1/"++LFNameLen -> %% BSD extended filename
		    NameLen = list_to_integer(LFNameLen, 10),
		    case file:read(Fd, NameLen) of
			{ok,BFName} ->
			    {ok,#file_header {
			       file_name = binary_to_list(BFName),
			       file_mtime  = FileMTime, 
			       owner_id  = OwnerId, group_id  = GroupId,
			       file_mode = FileMode, file_size = FileSize
			      }};
			Error ->
			    Error
		    end;
		"//" -> %% GNU future file names are stored here
		    {ok,#file_header {
		       file_name = "/", file_mtime  = FileMTime, 
		       owner_id  = OwnerId, group_id  = GroupId,
		       file_mode = FileMode, file_size = FileSize
		      }};
		"/"++LOffset -> %% GNU extended file name
		    Offset = list_to_integer(LOffset, 10),
		    {ok,#file_header {
		       file_name = {future,Offset}, file_mtime  = FileMTime, 
		       owner_id  = OwnerId, group_id  = GroupId,
		       file_mode = FileMode, file_size = FileSize
		      }};
		FileName ->
		    {ok,#file_header {
		       file_name = FileName, file_mtime  = FileMTime, 
		       owner_id  = OwnerId, group_id  = GroupId,
		       file_mode = FileMode, file_size = FileSize
		      }}
	    end;
	{ok, _} ->
	    {error, bad_file_header};
	Error ->
	    Error
    end.

-spec write_file_header(Fd::device(), Header::#file_header{}) -> 
			       ok | {error,Reason::term()}.

write_file_header(Fd, H) ->
    GSec = calendar:datetime_to_gregorian_seconds(H#file_header.file_mtime),
    File_MTime = GSec - ?GREGORIAN_UNIX_BIRTH,
    file:write(Fd,
	       [pad_string(H#file_header.file_name,16),
		number_to_string(File_MTime,10,12),
		number_to_string(H#file_header.owner_id,10,6),
		number_to_string(H#file_header.group_id,10,6),
		number_to_string(H#file_header.file_mode,8,8),
		number_to_string(H#file_header.file_size,10,10),
		?FILE_MAGIC]).

-spec add(Fd::device(),NameInArchive::string(),FileOrBinary::string()|binary()) ->
		 ok | {error, Reason::term()}.

%% Currently only GNU style filenames are supported
add(Fd, NameInArchive, Binary) when is_binary(Binary) ->
    %% Fixme add long filenames
    Size = byte_size(Binary),
    io:format("binary ~w byte to file ~s\n", 
	      [Size, NameInArchive]),
    H = #file_header { file_name  = NameInArchive++"/",
		       file_mtime = calendar:universal_time(),
		       owner_id   = 0,
		       group_id   = 0,
		       file_mode  = 8#755,
		       file_size  = Size },
    write_file_header(Fd, H),
    Data = if Size band 1 =:= 1 -> [Binary,$\n];
	      true -> Binary
	   end,
    file:write(Fd, Data);
add(Fd, NameInArchive, File) ->
    case file:read_file_info(File) of
	{ok,Info} ->
	    Size = Info#file_info.size,
	    %% Fixme add long filenames
	    H = #file_header { file_name  = NameInArchive++"/",
			       file_mtime = Info#file_info.mtime,
			       owner_id   = Info#file_info.uid,
			       group_id   = Info#file_info.gid,
			       file_mode  = Info#file_info.mode,
			       file_size  = Size },
	    %% fixme - add GNU long file name!?
	    case write_file_header(Fd, H) of
		ok ->
		    io:format("copy ~w byte of file ~s\n", 
			      [Info#file_info.size,File]),
		    case file:copy(File, Fd, Size) of
			{ok,Size} ->
			    if Size band 1 =:= 1 ->
				    ok = file:write(Fd, "\n");
			       true ->
				    ok
			    end;
			Error -> Error
		    end;
		Error -> Error
	    end;
	Error ->
	    Error
    end.


read_fd(Fd, H) when is_record(H, file_header) ->
    file:read(Fd, H#file_header.file_size).

skip_fd(Fd, H)  when is_record(H, file_header) ->
    file:position(Fd, {cur, H#file_header.file_size}).

%% list all files in archive
table(File) ->
    List = fold(File,
		fun(Fd, H, Acc) ->
			skip_fd(Fd, H),
			[H#file_header.file_name|Acc]
		end, []),
    {ok, reverse(List)}.

t(File) ->
    fold(File,
	 fun(Fd, H, Acc) ->
		 io:format("~s\n", [H#file_header.file_name]),
		 skip_fd(Fd, H),
		 Acc
	 end, ok).

create(Name, Filenames) ->
    create(Name, Filenames, []).

create(Name, FileList, _Options) ->
    case open(Name, [write]) of
	{ok, Fd} ->
	    ok = write_global_header(Fd),
	    Result =
		lists:foldl(
		  fun ({NameInArchive,FilenameOrBin}, Res) 
			when is_binary(FilenameOrBin); is_list(FilenameOrBin) ->
			  case add(Fd, NameInArchive,FilenameOrBin) of
			      ok -> Res;
			      {error,Reason} ->
				  [ {error,{NameInArchive,Reason}} | Res]
			  end;
		      (NameInArchive, Res) when is_list(NameInArchive) ->
			  case add(Fd, NameInArchive, NameInArchive) of
			      ok -> Res;
			      {error,Reason} ->
				  [ {error,{NameInArchive,Reason}} | Res]
			  end
		  end, [], FileList),
	    case {Result, close(Fd)} of
		{[], Res} -> Res;
		{Res, _} -> {error,Res}
	    end;
	Reason ->
	    Reason
    end.


extract(File, Options) ->
    FileList = proplists:get_value(files, Options, []),
    Memory   = proplists:get_bool(memory, Options),
    Res =
	fold(File,
	     fun(Fd, H, Err={error,_}) ->
		     skip_fd(Fd, H),
		     Err;
		(Fd, H, Acc) ->
		     NameInArchive = H#file_header.file_name,
		     case FileList =:= [] orelse
			 lists:member(NameInArchive, FileList) of
			 true ->
			     if Memory ->
				     case read_fd(Fd, H) of
					 {ok, Bin} ->
					     [{NameInArchive,Bin}|Acc];
					 {error,Reason} ->
					     {error,{NameInArchive,Reason}}
				     end;
				true ->
				     case file:copy(Fd, NameInArchive, 
						    H#file_header.file_size) of
					 {ok,_} ->
					     ok;
					 {error,Reason} ->
					     {error,{NameInArchive,Reason}}
				     end
			     end;
			 false ->
			     skip_fd(Fd, H),
			     Acc
		     end
	     end, []),
    if is_list(Res) ->
	    {ok,Res};
       true ->
	    Res
    end.


%% list contents of archive
fold(File, Fun, Acc) ->
    case open(File) of
	{ok,Fd} ->
	    case read_global_header(Fd) of
		{ok,_Header} ->
		    Res = fold_fd(Fd, Fun, Acc),
		    file:close(Fd),
		    Res;
		Error ->
		    file:close(Fd),
		    Error
	    end;
	Error ->
	    Error
    end.

fold_fd(Fd, Fun, Acc) ->
    case read_file_header(Fd) of
	{ok, H} ->
	    Acc1 = Fun(Fd, H, Acc),
	    fold_fd(Fd, Fun, Acc1);
	eof ->
	    Acc;
	Error ->
	    Error
    end.

%% Locate file by file header - let fd be positioned after header
find_fd(Fd, Name) ->
    case read_file_header(Fd) of
	{ok, H} ->
	    if H#file_header.file_name =:= Name ->
		    {ok,H#file_header.file_size};
	       true -> 
		    skip_fd(Fd,H),
		    find_fd(Fd, Name)
	    end;
	eof ->
	    false;
	Error ->
	    Error
    end.


	       
%% Utils

binary_to_integer(Bin, Base) ->
    list_to_integer(binary_to_list(Bin), Base).

trim(Bin) ->
    trim(Bin, byte_size(Bin)-1).

trim(Bin, Pos) when Pos < 0 ->
    Bin;
trim(Bin, Pos) ->
    case binary:at(Bin, Pos) of
	$\s -> trim(Bin, Pos-1);
	_ -> binary:part(Bin, 0, Pos+1)
    end.


trim_fname(Bin) ->
    trim_fname(Bin, byte_size(Bin)-1).

trim_fname(Bin, Pos) when Pos < 0 ->
    Bin;
trim_fname(Bin, Pos) ->
    case binary:at(Bin, Pos) of
	$\s -> trim_fname(Bin, Pos-1);
	$/ when Pos =:= 0 -> <<"/">>;     %% GNU special file
	$/  -> binary:part(Bin, 0, Pos);  %% GNU file name
	_ -> binary:part(Bin, 0, Pos+1)
    end.

pad_string(String, Length) ->
    N = length(String),
    String ++ lists:duplicate(Length-N, $\s).

number_to_string(Number, Base, Length) ->
    pad_string(integer_to_list(Number, Base), Length).
