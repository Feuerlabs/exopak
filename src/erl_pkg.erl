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
%%%    Handle debian packages (at least read dependency information)
%%% @end
%%% Created : 22 May 2012 by Tony Rogvall <tony@rogvall.se>

-module(erl_pkg).

-compile(export_all).

-define(DEBIAN_BINARY, "2.0\n").

-define(info(F,A), io:format((F)++"\n", (A))).

-define(is_digit(X), (((X) >= $0) andalso ((X) =< $9))).
-define(is_lower(X), (((X) >= $a) andalso ((X) =< $z))).
-define(is_upper(X), (((X) >= $A) andalso ((X) =< $Z))).
-define(is_alpha(X), (?is_lower((X)) orelse ?is_upper((X)))).
-define(is_alnum(X), (?is_alpha((X)) orelse ?is_digit((X)))).

-import(lists, [reverse/1, map/2]).

-export([compare_version/2]).
-export([parse_depends/1]).
-export([parse_depend/1]).
-export([parse_version/1]).
%%
%% package.deb  (ar)
%% package.opk  (ar)
%% package.ipk  (tar.gz)
%%  +- debian-binary   (text)
%%  +- control.tar.gz  (tarball)
%% ...+- control       (text)
%% ...+- preinst       (script)
%% ...+- postinst      (script)
%% ...+- prerm         (script)
%% ...+- postrm        (script)
%% +- data.tar.gz      (tarball)
%%    +- files
%%

make(Name0, Files, Options) ->
    Pkg = proplists:get_value(type, Options, opk),
    Prefix = proplists:get_value(prefix, Options, "./opt"),
    {Name,Type} =
	case filename:extension(Name0) of
	    ".opk" -> {Name0, opk};
	    ".ipk" -> {Name0, ipk};
	    ".deb" -> {Name0, deb};
	    ""     -> {Name0 ++ "."++atom_to_list(Pkg), Pkg};
	    _ -> {Name0, Pkg}
	end,
    case control_files(Files,Prefix) of
	{_, _, Err} when Err /= [] ->
	    {error, Err};
	{ControlFiles, DataFiles,_} ->
	    Control = make_control(Type, ControlFiles),
	    Data    = make_data(Type, DataFiles),
	    make_package(Name, Type, [Control,Data])
    end.

make_package(Name, opk, Files) ->
    erl_ar:create(Name, [{"debian-binary", <<?DEBIAN_BINARY>>} | Files]);
make_package(Name, deb, Files) ->
    erl_ar:create(Name, [{"debian-binary", <<?DEBIAN_BINARY>>} | Files]);
make_package(Name, ipk, Files) ->
    erl_tar:create(Name, [{"debian-binary", <<?DEBIAN_BINARY>>} | Files],
		   [compressed]).

make_control(_Type, ["control.tar.gz"]) ->
    "control.tar.gz";
make_control(_Type, Files) ->
    ok = erl_tar:create("control.tar.gz", Files, [compressed]),
    "control.tar.gz".

make_data(_Type, ["data.tar.gz"]) ->
    "data.tar.gz";
make_data(_Type, Files) ->
    ok = erl_tar:create("data.tar.gz", Files, [compressed]),
    "data.tar.gz".

control_files(Files,Prefix) ->
    control_files(Files,Prefix,[],[],[]).

control_files([Member={File,Binary}|Files],Prefix, Control, Data, Err)
  when is_binary(Binary) ->
    case is_control(File) of
	true ->
	    ?info("control file(bin): ~s", [File]),
	    control_files(Files,Prefix,[Member|Control], Data, Err);
	false ->
	    ?info("data file(bin): ~s", [File]),
	    IFile = filename:join(Prefix, File),
	    control_files(Files,Prefix,Control,[{IFile,Binary}|Data], Err)
    end;
control_files([File|Files],Prefix,Control, Data, Err) ->
    case is_control(File) of
	true ->
	    case filelib:is_regular(File) of
		true ->
		    ?info("control file: ~s", [File]),
		    control_files(Files,Prefix,[File|Control], Data, Err);
		false ->
		    ?info("control file: ~s not found", [File]),
		    control_files(Files,Prefix,Control,Data, 
				  [{error,{File,enoent}}|Err])
	    end;
	false ->
	    case filelib:is_regular(File) of
		true ->
		    ?info("data file: ~s", [File]),
		    IFile = filename:join(Prefix, File),
		    control_files(Files,Prefix,Control,
				  [{IFile,File}|Data], Err);
		false ->
		    case filelib:is_dir(File) of
			false ->
			    ?info("data file: ~s not found", [File]),
			    control_files(Files,Prefix,Control,Data, 
					  [{error,{File,enoent}}|Err]);
			true ->
			    {Data1,Err1} = dir_files([File],Prefix,Data,Err),
			    control_files(Files,Prefix,Control,Data1,Err1)
		    end
	    end
    end;
control_files([],_Prefixe,Control,Data,Err) ->
    {Control, Data, Err}.


dir_files([File|Files], Prefix, Acc, Err) ->
    case filelib:is_dir(File) of
	true ->
	    ?info("data dir: ~s", [File]),
	    case skip_file(File, dir) of
		true ->
		    dir_files(Files, Prefix, Acc, Err);
		false ->
		    case file:list_dir(File) of
			{ok, Fs} ->
			    DLs = lists:map(fun(F) ->
						    filename:join(File, F) 
					    end, Fs),
			    dir_files(Files++DLs, Prefix, Acc, Err);
			{error,Reason} ->
			    ?info("data dir: ~s error ~p", [File, Reason]),
			    dir_files(Files, Prefix, Acc, 
				      [{error,{File,Reason}}|Err])
		    end
	    end;
	false ->
	    case filelib:is_regular(File) of
		true ->
		    case skip_file(File, file) of
			true ->
			    dir_files(Files, Prefix, Acc, Err);
			false ->
			    ?info("data file: ~s", [File]),
			    IFile = filename:join(Prefix, File),
			    dir_files(Files, Prefix, [{IFile,File}|Acc], Err)
		    end;
		false ->
		    ?info("data file: ~s not found", [File]),
		    dir_files(Files, Prefix, Acc, [{error,{File,enoent}}|Err])
	    end
    end;
dir_files([],_Prefix, Acc, Err) ->
    {Acc, Err}.
		    

get_control(File) ->
    Ext = filename:extension(File),
    if Ext =:= ".opk"; Ext =:= ".deb" ->
	    {ok,Fd} = erl_ar:open(File),
	    {ok,_}  = erl_ar:read_global_header(Fd),
	    {ok,Size} = erl_ar:find_fd(Fd, "control.tar.gz"),
	    {ok,ControlTGZ} = file:read(Fd, Size),
	    {ok,[{"./control",Control}]} =
		erl_tar:extract({binary,ControlTGZ},
				[compressed,memory,{files,["./control"]}]),
	    erl_ar:close(Fd),
	    {ok,split(Control)};
       Ext =:= ".ipk" ->
	    {ok,[{"./control.tar.gz",ControlTGZ}]} =
		erl_tar:extract(File, [compressed,memory,
				       {files,["./control.tar.gz"]}]),
	    {ok,[{"./control",Control}]} =
		erl_tar:extract({binary,ControlTGZ}, [compressed,memory,
						      {files,["./control"]}]),
	    {ok,split(Control)};
       true ->
	    ?info("File ~s not supported\n", [File]),
	    {error, badarg}
    end.


skip_file(FileName,_Type) ->
    case filename:basename(FileName) of
	".gitignore" -> true;
	".svn" -> true;
	".git" -> true;
	_ -> false
    end.

is_control("control.tar.gz") -> true;
is_control("md5sums") -> true;
is_control("control") -> true;
is_control("conffiles") -> true;
is_control("preinst") -> true;
is_control("postinst") -> true;
is_control("prerm") -> true;
is_control("postrm") -> true;
is_control(_) -> false.

%% Fixme: handle multi line
split(Control) ->
    lists:foldl(fun(Part,Acc) ->
			case binary:split(Part, <<":">>) of
			    [Key,Value] ->
				[{Key, trim(Value)} | Acc];
			    [_] ->
				Acc
			end
		end, [],
		binary:split(Control, <<"\n">>,[global])).

trim(Bin) ->
    trim_b(trim_e(Bin)).

trim_e(Bin) -> trim_e(Bin, byte_size(Bin)-1).

trim_e(_Bin, Pos) when Pos < 0 -> <<"">>;
trim_e(Bin, Pos) ->
    case binary:at(Bin, Pos) of
	$\s -> trim_e(Bin, Pos-1);
	_ -> binary:part(Bin, 0, Pos+1)
    end.

trim_b(Bin) -> trim_b(Bin, 0).

trim_b(Bin, Pos) when Pos >= byte_size(Bin) ->
    <<"">>;
trim_b(Bin, Pos) ->
    case binary:at(Bin, Pos) of
	$\s -> trim_b(Bin, Pos+1);
	_ -> binary:part(Bin, Pos, byte_size(Bin)-Pos)
    end.

%%
%% Parse Depends syntax:
%%   <depends>    = <depend_alt> ( ',' <depend_alt> )*
%%   <depend_alt> = <depend> ( '|' <depend> ) *
%%   <depend> = <package> [ '(' [<op>] version ')']  [ '['[!]<architectur>']' ]
%%
%% Return {[ Alt | Alts ], Rest}
%% Where Alt = [Dep | Deps]
%% Where Dep = {Package,Vsn,Arch}
%% Package = <package-name>
%% Vsn = latest | {<op>,<version>,<revision>}
%% Arch = any | {'!',{<os>,<vsn>}} | {<os>,<vsn>}
%% <os> = any | <name>
%% <vsn> = any | <name>
%% <op> = '<<' | '<=' | '=' | '>=' | '>>' | '<' | '>'
%% 
parse_depends(Depends) ->
    try parse_depends_(Depends) of
	Result -> Result
    catch
	throw:R -> R
    end.

parse_depends_(Depends) ->
    map(
      fun(Depend) ->
	      map(
		fun(Dep) ->
			case parse_depend(trim_string(Dep)) of
			    Err = {error,_} -> throw(Err);
			    {D,""} ->
				D
			end
		end, string:tokens(Depend, "|"))
      end, string:tokens(Depends, ",")).

%%
%% <dep> = <package> [ '(' [<op>] version ')']  [ '['[!]<architectur>']' ]
%% <op> = '<<'   (strict earlier)
%%        '<='   (earlier or equal)
%%        '='    (exact equal)
%%        '>='   (later or equal)
%%        '>>'   (strict later)
%%        '<'    (deprecated - earlier equal)
%%        '>'    (deprecated - later equal)
%%
%% {{Package,Vsn,Arch}, Rest}
%%
parse_depend(Cs) ->
    case parse_package(Cs) of
	Err = {error,_} -> Err;
	{Package,Cs1} ->
	    parse_depend_(Package,trim_head(Cs1))
    end.

parse_depend_(Package,[$(|Cs]) ->
    case trim_head(Cs) of
	[$<,$<|Cs1] ->
	    parse_depend_vsn(Package,'<<',trim_head(Cs1));
	[$<,$=|Cs1] ->
	    parse_depend_vsn(Package,'<=',trim_head(Cs1));
	[$=|Cs1] ->
	    parse_depend_vsn(Package,'=',trim_head(Cs1));
	[$>,$=|Cs1] ->
	    parse_depend_vsn(Package,'>=',trim_head(Cs1));
	[$>,$>|Cs1] ->
	    parse_depend_vsn(Package,'>>',trim_head(Cs1));
	[$<|Cs1] -> %% deprecated
	    parse_depend_vsn(Package,'<=',trim_head(Cs1));
	[$>|Cs1] -> %% deprecated
	    parse_depend_vsn(Package,'>=',trim_head(Cs1));
	Cs1 ->
	    parse_depend_vsn(Package,'=',Cs1)
    end;
parse_depend_(Package,[$[|Cs]) ->
    case trim_head(Cs) of
	[$!|Cs1] ->
	    parse_depend_arch(Package, latest, '!', trim_head(Cs1));
	Cs1 ->
	    parse_depend_arch(Package, latest, none, trim_head(Cs1))
    end;
parse_depend_(Package, Cs) ->
    {{Package, latest, any}, Cs}.

%% parse version after seen '(' 
%% return {{Package,Vsn,Arch}, Rest}
parse_depend_vsn(Package, Op, Cs) ->
    case parse_version(Cs) of
	_Err = {error,_} ->
	    {error, no_version};
	{Vsn,Cs1} ->
	    case trim_head(Cs1) of
		[$)|Cs2] ->
		    case trim_head(Cs2) of
			[$[,$!|Cs3] ->
			    parse_depend_arch(Package,{Op,Vsn}, 
					      '!', trim_head(Cs3));
			[$[|Cs3] ->
			    parse_depend_arch(Package, {Op,Vsn},
					      none, trim_head(Cs3));
			Cs3 ->
			    {{Package,{Op,Vsn},any},Cs3}
		    end;
		_Cs2 -> %% missing ) ?
		    {error,no_version}
	    end
    end.

%% parse version after seen '[' 
%% return {{Package,Vsn,Arch}, Rest}
parse_depend_arch(Package,Vsn,ArchOp,Cs) ->
    case parse_architecture(Cs) of
	Err={error,_} -> Err;
	{Arch,Cs1} ->
	    case trim_head(Cs1) of
		[$]|Cs2] ->
		    Arch1 = if ArchOp =:= none ->
				    Arch;
			       true -> {ArchOp,Arch}
			    end,
		    {{Package,Vsn,Arch1},trim_head(Cs2)};
		_Cs2 ->
		    {error,no_arch}
	    end
    end.
	    
%%
%% <package> = [0-9a-zA-Z][0-9a-zA-Z+-.] (length >= 2)
%% return {Package, Rest}
%% or {error, Reason}
%%
parse_package([X1,X2|Xs]) when 
      ?is_alnum(X1), (?is_alnum(X2) 
		      orelse (X2 =:= $+)
		      orelse (X2 =:= $-)
		      orelse (X2 =:= $.)) ->
    parse_package_(Xs, [X2,X1]);
parse_package(_Xs) ->
    {error,no_package}.

parse_package_(Cs0=[C|Cs], Acc) ->
    if ?is_alnum(C) -> parse_package_(Cs, [C|Acc]);
       C =:= $+;
       C =:= $-;
       C =:= $. -> parse_package_(Cs, [C|Acc]);
       true ->
	    {reverse(Acc), Cs0}
    end;
parse_package_([],Acc) ->
    {reverse(Acc), ""}.

%%
%% <architecture> = <os>'-'<arch> | <arch> = {Os,Arch}
%%
parse_architecture(Cs) ->
    case parse_name(Cs) of
	Err={error,_} -> Err;
	{Nm, [$-|Cs1]} ->
	    case parse_name(Cs1) of
		Err={error,_} -> Err;
		{Nm1, Cs2} -> {{Nm,Nm1},Cs2}
	    end;
	{Nm,Cs1} ->
	    {{any,Nm}, Cs1}
    end.

parse_name([C|Cs]) when ?is_alnum(C) ->
    parse_name(Cs, [C]);
parse_name(_) ->
    {error, no_name}.

parse_name(Cs0=[C|Cs], Acc) ->
    if ?is_alnum(C) -> parse_name(Cs, [C|Acc]);
       true -> {reverse(Acc),Cs0}
    end;
parse_name([],Acc) ->
    {reverse(Acc),""}.

%%
%% <version> = [<epoch>:]<upstream_version>[-<debian_revision>]
%% <epoch> = [0-9]+
%% <upstream_version> = [0-9a-zA-Z][0-9a-zA-Z.+-:~]*  (CHANGED!)
%% <debian_revision> = [0-9a-zA-Z.+~]
%%
%% return {{Epoch,Version,Revision}, Rest}
%% or {error,Reason}
%%
parse_version(String) ->
    case string:to_integer(String) of
	{error, no_integer} ->
	    parse_version(0, String);
	{Epoch,[$:|String1]} ->
	    parse_version(Epoch, String1);
	{_, _Rest} ->
	    parse_version(0, String)
    end.


parse_version(Epoch, String) ->
    case string:rchr(String, $-) of
	0 ->
	    case parse_upstream_version(String) of
		Err = {error,_} -> Err;
		{Vsn, Rest} -> {{Epoch,Vsn,""}, Rest}
	    end;
	I ->
	    {String1,[$-|String2]} = lists:split(I-1, String),
	    case parse_upstream_version(String1) of
		Err = {error,_} -> Err;
		{Vsn, ""} ->
		    case parse_revision(String2) of
			Err = {error,_} -> Err;
			{Rev,Rest1} ->
			    {{Epoch,Vsn,Rev}, Rest1}
		    end;
		{_Vsn,_} ->
		    {error, no_version}
	    end
    end.
	    
%%
%% <upstream_version = [0-9][0-9a-zA-Z.+-:~]*
%% Change to [0-9a-zA-Z][0-9a-zA-Z.+-:~]*
parse_upstream_version([X|Xs]) when ?is_alnum(X) ->
    parse_upstream_version(Xs, [X]);
parse_upstream_version(_) -> {error,no_upstream_version}.

parse_upstream_version(Xs0=[X|Xs],Acc) ->
    if ?is_alnum(X) -> parse_upstream_version(Xs,[X|Acc]);
       X =:= $.; X =:= $+;
       X =:= $-; X =:= $:;
       X =:= $~ -> parse_upstream_version(Xs,[X|Acc]);
       true -> {reverse(Acc), Xs0}
    end;
parse_upstream_version([],Acc) ->
    {reverse(Acc), ""}.

parse_revision(Xs) ->
    parse_revision(Xs,[]).

parse_revision(Xs0=[X|Xs], Acc) ->
    if ?is_alnum(X) -> parse_revision(Xs,[X|Acc]);
       X =:= $.; X =:= $+;
       X =:= $~ -> parse_revision(Xs);
       true -> {reverse(Acc), Xs0}
    end;
parse_revision([], Acc) ->
    {reverse(Acc), ""}.

trim_string(Cs) ->
    reverse(trim_head(reverse(trim_head(Cs)))).

trim_head([$\s|Cs]) -> trim_head(Cs);
trim_head([$\t|Cs]) -> trim_head(Cs);
trim_head([$\n|Cs]) -> trim_head(Cs);
trim_head([$\r|Cs]) -> trim_head(Cs);
trim_head(Cs) -> Cs.
    
%%
%% Compare To versions Vsn1 and Vsn2 
%% return +1  if Vs1 > Vsn2  (later)
%% reutrn  0  if Vs1 == Vsn2
%% return -1  if Vsn1 < Vsn2 (earlier)
%%

compare_version({Epoch1,Vsn1,Rev1},{Epoch2,Vsn2,Rev2}) ->
    if Epoch1 =:= Epoch2 ->
	    case cmpvsn(Vsn1, Vsn2) of
		0 -> 
		    cmpvsn(Rev1, Rev2);
		C -> C
	    end;
       true ->
	    sign(Epoch1 - Epoch2)
    end.

cmpvsn([X|Xs], [Y|Ys]) when (not ?is_digit(X)), (not ?is_digit(Y)) ->
    if X =:= Y -> cmpvsn(Xs,Ys);
       X =:= $~ -> -1;
       Y =:= $~ -> 1;
       true -> sign(X-Y)
    end;
cmpvsn([X|_Xs],[Y|_Ys]) when not ?is_digit(X); not ?is_digit(Y) ->
    if 
	X =:= $~ -> -1;
	Y =:= $~ -> 1;
	?is_digit(X) -> -1;
	true -> 1
    end;
cmpvsn([X|Xs],[Y|Ys]) ->  %% digits
    {Xn,Xs1} = string:to_integer([X|Xs]),
    {Yn,Ys1} = string:to_integer([Y|Ys]),
    if Xn < Yn -> -1;
       Xn > Yn -> 1;
       true -> cmpvsn(Xs1,Ys1)
    end;
cmpvsn([], []) -> 0;
cmpvsn([X|_], []) -> 
    if X =:= $~ -> -1;
       true -> 1
    end;
cmpvsn([], [Y|_]) -> 
    if Y =:= $~ -> 1;
       true -> -1
    end.

sign(X) when X > 0 -> 1;
sign(X) when X < 0 -> -1;
sign(_X) -> 0.
