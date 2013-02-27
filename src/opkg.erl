%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%    Simple opkg (-cl) frontend
%%% @end
%%% Created : 27 Feb 2013 by Tony Rogvall <tony@rogvall.se>

-module(opkg).

-export([list/0,
	 list_installed/0,
	 list_upgradeable/0,
	 install/1,
	 remove/1]).

-define(OPKG, "opkg-cl").
	 
list() ->
    command_binary_lines(?OPKG ++ " list").

list_installed() ->
    command_binary_lines(?OPKG ++ " list-installed").

list_upgradeable() ->
    command_binary_lines(?OPKG ++ " list-upgradable").

install(Pkgs) ->
    command_binary_lines(?OPKG ++ " install " ++ 
			     lists:join(Pkgs, " ")).

remove(Pkgs) ->
    command_binary_lines(?OPKG ++ " remove " ++ 
			     lists:join(Pkgs, " ")).


command_binary_lines(Cmd) ->
    binary:split(command_binary(Cmd), <<"\n">>, [global]).

command_binary(Cmd) ->
    list_to_binary(command(Cmd)).

command(Cmd) ->
    os:cmd(Cmd).




