%%%-------------------------------------------------------------------
%%% @author Devin Butterfield <dbutter@db>
%%% @copyright (C) 2014, Devin Butterfield
%%% @doc
%%%
%%% @end
%%% Created : 25 Aug 2014 by Devin Butterfield <dbutter@db>
%%%-------------------------------------------------------------------
-module(app).

-export([load/2, unload/2, reload/2, start/2, stop/2, code_change/2]).

load_it(App) ->
    case application:load(App) of
	ok -> 
	    ok;
	{error, {already_loaded,_}} ->
	    ok;
	{error, Reason} ->
	    {error, Reason}
    end.

get_apps_list([], Apps) ->
    Apps;
get_apps_list([App|Rest], Apps) ->
    case load_it(App) of
	ok ->
	    %% recursively find all modules
	    {ok, AppDeps} = application:get_key(App, applications),
	    MoreApps = lists:filter(fun(X) -> 
					    X =/= stdlib 
						andalso X =/= crypto 
						andalso X =/= kernel 
				    end, 
				    AppDeps),
	    get_apps_list(MoreApps ++ Rest, MoreApps ++ Apps);
	{error, Reason} ->
	    {error, Reason}
    end.

get_mods_list([], Modules) ->
    Modules;
get_mods_list([App|Rest], Modules) ->
    {ok, MoreMods} = application:get_key(App, modules),
    get_mods_list(Rest, Modules ++ MoreMods).

get_appspec_list([], AppSpecs) ->
    AppSpecs;
get_appspec_list([App|Rest], AppSpecs) ->
    {ok,NewSpec} = file:consult(code:where_is_file(atom_to_list(App) ++ ".app")),
    get_appspec_list(Rest, AppSpecs ++ NewSpec).
	
load_modules(_Node, []) ->
    ok;
load_modules(Node, [Mod|List]) ->
    {Mod, Bin, File} = code:get_object_code(Mod),
    io:format("==> Loading module ~p ...",[Mod]),
    Reply = rpc:call(Node, code, load_binary, [Mod, File, Bin]),
    case Reply of
	{module, Mod} ->
	    io:format("ok~n"),
	    load_modules(Node, List);
	_ ->
	    io:format("failed: ~p~n", [Reply])
    end.
    
load_appspecs(_Node, []) ->
    ok;
load_appspecs(Node, [AppSpec|List]) ->
    io:format("==> Loading app spec ..."),
    Reply = rpc:call(Node, application, load, [AppSpec]),
    case Reply of
	ok ->
	    io:format("ok~n"),
	    load_appspecs(Node, List);
	{error, {already_loaded, _}} ->
	    io:format("already loaded~n"),
	    load_appspecs(Node, List);
	_ ->
	    io:format("failed: ~p~n", [Reply])
    end.

%%%
%%% Given an application name, find all application dependencies,
%%% modules, and app spec files, and load all onto the remote node.
%%% 
load(Node, Application) ->
    %% first get list of all applications our app depends on
    AppsList = get_apps_list([Application], []) ++ [Application],
    io:format("Found applications: ~p~n", [AppsList]),
    %% then get complete list of all application modules
    ModsList = get_mods_list(AppsList, []),
    io:format("Found modules: ~p~n", [ModsList]),
    %% next get list of all application specifications
    AppSpecs = get_appspec_list(AppsList, []),
    %% now load all modules
    case net_adm:ping(Node) of
    	pong ->
	    load_modules(Node, ModsList),
	    load_appspecs(Node, AppSpecs),
	    AppsList;
	pang ->
	    io:format("Failed to connect to ~p~n", Node),
	    error
    end.

%%%
%%% Stop the specified application on the specified remote node.
%%%
%%% NOTE: this function only unloads the top level application, not
%%% all dependencies!
%%% 
unload(Node, Application) ->
    rpc:call(Node, application, unload, [Application]).

%%
%% Force a complete stop, unload, load, and start for the specified
%% application. This is useful in situation where you need to change
%% things which cannot be done by hot swapping, such as application
%% environment parameters, etc.
%%
reload(Node, Application) ->
    ok = stop(Node, Application),
    ok = unload(Node, Application),
    start(Node, Application).

start_apps(_Node, []) ->
    ok;
start_apps(Node, [App|List]) ->
    io:format("==> Starting application ~p ...", [App]),
    Reply = rpc:call(Node, application, start, [App]),
    case Reply of
	ok ->
	    io:format("ok~n"),
	    start_apps(Node, List);
	{error, {already_started, _}} ->
	    io:format("already started~n"),
	    start_apps(Node, List);
	_ ->
	    io:format("failed: ~p~n", [Reply])
    end.
	    


%%%
%%% Load and start the specified application onto the specified remote
%%% node.
%%% 
start(Node, Application) ->
    AppList = load(Node, Application),
    start_apps(Node, AppList).


%%%
%%% Stop the specified application on the specified remote node.
%%% 
%%% NOTE: this function only stops the top level application, not all
%%% dependencies!
%%% 
stop(Node, Application) ->
    rpc:call(Node, application, stop, [Application]).

%%%
%%% Simple code_change command for applications which don't implement
%%% OTP behaviours
%%%
code_change(Node, Application) ->
    rpc:call(Node, Application, code_change, []).
