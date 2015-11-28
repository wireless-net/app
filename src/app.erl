%%%-------------------------------------------------------------------
%%% @author Lumenosys Robotics <dbutter@lumenosys.com>
%%% @copyright (C) 2014, Lumenosys Robotics
%%% @doc
%%%
%%% @end
%%% Created : 25 Aug 2014 by Lumenosys Robotics <dbutter@lumenosys.com>
%%%-------------------------------------------------------------------
-module(app).

-export([load/2, unload/2, reload/2, start/2, stop/2]).

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

%% @doc Given an application name, find all application dependencies,
%% modules, and app spec files, and load all onto the remote node.
%% @end
-spec load(Node, Application) -> [atom()] | 'error' when
	Node :: node(),
	Application :: atom().

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


%% @doc Stop the specified application on the specified remote node.
%%
%% NOTE: this function only unloads the specified application, not
%% all dependencies!
%% @end
-spec unload(Node, Application) -> 'ok' | {'error', Reason} when
	Node :: node(),
	Application :: atom(),
	Reason :: term().

unload(Node, Application) ->
    rpc:call(Node, application, unload, [Application]).

%% @doc Force a complete stop, unload, and load for the specified
%% application. This is useful in situation where you need to change
%% things which cannot be done by hot swapping, such as application
%% environment parameters, etc.
%% @end
-spec reload(Node, Application) -> 'ok' | 'error' when
	Node :: node(),
	Application :: atom().

reload(Node, Application) ->
    ok = case stop(Node, Application) of
             ok ->
                 io:format(" ...app stopped~n"),
                 ok;
             {error,{not_started, Application}} ->
                 io:format(" ...app not running~n"),
                 ok
             %% otherwise we crash here (something went wrong) 
         end,
    ok = unload(Node, Application),
    load(Node, Application).

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
	    start_apps(Node, List),
	    error;
	_ ->
	    io:format("failed: ~p~n", [Reply]),
	    error
    end.
	    

%% @doc Load and start the specified application onto the specified remote
%% node.
%% @end
-spec start(Node, Application) -> 'ok' | 'error' when
	Node :: node(),
	Application :: atom().

start(Node, Application) ->
    AppList = load(Node, Application),
    start_apps(Node, AppList).


%% @doc  Stop the specified application on the specified remote node.
%% 
%% NOTE: this function only stops the top level application, not all
%% dependencies!
%% @end
-spec stop(Node, Application) -> 'ok' | {'error', Reason} when
	Node :: node(),
	Application :: atom(),
	Reason :: term().

stop(Node, Application) ->
    rpc:call(Node, application, stop, [Application]).

%%% @doc Simple code_change command for applications which don't implement
%%% OTP behaviours
%%% @end
% code_change(Node, Application) ->
%     rpc:call(Node, Application, code_change, []).
