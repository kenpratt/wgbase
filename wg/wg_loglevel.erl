%%%----------------------------------------------------------------------
%%%
%%% wg copyright © 2009
%%%
%%% @author litaocheng@gmail.com
%%% @doc set the loglevel in the runtime, you can use
%%%  wg_logger to handle log. prefer to use Macros in
%%%  wg.hrl
%%%
%%%----------------------------------------------------------------------
-module(wg_loglevel).
-author('litaocheng@gmail.com').
-vsn('0.1').

-export([set/1]).

-define(LOGMODULE, "wg_logger").

%% @doc Error levels:
%% 0 -> No log
%% 1 -> Critical
%% 2 -> Error
%% 3 -> Warning
%% 4 -> Info
%% 5 -> Debug
-spec set(Loglevel :: pos_integer()) -> 
        {'module', 'wg_logger'} | {'error', any()}.
set(Loglevel) when is_integer(Loglevel) ->
   Forms = compile_string(?LOGMODULE, wg_logger_src(Loglevel)),
   load_logger(Forms, ?LOGMODULE, Loglevel).
                
%% --------------------------------------------------------------  
%% Compile a string into a module and returns the binary
compile_string(Mod, Str) ->
    Fname = filename:join([tempdir(), Mod ++ ".erl"]),
    io:format("wg_logger file is :~s", [Fname]),
    
    {ok, Fd} = file:open(Fname, [read, write]),
    file:write(Fd, Str),
    file:position(Fd, 0),
    case epp_dodger:parse(Fd) of
	{ok, Tree} ->
	    Forms = revert_tree(Tree),
	    file:close(Fd),
	    file:delete(Fname),
	    Forms;
	Error ->
	    file:close(Fd),
	    file:delete(Fname),
	    Error
    end.
   
revert_tree(Tree) ->
    [erl_syntax:revert(T) || T <- Tree].

load_logger(Forms, Mod, Loglevel) ->
    Fname = Mod ++ ".erl",
    case compile:forms(Forms, [binary, {d,'LOGLEVEL',Loglevel}]) of
        {ok, M, Bin} ->
            code:load_binary(M, Fname, Bin);
        Error ->
            {error, Error}
    end.

tempdir() ->
    case os:getenv("TMPDIR") of
        false ->
            "/tmp";
        Value ->
            Value
    end.

%% --------------------------------------------------------------
%% Code of the wg logger, dynamically compiled and loaded
%% This allows to dynamically change log level while keeping a
%% very efficient code.        
wg_logger_src(Loglevel) ->
    L = integer_to_list(Loglevel),
    "-module(wg_logger).
    -author('litaocheng@gmail.com').

    -export([debug_msg/4,
             info_msg/4,
             warning_msg/4,
             error_msg/4,
             critical_msg/4]).

    %% Helper functions
    debug_msg(Module, Line, Format, Args) when " ++ L ++ " >= 5 ->
            notify(info_msg,
                   \"D(~p:~p:~p) : \"++Format++\"~n\",
                   [self(), Module, Line]++Args);
    debug_msg(_,_,_,_) -> ok.

    info_msg(Module, Line, Format, Args) when " ++ L ++ " >= 4 ->
            notify(info_msg,
                   \"I(~p:~p:~p) : \"++Format++\"~n\",
                   [self(), Module, Line]++Args);
    info_msg(_,_,_,_) -> ok.

    warning_msg(Module, Line, Format, Args) when " ++ L ++ " >= 3 ->
            notify(error,
                   \"W(~p:~p:~p) : \"++Format++\"~n\",
                   [self(), Module, Line]++Args);
    warning_msg(_,_,_,_) -> ok.

    error_msg(Module, Line, Format, Args) when " ++ L ++ " >= 2 ->
            notify(error,
                   \"E(~p:~p:~p) : \"++Format++\"~n\",
                   [self(), Module, Line]++Args);
    error_msg(_,_,_,_) -> ok.

    critical_msg(Module, Line, Format, Args) when " ++ L ++ " >= 1 ->
            notify(error,
                   \"C(~p:~p:~p) : \"++Format++\"~n\",
                   [self(), Module, Line]++Args);
    critical_msg(_,_,_,_) -> ok.

    %% Distribute the message to the Erlang error logger
    notify(Type, Format, Args) ->
            LoggerMsg = {Type, group_leader(), {self(), Format, Args}},
            gen_event:notify(error_logger, LoggerMsg).
    ".
