%%%----------------------------------------------------------------------
%%%
%%% @copyright wg 2009
%%%
%%% @author litao cheng <litaocheng@gmail.com>
%%% @doc handle the configure info(use ets), the config file format is
%%%  the erlang term (example.conf):
%%%  {key1, value1}.
%%%  {key2, value2}.
%%%
%%%----------------------------------------------------------------------
-module(wg_config).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("crawl.hrl").
-behaviour(gen_server).

-export([start_link/2, start_link/3]).
-export([get/2, get/3]).
%-export([set/2, set/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
                            terminate/2, code_change/3]).

-type config_src() :: {'file', string()} | {'str', string()}.
-type config_mode() :: 'read_only' | 'read_write'.
-type config_opt() ::
      {'filename', string()}
    | {'mode', config_mode()}.

-type config_opts() :: [config_opt()].

-record(state, {
        mode = 'read_only' :: config_mode(),        % read_only, read_write 
        file = "" :: string()                       % the file name
    }).
        
%% @doc start the config
-spec start_link(Name :: atom(), Src :: config_src()) ->
    {'ok', pid()} | 'ignore' | {'error', any()}.
start_link(Name, Src) ->
    start_link(Name, Src, []).

%% @doc start the config
-spec start_link(Name :: atom(), Src :: config_src(), Opts :: config_opts()) ->
    {'ok', pid()} | 'ignore' | {'error', any()}.
start_link(Name, Src, Opts) ->
    ?DEBUG2("start_link ~p ~p ~n", [Src, Opts]),
    gen_server:start_link({local, Name}, ?MODULE, {Name, Src, Opts}, []).

%% @doc get the key 
-spec get(Name :: atom(), Par :: any()) -> any() | 'undefined'.
get(Name, Par) ->
    case ets:lookup(Name, Par) of
        [{Par, Val}] -> 
            Val;
        [] ->
            undefined
    end.

%% @doc get the key, if the key not exist return the default value
-spec get(Name :: atom(), Par :: any(), Def :: any()) -> any().
get(Name, Par, Def) ->
    case get(Name, Par) of
        undefined ->
            Def;
        Val ->
            Val
    end.

%% gen_server callbacks
init({Name, Src, Opts}) ->
    State = #state{} = parse_opts(Opts), 
    {ok, Configs} = load_config(Src),
    Name = ets:new(Name, [set, protected, named_table, {keypos, 1}, {write_concurrency, true}]),
    [true = ets:insert(Name, KV) || KV <- Configs],
    {ok, State}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.
    
%%-----------------------------------------------------------------------------
%%
%% internal API
%%
%%-----------------------------------------------------------------------------

%% parse the opts
parse_opts(Opts) ->
    File = proplists:get_value(filename, Opts, ""),
    Mode = proplists:get_value(mode, Opts, read_only),
    #state{file = File, mode = Mode}.

%% load the configs
load_config({file, FileName}) ->
    File = 
    case os:getenv("CH_CONFIG_PATH") of
        false ->
            FileName;
        Path ->
            filename:join([Path, FileName])
    end,
    case filelib:is_file(File) of
        true ->
            case file:consult(File) of
                {ok, Config} ->
                    {ok, Config};
                _ ->
                    ?ERROR2("the ~p file format error!~n", [File]),
                    {error, e_conf_format}
            end;
        false ->
            ?ERROR2("not exist ~p config file~n", [File]),
            {error, e_conf}
    end;
load_config({str, Config}) ->
    Lines = string:tokens(Config, "."),
    Config = 
    lists:map(
            fun(L) ->
                case parse_line_str(L) of
                    {error, _R} = Error ->
                        Error;
                    LineTerm ->
                        LineTerm
                end
            end,
            Lines),
    {ok, Config}.

parse_line_str(Str) ->
    case erl_scan:string(Str) of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Config} ->
                    Config;
                _ ->
                    ?ERROR2("error at :~p", [Str]),
                    {error, e_conf_format}
            end;
        _ ->
            {error, e_conf_format}
    end.

-ifdef(EUNIT).
-endif.
