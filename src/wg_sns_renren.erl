%%%----------------------------------------------------------------------
%%%
%%% @copyright wg 2009
%%%
%%% @author litao cheng <litaocheng@gmail.com>
%%% @doc the API for renren platform
%%%
%%%----------------------------------------------------------------------
-module(wg_sns_renren).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("wg.hrl").
-include("wg_sns_renren.hrl").

-type uid() :: integer().

-export([new/3, new/5]).
-export([admin/3, feed/3, friends/3, users/3, notifications/3]).

-define(GET2(K, L), (proplists:get_value(K, L))).
-define(GET3(K, L, Def), (proplists:get_value(K, L, Def))).

%% @doc create an new renren api object, the response format is json
-spec new(ApiKey :: string(), Secret :: string(), Session :: string()) ->
    renren_api().
new(ApiKey, Secret, Session) when is_list(ApiKey), is_list(Secret), is_list(Session) ->
    new(ApiKey, Secret, Session, 'JSON', false).

%% @doc create an new renren api object 
-spec new(ApiKey :: string(), Secret :: string(), Session :: string(), Format :: res_format(), Raw :: boolean()) ->
    renren_api().
new(ApiKey, Secret, Session, Format, Raw) 
    when is_list(ApiKey), is_list(Secret), is_list(Session) ->
    #renren_api{
        api_key = ApiKey,
        secret = Secret,
        session_key = Session,
        format = Format,
        raw_res = Raw}.

%% @doc admin APIs
%% Method: getAllocation 
%% Return: {ok, {notifications_per_day, requests_per_day}}
%%
-spec admin(Method :: atom(), Params :: list(), Api :: renren_api()) ->
    {'ok', {integer(), integer()}} | {'error', any(), any()}.
admin('getAllocation' = M, [], Api) -> 
    http_post(admin, M, [], fun rsp_admin/3, Api). 

%% @doc feed APIS
%% Method :: 'publishTemplatizedAction', 
%% Params :: [template_id, title_data, body_data]
%%
%% example:
%% Method = 'publishTemplatizedAction', 
%% Params =  [1, "{\"score\":90}", "{\"content\":\"go!\"}"],
%% feed(Method, Params, RenApi)
%%
-spec feed(Method :: atom(), Params :: list(), RenApi :: renren_api()) ->
    {'ok', boolean()} | {'error', any(), any()}.
feed('publishTemplatizedAction' = M, Params, Api) ->
    KVs = lists:zip(["template_id", "title_data", "body_data"], Params),
    http_post(feed, M, KVs, fun rsp_feed/3, Api).

%% @doc the friend related API
-spec friends
    ('areFriends', Params :: list(), Api :: renren_api()) ->
        {'ok', [{uid(), uid(), boolean()}]} | {'error', any(), any()};
    ('get', Params :: list(), Api :: renren_api()) ->
        {'ok', [uid()]} | {'error', any(), any()};
    ('getFriends', Params :: list(), Api :: renren_api()) ->
        any().
    
%% P : [UIds1, Uids2]
%% return : {ok, [{UId1, UId2, Bool}]}
friends('areFriends' = M, [_UIds1, _Uids2] = P, Api) ->
    Query = lists:zip(["uids1", "uids2"], P),
    http_post(friends, M, Query, fun rsp_friends/3, Api);

friends('get' = M, P, Api) ->
    Query =
    case P of
        [] -> 
            []; 
        [Page, Count] -> 
            [{"page", Page}, {"count", Count}]
    end,
    http_post(friends, M, Query, fun rsp_friends/3, Api);
friends('getFriends' = M, P, Api) ->
    Query =
    case P of
        [] ->
            [];
        [Page, Count] -> 
            [{"page", Page}, {"count", Count}]
    end,
    http_post(friends, M, Query, fun rsp_friends/3, Api);
friends('getAppUsers' = M, _P, Api) ->
    http_post(friends, M, [], fun rsp_friends/3, Api);
friends('getAppFriends' = M, P, Api) ->
    Query =
    case P of
        [] ->
            [];
        [Fields] ->            
            FieldsStr = string:join(Fields, ","),
            [{"fields", FieldsStr}]
    end,
    http_post(friends, M, Query, fun rsp_friends/3, Api).

%% @doc notifications API 
-spec notifications(Method :: atom(), Params :: list(), Api :: renren_api()) ->
    {'ok', boolean()} | {'error', any(), any()}.
notifications('send' = M, [ToIds, Notif], Api) ->
    IdsStr = string:join(ToIds, ","),
    Query = [{"to_ids", IdsStr}, {"notification", Notif}],
    http_post(notifications, M, Query, fun rsp_notifications/3, Api).

%% @doc the user related API
-spec users(Method :: atom(), Params :: list(), Api :: renren_api()) ->
    {'ok', any()} | {'error', any(), any()}.
users('getInfo' = M, P, Api) ->
    Query = 
    case P of
        [Uids] ->
            IdsStr = string:join(Uids, ","),
            [{"uids", IdsStr}];
        [Uids, Fields] ->
            IdsStr = string:join(Uids, ","),
            FieldsStr = string:join(Fields, ","),
            [{"uids", IdsStr}, {"fields", FieldsStr}]
    end,
    http_post(users, M, Query, fun rsp_users/3, Api);
users('getLoggedInUser' = M, [], Api) ->
    http_post(users, M, [], fun rsp_users/3, Api);
users('hasAppPermission' = M, [_] = P, Api) ->
    Query = lists:zip(["ext_perm"], P),
    http_post(users, M, Query, fun rsp_users/3, Api);
users('isAppUser' = M, P, Api) ->
    Query = 
    case P of
        [] ->
            [];
        [Uid] ->
            [{"uid", Uid}]
    end,
    http_post(users, M, Query, fun rsp_users/3, Api).

    
%%-----------------------------------------------------------------------------
%%
%% internal API
%%
%%-----------------------------------------------------------------------------

%% parse admin response
rsp_admin('getAllocation', 'JSON', {struct, RList}) ->
    NN = ?GET2(<<"notifications_per_day">>, RList),
    RN = ?GET2(<<"requests_per_day">>, RList),
    {ok, {NN, RN}}.

%% parse feed response
rsp_feed('publishTemplatizedAction', 'JSON', {struct, RList}) ->
    Bool = ?GET2(<<"result">>, RList),
    {ok, n2bool(Bool)}.

%% parse friends response
rsp_friends('areFriends', 'JSON', {struct, RList}) ->
    Ret =
    lists:map(
        fun({struct, PL}) ->
                U1 = ?GET2(<<"uid1">>, PL),
                U2 = ?GET2(<<"uid2">>, PL),
                IsF = ?GET2(<<"are_friends">>, PL),
            {U1, U2, IsF}
        end,
        RList),
    {ok, Ret};
rsp_friends('get', 'JSON', RList) ->
    {ok, RList};
rsp_friends('getFriends', 'JSON', RList) ->
    {ok, 
    lists:map(
        fun({struct, PL}) ->
                #renren_friend{
                   uid = ?GET2(<<"id">>, PL), 
                   name = ?GET2(<<"name">>, PL), 
                   headurl = ?GET3(<<"headurl">>, PL, <<>>), 
                   tinyurl = ?GET3(<<"tinyurl">>, PL, <<>>), 
                   headurl_with_logo = ?GET3(<<"headurl_with_logo">>, PL, <<>>), 
                   tinyurl_with_logo = ?GET3(<<"tinyurl_with_logo">>, PL, <<>>) 
                }
        end,
        RList)};
rsp_friends('getAppUsers', 'JSON', RList) ->
    {ok, RList};
rsp_friends('getAppFriends', 'JSON', []) ->
    {ok, []};
rsp_friends('getAppFriends', 'JSON', [_|_] = RList) ->
    case hd(RList) of
        V when is_integer(V) ->
            {ok, RList};
        V when is_tuple(V) ->
            {ok, 
            lists:map(
                fun({struct, PL}) ->
                    #renren_friend{
                        uid = ?GET2(<<"uid">>, PL), 
                        name = ?GET2(<<"name">>, PL), 
                        headurl = ?GET3(<<"headurl">>, PL, <<>>), 
                        tinyurl = ?GET3(<<"tinyurl">>, PL, <<>>) 
                    }
                end,
                RList)}
    end.

rsp_notifications('send', 'JSON', {struct, RList}) ->
    {ok, b2bool(?GET2(<<"result">>, RList))}.

rsp_users('getInfo', 'JSON', RList) ->
    Ret =
    lists:map(
        fun({struct, PL}) ->
                {struct, Location} = ?GET3(<<"hometown_location">>, PL, {struct, []}),
                WorkHistory = ?GET3(<<"work_history">>, PL, []),
                SchoolHistory = ?GET3(<<"university_history">>, PL, []),
                gen_renren_user(PL, Location, WorkHistory, SchoolHistory)
        end,
        RList),
    {ok, Ret};

rsp_users('getLoggedInUser', 'JSON', {struct, [{<<"uid">>, Uid}]}) ->
    {ok, Uid};
rsp_users('hasAppPermission', 'JSON', {struct, RList}) ->
    {ok, n2bool(?GET2(<<"result">>, RList))};
rsp_users('isAppUser', 'JSON', {struct, RList}) ->
    {ok, n2bool(?GET2(<<"result">>, RList))}.

http_post(Type, Method, Params, FHandler, Api) ->    
    %?DEBUG2("http post type:~p method:~p params:~p api:~p", [Type, Method, Params, Api]),
    % do the request
    Query = Params 
            ++ session_key(Api#renren_api.session_key) 
            ++ [{"api_key", Api#renren_api.api_key},
                {"call_id", call_id()},
                {"method", lists:concat(["xiaonei.", Type, '.', Method])},
                {"v", Api#renren_api.v},
                {"format", Api#renren_api.format}
            ],

    Sig = gen_sig(Query, Api#renren_api.secret),
    PostData = mochiweb_util:urlencode([{"sig", Sig}] ++ Query),
    ?DEBUG2("post data is :~p", [PostData]),

    {ok, {200, Response}} = wg_httpc:post(?API_SRV, PostData),
    ?DEBUG2("raw respons is:~s", [Response]),
    
    % handle the response
    catch http_response(Method, Api#renren_api.format, Response, FHandler, Api).

gen_sig(Query, Secret) ->
    %?DEBUG2("Query is :~p", [Query]),
    Sorted = lists:sort(Query),
    Str = iolist_to_binary([ [lists:concat([K, "=", V]) || {K, V} <- Sorted], Secret]),
    <<N:128>> = erlang:md5(Str),
    [Sig] = io_lib:format("~.16b", [N]),
    Sig.

call_id() ->
    {M,S,U} = erlang:now(),
    n2s(1000000 * (M*1000000 + S) + U). 

http_response(Method, 'JSON', Response, FHandler, Api) ->
    case Api#renren_api.raw_res of
        true ->
            {ok, Response};
        false ->
            Json = mochijson2:decode(Response),
            %?DEBUG2("json is:~p", [Json]),
            case Json of
                {struct, RList} ->
                    case ?GET2(<<"error_code">>, RList) of
                        E when E =/= undefined -> % error
                            throw({error, ?GET2(<<"error_code">>, RList), ?GET2(<<"error_msg">>, RList)});
                        _ ->
                            ok
                    end;
                _ ->
                    ok
            end,
            FHandler(Method, 'JSON', Json)
    end.


gen_renren_user(PL, Location, WorkHistory, SchoolHistory) ->
    #renren_user{
        uid = ?GET2(<<"uid">>, PL),
        name = ?GET2(<<"name">>, PL),
        sex = ?GET2(<<"sex">>, PL),
        star = ?GET2(<<"star">>, PL),
        zidou = ?GET2(<<"zidou">>, PL),
        birthday = ?GET2(<<"birthday">>, PL),
        email_hash = ?GET2(<<"email_hash">>, PL),
        tinyurl = ?GET2(<<"tinyurl">>, PL),
        headurl = ?GET2(<<"headurl">>, PL),
        mainurl = ?GET2(<<"mainurl">>, PL),
        hometown_location = 
            #renren_location{
                country = ?GET2(<<"country">>, Location),
                province = ?GET2(<<"province">>, Location),
                city = ?GET2(<<"city">>, Location)
            },
        work_history = 
        lists:map(fun({struct, Work}) ->
                    #renren_work{
                        company_name = ?GET2(<<"company_name">>, Work),
                        description =  ?GET2(<<"description">>, Work),
                        start_date = ?GET2(<<"start_date">>, Work),
                        end_date = ?GET2(<<"end_date">>, Work)
                    }
                end,
                WorkHistory),
        university_history =
        lists:map(fun({struct, School}) ->
                    #renren_school{
                        name = ?GET2(<<"name">>, School),
                        year = ?GET2(<<"year">>, School),
                        department = ?GET2(<<"department">>, School)
                    }
                end,
                WorkHistory)
    }.


session_key("") ->
    [];
session_key([_|_] = S) ->
    [{"session_key", S}].
            
b2bool(<<"1">>) -> true;
b2bool(<<"0">>) -> false.

n2bool(1) -> true;
n2bool(0) -> false.

n2s(N) ->
    integer_to_list(N).
    
-ifdef(EUNIT).
-endif.
