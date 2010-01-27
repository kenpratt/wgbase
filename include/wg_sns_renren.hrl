%%%----------------------------------------------------------------------
%%%
%%% @copyright wg 2009
%%%
%%% @author litao cheng <litaocheng@gmail.com>
%%% @doc the renren platform record defines
%%%
%%%----------------------------------------------------------------------
-define(API_SRV, "http://api.xiaonei.com/restserver.do?").

-type res_format() :: 'JSON' | 'XML'.

%% the renren api object
-record(renren_api, {
        api_key = "" :: string(),           % the api key
        secret = "" :: string(),            % the secret key
        session_key = "" :: string(),       % the session key
        v = "1.0" :: string(),              % the api version
        format = 'JSON' :: res_format(),    % the response format
        raw_res = false :: boolean()           % the response is in raw string(json or xml)
    }).
-type renren_api() :: #renren_api{}.

%% the 'firends.**' response record
-record(renren_friend, {
        uid = 0 :: integer(),
        name = <<>> :: binary(),
        headurl = <<>> :: binary(),
        tinyurl = <<>> :: binary(),
        headurl_with_logo = <<>> :: binary(),
        tinyurl_with_logo = <<>> :: binary()
    }).
-type renren_friend() :: #renren_friend{}.

-record(renren_location, {
        country = <<>> :: binary(),
        province = <<>> :: binary(),
        city = <<>> :: binary()
    }).
-type renren_location() :: #renren_location{}.

-record(renren_work, {
        company_name = <<>> :: binary(),
        description = <<>> :: binary(),
        start_date = <<>> :: binary(),
        end_date = <<>> :: binary()
    }).
-type renren_work() :: #renren_work{}.

-record(renren_school, {
        name = <<>> :: binary(),
        year = 0 :: integer(),
        department = <<>> :: binary()
    }).
-type renren_school() :: #renren_school{}.

%% the 'users.xxx' response record
-record(renren_user, {
        uid = 0 :: integer(),
        name = <<>> :: binary(),
        sex = 1 :: integer(),
        star = 0 :: integer(),
        zidou = 0 :: integer(),
        birthday = <<>> :: binary(),
        email_hash = <<>> :: binary(),
        tinyurl = <<>> :: binary(),
        headurl = <<>> :: binary(),
        mainurl = <<>> :: binary(),
        hometown_location = #renren_location{} :: renren_location(),
        work_history = [] :: [renren_work()],
        university_history = [] :: [renren_school()]
    }).
-type renren_user() :: #renren_user{}.
