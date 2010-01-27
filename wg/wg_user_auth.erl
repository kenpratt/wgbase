%%%----------------------------------------------------------------------
%%%
%%% wg copyright © 2009 
%%%
%%% @author litao cheng <litaocheng@gmail.com>
%%% @doc the user auth module
%%%
%%%----------------------------------------------------------------------
-module(wg_user_auth).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("wg.hrl").

-export([key/2]).
-export([decode/1]).

-define(KEY, <<"^|O2", 16#28, 16#31, 16#d5, 16#82>>).
-define(IVec, <<16#ad, 16#76, 16#1a, 16#49, 16#dc, 16#28, 16#e1, 16#31>>).

%% @doc use the  user and pwd generate user key
-spec key(User :: binary(), Pwd :: binary()) -> binary().
key(User, Pwd) when is_binary(User), is_binary(Pwd) ->
    Text = <<(byte_size(User)):8/integer, User/bytes, (byte_size(Pwd)):8/integer, Pwd/bytes>>,
    base64:encode(Text).

key1(User, Pwd) when is_binary(User), is_binary(Pwd) ->
    ULen = byte_size(User),
    PLen = byte_size(Pwd),
    Pad = 
    case (ULen + PLen + 2) rem 8 of
        0 -> 0;
        N -> 8 - N
    end,
    
    Text1 = <<ULen, User/binary, PLen, Pwd/binary, 0:Pad/unit:8>>,
    Cipher1 = crypto:des_cbc_encrypt(?KEY, ?IVec, Text1),
    base64:encode(Cipher1).

%% @doc decode the key obtain the  user and pwd
-spec decode(Cipher :: binary()) -> 
    {'ok', wg_token()} | {'error', atom()}.
decode(Cipher) ->
    Text = base64:decode(Cipher),
    <<Len:8/integer, User:Len/bytes, PLen:8/integer, Pwd:PLen/bytes>> = Text,
    {ok, #wg_token{user = User, pwd = Pwd}}.

decode1(Cipher) ->
    Text1 = base64:decode(Cipher),
    case catch crypto:des_cbc_decrypt(?KEY, ?IVec, Text1) of
        <<ULen, User:ULen/binary, PLen, Pwd:PLen/binary, _Pad/binary>> ->
            {ok, #wg_token{user=User, pwd=Pwd}};
        _ ->
            {error, decode}
    end.
    
-ifdef(EUNIT).

basic_test_() ->
    crypto:start(),
    [
        ?_assertEqual({ok, #wg_token{user= <<"litao">>, pwd= <<"cheng">>}}, 
            decode(key(<<"litao">>, <<"cheng">>))),
        ?_assertEqual({ok, #wg_token{user= <<"litao">>, pwd= <<"">>}}, 
            decode(key(<<"litao">>, <<"">>))),
        ?_assertEqual({ok, #wg_token{user= <<"">>, pwd= <<"">>}}, 
            decode(key(<<"">>, <<"">>))),
        ?_assertEqual({ok, #wg_token{user= <<"xxx#S">>, pwd= <<"">>}}, 
            decode(key(<<"xxx#S">>, <<"">>)))
    ].
-endif.
