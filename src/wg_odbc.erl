%%%----------------------------------------------------------------------
%%%
%%% wg copyright © 2009
%%%
%%% @author litaocheng@gmail.com
%%% @doc the odbc module
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(wg_odbc).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("wg.hrl").

-behaviour(gen_server).

%% External exports
-export([start_link/1]).

-export([sql_query/1,
        sql_query_wrap/2,
        sql_query_t/1,
        sql_transaction/1,
        escape/1,
        escape_like/1
    ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
                            terminate/2, code_change/3]).

-record(state, {db_ref, db_type}).

-define(STATE_KEY, wg_odbc_state).
-define(MAX_TRANSACTION_RESTARTS, 10).
-define(PGSQL_PORT, 5432).
-define(MYSQL_PORT, 3306).

-define(KEEPALIVE_QUERY, "SELECT 1;").

%% @doc start the odbc
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%% @doc do the query to the db
sql_query(Query) ->
    gen_server:call(wg_odbc_sup:get_random_pid(),
            {sql_query, Query}, 60000).

%% @doc first do the query, then transform the result with the ConvF
sql_query_wrap(Query, ConvF) ->
    case sql_query(Query) of
        {selected, _ColNames, Rows} ->
            lists:map(ConvF, Rows);
        {error, R} = E ->
            ?DEBUG2("select ~p error: ~p", [Query, R]),
            E
    end.

%% SQL transaction based on a list of queries
%% This function automatically
sql_transaction(Queries) when is_list(Queries) ->
    F = fun() ->
        lists:foreach(
            fun(Query) ->
                sql_query_t(Query)
            end,
        Queries)
    end,
    sql_transaction(F);

%% SQL transaction, based on a erlang anonymous function (F = fun)
sql_transaction(F) ->
    gen_server:call(wg_odbc_sup:get_random_pid(),
                        {sql_transaction, F}, 60000).

%% This function is intended to be used from inside an sql_transaction:
sql_query_t(Query) ->
    State = get(?STATE_KEY),
    QRes = sql_query_internal(State, Query),
    case QRes of
    {error, "No SQL-driver information available."} ->
        % workaround for odbc bug
        {updated, 0};
    {error, _} ->
        throw(aborted);
    Rs when is_list(Rs) ->
	    case lists:keymember(error, 1, Rs) of
		true ->
		    throw(aborted);
		_ ->
		    QRes
	    end;
	_ ->
	    QRes
    end.

%%
%% Util functions
%%

%% @doc Escape character that will confuse an SQL engine
escape(S) when is_list(S) ->
    [escape_ch(C) || C <- S].

%% @doc Characters to escape
escape_ch($\0) -> "\\0";
escape_ch($\n) -> "\\n";
escape_ch($\t) -> "\\t";
escape_ch($\b) -> "\\b";
escape_ch($\r) -> "\\r";
escape_ch($')  -> "\\'";
escape_ch($")  -> "\\\"";
escape_ch($\\) -> "\\\\";
escape_ch(C)   -> C.

%% @doc Escape character that will confuse an SQL engine
%% Percent and underscore only need to be escaped for pattern matching like
%% statement
escape_like(S) when is_list(S) ->
    [escape_like(C) || C <- S];
escape_like($%) -> "\\%";
escape_like($_) -> "\\_";
escape_like(C)  -> odbc_queries:escape(C).

%%
%% gen_server callbacks
%%
init({KeepaliveInterval, StartInterval, ODBCServer}) ->
    ?DEBUG2("init the wg_odbc server ~p", [self()]),
    timer:send_interval(KeepaliveInterval * 1000, self(), keep_alive),

    case ODBCServer of
        %% Default pgsql port
        {pgsql, Server, DB, Username, Password} ->
            pgsql_connect(Server, ?PGSQL_PORT, DB, Username, Password, StartInterval);
        {pgsql, Server, Port, DB, Username, Password} when is_integer(Port) ->
            pgsql_connect(Server, Port, DB, Username, Password, StartInterval);
        %% Default mysql port
        {mysql, Server, DB, Username, Password} ->
            mysql_connect(Server, ?MYSQL_PORT, DB, Username, Password, StartInterval);
        {mysql, Server, Port, DB, Username, Password} when is_integer(Port) ->
            mysql_connect(Server, Port, DB, Username, Password, StartInterval);
        ODBCDsn when is_list(ODBCDsn) ->
            odbc_connect(ODBCDsn, StartInterval)
    end.


handle_call({sql_query, Query}, _From, State) ->
    Reply = sql_query_internal(State, Query),
    {reply, Reply, State};
handle_call({sql_transaction, F}, _From, State) ->
    Reply = execute_transaction(State, F, ?MAX_TRANSACTION_RESTARTS),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
%% We receive the down signal when we loose the MySQL connection (we are
%% monitoring the connection)
%% => We exit and let the supervisor restart the connection.
handle_info({'DOWN', _MonitorRef, process, _Pid, _Info}, State) ->
    {stop, connection_dropped, State};
handle_info({timeout, _Ref, keep_alive}, State) ->
    sql_query_internal(State, ?KEEPALIVE_QUERY),
    ?DEBUG2("keep alive check the db connection", []),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%
%% Internal functions
%%

%% sql query internal
sql_query_internal(State, Query) ->
    ?DEBUG2("sql query : ~p. ~p", [Query, State]),
    case State#state.db_type of
    odbc ->
        odbc:sql_query(State#state.db_ref, Query);
    pgsql -> %% ?timeout
        pgsql_to_odbc(pgsql:squery(State#state.db_ref, Query));
    mysql -> %% ?timeout
        mysql_to_odbc(mysql_conn:fetch(State#state.db_ref, Query, self()))
    end.

%%
execute_transaction(_State, _F, 0) ->
    {aborted, restarts_exceeded};
execute_transaction(State, F, NRestarts) ->
    put(?STATE_KEY, State),
    sql_query_internal(State, "begin;"),
    case catch F() of
        aborted ->
            execute_transaction(State, F, NRestarts - 1);
        {'EXIT', Reason} ->
            sql_query_internal(State, "rollback;"),
            {aborted, Reason};
        Res ->
            sql_query_internal(State, "commit;"),
            {atomic, Res}
    end.

%% == pure ODBC code

%% part of init/1
%% Open an ODBC database connection
odbc_connect(SQLServer, StartInterval) ->
    application:start(odbc),
    case odbc:connect(SQLServer,[{scrollable_cursors, off}]) of
        {ok, Ref} ->
            ?DEBUG2("ODBC connection (~s) ok: ~p~n", [SQLServer, Ref]),
            erlang:monitor(process, Ref),
            {ok, #state{db_ref = Ref, db_type = odbc}};
        {error, Reason} ->
            ?ERROR2("ODBC connection (~s) failed: ~p~n", [SQLServer, Reason]),
            %% If we can't connect we wait before retrying
            timer:sleep(StartInterval),
            {stop, odbc_connection_failed}
    end.


%% == Native PostgreSQL code

%% part of init/1
%% Open a database connection to PostgreSQL
pgsql_connect(Server, Port, DB, Username, Password, StartInterval) ->
    case pgsql:connect(Server, DB, Username, Password, Port) of
	{ok, Ref} ->
	    erlang:monitor(process, Ref),
	    {ok, #state{db_ref = Ref, db_type = pgsql}};
	{error, Reason} ->
	    ?ERROR2("PostgreSQL connection failed: ~p~n", [Reason]),
	    %% If we can't connect we wait before retrying
	    timer:sleep(StartInterval),
	    {stop, pgsql_connection_failed}
    end.

%% Convert PostgreSQL query result to Erlang ODBC result formalism
pgsql_to_odbc({ok, PGSQLResult}) ->
    case PGSQLResult of
	[Item] ->
	    pgsql_item_to_odbc(Item);
	Items ->
	    [pgsql_item_to_odbc(Item) || Item <- Items]
    end.

pgsql_item_to_odbc({"SELECT", Rows, Recs}) ->
    {selected,
     [element(1, Row) || Row <- Rows],
     [list_to_tuple(Rec) || Rec <- Recs]};
pgsql_item_to_odbc("INSERT " ++ OIDN) ->
    [_OID, N] = string:tokens(OIDN, " "),
    {updated, list_to_integer(N)};
pgsql_item_to_odbc("DELETE " ++ N) ->
    {updated, list_to_integer(N)};
pgsql_item_to_odbc({error, Error}) ->
    {error, Error};
pgsql_item_to_odbc(_) ->
    {updated,undefined}.

%% == Native MySQL code

%% part of init/1
%% Open a database connection to MySQL
mysql_connect(Server, Port, DB, Username, Password, StartInterval) ->
    case mysql_conn:start(Server, Port, Username, Password, DB, fun log/3) of
	{ok, Ref} ->
	    erlang:monitor(process, Ref),
            mysql_conn:fetch(Ref, ["set names 'utf8';"], self()), 
	    {ok, #state{db_ref = Ref, db_type = mysql}};
	{error, Reason} ->
	    ?ERROR2("MySQL connection failed: ~p~nWaiting ~p seconds before retrying...~n",
		       [Reason, StartInterval div 1000]),
	    %% If we can't connect we wait before retrying
	    timer:sleep(StartInterval),
	    {stop, mysql_connection_failed}
    end.

%% Convert MySQL query result to Erlang ODBC result formalism
mysql_to_odbc({updated, MySQLRes}) ->
    {updated, mysql:get_result_affected_rows(MySQLRes)};
mysql_to_odbc({data, MySQLRes}) ->
    mysql_item_to_odbc(mysql:get_result_field_info(MySQLRes),
		       mysql:get_result_rows(MySQLRes));
mysql_to_odbc({error, MySQLRes}) when is_list(MySQLRes) ->
    {error, MySQLRes};
mysql_to_odbc({error, MySQLRes}) ->
    {error, mysql:get_result_reason(MySQLRes)}.

%% When tabular data is returned, convert it to the ODBC formalism
mysql_item_to_odbc(Columns, Recs) ->
    %% For now, there is a bug and we do not get the correct value from MySQL
    %% module:
    {selected,
     [element(2, Column) || Column <- Columns],
     [list_to_tuple(Rec) || Rec <- Recs]}.

%% log function used by MySQL driver
log(Level, Format, Args) ->
    case Level of
        debug ->
            ?DEBUG2(Format, Args);
        normal ->
            ?INFO2(Format, Args);
        error ->
            ?ERROR2(Format, Args)
    end.
