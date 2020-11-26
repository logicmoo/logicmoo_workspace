/*  Part of SWI-Prolog

    Author:        Mike Elston
                   Matt Lilley
    E-mail:        matt.s.lilley@gmail.com
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2014, Mike Elston, Matt Lilley
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

/*  PostgreSQL is a trademark of the PostgreSQL Global Development Group.
    Microsoft, SQL Server, and Windows are either registered trademarks or
    trademarks of Microsoft Corporation in the United States and/or other
    countries. SQLite is a registered trademark of Hipp, Wyrick & Company,
    Inc in the United States. All other trademarks or registered trademarks
    are the property of their respective owners.
*/

:-module(cql_database,
         [get_transaction_context/5,
          odbc_execute_with_statement_cache/7,
          save_database_event/6,
          application_value_to_odbc_value/7,
          odbc_value_to_application_value/5,
          cql_transaction/3,
          database_transaction_query_info/3,
          current_transaction_id/1,
          transaction_active/0,
          register_database_connection_details/2,
          resolve_deadlock/1,
          database_connection_details/2,
          odbc_connection_call/3,
          update_history/14,
          odbc_cleanup_and_disconnect/1]).

:-use_module(library(cql/cql)).

:-dynamic
        database_connection_details/2.
:-volatile
        database_connection_details/2.

:-thread_local
        database_event/6,
        transaction_active/0,
        transaction_context/4,
        database_transaction_query_info/3.

get_transaction_context(TransactionId, TrxId, AccessToken, TransactionTimestamp, Connection) :-
        ( transaction_context(TransactionId_, AccessToken_, TransactionTimestamp_, Connection_) ->
            TransactionId = TransactionId_,
            TrxId = {null},
            AccessToken = AccessToken_,
            TransactionTimestamp = TransactionTimestamp_,
            Connection = Connection_

        ; otherwise ->
            throw(no_database_transaction_active)
        ).


:-meta_predicate
        odbc_connection_call(+, -, 0).

:-thread_local
        % odbc_connection_available(Schema, Connection)
        odbc_connection_available/2,
        % odbc_connection_in_use(Schema)
        odbc_connection_in_use/1.

:-multifile(cql_max_db_connections_hook/1).
:-multifile(cql:odbc_connection_complete_hook/3).
odbc_connection_call(Schema, Connection, Goal) :-
        ( retract(odbc_connection_available(Schema, Connection)) ->                       % Get a connection from the pool
            assert(odbc_connection_in_use(Schema)),
            setup_call_cleanup(true,
                               Goal,
                               ( odbc_end_transaction(Connection, rollback),              % Ensure connections in the pool have no pending results dangling
                                 retract(odbc_connection_in_use(Schema)),
                                 assert(odbc_connection_available(Schema, Connection))))  % Put connection back in the pool
        ; aggregate_all(r(count), odbc_connection_in_use(Schema), r(N)),
          ( cql_max_db_connections_hook(MaxDbConnections)->
              true
          ; otherwise->
              MaxDbConnections = 10
          ),
          N >= MaxDbConnections ->
            thread_self(ThreadId),

            cql_error(too_many_schema_connections, 'Too many connections on ~w: Maximum is ~w', [ThreadId, MaxDbConnections])

        ; database_connection_details(Schema, ConnectionDetails) ->
            ( ConnectionDetails = driver_string(DriverString) ->
                true

            ; ConnectionDetails = dsn(Dsn, Username, Password) ->
                gethostname(HostName),
                format(atom(DriverString), 'DSN=~w;UID=~w;PWD=~w;WSID=~w;', [Dsn, Username, Password, HostName])

            ; ConnectionDetails = dsn(Dsn) ->
                gethostname(HostName),
                format(atom(DriverString), 'DSN=~w;WSID=~w;', [Dsn, HostName])

            ; otherwise ->
                throw(invalid_connection_details(ConnectionDetails))
            ),

            odbc_connect(-,
                         Connection,
                         [driver_string(DriverString),
                          silent(true),
                          null({null}),
                          auto_commit(false),
                          wide_column_threshold(8000),
                          mars(true)]),    % In theory this is not needed following bug-5181 but see comment in predicate description

            thread_at_exit(odbc_cleanup_and_disconnect(Connection)),
            assert(odbc_connection_available(Schema, Connection)),

            ignore(cql:odbc_connection_complete_hook(Schema, ConnectionDetails, Connection)),
            odbc_connection_call(Schema, Connection, Goal)

        ; otherwise ->
            throw(no_database_connection_details)
        ).





%%      odbc_cleanup_and_disconnect(+Connection) is det.
%
%       Rollback the current transaction, retract and free prepared statements, then disconnect.
%
%       To avoid leaks, all exiting threads with database connections should call this.  See odbc_connection_call/2 (thread_at_exit/1)
%
%       Note that any exception inside odbc_cleanup_and_disconnect/1 will result in it not going on to the next step.
%
%       We log exceptions to the event log because exceptions at this level are associated with the server process crashing
%       and the SE log is unlikely to capture anything useful.

odbc_cleanup_and_disconnect(Connection) :-
        catch_all(odbc_cleanup_and_disconnect_1(Connection),
                  E,
                  ( thread_self(ThreadId),
                    cql_log([], error, '[~w] odbc_cleanup_and_disconnect/1 : ~w', [ThreadId, E]))).

odbc_cleanup_and_disconnect_1(Connection) :-
        thread_self(ThreadId),
        debug(odbc_cleanup, 'BEFORE [~w] : ~w', [ThreadId, odbc_end_transaction(Connection, rollback)]),
        odbc_end_transaction(Connection, rollback),
        debug(odbc_cleanup, 'AFTER  [~w] : ~w', [ThreadId, odbc_end_transaction(Connection, rollback)]),
        forall(retract(cached_prepared_odbc_statement(_, _, Connection, _, _, CachedStatement, _)),
               ( debug(odbc_cleanup, 'BEFORE [~w] : ~w', [ThreadId, odbc_free_statement(CachedStatement)]),
                 odbc_free_statement(CachedStatement),
                 debug(odbc_cleanup, 'AFTER  [~w] : ~w', [ThreadId, odbc_free_statement(CachedStatement)])
               )
              ),
        retractall(lru_key(_)),
        retractall(lru_statement(_)),
        debug(odbc_cleanup, 'BEFORE [~w] : ~w', [ThreadId, odbc_disconnect(Connection)]),
        odbc_disconnect(Connection),
        debug(odbc_cleanup, 'AFTER  [~w] : ~w', [ThreadId, odbc_disconnect(Connection)]),
        % Get rid of these last so there is some evidence if odbc_disconnect/1 does not work
        retractall(sql_server_spid(Connection, _, _, _)).

%!      odbc_execute_with_statement_cache(+Connection, +FileName, +LineNumber, +Sql, +OdbcParameters, +OdbcParameterDataTypes, -Row)

:-thread_local
        % cached_prepared_odbc_statement(Sql, OdbcParameterDataTypes, Connection, FileName, LineNumber, Statement, MutexId)
        % Note that we need OdbcParameterDataTypes in here so we can look up the correct statement. Consider:
        % "SELECT * FROM some_table WHERE some_column = ?"
        % This can be compiled with varchar(4) as the datatype, to get statement S,
        % If we then want to do a query where user_id = 'ERIC' we are going to get a runtime type error.
        % Ordinarily this isn't a problem because the domain is well-established at compile-time, but this
        % is not the case when dealing with dynamic tables, specifically #cql_in.
        cached_prepared_odbc_statement/7.

:-thread_local
        lru_statement/1,
        statement_locked/1,
        lru_key/1.

max_lru_size(4000).

% This is called with the current statement locked
evict_cache_entries(_, 0):- !.
evict_cache_entries(Key, N):-
        N > 0,
        retract(lru_statement(MutexId)),
        % This statement cannot be locked unless the cache size is extremely small, since we JUST cycled the current statement to the bottom of the stack
        ( statement_locked(MutexId)->
            % Just do nothing in this case. We will get it next time. Besides, it is very unlikely to happen
            true
        ; otherwise->
            thread_self(ThreadId),
            retract(cached_prepared_odbc_statement(Sql, _, _, _, _, Statement, MutexId)),
            odbc_free_statement(Statement),
            debug(odbc_statement_cache, 'CACHE-EVICT [~w] ~w : ~@', [ThreadId, Statement, trimmed_sql(Sql, 80)]),
            flag(Key, X, X-1)
        ),
        NN is N-1,
        !,
        evict_cache_entries(Key, NN).


odbc_execute_with_statement_cache(Connection, _, _, Sql, OdbcParameters, OdbcParameterDataTypes, Row) :-
        cached_prepared_odbc_statement(Sql, OdbcParameterDataTypes, Connection, _, _, Statement, MutexId),
        !,
        setup_call_cleanup(assert(statement_locked(MutexId)),
                           ( thread_self(ThreadId),
                             retract(lru_statement(MutexId)),
                             assertz(lru_statement(MutexId)),
                             debug(odbc_statement_cache, 'CACHE-HIT [~w] ~w : ~@', [ThreadId, Statement, trimmed_sql(Sql, 80)]),
                             odbc_execute_with_statistics(Statement, OdbcParameters, OdbcParameterDataTypes, Row)
                           ),
                           retract(statement_locked(MutexId))).

odbc_execute_with_statement_cache(Connection, FileName, LineNumber, Sql, OdbcParameters, OdbcParameterDataTypes, Row) :-
        thread_self(ThreadId),
        debug(odbc_statement_cache, 'CACHE-MISS [~w] : ~@', [ThreadId, trimmed_sql(Sql, 80)]),
        odbc_prepare(Connection, Sql, OdbcParameterDataTypes, Statement, []),
        gensym(statement_lock_, MutexId),
        ( lru_key(Key)->
            true
        ; otherwise->
            gensym(lru_key_, Key),
            assert(lru_key(Key))
        ),
        setup_call_cleanup(assert(statement_locked(MutexId)),
                           ( assertz(cached_prepared_odbc_statement(Sql, OdbcParameterDataTypes, Connection, FileName, LineNumber, Statement, MutexId)),
                             assertz(lru_statement(MutexId)),
                             max_lru_size(MaxSize),
                             flag(Key, CacheSize, CacheSize+1),
                             ( CacheSize >= MaxSize->
                                 Delta is CacheSize - MaxSize,
                                 evict_cache_entries(Key, Delta)
                             ; otherwise->
                                 true
                             ),
                               flag(Key, Z, Z),
                               debug(odbc_statement_cache, 'CACHE-STORE [~w] ~w, ~w : ~@', [ThreadId, Statement, MutexId, trimmed_sql(Sql, 60)]),
                               odbc_execute_with_statistics(Statement, OdbcParameters, OdbcParameterDataTypes, Row)
                           ),
                             retract(statement_locked(MutexId))).



%%      save_database_event(+AccessToken,
%%                          +EventType,
%%                          +Schema,
%%                          +TableName,
%%                          +PrimaryKeyColumnName,
%%                          +PrimaryKey).
%
%       Need this because its called from the caller's module and we want the fact asserted
%       in this module
save_database_event(AccessToken,                             % +
                    EventType,                               % +
                    Schema,                                  % +
                    TableName,                               % +
                    PrimaryKeyColumnName,                    % +
                    PrimaryKey) :-                           % +
        ( database_event(AccessToken, EventType, Schema, TableName, PrimaryKeyColumnName, PrimaryKey) ->
            % No point storing an event more than once
            true
        ; otherwise->
            assert(database_event(AccessToken, EventType, Schema, TableName, PrimaryKeyColumnName, PrimaryKey))
        ).


:-meta_predicate(cql_transaction(+, +, 0)).

cql_transaction(Schema, AccessToken, Goal):-
        thread_self(ThreadId),
        setup_call_cleanup(assert(transaction_active),
                           cql_transaction_1(Schema, AccessToken, Goal, DatabaseEventsSet),
                           ( retractall(database_transaction_query_info(ThreadId, _, _)),
                             retractall(transaction_context(_, _, _, _)),
                             retractall(database_event(_, _, _, _, _, _)),
                             flag(transaction_count, Count, Count+1),
                             retractall(transaction_active))),   % Removed last so if transaction_active succeeds while executing Goal then the other facts are still available to Goal
        cql_process_database_events(DatabaseEventsSet).

cql_transaction_1(Schema, AccessToken, Goal, DatabaseEventsSet):-
        ( transaction_context(ExistingTransactionId, _, _, _) ->
            throw(database_transaction_already_in_progress(ExistingTransactionId))
        ; otherwise ->
            true
        ),
        resolve_deadlock(cql_transaction_2(Schema, AccessToken, Goal, DatabaseEventsSet)).

cql_transaction_2(Schema, AccessToken, Goal, DatabaseEventsSet) :-
        odbc_connection_call(Schema,
                             Connection,
                             ( ( dbms(Schema, 'Microsoft SQL Server')->
                                   odbc_query(Connection, 'SELECT CONVERT(VARCHAR(36), NEWID())', row(TransactionId))
                               ; dbms(Schema, 'PostgreSQL') ->
                                   odbc_query(Connection, 'SELECT uuid_generate_v1()', row(TransactionId))
                               ; dbms(Schema, 'SQLite') ->
                                   odbc_query(Connection, 'SELECT substr(u,1,8)||\'-\'||substr(u,9,4)||\'-4\'||substr(u,13,3)||\'-\'||v||substr(u,17,3)||\'-\'||substr(u,21,12) from (select lower(hex(randomblob(16))) as u, substr(\'89ab\',abs(random()) % 4 + 1, 1) as v)', row(TransactionId))
                               ; otherwise ->
                                   throw(no_dbms_for_schema(Schema))
                               ),
                                 dbms(Schema, DBMS),
                               store_transaction_info(AccessToken, Connection, DBMS, Goal),
                               get_time(ExecutionTime),
                               assert(transaction_context(TransactionId, AccessToken, ExecutionTime, Connection)),

                               ( cql_transaction_3(Goal, Connection, TransactionId, AccessToken, DatabaseEventsSet) ->
                                   true
                               ; otherwise ->
                                   % odbc_connection_call/3 always rolls back so no need for explicit rollback here
                                   log_transaction_state(AccessToken, TransactionId, transaction_rolled_back_on_logic_failure),
                                   fail
                               ))).


:-meta_predicate
        cql_transaction_3(0, +, +, +, -).

cql_transaction_3(Goal, Connection, TransactionId, AccessToken, DatabaseEventsSet) :-
        log_transaction_state(AccessToken, TransactionId, transaction_starting),
        catch(Goal, E, Error = E),
        % Note that this previously did a setof/3. This reorders events, which breaks event consolidation
        findall(database_event(AccessToken, EventType, Schema, TableName, PrimaryKeyColumnName, PrimaryKey),
                retract(database_event(AccessToken, EventType, Schema, TableName, PrimaryKeyColumnName, PrimaryKey)),
                DatabaseEvents),
        % list_to_set/2 is NlogN and preserves order
        list_to_set(DatabaseEvents, DatabaseEventsSet),
        ( var(Error) ->
            odbc_end_transaction(Connection, commit),
            log_transaction_state(AccessToken, TransactionId, transaction_committed)

        ; otherwise ->
            % odbc_connection_call/3 always rolls back so no need for explicit rollback here
            log_transaction_state(AccessToken, TransactionId, transaction_rolled_back_on_error),
            throw(Error)
        ).


%%      resolve_deadlock(:Goal)
%
%       Call Goal as in catch/3.  If a deadlock ('40001') error occurs then Goal is *|called again|* immediately if another transaction has completed in the
%       time since Goal was called, since that transaction may well have been the reason for the deadlock.
%       If no other transaction has completed Goal is *|called again|* after a random delay of 0.0 to 2.0 seconds.  The maximum number of retries
%       is specified by maximum_deadlock_retries/1.  It is important to note that the deadlock mechanism actually *|retries|* Goal, i.e. it calls it
%       *|again|*.
%
%       *|Use this only when you are sure Goal has no non-database side effects (assert/retract, file operations etc)|*
%
%       Originally developed for use inside cql_transaction/3, resolve_deadlock/1 can also be used to ensure non-transactional
%       operations can resolve deadlocks.

:-meta_predicate
        resolve_deadlock(0).

resolve_deadlock(Goal) :-
        thread_self(ThreadId),
        flag(transaction_count, InitialCount, InitialCount),

        maximum_deadlock_retries(MaximumDeadlockRetries),
        between(1, MaximumDeadlockRetries, RetryCount),         % BTP for deadlock retry

        ( RetryCount >= MaximumDeadlockRetries ->
            cql_log([debug(deadlocks)], warning, 'DEADLOCK_RESOLUTION_FAILED\tCOULD NOT RESOLVE deadlock on thread \'~w\'.  Goal:  ~w', [ThreadId, Goal]),
            throw(deadlock_retry_count_exceeded(MaximumDeadlockRetries))

        ; RetryCount > 1 ->
            % Check if another transaction has completed.  Note complete means committed -or- rolled back
            flag(transaction_count, CurrentCount, CurrentCount),
            ( CurrentCount =:= InitialCount ->
                Flag = no_other_transaction_completed
            ; otherwise ->
                Flag = another_transaction_completed
            )

        ; otherwise ->
            Flag = no_deadlock
        ),

        ( Flag == no_other_transaction_completed ->
            Delay is ( 2 << RetryCount) / 1000.0,         % Exponential backoff up to 2.048s
            sleep(Delay),
            cql_log([debug(deadlocks)], warning, 'DEADLOCK_RESOLUTION_ATTEMPT\tRETRYING deadlocked transaction on thread \'~w\'(attempt ~w).  Initiated by EXPIRY of RANDOM WAIT of ~w seconds.', [ThreadId, RetryCount, Delay])

        ; Flag == another_transaction_completed ->
            cql_log([debug(deadlocks)], warning, 'DEADLOCK_RESOLUTION_ATTEMPT\tRETRYING deadlocked transaction on thread \'~w\' (attempt ~w).  Initiated by COMPLETION of a TRANSACTION on another thread.', [ThreadId, RetryCount])
        ; otherwise ->
            true
        ),

        catch_all((Goal ->
                     LogicalStatus = 1
                  ; otherwise ->
                     true
                  ),
                  error(odbc('40001', _, _), _),
                  ( cql_log([debug(deadlocks)], warning, 'DEADLOCK_DETECTED\tThread \'~w\' selected as DEADLOCK VICTIM.  Goal:  ~w', [ThreadId, Goal]),
                    retractall(database_transaction_query_info(ThreadId, _, _)),
                    retractall(transaction_context(_, _, _, _)),
                    retractall(database_event(_, _, _, _, _, _)),
                    fail)),
        ( RetryCount > 1 ->
            cql_log([debug(deadlocks)], warning, 'DEADLOCK_RESOLVED\tdeadlocked transaction on thread \'~w\' RESOLVED (attempt ~w).', [ThreadId, RetryCount])

        ; otherwise ->
            true
        ),
        !,    % Don't want to backtrack into the deadlock retry between/3 when Goal fails
        LogicalStatus == 1.


%%      maximum_deadlock_retries(?MaximumDeadlockRetries:integer).
%
%       The maximum number of time to retry a deadlocked Goal

maximum_deadlock_retries(10).

%       log_transaction_state(+AccessToken, +TransactionId, +TransactionState)

log_transaction_state(AccessToken, TransactionId, TransactionState) :-
        cql_access_token_to_user_id(AccessToken, UserId),
        upcase_atom(TransactionState, TransactionStateUc),
        cql_log([], informational, '\t~p\t~p\t~p', [UserId, TransactionId, TransactionStateUc]).


%%      register_database_connection_details(+Schema:atom, +ConnectionDetails) is det.
%
%       This should be called once to register the database connection details.
%
%       @param ConnectionDetails driver_string(DriverString) or dsn(Dsn, Username, Password)

register_database_connection_details(Schema, ConnectionDetails) :-
        assert(database_connection_details(Schema, ConnectionDetails)).


update_history(Schema, TableName, AttributeName, PrimaryKeyAttributeName, PrimaryKeyValue, ApplicationValueBefore, ApplicationValueAfter, AccessToken, Info, TransactionId, TransactionTimestamp, ThreadId, Connection, Goal):-
        ignore(cql_update_history_hook(Schema, TableName, AttributeName, PrimaryKeyAttributeName, PrimaryKeyValue, ApplicationValueBefore, ApplicationValueAfter, AccessToken, Info, TransactionId, TransactionTimestamp, ThreadId, Connection, Goal)).



%%      application_value_to_odbc_value(+ApplicationValue, +OdbcDataType, +Schema, +TableName, +ColumnName, +Qualifiers, -OdbcValue).
:-multifile(cql:application_value_to_odbc_value_hook/7).
application_value_to_odbc_value(ApplicationValue, OdbcDataType, Schema, TableName, ColumnName, Qualifiers, OdbcValue):-
        ( var(ApplicationValue)->
            throw(instantiation_error(ApplicationValue))
        ; cql:application_value_to_odbc_value_hook(OdbcDataType, Schema, TableName, ColumnName, Qualifiers, ApplicationValue, OdbcValue)->
            true
        ; otherwise->
            OdbcValue = ApplicationValue
        ).


odbc_numeric_precision_limit(27).


%%      odbc_value_to_application_value(+Schema, +TableSpec, +ColumnName, +OdbcValue, ?ApplicationValue).
:-multifile(cql:odbc_value_to_application_value_hook/7).
odbc_value_to_application_value(Schema, TableSpec, ColumnName, OdbcValue, ApplicationValue):-
        cql_data_type(Schema, TableSpec, ColumnName, DatabaseDataType, _, _, _, Domain, _, _),
        !,
        ( cql:odbc_value_to_application_value_hook(DatabaseDataType, Schema, TableSpec, ColumnName, Domain, OdbcValue, ApplicationValue)->
            true
        ; otherwise->
            ApplicationValue = OdbcValue
        ).

% FIXME: What to do about this?
catch_all(A, B, C):- catch(A, B, C).


:-multifile(cql:process_database_events/1).
cql_process_database_events(Events):-
        ignore(cql:process_database_events(Events)).

:-multifile(cql:cql_transaction_info_hook/5).
store_transaction_info(AccessToken, Connection, DBMS, Goal):-
        ( cql:cql_transaction_info_hook(AccessToken, Connection, DBMS, Goal, Info)->
            true
        ; otherwise->
            Info = {null}
        ),
        thread_self(ThreadId),
        assert(database_transaction_query_info(ThreadId, Goal, Info)).

%%      current_transaction_id(-TransactionId).

current_transaction_id(TransactionId):-
        transaction_context(TransactionId, _, _, _).
