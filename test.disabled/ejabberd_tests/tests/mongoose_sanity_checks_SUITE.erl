-module(mongoose_sanity_checks_SUITE).

-compile(export_all).

all() ->
    [is_mongooseim].


is_mongooseim(Config) ->
   Q = [<<"insert into users(username, password) values ('bob', 'makrolika');">>],
   R = escalus_ejabberd:rpc(mongoose_rdbms, sql_query, [<<"localhost">>, Q]),
   ct:pal("insert ~p", [R]),
   R2 = escalus_ejabberd:rpc(mongoose_rdbms, sql_query, [<<"localhost">>, [<<"select * from users;">>]]),
   ct:pal("select ~p", [R2]),
    mongooseim = escalus_server:name(Config).
