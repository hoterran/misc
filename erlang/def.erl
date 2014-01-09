-module(def).
-compile(export_all).

-define(MYSQL_RDS_EXECED_MAXROWS, 2805).
-define(EXECEEDS_MAXROWS(N), {?MYSQL_RDS_EXECED_MAXROWS, "Number of resultset exceeeds the limit "++ integer_to_list(N)}).


start() ->
	io:format("~p", [?EXECEEDS_MAXROWS(100)]).
