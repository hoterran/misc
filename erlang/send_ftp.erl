#!/usr/local/bin/escript

%%%
%%% AUTHOR:		ruoyi.ruanry@alibaba-inc.com
%%% FEATURE:	send possible inject sql to ftp
%%%

%-define(FTP_SERVER, "1.1.1.1").
-define(FTP_SERVER, "0.0.0.0").
-define(FTP_PORT, 21).
%-define(FTP_USER, "tftpd").
-define(FTP_USER, "xxx").
%-define(FTP_PW, "tftpd").
-define(FTP_PW, "yyy").
-define(FTP_FILE, "ftp").
-define(FTP_LOCAL_DIR, "/tmp/").

-define(HEADER, "logs").
-define(ENTRY, "log").
-define(USER, "u").
-define(TIMESTAMP, "ts").
-define(QUERYTIME, "qt").
-define(QUERY, "qr").

-record(la_record, {user, timestamp, 'query', query_time, response_time}).

myerror(Info) ->
	io:format("Failed, Exited with ~p~n", [Info]).

main([Directory]) ->
	io:format("start ~p~n", [erlang:now()]),
	{ok, AppList} = file:list_dir(Directory),
	lists:foreach(fun(Dir) -> handle_dir(Directory, Dir) end, AppList),
	
	send_to_ftp(Directory),
	os:cmd("rm -rf "++ Directory ++ "/ftp*"),
	ok.

%%
%% send ftp_* file to ftp server
%%
send_to_ftp(Dir) ->
	{ok, FtpFileList} = file:list_dir(Dir),
	inets:start(),
	{ok, Pid} = inets:start(ftpc, [{host, ?FTP_SERVER}, {port, ?FTP_PORT}]),
	ok = ftp:user(Pid, ?FTP_USER, ?FTP_PW),

	ftp:lcd(Pid, Dir),
	io:format("~p~n", [FtpFileList]),
	lists:foreach(fun(F) -> ftp:send(Pid, F) end, 
		lists:filter(fun(File) -> lists:prefix(?FTP_FILE, File) end, FtpFileList)),

	inets:stop(ftpc, Pid).

%%
%% Dir = ladir/*
%%  filter . | .. | regular file
%%
handle_dir(Base, App) ->
	%io:format("here ~p~n", [Dir]),
	{ok, FileInfo} = file:read_file_info(Base ++ "/" ++ App),
	%%conclude is directory or file
	Type  = erlang:element(3, FileInfo),
	if Type =/= directory ->
		%io:format("only handle directory");
		ok;
	true ->
		%% is directory
		case lists:prefix(App, ".") of
			true ->
				%io:format("this directory not need handle~n");
				ok;
			false ->
				{ok, SqlFileList} = file:list_dir(Base ++ "/" ++ App),
				lists:foreach(fun(SqlFile) -> handle_file(Base, App, SqlFile) end, SqlFileList)
		end
	end.

%%File = sql_*.dat.*,  but not sql*_stats.dat*
%%
handle_file(Base, App, File) ->
	%io:format("here2 ~p ~n", [Dir ++ "/" ++  File]),
	case re:run(File, "\\d.dat") of
		{match, _} ->
			case re:run(File, "_stats.dat|dumpslow|pos|error") of 
				{match, _} ->
					ok;
					%io:format("~p not need handle~n", [File]);
				nomatch ->
					case re:run(File, "^ftp-") of
						{match, _} ->
							ok;
						nomatch ->
							handle_sql_file(Base, App, File)
					end
			end;
		nomatch ->
			ok
			%io:format("~p not need handle ~n", [File])
	end.

%%
%% FromFilename = sql..
%% ToFilename = ftp_sql...
%%
handle_sql_file(Base, App, File) ->
	FromFileName = Base ++ "/" ++ App ++ "/" ++ File,
	{{Y,M,D}, _} = erlang:localtime(),
        S = integer_to_list(Y) ++ integer_to_list(M) ++ integer_to_list(D),
        ToFileName = Base ++ "/" ++ ?FTP_FILE ++ "-"++ App ++ "-" ++ S ++ "-" ++ File,  
	
	%io:format("here3 ~p ~p~n", [FromFileName, ToFileName]),
	case file:read_file_info(FromFileName) of
		{error, Reason} -> myerror(Reason);
		none -> error("no read privilage");
		_ ->
			case parse_file(FromFileName, ToFileName) of
				{error, Reason} -> myerror(Reason);
				{no_need} ->ok;
				Entries ->
					io:format("Success, Total Entries ~p~n", [Entries])
		end
	end.

% file not exits, pos = 0, inode = 0
load_pos(FromFileName) ->
	io:format("~p~n", [FromFileName]),
	case file:open(FromFileName++ ".pos", [read]) of
		{ok, Fd} ->
			case file:read(Fd, 100) of
				{ok, Data} -> binary_to_term(list_to_binary(Data));
				_ -> [0, 0]
			end;
		{error,enoent} -> [0, 0]
	end.

%% save Pos Inode 
%% if Inode change, means a new file, so pos = 0
save_pos(FromFileName, Pos) ->
	Inode = get_inode(FromFileName),
	{ok, Fd} = file:open(FromFileName ++ ".pos", [write]),
	ok = file:write(Fd, term_to_binary([Pos, Inode])),
	ok = file:close(Fd),
	io:format("~p ~p ~p ~n", [FromFileName, Pos, Inode]),
	ok.

get_inode(FileName) ->
	case file:read_file_info(FileName) of
	{ok, FileInfo} ->
		element(12, FileInfo);
	{error, Reason} ->
		0
	end.

parse_file(FromFilename, ToFilename) ->
	case file:open(FromFilename, [raw, binary, read]) of
		{error, Reason} ->
			{error, Reason};
		{ok, FromFD} ->
			CurInode = get_inode(FromFilename),			
			[Pos, Inode] = load_pos(FromFilename),
			case CurInode == Inode of
				true ->
					TruePos = Pos;
				false ->
					TruePos = 0
			end,

			%io:format("1 ~p ~p~n", [Pos, FromFilename]),
			file:position(FromFD, TruePos),
			%io:format("3 ~p ~n", [FromFilename]),
			case file:open(ToFilename, [raw, write]) of
				{error, Reason} ->
					file:close(FromFD),
					{error, Reason};
				{ok, ToFD} ->
					add_header(ToFD),
					Res = parse_log_entries(FromFD, ToFD),
					file:close(FromFD),
					io:format("2 ~p ~p~n", [
FromFilename, filelib:file_size(FromFilename)]), 
					save_pos(FromFilename, filelib:file_size(FromFilename)),
					case Res of
						{error, Reason} ->
							file:close(ToFD),
							file:delete(ToFilename),
							{error, Reason};
						 0 ->
							%%no inject sql, so not need write file
							file:close(ToFD),
							file:delete(ToFilename),
							{no_need};
						 _ ->
							add_tail(ToFD),
							file:close(ToFD),
							Res
					end
			end
	end.

parse_log_entries(FromFD, ToFD) ->
	parse_log_entries(FromFD, ToFD, 0).

parse_log_entries(FromFD, ToFD, OEntries) ->
	case file:read(FromFD, 4) of
		{ok, <<_TS:32>>} ->
			{ok, <<_NO:32>>} = file:read(FromFD, 4),
			{ok, <<Size:32>>} = file:read(FromFD, 4),
			{ok, <<Binary/binary>>} = file:read(FromFD, Size),
			{ok, <<255:8>>} = file:read(FromFD, 1),
			Type = case OEntries of
				0 -> first;
				_ -> next
			end,
			LARecord = binary_to_term(Binary),
			%io:format("LARecord: ~p~n", [LARecord]),
			case write_json_file(ToFD, Type, LARecord) of
				{error, Reason} -> {error, Reason};
				{noneed, _} -> parse_log_entries(FromFD, ToFD, OEntries);
				_ -> parse_log_entries(FromFD, ToFD, OEntries + 1)
			end;
		eof -> OEntries
	end.

write_json_file(ToFD, Type, #la_record{user = User, timestamp = TS, query_time = QueryTime, 'query' = Query}) ->
	%add filter
	%"into outfile" | "load_file" | "version(), user(), database()" | "information_schema" | "benchmark, sleep"
	case re:run(Query, "^select", [caseless]) of
		nomatch ->
			{noneed, no_select};
		{match, _} ->
			case re:run(Query, "into outfile" ++ "|" ++ 
				"load_file" ++ "|" ++ 
				"version\\(\\)" ++ "|" ++
				"user\\(\\)" ++ "|" ++
				"system_user\\(\\)" ++ "|" ++
				"session_user\\(\\)" ++ "|" ++
				"current_user\\(\\)" ++ "|" ++
				"database\\(\\)" ++ "|" ++
				"INFORMATION_SCHEMA" ++ "|" ++
				"benchmark\\(" ++ "|" ++
				"sleep\\(", [no_auto_capture, caseless]) of
				{match, _} ->
					case re:run(Query, "^select database\\(\\)$|^select version\\(\\)$", [caseless]) of
						{match, _} ->	
							{noneed, no_that_sql};
						nomatch ->
							add_record(ToFD, Type, User, TS, QueryTime, Query)
					end;
				nomatch ->
					{noneed, no_that_sql}
			end
	end.

add_header(ToFD) ->
	file:write(ToFD, list_to_binary(io_lib:format("{~p:[~n", [?HEADER]))).
add_tail(ToFD) ->
	file:write(ToFD, list_to_binary(io_lib:format("]}~n", []))).
add_record(ToFD, Type, User, TS, QueryTime, Query) ->
	Record = construct_record(Type, User, TS, QueryTime, Query),
	case file:write(ToFD, Record) of
		{error, Reason} -> {error, Reason};
		_ -> byte_size(Record)
	end.
construct_record(first, User, TS, QueryTime, Query) ->
	list_to_binary(io_lib:format("    {~p:{~p:~p, ~p:~p, ~p: \"~s\"}}",
		[?ENTRY, ?TIMESTAMP, TS, ?QUERYTIME, QueryTime, ?QUERY, Query]));
	%list_to_binary(io_lib:format("    {~p:{~p:~p, ~p:~p, ~p:~p, ~p: \"~s\"}}",
		%[?ENTRY, ?USER, User, ?TIMESTAMP, TS, ?QUERYTIME, QueryTime, ?QUERY, Query]));
construct_record(next, User, TS, QueryTime, Query) ->
	list_to_binary(io_lib:format(",~n {~p:{~p:~p, ~p:~p, ~p: \"~s\"}}",
		[?ENTRY, ?TIMESTAMP, TS, ?QUERYTIME, QueryTime, ?QUERY, Query])).
	%list_to_binary(io_lib:format(",~n {~p:{~p:~p, ~p:~p, ~p:~p, ~p: \"~s\"}}",
	%	[?ENTRY, ?USER, User, ?TIMESTAMP, TS, ?QUERYTIME, QueryTime, ?QUERY, Query])).

