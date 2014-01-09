-module(mysql_proxy).

-behaviour(gen_fsm).

-export([start/0, 
	start_child/1,
	handle_client_connect/2, 
	handle_client_auth/2, 
	handle_connect_backend/2,
	handle_backend_handshake/2, 
	handle_backend_authresult/2, 
	handle_client_command/2, 
	handle_backend_result/2]).

-define(USERNAME,	"abc").
-define(PASSWORD,	"root").
-define(DBNAME,		"test").

-define(BACKSERVER, "127.0.0.1").
-define(BACKPORT,   3306).
-define(BACKUSER,	<<"root">>).
-define(BACKPW,		<<"root">>).

-record(state, {listen, client, server, cbuffer, sbuffer, 
				tid, scramble, packetnumber, maxpacketsize, clientflags, charset,
				username, dbname}).

-export([init/1, handle_event/3, handle_info/3, handle_sync_event/4, code_change/4, terminate/3]).            

%% proxy start here
start() ->
	{ok, ListenSocket} = gen_tcp:listen(3307, [binary, {active, false}, {reuseaddr, true}]),
	State = #state{tid=1, packetnumber=0, listen=ListenSocket},
	start_child(State).

start_child(State) ->
	gen_fsm:start(?MODULE, [State], []).

init([State]) ->
	gen_fsm:send_event(self(), accept),
	{ok, handle_client_connect, State}.

%%
handle_client_connect(accept, State=#state{listen=ListenSocket, tid=Tid, packetnumber=PacketNumber}) ->
	%io:format("accept, ~p", [self()]),
	case gen_tcp:accept(ListenSocket) of
	{ok, Client} ->
		%%?
		gen_tcp:controlling_process(Client, self()), 
		spawn(?MODULE, start_child, [State#state{tid=Tid+1}]),
		io:format("handle_client_connect----------------------------------------------~n~n"),
		io:format("client connect proxy: ~n"),
		{HandShake, Scramble} = encode_packet(handshake, Tid),
		HandShakePacket = encode_packet(0, HandShake),
		io:format("proxy send client: scramble [~p][~p] ~n", [binary_to_list(Scramble), Client]),
		gen_tcp:send(Client, HandShakePacket),
		gen_fsm:send_event(self(), read_auth),
		{next_state, handle_client_auth, State#state{scramble=Scramble, 
			packetnumber=1, client=Client, cbuffer= <<>> }};
	{'EXIT', Pid, Why} -> 
		io:format("fuck~n");
	{error, Reason} ->
		io:format("why ~p ~p~n", [self(), Reason])
	end.

handle_client_auth(read_auth, State=#state{scramble=Scramble, client=Client}) ->
	case gen_tcp:recv(Client, 0) of 
		{ok, RecvBinary} -> 
			io:format("handle_client_auth----------------------------------------------~n~n"),
			%io:format("recv ~p~n", [RecvBinary]),
			%%TODO half data possible
			{PacketLength, PacketNumber, Buffer} = decode_packet(RecvBinary),
			%io:format("------"),
			{ClientFlags, MaxPacketSize, Charset, Rest} = decode_packet(auth, Buffer),

			%% username Null-terminated string
			{UserNameBinary, Rest2} = z_split(Rest),
			UserName = binary_to_list(UserNameBinary),
			%io:format("username = ~p~n", [UserName]),

			%%salt 
			{SaltLen, Rest3} = decode_lcb(Rest2),
			{SaltBinary, Rest4} = decode_lcb(SaltLen, Rest3),
			Salt = binary_to_list(SaltBinary),
			%io:format("salt = ~p~n", [Salt]),
			if Rest4 =:= <<>> ->
				DbNameBinary = <<>>;
			true ->
				{DbNameBinary, <<>>} = z_split(Rest4)
			end,
			DbName = binary_to_list(DbNameBinary),
			%io:format("dbname = ~p~n", [DbName]),

			case UserName =:= ?USERNAME of
			true ->
				case DbName =:= ?DBNAME of
				true ->
					case compare_password(SaltBinary, Scramble, ?PASSWORD) of 
					true ->
					io:format("client send proxy: auth [~p][~p][~p] ~n", [UserName, DbName, ?DBNAME]),
					gen_fsm:send_event(self(), connect_backend),
					{next_state, handle_connect_backend, 
						State#state{clientflags=ClientFlags, maxpacketsize=MaxPacketSize,  charset=Charset,
						username=UserNameBinary, dbname=DbNameBinary, packetnumber=PacketNumber}};

					false ->
					%% take care here packet_number = 2
					gen_tcp:send(Client, encode_packet(2, error_packet(2080, "Password error"))),
					io:format("password error~n")
					end;
				false ->
				gen_tcp:send(Client, encode_packet(2, error_packet(2081, "DB No Exists"))),
				io:format("db not exists~n")
				end;
			false->
				gen_tcp:send(Client, encode_packet(2, error_packet(2082, "User No Exists"))),
				io:format("user not exists~p~n", [UserName])
			end;
		{error, Reason} ->
			io:format("with auth error"),
			{stop, normal, State}
	end.

handle_connect_backend(connect_backend, State = #state{client=Client}) ->
	%%tcp connect mysql
	%%TODO connect error
	%% connect must active
	case gen_tcp:connect(?BACKSERVER, ?BACKPORT, [binary, {packet, 0}, {active, false}]) of
		{ok, Server} ->
			io:format("handle_connect_backend----------------------------------------------~n~n"),
			io:format("proxy connect backend: [~p:~p]~n", [?BACKSERVER, ?BACKPORT]),
			gen_tcp:controlling_process(Server, self()), 
			gen_fsm:send_event(self(), send_handshake),
			{next_state, handle_backend_handshake, State#state{server=Server, sbuffer = <<>>}};
		{error, Reason} ->
			io:format("cant connect backend"),
			gen_tcp:close(Client),
			{stop, normal, State}
	end.

handle_backend_handshake(send_handshake, State = #state{server=Server, 
	clientflags=ClientFlags, maxpacketsize=MaxPacketSize, client=Client,
	charset=Charset, username=UserName, dbname=DbName}) ->
	%%backend send scramble 
	case gen_tcp:recv(Server, 0) of 
		{ok, RecvBinary} ->
		%%TODO half data
			io:format("handle_backend_handshake----------------------------------------------~n~n"),
			{PacketLength, PacketNumber, Binary} = decode_packet(RecvBinary),
			{Salt} = decode_packet(handshake, Binary),
			io:format("backend send proxy: scramble [~p][~n]", [Salt]),
			%%io:format("~p ~p ~p ~p ~p~n", [Salt, ClientFlags, MaxPacketSize, Charset, DbName]),
		%% send auth, pakcet number is 1
			AuthBinary = encode_packet(auth, Salt, ?BACKPW, ClientFlags, MaxPacketSize, Charset, ?BACKUSER, DbName),
			%%io:format("~p~n", [AuthBinary]),
			io:format("proxy send backend: auth [~p][~p]~n", [?BACKSERVER, DbName]),
			gen_tcp:send(Server, encode_packet(1, AuthBinary)),
			gen_fsm:send_event(self(), read_authresult),
			{next_state, handle_backend_authresult, State};
		{error, Reason} ->
			io:format("Server close [~p]~n", [Reason]),
			gen_tcp:close(Client),
			%%TODO close client
			{stop, normal, State}
	end.

handle_backend_authresult(read_authresult, State=#state{client=Client, server=Server}) ->
	%%backend send msg proxy to client

	case gen_tcp:recv(Server, 0) of
		{ok, RecvBinary} ->
			io:format("handle_backend_authresult----------------------------------------------~n~n"),
			%% is ok packet?
			%% is error packet?
			{PacketLength, PacketNumber, Rest} = decode_packet(RecvBinary),	
			io:format("backend send proxy: authresult is [~p] ~n", [Rest]),

			gen_tcp:send(Client, RecvBinary),

			io:format("proxy send client: authresult is [~p] ~n", [Rest]),
			gen_fsm:send_event(self(), read_cmd),
			{next_state, handle_client_command, State};
		{error, Reason} ->
			gen_tcp:close(Client),
			gen_tcp:close(Server),
			io:format("Server close [~p]~n", [Reason]),
			{stop, normal, State}
	end.
%%
%% Now recv data from client, then send it to server
%%
handle_client_command(read_cmd, State = #state{client=Client, server=Server}) ->
	case gen_tcp:recv(Client, 0) of 
		{ok, RecvBinary} ->
			io:format("handle_client_command----------------------------------------------~n~n"),
			%io:format("recv ~p~n", [RecvBinary]),
			%%TODO just check data 
			{PacketLength, PacketNumber, Binary} = decode_packet(RecvBinary),
			{CmdType, Sql} = decode_packet(cmd, Binary),
			io:format("client send proxy: [~p][~p]~n", [CmdType, Sql]),
			gen_tcp:send(Server, RecvBinary),
			io:format("proxy send backend: [~p][~p]~n", [CmdType, Sql]),
			gen_fsm:send_event(self(), read_result),
			{next_state, handle_backend_result, State};
		{error, closed} ->
			gen_tcp:close(Client),
			gen_tcp:close(Server),
			io:format("client connect close"),
			{stop, normal, State}	
	end.

handle_backend_result(read_result, State = #state{client=Client, server=Server}) ->
	%%TODO possible half data
	case gen_tcp:recv(Server, 0) of 
		{ok, RecvBinary} ->
			io:format("handle_backend_result---------------------------------------------~n~n"),
			%io:format("recv1 ~p~n", [RecvBinary]),

			{_, _, Binary} = decode_packet(multi,RecvBinary),
			%io:format("recv2 ~p~n", [Binary]),
			decode_packet(result, Binary),

			%%conclude resultset?
			io:format("backend send proxy: [~p]~n", [RecvBinary]),
			gen_tcp:send(Client, RecvBinary),
			io:format("proxy send client: [~p]~n", [RecvBinary]),
			gen_fsm:send_event(self(), read_cmd),
			{next_state, handle_client_command, State};
		{error, Reason} ->
			io:format("Server connect close ~p~n", [Reason]),
			gen_tcp:close(Server),
			gen_tcp:close(Client),
			{stop, normal, State}
	end.

%% Token client send
%% Scramble server generat salt
%% Password true password
compare_password(Token, Scramble, Password) ->
	crypto:start(),
	%%client ->
	%%Token = SHA1(scramble + SHA1(SHA1(password)) xor SHA1(password)

	%%Token2 = Token XOR (SHA1(scramble) + SHA1( mysql.user.Password))
	%%conclude Token2 == SHA1(SHA1(password))

    if 
    Token =:= <<>>, Password =:= [] -> true;
    Token =:= <<>>, Password =/= [] -> false;
    true ->
        Temp = crypto:sha(Password),
        Mysql_stored_password = crypto:sha(Temp),
		%%io:format("true password is ~p~n", [Mysql_stored_password]),
        Temp1 = crypto:sha_final(
                crypto:sha_update(
                    crypto:sha_update(crypto:sha_init(), Scramble), Mysql_stored_password
                )
            ),
        Stage1_hash = bxor_binary(Temp1, Token),
    
        case crypto:sha(Stage1_hash) == Mysql_stored_password of
            true -> true;
            false -> false
        end 
    end.

%%generate salt
make_token([], _Scramble) -> <<>>;
make_token(Password, Scramble) -> 
	Stage1_hash = crypto:sha(Password),
    Stage2_hash = crypto:sha(Stage1_hash),
    Res = crypto:sha_final(
        crypto:sha_update(
            crypto:sha_update(crypto:sha_init(), Scramble),  
            Stage2_hash)
        ),
    bxor_binary(Res, Stage1_hash).

dualmap(_F, [], []) ->
    [];
dualmap(F, [E1 | R1], [E2 | R2]) ->
    [F(E1, E2) | dualmap(F, R1, R2)].

bxor_binary(B1, B2) ->
    list_to_binary(dualmap(fun (E1, E2) -> E1 bxor E2 end, binary_to_list(B1), binary_to_list(B2))). 

encode_packet(auth, Scramble, ServerPw, ClientFlags, MaxPacketSize, Charset, ServerUser, DbName) ->
	ServerUserLength = byte_size(ServerUser), 
	DbNameLength = byte_size(DbName), 
	Salt = make_token(ServerPw, Scramble),

	io:format("~p~n", [Salt]),

	<<ClientFlags:32/little, MaxPacketSize:32/little,
	Charset:8/little, 0:184/little, 
	ServerUser/binary, 0:8/little,  %% NULL-terminated string
	(encode_lcb(Salt))/binary,
	DbName/binary, 0:8/little>>.        %% NULL-terminated string

%% PacketLength, PacketNumber

encode_packet(handshake, ThreadId) ->
	ProtocolVersion = 10,
	ServerVersion = list_to_binary("5.1.61-hoterran.log"),
	ServerVersionLength = byte_size(ServerVersion),
	{ScrambleBuf1, ScrambleBuf2} = scramble_buf(),
	ScrambleBufLength = byte_size(ScrambleBuf1),
	RestLength = byte_size(ScrambleBuf2),
	{ServerCap1, ServerCap2} = {0, 63487},
	Charset = 8,
	ServerStatus = 2,
	%% take care servercap, else auth packet is small
	{<<ProtocolVersion:8/little, ServerVersion:ServerVersionLength/binary,
	0:8/little, ThreadId:32/little, ScrambleBuf1:ScrambleBufLength/binary,
	0:8/little, ServerCap2:16/little, Charset:8/little,
	ServerStatus:16/little, ServerCap1:16/little, ScrambleBufLength:8/little,
	0:80/little, ScrambleBuf2:RestLength/binary, 0:8/little>>, 
	<<ScrambleBuf1:ScrambleBufLength/binary, ScrambleBuf2:RestLength/binary>>	
	};

encode_packet(Number, Binary) ->
	PacketLength = byte_size(Binary),
	<<PacketLength:24/little, Number:8/little, Binary:PacketLength/binary>>.

ok_packet(AffectRows, InsertId) ->
	<<AffectRows:8/little, InsertId:8/little, 2:16/little, 0:16/little >>.

error_packet(Errno, Message) ->
	<<16#FF:8/little, Errno:16/little, $#:8/little, <<"RRY00">>:5/binary, 
			(list_to_binary(Message))/binary>>.


%%%==============================decode=========================

%%multi columns multi rows
decode_packet(resultdata, Rest, FieldCount) ->
	%%io:format("xxx ~p~n", [Rest]),
	{_, _, Rest2} = decode_packet(multi, Rest),
	<<First:8, Rest3/binary>> = Rest2,
	io:format("~n"),
	case First of
		%%eof
		254 -> Rest3;
		%%rows
		_ -> decode_packet(resultdata, Rest2, FieldCount, 0)
	end;

decode_packet(fields, Rest, FieldCount) ->
	if FieldCount =:= 0 ->
		{_, _, Rest2} = decode_packet(multi, Rest),
		Rest2; %%eof packet
	true ->
		%io:format("here ~p", [Rest]),
		{_, _, Rest2} = decode_packet(multi, Rest),
		Rest3 = decode_packet(field, Rest2),
		decode_packet(fields, Rest3, FieldCount-1)
	end.

decode_packet(resultdata, Rest, FieldCount, CurrentFieldCount) ->
	if CurrentFieldCount >= FieldCount ->
		%%field over, next rows
		decode_packet(resultdata, Rest, FieldCount);
	true ->
		%%field 
		{ColumnValue, Rest2} =		decode_lcb2(Rest),
		io:format("[~p]", [ColumnValue]),
		decode_packet(resultdata, Rest2, FieldCount, CurrentFieldCount+1)
	end.

%%PacketLength, PacketNumber

decode_packet(Binary) ->
	<<PacketLength:24/little, Number:8/little, Rest:PacketLength/binary>> = Binary,
	{PacketLength, Number, Rest}.

decode_packet(multi, Binary) ->
	<<PacketLength:24/little, Number:8/little, Rest/binary>> = Binary,
	{PacketLength, Number, Rest};

%%result parse here
decode_packet(result, Binary) ->
	<<First:8/little, Rest/binary>> = Binary,
	case First of
		0 -> %% ok packet
			decode_packet(ok, Rest);
		255 -> %% error packet
			decode_packet(error, Rest);
		254 -> %% eof packet
			decode_packet(eof, Rest);
		_ ->
			decode_packet(resultset, Binary) %%  Binary not Rest
	end;

decode_packet(resultset, Rest) ->
	%%resultset header
	%%field
	%%eof
	%%resultset
	%%eof

	%io:format("here1 ~p~n", [Rest]),

	{FieldCount, Rest2} = decode_packet(resultsetheader, Rest),

	%%fields
	%io:format("here ~p~n", [Rest2]),

	Rest3 = decode_packet(fields, Rest2, FieldCount),

	%io:format("here4 ~p~n", [Rest3]),

	%%eof
	Rest4 = decode_packet(result, Rest3),

	%%row
	Rest5 = decode_packet(resultdata, Rest4, FieldCount),

	%io:format("here5 ~p~n", [Rest5]),

	%%eof
	decode_packet(eof, Rest5);

decode_packet(field, Rest) ->
	%io:format("here3 ~p", [Rest]),
	{Catalog, Rest2} =		decode_lcb2(Rest),
	{Db, Rest3} =			decode_lcb2(Rest2),
	{TableAlias, Rest4} =	decode_lcb2(Rest3),
	{Table, Rest5} =		decode_lcb2(Rest4),
	{ColumnAlias, Rest6} =	decode_lcb2(Rest5),
	{Column, Rest7} =		decode_lcb2(Rest6),
	io:format("table ~p, ~p, ~p, ~p, ~p , ~p~n", [Catalog, Db, TableAlias, Table, ColumnAlias, Column]),
	<<_:8/little, _:8/little, _:32/little, _:8/little, _:16/little, _:8/little
	,_:16/little, Rest8/binary>> = Rest7,
	{Default, Rest9} =		decode_lcb2(Rest8),
	Rest9;

decode_packet(resultsetheader, Rest) ->
	%%TODO lcb binary
	<<FieldCount:8/little, Rest2/binary>> = Rest,
	io:format("field count ~p~n", [FieldCount]),
	{FieldCount, Rest2};

decode_packet(error, Rest) ->
	<<Errno:16/little, $#:8/little, SqlState:40/binary, Message/binary>> = Rest,
	io:format("error packet ~p ~p ~p~n", [Errno, SqlState, Message]);

decode_packet(eof, Rest) ->
	<<WarningCount:16/little, StatusFlags:16/little, Rest2/binary>> = Rest,
	io:format("eof packet ~p ~p~n", [WarningCount, StatusFlags]),
	Rest2;

decode_packet(ok, Rest) ->
	%%TODO length code binary
	<<AffectRows:8/little, InsertId:8/little, 
		ServerStatus:16/little, WarningStatus:16/little>> = Rest,
	io:format("ok packet ~p ~p~n", [AffectRows, InsertId]);
	
decode_packet(auth, Binary) ->
	<<ClientFlags:32/little, MaxPacketSize:32/little,
	CharsetNumber:8/little, _:23/binary, Rest/binary>> = Binary,
	{ClientFlags, MaxPacketSize, CharsetNumber, Rest};

decode_packet(handshake, Binary) ->
	<<ProtocolVersion:8/little, Rest/binary>> = Binary,
	{Version, Rest2} = z_split(Rest),
	<<ThreadId:32/little, Salt1:8/binary, _:8/little, ServerCap2:16/little,
	Charset:8/little, ServerStatus:16/little, ServerCap1:16/little,
	ScrambleBufLength:8/little, _:80/little, Salt2:12/binary, _:8/little>> = Rest2,
	{list_to_binary(binary_to_list(Salt1) ++ binary_to_list(Salt2))};

decode_packet(cmd, Binary) ->
	<<Command:8/little, Rest/binary>> = Binary,
	{Command, Rest}.

%% Binary -> Lcb Binary
encode_lcb(Binary) ->                                           
    encode_lcb(byte_size(Binary), Binary).
                                                                
encode_lcb(Length, Binary) when Length < 251 ->                 
    <<Length:8/little, Binary:Length/binary>>; 
                                                                
encode_lcb(Length, Binary) when Length < 65536 ->               
    <<252:8/little, Length:16/little, Binary:Length/binary>>;
                                                                
encode_lcb(Length, Binary) when Length < 16777216 ->            
    <<253:8/little, Length:24/little, Binary:Length/binary>>;
                                                                
encode_lcb(Length, Binary) when Length < 4294967296 ->          
    <<254:8/little, Length:64/little, Binary:Length/binary>>.  

%% Lcb Binary -> Binary
decode_lcb(Value, Binary) ->
	<<S1:Value/binary, Rest/binary>> = Binary,
	{S1, Rest}.

decode_lcb2(Binary) ->
	case Binary of 
		<<251:8, Rest/binary>> -> {0, Rest};
		<<252:8, Value:16/little, Rest/binary>> -> {Value, Rest};
		<<253:8, Value:24/little, Rest/binary>> -> {Value, Rest};
		<<254:8, Value:64/little, Rest/binary>> -> {Value, Rest};
		<<Value:8, Rest/binary>> -> decode_lcb(Value, Rest)
	end.

decode_lcb(Binary) ->
	case Binary of 
		<<251:8, Rest/binary>> -> {0, Rest};
		<<252:8, Value:16/little, Rest/binary>> -> {Value, Rest};
		<<253:8, Value:24/little, Rest/binary>> -> {Value, Rest};
		<<254:8, Value:64/little, Rest/binary>> -> {Value, Rest};
		<<Value:8, Rest/binary>> -> {Value, Rest}
	end.

%% NULL-terminated string parse
z_split(B) ->
    z_split(B, 0). 
z_split(B, N) -> 
	case B of
   <<B1:N/binary,0,B2/binary>> ->
       {B1,B2};
   <<_:N/binary>>=B ->
       B;  
   <<_:N/binary,_/binary>>=B ->
       z_split(B, N+1)
    end.

%% create scramble
scramble_buf() ->
	{A1, A2, A3} = now(),
	random:seed(A1, A2, A3),
    R1 = random:uniform(255) rem 94 + 33,
    R2 = random:uniform(255) rem 94 + 33,
    R3 = random:uniform(255) rem 94 + 33,
    R4 = random:uniform(255) rem 94 + 33,
    R5 = random:uniform(255) rem 94 + 33,
    R6 = random:uniform(255) rem 94 + 33,
    R7 = random:uniform(255) rem 94 + 33,
    R8 = random:uniform(255) rem 94 + 33,
    R9 = random:uniform(255) rem 94 + 33,
    R10 = random:uniform(255) rem 94 + 33,
    R11 = random:uniform(255) rem 94 + 33,
    R12 = random:uniform(255) rem 94 + 33,
    R13 = random:uniform(255) rem 94 + 33,
    R14 = random:uniform(255) rem 94 + 33,
    R15 = random:uniform(255) rem 94 + 33,
    R16 = random:uniform(255) rem 94 + 33,
    R17 = random:uniform(255) rem 94 + 33,
    R18 = random:uniform(255) rem 94 + 33,
    R19 = random:uniform(255) rem 94 + 33,
    R20 = random:uniform(255) rem 94 + 33,
    {<<R1,R2,R3,R4,R5,R6,R7,R8>>, 
     <<R9,R10,R11,R12,R13,R14,R15,R16,R17,R18,R19,R20>>}.

%%

code_change(_OldVsn, State, StateData, _Extra) ->
    {ok, State, StateData}.
 
handle_event(Event, StateName, StateData) ->
    {stop, StateName, StateData}.
 
handle_info(_Info, StateName, StateData) ->
    {stop, StateName, StateData}.

handle_sync_event(Event, From, StateName, StateData) ->
    {stop, StateName, StateData}.
 
terminate(_Reason, StateName, StateData) ->
    ok. 
