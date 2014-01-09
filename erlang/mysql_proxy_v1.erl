-module(mysql_proxy).

-export([start/0, wait_connect/2]).

-define(USERNAME, "abc").
-define(PASSWORD, "root").
-define(DBNAME, "test").

%% false, need recv 
start() ->
	{ok, ListenSocket} = gen_tcp:listen(3307, [binary, {active, false}, {reuseaddr, true}]),
	wait_connect(ListenSocket, 0),
	ok.

wait_connect(ListenSocket, ThreadId) ->
	{ok, Socket} = gen_tcp:accept(ListenSocket),
	spawn(?MODULE, wait_connect, [ListenSocket, ThreadId + 1]),
	with_handshake(Socket, <<>>, ThreadId).

with_handshake(Socket, Buffer, ThreadId) ->
	%%send handshake to client
	{HandShake, Scramble} = generate_handshake(ThreadId),
	HandShakePacket = encode_packet(0, HandShake),
	io:format("send scramble ~p~n", [binary_to_list(Scramble)]),
	gen_tcp:send(Socket, HandShakePacket),
	with_auth(Socket, Buffer, ThreadId, Scramble).

%%
%% Now Start receive data
%%
with_command(Socket) ->
	case gen_tcp:recv(Socket, 0) of 
		{ok, RecvBinary} ->
			io:format("recv ~p~n", [RecvBinary]),
			{_, _, Command} = decode_packet(RecvBinary),

			{CmdType, Sql} = decode_packet(cmd, Command),

			%%now we receive a sql
			%%now fake return a resultset packet

			%%resultset header
			ResultHeaderPacket = encode_packet(1, <<1:8/little>>),
			io:format("header ~p~n", [ResultHeaderPacket]),
			%%field 
			FieldPacket = encode_packet(2, 
					<<(encode_lcb(<<"def">>))/binary,				%%def
					(encode_lcb(<<"test">>))/binary,					%%db
					(encode_lcb(<<"d">>))/binary,					%%table_alias
					(encode_lcb(<<"d">>))/binary,					%%org_table
					(encode_lcb(<<"m">>))/binary,					%%column_alias
					(encode_lcb(<<"m">>))/binary,					%%org_column
					0:8/little,								%%filler
					8:8/little,								%%charset
					20:32/little,							%%length
					1:8/little,								%%type
					0:16/little,							%%flags
					0:8/little,								%%decimal
				    0:16/little>>),							%%filler

			%%eof packet
			EofPacket1 = encode_packet(3, <<16#FE:16/little, 0:16/little, 0:16/little>>),
			%%result data packet column value = 10
			ResultDataPacket = encode_packet(4, encode_lcb(Sql)),
			%%eof packet
			EofPacket2 = encode_packet(5, <<16#FE:16/little, 0:16/little, 0:16/little>>),
			
			Packet = list_to_binary(binary_to_list(ResultHeaderPacket) ++
				binary_to_list(FieldPacket) ++
				binary_to_list(EofPacket1) ++
				binary_to_list(ResultDataPacket) ++
				binary_to_list(EofPacket2)),

			gen_tcp:send(Socket, Packet),

			with_command(Socket);
		{error, closed} ->
			gen_tcp:close(Socket),
			io:format("connect close"),
			ok
	end.

with_auth(Socket, Buf, ThreadId, Scramble) ->
	case gen_tcp:recv(Socket, 0) of 
		{ok, RecvBinary} -> 
			io:format("recv ~p~n", [RecvBinary]),

			{PacketLength, PacketNumber, Buffer} = decode_packet(RecvBinary),
			
			{ClientFlags, MaxPacketSize, CharsetNumber, Rest} = decode_packet(auth, Buffer),

			%% username Null-terminated string
			{UserNameBinary, Rest2} = z_split(Rest),
			UserName = binary_to_list(UserNameBinary),
			io:format("username = ~p~n", [UserName]),

			%%salt 
			{SaltLen, Rest3} = decode_lcb(Rest2),
			{SaltBinary, Rest4} = decode_lcb(SaltLen, Rest3),
			Salt = binary_to_list(SaltBinary),
			io:format("salt = ~p~n", [Salt]),
			if Rest4 =:= <<>> ->
				DbName = [];
			true ->
				{DbNameBinary, <<>>} = z_split(Rest4),
				DbName = binary_to_list(DbNameBinary)
			end,
			io:format("dbname = ~p~n", [DbName]),

			case UserName =:= ?USERNAME of
			true ->
				case DbName =:= ?DBNAME of
				true ->
					case compare_password(SaltBinary, Scramble, ?PASSWORD) of 
					true ->
					io:format("ok~n"),
					gen_tcp:send(Socket, encode_packet(2, ok_packet(0, 0))),
					%% client start work 
					with_command(Socket);	

					false ->
					%% take care here packet_number = 2
					gen_tcp:send(Socket, encode_packet(2, error_packet(2080, "Password error"))),
					io:format("password error~n")
					end;
				false ->
				gen_tcp:send(Socket, encode_packet(2, error_packet(2081, "DB No Exists"))),
				io:format("db not exists~n")
				end;
			false->
				gen_tcp:send(Socket, encode_packet(2, error_packet(2082, "User No Exists"))),
				io:format("user not exists~p~n", [UserName])
			end,
			timer:sleep(10000),
			ok;
		{error, closed} ->
			io:format("close~n"),
			gen_tcp:close(Socket),
			ok
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
		io:format("true password is ~p~n", [Mysql_stored_password]),
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

dualmap(_F, [], []) ->
    [];
dualmap(F, [E1 | R1], [E2 | R2]) ->
    [F(E1, E2) | dualmap(F, R1, R2)].

bxor_binary(B1, B2) ->
    list_to_binary(dualmap(fun (E1, E2) -> E1 bxor E2 end, binary_to_list(B1), binary_to_list(B2))). 

%% PacketLength, PacketNumber
encode_packet(Number, Binary) ->
	PacketLength = byte_size(Binary),
	<<PacketLength:24/little, Number:8/little, Binary:PacketLength/binary>>.

decode_packet(Binary) ->
	<<PacketLength:24/little, Number:8/little, Binary:PacketLength/binary>> = Binary,
	{PacketLength, Number, Binary}.

decode_packet(auth, Binary) ->
	<<ClientFlags:32/little, MaxPacketSize:32/little,
	CharsetNumber:8/little, _:23/binary, Rest/binary>> = Binary,
	{ClientFlags, MaxPacketSize, CharsetNumber, Rest};

decode_packet(cmd, Binary) ->
	<< Command:8/little, Rest/binary>> = Binary,
	{Command, Rest}.

ok_packet(AffectRows, InsertId) ->
	<<AffectRows:8/little, InsertId:8/little, 2:16/little, 0:16/little >>.

error_packet(Errno, Message) ->
	<<16#FF:8/little, Errno:16/little, $#:8/little, <<"RRY00">>:5/binary, 
		(list_to_binary(Message))/binary>>.

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

generate_handshake(ThreadId) ->
	ProtocolVersion = 10,
	ServerVersion = list_to_binary("5.1.61-hoterran.log"),
	ServerVersionLength = byte_size(ServerVersion),
	{ScrambleBuf1, ScrambleBuf2} = scramble_buf(),
	ScrambleBufLength = byte_size(ScrambleBuf1),
	RestLength = byte_size(ScrambleBuf2),
	{ServerCap1, ServerCap2} = {0, 63487},
	ServerLang = 8,
	ServerStatus = 2,
	%% take care servercap, else auth packet is small
	{<<ProtocolVersion:8/little, ServerVersion:ServerVersionLength/binary,
	0:8/little, ThreadId:32/little, ScrambleBuf1:ScrambleBufLength/binary,
	0:8/little, ServerCap2:16/little, ServerLang:8/little,
	ServerStatus:16/little, ServerCap1:16/little, ScrambleBufLength:8/little,
	0:80/little, ScrambleBuf2:RestLength/binary, 0:8/little>>, 
	<<ScrambleBuf1:ScrambleBufLength/binary, ScrambleBuf2:RestLength/binary>>	
	}.	

