-module(beam_test).
-export([start/3]).

%% generate beam, and load binary, can Key:Term() -> Value
%%
-spec start(atom(), atom(), term()) -> term().

start(Key, Term, Value) ->
	% generate abstact 
	Abstract = [ 
		%% -module()
		erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(Key)]),

		%% -export()
		erl_syntax:attribute(erl_syntax:atom(export), 
			[erl_syntax:list([erl_syntax:arity_qualifier(erl_syntax:atom(Term), erl_syntax:integer(0))])]),

		%% Term() -> Value
		erl_syntax:function(erl_syntax:atom(Term), [erl_syntax:clause([], none, [erl_syntax:abstract(Value)])])
	],

	% tree
	Tree = [erl_syntax:revert(X) || X <- Abstract],

	% beam
	{ok, Key, Bin} = compile:forms(Tree, [verbose, report_errors]),

	code:purge(Key),

	{module, Key} = code:load_binary(Key, atom_to_list(Key) ++ ".erl", Bin),

	Key:Term().
