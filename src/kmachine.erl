%% Krivine's machine
%%

-module(kmachine).

-export([kmachine_init/1,
	 eval/1]).


%% -------------------------------------------------------
%% API
%% -------------------------------------------------------

kmachine_init(StartTerm) ->
    {empty_env(), StartTerm, empty_stack()}.

eval({stop, Result}) ->
    Result;

eval(State) ->
    eval(transition(State)).


%% -------------------------------------------------------
%% Private
%% -------------------------------------------------------

%% Rule #1
%%

transition({[{suspension, Env, Exp} | _E], {deBruijn, 0}, Stack}) ->
    {Env, Exp, Stack};

%% Rule #2
%%

transition({[_V | Env], {deBruijn, N}, Stack}) ->
    {Env, {deBruijn, N-1}, Stack};

%% Rule #3
%%

transition({Env, {application, E0, E1}, Stack}) ->
    {Env, E0, [suspend(Env, E1) | Stack]};

%% Rule #4
%%

transition({Env, {abstraction, Body}, [HeadStack | Stack]}) ->
    {[HeadStack | Env], Body, Stack};

%% Rule #5
%%

transition({_E, Result, []}) ->
    {stop, Result}.

%%
%%

empty_env() ->
    [].

%%
%%

empty_stack() ->
    [].

%%
%%

suspend(Env, Term) ->
    {suspension, Env, Term}.


%% -------------------------------------------------------
%% TESTS
%% -------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

transition(Env, Term, Stack) ->
    transition({Env, Term, Stack}).

kmachine_init_with_env(StartTerm, Env) ->
    {Env, StartTerm, empty_stack()}.

%%
%%

eval_minimal_test_() ->
    Out = eval(kmachine_init(lambda:debruijn(0))),
    Expected = lambda:debruijn(0),
    ?_assert(Out =:= Expected).

eval_minimal_application_1_test_() ->
    StartTerm = lambda:application(lambda:identity(), lambda:identity()),
    Out = eval(kmachine_init(StartTerm)),
    Expected = lambda:identity(),
    ?_assert(Out =:= Expected).


eval_minimal_application_2_test_() ->
    StartTerm = lambda:application(lambda:identity(), lambda:const(1)),
    Out = eval(kmachine_init(StartTerm)),
    Expected = lambda:const(1),
    ?_assert(Out =:= Expected).

%%
%%

kmachine_transition_stop_test_() ->
    [ ?_assert(transition(kmachine_init(lambda:debruijn(0))) =:= {stop, lambda:debruijn(0)}),
      ?_assert(transition(kmachine_init(lambda:debruijn(1))) =:= {stop, lambda:debruijn(1)})
    ].

kmachine_transition_rule_3_test_() ->
    StartTerm = {
      application,
      lambda:debruijn(0),
      lambda:debruijn(1)
     },

    Expected = {
      empty_env(),
      lambda:debruijn(0),
      [{suspension, empty_env(), lambda:debruijn(1)}]
     },

    Result = transition(kmachine_init(StartTerm)),
    ?_assert(Result =:= Expected).

kmachine_transition_rule_1_test_() ->
    TargetEnv = empty_env(),
    TargetTerm = lambda:debruijn(42),

    StartTerm = lambda:debruijn(0),
    Stack = empty_stack(),

    Out = transition([{suspension, TargetEnv, TargetTerm}], StartTerm, Stack),
    Expected = {TargetEnv, TargetTerm, Stack},

    ?_assert(Out =:= Expected).

kmachine_transition_rule_2_test_() ->
    Env = [env2, env1, env0],
    Term = lambda:debruijn(1),
    Stack = empty_stack(),

    Out = transition(Env, Term, Stack),
    Expected = {[env1, env0], lambda:debruijn(0), Stack},
    ?_assert(Out =:= Expected).

kmachine_transition_rule_4_test_() ->
    Env = empty_env(),
    Term = lambda:abstraction(body),
    Stack = [stack2, stack1, stack0],

    Out = transition(Env, Term, Stack),
    Expected = {[stack2], body, [stack1, stack0]},
    ?_assert(Out =:= Expected).

-endif.
