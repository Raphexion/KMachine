-module(kmachine).
-export([eval/1]).
-import(kmachine_tr, [tr/1]).
-include_lib("eunit/include/eunit.hrl").

eval(State) ->
    case kmachine_tr:tr(State) of
        {stop, R} ->
            R;
        NewState ->
            eval(NewState)
    end.

%%
%% TESTS
%%

-define(NIL, []).
-define(V0, {deBruijn, 0}). %% #0
-define(V1, {deBruijn, 1}). %% #1

eval1_test_() ->
    In = {?NIL, ?V0, ?NIL},
    Out = eval(In),
    Expected = ?V0,
    ?_assert(Out =:= Expected).

-define(B0, {abstraction, ?V0}).      %% \.#0
-define(A0, {application, ?B0, ?B0}). %% (\.#0 \.#0)

eval2_test_() ->
    In = {?NIL, ?A0, ?NIL},
    Out = eval(In),
    Expected = ?B0,
    ?_assert(Out =:= Expected).

-define(B1, {abstraction, ?V1}).      %% \.#1
-define(A1, {application, ?B0, ?B1}). %% (\.#0 \.#1)

eval3_test_() ->
    In = {?NIL, ?A1, ?NIL},
    Out = eval(In),
    Expected = ?B1,
    ?_assert(Out =:= Expected).
