%%% *********************************************************
%%% ep_md_parse.erl
%%%
%%% {c) 2017 Lloyd R. Prentice
%%% Authors:    Patrice Bruno, Lloyd R. Prentice
%%% License:
%%% File:       ep_tests.erl
%%% Description:
%%%    Erlang wrapper around cmark Markdown parser
%%%    https://github.com/skaee/cmark
%%%    Should probably be re-implimented as an Erlang nif


%%% *********************************************************

-module(md_parse).

-define(COPY_PATH, "src/copy/copy_samples").

-define(SAMPLE1, "example.md").
-define(SAMPLE2, "paragraph.md").
-define(SAMPLE3, "test.md").

%% TEST
%%
%% Requires Dir: copy_samples
%%
%% test1:
%%
%% N> md_parse:parse(SAMPLE1).
%% test1: N> md_parse:parse(SAMPLE1).
%% test2: N> md_parse:parse(SAMPLE2).


%% ====================================================================
%% API functions
%% ====================================================================


-export([parse/1]).
-export([test_parse1/0, test_parse2/0, test_parse3/0]).


filename(F) ->
    filename:join([?COPY_PATH, F]).

test_parse1() ->
    FileName = filename(?SAMPLE1),
    md_parse:parse(FileName).

test_parse2() ->
    FileName = filename(?SAMPLE2),
    md_parse:parse(FileName).

test_parse3() ->
    FileName = filename(?SAMPLE3),
    md_parse:parse(FileName).

%% We need to parse out get_copy/1

parse(Source) ->
    Dest    = Source ++ ".erlang",
    Command = string:join([parser(), Source, "-t erlang >", Dest], " "),
    _ = os:cmd(Command), %% TODO: check errors
    {ok, [Terms]} = file:consult(Dest),
    Terms.

parser() ->
    PrivDir = ep_utils:priv_dir(?MODULE),
    filename:join([PrivDir, "cmark"]).
