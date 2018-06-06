%%% ==========================================================================
%%% md_parse.erl

%%% @author     Patrice Bruno, Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:
%%%   File:         md_parse.erl
%%%   Description:  Erlang wrapper around cmark Markdown parser
%%%                 https://github.com/skaee/cmark
%%%                 Should probably be re-implimented as an Erlang nif

%%% @end

%%% ==========================================================================


-module(md_parse).

-export([parse/1]).
-export([test_parse1/0, test_parse2/0, test_parse3/0]).
-export([get_copy/1]).

%% Requires Dir: copy_samples
%%
%% test1:
%%


%% N> md_parse:parse(SAMPLE1).
%% test1: N> md_parse:parse(SAMPLE1).
%% test2: N> md_parse:parse(SAMPLE2).


%% ====================================================================
%% MACRO
%% ====================================================================

-define(COPY_PATH, "../src/copy/test_samples").

-define(SAMPLE1, "example.md").
-define(SAMPLE2, "paragraph.md").
-define(SAMPLE3, "test.md").

%% ====================================================================
%% API functions
%% ====================================================================

test_parse1() ->
    test_parse(?SAMPLE1).

test_parse2() ->
    test_parse(?SAMPLE2).

test_parse3() ->
    test_parse(?SAMPLE3).

test_parse(Sample) ->
    Source = filename:join([?COPY_PATH, Sample]),
    md_parse:parse(Source).

%% We need to parse out get_copy/1

parse(Source) ->
%    Dest    = Source ++ ".erlang",
    Dest    = "../copy/test_samples/paragraph" ++ ".erlang",

    Command = string:join([parser(), Source, "-t erlang >", Dest], " "),

    io:format("parse - Command: ~p~n", [Command]),

    _ = os:cmd(Command), %% TODO: check errors

    io:format("parse - Dest: ~p~n", [Dest]),

    {ok, [Terms]} = file:consult(Dest),
    Terms.

parser() ->
    PrivDir = ep_utils:priv_dir(?MODULE),
    filename:join([PrivDir, "cmark"]).



get_copy(MDCopy) ->
    Source = "./src/copy/test_samples/" ++ MDCopy,
    io:format("%%%%%% get_copy/1 Source: ~p~n", [Source]),
    file:consult(Source).

