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

%% -define(COPY_PATH, "/home/lloyd/ep/src/copy/copy_samples/").
%% -define(PARSER, "/home/lloyd/ep/src/copy/cmark_parse/cmark ").

-define(PARSER, parser()).

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


test_parse1() ->
   md_parse:parse(?SAMPLE1).

test_parse2() ->
   md_parse:parse(?SAMPLE2).

test_parse3() ->
   md_parse:parse(?SAMPLE3).

%% We need to parse out get_copy/1

parse(FileName) ->
   CopySource  = FileName,
   Parser      = ?PARSER,
   Destination = CopySource ++ ".erlang",
   os:cmd(Parser ++ CopySource ++ " -t erlang > " ++ Destination),
   {ok, [Terms]} = get_file(FileName),
   Terms.

get_file(FileName) ->
   FilePath = FileName,
   Destination = FilePath ++ ".erlang",
   file:consult(Destination).

parser() ->
    PrivDir = priv_dir(?MODULE),
    filename:join([PrivDir, "cmark"]).

%%--------------------------------------------------------------------
%% @doc Returns the module's "priv" directory.
%% @end
%% @private
-spec priv_dir(atom()) -> string().
%%--------------------------------------------------------------------
priv_dir(Mod) ->
    case code:priv_dir(Mod) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(Mod)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end.
