

%%% *********************************************************   
%%% {c) 2017 Lloyd R. Prentice
%%% Author:     Lloyd R. Prentice
%dopt
%% License: 
%%% File:       ep_exper7.erl
%%% Description: 
%%%    Markdown parser 
%%% ********************************************************* 


-module (ep_exper7).

-export([parse_paragraph/1, parse_text/3, alter_font/3]).

%% Debug
-export([show_resolved/2, show_state/3]).

%% Test
-export([tests/0, test/1]).
-export([str0/0, str1/0, str2/0, str3/0, str4/0, str5/0]).
-export([str6/0, str7/0, str8/0, str9/0, str10/0, str11/0]).

% ***************************************************************
%% Parse markdown text
%% ***************************************************************

parse_paragraph(Text) ->
   {Tag, _} = get_tag(Text),
   case Tag of
      "p" -> Unresolved = [];
      _   -> Unresolved = [{1, "p", []} | []]
   end, 
   Resolved    = [],
   parse_text(Text, Unresolved, Resolved).

parse_text([], Unresolved, Resolved) ->
   [Span | _Unresolved1] = Unresolved,
    Resolved1 = [Span | Resolved],
   Resolved2 = lists:sort(Resolved1),
   [resolve_span(Span1) || Span1 <- Resolved2];

parse_text(Text, Unresolved, Resolved) ->
   {Tag, Text1} = get_tag(Text),
   Flag = if_close(Tag, Unresolved),
   case Flag of
      open  -> % io:format("~nENTERING PARSE open span~n"),
                {Text2, Unresolved1, Resolved1} =
                   open(Text, Unresolved, Resolved),
               parse_text(Text2, Unresolved1, Resolved1);

      close ->  % io:format("~nENTERING PARSE close span~n"),
               {Unresolved1, Resolved1} = 
                    close(Tag, Unresolved, Resolved),
               parse_text(Text1, Unresolved1, Resolved1)
   end.


if_close(_Tag, []) ->
   open;

if_close(Tag, Unresolved) ->
   Flag = lists:keymember(Tag, 2, Unresolved),
   case Flag of
      true  -> close;
      false -> open
   end.

%% *****************************************************
%% open/4
%% *****************************************************

open(Text, Unresolved, Resolved) ->
   N = next_n(Unresolved, Resolved),
   {Tag, Text1}  = get_tag(Text),
   {Text2, SpanText} = get_chars(Text1), 
   Span = {N, Tag, SpanText},
   Unresolved1 = [Span|Unresolved],
   {Text2, Unresolved1, Resolved}.
   
%% *****************************************************
%% close/4
%% *****************************************************

close(Tag, Unresolved, Resolved) ->
   {Span, Unresolved1} = orphans(Tag, Unresolved),
    Resolved1 = [Span | Resolved],
    {Unresolved1, Resolved1}.



resolve_span(Span) ->
   {N, Tag, SpanText} = Span,
   Text = lists:reverse(SpanText),
   {N, Tag, Text}.


orphans(Tag, Unresolved) ->
   Result = lists:keytake(Tag, 2, Unresolved),
   {_, Span, Unresolved1} = Result,
   SpanN = element(1, Span),
   OrphanList  = [Orphan || Orphan <- Unresolved, element(1, Orphan) > SpanN],
   Unresolved2 = [UR || UR <- Unresolved1, element(1, UR) < SpanN],
   Orphans = lists:reverse(OrphanList),
   Span1 = adopt_orphans(Span, Orphans),
   {Span1, Unresolved2}.
   
adopt_orphans(Span, []) ->
   Span;

adopt_orphans(Span, Orphans) ->
   [Orphan | Rest] = Orphans,
   Span1 = adopt(Span, Orphan),
   adopt_orphans(Span1, Rest).

adopt(Span, Orphan) ->
   {SpanN, SpanTag, SpanText} = Span,
   {_, OrphanTag, OrphanText} = Orphan, 
   OrphanChar = revert_tag(OrphanTag),
   SpanText1 = OrphanText ++ OrphanChar ++ SpanText,
   {SpanN, SpanTag, SpanText1}.
   
%% *****************************************************
%% get_text/4
%% *****************************************************

% get_text(Tag, Text, Unresolved, Resolved) ->
%   io:format("Entering get_text/4~n"),
%   N = next_n(Unresolved, Resolved),
%   {Text1, SpanText} = get_chars(Text),
%   io:format("Text: ~p; SpanText: ~p~n", [Text1, SpanText]),
%   Span = {N, Tag, SpanText},
%   {Text1, Span}.

last_n(Unresolved, Resolved) ->
   N1 = largest_n(Unresolved),
   N2 = largest_n(Resolved),
   max(N1, N2).

next_n(Unresolved, Resolved) ->
   last_n(Unresolved, Resolved) + 1.

largest_n(SpanList) ->
   Flag = SpanList == [],
   case Flag of
      true  -> N = 0;
      false -> List = [element(1, Span) || Span <- SpanList],
               N = lists:max(List)
   end,
   N.

%% ******************************************************
%% get_chars/1 -- Parse text up to next tag or end of text
%% ******************************************************

get_chars(Text) ->
   get_chars(Text, []).

get_chars([], SpanText) ->
  {[], SpanText};

get_chars(Text, SpanText) ->
   [Char | Rest ] = Text,
   Flag = is_text_char(Char),
   case Flag of
      true  -> SpanText1 = [Char | SpanText],
               get_chars( Rest, SpanText1);
      false -> % OK, we've hit a non-text char 
               {Text, SpanText}
   end.


is_text_char(Char) ->
   NonPChars = [$*, $_],
   Flag = lists:member(Char, NonPChars),
   case Flag == false of
      true  -> true;
      false -> false
   end.

%% *****************************************************
%% get_tag/2
%% *****************************************************

get_tag([]) ->
   {[$p], []};

get_tag(Text) -> % when length(Chars) =< 2
   Chars = string:left(Text, 2),
   {Offset, Tag} = resolve_tag(Chars),
   Text1 = string:sub_string(Text, Offset),
   {Tag, Text1}.
   

resolve_tag([$*])      -> {2, [$e,$*]};
resolve_tag([$*, $*])  -> {3,[$b,$*]};
resolve_tag([$*, _])   -> {2,[$e,$*]};
resolve_tag([$_])      -> {2,[$e,$_]};
resolve_tag([$_, $_])  -> {3,[$b,$_]};
resolve_tag([$_, _])   -> {2,[$e,$_]};
resolve_tag([_, _])    -> {1,[$p]};
resolve_tag([])        -> {1,[$p]}.

revert_tag("e*") -> "*";
revert_tag("b*") -> "**";
revert_tag("e_") -> "_";
revert_tag("b_") -> "__".


show_resolved(Unresolved, Resolved) ->
   io:format("Unresolved: ~p~n", [Unresolved]),
   io:format("Resolved: ~p~n~n", [Resolved]).

show_state(Text, Unresolved, Resolved) ->
   io:format("Text: ~p~n", [Text]),
   io:format("Unresolved: ~p~n", [Unresolved]),
   io:format("Resolved: ~p~n~n", [Resolved]).


alter_font(Text, OldTag, NewTag) ->
   [alter_tag(Span, OldTag, NewTag) || Span <- Text].

alter_tag(Span, OldTag, NewTag) ->
   Tag1 = element(2, Span),
   case Tag1 == OldTag of
      true  -> setelement(2, Span, NewTag);
      false -> Span
   end.


   


%% ***************************************************************
%% Parse markdown text -- test
%% ***************************************************************

tests() ->
   test(str0()),
   test(str1()),
   test(str2()),
   test(str3()),
   test(str4()),
   test(str5()),
   test(str6()),
   test(str7()),
   test(str8()),
   test(str9()).

test(Text) ->
  parse_paragraph(Text).


%% ***************************************************************
%% Sample text rendered in markdown;
%%  https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet
%% ***************************************************************

%% Need to preserve blank lines

str0() ->
  "my sentence without emphasized text".  % pass 

str1() ->
  "my *sentence with* _emphasized_ text". % pass

str2() ->
  "my sen*tence with* _emphasized_ text".  % pass

str3() ->
  "my *sentence* with __emphasized__ text". % pass

str4() ->
  "my **sentence with** __emphasized__ text". % pass

str5() ->
   "_my **sentence with em** orphan markup".

str6() ->
   "my **sentence with em** orphan _markup".

str7() ->
   "my **sentence _with em** orphan markup".

str8() ->
   "my **sentence _with *em** orphan markup".

str9() ->
   "*my italicezed sentence*".

str10() ->
   "*my mostly italicezed* sentence".

str11() ->
   "my mostly italicezed* sentence*".








