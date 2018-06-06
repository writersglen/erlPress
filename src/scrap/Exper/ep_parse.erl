

%%% *********************************************************   
%%% {c) 2017 Lloyd R. Prentice
%%% Author:     Lloyd R. Prentice
%dopt
%% License: 
%%% File:       ep_parse.erl
%%% Description: 
%%%    Base function for ep_parse_md.erl  
%%% ********************************************************* 


-module (ep_parse).


% -export([text/1, alter_font/3]).

%% Debug
% -export([show_resolved/2, show_state/3]).

-compile(export_all).



% ***************************************************************
%% Parse markdown text
%% ***************************************************************


% parse_paragraph(Text) ->
%   {Tag, _} = get_tag(Text),
%   case Tag of
%      "p" -> Unresolved = [];
%      _   -> Unresolved = [{1, "p", []} | []]
%   end, 
%   Resolved    = [],
%   parse_text(Text, Unresolved, Resolved).



text(Text) ->
   {Tag, _} = get_tag(Text),
   case Tag of
      "p" -> Unresolved = [];
      _   -> Unresolved = [{1, "p", []} | []]
   end,
   Resolved    = [],
   text(Text, Unresolved, Resolved).

    
text([], Unresolved, Resolved) ->
   [Span | _Unresolved1] = Unresolved,
    Resolved1 = [Span | Resolved],
   Resolved2 = lists:sort(Resolved1),
   [resolve_span(Span1) || Span1 <- Resolved2];

text(Text, Unresolved, Resolved) ->
   {Tag, Text1} = get_tag(Text),
   Flag = if_close(Tag, Unresolved),
   case Flag of
      open  -> % io:format("~nENTERING PARSE open span~n"),
                {Text2, Unresolved1, Resolved1} =
                   open(Text, Unresolved, Resolved),
                text(Text2, Unresolved1, Resolved1);

      close ->  % io:format("~nENTERING PARSE close span~n"),
               {Unresolved1, Resolved1} = 
                    close(Tag, Unresolved, Resolved),
               text(Text1, Unresolved1, Resolved1)
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
%% last_n/2 
%% *****************************************************

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
   NonPChars = [$*, $_, $`],
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
   % io:format("Get tag: ~p~n", [Text]),
   Chars = string:left(Text, 2),
   {Offset, Tag} = resolve_tag(Chars),
   Text1 = string:sub_string(Text, Offset),
   {Tag, Text1}.
   

% resolve_tag([$*])      -> {2, [$e,$*]};
% resolve_tag([$*, $*])  -> {3,[$b,$*]};
% resolve_tag([$*, _])   -> {2,[$e,$*]};
% resolve_tag([$_])      -> {2,[$e,$_]};
% resolve_tag([$_, $_])  -> {3,[$b,$_]};
% resolve_tag([$_, _])   -> {2,[$e,$_]};
% resolve_tag([$`])      -> {2,[$c,$d]};
% resolve_tag([$`,_])    -> {2,[$c,$d]};
% resolve_tag([_, _])    -> {1,[$p]};
% resolve_tag([])        -> {1,[$p]}.

resolve_tag([$*])      -> {2, em};
resolve_tag([$*, $*])  -> {3, b};
resolve_tag([$*, _])   -> {2, em};
resolve_tag([$_])      -> {2, em};
resolve_tag([$_, $_])  -> {3, b};
resolve_tag([$_, _])   -> {2, b};
resolve_tag([$`])      -> {2, code};
resolve_tag([$`,_])    -> {2, code};
resolve_tag([_, _])    -> {1, p};
resolve_tag([])        -> {1, p}.

revert_tag("e*") -> "*";
revert_tag(em)   -> "*";
revert_tag("b*") -> "**";
revert_tag(b)    -> "**";
revert_tag("e_") -> "_";
revert_tag("b_") -> "__";
revert_tag("cd") -> "`".


show_resolved(Unresolved, Resolved) ->
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


eptext2xml(EpText) ->
   [convert2xml(Span) || Span <- EpText].

convert2xml(Span) ->
   {_, Tag, Text} = Span,
   case Tag of
      p    -> {raw, Text};
      em   -> {em, [], [{raw, Text}]};
      b    -> {b, [], [{raw, Text}]};
      code -> {code, [], [{raw, Text}]};
      _    -> {error}
   end.

to_xml(Text) ->
   EpText = text(Text),
   eptext2xml(EpText).

%% *****************************************************
%% show_state/3 -- debugging aid
%% *****************************************************

show_state(Text, Unresolved, Resolved) ->
   io:format("Text: ~p~n", [Text]),
   io:format("Unresolved: ~p~n", [Unresolved]),
   io:format("Resolved: ~p~n~n", [Resolved]).

