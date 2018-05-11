
%%% *********************************************************   
%%% {c) 2016 Lloyd R. Prentice
%%% Author:     Lloyd R. Prentice
%%% License: 
%%% File:       ep_boxes.erl
%%% Description: 
%%%    Experimental print routines 
%%% *********************************************************   


-module (ep_exper6).

% -export([parse_paragraph/1]).
% -export([open/4, close/4]). 
% -export([tag_state/2, get_chars/1]). 
% -export([get_open_tag/1]). 
% -export([get_tag/2, last_n/2, next_n/2]). 
% -export([revert_tag/1, revert_span/1]). 

% -export([test/1]). 
% -export([str0/0, str1/0, str2/0, str3/0, str4/0, str5/0, str6/0, str7/0]).
% -export([str8/0]).

-compile([export_all]).

%% ***************************************************************
%% Parse markdown text
%% ***************************************************************


parse_paragraph(Text) ->
   io:format("Text: ~p~n~n", [Text]),
   Tag         = p,
   Unresolved  = [],
   Resolved    = [],
   parse_paragraph(Tag, Text, Unresolved, Resolved).


parse_paragraph(_Tag, [], Unresolved, Resolved) ->
   io:format("Done! ~p ~p~n", [Unresolved, Resolved]),
    show_state([], Unresolved, Resolved),
    {_, Span, Unresolved1} = lists:keytake(p, 2, Unresolved),
    {N, Tag1, SpanText} = Span,
    SpanText1 = lists:reverse(SpanText),
    Span1 = {N, Tag1, SpanText1},
    Resolved1 = [Span1 | Resolved],
    show_state([], Unresolved1, Resolved1),
    Resolved2 = lists:sort(Resolved1),
    {Unresolved1, Resolved2};

parse_paragraph(Tag, Text, Unresolved, Resolved) ->
   io:format("~nENTERING parse_paragraph!~n"),
   {Text1, Unresolved1, Resolved1} =
       get_text(Tag, Text, Unresolved, Resolved),
   Flag = Text1 == [],
   case Flag of
       true  -> io:format("Parse paragraph - DONE!~n~n"),
                parse_paragraph(Tag, Text1, Unresolved1, Resolved1);
       false -> io:format("Parse paragraph - PARSE!~n~n"), 
                {Tag1, Text2, Unresolved2, Resolved2} = 
                    parse(Tag, Text1, Unresolved1, Resolved1),  
                parse_paragraph(Tag1, Text2, Unresolved2, Resolved2)
   end.
   
parse(Tag, Text, Unresolved, Resolved) ->
   io:format("~nENTERING parse:~n"),
   io:format("Tag: ~p~n", [Tag]),
   show_state(Text, Unresolved, Resolved),   
   {Tag1, Text1} = get_open_tag(Text),

   io:format("Tags: ~p ~p~n", [Tag1, Text1]),
   % Open or close?    
   TagState = tag_state(Tag1, Unresolved),
   io:format("TagState: ~p~n", [TagState]),
   {Tag1, Text1} = get_open_tag(Text),
   io:format("Tags: ~p ~p~n", [Tag1, Text1]),
   case TagState of
      open  -> io:format("~nPARSE - open~n"), 
               Result = open(Tag1, Text1, Unresolved, Resolved),
               io:format("LEAVING PARSE - open~n~n"),
               Result;
               % parse_paragraph(Result);
      close -> io:format("~nPARSE - close~n"),  
               io:format("Tag: ~p~n", [Tag1]),
               show_state(Text1, Unresolved, Unresolved),
               % {Tag1, Text2, Unresolved1, Resolved1} = 
                Result =  close(Tag1, Text1, Unresolved, Resolved),
               io:format("LEAVING PARSE - close~n~n"),
                io:format("Result: ~p~n", [Result]),
                Result
                % parse_paragraph(Result)
   end.



%% *****************************************************
%% get_text/4
%% *****************************************************

get_text(Tag, Text, Unresolved, Resolved) ->
   N = next_n(Unresolved, Resolved),
   {Text1, SpanText} = get_chars(Text),
   Span = {N, Tag, SpanText},
   Unresolved1 = [Span | Unresolved],
   {Text1, Unresolved1, Resolved}.

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

get_chars(Text) ->   
   io:format("get_chars: ~p~n", [Text]),
   get_chars(Text, []).

get_chars([], SpanText) ->
  io:format("I'm done! ~p ~p~n", [[], SpanText]),
  {[], SpanText};

get_chars(Text, SpanText) ->
   io:format("get_chars/2: ~p ~p~n", [Text, SpanText]),
   [Char | Rest ] = Text,
   Flag = is_text_char(Char),
   case Flag of
      true  -> SpanText1 = [Char | SpanText],
               get_chars( Rest, SpanText1);
      false -> % OK, we've hit a non-text char 
               {Text, SpanText}
 %              get_chars([], {Text, SpanText})
   end. 
   
is_text_char(Char) ->
   NonPChars = [$*, $_],
   Flag = lists:member(Char, NonPChars),
   case Flag == false of
      true  -> true;
      false -> false 
   end.

%% *****************************************************
%% get_open_tag/1
%% *****************************************************

get_open_tag(Text) ->
   io:format("get_open_tag: Text = ~p~n", [Text]),
   Char = hd(Text),
   case Char of
      $* -> get_stars(Text);
      $_ -> get_underscores(Text);
      _  -> Text
   end.

get_stars(Text) ->
   Chars  = lists:takewhile(fun(C) -> C == $* end, Text),
   get_tag(Chars, Text).

get_underscores(Text) ->
   Chars = lists:takewhile(fun(C) -> C == $_ end, Text),
   get_tag(Chars, Text).
   

get_tag(Chars, Text) -> % when length(Chars) =< 2
   % io:format("Get tag: ~p ~p~n", [Chars, Text]),
   {Clip, Tag} = resolve_tag(Chars),
   %  io:format("Clip: ~p ~p ~p~n", [Clip, Tag, Text]),
   case Clip of
     [] -> text;
     _  -> {Tag, string:sub_string(Text, Clip)}
   end.

resolve_tag([$*])      -> {2, [$e,$*]};
resolve_tag([$*, $*])  -> {3,[$b,$*]};
resolve_tag([$_])      -> {2,[$e,$_]};
resolve_tag([$_, $_])  -> {3,[$b,$_]};
resolve_tag([])        -> {0, []}.       


revert_tag("e*") -> "*";
revert_tag("b*") -> "**";
revert_tag("e_") -> "_";
revert_tag("b_") -> "__".

revert_span(Span) ->
   {N, Tag, SpanText} = Span,
   Chars = revert_tag(Tag),
   Text = Chars ++ SpanText,
   {N, Text}. 
   



%% *****************************************************
%% tag_state/2
%% *****************************************************

tag_state(Tag, Unresolved) ->
   Result = lists:keytake(Tag, 2, Unresolved),
   Flag = is_tuple(Result),
   case Flag of
      true  -> close;
      false -> open
   end.

%% *****************************************************
%% open/4
%% *****************************************************

open(Tag, Text, Unresolved, Resolved) ->
   io:format("Entering open~n"),
   io:format("Tag: ~p~n", [Tag]),
   show_state(Text, Unresolved, Resolved),



   io:format("Leaving open~n"),
   {Tag, Text, Unresolved, Resolved}.

%% *****************************************************
%% close/4
%% *****************************************************

close(Tag, Text, Unresolved, Resolved) ->
   io:format("~nEntering close~n"),
   show_state(Text, Unresolved, Resolved),
   io:format("Tag: ~p~n", [Tag]),
   {_, Span, Unresolved1} = lists:keytake(Tag, 2, Unresolved),
   io:format("Tuple: ~p ~p~n", [Span, Unresolved1]),

   % Resolve Span1
   {N, Tag, SpanText} = Span,
   SpanText1 = lists:reverse(SpanText),
   Span1 = {N, Tag, SpanText1},

   NZ = next_n(Unresolved, Resolved),
   io:format("Span1: N ~p ~p ~p~n", [Span1, NZ, N]),
   Resolved1 = [Span1 | Resolved],
   io:format("Resolved: ~p~n", [Resolved1]),

   % Resolve span2
   [Span2 | Unresolved2] = Unresolved1,
   {N1, Tag1, SpanText2} = Span2,
   SpanText3 = lists:reverse(SpanText2),
   Span3 = {N1, Tag1, SpanText3},
   io:format("Span3: ~p ~p ~p~n", [N1, Tag1, SpanText3]),
   
%   Tag2 = element(2, Span2),
   Resolved2 = [Span3 | Resolved1],
%   Span = new_span(Tag, Unresolved, Resolved),
%   Unresolved1 = [Span | Unresolved],
   io:format("Tag1: ~p~n", [Tag1]),
%   Tag1 = new_tag(Unresolved2),
   show_state(Text, Unresolved2, Resolved2),
   io:format("Leaving close~n"),
   {Tag1, Text, Unresolved2, Resolved2}.



%% ***************************************************************
%% Parse markdown text -- test
%% ***************************************************************

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


%% *****************************************************
%% show_state/3 --- for debugging 
%% *****************************************************

show_state(Text, Unresolved, Resolved) ->
   io:format("Text: ~p~n", [Text]),
   io:format("Unresolved: ~p~n", [Unresolved]),
   io:format("Resolved: ~p~n~n", [Resolved]).

%% *****************************************************
%% resolve_span/2 
%% *****************************************************

% resolve_span(Span, Resolved) ->
%   {N, Tag, SpanText} = Span,
%   SpanText1 = lists:reverse(SpanText),
%   Span1 = {N, Tag, SpanText1},
%   [Span1 | Resolved].


% new_tag(Unresolved) ->
%    io:format("Entering new_span~n"),
%    N = next_n(Unresolved, Resolved),
%    Span = hd(Unresolved),
%    Tag1 = element(2, Span),
%    Span1 = {N, Tag1, []},
%    Unesolved1 = [Span1 | Unresolved],
%     Tag1.


   




% process_char($*, Text, Unresolved, Resolved)  ->
%   io:format("It's a star!~n"),
%   Tag = resolve_tag($*, Text),
%   TagState = tag_state(Tag, Unresolved),


%   {Text, Unresolved1, Resolved1} = 
%    Result =
%         process_span(TagState, Tag, Text, Unresolved, Resolved), 
%    io:format("Result: ~p~n", [Result]),
    
%   {Text, Unresolved1, Resolved1} = Result,

%   show_state(Text, Unresolved1, Resolved1),
%   parse_paragraph(Text, Unresolved1, Resolved1);

% process_char($_, Text, Unresolved, Resolved)   ->
%   io:format("It's an underscore!~n"),
%   Tag = resolve_tag($_, Text),
%   TagState = tag_state(Tag, Unresolved),
%   {Text, Unresolved1, Resolved} =
%        process_span(TagState, Tag, Text, Unresolved, Resolved), 

%   show_state(Text, Unresolved1, Resolved),
%   parse_paragraph(Text, Unresolved1, Resolved);


% process_char(Char, Text, Unresolved, Resolved) ->
%   io:format("ENTERING process_char~n"),
%   io:format("Char: ~p~n~n", [Char]),
%   Flag = Unresolved == [],
%   case Flag of
%      true  -> Unresolved1 = first_char(Char, Unresolved, Resolved);
%      false -> Unresolved1 = more_chars(Char, Unresolved)
%   end,
%   show_state(Text, Unresolved1, Resolved),
%   io:format("LEAVING process_char~n~n"),
%   parse_paragraph(Text, Unresolved1, Resolved).



% first_char(Char, Unresolved, Resolved) ->
%   N = next_n(Unresolved, Resolved),
%   SpanText = [],
%   SpanText1 = [Char | SpanText],
%   Span = {N, p, SpanText1},
%   [Span | Unresolved].

% more_chars(Char, Unresolved) ->
%   [Span | Unresolved1] = Unresolved,
%   {N, Tag, SpanText} = Span,
%   SpanText1 = [Char | SpanText],
%   Span1 = {N, Tag, SpanText1},
%   [Span1 | Unresolved1].



%% @doc If text prefixed with *, Returns tag for emphasis, else 
%%      returns tag for bold  
  





% is_p_char(Char) ->
%   NonPChars = [$*, $_],
%   Flag = lists:member(Char, NonPChars),
%   case Flag of
%      true  -> false;
%      false -> true
%   end.
   


% process_span(open, Tag, Text, Unresolved, Resolved) ->
%   N = next_n(Unresolved, Resolved),
%   io:format("ENTERING process_span open: ~p~n", [Unresolved]),
%   io:format("Tag: ~p~n", [Tag]),
%   io:format("N: ~p~n", [N]),
%   NewSpan = {N, Tag, []},
%   Unresolved1 = [NewSpan | Unresolved],
%   show_state(Text, Unresolved1, Resolved),
%   io:format("LEAVING process_span open~n"),
%   {Text, Unresolved1, Resolved}; 
  
% process_span(close, Tag, Text, Unresolved, Resolved) ->
%   io:format("ENTERING process_span close span~n"),
%   io:format("Tag: ~p~n", [Tag]),

%   {_, Span, Unresolved1} = lists:keytake(Tag, 2, Unresolved),
%   Resolved1 = resolve_span(Span, Resolved), 
%   io:format("~nBIG CHOICE...~n"),
%   io:format("open new unresolved or continue accumulating chars~n~n"),
%   show_state(Text, Unresolved1, Resolved1),

%  Flag = is_p_char(hd(Text)),
%   case Flag of
%      true  -> N = next_n(Unresolved1, Resolved), 
%               Span   = hd(Unresolved1),
%               NewTag = element(2, Span),
%               NewSpan1 = {N, NewTag, []},
%               Unresolved2 = [NewSpan1, Unresolved1];
%      false -> Unresolved2 = Unresolved1
%   end,
 %  io:format("P-char flag: ~p~n", [Flag]),
%
%   show_state(Text, Unresolved1, Resolved1),
%   io:format("LEAVING process_span close~n~n"),
%   intervene(Text, Unresolved1, Resolved1). 



% intervene(Text, Unresolved, Resolved) ->
%   Flag = is_p_char(hd(Text)),
%   io:format("P-char flag: ~p~n", [Flag]),
%   case Flag of
%       true  -> io:format("NEED TO INTERVENE~n"),
%                [Span | Unresolved1] = Unresolved,
%                {N, Tag, SpanText} = Span, 
%                SpanText1 = lists:reverse(SpanText),
%                Span1 = {N, Tag, SpanText1},
%                Resolved1 = [Span1 | Resolved],
%                N1 = next_n(Unresolved, Resolved),
%                Span2 = {N1, Tag, []},
%                Unresolved2 = [Span2 |Unresolved1],
%                {Text, Unresolved2, Resolved1};
%       false -> {Text, Unresolved, Resolved}
%   end. 


% count_chars([]) ->
%   0;

% count_chars(String) ->
%   Char = hd(String),
%   count_chars(String, Char, 0).

% count_chars([], _Char, N) ->
%   N;

% count_chars(String, Char, N) ->
%   [First | Rest ] = String,
%   Flag = First == Char,
%   case Flag of
%      true  -> count_chars(Rest, Char, N + 1);
%      _  -> N
%   end.



% tag_span(Span, Unresolved) ->
%   Span1 = lists:reverse(Span),
%   [TaggedSpan| Rest] = Unresolved,
%   {Index, Tag, _} = TaggedSpan,
%   TaggedSpan1 = {Index, Tag, Span1},
%   Unresolved1 = [ TaggedSpan1 | Rest],
%   {"", Unresolved1}.

% get_close_tag(Tag, Unresolved) ->
%   Result      = lists:keytake(Tag, 2, Unresolved),
%   Tuple       = element(2, Result),
%   element(2, Tuple).




 
