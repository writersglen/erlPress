
%%% *********************************************************   
%%% {c) 2016 Lloyd R. Prentice
%%% Author:     Lloyd R. Prentice
%%% License: 
%%% File:       ep_boxes.erl
%%% Description: 
%%%    Experimental print routines 
%%% *********************************************************   

-module (ep_exper5).

% -export([str1/0, parsed/0, text/0, test/1, parse/2]).
% -export([test/1, parse_paragraph/1, count_chars/1]).
% -export([str0/0, str1/0, str2/0, str3/0, str4/0]).
% -export([init_tag/1, push_tag/2, top_tag/1, drop_tag/1, replace_tag/2]).
% -export([e_or_b/5, count_spaces/1]).
% -export([show_state/4]).

-compile([export_all]).


%% ***************************************************************
%% Sample text rendered in markdown
% https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet
%% ***************************************************************

%% Need to preserve blank lines

str0() ->
  "my sentence without emphasized text". % pass

str1() ->
  "my *sentence with* _emphasized_ text". % pass

str2() ->
  "my sen*tence with* _emphasized_ text".  % pass

str3() ->
  "my *sentence* with __emphasized__ text". % fial

str4() ->
  "my **sentence with** __emphasized__ text". % fail


%% ***************************************************************
%% Parse markdown text -- test
%% ***************************************************************

test(Text) ->
  parse_paragraph(Text).


%% ***************************************************************
%% Parse markdown text
%% ***************************************************************


parse_paragraph(Text) ->
   io:format("Text: ~p~n~n", [Text]),
   TagStack = init_tag(p),
   % Functionally, * and _ have the same meaning in markdown
   % So let's simplify by replacing all _ with *
   Text1 = re:replace(Text, [$_], [$*], [global, {return, list}]),
   parse_paragraph(Text1, [], [], TagStack).

parse_paragraph([], Word, Parsed, TagStack) ->
   Tag = hd(TagStack),
   Word1 = lists:reverse(Word),
   List = [{word, 0, Tag, Word1 } | Parsed],
   lists:reverse(List);

parse_paragraph(Text, Word, Parsed, TagStack) ->
   [Char | Rest] = Text,
   case Char of
       $\s -> parse_space(Rest, Word, Parsed, TagStack);
       $\n -> parse_nl(Rest, Word, Parsed, TagStack);

       $*  -> % io:format("Word: ~p~n", [Word]),
              parse_em(Rest, Word, Parsed, TagStack);

       _   -> Word1 = [Char | Word],
              parse_paragraph(Rest, Word1, Parsed, TagStack)
   end.
    
parse_space(Text, Word, Parsed, TagStack) ->
   % io:format("Parsed: ~p~n", [Parsed]),
   Tag = hd(TagStack),
   Parsed1 = post_word(Tag, Word, Parsed),
   Parsed2 = [{space, 0, Tag} | Parsed1],
   parse_paragraph(Text, [], Parsed2, TagStack).   

parse_nl(Text, Word, Parsed, TagStack) ->
   Tag = hd(TagStack),
   Parsed1 = post_word(Tag, Word, Parsed),
   Parsed2 = [{nl, 0, Tag} | Parsed1],
   parse_paragraph(Text, [], Parsed2, TagStack).   

parse_em(Text, Word, Parsed, TagStack) ->
     Tag = em,
     parse_tag(Tag, Text, Word, Parsed, TagStack). 

parse_tag(Tag, Text, Word, Parsed, TagStack) ->
     TopTag = top_tag(TagStack),
     Flag = Tag == TopTag,
     case Flag of
        true  -> close_tag(Text, Word, Parsed, TagStack);
        false -> open_tag(Tag, Text, Word, Parsed, TagStack)
     end.


e_or_b(Tag, Text, Word, Parsed, TagStack) ->
     [NextChar | Text1] = Text,
     Flag = NextChar == Tag,
     case Flag of
         true  -> TagStack1 = [NextChar | TagStack],
                  {Text1, [], Parsed, TagStack1};
         false -> Word1 = [NextChar | Word],
                  {Text1, Word1, Parsed, TagStack}
     end.


%% count_spaces/1 will be useful in justication routines
%% JUSTIFIED
%% Get width of line
%% Get Increment = (Measure - Width) / Spaces
%% Add Increment to each space
%% CENTER
%% Set line at X + (Measure - Width) /2
%% RIGHT
%% Set line at X + (Measure - Width)
     
count_spaces(AText) ->
   count_spaces(AText, 0).

count_spaces([], Spaces) ->
   Spaces;

count_spaces(AText, Spaces) ->
   [Item | Rest ] = AText,
   Token = element(1, Item),
   case Token of
      space -> Spaces1 = Spaces + 1; 
      _     -> Spaces1 = Spaces
   end,
   count_spaces(Rest, Spaces1).


show_state(Text, Word, Parsed, TagStack) ->
   io:format("Parse em: 
 Tag:      ~p 
 Text:     ~p
 Word:     ~p
 Parsed:   ~p
 TagStack: ~p~n", ["*", Text, Word, Parsed, TagStack]).

%% **********************************************************

post_word(Tag, Word, Parsed) ->
   L = length(Word),
   case L of
      0   -> Parsed;
      _   -> Word1 = lists:reverse(Word),
             [{word, 0, Tag, Word1} | Parsed]
   end.

close_tag(Text, Word, Parsed, TagStack) ->
    TopTag = top_tag(TagStack),
    Parsed1 = post_word(TopTag, Word, Parsed),
    TagStack1 = drop_tag(TagStack),
    parse_paragraph(Text, [], Parsed1, TagStack1).

open_tag(Tag, Text, Word, Parsed, TagStack) ->
    TopTag = top_tag(TagStack),
    Parsed1 = post_word(TopTag, Word, Parsed),
    TagStack1 = push_tag(Tag, TagStack),
    parse_paragraph(Text, [], Parsed1, TagStack1).
    

%% ******************************************************







init_tag(Tag) ->
   Stack = [],
   [Tag | Stack].

push_tag(Tag, TagStack) ->
   [Tag | TagStack].

top_tag([]) ->
   empty;

top_tag(TagStack) ->
   hd(TagStack).
  
drop_tag([]) ->
   stack_underflow;

drop_tag(TagStack) ->
   [_ | Rest] = TagStack,
   Rest.

replace_tag(Tag, TagStack) ->
  TagStack1 = drop_tag(TagStack),
  push_tag(Tag, TagStack1).


count_chars([]) ->
   0;

count_chars(String) ->
   Char = hd(String),
   count_chars(String, Char, 0).

count_chars([], _Char, N) ->
   N;

count_chars(String, Char, N) ->
   [First | Rest ] = String,
   Flag = First == Char,
   case Flag of
      true  -> count_chars(Rest, Char, N + 1);
      _  -> N
   end.


