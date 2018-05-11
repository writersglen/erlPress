
%%% *********************************************************
%%% {c) 2016 Lloyd R. Prentice
%%% Author:     Lloyd R. Prentice
%%% License: 
%%% File:       ep_boxes.erl
%%% Description: 
%%%    Experimental print routines 
%%% *********************************************************   


-module (ep_exper4).

% -export([test/0, if_hard_break/1, parse_paragraph/1]).

-compile([export_all]).


%% ***************************************************************
%% Sample text rendered in markdown
%% https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet
%% ***************************************************************

%% Need to preserve blank lines


text() ->
   "# Header1 
## Header2
### Header3
#### Header4
##### Header5
###### Header6 
     
This is a *paragraph* with __emphasicized__ elements

This is a much longer
paragraph. New line characters
should probably be deleted.
> This is a blockquote
* Unordered list
+ Unordered list
- Unordered list
1. Ordered list
2. Ordered list
".


%% ***************************************************************
%% Parse markdown text -- test
%% ***************************************************************


test() ->
  parse(text()).


%% ***************************************************************
%% Parse markdown text
%% ***************************************************************

parse(Text) ->
   List = prep_text(Text),
   parser(List).

prep_text(Text) ->
%    Text1 = drop_spaces(Text),
    Text1 = re:replace(Text, [$\n], [$\n,$\f], [global, {return, list}]),
    string:tokens(Text1, [$\f]).



parser(List) ->
    io:format("Length text: ~p~n", [length(List)]),
    List1 = [{[hd(String)], String} || String <- List],
    List2 = tag_lines(List1),
    List3 = ennumerate_list_items(List2),
    gather_lines(List3).
    
%    io:format("List: ~p~n", [List2]),
%    List2.

%%    [parse_path(Tuple) || Tuple <- List2].

ennumerate_list_items(List) ->
   N = length(List),
   List1 = lists:seq(1, N),
   lists:zip(List1, List).



tag_lines(List) ->
   tag_lines(List, []).

tag_lines([], TaggedLines) ->
   lists:reverse(TaggedLines);

tag_lines(List, TaggedLines) ->
   [Tuple| Rest] = List,
   Line = element(2, Tuple), 
   Result = tag_line(Line),
   Tagged = [Result | TaggedLines],
   tag_lines(Rest, Tagged).
   

tag_line(Line) ->
   T = [hd(Line)],
   case T of 
      [$#]  -> parse_header(Line);
      [$\s] -> {space, Line};
      [$*]  -> {asterisk, Line};
      [$+]  -> {unordered_list, Line};
      [$-]  -> {hyphen, Line};
      [$_]  -> {underscore, Line};
      [$[]  -> {link, Line};
      [$`]  -> {backtick, Line};
      [$!]  -> {image, Line};
      [$\n] -> {empty_line, Line};
      [$>]  -> {block_quote, Line};
      _     -> parse_misc(Line) 
   end.


parse_header(Line) ->
   {N, Char, Line1} = count_chars(Line),
   Flag = N > 6,
   case Flag of
      true   -> {error, Char, Line1};
      false  -> Char1 = integer_to_list(N), 
                HTag  = "header" ++ Char1,
                Tag = list_to_atom(HTag),
                {Tag, Line1}
   end.

parse_misc(Line) ->
   T = [hd(Line)],
   Result = string:to_integer(T),
    case  Result of
       {error, _}  -> {p, Line};
       _           -> {ordered_list, Line} 
    end.








parse_path(Tuple) ->
   {LineNo, {Tag, String}} = Tuple,
   Result = string:to_integer(Tag),
    case  Result of
       {error, _}  -> parse_elements(LineNo, Tag, String);
       _           -> parse_ordered_list(LineNo, String)
    end.

parse_elements(LineNo, Tag, String) ->
    case Tag of
       "#"   -> parse_header(LineNo, String);
       " "   -> parse_space(LineNo, String);
       _     -> parse_misc(LineNo, Tag, String)
    end.

parse_ordered_list(LineNo, String) ->
    {LineNo, {ordered_list, 0, String}}.


parse_header(LineNo, String) ->
   {N, Char, String1} = count_chars(String),
   Flag = N > 6,
   case Flag of
      true   -> {error, Char, String1, LineNo};
      false  -> Char1 = integer_to_list(N), 
                HTag  = "header" ++ Char1,
                Tag = list_to_atom(HTag),
                parse_in_line(LineNo, Tag, String1)
   end.

parse_space(LineNo, String) ->
    {LineNo, space, String}.

parse_misc(LineNo, Tag, String) ->
   io:format("Misc tag: ~p~n", [Tag]),
   case Tag of
     "\n"  -> {LineNo, blank_line, []};
     _     -> String1 = re:replace(String, [$\s], [$\s, $\f], [global, {return, list}]),
              List = string:tokens(String1, [$\f]),
              {LineNo, paragraph, List}
   end.



gather_lines(TaggedList) ->
  gather_lines(TaggedList, []).

gather_lines([], GatheredList) ->
  lists:reverse(GatheredList);
 
gather_lines(TaggedList, []) ->
  [Tagged | Rest] = TaggedList,
  GatheredLines = [Tagged | []],
  gather_lines(Rest, GatheredLines);

gather_lines(TaggedList, GatheredList) ->
  [Tagged | Rest] = TaggedList,
  {_, {NextTag, NextText}} = Tagged,

  [Gathered | Previous] = GatheredList,
  {LineNo, {LastTag, LastText}} = Gathered,

  Flag = LastTag == NextTag,
  case Flag of
     true  -> NewLine = {LineNo, {LastTag, LastText ++ NextText}},
              GatheredList1 = [NewLine | Previous];  
     false -> GatheredList1 = [Tagged | GatheredList]
  end,
  gather_lines(Rest, GatheredList1).
  
  
   


% append_line(Line, PreviousLine) ->
%   lists:concat(PreviousLine, Line).

parse_in_line(LineNo, Tag, String) ->
  {LineNo, Tag, String}.

%% ***************************************************************
%% Parse in-line 
%% ***************************************************************

% parse_in_line(LineNo, Tag, String) ->

if_hard_break(String) ->
   N = string:rstr(String, "  "),
   N > 0.


%% ***************************************************************
%% Helpers 
%% ***************************************************************


emph(String) ->
   [H | Rest] = String,
   io:format("Emph: ~p~n", [H]),
   case H of
      $*  -> strong_emph(Rest);
      $_  -> strong_emph(Rest);
      _    -> String
   end.

strong_emph(String) ->
   [H | Rest] = String,
   case H of
      $*    -> T = {strong_emph, {Rest}};
      $_    -> {strong_emph, {Rest}};
      _     -> {emph, {String}}
   end.

% end_strong_emph(Tuple) ->
%   {Tag, String} = Tuple,
%   Flag1 = string:rstr(String, "**")

parse_paragraph(String) ->
   S = string:tokens(String, " "),
   [ emph(S1) || S1 <- S].




count_chars([]) ->
   {0, undefined, undefined};

count_chars(String) ->
   Char = hd(String),
   count_chars(String, Char, 0).

count_chars([], Char, N) ->
   {N, Char, undefined};

count_chars(String, Char, N) ->
   [First | Rest ] = String,
   Flag = First == Char,
   case Flag of
      true  -> count_chars(Rest, Char, N + 1);
      _  -> {N, [Char], Rest}
   end.



