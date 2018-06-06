%%==========================================================================
%%% ep_parse.erl
%%%
%%% Copyright (C) 2017 Lloyd R. Prentice
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a
%%% copy of this software and associated documentation files (the
%%% "Software"), to deal in the Software without restriction, including
%%% without limitation the rights to use, copy, modify, merge, publish,
%%% distribute, sublicense, and/or sell copies of the Software, and to permit
%%% persons to whom the Software is furnished to do so, subject to the
%%% following conditions:
%%% 
%%% The above copyright notice and this permission notice shall be included
%%% in all copies or substantial portions of the Software.
%%% 
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
%%% OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
%%% NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
%%% DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
%%% OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
%%% USE OR OTHER DEALINGS IN THE SOFTWARE.
%%%
%%% Author: Lloyd R. Prentice <lloyd@writersglen.com>, 
%%%
%%% Purpose: Proof and make up markdown-tagged text 
%%% ==========================================================================


-module (ep_tag_elements).

% -export([text/0, article/0, boxes/0, tag/1]).
% -export([proof_report/1, page_proof_report/2, report_to_pdf/2]).
% -export([report_lines/2, typespec_report/1]).
 
-compile(export_all).
 
%% ***************************************************************
%% Sample text rendered in markdown 
%% https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet
%% http://fletcher.github.io/MultiMarkdown-5/tables.html
%% ***************************************************************


%% ***************************************************************
%% Parse markdown text
%% ***************************************************************

tag(Article) ->
%    io:format("Article: ~p~n", [Article]),
    Text1 = prep_text(Article),
    List  = string:tokens(Text1, "$%"),
    tag_element(List, []).

%% ***************************************************************
%% Prep text 
%% ***************************************************************


prep_text(Text) ->
     re:replace(Text, [$\n,$\n], "%", [global, {return, list}]).

%% ***************************************************************
%% Tag text 
%% ***************************************************************


tag_element([], Parsed) ->
   lists:reverse(Parsed);

tag_element(List, Parsed) ->
   [Element | Rest] = List,
   Tag = string:left(Element, 1),
   Parsed1 = parse_tags(Tag, Element, Parsed),
   tag_element(Rest, Parsed1).


parse_tags(Tag, Copy, Parsed) ->
%   io:format("Copy: ~p~n", [Copy]),
   Result = string:to_integer(Tag),
   case element(1, Result) of
      error  -> parse_non_ul_tags(Tag, Copy, Parsed);
      _      -> Parsed
   end. 

parse_non_ul_tags(Tag, Copy, Parsed) ->
   case Tag of
      [$#]  -> io:format("Header~n"),
               Tagged = tag_header(Copy),
               [Tagged | Parsed];
      [$>]  -> io:format("Block quote~n"),
               Tagged = {bq, Copy},
               [Tagged | Parsed];
      [$*]  -> io:format("Unordered list item or maybe hr~n"),
               Parsed;
      [$-]  -> io:format("Unordered list item or maybe hr~n"),
               Parsed;
      [$+]  -> io:format("Unordered list item~n"),
               Parsed;
      [$\s] -> io:format("Blocks that begin with space~n"),
               Parsed;
      [$!]  -> io:format("Image~n"),
               Parsed;
      [$|]  -> io:format("Table~n"),
               Parsed;
      _     -> io:format("Paragraph~n"),
               Tagged = {p, Copy},
               [Tagged | Parsed]
   end.



%% ***************************************************************
%% Markdown parsers 
%% ***************************************************************

tag_header(Copy) ->
   Text = lists:dropwhile(fun(C) -> C /= $\s end, Copy),
   Text1 = string:strip(Text),
   Hashes = lists:takewhile(fun(C) -> C /= $\s end, Copy),
   Count = length(Hashes),
   N = integer_to_list(Count),
   Tag = list_to_atom("h" ++ N),
   {Tag, Text1}. 


%% ***************************************************************
%% Count lines 
%% ***************************************************************

max_lines(Box, TypeSpec) ->
    ep_copyfit:max_lines(Box, TypeSpec).

lines(TypedArticle, Boxes) ->
    ep_copyfit:lines(TypedArticle, Boxes).

line_count(TypedCopy, Boxes) ->
    ep_copyfit:line_count(TypedCopy, Boxes).

