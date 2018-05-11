%%% *********************************************************   
%%% ep_layout.erl
%%%
%%% {c) 2018 Lloyd R. Prentice
%%% Author:     Lloyd R. Prentice
%%% License: 
%%% File:       ep_copyfit.erl
%%% Description: 
%%%    Layout functions  
%%% ********************************************************* 


-module (ep_layout).

-export([ layout_widths/3
        , lines/3
        , quotas/3
 ]).

-include("../../include/ep.hrl").


-define(LEADING_FACTOR, 1.25).


%% ********************************************************* 
%% layout_widths/2
%% Return widths of boxes in JumpList 
%% JumpList: List of boxes designated to display an article
%% Article: Markdown text
%% Leading: From Typespec 
%% ********************************************************* 

layout_widths(JumpList, Tag, TypeSpec) ->
   layout_widths([], JumpList, Tag, TypeSpec).

%%%% ***************
%% NOTE!!! The indent adjustment below needs to apply only to 
%% paragraphs


layout_widths(Widths, [], Tag, _TypeSpec) ->
   Widths1 = lists:reverse(Widths),
   indent_paragraph(Widths1, Tag);

layout_widths(Widths, JumpList, Tag, TypeSpec) ->
   [Box | Rest] = JumpList,
   {VacantLines, Measure, Indent} = line_specs(Box, TypeSpec),
   Widths1 = text_widths(VacantLines, Measure, Indent),
   Widths2 = lists:append(Widths1, Widths),
   layout_widths(Widths2, Rest, Tag, TypeSpec).

%% ********************************************************* 
%% layout_widths/2 helpers
%% ********************************************************* 

indent_paragraph(Widths, Tag) ->
   Indent = indent(Tag),
   [FirstWidth | Rest] = Widths,
   Indented   = FirstWidth - Indent,
   [Indented | Rest].

indent(Tag) ->
   case Tag == p of
      true  -> 20;
      false -> 0
   end.

text_widths(VacantLines, Measure, Indent) ->
    [Measure - Indent|lists:duplicate(VacantLines - 1, Measure)].

line_specs(Box, TypeSpec) ->
    AvailableLines = available_lines(Box, TypeSpec),
    {_, _, Measure, _} = ep_box:text_box(Box),
%    {_, _, Measure, _} = ep_panel:text_box(Box),
    Indent = ep_box:indent(Box),
    {AvailableLines, Measure, Indent}.

available_lines(Box, TypeSpec) ->
   Leading = ep_typespec:leading(TypeSpec),
   ep_box:available_lines(Box, Leading).
%   ep_panel:vacant_lines(Box, Leading).

%% ********************************************************* 
%% lines/3
%% Break richText into lines of specified width
%% Justification: justified | ragged | preformatted | centered | 
%%      ragged_force_split | simple_hyphenate
%% ********************************************************* 

lines(RichText, Justification, Widths)->
    break_lines(RichText, Justification, Widths).
 
%% ********************************************************* 
%% lines/3 helpers
%% ********************************************************* 

break_lines(RichText, Justification, Widths) ->
   case ep_line_break:break_richText(RichText, { Justification, Widths}) of
        impossible ->
            io:format("Cannot break line are widths ok~n");
        {Lines,_,_} -> Lines
   end.

%% ********************************************************* 
%% layout_quotas/2
%% Return number of lines that will fit in each box in JumpList 
%% JumpList: List of boxes designated to display an article
%% Leading: From Typespec: ; height of line, plus paddng above
%%      line
%% ********************************************************* 

layout_quotas(JumpList, TypeSpec) ->
    layout_quotas([], JumpList, TypeSpec).

layout_quotas(List, [], _TypeSpec) ->
    lists:reverse(List);

layout_quotas(List, JumpList, TypeSpec) ->
    [Box | Rest] = JumpList,
    AvailableLines = available_lines(Box, TypeSpec),
    NewList = [AvailableLines | List],
    layout_quotas(NewList, Rest, TypeSpec).


%% ********************************************************* 
%% quotas/2
%% ********************************************************* 

quotas(Lines, TypeSpec, JumpList) ->
    QuotaList     = layout_quotas(JumpList, TypeSpec),
    fill_quotas(QuotaList, Lines).


%% ********************************************************* 
%% fill_quotas/2 
%% Return list lines designated for each box in JumpList
%% QuotaList: List of number of lines that will fit in each 
%%     box in JumpList 
%% Lines: List of richText lines 
%% ********************************************************* 

fill_quotas(QuotaList, Lines) ->
    fill_quotas([], QuotaList, Lines).

fill_quotas(Quotas, [], _Lines) ->
    lists:reverse(Quotas);

fill_quotas(Quotas, QuotaList, Lines) ->
    [NLines | NewList] = QuotaList,
     LL = length(Lines),
     NLines1 = min(LL, NLines),
    {Quota, Rest} = fill_quota(NLines1, Lines),
    NewQuotas = [Quota | Quotas],
    fill_quotas(NewQuotas, NewList, Rest).

%% ********************************************************* 
%% fill_quotas/2 helper
%%
%% fill_quota/2 
%% Return a given number of lines
%% VacantLines: Unfilled text lines in a box
%% Lines: List of richText lines 
%% ********************************************************* 

fill_quota(NLines, Lines) ->
    Quota = lists:sublist(Lines, NLines),
    Rest  = lists:nthtail(NLines, Lines),
    {Quota, Rest}.
