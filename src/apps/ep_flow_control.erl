%%% *********************************************************
%%% ep_flow_control.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:
%%%   File:         ep_flow_control.erl
%%%   Description:  Manage content flow across panels and pages 
%%% @end

%%% ==========================================================================


-module (ep_flow_control).

-export([ rich_text/2
        , quotas/3
        , get_panel/1
        , content_here/1
        , invert_box/2
        , invert_panel/2 ]).


-define(LEADING_FACTOR, 1.25).
-define(P_INDENT, 20).
-define(LI_INDENT, 20).
-define(CODE_INDENT, 20).

%% ********************************************************* 
%% ********************************************************* 
%% RichText 
%% ********************************************************* 
%% ********************************************************* 

%% @doc Convert CopyBlock to RichText

-spec rich_text(list(), list()) -> list().

rich_text(CopyBlock, Layout) ->
   {TypeSpec, Xml}                         = CopyBlock,
   {Tag, _TagMap, Justification, RichText} = styling(TypeSpec, Xml),
   Widths                                  = widths(Tag, TypeSpec, Layout), 
   Lines                                   = lines(RichText, Justification, Widths),
   {Widths, Lines}.


%% ********************************************************* 
%% ********************************************************* 
%% Styling 
%% ********************************************************* 
%% ********************************************************* 

%% @doc Get RichText and styling attributes

styling(TypeSpec, Xml) ->
   {Justification, TagMap} = type_params(TypeSpec),
   Norm   = eg_xml2richText:normalise_xml(Xml, TagMap),
   {Tag, _, RichText} = Norm,
   {Tag, TagMap, Justification, RichText}.

%% ********************************************************* 
%% Styling helper 
%% ********************************************************* 

type_params(TypeSpec) ->
   Justification = ep_typespec:justification(TypeSpec),
   TagMap        = ep_typespec:tagmap(TypeSpec),
   {Justification, TagMap}.


%% ********************************************************* 
%% ********************************************************* 
%% Widths 
%% ********************************************************* 
%% ********************************************************* 

%% @doc Give TypeSpec and Layout, return list of line widths

widths(Tag, TypeSpec, Layout) ->
   layout_widths([], Tag, TypeSpec, Layout).

%% ********************************************************* 
%% widths/3 helpers 
%% ********************************************************* 

layout_widths(Widths, Tag, _TypeSpec, []) ->
   Widths1 = lists:reverse(Widths),
   adjust_widths(Widths1, Tag);

layout_widths(Widths, Tag, TypeSpec, Layout) ->
   % io:format("layout_widths/4 Layout 87: ~n~p~n~n", [Layout]),
   [Box | Rest] = Layout,
   {VacantLines, Measure, Indent} = line_specs(Box, TypeSpec),
   Widths1 = text_widths(VacantLines, Measure, Indent),
   Widths2 = lists:append(Widths1, Widths),
   layout_widths(Widths2, Tag, TypeSpec, Rest).

%% ********************************************************* 
%% layout_widths/2 helpers
%% ********************************************************* 

%% @doc Given TypeSpec, returns number of lines that fit in box,
%%      line width, and paragraph indent in pixels

line_specs(Box, TypeSpec) ->
    AvailableLines = available_lines(Box, TypeSpec),
    {_, _, Measure, _} = ep_box:text_box(Box),
    Indent = ep_box:indent(Box),
    {AvailableLines, Measure, Indent}.

adjust_widths(Widths, Tag) ->
   Widths1 = lists:reverse(Widths),
   case Tag of
      li   -> indent_list_item(Widths1); 
      code -> indent_code(Widths1); 
      _  -> indent_code(Widths) 
   end.

%% @doc Given TypeSpec, returns number of lines that fit in box

available_lines(Box, TypeSpec) ->
   Leading = ep_typespec:leading(TypeSpec),
   ep_box:available_lines(Box, Leading).

text_widths(VacantLines, Measure, Indent) ->
    [Measure - Indent|lists:duplicate(VacantLines - 1, Measure)].


%% ********************************************************* 
%% Copy indents
%% ********************************************************* 

% indent_paragraph(Widths, Tag) ->
%   Indent = indent(Tag),
%   [FirstWidth | Rest] = Widths,
%   Indented   = FirstWidth - Indent,
%   [Indented | Rest].

% indent(Tag) ->
%   case Tag == p of
%      true  -> ?P_INDENT;
%      false -> 0
%   end.

indent_list_item(Widths) ->
   [Width - ?LI_INDENT || Width <- Widths].

indent_code(Widths) ->
   [Width - ?CODE_INDENT || Width <- Widths].




%% ********************************************************* 
%% ********************************************************* 
%% Lines 
%% ********************************************************* 
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
%% Quotas 
%% ********************************************************* 

%%%%%%%%%%%%%%%% Broken

%% We have list of lines in this text block from lines/3 above
%%
%% Layout list provides a list of boxes.
%% layout_quotas/2, below, returns a quotas for each box in the layout list
%% fill_quotas/2...

%% paste(PDF, Lines, TypeSpec, Layout) needs to return updated Layout for paste up of next CopyBlock


quotas(Lines, TypeSpec, Layout) ->
    QuotaList     = layout_quotas(TypeSpec, Layout),
    fill_quotas(Lines, QuotaList).

%% ********************************************************* 
%% layout_quotas/2
%% Return number of lines that will fit in each box in JumpList 
%% JumpList: List of boxes designated to display an article
%% Leading: From Typespec: ; height of line, plus paddng above
%%      line
%% ********************************************************* 

layout_quotas(TypeSpec, Layout) ->
    layout_quotas([], TypeSpec, Layout).

layout_quotas(List, _TypeSpec, []) ->
    lists:reverse(List);

layout_quotas(List, TypeSpec, Layout) ->
    [Box | Rest] = Layout,
    AvailableLines = available_lines(Box, TypeSpec),
%    BoxQuota = {AvailableLines, Box},
    NewList = [AvailableLines|List], 
    layout_quotas(NewList, TypeSpec, Rest).


%% ********************************************************* 
%% fill_quotas/2 
%% Return list lines designated for each box in JumpList
%% QuotaList: List of number of lines that will fit in each 
%%     box in JumpList 
%% Lines: List of richText lines 
%% ********************************************************* 


fill_quotas(Lines, QuotaList) ->
    fill_quotas([],  Lines, QuotaList).





fill_quotas(Quotas, [], _Lines) ->
    lists:reverse(Quotas);

fill_quotas(Quotas, Lines, QuotaList) ->
    [NLines | NewList] = QuotaList,
     LL = length(Lines),
     NLines1 = min(LL, NLines),
    {Quota, RemainingQuotas} = fill_quota(NLines1, Lines),
    NewQuotas = [Quota | Quotas],
    fill_quotas(NewQuotas, RemainingQuotas, NewList).


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


%% ********************************************************* 
%% Panel functions 
%%
%% NOTE: Y coordinates in grids run 0 to height of paper stock.
%%       They must be inverted, e.g. height of paper stock to 0
%%       for rendering in PDF.
%% ********************************************************


get_panel(Grid) ->
   PaperStock = ep_grid:paper_stock(Grid),
   Panel = ep_grid:get_panel(Grid, "body"),
   Panel1 = invert_panel(Panel, PaperStock),
   PanelMap = element(3, Panel1),
   PanelMap.


content_here(Panel) ->
   Y = ep_panel:y(Panel),
   Filled = ep_panel:filled(Panel),
   Y - Filled.


invert_box(Box, PaperStock) ->
   Y = ep_box:y(Box),
   Y1 = ep_lib:v_flip(Y, PaperStock),
   ep_box:update_y(Y1, Box).


invert_panel(Panel, PaperStock) ->
   Y = ep_panel:y(Panel),
   Y1 = ep_lib:v_flip(Y, PaperStock),
   ep_panel:update_y(Y1, Panel).



