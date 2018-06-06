%%% ==========================================================================
%%% ep_copyfit.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:
%%%   File:         ep_copyfit.erl
%%%   Description:  Page makeup functions 
%%% @end

%%% ==========================================================================


-module (ep_copyfit).

% -export([text/1, alter_font/3]).

-compile(export_all).

-define(PDF_DIR, "./pdf/").


% content_blocks(Type, MDFileName) ->
typespec_content(Type, MDFileName) ->
  ContentBlocks = md_parse:parse(MDFileName),
  ep_typespec:specify(Type, ContentBlocks).


do_content_blocks(ContentBlocks) ->
  [do_content_block(ContentBlock) || ContentBlock <- ContentBlocks].

do_content_block(ContentBlock) ->
   Text = text(ContentBlock),
   io:format("~n~p~n", [Text]).

get_fontspec(ContentBlock) ->
   {TypeSpec, TaggedText} = ContentBlock,
   {TypeSpec, TaggedText}.


justification(ContentBlock) ->
   FontMap = element(1, ContentBlock),
   maps:get(justification, FontMap).

leading(ContentBlock) ->
   FontMap = element(1, ContentBlock),
   maps:get(leading, FontMap).

point_size(ContentBlock) ->
   FontMap = element(1, ContentBlock),
   maps:get(pt_size, FontMap).

tag_map(ContentBlock) ->  
   FontMap = element(1, ContentBlock),
   maps:get(tagmap, FontMap).

tag(ContentBlock) ->
   TaggedText = element(2, ContentBlock),
   element(1, TaggedText).

text(ContentBlock) ->
   TaggedText = element(2, ContentBlock),
   element(2, TaggedText).






galley(JumpList, TypeTag, MDFileName) ->
    ContentBlocks = typespec_content(TypeTag, MDFileName),
    io:format("ep_copyfit:galley/3 JumpList: ~p~n", [JumpList]),
    ContentBlocks.



available_lines(Box, ContentBlock) ->
   Leading = leading(ContentBlock),
   ep_box:available_lines(Box, Leading).

line_specs(Box, ContentBlock) ->
    AvailableLines = available_lines(Box, ContentBlock),
    {_, _, Measure, _} = ep_box:text_box(Box),
%    {_, _, Measure, _} = ep_panel:text_box(Box),
    Indent = ep_box:indent(Box),
    {AvailableLines, Measure, Indent}.


layout_widths(JumpList, CopyBlock) ->
   Tag = tag(CopyBlock),
   layout_widths([], JumpList, Tag, CopyBlock).

layout_widths(Widths, [], Tag, _CopyBlock) ->
   Widths1 = lists:reverse(Widths),
   indent_paragraph(Widths1, Tag);

layout_widths(Widths, JumpList, Tag, CopyBlock) ->
   [Box | Rest] = JumpList,
   {VacantLines, Measure, Indent} = line_specs(Box, CopyBlock),
   Widths1 = text_widths(VacantLines, Measure, Indent),
   Widths2 = lists:append(Widths1, Widths),
   layout_widths(Widths2, Rest, Tag, CopyBlock).



%% Testing

normalize(Tag, ContentBlock, TagMap) ->
    Text = text(ContentBlock),
    Text1 = {Tag, [], Text},
    eg_xml2richText:normalise_xml(Text1, TagMap).

%% Question: What is EpText





function_test() ->
   JumpList = jump_list(),
   ContentBlocks = galley(JumpList, report, "test.md"),
   ContentBlock = hd(ContentBlocks),
   Tag = tag(ContentBlock),
   TagMap = tag_map(ContentBlock),
   normalize(Tag, ContentBlock, TagMap).
   








%% Broken
    

%    [{TypeSpec, Justification, Tag, ContentBlocks}] = content_blocks(TypeTag, MDFileName),
%    io:format("galley TypeSpec 24 ~n~p~n~n", [TypeSpec]),

%    Widths = layout_widths(JumpList, Tag, TypeSpec),
%    io:format("Widths: ~n~p~n~n", [Widths]),
%    Lines  = lines(RichText, Justification, Widths),
%    io:format("I'm at galley: 27~n~p~n~n", [Lines]),
%    Quotas = quotas(Lines, TypeSpec, JumpList),
%    io:format("I'm at galley: 29~n~p~n~n", [Quotas]).


%% ********************************************************* 
%% rich_text/2
%% Transform markdown text to richText
%% Returns Typespec, Justification, richText
%% Typetags: report 
%% Typespec: Type specification drerived from TypeTag
%% Justification: justified | ragged | preformatted | centered | 
%%      ragged_force_split | simple_hyphenate
%% richText: List of display specifications for every
%%    word in Text
%% ********************************************************* 


%    TypeSpecs = get_type_specs(Typext),
%    [TypeSpec | _Rest] = TypeSpecs,
%    io:format("rich_text 49: ~n~p~n~n", [CopySpecs]),
%    blip_text(TypeSpec).

%    Item = hd(List),
%    {TypeSpec, TaggedText} = Item,

% -spec ep_copyfit:get_type_parameters(list()) -> list().

% get_type_parameters(TypedCopyBlocks) ->
%   [type_parameters(CopyBlock) || CopyBlock <- TypedCopyBlocks].

   

% -spec ep_copyfit:type_parameters(tuple()) -> {map(), tuple()}.

% type_parameters(TypedCopyBlock) ->
%    {{TypeSpec}, ContentBlock} = TypedCopyBlock,
%    io:format("type_parameters/1 - TypeSpec: 65 ~n~p~n~n", [TypeSpec]),
%    io:format("type_parameters/1 - ContentBlock: 65 ~n~p~n~n", [ContentBlock]),
%    {TypeSpec, ContentBlock}.




%    {Tag, EpText} = parse(ContentBlock),

%    -spec type_params(map()) -> {atom(),

%    {Justification, TagMap} = type_params(TypeSpec),
%    {_,_,RichText} = normalize(Tag, EpText, TagMap),

%    io:format("****** type_parameters/1 Tag 64: ~n~p~n~n", [Tag]),
%    io:format("****** type_parameters/1 EpText 64: ~n~p~n~n", [EpText]).

%    io:format("****** rich_text/2 Tag 54: ~n~p~n~n", [Tag]),
%    io:format("****** rich_text/2 RichText 54: ~n~p~n~n", [RichText]),
%     {TypeSpec, Justification, Tag, RichText}.



    


%% ********************************************************* 
%% rich_text/2 helper
%% ********************************************************* 



parse(TaggedText) ->
    {Tag, Text} = TaggedText,
%    io:format("********** Text Line 70: ~n~p~n~n", [Text]),
    EpText = ep_parse:text(Text),  %% LRP's parser
%    io:format("********** EPText Line 70: ~n~p~n~n", [EpText]),
    EpText1 = ep_parse:eptext2xml(EpText),
    {Tag, EpText1}.

type_params(TypeSpec) ->
   Justification = ep_type:justification(TypeSpec),
   TagMap        = ep_type:tagmap(TypeSpec),
   {Justification, TagMap}.


%% ********************************************************* 
%% layout_widths/2
%% Return widths of boxes in JumpList 
%% JumpList: List of boxes designated to display an article
%% Article: Markdown text
%% Leading: From Typespec 
%% ********************************************************* 



jump_list() ->
   [box1(), box2(), box3(), box4()].


box1() ->
   Box = ep_box:create(72, 680, 300, 48),
   ep_box:update_indent(20, Box).

box2() ->
   ep_box:create(72, 620, 145, 48).


box3() ->
   ep_box:create(227, 620, 145, 200).


box4() ->
   ep_box:create(72, 720, 500, 600).

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


%   ep_panel:vacant_lines(Box, Leading).

%% ********************************************************* 
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
        {Lines,_,_} ->  io:format("I'm here~n~p~n~n",[Lines]),
                          Lines
   end.


%% ********************************************************* 
%% Paste-up functions 
%% ********************************************************* 

% paste_text(PDF, Text, TypeTag, JumpList) ->
%   {TypeSpec, Justification, Tag, RichText} = rich_text(TypeTag, Text),

%   Widths        = layout_widths(JumpList, Tag, TypeSpec),
%   Lines         = lines(RichText, Justification, Widths),
%   case length(Lines) > length(Widths) of
%      true  -> {error, layout_overflow} ;
%      false -> paste(PDF, Lines, TypeSpec, JumpList)
%   end.


%% ********************************************************* 
%% paste_text/4 helpers 
%% ********************************************************* 

paste(PDF, Lines, TypeSpec, JumpList) ->
    Quotas = quotas(Lines, TypeSpec, JumpList),
    paste_element(PDF, TypeSpec, JumpList, Quotas).


paste_element(PDF, Typespec, JumpList, Quotas) ->
    Zip = lists:zip(JumpList, Quotas),
    [paste_box(PDF, Typespec, Lines, JumpList, Box, []) || {Box, Lines} <- Zip].


paste_box(PDF, Typespec, Lines, JumpList, Box, []) ->
    % LineCount = length(Lines),
    % Leading = ep_typespec:leading(Typespec), 
%    Consumed = LineCount * Leading,
    %Vacancies = ep_box:available_lines(Box, Leading),
    % Flag = ep_box:will_fit(Box, Lines, Leading),
    CodeList = encode_text(PDF, Typespec, Lines, JumpList, Box, []),
    box_to_pdf(PDF, Box, CodeList).

encode_text(PDF, Typespec, Lines, JumpList, Box, CodeList) ->
  Code = ep_richText2pdf:richText2pdf(PDF, Lines, JumpList, Box, Typespec),
  [Code | CodeList].

box_to_pdf(PDF, Box, CodeList) ->
   Flag = ep_box:if_border(Box),
   case Flag of
      true  -> render_box(PDF, Box),
               render_text(PDF, CodeList, Box);
               % render_box(PDF, Box);
      false -> render_text(PDF, CodeList, Box)
   end.

render_box(PDF, Box) ->
   {Border, BorderColor, BGColor, TxtColor} = ep_box:background(Box),
%   {Border, BorderColor, BGColor, TxtColor} = ep_panel:background(Box),
   {X, Y, W, H} = ep_panel:outer_box(Box),
    eg_pdf:set_line_width(PDF, Border),
    eg_pdf:set_fill_color(PDF, BorderColor),
    eg_pdf:rectangle(PDF,{X - Border, Y - Border },{W + (2 * Border),H + (2 * Border)}),
    eg_pdf:path(PDF, fill),
    eg_pdf:set_fill_color(PDF, BGColor),
    eg_pdf:rectangle(PDF,{X, Y},{W,H}),
    eg_pdf:path(PDF, fill),
    ep_layout_prims:check_box(PDF, X, Y),
    eg_pdf:set_fill_color(PDF, TxtColor).

render_text(PDF, CodeList, Box) ->
    {X, Y, _W, _H} = ep_box:text_box(Box),
%    {X, Y, _W, _H} = ep_panel:text_box(Box),
    eg_pdf:set_text_pos(PDF, X, Y),
    lists:reverse(CodeList),
    eg_pdf:begin_text(PDF),
   [eg_pdf:append_stream(PDF, Code) || Code <- CodeList].


paste_paragraph() ->
%    OFile = "paste_paragraph.pdf",
    OFile = "paste_all.md.pdf",
    PDF = eg_pdf:new(),
    eg_pdf:set_pagesize(PDF, letter),
    eg_pdf:set_page(PDF, 1),

%    test1(PDF),

   {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file(?PDF_DIR ++ OFile,[Serialised]),
    eg_pdf:delete(PDF).


%% ********************************************************* 
%% quotas/2
%% ********************************************************* 

quotas(Lines, TypeSpec, JumpList) ->
    QuotaList     = layout_quotas(JumpList, TypeSpec),
    io:format("quotas 296: ~n~p~n~n", [QuotaList]),
    fill_quotas(QuotaList, Lines).

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
    io:format("fill_quotas: 339 ~n~p~n~n", [NewQuotas]),
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

