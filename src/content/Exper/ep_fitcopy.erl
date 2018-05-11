%%% *********************************************************   
%%% ep_copyfit.erl
%%%
%%% {c) 2017 Lloyd R. Prentice
%%% Author:     Lloyd R. Prentice
%%% License: 
%%% File:       ep_parse.erl
%%% Description: 
%%%    Page make up functions  
%%% ********************************************************* 

-module (ep_fitcopy).

% -export([text/1, alter_font/3]).

-compile(export_all).

-define(PDF_DIR, "./pdf/").



%% ********************************************************* 
%% rich_text/3
%% Transform markdown text to richText
%% Returns Typespec, Justification, richText
%% Typetags: report 
%% Typespec: Type specification drerived form TypeTag
%% Justification: justified | ragged | preformatted | centered | 
%%      ragged_force_split | simple_hyphenate
%% richText: List of display specifications for every
%%    word in Text
%% ********************************************************* 


rich_text(TypeTag, Text) ->
    io:format("******ep_fitcopy:rich_text Text: ~p~n", [Text]),
    TypedText = ep_typespec:specify(TypeTag, Text),
    rich_text(TypedText).

rich_text(TypedText) ->
   {Typespec, TaggedText} = get_copyspecs(TypedText),

   io:format("parse - Typespec 40: ~n~p~n~n", [Typespec]),
   io:format("parse - TaggedText 40: ~n~p~n~n", [TaggedText]),
%   Tag = element(1, TaggedText),
%   EpText = [element(2, TaggedText)],
   {Tag, EpText} = parse(TaggedText),
   io:format("parse - EpText 42: ~n~p~n~n", [EpText]),
   {Justification, TagMap} = type_params(Typespec),

   {_, _, RichText} = normalize(Tag, EpText, TagMap),

   {Typespec, Justification, Tag, RichText}.


%% ********************************************************* 
%% rich_text/2 helper
% *********************************************************  

get_copyspecs(TypedArticle) ->
   [TypedText | _TypedCopyList] = TypedArticle,
   get_copyspec(TypedText).

get_copyspec(TypedText) ->
   {Typespec, TaggedText} = TypedText,
   {Typespec, TaggedText}.  

parse(Copy) ->
    {Tag, Text} = Copy,
    EpText = ep_parse:text(Text),
    EpText1 = ep_parse:eptext2xml(EpText),
    {Tag, EpText1}.


type_params(TypeSpec) ->
   Justification = ep_typespec:justification(TypeSpec),
   TagMap        = ep_typespec:tagmap(TypeSpec),
   {Justification, TagMap}.

normalize(Tag, EpText, TagMap) ->
    EpText1 = {Tag, [], EpText},
    eg_xml2richText:normalise_xml(EpText1, TagMap).



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


paste_image(PDF, FilePath, Position, Size) ->
    eg_pdf:image(PDF, FilePath, Position, Size).

%% ********************************************************* 
%% Paste-up functions 
%% ********************************************************* 

paste_text(PDF, Text, TypeTag, JumpList) ->
   {TypeSpec, Justification, Tag, RichText} = rich_text(TypeTag, Text),
   Widths        = layout_widths(JumpList, Tag, TypeSpec),
   Lines         = lines(RichText, Justification, Widths),
   case length(Lines) > length(Widths) of
      true  -> {error, layout_overflow} ;
      false -> paste(PDF, Lines, TypeSpec, JumpList)
   end.

%% ********************************************************* 
%% paste_text/4 helpers 
%% ********************************************************* 
  
paste(PDF, Lines, TypeSpec, JumpList) ->  
    Quotas = quotas(Lines, TypeSpec, JumpList),
    paste_element(PDF, TypeSpec, JumpList, Quotas).


%% NOTE: Need to fix this

paste_element(PDF, Typespec, JumpList, Quotas) ->
    Zip = lists:zip(JumpList, Quotas),
%    [paste_box(PDF, Typespec, Lines, JumpList, Box, []) || {Box, Lines} <- Zip].
    [paste_box(PDF, Typespec, Lines, Box, []) || {Box, Lines} <- Zip].


% paste_box(PDF, Typespec, Lines, JumpList, Box, []) ->
paste_box(PDF, Typespec, Lines, Box, []) ->

    % LineCount = length(Lines),
    % Leading = ep_typespec:leading(Typespec), 
%    Consumed = LineCount * Leading,

    %Vacancies = ep_box:available_lines(Box, Leading),

    % Flag = ep_box:will_fit(Box, Lines, Leading),

    CodeList = encode_text(PDF, Typespec, Lines, Box, []),
%    CodeList = encode_text(PDF, Typespec, Lines, JumpList, Box, []),

    box_to_pdf(PDF, Box, CodeList).


%% Work-in-progress: test carefully

encode_text(PDF, Typespec, Lines, Box, CodeList) ->
  Code = ep_richText2pdf:richText2pdf(PDF, Lines, Box, Typespec),
  [Code | CodeList].


% encode_text(PDF, Typespec, Lines, JumpList, Box, CodeList) ->
%  Code = ep_richText2pdf:richText2pdf(PDF, Lines, JumpList, Box, Typespec),
%  [Code | CodeList].

box_to_pdf(PDF, Box, CodeList) ->
   Flag = ep_box:if_border(Box),
%   Flag = ep_panel:if_border(Box),
%   io:format("Background flag: ~p~n", [Flag]),
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

%% ********************************************************* 
%% Panel functions 
%%
%% NOTE: Y coordinates in grids run 0 to height of paper stock.
%%       They must be inverted, e.g. height of paper stock to 0
%%       for rendering in PDF.
%% ********************************************************


get_panel() ->
   Grid = ep_grid:report(),
   PaperStock = ep_grid:paper_stock(Grid),
   Panel = ep_grid:get_panel(Grid, "body"),
   Panel1 = invert_panel(Panel, PaperStock),
   PanelMap = element(3, Panel1),
   PanelMap.

content_here(Panel) ->
   Y = ep_panel:y(Panel),
   Filled = ep_panel:filled(Panel),
   Y - Filled.




invert_panel(Panel, PaperStock) ->
   Y = ep_panel:y(Panel),
   Y1 = ep_lib:v_flip(PaperStock, Y),
   ep_panel:update_y(Panel, Y1).


    
%% ********************************************************* 
%% Test functions 
%% ********************************************************

paste_blocks(PDF) ->
   Blocks = tokens(),
   [paste_block(PDF, Block) || Block <- Blocks].
  
get_guide() ->
   file:read_file("/home/lloyd/epDocs/user_guide_EP/user_guide.md").
 
tokens() ->
   {ok, MD} = file:read_file("/home/lloyd/epDocs/user_guide_EP/user_guide.md"),
   Text = binary_to_list(MD),
   Tokens = string:tokens(Text, [$\n,$\n]),
   Tokens.


paste_block(PDF, Block) ->
   TypeTag   = report,
   JumpList  = jump_list(),
   paste_text(PDF, Block, TypeTag, JumpList).




test5(PDF) ->
   paste_blocks(PDF).





h1() ->
    "# Fake News is No News". 

h2() ->
    "## Fake News is No News". 

h3() ->
    "### Fake News is No News". 

h4() ->
    "#### Fake News is No News". 
   
h5() ->
    "# ErlPress".

paragraph() ->
  "This is normal text with *emphasized terms*. It implements the TeX\nhyphenation algorithm.\nIt includes *cursive text* and an Erlang term, `{person, \"Joe\"}`. \nThe variable `X`, is immediately followed by\na comma. The justification algorithm does proper _kerning_,\nwhich is more than *Microsoft Word* can do. AWAY is\ncorrectly kerned! Erlang terms `{like, this}`\nare typeset in `courier`.".



box4() ->
   Panel = get_panel(),
   invert_panel(Panel, letter).




image_box() ->
   Box = ep_box:create(72, 720, 300, 80),
   Box1 = ep_box:update_indent(1, Box),
   [Box1].


jump_list() ->
   [box5(), box2(), box3()].


box1() ->
   Box = ep_box:create(72, 680, 100, 18),
   ep_box:update_indent(20, Box).
%  ep_panel:update_indent(20, Box).

box2() ->
   ep_box:create(72, 620, 145, 48).


box3() ->
   ep_box:create(227, 620, 145, 200).


paste_paragraph(Text) ->
    OFile = "paste_paragraph.pdf",
    PDF = eg_pdf:new(),
    eg_pdf:set_pagesize(PDF, letter),
    eg_pdf:set_page(PDF, 1),
    test1(PDF, Text),
   {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file(?PDF_DIR ++ OFile,[Serialised]),
    eg_pdf:delete(PDF).

paste_h1() ->
    OFile = "paste_h1.pdf",
    PDF = eg_pdf:new(),
    eg_pdf:set_pagesize(PDF, letter),
    eg_pdf:set_page(PDF, 1),
    test2(PDF),
   {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file(?PDF_DIR ++ OFile,[Serialised]),
    eg_pdf:delete(PDF).

paste_h4() ->
    OFile = "paste_h4.pdf",
    PDF = eg_pdf:new(),
    eg_pdf:set_pagesize(PDF, letter),
    eg_pdf:set_page(PDF, 1),
    test3(PDF),
   {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file(?PDF_DIR ++ OFile,[Serialised]),
    eg_pdf:delete(PDF).


paste_image() ->
    OFile = "paste_image.pdf",
    PDF = eg_pdf:new(),
    eg_pdf:set_pagesize(PDF, letter),
    eg_pdf:set_page(PDF, 1),

%    headline(PDF),
    test5(PDF),
 %   test1(PDF),
    image(PDF),
    
   {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file(?PDF_DIR ++ OFile,[Serialised]),
    eg_pdf:delete(PDF).

% image(PDF) ->
%    eg_pdf:image(PDF, FilePath, {72, 340}, {width, 140}).

image(PDF) ->
    Position = {72, 340},
    Size   = {width, 140},
    FilePath = ep_image:image_path("test", "book_cover.jpg"),
    paste_image(PDF, FilePath, Position, Size).
    


headline(PDF) ->
   Text      = h5(),
   TypeTag   = report,
   JumpList  = image_box(),
   paste_text(PDF, Text, TypeTag, JumpList).



test1(PDF, Text) ->
%   Text      = paragraph(),
   TypeTag   = report,
   JumpList  = jump_list(),
   paste_text(PDF, Text, TypeTag, JumpList).

test2(PDF) ->
   Text      = h1(),
   TypeTag   = report,
   JumpList  = jump_list(),
   paste_text(PDF, Text, TypeTag, JumpList).

test3(PDF) ->
   Text      = h4(),
   TypeTag   = report, JumpList  = jump_list(),
   paste_text(PDF, Text, TypeTag, JumpList).

test4(PDF) ->
%   {ok, Text}  = file:read_file("/home/lloyd/EP/user_guide/user_guide.md"),
   Text      = paragraph(),
   TypeTag   = report,
   JumpList  = jump_list(),
   paste_text(PDF, Text, TypeTag, JumpList).





jump_list2() ->
   [box4].

box5() ->
   ep_box:create(72, 720, 100, 50).

    


%    paste_box(PDF, TypeSpec, Lines, Box, []), 
   
% paste() ->
%    Text   = paragraph(),
%    {TypeSpec, Justification, RichText} = rich_text(report, Text),
%    Box     = box4(),
%    JumpList = [Box],



%    Widths = layout_widths(JumpList, p, TypeSpec),
%    Lines  = lines(RichText, Justification, Widths),
    

%    OFile = "paste.pdf",
%    PDF = eg_pdf:new(),
%    eg_pdf:set_pagesize(PDF, letter),
%    eg_pdf:set_page(PDF, 1),

    
%    paste_box(PDF, TypeSpec, Lines, Box, []), 

%   {Serialised, _PageNo} = eg_pdf:export(PDF),
%    file:write_file(?PDF_DIR ++ OFile,[Serialised]),
%    eg_pdf:delete(PDF).















% text_block(NLines, Measure, Indent) ->
%    Widths = widths(NLines, Measure, Indent),
%    break_lines(RichText, Justification, Widths).

offsets(NLines, Indent) ->
    [Indent|lists:duplicate(NLines-1, 0)].

typespec(TypeMap, Text) ->
    ep_typespec:specify(TypeMap, Text).


text_box(bq, Box) ->
    % adjust measure
    Box1 = Box,
    Box1;

text_box(_, Box) ->
    Box.

% if_copy_overflow(Boxes, TypeSpec, Lines) -> 
%  [Box|Jumplist] = Boxes,
%  NLines = ep_box:n_lines(Box, TypeSpec),
%  CopyLines = length(Lines),
%  CopyLines > NLines.


copy_overflow(PDF, Typespec, Lines, Boxes) ->
   paste_text(PDF, Typespec, Lines, Boxes).


% update_box(Box, Lines, Typespec) ->
%  NoLines = length(Lines),
%  TextLines = NoLines + 1,
%  Leading = ep_typespec:leading(Typespec),
%  Box1 = ep_box:hide_background(Box),
%  ep_box:update_to_next_line(TextLines, Leading, Box1).

more_copy(PDF, CopyList, _Boxes) ->
  case CopyList == [] of
     true   -> eg_pdf:end_text(PDF);
     false  -> ok % article(PDF, CopyList, Boxes) 
  end.


split_lines(Typespec, Lines, Box) ->
    NLines = ep_box:n_lines(Box, Typespec),
    CopyLines = length(Lines),
    CopyOverflow = CopyLines > NLines,
    case CopyOverflow of
      true  -> {Lines1, MoreLines} = lists:split(NLines, Lines);
      false -> Lines1 = Lines,
               MoreLines = []
    end,
    {Lines1, MoreLines}.

%% ********************************************************* 
%% Copyfit - article/3
%% ********************************************************* 


% line_ptr(Boxes) ->
%    Box = hd(Boxes),
%    LinePtr = ep_box:get_cur_y(Box),
%    io:format("LinePtr: ~p~n", [LinePtr]),
%    LinePtr.



test_type_spec(Text) ->
   TypedText = ep_typespec:report_spec(Text),
   {Typespec, _TaggedText} = get_copyspecs(TypedText),
   Typespec.


%% ********************************************************* 
%% Resources 
%% ********************************************************* 







