%%% *********************************************************   
%%% ep_paste_copy.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:
%%%   File:         ep_paste_copy.erl
%%%   Description:  Page makup functions 
%%% @end

%%% ==========================================================================


-module (ep_paste_copy).

-export([print_copy/3
        , split_quota/2
        , box_to_pdf/3 ]).

-include("../../include/ep.hrl").

-define(PAPER_SIZE, letter).
-define(PAGE_SIZE, letter).

%% Need to clean up and document code in this module
%% Need to feed copy from list
%% Need to insert page jump note
%% Need to render lists
%% Need to render check boxes
%% Need to render tables
%% Need to accept accept MarkDown input


-define(LEADING_FACTOR, 1.25).

%% Test
%% FileName:  "tc1.xml"
%% Format:    report
%% Layout:    Layout = ep_copyfit:layout()
%% Test:      ep_copyfit:print_copy("tc1.xml", report, Layout)


%% ********************************************************* 
%% print_copy/3
%% ********************************************************* 

%% @doc Paste up copy

-spec print_copy(FileName :: list(), Format :: atom(), Layout :: list()) -> {ok, pasted}. 

print_copy(FileName, Format, Layout) ->
    OFile = FileName ++ ".pdf",
    PDF = eg_pdf:new(),
    eg_pdf:set_pagesize(PDF, ?PAGE_SIZE),
    eg_pdf:set_page(PDF, 1),
    CopyBlocks = get_copy_blocks(FileName, Format),
    boxes_to_pdf(PDF, Layout),
    paste_blocks(PDF, CopyBlocks, Layout),
     {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file(?PDF_DIR ++ OFile,[Serialised]),
    eg_pdf:delete(PDF),
    {ok, pasted}.


%% ********************************************************* 
%% ********************************************************* 
%% print_copy/3 helpers
%% ********************************************************* 
%% ********************************************************* 

%% ********************************************************* 
%% get_copy_blocks/2
%% ********************************************************* 

%% @doc Given file name and format, return type specified 
%%      copy

-spec get_copy_blocks(FileName :: list(), Format :: atom()) -> list().

get_copy_blocks(FileName, Format) ->
    XML        = get_copy(FileName),
    Xml        = parse_copy(XML),
    typespec_copy(Format, Xml).

%% ********************************************************* 
%% boxes_to_pdf/2
%% ********************************************************* 

%% @doc Print box borders and background

-spec boxes_to_pdf(PDF :: identifier(), Layout :: list()) -> ok. 

boxes_to_pdf(PDF, Layout) ->
    [render_box(PDF, Box) || Box <- Layout].


%% ********************************************************* 
%% paste_blocks/3
%% ********************************************************* 

%% @doc Paste up list of copy blocks where copy block
%%      is {TypeSpec, Copy}

-spec paste_blocks(PDF :: identifier(), CopyBlocks :: list(), Layout :: list()) -> tuple().
paste_blocks(_PDF, _CopyBlocks, [ok]) ->
    {ok, copy_pasted};

paste_blocks(_PDF, [], _Layout) ->
    copy_pasted;

paste_blocks(PDF, CopyBlocks, Layout) ->
    [CopyBlock | Rest] = CopyBlocks,
        % io:format("~n////  entering paste-block/3 CopyBlock;~n~p~n~n", [CopyBlock]),
    Layout1 = paste_block(PDF, CopyBlock, Layout),
        % io:format("paste_block/3 Layout1 75 ~n~p ~n~n", [Layout1]),
    paste_blocks(PDF, Rest, Layout1).


%% ********************************************************* 
%% get_copy_blocks/2 - helpers
%% ********************************************************* 

get_copy(FileName) ->
    Path = "/home/lloyd/Dev/ep/src/copy/",
    {ok, XML} = file:read_file(Path ++ FileName),
    binary_to_list(XML).
    

parse_copy(XML) ->
   eg_xml_lite:parse_all_forms(XML).


typespec_copy(Format, Xml) ->
    ep_typespec:specify(Format, Xml).


%% ********************************************************* 
%% paste_block/3
%% ********************************************************* 

%% @doc Past up copy block where copy block
%%      is {TypeSpec, Copy}

-spec paste_block(PDF :: identifier(), CopyBlock :: list(), Layout :: list()) -> list().

paste_block(PDF, CopyBlock, Layout) ->
    {Lines, TypeSpec, Layout} = extract_lines(CopyBlock, Layout),
    {Quotas, TypeSpec1, Layout} = get_quotas(Lines, TypeSpec, Layout),
    paste_quotas(PDF, Quotas, TypeSpec1, Layout).


%% ********************************************************* 
%% paste_quotas/4
%% ********************************************************* 

%% @doc Paste list of text blocks represented as rich text

-spec paste_quotas(PDF :: identifier(), Quotas :: list(), 
                   TypeSpec :: map(), Layout :: list()) -> list().

paste_quotas(_PDF, [], _TypeSpec, Layout) ->
     Layout;

paste_quotas(PDF, Quotas, TypeSpec, Layout) ->
    [Quota | Rest] = Quotas,
    QuotaWillFit = quota_will_fit(Quota, TypeSpec, Layout),
    Layout1 = case QuotaWillFit of
       true  -> paste_quota(PDF, Quota, TypeSpec, Layout);
       false -> paste_quota(PDF, Quota, TypeSpec, tl(Layout))
    end,
    paste_quotas(PDF, Rest, TypeSpec, Layout1).


%% ********************************************************* 
%% paste_quota/4
%% ********************************************************* 

%% @doc Paste text block represented as rich text

-spec paste_quota(PDF :: identifier(), Quota :: list(), 
                  TypeSpec :: map(), Layout :: list()) -> list().

paste_quota(_PDF, [], _TypeSpec, Layout) ->
    Layout;

paste_quota(PDF, Quota, TypeSpec, Layout) ->
    [Box | Rest] = Layout,
    % returns updated box
    LineCount = length(Quota),
    Leading = maps:get(leading, TypeSpec),
    paste_text(PDF, Quota, TypeSpec, Box, []), 
    SpaceConsumed = LineCount * Leading,
    Box1 = ep_box:update_available(Box, SpaceConsumed),
    [Box1 | Rest].


%% ********************************************************* 
%% split_quota/2
%% ********************************************************* 

%% @doc Split list of text lines based on space available

-spec split_quota(AvailableLines :: integer(), Quota :: list()) -> list().

split_quota(AvailableLines, Quota) ->
     lists:split(AvailableLines, Quota).


%% ********************************************************* 
%% ********************************************************* 
%% paste_block/3 helpers
%% ********************************************************* 
%% ********************************************************* 

%% ********************************************************* 
%% extract_lines/2
%% ********************************************************* 

%% @doc Transfrom copy to rich text

-spec extract_lines(CopyBlock :: list(), Layout :: list()) -> tuple(). 

extract_lines(CopyBlock, Layout) ->
    TypeSpec = element(1, CopyBlock),
    {Widths, Lines} = ep_flow_control:rich_text(CopyBlock, Layout),
   case length(Lines) > length(Widths) of
      true  -> {error, layout_overflow} ;
      false -> {Lines, TypeSpec, Layout}
   end.
    
%% ********************************************************* 
%% get_quotas/3
%% ********************************************************* 

%% @doc Fit lines to boxes in Layout

-spec get_quotas(Lines :: list(), TypeSpec :: map(), Layout :: list()) -> tuple(). 


get_quotas(Lines, TypeSpec, Layout) ->
    Quotas = ep_flow_control:quotas(Lines, TypeSpec, Layout),
    {Quotas, TypeSpec, Layout}.

%% ********************************************************* 
%% quota_will_fit/3 
%% ********************************************************* 

%% @doc Test if list of lines fits in first box of Layout

-spec quota_will_fit(Quota :: list(), TypeSpec :: map(), Layout :: list()) -> boolean().

quota_will_fit(Quota, TypeSpec, Layout) ->
    [Box | _Rest] = Layout,
    LineCount      = length(Quota),
    Leading = maps:get(leading, TypeSpec),
    AvailableLines = ep_box:available_lines(Box, Leading),
        io:format("quota_will_fit/3 AvailableLines 245: ~n~p~n~n", [AvailableLines]),
        io:format("quota_will_fit/3 LineCount 245: ~n~p~n~n", [LineCount]),
    AvailableLines >= LineCount.


%% ********************************************************* 
%% paste_text/5
%% ********************************************************* 

%% @doc Paste up lines 

-spec paste_text(PDF :: identifier(), Quota :: list(), TypeSpec :: map(), 
                 Box :: map(), CodeList :: list()) -> tuple(). 

paste_text(PDF, Quota, Typespec, Box, []) ->

    CodeList = encode_text(PDF, Quota, Typespec, Box, []),
    text_to_pdf(PDF, Box, CodeList).

%% ********************************************************* 
%% encode_text/5
%% ********************************************************* 

%% @doc Rich text to pdf

-spec encode_text(PDF :: identifier(), Lines :: list(), TypeSpec :: map(), 
                  Box :: map(), CodeList :: list()) -> list().

encode_text(PDF, Lines, TypeSpec, Box, CodeList) ->
    io:format("encode_text/5 TypeSpec 284: ~n~p~n~n", [TypeSpec]),
    ensure_fonts_are_loaded(PDF, TypeSpec),
    Code = ep_richText2pdf:richText2pdf(PDF, Lines, Box, TypeSpec),
    [Code | CodeList].

%% ********************************************************* 
%% text_to_pdf/3
%% ********************************************************* 

%% @doc Render text to pdf

-spec text_to_pdf(PDF :: identifier, Box :: map(), CodeList :: list()) -> ok.

text_to_pdf(PDF, Box, CodeList) ->
    render_text(PDF, CodeList, Box).

%% ********************************************************* 
%% text_to_pdf/3
%% ********************************************************* 

%% @doc Render box to pdf

-spec box_to_pdf(PDF :: identifier, Box :: map(), CodeList :: list()) -> ok.

box_to_pdf(PDF, Box, CodeList) ->
   Flag = ep_box:if_border(Box),
   case Flag of
      true  -> % render_box(PDF, Box),
               render_text(PDF, CodeList, Box);
      false -> render_text(PDF, CodeList, Box)
   end.

%% ********************************************************* 
%% render_box/2
%% ********************************************************* 

%% @doc Render box to pdf

%% NOTE: Need to distinguish between box as column in page grid
%%       and box as content. So, perhaps, need render_column/2.
%%       This will be an issue when we print check lists

-spec render_box(PDF :: identifier, Box :: map()) -> ok.

render_box(PDF, Box) ->
   {Border, _BorderColor, BGColor, _TxtColor} = ep_box:background(Box),
   case Border > 0 of
      true  -> {X, Y, W, H} = ep_box:outer_box(Box),
               Margin = ep_box:text_margin(Box),
               %     eg_pdf_lib:showGrid(PDF, letter),
              eg_pdf:set_line_width(PDF, Border),
              eg_pdf:set_fill_color(PDF, BGColor), % BorderColor),
              eg_pdf:rectangle(PDF,{X, Y - H + Margin},{W,H}),
              eg_pdf:path(PDF, fill_stroke);
       false -> ok
    end.


%% ********************************************************* 
%% render_text/3
%% ********************************************************* 

%% @doc Render text to pdf

-spec render_text(PDF :: identifier, CodeList :: list(), Box :: map()) -> ok.

render_text(PDF, CodeList, Box) ->
    {XPtr, YPtr}   = ep_box:here(Box), 
       Here           = ep_box:here(Box),
       io:format(">>>>>>>>>>>>>>>>> render_text/3 Here 333: ~p~n~n", [Here]),
    TxtColor       = ep_box:text_color(Box),
    eg_pdf:set_text_pos(PDF, XPtr, YPtr),
    eg_pdf:set_fill_color(PDF, TxtColor),
    lists:reverse(CodeList),
    eg_pdf:begin_text(PDF),
    [eg_pdf:append_stream(PDF, Code) || Code <- CodeList],
    eg_pdf:end_text(PDF).


%% ********************************************************* 
%% ensure_fonts_are_loaded/2 
%% ********************************************************* 

ensure_fonts_are_loaded(PDF, TypeSpec) ->
    TagMap = maps:get(tagmap, TypeSpec),
    FontList = element(2, TagMap),
    lists:foreach(fun({_, Face}) ->
                          FontHandler = eg_richText:fontFromFace(Face),
                          Font = FontHandler:fontName(),
                          eg_pdf:ensure_font_gets_loaded(PDF, Font)
    end, FontList).
