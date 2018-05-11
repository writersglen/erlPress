%%==========================================================================
%% ep_layout_prims.erl
%%% {c) 2017 Lloyd R. Prentice
%%% Author:     Lloyd R. Prentice
%%% License: 
%%% File:       ep_boxes.erl
%%% Description: 
%%%    Layout primitives 
%%==========================================================================



-module (ep_layout_prims).

-export ([layout_grid/1, print_page_map/2, print_crop_marks/2, page_outline/5, print_outline/6]).
-export ([print_box/2, print_red_box/1, print_two_column_red/1]).
-export ([ report_grid_p1/0
         , page_grid/1
         , page_dim/0 ]).
-export ([ test_box/0
         , test_text/0
         , check_box/3
         , print_line/0
         , print_nested_boxes/4
         , print_circle/0
         , print_circle1/0
         , print_solid_circle/0
         , print_ellipse/0
         , print_open_ellipse/0
         , print_solid_ellipse/0

]).


-include("../../include/ep.hrl").

-define(ELEMENT_DIR,  "./pdf/elements/").

%%==========================================================================
%%  layout_grid/1 - layout planning sheet
%%    PaperStock = "letter", "legal", "a4"
%%    This grid would be more useful if:
%%       Y values incremented from top left corner of paper stock
%%       Grid resolution was 12 points; e.g. distance between h_lines
%%          and v_lines was one pica 
%%==========================================================================

layout_grid(PaperStock)->
    PageType = atom_to_list(PaperStock),
    OFile = PageType ++ "_layout.pdf",
    PDF = eg_pdf:new(),

    ep_show_grid:show_grid(PDF, PaperStock),

   {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file(?ELEMENT_DIR ++ OFile,[Serialised]),
    eg_pdf:delete(PDF).






page_outline(PDF, X, Y, W, H) ->
   box_outline(PDF, X, Y, W, H).

print_outline(Color, Border, X, Y, W, H) ->
    OFile = "print_outline.pdf",
    PDF = eg_pdf:new(),
    eg_pdf:set_pagesize(PDF, letter),
    eg_pdf:set_stroke_color(PDF, Color),
    eg_pdf:set_line_width(PDF, Border),
    eg_pdf:set_line_join(PDF, bevel_join),
    eg_pdf:set_page(PDF, 1),

    box_outline(PDF, X, Y, W, H),
    box_outline(PDF, 100, 100, 100, 100),

   {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file(?ELEMENT_DIR ++ OFile,[Serialised]),
    eg_pdf:delete(PDF).


%%==========================================================================
%% report_grid_p1/0
%%==========================================================================
 
report_grid_p1() ->
   PageDim = page_dim(),
   BoxDims = page_grid(report_p1),
   print_grid("report_p1", 1, PageDim, BoxDims).
   

%%==========================================================================
%% report_grid_p1/0 -- Helpers
%%==========================================================================
    
page_grid(report_p1) ->
    [{header, 108, 720, 432, 24},
     {copy,   108, 134, 432, 500},
     {footer, 108,  72, 432, 24}
    ].

page_dim() ->
    {0, 0, 610, 792}.
 
%%==========================================================================
%% print_grid/4 
%%==========================================================================
    
print_grid(GridName, Border, PageDim, BoxDims) ->
    {PX, PY, PW, PH} = PageDim,
    PDF = eg_pdf:new(),
    eg_pdf:set_pagesize(PDF, letter),
    eg_pdf:set_stroke_color(PDF, black),
    eg_pdf:set_line_width(PDF, Border),
    eg_pdf:set_line_join(PDF, bevel_join),
    eg_pdf:set_page(PDF, 1),

     box_outline(PDF, PX, PY, PW, PH),
     eg_pdf:set_stroke_color(PDF, blue),
    [box_outline(PDF,  X,  Y,  W,  H) || {_Header, X, Y, W, H} <- BoxDims],

   {Serialised, _PageNo} = eg_pdf:export(PDF),
    CustomName = GridName ++ "_grid.pdf",
    file:write_file(?ELEMENT_DIR ++ CustomName,[Serialised]),
    eg_pdf:delete(PDF).

%%==========================================================================
%% print_grid/4 -- Helpers
%%==========================================================================

box_outline(PDF, X, Y, W, H) ->
  NE = {X,Y},
  NW = {X + W, Y},
  SE = {X, Y + H},
  SW = {X + W, Y + H},
  Top    = {NE, NW},
  Right  = {NW, SW},
  Bottom = {SW, SE},
  Left   = {SE, NE},
  eg_pdf:line(PDF, Top),
  eg_pdf:line(PDF, Right),
  eg_pdf:line(PDF, Bottom),
  eg_pdf:line(PDF, Left).




check_box(PDF, X, Y) ->
    eg_pdf:set_stroke_color(PDF, black),
    box_outline(PDF, X, Y, 12, 12).


print_line() ->
    OFile = "print_line.pdf",
    Line = {{90,540},{522,540}},
    PDF = eg_pdf:new(),
    eg_pdf:set_pagesize(PDF, letter),
    eg_pdf:set_stroke_color(PDF, red),
    eg_pdf:set_line_width(PDF, 1),
    eg_pdf:line(PDF, Line),
   {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file(?ELEMENT_DIR ++ OFile,[Serialised]),
    eg_pdf:delete(PDF).



box(PDF, OLColor, BGColor, X, Y, W, H) ->
    LineWidth = 1,
    eg_pdf:set_line_width(PDF, LineWidth),
    eg_pdf:set_stroke_color(PDF, OLColor),
    eg_pdf:set_fill_color(PDF, BGColor),
    eg_pdf:rectangle(PDF,{X, Y},{W,H}),
    eg_pdf:path(PDF, fill_stroke).


outer_box(PDF, X, Y, W, H) ->
    box(PDF, black, white, X, Y, W, H).

inner_box(PDF, X1, Y1, W1, H1) ->
    box(PDF, black, red, X1, Y1, W1, H1).


print_nested_boxes(X, Y, W, H) ->
   B = 20,
   X1 = X + B,
   Y1 = Y + B,
   W1 = W - (2 * B),
   H1 = H - (2 * B),
   PDF = eg_pdf:new(),
   eg_pdf:set_pagesize(PDF, letter),
   eg_pdf:set_page(PDF, 1),

   outer_box(PDF, X, Y, W, H),
   inner_box(PDF, X1, Y1, W1, H1),

   {Serialized, _PageNo}  = eg_pdf:export(PDF),
   file:write_file("./pdf/elements/nested__boxes.pdf", [Serialized]),
   eg_pdf:delete(PDF).


print_circle() ->
    OFile = "test_circle.pdf", 
    PDF = eg_pdf:new(),
    eg_pdf:set_dash(PDF,solid),
    eg_pdf:set_stroke_color(PDF,black),
    eg_pdf:set_fill_color(PDF,red),
    eg_pdf:circle(PDF, {72,720}, 10),
    eg_pdf:path(PDF, fill_stroke),
   {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file(?ELEMENT_DIR ++ OFile,[Serialised]),
    eg_pdf:delete(PDF).


print_solid_circle() ->
    OFile = "test_solid_circle.pdf", 
    PDF = eg_pdf:new(),
    eg_pdf:set_dash(PDF,solid),
    eg_pdf:set_stroke_color(PDF,black),
    eg_pdf:circle(PDF, {72,720}, 10),
    eg_pdf:path(PDF, fill_stroke),
   {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file(?ELEMENT_DIR ++ OFile,[Serialised]),
    eg_pdf:delete(PDF).

print_ellipse() ->
    OFile = "test_ellipse.pdf", 
    PDF = eg_pdf:new(),
    eg_pdf:set_dash(PDF,solid),
    eg_pdf:set_stroke_color(PDF,black),
    eg_pdf:set_fill_color(PDF, red),
    eg_pdf:ellipse(PDF, {200,720}, {100, 50}),
    eg_pdf:path(PDF, fill_stroke),
   {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file(?ELEMENT_DIR ++ OFile,[Serialised]),
    eg_pdf:delete(PDF).

print_open_ellipse() ->
    OFile = "test_open_ellipse.pdf", 
    PDF = eg_pdf:new(),
    eg_pdf:set_dash(PDF,solid),
    eg_pdf:set_stroke_color(PDF,black),
    eg_pdf:ellipse(PDF, {200,720}, {100, 50}),
    eg_pdf:path(PDF, stroke),
   {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file(?ELEMENT_DIR ++ OFile,[Serialised]),
    eg_pdf:delete(PDF).


print_solid_ellipse() ->
    OFile = "test_open_ellipse.pdf", 
    PDF = eg_pdf:new(),
    eg_pdf:set_dash(PDF,solid),
    eg_pdf:set_stroke_color(PDF,black),
    eg_pdf:ellipse(PDF, {200,720}, {100, 50}),
    eg_pdf:path(PDF, fill_stroke),
   {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file(?ELEMENT_DIR ++ OFile,[Serialised]),
    eg_pdf:delete(PDF).


print_circle1() ->
    OFile = "test_circle.pdf", 
    PDF = eg_pdf:new(),
%    eg_pdf:set_pagesize(PDF, letter),
%    eg_pdf:set_fill_color(PDF, red),
%    eg_pdf:circle(PDF, {X, Y}, R),
%    eg_pdf:path(PDF, close_fill_stroke),

%    eg_pdf:new_page(PDF),
%    eg_pdf:line(PDF,100,100,200,100),
%    eg_pdf:bezier(PDF,100,100,100,200,200,200,200,100),
%    eg_pdf:path(PDF,stroke),
%    eg_pdf:set_fill_color_RGB(PDF,0.5,0.7,0.2),
%    eg_pdf:bezier(PDF,300,100,300,200,500,200,500,100),
%    eg_pdf:path(PDF,close_fill_stroke),
%    eg_pdf:set_fill_color(PDF,purple),
%    eg_pdf:bezier(PDF,{300,400},{300,450},{500,450},{500,400}),
%    eg_pdf:bezier_c(PDF,{500,350},{400,350},{300,400}),
%    eg_pdf:path(PDF,fill),
%    eg_pdf:set_dash(PDF,dash),
%    eg_pdf:set_fill_color(PDF,slateblue),
%    eg_pdf:line(PDF,100,250,400,250),
%    eg_pdf:poly(PDF,[{100,300},{150,350},{200,350},{250,300}]),
%    eg_pdf:path(PDF,fill_stroke),
    eg_pdf:set_dash(PDF,solid),
    eg_pdf:set_stroke_color(PDF,black),
    eg_pdf:circle(PDF, {72,720}, 1),
    eg_pdf:path(PDF, fill_stroke),
%    eg_pdf:set_stroke_color(PDF, {16#00,16#FF,16#00}),
%    eg_pdf:circle(PDF, {200,300}, 50),
%    eg_pdf:path(PDF, stroke),
%    eg_pdf:ellipse(PDF, {200,300}, {50,100}),
%    eg_pdf:path(PDF, stroke),
%    eg_pdf:ellipse(PDF, {200,300}, {100,50}),
%    eg_pdf:path(PDF, stroke),
%    eg_pdf:circle(PDF, {200,300}, 100),
%    eg_pdf:path(PDF, stroke),
%    eg_pdf:grid(PDF,[50,100,150],[600,700,800]),
%    eg_pdf:round_rect(PDF,{300,600},{200,100},20),
%    eg_pdf:path(PDF, stroke),
%    eg_pdf:rectangle(PDF,{300,600},{200,100}, stroke),

%    eg_pdf:set_page(PDF, 1),


   {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file(?ELEMENT_DIR ++ OFile,[Serialised]),
    eg_pdf:delete(PDF).

%% doc ep_print:print_page_map(Job, "xxx.pdf")

print_page_map(Job, OFile) ->
    Top    = ep_job:page_top_coords(Job),
    Right  = ep_job:page_right_coords(Job),
    Bottom = ep_job:page_bottom_coords(Job),
    Left   = ep_job:page_left_coords(Job),
    {StockWidth, StockHeight} = ep_job:get_stock_dim(Job),
    PDF = eg_pdf:new(),
    eg_pdf:set_pagesize(PDF, StockWidth, StockHeight),
    eg_pdf:set_fill_gray(PDF, 0.75),
    eg_pdf:set_line_width(PDF, 1),
    eg_pdf:set_page(PDF, 1),
    eg_pdf:line(PDF, Top),
    eg_pdf:line(PDF, Right),
    eg_pdf:line(PDF, Bottom),
    eg_pdf:line(PDF, Left),
   {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file(?ELEMENT_DIR ++ OFile,[Serialised]),
    eg_pdf:delete(PDF).

%% doc ep_print:crop_marks_map(Job, "xxx.pdf")

print_crop_marks(Job, OFile) ->
    {NHFrom, NHTo, NVFrom, NVTo} = ep_job:n_crop(Job),
    {EHFrom, EHTo, EVFrom, EVTo} = ep_job:e_crop(Job),
    {SHFrom, SHTo, SVFrom, SVTo} = ep_job:s_crop(Job),
    {WHFrom, WHTo, WVFrom, WVTo} = ep_job:w_crop(Job),
    {StockWidth, StockHeight} = ep_job:get_stock_dim(Job),
    PDF = eg_pdf:new(),
    eg_pdf:set_pagesize(PDF, StockWidth, StockHeight),
    eg_pdf:set_fill_gray(PDF, 0.75),
    eg_pdf:set_line_width(PDF, 1),
    eg_pdf:set_page(PDF, 1),
    eg_pdf:line(PDF, NHFrom, NHTo),
    eg_pdf:line(PDF, NVFrom, NVTo),
    eg_pdf:line(PDF, EHFrom, EHTo),
    eg_pdf:line(PDF, EVFrom, EVTo),
    eg_pdf:line(PDF, SHFrom, SHTo),
    eg_pdf:line(PDF, SVFrom, SVTo),
    eg_pdf:line(PDF, WHFrom, WHTo),
    eg_pdf:line(PDF, WVFrom, WVTo),
   {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file(?ELEMENT_DIR ++ OFile,[Serialised]),
    eg_pdf:delete(PDF).

%% See: doc/pdf.md for list of StrokeTypes
%%  close, stroke, close_stroke, fill, fill_even_odd, fill_stroke, 
%% fill_then_stroke, fill_stroke_even_odd, close_fill_stroke, 
%% close_fill_stroke_even_odd, or endpath

print_box(Box, OFile) ->
    StrokeType = close,
    {X, Y, Measure, Height} = ep_box:box_spec(Box),

    PDF = eg_pdf:new(),
    eg_pdf:set_pagesize(PDF, letter),
    eg_pdf:set_fill_gray(PDF, 0.75),
    eg_pdf:set_line_width(PDF, 1),
    eg_pdf:set_page(PDF, 1),
%    eg_pdf:set_fill_color(PDF, red),

    eg_pdf:rectangle(PDF, X, Y, Measure, Height, StrokeType),

   {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file(?ELEMENT_DIR ++ OFile,[Serialised]),
    eg_pdf:delete(PDF).



print_red_box(OFile) ->
    PDF = eg_pdf:new(),
    eg_pdf:set_pagesize(PDF,letter),
    eg_pdf:set_page(PDF,1),
    eg_pdf:set_fill_color(PDF,red),

    eg_pdf:rectangle(PDF, 72,648,180,72, fill),

    {Serialised, _PageNo} = eg_pdf:export(PDF),
    file:write_file(?ELEMENT_DIR ++ OFile,[Serialised]),
    eg_pdf:delete(PDF).

    
print_two_column_red(OFile) ->
    PDF = eg_pdf:new(),
    eg_pdf:set_pagesize(PDF,letter),
    eg_pdf:set_page(PDF,1),
    eg_pdf:set_fill_color(PDF,red),

    eg_pdf:rectangle(PDF, 72,  576, 180, 144, fill),
    eg_pdf:rectangle(PDF, 270, 576, 180, 144, fill),

    {Serialised, _PageNo} = eg_pdf:export(PDF),

    file:write_file(?ELEMENT_DIR ++ OFile,[Serialised]),
    eg_pdf:delete(PDF).


%% =======================================================
%% Text blocks
%% =======================================================




test_text() ->
  "This is normal text with some *emphasised term.* The TeX\nhyphenation algorithm is also implemented.\nI have also some *cursive text* and an example of\nan Erlang term. The term `{person, \"Joe\"}` is an Erlang term.\nThe variable *X*, was immediately followed by\na comma. The justification algorithm does proper _kerning_,\nwhich is more than *Microsoft Word* can do. AWAY again is\ncorrectly kerned! Erlang terms *{like, this}*\nare typeset in *courier*.".    



%    TestString = "Width = " ++ eg_pdf_op:n2s(Width) ++ " -- Height = " ++ eg_pdf_op:n2s(Height),
%    LabelString = SheetName ++ " template planning sheet-",
%    Stringsize = eg_pdf:get_string_width(PDF, "Times-Roman", 36, TestString),
%    TargetSize = 24,
%    Indent = round(Width * 0.15),
%    FontSize = round(TargetSize * (((Width - (2 * Indent)) / Stringsize ))),
%    eg_pdf:set_font(PDF,"Times-Roman", FontSize),
%    Base = round(Height * 0.71),
%    eg_pdf_lib:moveAndShow(PDF, Indent,Base, LabelString),

%    eg_pdf_lib:moveAndShow(PDF, Indent, Base - (FontSize + 4), TestString),

% print_box(Box, OFile) ->
%    PageWidth  = maps:get(page_width, Box),    
%    PageHeight = maps:get(page_height, Box),    
%    Width      = maps:get(width, Box),
%    Height    = maps:get(height, Box),
%    X         = maps:get(x, Box),
%    Y         = PageHeight - maps:get(y, Box),
    

%    PDF = eg_pdf:new(),
%    eg_pdf:set_pagesize(PDF, PageWidth, PageHeight),
%    eg_pdf:set_fill_gray(PDF, 0.75),
%    eg_pdf:set_line_width(PDF, 1),
%    eg_pdf:set_page(PDF, 1),
    % Top line
%    eg_pdf:line(PDF, X, Y, X + Width, Y),
%    % Right line 
%    eg_pdf:line(PDF, X + Width, Y, X + Width, Y - Height),
%    % Bottom line 
%    eg_pdf:line(PDF, X + Width, Y - Height, X, Y - Height),
%    % Left line 
%    eg_pdf:line(PDF, X,  Y - Height, X, Y),


 %  {Serialised, _PageNo} = eg_pdf:export(PDF),
 %   file:write_file(?PDF_DIR ++ OFile,[Serialised]),
 %   eg_pdf:delete(PDF).


test_box() ->
   #{background => none,
     from_here => 100,
     id => "my box",
     length => 100,
     line_height => 14,
     max_height => 100,
     max_width => 100,
     next_line => 100,
     pad_bottom => 0,
     pad_left => 0,
     pad_right => 0,
     pad_top => 0,
     page_height => 792,
     page_width => 612,
     status => empty,
     width => 100,
     x => 20,
     y => 500,
     z => 0}.
