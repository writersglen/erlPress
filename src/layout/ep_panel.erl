%%% *********************************************************
%%% ep_panel.erl
%%% 
%%% Author:     Lloyd R. Prentice
%%% License: 
%%% File:       ep_panel.erl
%%
%% NOTE: Panels "contain" content elements, either text or images.
%% Boxes are composed on "page grids" to organize content and
%% direct "eye path." 
%%
%% Panels wrap boxes (ep_box.erl) in a tuple to facilitate page grid
%% design and layout functions
%%
%% X = upper left X coordinate of panel in points  
%% Y = upper left Y coordinate of panel in points
%% Width = width of box in points
%% Height = height of box in pnts 
%% id = {page number, box number, alias}
%%
%% IMPORTANT NOTE:
%%
%% PDF assumes that 0, 0 XY coordinates are at lower-left
%% This seems unnatural for folks who read top-to_bottom
%% So for convenience, we'll assume that 0, 0 XY is
%% at upper-left of the panel.  This means that y will 
%% have to be inverted relative to paper stock height when we 
%% print.
%% 
%%% Description: 
%%%   Create content panels 
%%% *********************************************************   



-module (ep_panel).

-export([create/6]).
-export([index/1, name/1]).
-export([get_box/1, replace_box/2, id/1]).
-export([x/1, y/1, width/1, measure/1, height/1]).
-export([panel_dimensions/1]). 
-export([end_x/1, end_y/1]).
-export([x_within/2, y_within/2]).
-export([gutter/1, leading/1]).
-export([default_gutter/0, default_leading/0]). 
-export([filled/1, available/1, available_lines/2]).
-export([will_fit/3]).
-export([bg_flag/1, if_background/1, border/1, border_type/1, border_color/1]).
-export([text_margin/1, text_color/1, background_color/1]).
-export([stroke/1, stroke_color/1, fill_color/1]).
-export([indent/1, continue/1]).
-export([update_id/2]).
-export([update_x/2, update_y/2, update_width/2, update_height/2]).
-export([update_gutter/2, update_leading/2, update_filled/2]).
-export([update_border/2, update_border_type/2, update_border_color/2]).
-export([update_text_margin/2, update_text_color/2, update_background_color/2]).
-export([update_stroke/2, update_stroke_color/2, update_fill_color/2]).
-export([update_indent/2, update_continue/2]).
-export([position/1, dimensions/1, box_spec/1]).
-export([shift/3, clip/3]).
-export([outer_box/1, inner_box/1, text_box/1]).
-export([if_border/1, show_border/1, hide_border/1]).
-export([background/1, corners/1]).
-export([print_panel/3]).
  

% -compile(export_all).

-include("../../include/ep.hrl").

%% ***********************************************************
%% @doc Create panel 
%% ***********************************************************

-spec create(Index :: integer(), Name :: string(), X :: integer(), 
          Y :: integer(), Width :: integer(), Height :: integer()) -> map().

create(Index, Name, X, Y, Width, Height) ->
   Box = ep_box:create(X, Y, Width, Height),
   {Index, Name, Box}.

%% ***********************************************************
%% @doc Return index 
%% ***********************************************************

-spec index(Panel :: tuple()) -> integer().

index(Panel) -> 
   element(1, Panel).

%% ***********************************************************
%% @doc Return name 
%% ***********************************************************

-spec name(Panel :: tuple()) -> string().

name(Panel) ->
   element(2, Panel).


%% ***********************************************************
%% Unwrap box 
%% ***********************************************************

-spec get_box(Panel :: tuple()) -> map().

get_box(Panel) ->
   io:format("get_box/1 - Panel 107 ~n~p~n~n", [Panel]),
   element(3, Panel).

%% ***********************************************************
%% Replace box 
%% ***********************************************************

-spec replace_box(Panel :: tuple(), Box :: map()) -> tuple().

replace_box(Panel, Box) ->
   setelement(3, Panel, Box).


%% ***********************************************************
%% Get panal parameters 
%% ***********************************************************

%% @doc Return id of box

-spec id(Panel :: tuple()) -> string().

id(Panel) ->
    Box = get_box(Panel),
    ep_box:id(Box).

%% @doc Return left-most edge of panel 

-spec x(Panel :: tuple()) -> integer().

x(Panel) ->
    Box = get_box(Panel),
    ep_box:x(Box).

%% @doc Return y coordinate of vertical cursor 

-spec y(Panel :: tuple()) -> integer().

y(Panel) ->
    Box = get_box(Panel),
    ep_box:y(Box).

%% @doc return width of panel

-spec width(Panel :: tuple()) -> integer().

width(Panel) ->
    Box = get_box(Panel),
    ep_box:width(Box).

%% @doc Typeset speak; alias of width;  

-spec measure(Panel :: tuple()) -> integer().

measure(Panel) ->
    width(Panel).

%% @doc return height panel

-spec height(Panel :: tuple()) -> integer().

height(Panel) ->
    Box = get_box(Panel),
    ep_box:height(Box).


-spec panel_dimensions(Panel :: tuple()) -> tuple().

panel_dimensions(Panel) ->
  Width  = width(Panel),
  Height = height(Panel),
  {Width, Height}.




%% @doc Return right-most edge of panel 

-spec end_x(Panel :: tuple()) -> integer().

end_x(Panel) ->
    Box = get_box(Panel),
    ep_box:end_x(Box).

%% @doc Return bottom edge of box

-spec end_y(Panel :: tuple()) -> integer().

end_y(Panel) ->
    Box = get_box(Panel),
    ep_box:end_y(Box).

x_within(Panel, X) ->
    PanelX    = x(Panel),
    PanelEndX = end_x(Panel),
    (X >= PanelX) and (X =< PanelEndX).

y_within(Panel, Y) ->
    PanelY    = y(Panel),
    PanelEndY = end_y(Panel),
    (Y >= PanelY) and (Y =< PanelEndY).

%% @doc return space between panel an next panel to right 

-spec gutter(Panel :: tuple()) -> integer().

gutter(Panel) ->
    Box = get_box(Panel),
    ep_box:gutter(Box).

%% @doc return space between panel an next panel below 

-spec leading(Panel :: tuple()) -> integer().

leading(Panel) ->
    Box = get_box(Panel),
    ep_box:leading(Box).

%% @doc return default space between panel an next panel to right 

-spec default_gutter() -> integer().

default_gutter() ->
    ep_box:default_gutter().

%% @doc return default space between panel an next panel below 

-spec default_leading() -> integer().

default_leading() ->
    ep_box:default_leading().

%% @doc return vertical space in points filled by content  

-spec filled(Panel :: tuple()) -> integer().

filled(Panel) ->
    Box = get_box(Panel),
    ep_box:filled(Box).

%% @doc Return vertical space available for content 

-spec available(Panel :: tuple()) -> integer().

available(Panel) ->
    Box = get_box(Panel),
    ep_box:available(Box).

%% @doc Given leadind, return number of lines that will
%%   fit in available space 

-spec available_lines(Panel :: tuple(), Leading :: integer()) -> integer().

available_lines(Panel, Leading) ->
    Box = get_box(Panel),
    ep_box:available_lines(Box, Leading).

%% @doc Given leading, returns true if lines will fit in panel

-spec will_fit(Panel :: tuple(), Lines :: integer(), 
       Leading :: integer()) -> boolean().

will_fit(Panel, Lines, Leading) ->
    AvailableLines = available_lines(Panel, Leading),
    AvailableLines =< Lines.
   
%% @doc If true, display content in box with outline and color background

-spec bg_flag(Panel :: tuple()) -> boolean().

bg_flag(Panel) ->
    Box = get_box(Panel),
    ep_box:bg_flag(Box).

%% @doc Alias for bg_flag/1

if_background(Panel) ->
   bg_flag(Panel).


%% @doc Return width of outline around panel 

-spec border(Panel :: tuple()) -> integer().

border(Panel) ->
    Box = get_box(Panel),
    ep_box:border(Box).

%% @doc Return type of outline: soliid, dashed, beveled 

-spec border_type(Panel :: tuple()) -> atom().

border_type(Panel) ->
    Box = get_box(Panel),
    ep_box:border_type(Box).

%% @doc Return border color; white, silver, gray, black, maroon, 
%% red, fuchsia, purple lime, green, olive, yellow, navy, blue, teal, aqua


-spec border_color(Panel :: tuple()) -> atom().

border_color(Panel) ->
    Box = get_box(Panel),
    ep_box:border_color(Box).

%% @doc return margin: space around text when displayed in outlined panel

-spec text_margin(Panel :: tuple()) -> integer().

text_margin(Panel) ->
    Box = get_box(Panel),
    ep_box:text_margin(Box).

%% @doc Return text color; white, silver, gray, black, maroon, 
%% red, fuchsia, purple lime, green, olive, yellow, navy, blue, teal, aqua


-spec text_color(Panel :: tuple()) -> atom().

text_color(Panel) ->
    Box = get_box(Panel),
    ep_box:text_color(Box).

%% @doc Return background color; white, silver, gray, black, maroon, 
%% red, fuchsia, purple lime, green, olive, yellow, navy, blue, teal, aqua

-spec background_color(Panel :: tuple()) -> atom().

background_color(Panel) ->
    Box = get_box(Panel),
    ep_box:background_color(Box).

%% @doc See eg_pdf_op.erl 

-spec stroke(Panel :: tuple()) -> atom().

stroke(Panel) ->
    Box = get_box(Panel),
    ep_box:stroke(Box).

%% @doc See eg_pdf_op.erl 

-spec stroke_color(Panel :: tuple()) -> atom().

stroke_color(Panel) ->
    Box = get_box(Panel),
    ep_box:stroke_color(Box).

%% @doc See eg_pdf_op.erl 

-spec fill_color(Panel :: tuple()) -> atom().

fill_color(Panel) ->
    Box = get_box(Panel),
    ep_box:fill_color(Box).

%% @doc Return width of paragraph indent of text in panel 

-spec indent(Panel :: tuple()) -> integer().

indent(Panel) ->
    Box = get_box(Panel),
    ep_box:indent(Box).

%% @doc Return action to take when text overflows panel 

continue(Panel) ->
    Box = get_box(Panel),
    ep_box:continue(Box).


%% ***********************************************************
%% Update panal parameters 
%% ***********************************************************

%% @doc Update id

-spec update_id(Panel :: tuple(), ID :: string()) -> map().

update_id(Panel, ID) ->
    Box = get_box(Panel),
    Box1 = ep_box:update_id(Box, ID),
    replace_box(Panel, Box1).

%% @doc Update left edge of panel 

-spec update_x(Panel :: tuple(), X :: integer()) -> map().

update_x(Panel, X) ->
    Box = get_box(Panel),
    Box1 = ep_box:update_x(Box, X),
    replace_box(Panel, Box1).

%% @doc Update top edge of panel 

-spec update_y(Panel :: tuple(), Y :: integer()) -> map().

update_y(Panel, Y) ->
    Box = get_box(Panel),
    Box1 = ep_box:update_y(Box, Y),
    replace_box(Panel, Box1).

%% @doc Update panel width
    
-spec update_width(Panel :: tuple(), Width :: integer()) -> map().

update_width(Panel, Width) ->
    Box = get_box(Panel),
    Box1 = ep_box:update_width(Box, Width),
    replace_box(Panel, Box1).

%% @doc Update panel width

-spec update_height(Panel :: tuple(), Height :: integer()) -> map().

update_height(Panel, Height) ->
    Box = get_box(Panel),
    Box1 = ep_box:update_height(Box, Height),
    replace_box(Panel, Box1).

%% @doc Update gutter; e.g. reserved space to right of panel 

-spec update_gutter(Panel :: tuple(), Points :: integer()) -> map().

update_gutter(Panel, Points) ->
    Box = get_box(Panel),
    Box1 = ep_box:update_gutter(Box, Points),
    replace_box(Panel, Box1).
    
%% @doc Update leading; e.g. reserved space to below the panel 

-spec update_leading(Panel :: tuple(), Points :: integer()) -> map().

update_leading(Panel, Points) ->
    Box = get_box(Panel),
    Box1 = ep_box:update_leading(Box, Points),
    replace_box(Panel, Box1).
    
%% @doc Update leading; e.g. reserved space to below the panel 

-spec update_filled(Panel :: tuple(), Points :: integer()) -> map().

update_filled(Panel, Points) ->
    Box = get_box(Panel),
    Box1 = ep_box:update_filled(Points, Box),
    replace_box(Panel, Box1).
    
%% @doc  Update width of outline around panel; e.g. 1 to n 

-spec update_border(Panel :: tuple(), Points :: integer()) -> map().

update_border(Panel, Border) when Border >= 0, Border < 5 ->
    Box = get_box(Panel),
    Box1 = ep_box:update_border(Box, Border),
    replace_box(Panel, Box1).
  
%% @doc Update type of outline: soliid, dashed, beveled 

-spec update_border_type(Panel :: tuple(), BorderType :: atom()) -> map().

update_border_type(Panel, BorderType) ->
    Box = get_box(Panel),
    Box1 = ep_box:update_border_type(Box, BorderType),
    replace_box(Panel, Box1).
  
%% @doc Update color of outline: soliid, dashed, beveled 

-spec update_border_color(Panel :: tuple(), BorderColor :: atom()) -> map().

update_border_color(Panel, BorderColor) ->
    Box = get_box(Panel),
    Box1 = ep_box:update_border_color(Box, BorderColor),
    replace_box(Panel, Box1).
    
%% @doc Update margin: space around text when displayed in outlined panel

-spec update_text_margin(Panel :: tuple(), Points :: integer()) -> map().

update_text_margin(Panel, Points) ->
    Box = get_box(Panel),
    Box1 = ep_box:update_text_margin(Box, Points),
    replace_box(Panel, Box1).

%% @doc update background_color
%%      white, silver, gray, black, maroon, red, fuchsia, purple
%%      lime, green, olive, yellow, navy, blue, teal, aqua
   

-spec update_background_color(Panel :: tuple(), BGColor :: atom()) -> map().
 
update_background_color(Panel, BGColor) ->
    Box = get_box(Panel),
    Box1 = ep_box:update_background_color(Box, BGColor),
    replace_box(Panel, Box1).
    
%% @doc update background_color
%%      white, silver, gray, black, maroon, red, fuchsia, purple
%%      lime, green, olive, yellow, navy, blue, teal, aqua
    
-spec update_text_color(Panel :: tuple(), TextColor :: atom()) -> map().

update_text_color(Panel, TextColor) ->
    Box = get_box(Panel),
    Box1 = ep_box:update_text_color(Box, TextColor),
    replace_box(Panel, Box1).

%% @doc Update stroke type; close, stroke, close_stroke, fill, 
%% fill_even_odd, fill_stroke, fill_then_stroke, fill_stroke_even_odd,
%% close_fill_stroke, close_fill_stroke_even_odd, endpath
%% SEE eg_pdf_op:path/1
    
-spec update_stroke(Panel :: tuple(), Stroke :: atom()) -> map().

update_stroke(Panel, Stroke) ->
    Box = get_box(Panel),
    Box1 = ep_box:update_stroke(Box, Stroke),
    replace_box(Panel, Box1).
   
%% @doc Update stroke color; white, silver, gray, black, maroon, 
%% red, fuchsia, purple lime, green, olive, yellow, navy, blue, teal, aqua

-spec update_stroke_color(Panel :: tuple(), Color :: atom()) -> map().

update_stroke_color(Panel, StrokeColor) ->
    Box = get_box(Panel),
    Box1 = ep_box:update_stroke_color(Box, StrokeColor),
    replace_box(Panel, Box1).

%% @doc Update fill color; white, silver, gray, black, maroon, 
%% red, fuchsia, purple lime, green, olive, yellow, navy, blue, teal, aqua

-spec update_fill_color(Panel :: tuple(), Color :: atom()) -> map().
    
update_fill_color(Panel, FillColor) ->
    Box = get_box(Panel),
    Box1 = ep_box:update_fill_color(Box, FillColor),
    replace_box(Panel, Box1).
    
%% @doc Update width of paragraph indent of text in panel 

-spec update_indent(Box :: tuple(), Indent :: integer()) -> map().

update_indent(Panel, Indent) ->
    Box = get_box(Panel),
    Box1 = ep_box:update_indent(Box, Indent),
    replace_box(Panel, Box1).
    
%% @doc Update action to take when text overflows panel 

update_continue(Panel, Continue) ->
    Box = get_box(Panel),
    Box1 = ep_box:update_continue(Continue, Box),
    replace_box(Panel, Box1).

%% ***********************************************************
%% Panel position and dimensions 
%% ***********************************************************

%% @doc Return upper-left xy coordinates of panel 

-spec position(Panel :: tuple()) -> tuple().

position(Panel) ->
    Box = get_box(Panel),
    ep_box:position(Box).

%    Box1 = ep_box:position(Box),
%    replace_box(Panel, Box1).
    
%% @doc Return x, y, width, height 

-spec dimensions(Panel :: tuple()) -> tuple().

dimensions(Panel) ->
    Box = get_box(Panel),
    Box1 = ep_box:dimensions(Box),
    replace_box(Panel, Box1).
    
%% @doc Return print specs of panel

-spec box_spec(Panel :: tuple()) -> tuple().

box_spec(Panel) ->
    Box = get_box(Panel),
    Box1 = ep_box:box_spec(Box),
    replace_box(Panel, Box1).

%% ***********************************************************
%% Border flag 
%% ***********************************************************

%% @doc Test if background flag is set; if so draw outline
%%      around text and display background color 

-spec if_border(Panel :: tuple()) -> boolean().

if_border(Panel) ->
    Box = get_box(Panel),
    ep_box:if_border(Box).

%% @doc Set backround flag true 

-spec show_border(Panel :: tuple()) -> map().

show_border(Panel) ->
    Box = get_box(Panel),
    Box1 = ep_box:show_border(Box),
    replace_box(Panel, Box1).

%% @doc Set backround flag false 

-spec hide_border(Box :: tuple()) -> map().

hide_border(Panel) ->
    Box = get_box(Panel),
    Box1 = ep_box:hide_border(Box),
    replace_box(Panel, Box1).

%% ***********************************************************
%% Panel dimensions 
%% PDF assumes that 0, 0 XY coordinate is lower-left
%% For convenience sake, we'll assume upper left here
%% but the y will have to be inverted when we print
%% ***********************************************************

%% @doc return outer box dimensions 

-spec outer_box(Panel :: tuple()) -> tuple().

outer_box(Panel) ->
    Box = get_box(Panel),
    ep_box:outer_box(Box).

%% @doc return inner box dimensions 

-spec inner_box(Panel :: tuple()) -> tuple().

inner_box(Panel) ->
    Box = get_box(Panel),
    ep_box:inner_box(Box).

%% @doc return text box dimensions 

-spec text_box(Panel :: tuple()) -> tuple().

text_box(Panel) ->
    Box = get_box(Panel),
    ep_box:text_box(Box).

%% ***********************************************************
%%  Out-of-bounds warnings 
%% ***********************************************************

%% @doc Return  if X or Y are out-of-bounds of panel 

% out_of_bounds(Panel, X, Y) ->
%    Warnings = [],
%    XLeft  = x(Panel),
%    XRight = end_x(Panel),
%    if  X < XLeft  -> Warnings1 = [{warning, x_out_of_bounds_left}| Warnings];
%        X > XRight -> Warnings1 = [{warning, x_out_of_bounds_right}| Warnings];
%        true       -> Warnings1 = Warnings 
%    end,
%    YTop    = y(Panel),
%    Height  = height(Panel),
%    YBottom = YTop + Height,
%    if  Y < YTop    -> Warnings2 = [{warning, y_out_of_bounds_top}| Warnings1];
%        Y > YBottom -> Warnings2 = [{warning, y_out_of_bounds_bottom}| Warnings1];
%        true        -> Warnings2 = Warnings1 
%    end,
%    case length(Warnings2) == 0 of
%        true  -> ok;
%        false -> Warnings2
%    end.
        
%% ***********************************************************
%%  Panel modification functions 
%% ***********************************************************

%% @doc Clip panel top, right, bottom, or left

-spec clip(Panel :: map(), Edge :: atom(), N :: integer()) -> map().


clip(Panel, top, Points) ->
    Box   = get_box(Panel),
    Box1  = ep_box:clip(Box, top, Points),
    replace_box(Panel, Box1);

clip(Panel, right, Points) ->
    Box  = get_box(Panel),
    Box1 = ep_box:clip(Box, right, Points),
    replace_box(Panel, Box1);

 clip(Panel, bottom, Points) ->
    Box  = get_box(Panel),
    Box1 = ep_box:clip(Box, bottom, Points),
    replace_box(Panel, Box1);

clip(Panel, left, Points) ->
    Box  = get_box(Panel),
    Box1 = ep_box:clip(Box, left, Points),
    replace_box(Panel, Box1).

%% @doc Shift panel up, down, right, or left 

-spec shift(Panel :: map(), Direction :: atom(), N :: integer()) -> map().

shift(Panel, up, N) ->
    Box  = get_box(Panel),
    Box1 = ep_box:shift(Box, up, N),
    replace_box(Panel, Box1);

shift(Panel, down, N) ->
    Box = get_box(Panel),
    Box1 = ep_box:shift(Box, down, N),
    replace_box(Panel, Box1);

shift(Panel, right, N) ->
    Box = get_box(Panel),
    Box1 = ep_box:shift(Box, right, N),
    replace_box(Panel, Box1);

shift(Panel, left, N) ->
    Box = get_box(Panel),
    Box1 = ep_box:shift(Box, left, N),
    replace_box(Panel, Box1).

%% ***********************************************************
%% Print support fuctions 
%% ***********************************************************


%% @doc Return panel background parameters 

-spec background(Panel :: map()) -> tuple().

background(Panel) ->
    Box = get_box(Panel),
    ep_box:background(Box).

%% @doc return corner coordiates of box

-spec corners(Panel :: map()) -> tuple().

corners(Panel) ->
    Box = get_box(Panel),
    ep_box:corners(Box).




    
%% ***********************************************************
%% Print panels 
%% ***********************************************************

-spec print_panel(Panel :: map(), PaperStock :: atom(),
      OFile :: string()) -> ok.

print_panel(Panel, PaperStock, OFile) ->
    Box = get_box(Panel),
    Box1 = ep_box:v_flip_box(Box, PaperStock),
    print_box(Box1, PaperStock, OFile).

print_box(Box, PaperStock, OFile) ->
    Box1 = ep_box:v_flip_box(Box, PaperStock),
    PDF  = eg_pdf:new(),
    ep_show_grid:show_grid(PDF, PaperStock),
    ep_box:print_box(Box1, PaperStock, OFile).







