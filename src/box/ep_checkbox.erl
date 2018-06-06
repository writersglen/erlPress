%%==========================================================================
%% ep_checkbox.erl
%%
%% @copyright  2018 Lloyd R. Prentice
%% @author     Lloyd R. Prentice
%% @doc
%% License: 
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the
%% "Software"), to deal in the Software without restriction, including
%% without limitation the rights to use, copy, modify, merge, publish,
%% distribute, sublicense, and/or sell copies of the Software, and to permit
%% persons to whom the Software is furnished to do so, subject to the
%% following conditions:
%% 
%% The above copyright notice and this permission notice shall be included
%% in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
%% OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
%% NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
%% DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
%% OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
%% USE OR OTHER DEALINGS IN THE SOFTWARE.
%% File:       ep_grid.erl
%%
%% File:       ep_checkbox.erl
%% Description: 
%%    Layout primitives
%% @end 
%%==========================================================================


-module (ep_checkbox).

-export ([create/1, checkbox/3, checked_box/3]).

% -compile(export_all).

-include("../../include/ep.hrl").


%% ***********************************************************
%% create/3 
%% ***********************************************************

%% @doc Create checked box map

-spec create(BoxPosition :: tuple()) -> map().

create(BoxPosition) ->
   #{ box_position  => BoxPosition 
    , width         => 12
    , height        => 12
    , outline       => 1
    , outline_type  => solid
    , outline_color => black
    , fill_color    => white
    }.


%% ***********************************************************
%% checkbox/3  
%% **********************************************************

%% @doc Create checkbox

-spec checkbox(PDF         :: identifier(),
               Job         :: map(),
               CheckboxMap :: map()) -> map().

checkbox(PDF, Job, CheckboxMap) ->
   {PaperStock, PagePosition} = ep_job:stock_position(Job),
   BoxPosition  = maps:get(box_position, CheckboxMap), 
   BoxPosition1 = ep_lib:impose_xy(BoxPosition, PagePosition, PaperStock),
   io:format("BoxPositions1: ~p~n", [BoxPosition1]),
   Width        = maps:get(width, CheckboxMap),
   Height       = maps:get(height, CheckboxMap),
   Outline      = maps:get(outline, CheckboxMap),
   OutlineType  = maps:get(outline_type, CheckboxMap),
   OutlineColor = maps:get(outline_color, CheckboxMap),
   FillColor    = maps:get(fill_color, CheckboxMap),
   eg_pdf:save_state(PDF),
   eg_pdf:set_line_width(PDF, Outline),
   eg_pdf:set_dash(PDF, OutlineType),
   eg_pdf:set_stroke_color(PDF, OutlineColor),
   eg_pdf:set_fill_color(PDF, FillColor), 
   eg_pdf:rectangle(PDF, BoxPosition1, {Width, Height}),
   eg_pdf:path(PDF, fill_stroke),
   eg_pdf:restore_state(PDF),
   ok.


%% ***********************************************************
%% checked_box/3  
%% **********************************************************

%% @doc Create checked box

-spec checked_box(PDF         :: identifier(),
                  Job         :: map(),
                  CheckboxMap :: map()) -> map().


checked_box(PDF, Job, CheckboxMap) ->
   checkbox(PDF, Job, CheckboxMap),
   {PaperStock, PagePosition} = ep_job:stock_position(Job),
   BoxPosition  = maps:get(box_position, CheckboxMap), 
   {X, Y} = ep_lib:impose_xy(BoxPosition, PagePosition, PaperStock),
   eg_pdf:begin_text(PDF),
   eg_pdf:set_text_pos(PDF, X + 2, Y + 2),
   eg_pdf:set_font(PDF, "Helvetica-Bold", 12),
   eg_pdf:set_fill_color(PDF, black),
   eg_pdf:text(PDF, "X"),
   eg_pdf:end_text(PDF),
   ok.


