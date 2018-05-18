%%==========================================================================
%% ep_cropmark.erl

%% @copyright  2018 Lloyd R. Prentice
%% @author     Lloyd R. Prentice
%%
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
%% File:       ep_cropmark.erl
%% Description: 
%%    Layout primitives 
%% @end
%%==========================================================================

-module (ep_cropmark).

-export ([create/1, cropmark/3]).

% -compile(export_all).

-include("../../include/ep.hrl").

%% ***********************************************************
%% create 
%% ***********************************************************


create(Position) ->
   New = [{position,      Position}],
   maps:from_list(New).



%% ***********************************************************
%% dot/3  
%% ***********************************************************

cropmark(PDF, PageMap, CropmarkMap) ->
    PaperStock  = maps:get(paper_stock, PageMap),
    [PageXY]    = maps:get(page_xy, PageMap),
    Position    = maps:get(position, CropmarkMap),
    {X1, Y1}    = ep_lib:impose_xy(Position, PageXY, PaperStock),
    HFrom       = {(X1 - 10), Y1},  
    HTo         = {(X1 + 10), Y1},  
    VFrom       = {X1, (Y1 - 10)},  
    VTo         = {X1, (Y1 + 10)}, 
    eg_pdf:save_state(PDF),
    eg_pdf:set_line_width(PDF, 1),
    eg_pdf:set_dash(PDF, solid),
    eg_pdf:set_stroke_color(PDF, black),
    eg_pdf:set_fill_color(PDF, black),
    eg_pdf:line(PDF, HFrom, HTo),
    eg_pdf:line(PDF, VFrom, VTo),
    eg_pdf:path(PDF, fill_stroke),
    eg_pdf:restore_state(PDF).

