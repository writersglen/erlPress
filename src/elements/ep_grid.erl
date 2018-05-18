%%==========================================================================
%% ep_grid.erl

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
%% File:       ep_gird.erl
%% Description: 
%%    Layout primitives 
%% @end
%%==========================================================================

-module (ep_grid).

-export ([create/2, grid/3]).

% -compile(export_all).

-include("../../include/ep.hrl").

%% ***********************************************************
%% create 
%% ***********************************************************


create(XList, YList) ->
    #{ xlist    => XList
     , ylist    => YList
     }.


%% ***********************************************************
%% grid/3  
%% ***********************************************************

grid(PDF, PageMap, GridMap) ->
    PaperStock  = maps:get(paper_stock, PageMap),
    [PageXY]    = maps:get(page_xy, PageMap),
    XList       = maps:get(xlist, GridMap),
    YList       = maps:get(ylist, GridMap),
    XList1      = [ep_lib:impose_xy(XPt, PageXY, PaperStock) ||
                      XPt <- XList],
    YList1      = [ep_lib:impose_xy(YPt, PageXY, PaperStock) ||
                      YPt <- YList],
    eg_pdf:save_state(PDF),
    eg_pdf:set_line_width(PDF, 1),
    eg_pdf:set_dash(PDF, solid),
    eg_pdf:set_stroke_color(PDF, black),
    eg_pdf:set_fill_color(PDF, black),
    eg_pdf:grid(PDF, XList1, YList1),
    eg_pdf:path(PDF, fill_stroke),
    eg_pdf:restore_state(PDF).

