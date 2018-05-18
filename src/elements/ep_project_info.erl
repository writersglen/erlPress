%%==========================================================================
%% ep_project_info.erl

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
%% File:       ep_project_info.erl
%% Description: 
%%    Layout primitives 
%% @end
%%==========================================================================

%% NOTE: This module is a work-in-progress



-module (ep_project_info).

-export ([info_tagmap/1, info/1]). 

% -compile(export_all).

-include("../../include/ep.hrl").

info_tagmap(Points) ->
   {[p],
    [{default, eg_richText:mk_face("Helvetica", Points, true, default, 0)},
     {hb,      eg_richText:mk_face("Helvetica-Bold", Points, true, default, 0)},
     {em,      eg_richText:mk_face("Helvetica-Oblique", Points, true, default, 0)}
   ]}.

info(ProjectMap) ->
   ProjectName  = maps:get(project_name, ProjectMap),
   Client       = maps:get(client, ProjectMap),
   Author       = maps:get(author, ProjectMap),
   Deadline     = maps:get(deadline, ProjectMap),
   Description  = maps:get(description, ProjectMap),
   PaperStock   = maps:get(paper_stock, ProjectMap),
   PageFormat   = maps:get(page_format, ProjectMap),
   {ProjectName, Client, Author, Deadline, 
    Description, PaperStock, PageFormat
   }.





