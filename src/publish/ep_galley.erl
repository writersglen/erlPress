
%%% ********************************************************* 
%%% ep_galley.erl
%%%
%%% @copyright   2018 Lloyd R. Prentice
%%% @author      Lloyd R. Prentice
%%% @doc         
%%%    License: MIT
%%%    File:    ep_tests.erl
%%%    Description: 
%%%       Create and publish galleys 
%%% @end 
%%% ********************************************************* 

-module (ep_galley).

-export([create/3, publish/1]).

% -compile([export_all]).

-define(DEFAULT_DIR, "./pdf/galleys/").


%% **********************************************************
%% Create galley
%% **********************************************************


create(ProjectName, Author, PageNumber) -> 
   Project    = ep_project:create(ProjectName),
   ProjectMap = maps:put(author, Author, Project),

   PageNumber1 = integer_to_list(PageNumber),
   OFile = ?DEFAULT_DIR ++ "/galley" ++ PageNumber1 ++ ".pdf",
   ep_page:create(ProjectMap, PageNumber, OFile).


%% **********************************************************
%% Publish galley
%% **********************************************************

%% @doc Print list galleys 

-spec publish(Galleys :: list()) -> atom().

publish(Galleys) ->
    PDF = eg_pdf:new(),

    [paste_galley(PDF, PageMap, PageMakeup) ||
         {PageMap, PageMakeup} <- Galleys],

    eg_pdf:delete(PDF),
    ok.


%% **************************************************
%% Print galley 
%% **************************************************


paste_galley(PDF, PageMap, PageMakeup) ->
    PageFormat = maps:get(page_format, PageMap),
    OFile      = maps:get(output_file, PageMap),
    eg_pdf:set_pagesize(PDF, PageFormat),
    paste_to_pdf(PDF, PageMap, PageMakeup),
    {Serialised, _PageNumber} = eg_pdf:export(PDF),
    file:write_file(OFile,[Serialised]).



%% **************************************************
%% Paste elements to pdf 
%% **************************************************


paste_to_pdf(PDF, PageMap, PageMakeup) ->
   [paste_to_pdf(PDF, Element, PageMap, Map) ||
                     {Element, Map} <- PageMakeup].


paste_to_pdf(PDF, page_header, PageMap, Map) ->
   ep_page_header:page_header(PDF, PageMap, Map);

paste_to_pdf(PDF, page_number, PageMap, Map) ->
   ep_page_number:page_number(PDF, PageMap, Map);

paste_to_pdf(PDF, dot, PageMap, Map) ->
   ep_dot:dot(PDF, PageMap, Map);

paste_to_pdf(PDF, cropmark, PageMap, Map) ->
   ep_cropmark:cropmark(PDF, PageMap, Map);

paste_to_pdf(PDF, line, PageMap, Map) ->
   ep_line:line(PDF, PageMap, Map);

paste_to_pdf(PDF, lines, PageMap, Map) ->
   ep_lines:lines(PDF, PageMap, Map);

paste_to_pdf(PDF, bezier, PageMap, Map) ->
   ep_bezier:bezier(PDF, PageMap, Map);

paste_to_pdf(PDF, circle, PageMap, Map) ->
   ep_circle:circle(PDF, PageMap, Map);

paste_to_pdf(PDF, ellipse, PageMap, Map) ->
   ep_ellipse:ellipse(PDF, PageMap, Map);

paste_to_pdf(PDF, image, PageMap, Map) ->
   ep_image:image(PDF, PageMap, Map).


