
%%% ********************************************************* 
%%% ep_paste.erl
%%%
%%% @copyright   2018 Lloyd R. Prentice
%%% @author      Lloyd R. Prentice
%%% @doc         
%%%    License: MIT
%%%    File:    ep_print.erl
%%%    Description: 
%%%       Paste page elements 
%%% @end 
%%% ********************************************************* 

-module (ep_paste).

-export([paste_up/2]).

-define(FORMAT, letter).

-define(TEST_DIR, "./pdf/galleys/").

%% **************************************************
%% Paste up conent elements
%% **************************************************


%% @doc Paste list of content elements to PDF page

-spec paste_up(Composition :: list(),
               PageNo :: integer()) -> atom().

paste_up(Composition, PageNo) ->
    PDF = eg_pdf:new(),
    eg_pdf:set_pagesize(PDF, ?FORMAT),
    eg_pdf:set_page(PDF, PageNo),

    paste_to_pdf(PDF, Composition),

    {Serialised, _PageNo} = eg_pdf:export(PDF),
    OFile = "galley" ++ integer_to_list(PageNo) ++ ".pdf",
    file:write_file(?TEST_DIR ++ OFile,[Serialised]),
    eg_pdf:delete(PDF),
    ok.


paste_to_pdf(PDF, PageComposition) ->
   [paste_to_pdf(PDF, CopyBlock, Map) || {CopyBlock, Map} <- PageComposition].  


paste_to_pdf(PDF, page_header, Map) ->
   ep_page_header:page_header(PDF, Map);
 
paste_to_pdf(PDF, page_no, Map) ->
   ep_page_no:page_no(PDF, Map);
 
paste_to_pdf(PDF, line, Map) ->
   ep_line:line_to_pdf(PDF, Map);

paste_to_pdf(PDF, circle, Map) ->
   ep_circle:circle(PDF, Map);

paste_to_pdf(PDF, image, Map) ->
   ep_image:image_to_pdf(PDF, Map).
 
