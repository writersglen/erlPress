
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

-export([paste_up/4]).

-define(FORMAT, letter).

-define(TEST_DIR, "./pdf/galleys/").

%% **************************************************
%% Paste up conent elements
%% **************************************************


%% @doc Paste list of content elements to PDF page
%%      Example, see ep_tests/galley1/0

-spec paste_up(PaperStock  :: atom(),
               PageSpec    :: tuple(),     % {Format, {PageX, PageY}}
               PageNumber  :: integer(),
               Composition :: list()) -> atom().,

paste_up(PaperStock, PageSpec, PageNumber, Composition) ->
    Format = element(1, PageSpec),
    PDF = eg_pdf:new(),
    eg_pdf:set_pagesize(PDF, Format),
    eg_pdf:set_page(PDF, PageNo),

    paste_to_pdf(PDF, PaperStock, PageSpec, PaperStock, Format, Composition),

    {Serialised, _PageNo} = eg_pdf:export(PDF),
    OFile = "galley" ++ integer_to_list(PageNo) ++ ".pdf",
    file:write_file(?TEST_DIR ++ OFile,[Serialised]),
    eg_pdf:delete(PDF),
    ok.


paste_to_pdf(PDF, PaperStock, PageSpec, PageComposition) ->
   [paste_to_pdf(PDF, PaperStock, PageSpec, CopyItem, Map) || 
                     {CopyItem, Map} <- PageComposition].  


paste_to_pdf(PDF, page_header, PaperStock, PageSpec, Map) ->
   ep_page_header:page_header(PDF, Map);
 
paste_to_pdf(PDF, page_number, PaperStock, Format, Map) ->
   ep_page_no:page_no(PDF, Map);
 
paste_to_pdf(PDF, line, PaperStock, Format, Map) ->
   ep_line:line_to_pdf(PDF, Map);

paste_to_pdf(PDF, bezier, PaperStock, Format, Map) ->
   ep_bezier:bezier_to_pdf(PDF, Map);

paste_to_pdf(PDF, circle, PaperStock, Format, Map) ->
   ep_circle:circle(PDF, Map);

paste_to_pdf(PDF, ellipse, PaperStock, Format, Map) ->
   ep_ellipse:ellipse(PDF, Map);

paste_to_pdf(PDF, image, PaperStock, Format, Map) ->
   ep_image:image_to_pdf(PDF, Map).
 
