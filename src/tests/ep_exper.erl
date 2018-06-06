%%% ==========================================================================
%%% ep_exper.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:
%%%   File:         ep_exper.erl
%%%   Description:  Test page construction 
%%% @end

%%% ==========================================================================


-module(ep_exper).

-export([run/0]).


run()->
    Job = ep_job:create("erlPress: Highlights", "LRP"),
    OutFile = "./pdf/galleys/" ++ ?MODULE_STRING ++ ".pdf", 

    PDF = eg_pdf:new(),
    ep_page:first_page(PDF),  % ****** Page1



    % ********** Page 1 makeup

     Text1B      = "<h1>This is a headline</h1>",
     Position1B  = {72, 140},
     Measure1B   = 450,
     BlockMap1C  = ep_text_block:create(Text1B, Position1B, Measure1B),
     BlockMap1D  = maps:put(nlines, 6, BlockMap1C),
     BlockMap1E  = maps:put(margin, 0, BlockMap1D),
     BlockMap1H  = maps:put(background_color, white, BlockMap1E),
     BlockMap1I  = maps:put(border_color, white, BlockMap1H),
     ep_text_block:text_block(PDF, Job, BlockMap1I),


ep_job:save_job(PDF, OutFile).

