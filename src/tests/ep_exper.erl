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
    Job = ep_job:create("Test article", "LRP"),
    OutFile = "./pdf/galleys/" ++ ?MODULE_STRING ++ ".pdf", 

    PDF = eg_pdf:new(),

    ep_page:first_page(PDF),  % ****** Page1

    % ********** Page 1 makeup

     ep_show_grid:show_grid(PDF, letter),
     
     Panel1 = ep_panel:create({1, 1, top}, {72, 72}, {420, 200}),
     Panel2 = ep_panel:create({1, 2, continue}, {72, 272}, {200, 300}),
     Panel3 = ep_panel:create({1, 3, final}, {272, 272}, {220, 200}),
     Beads  = [Panel1, Panel2, Panel3],
     Copy   = ep_sample_text:article(),
     ArticleMap = ep_article:create(Copy, Beads),

     ep_article:article(PDF, Job, ArticleMap),

    ep_job:save_job(PDF, OutFile).


