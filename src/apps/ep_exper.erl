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

    Text1         = "Article",
    Font1         = "Helvetica",
    TextPosition1 = {72, 72},
    FontSize1     = 36,
    TextMap1      = ep_text:create(Font1, TextPosition1, FontSize1),
    ep_text:one_line(PDF, Text1, Job, TextMap1),
     

    Panel1 = ep_panel:create({1, 1, top}, {72, 150}, {468, 400}),
    Beads  = [Panel1],
    Copy   = ep_sample_text:article(),
    ArticleMap = ep_article:create(Copy, Beads),

    ep_article:article(PDF, Job, ArticleMap),


    ep_page:next_page(PDF),    % ****** Page 2


    % ********** Page 1 makeup

    Text2         = "Article and Beads",
    Font2         = "Helvetica",
    TextPosition2 = {72, 72},
    FontSize2     = 36,
    TextMap2      = ep_text:create(Font2, TextPosition2, FontSize2),
    ep_text:one_line(PDF, Text2, Job, TextMap2),

     Panel2 = ep_panel:create({1, 1, top}, {72, 150}, {400, 200}),
     Panel3 = ep_panel:create({1, 2, continue}, {72, 350}, {195, 200}),
     Panel4 = ep_panel:create({1, 3, final}, {277, 350}, {195, 200}),
     Beads1  = [Panel2, Panel3, Panel4],
     Copy   = ep_sample_text:article(),
     ArticleMap1 = ep_article:create(Copy, Beads1),

     ep_article:article(PDF, Job, ArticleMap1),


    ep_job:save_job(PDF, OutFile).
