%%%n ==========================================================================
%%% ep_text_tests.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:
%%%   File:         ep_text1.erl
%%%   Description:  Test text display functions 
%%% @end

%%% ==========================================================================


-module (ep_text_tests).

% -export([test_text_block/0, test_justified_text/0, test_ragged_text/0]).

-compile(export_all).



%% ***********************************************************
%% ***********************************************************
%% Test functions 
%% ***********************************************************
%% ***********************************************************


%% @doc Returns {pastable lines, spill, updated panelmap

text_block() ->
   Copy                        = ep_sample_text:erlpress(),
   XML                         = ep_xml_lib:parse_xml(Copy),
   PanelMap                    = ep_panel:create({1, 1, test}, {72, 20}, {460, 700}),
   PanelMap1                   = ep_panel:reveal(PanelMap),
   io:format("==== PanelMap: ~p~n~n", [PanelMap1]),
   {Paste, _Spill, _PanelMap1} = ep_xml_lib:fit_xml(XML, PanelMap1),
   ep_paste_lib:paste_copy(Paste, [], PanelMap1).



test_times() ->
    PanelName                   = {1, 1, justify},
    Copy                        = ep_sample_text:times_14(),
    Position                    = {72, 140},
    Size                        = {450, 150},
    PanelMap                    = ep_panel:create(PanelName, Position, Size),
    {Paste, _Spill, _PanelMap1} = ep_text_block:fit_copy(Copy, PanelMap),
    ep_paste_lib:paste_copy(Paste, [], PanelMap).


test_helvetica() ->
    PanelNameE                    = {1, 1, boxed_text},
     CopyE                         = ep_sample_text:helvetica_10(),
     PositionE                     = {72, 350},
     SizeE                         = {450, 110},
     PanelMapE                     = ep_panel:create(PanelNameE, PositionE, SizeE),
     PanelMapE1                    = ep_panel:update_typestyle(justify_report_hv, PanelMapE),
     PanelMapE2                    = ep_panel:reveal(PanelMapE1),
     PanelMapE3                    = ep_panel:update_background_color(gainsboro, PanelMapE2),
     {PasteE, _Spill, _PanelMapE4} = ep_text_block:fit_copy(CopyE, PanelMapE3),
     ep_paste_lib:paste_copy(PasteE, [], PanelMapE3).

test_poetry() ->
     PanelNameF                    = {1, 3, poetry},
     CopyF                         = ep_sample_text:the_road_not_taken(),
     PositionF                     = {72, 580},
     SizeF                         = {200, 150},
     PanelMapF1                     = ep_panel:create(PanelNameF, PositionF, SizeF),
     PanelMapF2                    = ep_panel:reveal(PanelMapF1),
     PanelMapF3                    = ep_panel:update_typestyle(preformatted_report, PanelMapF2),
     {PasteF, _Spill, _PanelMapF4} = ep_text_block:fit_copy(CopyF, PanelMapF3),
     ep_paste_lib:paste_copy(PasteF, [], PanelMapF3).


test_centered() ->
     PanelNameF                    = {2, 1, centered},
     CopyF                         = ep_sample_text:the_road_not_taken(),
     PositionF                     = {72, 580},
     SizeF                         = {200, 150},
     PanelMapF1                     = ep_panel:create(PanelNameF, PositionF, SizeF),
     PanelMapF2                    = ep_panel:reveal(PanelMapF1),
     PanelMapF3                    = ep_panel:update_typestyle(preformatted_report, PanelMapF2),
     {PasteF, _Spill, _PanelMapF4} = ep_text_block:fit_copy(CopyF, PanelMapF3),
     ep_paste_lib:paste_copy(PasteF, [], PanelMapF3).


test_ragged_left() ->
     PanelNameG                      = {2, 2, ragged_left},
     CopyG                           = ep_sample_text:times_14(),
     PositionG                       = {72, 580},
     SizeG                           = {450, 110},
     PanelMapG1                      = ep_panel:create(PanelNameG, PositionG, SizeG),
     PanelMapG2                      = ep_panel:reveal(PanelMapG1),
     PanelMapG3                      = ep_panel:update_background_color(gainsboro, PanelMapG2),
     PanelMapG4                      = ep_panel:update_typestyle(ragged_left_report, PanelMapG3),
     io:format("=========== CopyG: ~p~n~n", [CopyG]),
     io:format("=========== PanelMap: ~p~n~n", [PanelMapG4]),
     {PasteG, _Spill, _PanelMapG5}   = ep_text_block:fit_copy(CopyG, PanelMapG4),
     io:format("=========== Paste: ~p~n~n", [PasteG]),
     ep_paste_lib:paste_copy(PasteG, [], PanelMapG4).



%% ***********************************************************
%% justified_text/0   
%% ***********************************************************


%% @doc Returns {pastable lines, spill, updated panelmap

justified_text() ->
   Copy                        = ep_sample_text:times_14(), 
   XML                         = ep_xml_lib:parse_xml(Copy),
   PanelMap                    = ep_panel:create({1, 1, test}, {72, 20}, {400, 150}),
   io:format(" ===== justified_text/0 - PanelMap: ~p~n~n", [PanelMap]),
   {Paste, _Spill, _PanelMap1} = ep_xml_lib:fit_xml(XML, PanelMap),
   ep_paste_lib:paste_copy(Paste, [], PanelMap, "justified").


%% ***********************************************************
%% ragged_text/0   
%% ***********************************************************


%% @doc Returns {pastable lines, spill, updated panelmap

ragged_text() ->
   Copy                        = ep_sample_text:article(),
   XML                         = ep_xml_lib:parse_xml(Copy),
   PanelMap                    = ep_panel:create({1, 1, test}, {72, 20}, {300, 400}),
   PanelMap1                   = ep_panel:update_typestyle(ragged_report, PanelMap),
   io:format("==== PanelMap: ~p~n~n", [PanelMap1]),
   {Paste, _Spill, _PanelMap2} = ep_xml_lib:fit_xml(XML, PanelMap1),
   ep_paste_lib:paste_copy(Paste, [], PanelMap1, "ragged").



%% ***********************************************************
%% test_ragged/0 helpers  
%% ***********************************************************



test_justify()->
    Job = ep_job:create("erlPress Test", "LRP"),
    NameString  = "_justify",
    OutFile     = "./pdf/galleys/" ++ 
                  ?MODULE_STRING ++ 
                  NameString ++
                  ".pdf",

     PDF         = eg_pdf:new(),

     Text1B      = ep_sample_text:times_14(),
     Position1B  = {72, 140},
     Measure1B   = 450,
     BlockMap1C  = ep_text_block:create(Text1B, Position1B, Measure1B),
     BlockMap1D  = maps:put(nlines, 6, BlockMap1C),
     BlockMap1E  = maps:put(margin, 20, BlockMap1D),
     BlockMap1H  = maps:put(background_color, white, BlockMap1E),
     BlockMap1I  = maps:put(border_color, white, BlockMap1H),
     ep_text_block:text_block(PDF, Job, BlockMap1I),


    ep_job:save_job(PDF, OutFile).


test_justify_panel()->
     Copy        = ep_sample_text:times_14(),
     XML         = ep_block:parse_xml(Copy),
     Position    = {72, 140},
     Size        = {450, 700},
     BlockMap    = ep_panel:create({1, 1, test}, Position, Size),
     BlockMap1   = maps:put(margin, 20, BlockMap),
     BlockMap2   = maps:put(border_color, white, BlockMap1),
     {Paste, _Spill, _PanelMap1} = ep_xml_lib:fit_xml(XML, BlockMap1),
     ep_paste_lib:paste_copy(Paste, [], BlockMap2, "justified").



test_ragged_panel()->
     Copy        = ep_sample_text:times_14(),
     XML         = ep_block:parse_xml(Copy),
     Position    = {72, 140},
     Size        = {450, 700},
     BlockMap    = ep_panel:create({1, 1, test}, Position, Size),
     BlockMap1   = ep_panel:update_margin(20, BlockMap),
     BlockMap2   = ep_panel:update_typestyle(ragged_report, BlockMap1),
     BlockMap3   = ep_panel:update_border_color(white, BlockMap2),
     io:format("==== BlockMap3: ~p~n~n", [BlockMap3]),
     {Paste, _Spill, _PanelMap1} = ep_xml_lib:fit_xml(XML, BlockMap3),
     ep_paste_lib:paste_copy(Paste, [], BlockMap3, "ragged").



test_preformatted_panel()->
     Copy        = ep_sample_text:the_road_not_taken(),
     XML         = ep_block:parse_xml(Copy),
     Position    = {72, 72},
     Size        = {230, 150},
     BlockMap    = ep_panel:create({1, 1, test}, Position, Size),
     BlockMap1   = ep_panel:update_margin(20, BlockMap),
     BlockMap2   = ep_panel:update_typestyle(preformatted_report, BlockMap1),
     BlockMap3   = ep_panel:update_border_color(white, BlockMap2),
     io:format("==== BlockMap3: ~p~n~n", [BlockMap3]),
     {Paste, _Spill, _PanelMap1} = ep_xml_lib:fit_xml(XML, BlockMap3),
     ep_paste_lib:paste_copy(Paste, [], BlockMap3, "preformatited").


test_centered_panel()->
     Copy        = ep_sample_text:the_road_not_taken(),
     XML         = ep_block:parse_xml(Copy),
     Position    = {72, 72},
     Size        = {230, 150},
     BlockMap    = ep_panel:create({1, 1, test}, Position, Size),
     BlockMap1   = ep_panel:update_margin(20, BlockMap),
     BlockMap2   = ep_panel:update_typestyle(centered_report, BlockMap1),
     BlockMap3   = ep_panel:update_border_color(white, BlockMap2),
     io:format("==== BlockMap3: ~p~n~n", [BlockMap3]),
     {Paste, _Spill, _PanelMap1} = ep_xml_lib:fit_xml(XML, BlockMap3),
     ep_paste_lib:paste_copy(Paste, [], BlockMap3, "centered").



test_ragged_left_panel()->
     Copy        = ep_sample_text:the_road_not_taken(),
     XML         = ep_block:parse_xml(Copy),
     Position    = {72, 72},
     Size        = {430, 550},
     BlockMap    = ep_panel:create({1, 1, test}, Position, Size),
     BlockMap1   = ep_panel:update_margin(20, BlockMap),
     BlockMap2   = ep_panel:update_typestyle(ragged_left_report, BlockMap1),
     BlockMap3   = ep_panel:update_border_color(white, BlockMap2),
     io:format("==== BlockMap3: ~p~n~n", [BlockMap3]),
     {Paste, _Spill, _PanelMap1} = ep_xml_lib:fit_xml(XML, BlockMap3),
     ep_paste_lib:paste_copy(Paste, [], BlockMap3, "ragged_left").


test_one_line() ->
   Copy                        = "<h1>Test One Line</h1>",
   XML                         = ep_xml_lib:parse_xml(Copy),
   PanelMap                    = ep_panel:create({1, 1, one_line}, {72, 20}, {460, 700}),
   {Paste, _Spill, _PanelMap1} = ep_xml_lib:fit_xml(XML, PanelMap),
   ep_paste_lib:paste_copy(Paste, [], PanelMap, "centered").

