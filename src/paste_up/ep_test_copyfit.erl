%%% *********************************************************
%%% ep_test_copyfit.erl
%%%
%%% {c) 2018 Lloyd R. Prentice
%%% Author:     Lloyd R. Prentice
%%% License:
%%% File:       ep_copyfit.erl
%%% Description:
%%%    Page make up functions
%%% *********************************************************


-module (ep_test_copyfit).

-export([ layout/0
        , test1/0
        , test2/0  ]).


-include("../../include/ep.hrl").

-define(LEADING_FACTOR, 1.25).

%% TagMap = ep_typespec:times_roman(p, 12)
%% TypeSpec = ep_copyfit:typespec(12, justified, TagMap)
%% JumpList = ep_copyfit: jump_list(letter)
%% ep_copyfit:paste_copy("paragraph.xml", TypeSpec, JumpList)


%% Test thoroughly!!!!

layout() ->
  report.

%%  ep_copyfit:layout().

test1() ->
   FileName = "test_copyfit_test1",
   Layout = layout(),
   CopyBlocks = ep_copyfit:get_copy_blocks("test_copy.xml", Layout),
%    CopyBlocks = ep_copyfit:get_copy_blocks("test_copy.xml", report, Layout),
   CopyBlock = lists:nth(1, CopyBlocks),
   ep_copyfit:print_copy(FileName, CopyBlock, Layout).


test2() ->
   FileName = "test_copy_test2",
   Layout = layout(),
   CopyBlocks = ep_copyfit:get_copy_blocks("test_copy.xml", Layout),
%   CopyBlocks = ep_copyfit:get_copy_blocks("test_copy.xml", report, Layout),
   CopyBlock = lists:nth(2, CopyBlocks),
   ep_copyfit:print_copy(FileName, CopyBlock, Layout).
