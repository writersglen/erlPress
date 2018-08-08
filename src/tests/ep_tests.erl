%%% ==========================================================================
%%% ep_tests.erl

%%% @author     Lloyd R. Prentice
%%% @copyright  2018 Lloyd R. Prentice
%%% @version   .01
%%% @doc
%%%   License:
%%%   File:         ep_tests.erl
%%%   Description:  Comprehensive test 
%%% @end

%%% ==========================================================================


-module (ep_tests).

-export([test_all/0]).

% -compile(export_all).


%% ***********************************************************
%% ***********************************************************
%% Tests
%%
%% Review results of tests in *.PDFs in ../pdf/galleys
%% ***********************************************************
%% ***********************************************************


test_all() ->
   ep_show_n_tell:run(),
   ep_text_tests:test_text_block().


