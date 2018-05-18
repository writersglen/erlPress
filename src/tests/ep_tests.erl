%%% ********************************************************* 
%%% ep_tests.erl
%%%
%%% @copyright   2018 Lloyd R. Prentice
%%% @author      Lloyd R. Prentice
%%% @doc         
%%%    License: MIT
%%%    File:    ep_tests.erl
%%%    Description: 
%%%       Test galleys 
%%% @end 
%%% ********************************************************* 


-module (ep_tests).

-export([run1/0]).


run1() ->
   Document = [ ep_galley1:page1()
              , ep_galley2:page2()
              ],
   ep_galley:publish(Document).



