%%% *********************************************************
%%% {c) 2018    Lloyd R. Prentice
%%% Author:     Lloyd R. Prentice
%%% License: 
%%% File:       ep_print_media.erl
%%% Description: 
%%%   ErlPress print media functions 
%%% *********************************************************      

-module (ep_print_media).

-export ([to_picas/1, to_points/1]).


%% ****************************************************************
%% Convert inches to printer-friendly units 
%% **************************************************************** 


%% @doc Convert {Width, Height} in inches to picas

-spec to_picas(Dimensions :: tuple()) -> tuple().

to_picas(Dimensions) ->
   {Width, Height} = Dimensions,
   Width1  = round(Width * 6),
   Height1 = round(Height * 6),
   {Width1, Height1}.

%% @doc Convert page type dimensions in inches to points

-spec to_points(Dimensions :: tuple()) -> tuple().

to_points(Dimensions) ->
   {Width, Height} = Dimensions,
   Width1  = round(Width * 72),
   Height1 = round(Height * 72),
   {Width1, Height1}.



