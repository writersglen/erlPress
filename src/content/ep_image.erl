%%==========================================================================
%%% ep_image.erl
%%%
%%% @copyright    2018 Lloyd R. Prentice
%%% @author       Lloyd R. Prentice
%%% @doc
%%% Permission is hereby granted, free of charge, to any person obtaining a
%%% copy of this software and associated documentation files (the
%%% "Software"), to deal in the Software without restriction, including
%%% without limitation the rights to use, copy, modify, merge, publish,
%%% distribute, sublicense, and/or sell copies of the Software, and to permit
%%% persons to whom the Software is furnished to do so, subject to the
%%% following conditions:
%%% 
%%% The above copyright notice and this permission notice shall be included
%%% in all copies or substantial portions of the Software.
%%% 
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
%%% OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
%%% NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
%%% DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
%%% OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
%%% USE OR OTHER DEALINGS IN THE SOFTWARE.
%%%
%%% File: ep_image.erl
%%% Description 
%%%    Manage, scale, and render images 
%%% @end
%%% ==========================================================================

-module (ep_image).

%  -export([article/0, tag/1]).
%  -export([proof_report/1, page_proof_report/2, report_to_pdf/2]).
%  -export([report_lines/2, typespec_report/1]).
 
-compile(export_all).


-define(CLIENT, "LRP").
-define(CLIENT_DIR, "./clients/" ++ ?CLIENT).
-define(PROJECTS, ?CLIENT_DIR ++ "/projects").
-define(FORMAT, letter).

 
%% ***************************************************************
%% Create image map 
%% ***************************************************************


%% @doc Create image map
%%      Position is {X,Y}
%%      Size is {width, W} | {height, H} | {W,H} | {max, W, H} 
%%      The max Size version can be used to set a max limit on width, 
%%      height or both dimensions (undefined is a valid value for at 
%%      most 1 W or H value)

-spec create( FilePath :: list()
            , Position :: tuple()
            , Size     :: tuple()) -> map().

create(ImageFileName, Position, Size) ->
   #{ filepath    => image_path(ImageFileName) 
    , position    => Position 
    , size        => Size 
    }.


%% ***************************************************************
%% Get image attributes 
%% ***************************************************************


%% @doc Return file path

-spec file_path(ImageMap :: map) -> list().

file_path(ImageMap) ->
    maps:get(filepath, ImageMap).


%% @doc Return position 

-spec position(ImageMap :: map) -> tuple().

position(ImageMap) ->
    maps:get(position, ImageMap).


%% @doc Return image size 

-spec image_size(ImageMap :: map) -> tuple().

image_size(ImageMap) ->
    maps:get(size, ImageMap).


%% ***************************************************************
%% Update image attributes 
%% ***************************************************************


%% @doc Update file path 

-spec update_file_path( FilePath :: list(),
                        ImageMap :: map()) -> tuple().

update_file_path(FilePath, ImageMap) ->
    maps:put(filepath, FilePath, ImageMap).


%% @doc Update image position 

-spec update_position( Position :: tuple()
                     , ImageMap :: map()) -> tuple().

update_position(Position, ImageMap) ->
    maps:put(position, Position, ImageMap).


%% @doc Update image size 

-spec update_image_size( ImageSize :: tuple(),
                         ImageMap :: map()) -> tuple().

update_image_size(ImageSize, ImageMap) ->
    maps:put(size, ImageSize, ImageMap).

%% ***************************************************************
%% Image to pdf 
%% ***************************************************************


image_to_pdf(PDF, ImageMap) ->
    FilePath  = file_path(ImageMap),
    {X, Y}    = position(ImageMap),
    Y1        = ep_lib:v_flip(Y, ?FORMAT),
    Position1 = {X, Y1},
    Size      = image_size(ImageMap),
    eg_pdf:image(PDF, FilePath, Position1, Size).
    

image_to_pdf(PDF, ImageFileName, X, Y, Size) ->
    FilePath = image_path(ImageFileName), 
    eg_pdf:image(PDF, FilePath, {X, Y}, Size).

image_path(ImageFileName) ->
     ?PROJECTS ++ "/" ++ ImageFileName.


get_image(ImageFileName) ->
   ImageFile = image_path(ImageFileName),
   {ok, Image} = file:read_file(ImageFile),
   Image.


   

list_files() ->
   Dir = ?PROJECTS,
   {ok, Images} = file:list_dir(Dir),
   Images.






