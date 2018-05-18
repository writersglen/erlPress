%%% *********************************************************
%%% ep_project.erl
%%%
%%% @copyright   2018 Lloyd R. Prentice
%%% @author      Lloyd R. Prentice
%%% @doc
%%% License: 
%%%
%%% File:       ep_project.erl
%%% Description: 
%%%   Create and specify project 
%%% @end
%%% *********************************************************      

-module (ep_project).

-export([create/1]).
-export([project_name/1, client/1, client_dir/1]).
-export([author/1, start_date/1]).
-export([deadline/1, description/1, paper_stock/1]).
-export([page_format/1, pages/1]).

-export([page_numbers/1, page/2, append_page/1]).

-export([put_project_name/2, put_client/2, put_client_dir/2]).
-export([put_author/2, put_deadline/2, put_description/2]).
-export([put_paper_stock/2, put_page_format/2, put_pages/2]).

-define(DEFAULT_PAPER_STOCK, letter).
%% -define(DEFAULT_PAPER_STOCK, legal).
%% -define(DEFAULT_PAPER_STOCK, a4).

-define(DEFAULT_PAGE_FORMAT, letter).

-define(CLIENT, "LRP").
-define(DEFAULT_CLIENT_DIR, "Clients/?CLIENT").


%%% *********************************************************      
%%% Create project 
%%% *********************************************************      


create(ProjectName) ->
   #{ project_name  => ProjectName 
    , client        => ?CLIENT 
    , client_dir    => ?DEFAULT_CLIENT_DIR 
    , author        => undefined
    , start_date    => calendar:universal_time()
    , deadline      => undefined
    , description   => undefined
    , paper_stock   => ?DEFAULT_PAPER_STOCK 
    , page_format   => ?DEFAULT_PAGE_FORMAT 
    , pages         => gb_trees:empty()
   }.


%%% *********************************************************      
%%% Get project attributes
%%% *********************************************************      

%% @doc return project name

-spec project_name(ProjectMap :: map()) -> string().

project_name(ProjectMap) ->
   maps:get(project_name, ProjectMap).


%% @doc return client 

-spec client(ProjectMap :: map()) -> string().

client(ProjectMap) ->
   maps:get(client, ProjectMap).


%% @doc return client directory

-spec client_dir(ProjectMap :: map()) -> string().

client_dir(ProjectMap) ->
   maps:get(client_dir, ProjectMap).


%% @doc return author 

-spec author(ProjectMap :: map()) -> string().

author(ProjectMap) ->
   maps:get(author, ProjectMap).


%% @doc return start date 

-spec start_date(ProjectMap :: map()) -> tuple().

start_date(ProjectMap) ->
   maps:get(start_date, ProjectMap).


%% @doc return deadline 

-spec deadline(ProjectMap :: map()) -> tuple().

deadline(ProjectMap) ->
   maps:get(deadline, ProjectMap).


%% @doc return description 

-spec description(ProjectMap :: map()) -> string().

description(ProjectMap) ->
   maps:get(description, ProjectMap).


%% @doc return paper stock 

-spec paper_stock(ProjectMap :: map()) -> atom().

paper_stock(ProjectMap) ->
   maps:get(paper_stock, ProjectMap).


%% @doc return page format 

-spec page_format(ProjectMap :: map()) -> atom().

page_format(ProjectMap) ->
   maps:get(format, ProjectMap).


%% @doc return pages

-spec pages(ProjectMap :: map()) -> tuple().   

pages(ProjectMap) ->
   maps:get(pages, ProjectMap).


%% @doc return list of page numbers 

-spec page_numbers(ProjectMap :: map()) -> list().   

page_numbers(ProjectMap) ->
    Pages = pages(ProjectMap),
    gb_trees:keys(Pages).


%% @doc return page  

-spec page(PageNumber :: integer(),
           ProjectMap :: map()) -> map().   

page(PageNumber, ProjectMap) ->
    Pages = pages(ProjectMap),
    gb_trees:lookup(PageNumber, Pages).



%% @doc Append page to   

-spec append_page(ProjectMap :: map()) -> map().   

append_page(ProjectMap) ->
   Pages      = maps:get(pages, ProjectMap),
   PageNumber = gb_trees:size(Pages) + 1,
   PageSpec   = ep_page:create_page(ProjectMap, PageNumber),
   Pages1     = gb_trees:insert(PageNumber, PageSpec, Pages),
   maps:put(pages, Pages1, ProjectMap).


%%% *********************************************************      
%%% Update project attributes
%%% *********************************************************      

put_project_name(ProjectName, ProjectMap) ->
    maps:put(project_name, ProjectName, ProjectMap).

put_client(Client, ProjectMap) ->
    maps:put(client, Client, ProjectMap).

put_client_dir(ClientDir, ProjectMap) ->
    maps:put(client_dir, ClientDir, ProjectMap).

put_author(Author, ProjectMap) ->
    maps:put(author, Author, ProjectMap).

put_deadline(Deadline, ProjectMap) ->
    maps:put(deadline, Deadline, ProjectMap).

put_description(Description, ProjectMap) ->
    maps:put(description, Description, ProjectMap).

put_paper_stock(PaperStock, ProjectMap) ->
    maps:put(paper_stock, PaperStock, ProjectMap).

put_page_format(PageFormat, ProjectMap) ->
    maps:put(page_format, PageFormat, ProjectMap).

put_pages(Pages, ProjectMap) ->
    maps:put(pages, Pages, ProjectMap).




