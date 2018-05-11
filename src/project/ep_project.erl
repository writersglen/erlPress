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

-export([create_project/2]).
-export([project_id/1, client/1, client_dir/1]).
-export([project_name/1, author/1, start_date/1]).
-export([deadline/1, description/1, paper_stock/1]).
-export([format/1, pages/1]).
-export([page_numbers/1, page/2, append_page/1]).
-export([info_tagmap/1, info/1]).

-export([put_client/2, put_client_dir/2, put_project_name/2]).
-export([put_author/2, put_deadline/2, put_description/2]).
-export([put_paper_stock/2, put_format/2, put_pages/2]).

-define(DEFAULT_PAPER_STOCK, letter).
%% -define(DEFAULT_PAPER_STOCK, legal).
%% -define(DEFAULT_PAPER_STOCK, a4).

-define(CLIENT, "LRP").
-define(CLIENT_DIR, "Clients/?CLIENT").


%%% *********************************************************      
%%% Create project 
%%% *********************************************************      

create_project(ProjectName, Format) ->
   #{ project_id    => ep_lib:page_id(4)
    , client        => ?CLIENT
    , client_dir    => ?CLIENT_DIR
    , project_name  => ProjectName
    , author        => undefined
    , start_date    => calendar:universal_time()
    , deadline      => undefined
    , description   => undefined
    , paper_stock   => ?DEFAULT_PAPER_STOCK 
    , format        => Format
    , pages         => gb_trees:empty()
   }.


%%% *********************************************************      
%%% Get project attributes
%%% *********************************************************      

project_id(Project) ->
   maps:get(project_id, Project).

client(Project) ->
   maps:get(client, Project).

client_dir(Project) ->
   maps:get(client_dir, Project).

project_name(Project) ->
   maps:get(project_name, Project).

author(Project) ->
   maps:get(author, Project).

start_date(Project) ->
   maps:get(start_date, Project).

deadline(Project) ->
   maps:get(deadline, Project).

description(Project) ->
   maps:get(description, Project).

paper_stock(Project) ->
   maps:get(paper_stock, Project).

format(Project) ->
   maps:get(format, Project).

pages(Project) ->
   maps:get(pages, Project).

page_numbers(Project) ->
    Pages = pages(Project),
    gb_trees:keys(Pages).

page(PageNo, Project) ->
    Pages = pages(Project),
    gb_trees:lookup(PageNo, Pages).

append_page(Project) ->
   Pages = pages(Project),
   PageNo = gb_trees:size(Pages) + 1,
   Page = ep_page:create_page(PageNo, Project),
   Pages1 = gb_trees:insert(PageNo, Page, Pages),
   maps:put(pages, Pages1, Project).

info_tagmap(Points) ->
   {[p],
    [{default, eg_richText:mk_face("Helvetica", Points, true, default, 0)},
     {hb,      eg_richText:mk_face("Helvetica-Bold", Points, true, default, 0)},
     {em,      eg_richText:mk_face("Helvetica-Oblique", Points, true, default, 0)}
   ]}.

info(Project) ->
   ProjectID    = project_id(Project),
   Client       = client(Project),
   ProjectName  = project_name(Project),
   Author       = author(Project),
   Deadline     = deadline(Project),
   Description  = description(Project),
   PaperStock   = paper_stock(Project),
   [ProjectID, Client, ProjectName, Author, Deadline, Description, PaperStock].


%%% *********************************************************      
%%% Put project attributes
%%% *********************************************************      


put_client(Client, Project) ->
    maps:put(client, Client, Project).

put_client_dir(ClientDir, Project) ->
    maps:put(client_dir, ClientDir, Project).

put_project_name(ProjectName, Project) ->
    maps:put(project_name, ProjectName, Project).

put_author(Author, Project) ->
    maps:put(author, Author, Project).

put_deadline(Deadline, Project) ->
    maps:put(deadline, Deadline, Project).

put_description(Description, Project) ->
    maps:put(description, Description, Project).

put_paper_stock(PaperStock, Project) ->
    maps:put(paper_stock, PaperStock, Project).

put_format(Format, Project) ->
    maps:put(format, Format, Project).

put_pages(Pages, Project) ->
    maps:put(pages, Pages, Project).




