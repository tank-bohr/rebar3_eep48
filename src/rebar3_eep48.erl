-module(rebar3_eep48).

-include_lib("edoc/src/edoc.hrl").

-export([
    init/1,
    do/1,
    format_error/1
]).

-define(PROVIDER, eep48).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 eep48"},    % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "EEP48 support"},
            {desc, "Compile erlang sources to EEP48-compatible beam"},
            {profiles, [eep48]}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Apps = rebar_state:project_apps(State),
    ok = lists:foreach(fun(AppInfo) ->
        do_app(State, AppInfo)
    end, Apps),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

do_app(State, AppInfo) ->
    Name = rebar_app_info:name(AppInfo),
    rebar_api:info("Running eep48 on application ~s", [Name]),
    BaseDir = rebar_app_info:dir(AppInfo),
    EbinDir = rebar_app_info:ebin_dir(AppInfo),
    ok = edocc(BaseDir, EbinDir),
    rebar_api:debug("Compile app.src for ~s", [Name]),
    {ok, _} = rebar_otp_app:compile(State, AppInfo).

edocc(Dir, OutDir) ->
    SrcDir = filename:join(Dir, src),
    IncludeDir = filename:join(Dir, include),
    SrcList = filelib:wildcard(SrcDir ++ "/*.erl"),
    ok = mkdir_p(OutDir),
    rebar_api:info("BEAM-files will be available in ~s dir", [OutDir]),
    lists:foreach(fun(Src) ->
        rebar_api:info("Compile ~s with eep48", [Src]),
        compile_with_docs_chunk(Src, SrcDir, IncludeDir, OutDir)
    end, SrcList).

compile_with_docs_chunk(Src, SrcDir, IncludeDir, OutDir) ->
    Docs = edocs(Src),
    DocsChunkData = term_to_binary(Docs, [compressed]),
    ExtraChunks = [{<<"Docs">>, DocsChunkData}],
    compile:file(Src, [
        debug_info,
        {extra_chunks, ExtraChunks},
        {outdir, OutDir},
        {i, IncludeDir},
        {i, SrcDir}
    ]).

edocs(Src) ->
    Opts = [],
    Env = edoc_lib:get_doc_env(Opts),
    {_Module, Entries} = rebar3_eep48_extract:source(Src, Env, Opts),
    docs(Entries).

docs(Entries) ->
    #entry{name = module, line = Line, data = Data} = get_entry(module, Entries),
    {
        docs_v1,
        erl_anno:new(Line),
        erlang,
        <<"text/markdown">>,
        #{ <<"en">> => doc_string(Data) },
        #{},
        function_docs(Entries)
    }.

get_entry(Name, [#entry{name = Name} = E | _Es]) -> E;
get_entry(Name, [_ | Es]) -> get_entry(Name, Es).

function_docs(Entries) ->
    lists:filtermap(fun function_doc/1, Entries).

function_doc(#entry{data = [#tag{name = private}]}) ->
    false;
function_doc(#entry{name = {_Name, _Arity}} = Entry) ->
    {true, process_function_entry(Entry)};
function_doc(_) ->
    false.

process_function_entry(#entry{name = {Name, Arity}, line = Line, args = Args, data = Data}) ->
    rebar_api:debug("Process function ~s/~p", [Name, Arity]),
    {
        {function, Name, Arity},
        erl_anno:new(Line),
        signature(Name, Args),
        #{ <<"en">> => doc_string(Data) },
        #{}
    }.

signature(Name, ArgList) ->
    Args = lists:join(", ", [atom_to_list(A) || A <- ArgList]),
    [iolist_to_binary(io_lib:format("~s(~s)", [Name, Args]))].

doc_string(Data) ->
    iolist_to_binary(
        lists:map(fun tag_to_markdown/1, Data)).

tag_to_markdown(#tag{name = doc, data = Xml}) ->
    xmerl:export_simple(Xml, edown_xmerl);
tag_to_markdown(_) ->
    "".

mkdir_p(Dir) ->
    ok = filelib:ensure_dir(Dir),
    case file:make_dir(Dir) of
        ok ->
            ok;
        {error,eexist} ->
            ok;
        Error ->
            Error
    end.
