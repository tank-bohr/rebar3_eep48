-module(rebar3_eep48).

-include_lib("edoc/src/edoc.hrl").
-include_lib("edoc/src/edoc_types.hrl").

-export([
    init/1,
    do/1,
    format_error/1
]).

-define(PROVIDER, eep48).
-define(DEPS, [app_discovery]).

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
    #{
        src_dirs     := SrcDirs,
        include_dirs := IncDirs
    } = rebar_compiler_erl:context(AppInfo),
    BaseDir = rebar_app_info:dir(AppInfo),
    AbsSrcDirs = abs_dirs(BaseDir, SrcDirs),
    AbsIncDirs = abs_dirs(BaseDir, IncDirs),
    EbinDir = rebar_app_info:ebin_dir(AppInfo),
    ok = edocc(AbsSrcDirs, AbsIncDirs, EbinDir),
    rebar_api:debug("Compile app.src for ~s", [Name]),
    {ok, _} = rebar_otp_app:compile(State, AppInfo).

edocc(SrcDirs, IncDirs, OutDir) ->
    rebar_api:debug("Source dirs are: ~p", [SrcDirs]),
    rebar_api:debug("Include dirs are: ~p", [IncDirs]),
    rebar_api:info("BEAM-files will be available in ~s dir", [OutDir]),
    SrcList = lists:flatmap(fun(SrcDir) ->
        filelib:wildcard(SrcDir ++ "/*.erl")
    end, SrcDirs),
    ok = mkdir_p(OutDir),
    lists:foreach(fun(Src) ->
        rebar_api:info("Compile ~s with eep48", [Src]),
        compile_with_docs_chunk(Src, IncDirs, OutDir)
    end, SrcList).

compile_with_docs_chunk(Src, IncDirs, OutDir) ->
    rebar_api:debug("Compile with `Docs` chunk: [~s]", [Src]),
    Docs = edocs(Src),
    DocsChunkData = term_to_binary(Docs, [compressed]),
    ExtraChunks = [{<<"Docs">>, DocsChunkData}],
    compile:file(Src, [
        debug_info,
        {extra_chunks, ExtraChunks},
        {outdir, OutDir},
        {i, IncDirs}
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
    lists:flatten(lists:filtermap(fun function_doc/1, Entries)).

function_doc(#entry{name = {_Name, _Arity}, data = Tags} = Entry) ->
    IsPrivate = lists:any(fun is_private/1, Tags),
    Types = lists:filter(fun is_type/1, Tags),
    case {IsPrivate, Types} of
        {true, []} ->
            false;
        {false, _} ->
            {true, [process_function_entry(Entry) | process_types(Types)]};
        {true, _} ->
            {true, process_types(Types)}
    end;
function_doc(_) ->
    false.

is_private(#tag{name = private}) ->
    true;
is_private(_) ->
    false.

%% ex_doc doesn't see type from comments
is_type(#tag{name = type, origin = code}) ->
    true;
is_type(_) ->
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

process_types(Types) ->
    lists:map(fun process_type_tag/1, Types).

process_type_tag(#tag{
    name = type,
    line = Line,
    data = {
        #t_typedef{
            name = #t_name{name = Name},
            args = Args
        },
        Xml
    }
}) ->
    {
        {type, Name, length(Args)},
        erl_anno:new(Line),
        [],
        #{ <<"en">> => type_doc(Xml) },
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

type_doc([]) ->
    nil;
type_doc(Xml) ->
    list_to_binary(xmerl:export_simple(Xml, edown_xmerl)).

abs_dirs(BaseDir, Dirs) ->
    lists:map(fun(Dir) -> filename:join(BaseDir, Dir) end, Dirs).

mkdir_p(Dir) ->
    ok = filelib:ensure_dir(Dir),
    case file:make_dir(Dir) of
        ok ->
            ok;
        {error, eexist} ->
            ok;
        Error ->
            Error
    end.
