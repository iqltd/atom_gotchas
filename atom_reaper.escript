#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./_build/default/lib/parse_trans/ebin/

-mode(compile).

%% The include directory name seems not to be configurable in rebar.config
-define(STANDARD_INCLUDE_PATH, "include").

%% The default values used by rebar
%% see https://rebar3.readme.io/docs/configuration#directories
-define(DEFAULT_PROJECT_APP_DIRS, ["apps/*", "lib/*", "."]).
-define(DEFAULT_SRC_DIRS, ["src"]).

%% The name of the `.hrl` file where we will write the atoms
-define(ALL_ATOMS_HRL, "all_atoms.hrl").
%% The heading comment used in the above `.hrl` file
-define(HRL_COMMENT, "%% This file is generated \n\n").

main([]) ->
    main(["rebar.config"]);
main([RebarConfigPath]) ->
    %% Read the relevant configurations from the rebar.config file
    {ok, RebarConfig} = file:consult(RebarConfigPath),
    ConfigSrcDirs = read_configuration(src_dirs, RebarConfig),
    ConfigAppDirs = read_configuration(project_app_dirs, RebarConfig),
    info("Configured src paths: ~p", [ConfigSrcDirs]),
    info("Configured project app paths: ~p", [ConfigAppDirs]),

    %% Identify all app dirs in the current project
    FilePaths = get_existing_paths(ConfigAppDirs),
    debug("Existing physical files: ~p", [FilePaths]),
    AppDirs = [Path || Path <- FilePaths, is_app_dir(Path, ConfigSrcDirs)],
    info("App dirs: ~p", [AppDirs]),

    %% For every app, extract all atom literals and write them in the
    %% corresponding ".hrl" file
    lists:foreach(
      fun(AppDir) ->
              info("Processing app dir: ~p", [AppDir]),
              AllAtoms = process_app(AppDir, ConfigSrcDirs),
              info("All atoms: ~p", [AllAtoms])
      end, AppDirs).

process_app(AppDir, ConfigSrcDirs) ->
    FullSrcDirPaths = [filename:join(AppDir, SrcDir) || SrcDir <- ConfigSrcDirs],
    debug("Full source dir paths: ~p", [FullSrcDirPaths]),

    SourceFiles = get_source_files(get_children(FullSrcDirPaths)),
    debug("All source files found: ~p", [SourceFiles]),

    AllAtoms = list_atom_literals(AppDir, SourceFiles),
    write_to_file(AppDir, AllAtoms),
    AllAtoms.

read_configuration(src_dirs, Config) ->
    proplists:get_value(src_dirs, Config, ?DEFAULT_SRC_DIRS);
read_configuration(project_app_dirs, Config) ->
    proplists:get_value(project_app_dirs, Config, ?DEFAULT_PROJECT_APP_DIRS).

get_existing_paths(WildcardPaths) ->
    [File ||
        Path <- WildcardPaths, % for every path in the paths lists
        File <- filelib:wildcard(Path)]. % expand wildcards to existing files on the filesystem

is_app_dir(Path, SrcPaths) ->
    case filelib:is_dir(Path) of
        true ->
            %% We assume that a directory with a source dir inside is an app dir
            {ok, Children} = file:list_dir(Path),
            [{X, Y} || X <- SrcPaths, Y <- Children, X =:= Y] /= [];
        false ->
            false
    end.

get_children(Paths) ->
    lists:flatmap(
      fun(Path) ->
              {ok, Files} = file:list_dir(Path),
              [filename:join(Path, File) || File <- Files]
      end, Paths).

get_source_files(Paths) ->
    get_source_files(Paths, []).

get_source_files([], Files) ->
    Files;
get_source_files([First|Rest], Acc) ->
    IsDir = filelib:is_dir(First),
    IsErl = filename:extension(First) == ".erl",
    NewSrcFiles = case {IsDir, IsErl} of
                      {true, _} ->
                          %% If it's a directory, go deeper
                          Children = get_children([First]),
                          get_source_files(Children, Acc);
                      {false, true} ->
                          %% If it has the extension, add it to the list
                          [First|Acc];
                      {false, false} ->
                          Acc
                  end,
    get_source_files(Rest, NewSrcFiles).

list_atom_literals(AppDir, Files) ->
    %% Give include folder file as option to the parse file
    IncludeDir = filename:join(AppDir, ?STANDARD_INCLUDE_PATH),
    CompileOpts = [{includes, [IncludeDir]}],
    Groups = lists:map(
               fun(File) ->
                       info("Parsing file ~s", [File]),
                       {ok, AST} = epp:parse_file(File, CompileOpts),
                       extract_atom_literals(AST)
               end, Files),
    lists:usort(
      lists:flatten(Groups)
     ).

extract_atom_literals(Forms) ->
    %% Parse the abstract source tree
    parse_trans:inspect(fun extract_literals/4,
                        [],
                        Forms,
                        []).

extract_literals(atom, {atom, _, Literal}, _Context, Acc) ->
    %% Add to the list the atom literal found
    {false, [Literal|Acc]};
extract_literals(application, {call, _, Function, Args}, Context, Acc) ->
    %% Warn if it's a "dangerous" function call
    File = parse_trans:context(file, Context),
    CallingFunction = parse_trans:context(function, Context),
    case {Function, Args} of
        {{remote, _, {atom,_,maps}, {atom,_,get}}, [{atom,_,_}|_]} ->
            %% `maps:get` with an atom literal as first parameter is safe
            ok;
        {{remote, _, {atom,_,maps}, {atom,_,get}}, [First|_]} ->
            warn("Found a maps:get call with the following first arg ~p", [First]);
        {{atom, Line, list_to_atom}, _} ->
            warn("Found a list_to_atom call in ~s:~w (function ~s)",
                 [File, Line, CallingFunction]);
        {{atom, Line, binary_to_atom}, _} ->
            warn("Found a binary_to_atom call in ~s:~w (function ~s)",
                 [File, Line, CallingFunction]);
        _ ->
            ok
    end,
    {true, Acc};
extract_literals(_Type, _Pattern, _, Acc) ->
    {true, Acc}.

write_to_file(AppDir, Atoms) ->
    %% Build the path of the resulting `.hrl` file
    IncludeDir = filename:join(AppDir, ?STANDARD_INCLUDE_PATH),
    IncludeFile = filename:join(IncludeDir, ?ALL_ATOMS_HRL),
    %% Build the content of the `.hrl` file
    Content = ?HRL_COMMENT ++ io_lib:format("-define(ALL_ATOMS, ~p).", [Atoms]),
    file:write_file(IncludeFile, Content).


%% Some convenience log function
-define(LOG_LEVEL, 20).

debug(String, Args) ->
    log(10, "[debug] " ++ String, Args).

info(String, Args) ->
    log(20, "[info] " ++ String, Args).

warn(String, Args) ->
    log(30, "[WARNING] " ++ String, Args).

log(Severity, String, Args) when Severity >= ?LOG_LEVEL ->
    io:format(String ++ "~n", Args);
log(_, _, _) ->
    ok.
