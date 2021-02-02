-module(builder).

-export([make_scripts/0]).

% read() -> rpc(io, read).

% write(X) -> rpc(io, {write, X}).

% env(Key) -> rpc(env, {lookup, Key}).

% handle_env({lookup, Key}, Dict) ->
%     {lookup(Key, Dict), Dict}.

% start_env() ->
%     Env = case init:get_argument(environment) of
%               {ok, [L]} -> L;
%               error -> fatal({missing, '-environment ...'})
%           end,
%     lists:map(fun split_env/1, Env).

% split_env(Str) -> split_env(Str, []).

% split_env([$= | T], L) -> {reverse(L), T};
% split_env([], L) -> {reverse(L), []};
% split_env([H | T], L) -> split_env(T, [H | L]).

% get_module_name() ->
%     case init:get_argument(load) of
%         {ok, [[Arg]]} -> module_name(Arg);
%         error -> fatal({missing, '-load Mod'})
%     end.

% lookup(Key, L) ->
%     case lists:keysearch(Key, 1, L) of
%         {value, T} -> {found, element(2, T)};
%         false -> not_found
%     end.

preloaded() ->
    [zlib,
     prim_file,
     prim_zip,
     prim_inet,
     erlang,
     otp_ring0,
     init,
     erl_prim_loader].

make_scripts() ->
    % This script is run in a normal erlang runtime.
    % There is no need for this code to be done in erlang, it is in erlang because it needs a call to term_to_binary, and I was following an example in erlang.
    {ok, Cwd} = file:get_cwd(),
    StdlibPath = code:lib_dir(stdlib),

    erlang:display(os:cmd("rm -rf ebin gen")),
    erlang:display(os:cmd("mkdir gen")),

    % The runtime module is called see and is different to the buildtime module also called see.
    erlang:display(os:cmd("cp runtime.erl gen/see.erl")),
    % compile the Gleam code, I'm pretty sure at this point the name doesn't mean anything.
    erlang:display(os:cmd("gleam compile-package --src src --out "
                          "gen --name TODO")),
    erlang:display(os:cmd("mkdir ebin")),
    % The lists module is copied because it is used within the init module which starts the runtime.
    % This is a bit of a conflation of responsibilities for stdlib and runtime so this hack exists for now.
    erlang:display(os:cmd("cp " ++
                              StdlibPath ++ "/ebin/lists.beam ebin")),
    % compile all the erlang files that exist in gen, this is those compiled from Gleam and the runtime erlang file
    erlang:display(os:cmd("erlc -o ebin/ -pa ebin gen/*.erl")),

    % All files in the ebin directory are automatically loaded at boot.
    % By default this setup runs the vm in embed mode so modules are not loaded on demand at runtime.
    Files = [K
             || "ebin/" ++ F <- filelib:wildcard("ebin/*.beam"),
                K <- string:replace(F, ".beam", ""), length(K) > 0],
    erlang:display(Files),
    Mods = [list_to_atom(F) || F <- Files],
    erlang:display(Mods),
    Script = {script,
              {"see", "1.0"},
              [{preLoaded, preloaded()},
               {progress, preloaded},
               {path, [Cwd ++ "/ebin"]},
               {primLoad, Mods},
               {kernel_load_completed},
               {progress, kernel_load_completed},
               {progress, started},
               {apply, {see, main, []}}]},
    io:format("Script:~p~n", [Script]),
    file:write_file("see.boot", term_to_binary(Script)),
    file:write_file("run",
                    ["#!/bin/sh\nerl ",
                     %%" -init_debug ",
                     " -boot ",
                     Cwd,
                     "/see ",
                     "-environment `printenv` -mode embedded "
                     "-load $1\n"]),
    os:cmd("chmod a+x run"),
    init:stop(),
    true.
