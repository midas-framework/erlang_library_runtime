-module(see).

-export([main/0]).

main() ->
    Mod = get_main_module(),
    erlang:display({"Starting", Mod}),
    % erlang:display(code:all_loaded()),
    % erlang:display(length(processes())),
    % length(registered())).
    R = Mod:main(),
    erlang:display({"Done", R}),
    erlang:halt().

module_name(Str) ->
    case catch list_to_atom(Str) of
        {'EXIT', _} ->
            erlang:display("Bad module name: " ++ Str),
            erlang:halt(1);

        ?MODULE ->
            erlang:display("Attempting to run boot module. Try another module"),
            erlang:halt(1);

        Mod -> Mod
    end.

get_main_module() ->
    case init:get_argument(load) of
        {ok, [[Arg]]} -> module_name(Arg);
        _ ->
            erlang:display("Missing command line argument: -load Mod"),
            erlang:halt(1)
    end.
