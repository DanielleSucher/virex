%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the virex application.

-module(virex_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for virex.
start(_Type, _StartArgs) ->
    virex_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for virex.
stop(_State) ->
    ok.
