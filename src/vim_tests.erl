%% @author Danielle E. Sucher <dsucher@gmail.com>
%% @copyright 2013 Danielle E. Sucher

-module(vim_tests).

-include_lib("eunit/include/eunit.hrl").

handle_regex_test() ->
  Result = vim:handle_regex("foo", "f"),
  io:format("~s", [Result]),
  ?assert(Result =:= <<"<span class='highlight'>f</span>VRMG<br>&#92;1: <br>&#92;2: <br>&#92;3: VRMGoo\n">>).

handle_regex_redos_test() ->
  Result = vim:handle_regex("foo", "f\{999,99999}"),
  ?assert(Result =:= <<"Your pattern has been rejected. Please email me with your use case.">>).
