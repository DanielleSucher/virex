%% @author Danielle E. Sucher <dsucher@gmail.com>
%% @copyright 2013 Danielle E. Sucher

-module(vim_tests).

-include_lib("eunit/include/eunit.hrl").

handle_regex_test() ->
  Result = vim:handle_regex("foo", "f"),
  ?assert(Result =:= <<"<span class='highlight'>f</span>VRMG<br>&#92;1: <br>&#92;2: <br>&#92;3: VRMGoo\n">>).

handle_regex_redos_test() ->
  Result = vim:handle_regex("foo", "f\{999,99999}"),
  ?assert(Result =:= <<"Your pattern has been rejected. Please email me with your use case.">>).

handle_regex_redos_with_space_test() ->
  Result = vim:handle_regex("foo", "f\{ 999,99999}"),
  ?assert(Result =:= <<"Your pattern has been rejected. Please email me with your use case.">>).

handle_regex_slash_test() ->
  Result = vim:handle_regex("foo", "f/"),
  ?assert(Result =:= <<"Your pattern has been rejected. Please email me with your use case.">>).

handle_regex_escaped_slash_test() ->
  Result = vim:handle_regex("f/oo", "f\\/"),
  ?assert(Result =:= <<"<span class='highlight'>f/</span>VRMG<br>&#92;1: <br>&#92;2: <br>&#92;3: VRMGoo\n">>).

handle_regex_double_escaped_slash_test() ->
  Result = vim:handle_regex("f/oo", "f\\\\/"),
  ?assert(Result =:= <<"Your pattern has been rejected. Please email me with your use case.">>).
