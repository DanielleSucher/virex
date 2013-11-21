%% @author Danielle E. Sucher <dsucher@gmail.com>
%% @copyright 2013 Danielle E. Sucher

-module(vim_tests).

-include_lib("eunit/include/eunit.hrl").

handle_regex_test() ->
  Result = vim:handle_regex("foo", "f"),
  ?assert(Result =:= {"<span class='highlight'>f</span>oo\n", ""}).

handle_regex_with_quotes_test() ->
  Result = vim:handle_regex("f\"oo", "f\""),
  ?assert(Result =:= {"<span class='highlight'>f\"</span>oo\n", ""}).

handle_regex_with_parens_test() ->
  Result = vim:handle_regex("foo", "\\(f\\)"),
  ?assert(Result =:= {"<span class='highlight'>f</span>oo\n", "<br>&#92;1: f<br>&#92;2: <br>&#92;3: "}).

handle_regex_redos_test() ->
  Result = vim:handle_regex("foo", "f\{999,99999}"),
  ?assert(Result =:= {"Your pattern has been rejected. Please email me with your use case.", ""}).

handle_regex_redos_with_backslash_test() ->
  Result = vim:handle_regex("foo", "f\{999,99999\}"),
  ?assert(Result =:= {"Your pattern has been rejected. Please email me with your use case.", ""}).

handle_regex_redos_with_space_test() ->
  Result = vim:handle_regex("foo", "f\{ 999,99999}"),
  ?assert(Result =:= {"Your pattern has been rejected. Please email me with your use case.", ""}).

handle_regex_slash_test() ->
  Result = vim:handle_regex("foo", "f/"),
  ?assert(Result =:= {"Your pattern has been rejected. Please email me with your use case.", ""}).

handle_regex_slash_start_of_line_test() ->
  Result = vim:handle_regex("foo", "/f"),
  ?assert(Result =:= {"Your pattern has been rejected. Please email me with your use case.", ""}).

handle_regex_odd_slash_test() ->
  ResultOne = vim:handle_regex("f/oo", "f\\/"),
  ResultThree = vim:handle_regex("f/oo", "f\\\\\\/"),
  ?assert(ResultOne =/= {"Your pattern has been rejected. Please email me with your use case.", ""}),
  ?assert(ResultThree =/= {"Your pattern has been rejected. Please email me with your use case.", ""}).

handle_regex_even_escaped_slash_test() ->
  ResultTwo = vim:handle_regex("f/oo", "f\\\\/"),
  ResultFour = vim:handle_regex("f/oo", "f\\\\\\\\/"),
  ?assert(ResultTwo =:= {"Your pattern has been rejected. Please email me with your use case.", ""}),
  ?assert(ResultFour =:= {"Your pattern has been rejected. Please email me with your use case.", ""}).
