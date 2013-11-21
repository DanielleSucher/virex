%% @author Danielle E. Sucher <dsucher@gmail.com>
%% @copyright 2013 Danielle E. Sucher

-module(vim).
-export([handle_regex/2]).

-define(SUBSTITUTION, "/VIREXSTARTH\\0VIREXSTOPH").

-define(GROUP_SUBSTITUTION, "VRMGVIREXBR1: \\1VIREXBR2: \\2VIREXBR3: \\3VRMG").

-define(HTMLFORMAT, " -c \"%s/<\\|>\\|&\\|VIREXSTARTH\\|" ++
  "VIREXSTOPH\\|VIREXBR/\\={'&' : '&amp;', '<' : '&lt;', '>' : '&gt;', " ++
  "'VIREXSTARTH' : '<span class=''highlight''>', 'VIREXSTOPH' : \\\"<\\/" ++
  "span>\\\", 'VIREXBR' : '<br>&#92;'}[submatch(0)]/g \"").


handle_regex(TestString, Pattern) ->
  case secure_pattern(Pattern) of
    rejected ->
      {"Your pattern has been rejected. Please email me with your use case.", ""};
    SafePattern ->
      Filename = create_test_file(TestString),
      VimCommand = substitution_command(Filename, SafePattern),
      Port = open_port({spawn, VimCommand}, [exit_status, stderr_to_stdout]),
      loop_until_vim_is_done(Filename, Port)
  end.


loop_until_vim_is_done(Filename, Port) ->
  receive
    {Port, {data, _}} ->
      loop_until_vim_is_done(Filename, Port);
    {Port, {exit_status, 0}} ->
      {ok, VimResult} = file:read_file(Filename),
      file:delete(Filename),
      format(VimResult);
    _ ->
      error
  end.


create_test_file(TestString) ->
  Filename = mktemp(),
  file:write_file(Filename, TestString),
  Filename.


substitution_command(Filename, Pattern) ->
  Replacement = replacement_command(Pattern),
  Substitute = " -c \"%s/" ++ Pattern ++ Replacement ++ "/g\"" ++ ?HTMLFORMAT,
  "vim -X " ++ Filename ++ Substitute ++ " -c \"x\"".


replacement_command(Pattern) ->
  case re:run(Pattern, "\\(.*\\)") of
    {match, _} -> ?SUBSTITUTION ++ ?GROUP_SUBSTITUTION;
    nomatch -> ?SUBSTITUTION
  end.


secure_pattern(Pattern) ->
  case safe(Pattern) of
    true ->
      escape_pattern_quotes(Pattern);
    false ->
      rejected
  end.


safe(Pattern) when erlang:length(Pattern) > 80 ->
  false;
safe(Pattern) ->
  DangerousRegex = "\{-?[0-9]{3,}|[0-9]{3,}\\\\?\}|([^\\\\]|^)(\\\\\\\\)*/",
  re:run(Pattern, DangerousRegex) =:= nomatch.


escape_pattern_quotes(Pattern) ->
  re:replace(Pattern, "\"", "\\\\\\\"", [global, {return, list}]).


format(VimResult) ->
  Tokens = string:tokens(binary_to_list(VimResult), "VRMG"),
  case Tokens of
    [_|[]] ->
      {string:join(Tokens, ""), ""};
    _ ->
      split_highlighted_and_groups(Tokens)
  end.


split_highlighted_and_groups(Tokens) ->
  IsMatchGroup = fun([A,B|_]) -> [A] =:= "<" andalso B =:= $b; (_) -> false end,
  {Groups, Match} = lists:partition(IsMatchGroup, Tokens),
  {string:join(Match, ""), string:join(Groups, "<br>")}.


mktemp() ->
  string:strip(os:cmd("mktemp -t virextmp.XXXXXXX"), both, $\n).
