-module(owbot).
-import(unicode, [characters_to_list/1, characters_to_binary/1]).
-export([start/0]).
-include("token.hrl").

-define(BASE_URL, "https://api.telegram.org/bot" ++ ?TOKEN).
-define(GET_COMMAND_URL, ?BASE_URL ++ "/getUpdates?offset=").
-define(SET_COMMAND_URL, ?BASE_URL ++ "/sendMessage").
-define(SET_QUERY_URL, ?BASE_URL ++ "/answerInlineQuery").
-define(OW_URL,"https://playoverwatch.com/en-us/career/pc").

start() ->
  inets:start(),
  ssl:start(),
  io:format("BOT start!"),

  command_handler(?GET_COMMAND_URL, 0).

  % % Url = "https://playoverwatch.com/en-us/career/pc/eu/sedia-21534",
  % Url = "https://playoverwatch.com/en-us/career/pc/eu/Dreadino-2592",
  % Html = get_html(Url),
  %
  % Quick = get_quick(Html),
  % Comp = get_comp(Html),
  % Rank = get_rank(Html),
  %
  % io:format("Heroes in quick~n~p~n", [Quick]),
  % io:format("~nHeroes in comp~n~p~n", [Comp]),
  % io:format("~p~n", [Rank]).

  % io:format("█").

command_handler(Url, UpdateId) ->
  Response = parse_response(get_command(Url ++ integer_to_list(UpdateId + 1))),
  {JsonObj} = jiffy:decode(Response),
  Result = proplists:get_value(<<"result">>, JsonObj, []),
  % io:format("~p~n",[length(Result)]),
  NewUpdateId = consume_message(Result, UpdateId),
  timer:sleep(1000),
  command_handler(Url, NewUpdateId).

consume_message([], UpdateId) -> UpdateId;
consume_message([H|T], UpdateId) ->
  case H of
    {[{<<"update_id">>, NewUpdateId}, {<<"message">>, {Message}} |_]} ->
      case parse_message(Message) of
        {command, ChatID, _, Msg_str} -> run_command(ChatID, Msg_str);
        {text, _, _, _} -> ok;
         notxt -> ok
      end;
    {[{<<"update_id">>, NewUpdateId}, {<<"inline_query">>, {Message}} |_]} ->
      % io:format("~p~n", [Message]),
      parse_inline(Message);
    % [ {[{<<"update_id">>, NewUpdateId}, {<<"edited_message">>, {_Message}} |_]} |_] ->
    %   _Message;
    {[{<<"update_id">>, NewUpdateId} |_]} ->
      notxt;
    _ ->
      NewUpdateId = UpdateId,
      notxt
  end,
  consume_message(T, NewUpdateId).

parse_inline(Msg) ->
  Id = characters_to_list(proplists:get_value(<<"id">>, Msg)),
  Query = characters_to_list(proplists:get_value(<<"query">>, Msg)),
  io:format("~p ~p~n",[Id, Query]),
  Query1 = re:replace(Query, " ", "-", [{return, list}]),
  Query2 = re:replace(Query1, "#", "-", [{return, list}]),
  Url = ?OW_URL ++ "/eu/" ++ Query2,
  % io:format("~p~n",[Url]),
  Html = get_html(Url),
  Name = re:replace(Query2, "-", "#", [{return, list}]),
  Quick = get_quick(Html),
  if
    Quick /= not_found ->
      Comp = get_comp(Html),
      Rank = get_rank(Html),
      Result = "*" ++ Name ++ "*\n" ++
               "\n*Rank* `" ++ Rank ++ "`\n" ++
               "\n*Quick Heroes*\n" ++ format_heroes(Quick) ++
               "\n*Competitive Heroes*\n" ++ format_heroes(Comp) ++
               "\n[Click here]("++ Url ++") for player page",
      io:format("~p~n", [Result]),
      [{FirtComp,_}|_] = Comp,
      [{FirtQuick,_}|_] = Quick,
      Description = "Rank " ++ Rank ++ "\n" ++
                    "Most used heroes " ++ FirtQuick ++ " and " ++FirtComp,
      send_query(Id, Query, Description, Result);
    true -> skip
  end.

format_heroes([]) -> "";
format_heroes(not_found) -> "";
format_heroes([H|T]) ->
  {Name, Desc} = H,
  "`" ++ Name ++ gen_spaces(12-length(Name)) ++ Desc ++ "`\n"
  ++ format_heroes(T).

gen_spaces(0) -> "";
gen_spaces(Num) ->
  " " ++ gen_spaces(Num-1).

parse_message(notxt) -> notxt;
parse_message(Message) ->
  {Chat} = proplists:get_value(<<"chat">>, Message),
  ChatID = proplists:get_value(<<"id">>, Chat),
  Command = proplists:get_value(<<"text">>, Message),
  MsgID = proplists:get_value(<<"message_id">>, Message),
  case Command of
    undefined -> notxt;
    _ -> Msg_str = characters_to_list(Command),
      case Msg_str of
        [H|_] when H==47 -> {command, ChatID, MsgID, Msg_str};
        _  -> {text, ChatID, MsgID, Msg_str}
      end
  end.


send_message(ChatID, Text) ->
  set_command(?SET_COMMAND_URL, "chat_id=" ++ integer_to_list(ChatID) ++ "&text=" ++ Text).

send_message(ChatID, MsgID, Text) ->
  set_command(?SET_COMMAND_URL, "chat_id=" ++ integer_to_list(ChatID) ++ "&text=" ++ Text ++ "&reply_to_message_id=" ++ integer_to_list(MsgID)).

send_query(QueryId, Title, Description, Text) ->
  % Json = "[{\"type\":\"Article\", \"id\":\"sedia-21534\", \"title\":\"sedia\", \"input_message_content\":{\"message_text\": \"prova123\"}}]",
  EJson = [{[
    {type, <<"Article">>},
    {id, characters_to_binary(QueryId ++ Title)},
    {title, characters_to_binary(Title)},
    {description, characters_to_binary(Description)},
    {input_message_content, {[
      {message_text, characters_to_binary(Text)},
      {parse_mode, <<"Markdown">>},
      {disable_web_page_preview, true}
      ]}}
    ]}],
  Json = binary_to_list(jiffy:encode(EJson)),
  io:format("~p~n", [Json]),
  set_command(?SET_QUERY_URL, "inline_query_id=" ++ QueryId ++ "&results=" ++ Json).

get_command(Url) ->
  request(get, {Url, []}).

set_command(Url, Data) ->
  Response = request(post, {Url, [], "application/x-www-form-urlencoded", Data}),
  {ok, {{"HTTP/1.1",ReturnCode, State}, _, _}} = Response,
  io:format("~w / ~p~n", [ReturnCode, State]).

request(Method, Body) ->
  httpc:request(Method, Body, [{ssl,[{verify,0}]}], []).

parse_response({ok, { _, _, Body}}) -> Body.

terminate() ->
  ssl:stop(),
  inets:stop().

run_command(ChatID, "/help") ->
  send_message(ChatID, "Help text");
run_command(ChatID, Msg) ->
  io:format("receved ~p~n", [Msg]);
run_command(_, _ ) -> ok.

parse_heroes_rec(_, 0) -> [];
parse_heroes_rec([], _) -> [];
parse_heroes_rec([H|T], Num) ->
  {_,_,[_,{_,_,[_,{_,_,[NameTag,DescTag]}]}]} = H,
  % [Name, Desc] = Children,
  {_,_,[Name]} = NameTag,
  {_,_,[Desc]} = DescTag,
  % io:format("~p ~p~n", [Name,Desc]).
  [{unicode:characters_to_list(Name),unicode:characters_to_list(Desc)}] ++ parse_heroes_rec(T, Num-1).

parse_heroes(List) -> parse_heroes_rec(List,3).

get_html(Url)->
  Request = {Url, []},
  {ok, { _, _, Body}} = httpc:request(get, Request, [], []),
  mochiweb_html:parse(Body).

get_quick(Tree)->
  Xquick = "//*[@id=\"quickplay\"]/section[2]/div/div[2]",
  try
    [{_,_,QuickHeroes}] = mochiweb_xpath:execute(Xquick, Tree),
    parse_heroes(QuickHeroes)
  catch
    _:_ -> not_found
  end.

get_comp(Tree)->
  Xcomp ="//*[@id=\"competitive\"]/section[2]/div/div[2]",
  try
    [{_,_,CompHeroes}] = mochiweb_xpath:execute(Xcomp, Tree),
    parse_heroes(CompHeroes)
  catch
    _:_ -> not_found
  end.

get_rank(Tree)->
  Xrank = "//*[@id=\"overview-section\"]/div/div[2]/div/div/div[1]/div/div[2]/div",
  try
    [{_,_,[RankStr]}] = mochiweb_xpath:execute(Xrank, Tree),
    % Result = mochiweb_xpath:execute(Xrank, Tree),
    % io:format("~p~n", [Result]).
    Rank= characters_to_list(RankStr),
    Rank
  catch
      _:_ -> "no rank"
  end.
