-module(wa).
-export([start/0]).
% -define(HTTP_TIMEOUT, 100000).

start() ->
  ok = start_lager(),

  ok = application:start(sasl),
  ok = application:start(crypto),
  ok = application:start(asn1),
  ok = application:start(public_key),
  ok = application:start(ssl),
  ok = application:start(ranch),
  ok = application:start(cowlib),
  ok = application:start(cowboy),
  ok = application:start(emongo),
  ok = application:start(lhttpc),

  ok = emongo:add_pool(aucs_db, "ds051740.mongolab.com", 51740, "auc2", 1, [{user, "kovalev"}, {password, "endless1987"}]),
  ok = application:start(wa),
  ok.

  % {ok, Phase2UTC, Phase2Url} = get_data_url(),
  % io:format("AT ~p: ~p~n", [Phase2UTC, Phase2Url]),
  % case is_fetched(Phase2UTC) of
  %   true -> 
  %     lager:info("DB OK ~p", [Phase2UTC]),
  %     ok;
  %   false -> 
  %     lager:info("DB FETCHING ~p", [Phase2UTC]),
  %     Aucs = get_items(Phase2UTC, Phase2Url),
  %     lager:info("DB SAVING ~p", [Phase2UTC]),
  %     write_items(C, Aucs),
  %     emongo:insert(aucs_db, "dumps", [{utc, Phase2UTC}]),
  %     lager:info("DB OK ~p", [Phase2UTC]),
  %     ok
  % end,

  % % fetch().
  % ok.

% is_fetched(UTC) -> 
%   case emongo:find_one(aucs_db, "dumps", [{"utc", UTC}]) of
%     [] -> false;
%     _ -> true
%   end.

start_lager() ->
  application:load(lager),
  application:set_env(lager,crash_log,undefined),

  LogDir = case os:getenv("LOGDIR") of
    false -> "log";
    OsLogDir -> OsLogDir
  end,

  ConsoleFormat = [time, " ", pid, {pid, [" "], ""},
    {module, [module, ":", line, " "], ""},
    message, "\n"
  ],
  application:set_env(lager,handlers,[
    {lager_console_backend,[
      debug,
      {lager_default_formatter, ConsoleFormat}]}
  ]),

  application:set_env(lager,crash_log,LogDir ++ "/crash.log"),
  application:set_env(lager,crash_log_msg_size,16384),
  application:set_env(lager,crash_log_size,1048576),
  application:set_env(lager,crash_log_date,"$D04"),
  application:set_env(lager,crash_log_count,5),
  application:set_env(lager,error_logger_hwm,300),

  lager:start().

% fetch() ->
%   % ds063889.mongolab.com:63889/auc1 -u <dbuser> -p <dbpassword>
%   Host = "ds051740.mongolab.com",
%   Port = 51740,
%   Database = <<"auc1">>,
%   {ok, C} = mongo:connect (Host, Port, Database),
%   {true,_} = mongo:auth(C, <<"kovalev">>, <<"endless1987">>),

%   {ok, Phase2UTC, Phase2Url} = get_data_url(),
%   io:format("AT ~p: ~p~n", [Phase2UTC, Phase2Url]),
%   Aucs0 = get_items(Phase2UTC, Phase2Url),
%   write_items(C, Aucs0),
%   % mongo:insert(C, Collection, Aucs).
%   ok.

% write_items(_C, []) ->
%   ok;

% write_items(C, Items) ->
%   {ToWrite, Rest} = 
%   case length(Items) > 200 of
%     true -> lists:split(200, Items);
%     _ -> {Items, []}
%   end,
%   % io:format("w ~p~n", [length(ToWrite)]),
%   emongo:insert(C, "aucs", ToWrite),
%   write_items(C, Rest).


% get_data_url()->
%   Phase1Url = <<"http://eu.battle.net/api/wow/auction/data/%D1%82%D0%B5%D1%80%D0%BC%D0%BE%D1%88%D1%82%D0%B5%D0%BF%D1%81%D0%B5%D0%BB%D1%8C">>,
%   Phase1Json = get_json(Phase1Url),
%   [Files|_] = proplists:get_value(<<"files">>, Phase1Json),
%   Phase2Url = proplists:get_value(<<"url">>, Files),
%   Phase2UTC = proplists:get_value(<<"lastModified">>, Files),
%   {ok, Phase2UTC div 1000, Phase2Url}.


% get_items(UTC, DataUrl) ->
%   Json = get_json(DataUrl),
%   Aucs0 = proplists:get_value(<<"auctions">>, 
%         proplists:get_value(<<"auctions">>, Json)),

%   Aucs = [[{utc, UTC} | A] || A <- Aucs0],
%   Aucs.


% get_json(Url) ->
%   {ok, {{200,_},_,Body}} = lhttpc:request(Url, get, [], [], ?HTTP_TIMEOUT),
%   jsx:decode(Body).