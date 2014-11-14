-module(wa_fetcher).
-export([start_link/1, start_link/2, fetch/0]).
-export([init/1, terminate/2, handle_call/3, handle_info/2]).

-define(HTTP_TIMEOUT, 100000).
-define(FETCH_INTERVAL, 30*60*1000).
-define(RETRY_INTERVAL,    30*1000).
-define(PHASE1_URL, <<"http://eu.battle.net/api/wow/auction/data/%D1%82%D0%B5%D1%80%D0%BC%D0%BE%D1%88%D1%82%D0%B5%D0%BF%D1%81%D0%B5%D0%BB%D1%8C">>).

-record(?MODULE, {
  db,
  interval,
  timer
}).

start_link(DB) ->
  start_link(DB, ?FETCH_INTERVAL).

start_link(DB, Interval) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [DB, Interval], []).


fetch() ->
  gen_server:call(?MODULE, fetch).


init([DB, Interval]) ->
  T = erlang:send_after(0, self(), fetch),
  State = #?MODULE{db = DB, interval= Interval, timer = T},
  {ok, State}.

terminate(Reason,_) ->
  lager:warning("fetcher dead: ~p", [Reason]),
  ok.



handle_call(fetch, _From, #?MODULE{interval= I, timer = T, db = DB}=State) ->
  erlang:cancel_timer(T),
  {Result, Interval} = case fetch_db(DB, ?PHASE1_URL) of
    {ok, R} -> {{ok, R}, I};
    {retry, After} -> {retry, After}
  end,
  T1 = erlang:send_after(Interval, self(), fetch),
  {reply, Result, State#?MODULE{timer = T1}}.



handle_info(fetch, #?MODULE{}=State) ->
  {reply, _Result, State1} = handle_call(fetch, undefined, State),
  {noreply, State1};

handle_info(Msg, #?MODULE{} = State) ->
  lager:info("unexpected msg: ~p", [Msg]),
  {noreply, State}.



fetch_db(DB, Url) ->
  RetryAfter = ?RETRY_INTERVAL,
  case get_data_url(Url) of
    {ok, DataUTC, DataUrl} -> 
      case is_fetched(DataUTC) of
        true -> 
          lager:info("DB OK ~p", [DataUTC]),
          {ok, 0};
        false -> 
          lager:info("DB FETCHING ~p", [DataUTC]),
          case get_items(DataUTC, DataUrl) of
            {ok, Aucs} ->
              lager:info("DB SAVING ~p", [DataUTC]),
              write_items(DB, Aucs),
              emongo:insert(DB, "dumps", [{utc, DataUTC}]),
              lager:info("DB OK ~p", [DataUTC]),
              {ok, length(Aucs)};
            Other ->
              lager:info("RETRY AFTER ~p(~p)", [RetryAfter, Other]),
              {retry, RetryAfter}
          end
      end;
    Other ->
      lager:info("RETRY AFTER ~p(~p)", [RetryAfter, Other]),
      {retry, RetryAfter}
  end.



is_fetched(UTC) -> 
  case emongo:find_one(aucs_db, "dumps", [{"utc", UTC}]) of
    [] -> false;
    _ -> true
  end.


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

write_items(_C, []) ->
  ok;

write_items(C, Items) ->
  {ToWrite, Rest} = 
  case length(Items) > 200 of
    true -> lists:split(200, Items);
    _ -> {Items, []}
  end,
  % io:format("w ~p~n", [length(ToWrite)]),
  emongo:insert(C, "aucs", ToWrite),
  write_items(C, Rest).


get_data_url(Url)->
  case get_json(Url) of
    {ok, Json} ->
      [Files|_] = proplists:get_value(<<"files">>, Json),
      DataUrl = proplists:get_value(<<"url">>, Files),
      DataUTC = proplists:get_value(<<"lastModified">>, Files),
      {ok, DataUTC div 1000, DataUrl};
    Other ->
      Other
  end.

get_items(UTC, DataUrl) ->
  lager:info("DB DATA ~p", [DataUrl]),
  case get_json(DataUrl) of
    {ok, Json} -> 
      Aucs = proplists:get_value(<<"auctions">>, 
              proplists:get_value(<<"auctions">>, Json)),
      {ok, [[{utc, UTC} | A] || A <- Aucs]};
    Other -> 
      Other
  end.


get_json(Url) ->
  case lhttpc:request(Url, get, [], [], ?HTTP_TIMEOUT) of
    {ok, {{200,_},_,Body}} -> 
      {ok, jsx:decode(Body)};
    {ok, {{404,_},_,_}} ->
      {error, not_found}
  end.