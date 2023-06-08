-module(ws_h).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, Opts) ->
	{cowboy_websocket, Req, Opts}.

websocket_init(State) ->
	erlang:start_timer(100000000000, self(), <<"Hello, welcome to Twitter Clone!">>),
	{[], State}.

websocket_handle({text, Msg}, State) ->
	Tokens = string:tokens(binary_to_list(Msg), ">>>>"),
	Condition = lists:nth(1, Tokens),
	io:fwrite("Condition ~p",[Condition]),
	io:fwrite("Msg ~p",[Msg]),
	case Condition of 
		"Register" -> 
			Uname = list_to_binary(lists:nth(2, Tokens)),
			Username = list_to_atom(lists:nth(2, Tokens)),
			Response = twitter_engine:register(Username,lists:nth(3, Tokens)),
			io:format("Response in handler ~p", [Response]),
			Key = lists:nth(1, Response),
			if
				Key == "Success" ->
					{[{text, << "Successfully registered username: ",Uname/binary>>}], State};
				true ->
					{[{text, << Uname/binary, " already registered, try logging in.">>}], State}
			end;
		"Login" -> 
			Uname = list_to_binary(lists:nth(2, Tokens)),
			Username = list_to_atom(lists:nth(2, Tokens)),
			Response = twitter_engine:login(Username,lists:nth(3, Tokens)),
			io:format("Response in handler ~p", [Response]),
			Key = lists:nth(1, Response),
			if
				Key == "Success" ->
					{[{text, << "Login successful for ",Uname/binary>>}], State};
				true ->
					{[{text,<<"Incorrect username/password. Try logging in again.">>}], State}
			end;
		"Tweet" ->
			Username = list_to_atom(lists:nth(2, Tokens)),
			Message = lists:nth(3, Tokens),
			Response = list_to_binary(twitter_engine:tweet(Username,Message)),
			{[{text,<<Response/binary>>}], State};
		"Follow" ->
			Username = list_to_atom(lists:nth(2, Tokens)),
			UserToFollow = list_to_atom(lists:nth(3, Tokens)),
			Response = list_to_binary(twitter_engine:subscribe(Username,UserToFollow)),
			{[{text,<<Response/binary>>}], State};
		"ViewFeed" ->
			Username = list_to_atom(lists:nth(2, Tokens)),
			Response = twitter_engine:fetchTweets(Username),
			Tweets = lists:flatten(io_lib:format("~p", [Response])),
			TweetsBinary = list_to_binary(Tweets),
			{[{text,<<TweetsBinary/binary>>}], State};
		"Hashtag" ->
			Message = lists:nth(2, Tokens),
			Response = twitter_engine:fetchTweetsWithHashtag(Message),
			Tweets = lists:flatten(io_lib:format("~p", [Response])),
			TweetsBinary = list_to_binary(Tweets),
			{[{text,<<TweetsBinary/binary>>}], State};
		"Mention" ->
			Message = lists:nth(2, Tokens),
			Response = twitter_engine:fetchTweetsWithAtMention(Message),
			Tweets = lists:flatten(io_lib:format("~p", [Response])),
			TweetsBinary = list_to_binary(Tweets),
			{[{text,<<TweetsBinary/binary>>}], State};
		"Retweet" ->
			Username = list_to_atom(lists:nth(2, Tokens)),
			Message = lists:nth(3, Tokens),
			Response = list_to_binary(twitter_engine:retweet(Username,Message)),
			{[{text,<<Response/binary>>}], State};
		"Logout" ->
			Username = list_to_atom(lists:nth(2, Tokens)),
			Response = list_to_binary(twitter_engine:logout(Username)),
			{[{text,<<Response/binary>>}], State}
	end;
websocket_handle(_Data, State) ->
	{[], State}.

websocket_info({timeout, _Ref, Msg}, State) ->
	erlang:start_timer(100000000000, self(), <<"Connected.">>),
	{[{text, Msg}], State};
websocket_info(_Info, State) ->
	{[], State}.
