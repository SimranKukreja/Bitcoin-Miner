-module(twitter_engine).
-compile(export_all).
-behavior(gen_server).

%--------------------------------------------------------------------------------------
% Gen server initialization
%--------------------------------------------------------------------------------------
init(UserID) ->
    repository:start(),
    {ok, UserID}.

%Gen server handle call callback implementation
handle_call(Msg, UserID, State) ->
    {reply, "msg", State}.

%Gen server handle cast callback implementation
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast({getTweet, Tweet}, State) ->
    {noreply, State}.

%Ending gen server session in case of any unforseen terminations
terminate(_Reason, _State) ->
    io:fwrite("\nGen server terminated\n").

%--------------------------------------------------------------------------------------
% User Account Registration and Logout
%--------------------------------------------------------------------------------------

login(UserID, Password) ->
    repository:start(),
    Response = gen_server:call({global, repository}, {login, UserID, Password}),
    if
        Response == "Success" ->
            gen_server:start_link({global, UserID}, ?MODULE, UserID, []);
        true ->
            io:format("\nLogin unsuccessful\n")
    end,
    [Response].

%register the current user in the repo
register(UserID, Password) ->
    repository:start(),
    Response = gen_server:call({global, repository}, {register, UserID, Password}),
    if
        Response == "Success" ->
            gen_server:start_link({global, UserID}, ?MODULE, UserID, []);
        true ->
            io:format("\nRegistration unsuccessful\n")
    end,
    [Response].

%logout the user from the account
logout(UserID) ->
    gen_server:cast({global, UserID}, stop),
    ["Logged out successfully"].

%--------------------------------------------------------------------------------------
% User Subscription or follow
%--------------------------------------------------------------------------------------

%subscribe or follow another user, to have access to their tweers
%parameterizing with the followerid and the followingid
subscribe(FollowerID, FollowingID) ->
    Reply = gen_server:call({global, repository}, {subscribe, FollowerID, FollowingID}),
    io:format("\nSubscribe status: ~p", [Reply]),
    ["Followed successfully"].

%--------------------------------------------------------------------------------------
% User Tweet and Re-tweet
%--------------------------------------------------------------------------------------

%posting a tweet, senderid = id of user making the tweet
tweet(SenderID, Data) ->
    gen_server:cast({global, repository}, {saveData, SenderID, Data}),
    ["Tweet successful"].

%retweet function to tweet the currIdx tweet made by the userid having
retweet(UserID, CurrIdx) ->
    gen_server:cast({global, repository}, {retweet, UserID, CurrIdx}),
    ["Retweet successful"].

%--------------------------------------------------------------------------------------
% Fetching tweets from the repository created based on @mention, hashtags and queries
%--------------------------------------------------------------------------------------
% Fetching tweets of all the people you follow on the current user's feed
% Use UserId to fetch the tweets from the repo
fetchTweets(UserID) ->
    Tweets = gen_server:call({global, repository}, {fetchTweets, UserID}),
    ListTweets = [{element(1,X), element(2,X), element(3, X)}  || X <- Tweets],
    ListTweets.

%find tweets in repo having the hashtag as as entered by current user
fetchTweetsWithHashtag(CurrHashTag) ->
    Tweets = gen_server:call({global, repository}, {fetchRepoTweetsWithHashtag, CurrHashTag}),
    ListTweets = [{element(1,X),element(3, X)}  || X <- Tweets],
    ListTweets.

%find tweets in repo having the @mention as entered by current user
fetchTweetsWithAtMention(MentionedUserID) ->
    Tweets = gen_server:call({global, repository}, {fetchRepoTweetsWithMention, MentionedUserID}),
    ListTweets = [{element(1,X),element(3, X)}  || X <- Tweets],
    ListTweets.


getPeopleYouFollow(UserID) ->
    gen_server:call({global, repository}, {getPeopleYouFollow, UserID}).

getMyOwnTweets(UserID) ->
    gen_server:call({global, repository}, {getMyOwnTweets, UserID}).
