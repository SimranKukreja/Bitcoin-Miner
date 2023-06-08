-module(repository).
-behavior(gen_server).
-compile(export_all).
-import(lists, [nth/2]).

%--------------------------------------------------------------------------------------
% Making gen server database calls for tweets and subscribers
%--------------------------------------------------------------------------------------
% start the gen server repository
start() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

%--------------------------------------------------------------------------------------
% Gen server initialization and termination
%--------------------------------------------------------------------------------------
init(_Args) ->
    ets:new(peopleYouFollowTable, [bag, named_table]),
    ets:new(yourFollowersTable, [bag, named_table]),
    ets:new(userTweetIdTable, [bag, named_table]),
    ets:new(retweetTable, [bag, named_table]),
    ets:new(tweetIDTweetTable, [set, named_table]),
    ets:new(hashtagTable, [bag, named_table]),
    ets:new(mentionsTable, [bag, named_table]),
    ets:new(usersTable, [set, named_table]),
    {ok, self()}.

stop() ->
    gen_server:cast(?MODULE, stop).

terminate(_Reason, _State) ->
    io:fwrite("\nGen server terminated\n").

%--------------------------------------------------------------------------------------
% Handling the gen server database calls in the handle call and handle cast methods
%--------------------------------------------------------------------------------------
handle_call({register, UserID, InputPassword}, _FromID, State) ->
    UserEntry = ets:lookup(usersTable, UserID),
    LenUserEntry = length(UserEntry),
    if
        LenUserEntry == 0 ->
            ets:insert(usersTable, {UserID, InputPassword}),
            ReplyMessage = "Success";
        true ->
            ReplyMessage = "Failed"
    end,
    {reply, ReplyMessage, State};

handle_call({login, UserID, InputPassword}, _FromID, State) ->
    UserEntry = ets:lookup(usersTable, UserID),
    LenUserEntry = length(UserEntry),
    if
        LenUserEntry == 0 ->
            ReplyMessage = "Failed";
        true ->
            StoredPassword = element(2, nth(1, UserEntry)),
            if
                InputPassword == StoredPassword ->
                    ReplyMessage = "Success";
                true ->
                    ReplyMessage = "Failed"
            end
    end,
    {reply, ReplyMessage, State};

% inserting the followers and the users they are following for every user in the db
handle_call({subscribe, FollowerID, FollowingID}, _FromID, State) ->
    FollowingUser = ets:lookup(usersTable, FollowingID),
    FollowingUserLen = length(FollowingUser),
    if
        FollowingUserLen =/= 0 ->
            ets:insert(peopleYouFollowTable, {FollowerID, FollowingID}),
            ets:insert(yourFollowersTable, {FollowingID, FollowerID}),
            Reply = "Success";
        true ->
            Reply = "Failed"
    end,
    {reply, Reply, State};

% fetching tweets from db of all the users that the user is following
handle_call({fetchTweets, User}, _FromID, State) ->
    ListOfFollowing = ets:lookup(peopleYouFollowTable, User),
    ListLen = length(ListOfFollowing),
    ListOfTweets = fetchTweets(userTweetIdTable, ListOfFollowing, ListLen, []),
    {reply, ListOfTweets, State};

% fetching tweets from db by looking up the hastags in the db
handle_call({fetchRepoTweetsWithHashtag, CurrHashTag}, _FromID, State) ->
    Tweets = ets:lookup(hashtagTable, CurrHashTag),
    {reply, Tweets, State};

% fetching tweets from db by looking up the @mentions in the db
handle_call({fetchRepoTweetsWithMention, MentionedUserID}, _FromID, State) ->
    Tweets = ets:lookup(mentionsTable, MentionedUserID),
    {reply, Tweets, State};

handle_call({getPeopleYouFollow, UserID}, _FromID, State) ->
    List = ets:lookup(peopleYouFollowTable, UserID),
    {reply, List, State};

handle_call({getMyOwnTweets, UserID}, _FromID, State) ->
    List = ets:lookup(userTweetIdTable, UserID),
    {reply, List, State}.

% inserting the user id and tweet id for every user in the user tweet table
% inserting the tweet id and tweet message in the tweet table
handle_cast({saveData, SenderID, TweetData}, State) ->
    TableLen = getSizeOfTable(userTweetIdTable),
    TweetIndex = TableLen + 1,
    Tweet = {SenderID, TweetIndex, TweetData},
    ets:insert(userTweetIdTable, Tweet),
    ets:insert(tweetIDTweetTable, {TweetIndex, TweetData, SenderID}),
    pushTweetToFollowerDriver(SenderID, Tweet),
    ListOfHashTags = fetchListOfHashTags(TweetData),
    InsertHashTagInList = fun(HashTagToInsert) ->
        ets:insert(hashtagTable, {nth(1, HashTagToInsert), TweetIndex, TweetData}),
        true
    end,
    lists:all(InsertHashTagInList, ListOfHashTags),
    ListOfMentions = fetchMentionsList(TweetData),
    InsertMentionInList = fun(MentionToInsert) ->
        ets:insert(mentionsTable, {nth(1, MentionToInsert), TweetIndex, TweetData}),
        true
    end,
    lists:all(InsertMentionInList, ListOfMentions),
    {noreply, State};

% inserting the retweet id and the retweet message in the retweet table
handle_cast({retweet, UserID, CurrIdx}, State) ->
    IntCurrIdx = list_to_integer(CurrIdx),
    LookupRes = ets:lookup(tweetIDTweetTable, IntCurrIdx),
    LenLookupRes = length(LookupRes),
    if LenLookupRes == 1 ->
        [{TweetIdx, Msg, TweeterID}] = LookupRes,
        TableLen = getSizeOfTable(retweetTable),
        RetweetTableIdx = TableLen + 1,
        ReTweet = {UserID, RetweetTableIdx, Msg, TweeterID, TweetIdx},
        ets:insert(retweetTable, ReTweet),

        Tweet = {UserID, TweetIdx, Msg},
        ets:insert(userTweetIdTable, Tweet),
        pushTweetToFollowerDriver(UserID, ReTweet);
    true ->
        io:format("\nNo such tweet present\n")
    end,
    {noreply, State};

% handling logging out from account for the user
handle_cast(logoutFromAccount, State) ->
    {stop, normal, State}.

% compute the gen server repository size
getSizeOfTable(Table) ->
    TableInfo = ets:info(Table),
    if
        (TableInfo =/= undefined) ->
            TableSize = element(2, nth(10, TableInfo));
        true ->
            TableSize = 0
    end,
    TableSize.

% adding the tweet to the gen server table
pushTweetToFollower(_, _, _, 0) ->
    ok;
pushTweetToFollower(SenderUserID, TweetMessage, ListOfFollowers, FollowerIdx) ->
    ReceiverUserID = element(2, nth(FollowerIdx, ListOfFollowers)),
    gen_server:cast({global, ReceiverUserID}, {getTweet, TweetMessage}),
    pushTweetToFollower(SenderUserID, TweetMessage, ListOfFollowers, FollowerIdx - 1).

% adding the tweet to the followetlist tweet
% each follower should be able to view the tweet made
pushTweetToFollowerDriver(UserID, TweetMessage) ->
    ListOfFollowers = ets:lookup(yourFollowersTable, UserID),
    NoOfFoll = length(ListOfFollowers),
    pushTweetToFollower(UserID, TweetMessage, ListOfFollowers, NoOfFoll).

% Fetch a list of all hashtags in a tweet in case of match with the regex expression
fetchListOfHashTags(TweetData) ->
    RegexRes = re:run(TweetData, "(?<=#)\\w+", [global, {capture, all, list}]),
    if
        RegexRes /= nomatch ->
            {match, ListOfHashTags} = RegexRes,
            ListOfHashTags;
        true ->
            ListOfHashTags = [],
            ListOfHashTags
    end.

% Fetch the @mentions lists using the regex expression
fetchMentionsList(TweetData) ->
    RegexRes = re:run(TweetData, "(?<=@)\\w+", [global, {capture, all, list}]),
    if
        RegexRes /= nomatch ->
            {match, ListOfMentions} = RegexRes,
            ListOfMentions;
        true ->
            ListOfMentions = [],
            ListOfMentions
    end.

% Fetching the tweets from the repo based on the the users that they are following
fetchTweets(_, _, 0, ListOfTweets) ->
    ListOfTweets;
fetchTweets(Table, ListOfFollowing, FollowingIdx, ListOfTweets) ->
    User = element(2, nth(FollowingIdx, ListOfFollowing)),
    UserTweets = ets:lookup(Table, User),
    TmpList = lists:append(ListOfTweets, UserTweets),
    fetchTweets(Table, ListOfFollowing, FollowingIdx - 1, TmpList).
