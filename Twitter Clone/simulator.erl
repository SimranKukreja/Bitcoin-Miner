-module(simulator).
-compile(export_all).

%--------------------------------------------------------------------------------------
% Starting Twitter Engine for as many users as possible
%--------------------------------------------------------------------------------------
%
start() ->
    {ok, NoOfUsers} = io:read("Enter number of users:"),
    {ok, NoOfSubscribersMax} = io:read("Enter number of subscribers:"),
    {_, WallClockTime1} = statistics(wall_clock),
    repository:startRepository(),
    startTwitterEngine(NoOfUsers),
    zipfSubscriberTweetDist(NoOfUsers, NoOfUsers, NoOfSubscribersMax),
    {_, WallClockTime2} = statistics(wall_clock),
    io:format("Time taken for zipf: ~p \n", [WallClockTime2]).

%handling twitter engine start w.r.t the number of subscribers and users
startTwitterEngine(0) ->
    ok;
startTwitterEngine(NoOfUsers) ->
    ListOfUsers = list_to_atom(integer_to_list(NoOfUsers)),
    twitter_engine:registerUserAccount(ListOfUsers, "password"),
    twitter_engine:zipfTweet(NoOfUsers),
    startTwitterEngine(NoOfUsers - 1).

%--------------------------------------------------------------------------------------
% Zipf distribution handling for subscribers and tweets
%--------------------------------------------------------------------------------------
zipfFollowerSimulation(_, _, 0) ->
    ok;
zipfFollowerSimulation(UserID, NoOfUsers, NoOfFollowers) ->
    % distribution based on the number of subscribers
    FollowerIndex = rand:uniform(NoOfUsers),
    if
        (FollowerIndex == UserID) ->
            zipfFollowerSimulation(UserID, NoOfUsers, NoOfFollowers);
        true ->
            twitter_engine:subscribe(list_to_atom(integer_to_list(FollowerIndex)), list_to_atom(integer_to_list(UserID))),
            zipfFollowerSimulation(UserID, NoOfUsers, NoOfFollowers - 1)
    end.
zipfSubscriberTweetDist(0, _, _) ->
    ok;
zipfSubscriberTweetDist(UserID, NoOfUsers, NoOfSubscribersMax) ->
    NoOfFollowers = trunc(NoOfSubscribersMax / UserID),
    zipfFollowerSimulation(UserID, NoOfUsers, NoOfFollowers),
    zipfSubscriberTweetDist(UserID - 1, NoOfUsers, NoOfSubscribersMax).
