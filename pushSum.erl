-module(pushSum).
-compile(export_all).
-import(math, [sqrt/1, pow/2]).

startPushSum(NumNodes, Topology) ->
    buildPushSumTopology(NumNodes, Topology).

slavePushSum(Master, NumNodes, SelfS, SelfW, Rounds, TerminationActorPID) ->
    %% sending actor ids to the master
    Master ! {id, self()},

    if
        Rounds == 0 ->
            % io:format("~n Process ~w reached 0 ~n", [self()]),
            TerminationActorPID ! {terminate, self()},
            exit(self());
        true ->
            ok
    end,

    %receiving rumor
    receive
        {startPushSum, NodeMap} ->
            % io:format("~n PushSum started from node: ~w ~n", [self()]),
            Neighbors = maps:get(self(), NodeMap),
            NodeToSend = lists:nth(rand:uniform(length(Neighbors)), Neighbors),
            % io:format("~n Sending message to first neighbour: ~w ~n", [NodeToSend]),
            NodeToSend ! {topoMessage, SelfS / 2, SelfW / 2, NodeMap},
            First = SelfS / SelfW,
            slavePushSum(Master, NumNodes, SelfS / 2, SelfW / 2, 3, TerminationActorPID);
        {topoMessage, ReceivedS, ReceivedW, NodeMap} ->
            Neighbors = maps:get(self(), NodeMap),
            NodeToSend = lists:nth(rand:uniform(length(Neighbors)), Neighbors),
            NewS = (SelfS + ReceivedS) / 2,
            NewW = (SelfW + ReceivedW) / 2,
            NodeToSend ! {topoMessage, NewS, NewW, NodeMap},
            RatioDiff = abs((NewS / NewW) - (SelfS / SelfW)),
            % io:format("In process: ~w, Rounds: ~w, W: ~w, S: ~w, RatioDiff: ~w, Sending rumor to process: ~w ~n", [
            %     self(), Rounds, NewW, NewS, RatioDiff, NodeToSend
            % ]),
            io:format("~w: Sending rumor to process ~w ~n", [self(), NodeToSend]),
            if
                RatioDiff < 0.0000000001 ->
                    slavePushSum(Master, NumNodes, NewS, NewW, Rounds - 1, TerminationActorPID);
                true ->
                    slavePushSum(Master, NumNodes, NewS, NewW, 3, TerminationActorPID)
            end
    end.

terminationActor(HasConverged) ->
    receive
        {terminate, PID} ->
            if
                HasConverged ->
                    terminationActor(true);
                true ->
                    {_, Walltime2} = statistics(wall_clock),
                    io:format("~n Convergence Time: ~w ~n", [Walltime2]),
                    terminationActor(true)
            end
    end.

buildPushSumTopology(NumNodes, Topology) ->
    case Topology of
        "Full" ->
            TerminationActorPID = spawn(fun() -> terminationActor(false) end),
            Pids = [spawn(fun() -> slavePushSum(self(), NumNodes, X, 1, 3, TerminationActorPID) end) || X <- lists:seq(1, NumNodes)],
            TempList = [{X, 1} || X <- lists:seq(1, NumNodes)],
            SWMap = maps:from_list(TempList),
            List_Of_Neighbors = buildTopology:buildFullTopo(NumNodes, Pids),
            Map_Of_Neighbors = maps:from_list(List_Of_Neighbors),
            ReceivingNode = lists:nth(rand:uniform(NumNodes), Pids),
            statistics(wall_clock),
            ReceivingNode ! {startPushSum, Map_Of_Neighbors};
        "2D" ->
            GridSize = round(sqrt(NumNodes)),
            SpawnNumber = GridSize * GridSize,
            TerminationActorPID = spawn(fun() -> terminationActor(false) end),
            Pids = [spawn(fun() -> slavePushSum(self(), SpawnNumber, X, 1, 3, TerminationActorPID) end) || X <- lists:seq(1, SpawnNumber)],
            TempList = [{X, 1} || X <- lists:seq(1, SpawnNumber)],
            SWMap = maps:from_list(TempList),
            List_Of_Neighbors = buildTopology:build2DTopo(GridSize, GridSize * GridSize, Pids, SpawnNumber),
            Map_Of_Neighbors = maps:from_list(List_Of_Neighbors),
            ReceivingNode = lists:nth(rand:uniform(SpawnNumber), Pids),
            statistics(wall_clock),
            ReceivingNode ! {startPushSum, Map_Of_Neighbors};
        "Line" ->
            TerminationActorPID = spawn(fun() -> terminationActor(false) end),
            Pids = [spawn(fun() -> slavePushSum(self(), NumNodes, X, 1, 3, TerminationActorPID) end) || X <- lists:seq(1, NumNodes)],
            TempList = [{X, 1} || X <- lists:seq(1, NumNodes)],
            SWMap = maps:from_list(TempList),
            List_Of_Neighbors = buildTopology:buildLineTopo(NumNodes, Pids),
            Map_Of_Neighbors = maps:from_list(List_Of_Neighbors),
            ReceivingNode = lists:nth(rand:uniform(NumNodes), Pids),
            io:format("~n Start Node: ~p ~n", [ReceivingNode]),
            statistics(wall_clock),
            ReceivingNode ! {startPushSum, Map_Of_Neighbors};
        "3D" ->
            TerminationActorPID = spawn(fun() -> terminationActor(false) end),
            GridSize = round(pow(NumNodes, 1 / 3)),
            SpawnNumber = GridSize * GridSize * GridSize,
            Pids = [spawn(fun() -> slavePushSum(self(), SpawnNumber, X, 1, 3, TerminationActorPID) end) || X <- lists:seq(1, SpawnNumber)],
            TempList = [{X, 1} || X <- lists:seq(1, SpawnNumber)],
            SWMap = maps:from_list(TempList),
            List_Of_Neighbors = buildTopology:build3DTopo(GridSize, SpawnNumber, NumNodes, Pids, NumNodes),
            Map_Of_Neighbors = maps:from_list(List_Of_Neighbors),
            ReceivingNode = lists:nth(rand:uniform(SpawnNumber), Pids),
            % io:format("~p~n", [ReceivingNode]),
            statistics(wall_clock),
            ReceivingNode ! {startPushSum, Map_Of_Neighbors}
    end.
