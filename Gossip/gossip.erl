-module(gossip).
-compile(export_all).
-import(math, [sqrt/1, pow/2]).

startGossip(NumNodes, Topology) ->
    buildTopology(NumNodes, Topology).

buildTopology(NumNodes, Topology) ->
    case Topology of
        "Full" ->
            TerminationActorPID = spawn(fun() -> terminationActor(false) end),
            Pids = [spawn(fun() -> slaveGossip(self(), NumNodes, 10, TerminationActorPID) end) || X <- lists:seq(1, NumNodes)],
            NeightborList = buildTopology:buildFullTopo(NumNodes, Pids),
            NeighborsMap = maps:from_list(NeightborList),
            ReceivingNode = lists:nth(rand:uniform(NumNodes), Pids),
            ReceivingNode ! {topoMessage, "Full_Rumor", NeighborsMap};
        "2D" ->
            TerminationActorPID = spawn(fun() -> terminationActor(false) end),
            GridSize = round(sqrt(NumNodes)),
            SpawnNumber = GridSize * GridSize,
            Pids = [spawn(fun() -> slaveGossip(self(), SpawnNumber, 10, TerminationActorPID) end) || X <- lists:seq(1, SpawnNumber)],
            NeightborList = buildTopology:build2DTopo(GridSize, GridSize * GridSize, Pids, NumNodes),
            NeighborsMap = maps:from_list(NeightborList),
            ReceivingNode = lists:nth(rand:uniform(GridSize * GridSize), Pids),
            ReceivingNode ! {topoMessage, "2D_Rumor", NeighborsMap};
        "Line" ->
            TerminationActorPID = spawn(fun() -> terminationActor(false) end),
            Pids = [spawn(fun() -> slaveGossip(self(), NumNodes, 10, TerminationActorPID) end) || X <- lists:seq(1, NumNodes)],
            NeightborList = buildTopology:buildLineTopo(NumNodes, Pids),
            NeighborsMap = maps:from_list(NeightborList),
            ReceivingNode = lists:nth(rand:uniform(NumNodes), Pids),
            ReceivingNode ! {topoMessage, "Line_Rumor", NeighborsMap};
        "3D" ->
            TerminationActorPID = spawn(fun() -> terminationActor(false) end),
            GridSize = round(pow(NumNodes, 1 / 3)),
            SpawnNumber = GridSize * GridSize * GridSize,
            Pids = [spawn(fun() -> slaveGossip(self(), SpawnNumber, 10, TerminationActorPID) end) || X <- lists:seq(1, SpawnNumber)],
            NeightborList = buildTopology:build3DTopo(GridSize, SpawnNumber, NumNodes, Pids, NumNodes),
            NeighborsMap = maps:from_list(NeightborList),
            ReceivingNode = lists:nth(rand:uniform(SpawnNumber), Pids),
            ReceivingNode ! {topoMessage, "Imp_3D_Rumor", NeighborsMap}
    end.

slaveGossip(Master, NumNodes, Counter, TerminationActorPID) ->
    %% sending actor ids to the master
    Master ! {id, self()},
    %receiving rumor
    receive
        {topoMessage, Message, NodeMap} ->
            Neighbors = maps:get(self(), NodeMap),
            ReceivingNode = lists:nth(rand:uniform(length(Neighbors)), Neighbors),
            ReceivingNode ! {topoMessage, Message, NodeMap},
            io:format("~p received rumour ~p ~n", [ReceivingNode, Message]),
            if
                Counter == 0 ->
                    io:format("~n Process ~w reached 0 ~n", [self()]),
                    TerminationActorPID ! {terminate, self()},
                    exit(self());
                true ->
                    slaveGossip(Master, NumNodes, Counter - 1, TerminationActorPID)
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
