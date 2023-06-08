-module(chord).
-compile(export_all).

start(NoOfNodes, NoOfRequests) ->
    M = math:ceil(math:log2(NoOfNodes)),
    RingSize = round(math:pow(2, M)),
    FingerTableSize = round(math:log2(RingSize)),
    ListOfServerIden = createListOfServerIden([], NoOfNodes, RingSize),
    ServerPidList = [spawn(fun() -> server([], lists:nth(X, ListOfServerIden), FingerTableSize) end) || X <- lists:seq(1, NoOfNodes)],
    ServerIdenPIDMap = createMap(maps:new(), ServerPidList, ListOfServerIden, NoOfNodes),
    SortedServerIdenList = lists:sort(ListOfServerIden),
    FingerTables = [
        createFingerTable(CurrServerIden, RingSize, SortedServerIdenList, maps:get(CurrServerIden, ServerIdenPIDMap), FingerTableSize, NoOfNodes)
     || CurrServerIden <- SortedServerIdenList
    ],
    MasterPID = spawn(chord, master, [NoOfNodes, NoOfRequests, ServerIdenPIDMap, SortedServerIdenList, 0, 0, FingerTables]),
    timer:sleep(5000),
    lookupDriver(NoOfRequests, NoOfNodes, ServerIdenPIDMap, SortedServerIdenList, RingSize, MasterPID),
    timer:sleep(15000),
    MasterPID ! {terminate}.

% Sends lookup queries to servers
lookupDriver(NoOfRequests, NoOfNodes, ServerIdenPIDMap, ListOfServerIden, RingSize, MasterPID) ->
    MaxNode = lists:last(ListOfServerIden),
    MinNode = lists:nth(1, ListOfServerIden),
    [
        spawn(fun() ->
            startHopping(
                lists:nth(X, ListOfServerIden),
                lists:nth(X, ListOfServerIden),
                rand:uniform(RingSize),
                ServerIdenPIDMap,
                1,
                MasterPID,
                lists:nth(((X - 1) rem NoOfNodes) + 1, ListOfServerIden),
                ListOfServerIden,
                MaxNode,
                MinNode
            )
        end)
     || X <- lists:seq(1, NoOfNodes), Y <- lists:seq(1, NoOfRequests)
    ].

% Start hops for each lookup
startHopping(StartServerIden, ServerIden, DataIden, ServerIdenPIDMap, NoOfHops, MasterPID, NextServerIden, ListOfServerIden, MaxNode, MinNode) ->
    CurrServerPID = maps:get(ServerIden, ServerIdenPIDMap),
    CurrServerPID ! {getSuccessor, DataIden, self(), NextServerIden, MaxNode, MinNode, NoOfHops},
    receive
        {getSuccessor, SuccessorIden, NewHopCount} ->
            startHopping(
                StartServerIden,
                SuccessorIden,
                DataIden,
                ServerIdenPIDMap,
                NewHopCount,
                MasterPID,
                index_of(SuccessorIden, ListOfServerIden) + 1,
                ListOfServerIden,
                MaxNode,
                MinNode
            );
        {doneHopping, FinalServerIden, NewHopCount} ->
            MasterPID ! {addHopCount, NewHopCount}
    end.

createListOfServerIden(IdenList, 0, RingSize) ->
    IdenList;
createListOfServerIden(IdenList, N, RingSize) ->
    ServerIden = rand:uniform(RingSize),
    IsAlreadyPresent = lists:member(ServerIden, IdenList),
    if
        IsAlreadyPresent ->
            createListOfServerIden(IdenList, N, RingSize);
        true ->
            createListOfServerIden([ServerIden | IdenList], N - 1, RingSize)
    end.

createMap(SuccessorMap, ServerPidList, ListOfServerIden, 0) ->
    SuccessorMap;
createMap(SuccessorMap, ServerPidList, ListOfServerIden, N) ->
    NewMap = maps:put(lists:nth(N, ListOfServerIden), lists:nth(N, ServerPidList), SuccessorMap),
    createMap(NewMap, ServerPidList, ListOfServerIden, N - 1).

createFingerTable(CurrServerIden, RingSize, SortedServerIdenList, CurrServerPID, FingerTableSize, NumNodes) ->
    FingerTable = [getServerIdenForFT(I, CurrServerIden, FingerTableSize, SortedServerIdenList, NumNodes) || I <- lists:seq(1, FingerTableSize)],
    CurrServerPID ! {fingerTable, FingerTable},
    FingerTable.

getServerIdenForFT(I, CurrServerIden, FingerTableSize, SortedServerIdenList, NumNodes) when I > FingerTableSize -> [];
getServerIdenForFT(I, CurrServerIden, FingerTableSize, SortedServerIdenList, NumNodes) ->
    RingSize = round(math:pow(2, FingerTableSize)),
    TmpIden = ((CurrServerIden + round(math:pow(2, I - 1)) - 1) rem RingSize) + 1,
    ContainsTmpIden = lists:member(TmpIden, SortedServerIdenList),
    if
        ContainsTmpIden ->
            TmpIden;
        true ->
            getNextAvailableServer(TmpIden, SortedServerIdenList, NumNodes, RingSize)
    end.

getNextAvailableServer(RingIdx, SortedServerIdenList, NumNodes, RingSize) ->
    IsPresent = index_of(RingIdx, SortedServerIdenList),
    if
        IsPresent == not_found ->
            NextIdx = (((RingIdx + 1) - 1) rem RingSize) + 1,
            getNextAvailableServer(NextIdx, SortedServerIdenList, NumNodes, RingSize);
        true ->
            RingIdx
    end.

server(FingerTable, CurrServerIden, FingerTableSize) ->
    receive
        {getSuccessor, DataIden, CallbackPID, NextServerIden, MaxNode, MinNode, NoOfHops} ->
            Successor = lists:nth(1, FingerTable),
            if
                DataIden == CurrServerIden ->
                    CallbackPID ! {doneHopping, CurrServerIden, NoOfHops + 1},
                    server(FingerTable, CurrServerIden, FingerTableSize);
                true ->
                    if
                        (DataIden > MaxNode) or (DataIden =< MinNode) ->
                            CallbackPID ! {doneHopping, MinNode, NoOfHops + 1},
                            server(FingerTable, CurrServerIden, FingerTableSize);
                        true ->
                            if
                                (DataIden > CurrServerIden) and (DataIden =< Successor) ->
                                    CallbackPID ! {doneHopping, Successor, NoOfHops + 1},
                                    server(FingerTable, CurrServerIden, FingerTableSize);
                                true ->
                                    ClosestPrecedingNeighbor = getClosestPrecedingNeighbor(
                                        CurrServerIden, DataIden, FingerTable, FingerTableSize, round(math:pow(2, FingerTableSize))
                                    ),
                                    CallbackPID ! {getSuccessor, ClosestPrecedingNeighbor, NoOfHops + 1},
                                    server(FingerTable, CurrServerIden, FingerTableSize)
                            end
                    end
            end;
        {fingerTable, NewFingerTable} ->
            server(NewFingerTable, CurrServerIden, FingerTableSize)
    end.

getClosestPrecedingNeighbor(CurrServerIden, DataIden, FingerTable, 0, RingSize) ->
    CurrServerIden;
getClosestPrecedingNeighbor(CurrServerIden, DataIden, FingerTable, N, RingSize) ->
    NthFingerTableEle = lists:nth(N, FingerTable),
    case CurrServerIden of
        CurrServerIden when CurrServerIden < DataIden ->
            if
                (NthFingerTableEle > CurrServerIden) and (NthFingerTableEle < DataIden) ->
                    NthFingerTableEle;
                true ->
                    getClosestPrecedingNeighbor(CurrServerIden, DataIden, FingerTable, N - 1, RingSize)
            end;
        CurrServerIden when CurrServerIden == DataIden ->
            NthFingerTableEle;
        CurrServerIden when CurrServerIden > DataIden ->
            if
                (NthFingerTableEle > CurrServerIden) or
                    (NthFingerTableEle < DataIden) ->
                    NthFingerTableEle;
                true ->
                    getClosestPrecedingNeighbor(CurrServerIden, DataIden, FingerTable, N - 1, RingSize)
            end;
        _ ->
            getClosestPrecedingNeighbor(CurrServerIden, DataIden, FingerTable, N - 1, RingSize)
    end.

master(NoOfNodes, NoOfRequests, ServerIdenPIDMap, ListOfServerIden, SumOfHopCounts, NoOfRequestsServed, FingerTables) ->
    receive
        {addHopCount, HopCount} ->
            master(NoOfNodes, NoOfRequests, ServerIdenPIDMap, ListOfServerIden, SumOfHopCounts + HopCount, NoOfRequestsServed + 1, FingerTables);
        {terminate} ->
            AverageHopCount = SumOfHopCounts / NoOfRequestsServed,
            io:format("~n Average hop count: ~w ~n", [AverageHopCount])
    end.

index_of(Item, List) -> index_of(Item, List, 1).
index_of(_, [], _) -> not_found;
index_of(Item, [Item | _], Index) -> Index;
index_of(Item, [_ | Tl], Index) -> index_of(Item, Tl, Index + 1).
