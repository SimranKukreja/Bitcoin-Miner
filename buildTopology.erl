-module(buildTopology).
-compile(export_all).

%Building 3D Grid Topology
build3DTopo(_, 0, _, _, _) ->
    [];
build3DTopo(GridSize, SpawnNumber, NumNodes, Pids, TotalNumNodes) when
    NumNodes > 0
->
    Prods = [[K, I, J] || K <- lists:seq(0, GridSize - 1), I <- lists:seq(0, GridSize - 1), J <- lists:seq(0, GridSize - 1)],
    [K, I, J] = lists:nth(SpawnNumber, Prods),
    Key = lists:nth(SpawnNumber, Pids),
    Neighbor_list =
        threeDCon1(I, J, K, GridSize, Pids) ++ threeDCon2(I, J, K, GridSize, Pids) ++ threeDCon3(I, J, K, GridSize, Pids) ++
            threeDCon4(I, J, K, GridSize, Pids) ++ threeDCon5(I, J, K, GridSize, Pids) ++ threeDCon6(I, J, K, GridSize, Pids),
    Non_neighbor_list = Pids -- Neighbor_list -- [Key],
    Random_Node = lists:nth(rand:uniform(length(Non_neighbor_list)), Non_neighbor_list),
    build3DTopo(GridSize, SpawnNumber - 1, NumNodes, Pids, TotalNumNodes) ++ [{Key, Neighbor_list ++ [Random_Node]}].

%Conditions for finding 3D Grid Topology Neighbors
threeDCon1(I, J, K, GridSize, Actors) ->
    if
        J + 1 < GridSize ->
            [lists:nth(K * GridSize * GridSize + I * GridSize + J + 1 + 1, Actors)];
        true ->
            []
    end.

threeDCon2(I, J, K, GridSize, Actors) ->
    if
        J - 1 >= 0 ->
            [lists:nth(K * GridSize * GridSize + I * GridSize + J - 1 + 1, Actors)];
        true ->
            []
    end.

threeDCon3(I, J, K, GridSize, Actors) ->
    if
        I - 1 >= 0 ->
            [lists:nth(K * GridSize * GridSize + (I - 1) * GridSize + J + 1, Actors)];
        true ->
            []
    end.

threeDCon4(I, J, K, GridSize, Actors) ->
    if
        I + 1 < GridSize ->
            [lists:nth(K * GridSize * GridSize + (I + 1) * GridSize + J + 1, Actors)];
        true ->
            []
    end.

threeDCon5(I, J, K, GridSize, Actors) ->
    if
        K - 1 >= 0 ->
            [lists:nth((K - 1) * GridSize * GridSize + I * GridSize + J + 1, Actors)];
        true ->
            []
    end.

threeDCon6(I, J, K, GridSize, Actors) ->
    if
        K + 1 < GridSize ->
            [lists:nth((K + 1) * GridSize * GridSize + I * GridSize + J + 1, Actors)];
        true ->
            []
    end.

%Building Full Network topology
buildFullTopo(0, _) ->
    [];
buildFullTopo(NumNodes, Pids) when NumNodes > 0 ->
    Key = lists:nth(NumNodes, Pids),
    Neighbor_list = lists:delete(Key, Pids),
    buildFullTopo(NumNodes - 1, Pids) ++ [{Key, Neighbor_list}].

%Building Line Topology
buildLineTopo(0, _) ->
    [];
buildLineTopo(NumNodes, Pids) when NumNodes > 0 ->
    Key = lists:nth(NumNodes, Pids),
    if
        NumNodes == length(Pids) ->
            buildLineTopo(NumNodes - 1, Pids) ++ [{Key, [lists:nth(NumNodes - 1, Pids)]}];
        NumNodes == 1 ->
            buildLineTopo(NumNodes - 1, Pids) ++ [{Key, [lists:nth(NumNodes + 1, Pids)]}];
        true ->
            buildLineTopo(NumNodes - 1, Pids) ++ [{Key, [lists:nth(NumNodes - 1, Pids), lists:nth(NumNodes + 1, Pids)]}]
    end.

%Building 2D Grid Topology
build2DTopo(_, 0, _, _) ->
    [];
build2DTopo(GridSize, NumNodes, Pids, TotalNumNodes) when GridSize > 0 ->
    Prods = [[I, J] || I <- lists:seq(0, GridSize - 1), J <- lists:seq(0, GridSize - 1)],
    [I, J] = lists:nth(NumNodes, Prods),
    Key = lists:nth(NumNodes, Pids),
    build2DTopo(GridSize, NumNodes - 1, Pids, TotalNumNodes) ++
        [
            {Key,
                twoDCon1(I, J, GridSize, Pids) ++ twoDCon2(I, J, GridSize, Pids) ++ twoDCon3(I, J, GridSize, Pids) ++ twoDCon4(I, J, GridSize, Pids) ++
                    twoDCon5(I, J, GridSize, Pids) ++ twoDCon6(I, J, GridSize, Pids) ++ twoDCon7(I, J, GridSize, Pids) ++
                    twoDCon8(I, J, GridSize, Pids)}
        ].

%Conditions for finding 2D topology neighbors
twoDCon1(I, J, GridSize, Actors) ->
    if
        J + 1 < GridSize ->
            [lists:nth(I * GridSize + J + 1 + 1, Actors)];
        true ->
            []
    end.

twoDCon2(I, J, GridSize, Actors) ->
    if
        J - 1 >= 0 ->
            [lists:nth(I * GridSize + J, Actors)];
        true ->
            []
    end.

twoDCon3(I, J, GridSize, Actors) ->
    if
        I - 1 >= 0 ->
            [lists:nth((I - 1) * GridSize + J + 1, Actors)];
        true ->
            []
    end.
twoDCon4(I, J, GridSize, Actors) ->
    if
        I + 1 < GridSize ->
            [lists:nth((I + 1) * GridSize + J + 1, Actors)];
        true ->
            []
    end.

twoDCon5(I, J, GridSize, Actors) ->
    if
        (I - 1 >= 0) and (J - 1 >= 0) ->
            [lists:nth((I - 1) * GridSize + J, Actors)];
        true ->
            []
    end.

twoDCon6(I, J, GridSize, Actors) ->
    if
        (I + 1 < GridSize) and (J + 1 < GridSize) ->
            [lists:nth((I + 1) * GridSize + J + 1 + 1, Actors)];
        true ->
            []
    end.

twoDCon7(I, J, GridSize, Actors) ->
    if
        (I - 1 >= 0) and (J + 1 < GridSize) ->
            [lists:nth((I - 1) * GridSize + J + 1 + 1, Actors)];
        true ->
            []
    end.

twoDCon8(I, J, GridSize, Actors) ->
    if
        (I + 1 < GridSize) and (J - 1 >= 0) ->
            [lists:nth((I + 1) * GridSize + J, Actors)];
        true ->
            []
    end.
