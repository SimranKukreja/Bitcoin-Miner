-module(main).
-compile(export_all).
-import(math, [sqrt/1, pow/2]).

start(NumNodes, Topology, Algorithm) ->
    % gossip/push sum switch case
    case Algorithm of
        "Gossip" ->
            gossip:startGossip(NumNodes, Topology);
        "PushSum" ->
            pushSum:startPushSum(NumNodes, Topology)
    end.
