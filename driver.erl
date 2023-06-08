-module(driver).
-compile(export_all).

startServer(NoOfZeros) ->
    register(server, spawn(server, start, [NoOfZeros])).

startClient(ServerAddress) ->
    register(client, spawn(client, start, [ServerAddress])).
