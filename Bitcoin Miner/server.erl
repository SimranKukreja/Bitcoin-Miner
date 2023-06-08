-module(server).
-compile(export_all).

start(NoOfZeros) ->
    NoOfActors = 10 * erlang:system_info(logical_processors_available),
    % Start calculating CPU and clock time
    statistics(runtime),
    statistics(wall_clock),
    WorkUnitSize = 1000000,
    % Spawn actors on server and start listening for bitcoins generated and sent by worker actors and clients.
    spawnActors(NoOfActors, NoOfZeros, WorkUnitSize),
    receiveBitcoins(NoOfZeros, NoOfActors, WorkUnitSize, 80).

receiveBitcoins(NoOfZeros, NoOfActors, WorkUnitSize, NoOfBitcoinsToFind) ->
    receive
        % Listen for any client requests and pass NoOfZeros and WorkUnitSize to clients who connect for the first time
        {clientRequestParams, ClientPID} ->
            ClientPID ! {clientParams, NoOfZeros, WorkUnitSize},
            receiveBitcoins(NoOfZeros, NoOfActors, WorkUnitSize, NoOfBitcoinsToFind);
        % Receive bitcoins mined by client
        {clientMinedBitcoin, ClientPID, Msg, Lower} ->
            io:format(" [Client:~w] \t ~s \t ~s ~n", [ClientPID, Msg, Lower]),

            % Stop if all bitcoins found
            ReachedEnd = (NoOfBitcoinsToFind == 0),
            if
                ReachedEnd ->
                    {_, Runtime2} = statistics(runtime),
                    {_, Walltime2} = statistics(wall_clock),
                    io:format("~n Runtime: ~w \tClock time: ~w \tRatio: ~w ~n", [Runtime2, Walltime2, Runtime2 / Walltime2]),
                    exit(self());
                true ->
                    receiveBitcoins(NoOfZeros, NoOfActors, WorkUnitSize, NoOfBitcoinsToFind - 1)
            end;
        % Receive bitcoins mined by server's actors
        {serverMinedBitcoin, WorkerPID, Msg, Lower} ->
            io:format(" [Server: ~w] \t ~s \t ~s ~n", [WorkerPID, Msg, Lower]),

            % Stop if all bitcoins found
            ReachedEnd = (NoOfBitcoinsToFind == 0),
            if
                ReachedEnd ->
                    {_, Runtime2} = statistics(runtime),
                    {_, Walltime2} = statistics(wall_clock),
                    io:format("~n Runtime: ~w \tClock time: ~w \tRatio: ~w ~n", [Runtime2, Walltime2, Runtime2 / Walltime2]),
                    exit(self());
                true ->
                    receiveBitcoins(NoOfZeros, NoOfActors, WorkUnitSize, NoOfBitcoinsToFind - 1)
            end
    end.

% For spawning actors recursively
spawnActors(0, NoOfZeros, WorkUnitSize) ->
    WorkerPID = spawn(fun() -> mine(WorkUnitSize) end),
    WorkerPID ! {self(), NoOfZeros, WorkerPID},
    ok;
spawnActors(NoOfActors, NoOfZeros, WorkUnitSize) ->
    ServerPID = self(),
    WorkerPID = spawn(fun() -> mine(WorkUnitSize) end),
    WorkerPID ! {ServerPID, NoOfZeros, WorkerPID},
    spawnActors(NoOfActors - 1, NoOfZeros, WorkUnitSize).

mine(0) ->
    exit(self());
mine(WorkUnitSize) ->
    receive
        {MasterPid, NoOfZeros, WorkerId} ->
            {_, Output} = randomizer(9),
            Msg = "s.kukreja;" ++ Output,
            Hash = crypto:hash(sha256, Msg),
            EncodeHex = binary:encode_hex(Hash),
            Lower = string:lowercase(EncodeHex),
            ZeroString = getZeroString(NoOfZeros),
            SlicedString = string:slice(Lower, 0, NoOfZeros),
            ComparisonResult = string:equal(SlicedString, ZeroString),
            if
                % If bitcoin found, pass it to the boss actor and continue mining
                ComparisonResult ->
                    MasterPid ! {serverMinedBitcoin, self(), Msg, Lower};
                true ->
                    % Else, continue mining
                    WorkerId ! {MasterPid, NoOfZeros, WorkerId}
            end,
            mine(WorkUnitSize - 1)
    end.

getZeroString(1) -> "0";
getZeroString(K) -> "0" ++ getZeroString(K - 1).

randomizer(Length) ->
    RandBytes = crypto:strong_rand_bytes(Length),
    Base64 = base64:encode(RandBytes),
    BinaryPart = binary_part(Base64, {0, Length}),
    Lower = string:lowercase(BinaryPart),
    {RandBytes, Lower}.
