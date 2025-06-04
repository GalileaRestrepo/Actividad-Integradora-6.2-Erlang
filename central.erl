%%% Archivo: central.erl
%%% 

-module(central).
-export([
    abre_central/1,
    cierra_central/0,
    lista_taxis/0,
    lista_viajeros/0,
    viajes_completados/0,
    loop/1
]).

%%% ========================
%%% COMANDO: abre_central/1
%%% ========================
abre_central(Ubicacion) when is_tuple(Ubicacion) ->
    case whereis(central) of
        undefined ->
            Pid = spawn(?MODULE, loop, [{Ubicacion, [], [], [], 1}]),
            register(central, Pid),
            io:format("Central iniciada en ~p~n", [Ubicacion]),
            {ok, Pid};
        _ ->
            io:format("Ya existe una central activa.~n"),
            {error, ya_iniciada}
    end.

%%% =========================
%%% COMANDO: cierra_central/0
%%% =========================
cierra_central() ->
    case whereis(central) of
        undefined ->
            io:format("No hay una central activa para cerrar.~n"),
            {error, no_activa};
        Pid ->
            Pid ! stop,
            unregister(central),
            io:format("Central cerrada correctamente.~n"),
            ok
    end.

%%% =========================
%%% COMANDOS DE VISUALIZACIÓN
%%% =========================
lista_taxis() ->
    central ! {mostrar_taxis, self()},
    ok.

lista_viajeros() ->
    central ! {mostrar_viajeros, self()},
    ok.

viajes_completados() ->
    central ! {mostrar_historial, self()},
    ok.

%%% ====================
%%% LOOP PRINCIPAL
%%% ====================
loop({Ubicacion, Taxis, Viajeros, Historial, Contador}) ->
    receive

        %% ======= Solicitud de taxi =======
        {solicita_taxi, PidViajero, Nombre, Origen, Destino} ->
            io:format("Recibe: solicitud de taxi de ~p desde ~p hacia ~p~n", [Nombre, Origen, Destino]),
            case lists:keyfind(Nombre, 1, Viajeros) of
                false ->
                    %% Aquí luego se agregará la lógica para asignar taxi real
                    PidViajero ! {no_disponible},
                    loop({Ubicacion, Taxis, [{Nombre, PidViajero} | Viajeros], Historial, Contador});
                _ ->
                    io:format("Viajero ~p ya tiene una solicitud activa.~n", [Nombre]),
                    loop({Ubicacion, Taxis, Viajeros, Historial, Contador})
            end;

        %% ======= Registro de taxi =======
        {registrar_taxi, TaxiId, PidTaxi, Ubic} ->
            io:format("Recibe: registro de taxi ~p desde ~p~n", [TaxiId, Ubic]),
            %% Verifica que el ID no esté repetido
            case lists:keyfind(TaxiId, 1, Taxis) of
                false ->
                    loop({Ubicacion, [{TaxiId, PidTaxi, Ubic} | Taxis], Viajeros, Historial, Contador});
                _ ->
                    io:format("Taxi ~p ya estaba registrado.~n", [TaxiId]),
                    loop({Ubicacion, Taxis, Viajeros, Historial, Contador})
            end;

        %% ======= Solicitud de PID de taxi (para taxi.erl)=======
                {solicitar_pid, TaxiId, From} ->
            case lists:keyfind(TaxiId, 1, Taxis) of
                false ->
                    From ! {pid_taxi, undefined},
                    loop({Ubicacion, Taxis, Viajeros, Historial, Contador});
                {_, Pid, _} ->
                    From ! {pid_taxi, Pid},
                    loop({Ubicacion, Taxis, Viajeros, Historial, Contador})
            end;

        %% ======= Mostrar lista de taxis =======
        {mostrar_taxis, _From} ->
            io:format("=== Taxis activos ===~n"),
            lists:foreach(fun({Id, Pid, Ubic}) ->
                io:format("Taxi: ~p | PID: ~p | Ubicación: ~p~n", [Id, Pid, Ubic])
            end, Taxis),
            loop({Ubicacion, Taxis, Viajeros, Historial, Contador});

        %% ======= Mostrar lista de viajeros =======
        {mostrar_viajeros, _From} ->
            io:format("=== Viajeros activos ===~n"),
            lists:foreach(fun({Nombre, Pid}) ->
                io:format("Viajero: ~p | PID: ~p~n", [Nombre, Pid])
            end, Viajeros),
            loop({Ubicacion, Taxis, Viajeros, Historial, Contador});

        %% ======= Mostrar historial de viajes =======
        {mostrar_historial, _From} ->
            io:format("=== Viajes completados ===~n"),
            lists:foreach(fun({IdViaje, TaxiId, Viajero, Origen, Destino}) ->
                io:format("Viaje ~p | Taxi: ~p | Viajero: ~p | De: ~p → A: ~p~n",
                          [IdViaje, TaxiId, Viajero, Origen, Destino])
            end, Historial),
            loop({Ubicacion, Taxis, Viajeros, Historial, Contador});

        %% ======= Cierre de central =======
        stop ->
            io:format("Recibe: cierre de central. Terminando taxis y viajeros...~n"),
            [PidT ! stop || {_Id, PidT, _U} <- Taxis],
            [PidV ! terminar || {_N, PidV} <- Viajeros],
            io:format("Central apagada.~n"),
            ok;

        %% ======= Otros mensajes no reconocidos =======
        _Otro ->
            io:format("Mensaje desconocido recibido.~n"),
            loop({Ubicacion, Taxis, Viajeros, Historial, Contador})
    end.
