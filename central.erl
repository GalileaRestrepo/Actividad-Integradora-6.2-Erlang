%%% Archivo: central.erl
%%% Autoras: Anna Galilea Restrepo Martínez A01178273 y Regina Romero Alvarado A00840840

-module(central).
-compile({nowarn_unused_function, [distancia/2]}).
-compile({nowarn_unused_function, [loop/1]}).
-compile({nowarn_unused_function, [ubicacion/1]}).
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
            Pid = spawn(?MODULE, loop, [{Ubicacion, [], [], [], 1, []}]),
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
loop({Ubicacion, Taxis, Viajeros, Historial, Contador, ViajesActivos}) ->
    receive
        %% ======= Solicitud de taxi =======
            {solicita_taxi, PidViajero, Nombre, Origen, Destino} ->
            io:format("Recibe: solicitud de taxi de ~p desde ~p hacia ~p~n", [Nombre, Origen, Destino]),
            case lists:keyfind(Nombre, 1, Viajeros) of
                false ->
                    %% Buscar taxis disponibles
                    Disponibles = [ {Id, Pid, U} || {Id, Pid, U} <- Taxis, is_pid(Pid)],
                    case Disponibles of
                        [] ->
                            PidViajero ! {no_disponible},
                            loop({Ubicacion, Taxis, [{Nombre, PidViajero} | Viajeros], Historial, Contador, ViajesActivos});
                        _ ->
                            %% Seleccionar el más cercano
                            OrigenCoord = ubicacion(Origen),
                            {TaxiAsignado, PidTaxi, _} = lists:foldl(
                                fun(Elem, MinSoFar) ->
                                    case distancia(ubicacion(element(3, Elem)), OrigenCoord) < distancia(ubicacion(element(3, MinSoFar)), OrigenCoord) of
                                        true -> Elem;
                                        false -> MinSoFar
                                    end
                                end,
                                hd(Disponibles),
                                tl(Disponibles)
                            ),  
                            
                            %% Enviar mensajes de asignación
                            PidViajero ! {taxi_asignado, TaxiAsignado, Contador},
                            PidTaxi ! {servicio_asignado, Contador, PidViajero, Origen, Destino},

                            %% Agregar viaje activo
                            NuevoViaje = {TaxiAsignado, Contador, PidViajero, Origen, Destino},
                            loop({Ubicacion, Taxis, [{Nombre, PidViajero} | Viajeros], Historial, Contador + 1, [NuevoViaje | ViajesActivos]})
                    end;
                _ ->
                    io:format("Viajero ~p ya tiene una solicitud activa.~n", [Nombre]),
                    loop({Ubicacion, Taxis, Viajeros, Historial, Contador, ViajesActivos})
            end;


        %% ======= Registro de taxi =======
        {registrar_taxi, TaxiId, PidTaxi, Ubic} ->
            io:format("Recibe: registro de taxi ~p desde ~p~n", [TaxiId, Ubic]),
            %% Verifica que el ID no esté repetido
            case lists:keyfind(TaxiId, 1, Taxis) of
                false ->
                    loop({Ubicacion, [{TaxiId, PidTaxi, Ubic} | Taxis], Viajeros, Historial, Contador, ViajesActivos});
                _ ->
                    io:format("Taxi ~p ya estaba registrado.~n", [TaxiId]),
                    loop({Ubicacion, Taxis, Viajeros, Historial, Contador, ViajesActivos})
            end;

        %% ======= Solicitud de PID de taxi (para el comando consultar_estado/1)=======
        {solicitar_pid, TaxiId, From} ->
            case lists:keyfind(TaxiId, 1, Taxis) of
                false ->
                    From ! {pid_taxi, undefined},
                    loop({Ubicacion, Taxis, Viajeros, Historial, Contador, ViajesActivos});
                {_, Pid, _} ->
                    From ! {pid_taxi, Pid},
                    loop({Ubicacion, Taxis, Viajeros, Historial, Contador, ViajesActivos})
            end;

        %% ======= Servicio iniciado de taxi =======
        {servicio_iniciado, TaxiId} ->
            io:format("Central: Servicio iniciado por taxi ~p~n", [TaxiId]),
            %% Aquí luego actualizamos estado del taxi
            %% y registramos inicio real del servicio
            loop({Ubicacion, Taxis, Viajeros, Historial, Contador, ViajesActivos});

        %% ======= Servicio completado de taxi =======
        {servicio_completado, TaxiId} ->
            case lists:keytake(TaxiId, 1, ViajesActivos) of
                false ->
                    io:format("Central: Taxi ~p no tiene viaje activo.~n", [TaxiId]),
                    loop({Ubicacion, Taxis, Viajeros, Historial, Contador, ViajesActivos});
                {value, {TaxiId, IdViaje, PidViajero, Origen, Destino}, ViajesRestantes} ->

                %% Obtener el nombre del viajero a partir del Pid
                Nombre = case lists:keyfind(PidViajero, 2, Viajeros) of
                    {N, _} -> N;
                    _ -> anonimo
                end,

                %% Actualizar ubicación y disponibilidad del taxi
                TaxisActualizados = lists:map(
                    fun({Id, Pid, _}) when Id =:= TaxiId -> {Id, Pid, ubicacion(Destino)};
                        (T) -> T
                    end, Taxis),

                %% Terminar viajero
                PidViajero ! terminar,

                %% Guardar historial
                NuevoHistorial = [{IdViaje, TaxiId, Nombre, Origen, Destino} | Historial],

                %% Continuar el loop
                loop({Ubicacion, TaxisActualizados, Viajeros, NuevoHistorial, Contador, ViajesRestantes})
        end;

        %% ======= Eliminar taxi =======
        {eliminar_taxi, TaxiId, From} ->
            case lists:keytake(TaxiId, 1, Taxis) of
                false ->
                    From ! {no_se_puede_eliminar, TaxiId, "Taxi no registrado"},
                    loop({Ubicacion, Taxis, Viajeros, Historial, Contador, ViajesActivos});
                {value, {TaxiId, Pid, Ubic}, TaxisRestantes} ->
                    %% Pregunta al taxi si está disponible
                    Pid ! {consultar_estado, self()},
                    receive
                        {estado, disponible, _} ->
                            Pid ! stop,
                            From ! {eliminado, TaxiId},
                            loop({Ubicacion, TaxisRestantes, Viajeros, Historial, Contador, ViajesActivos});
                        {estado, ocupado, _} ->
                            From ! {no_se_puede_eliminar, TaxiId, "Taxi ocupado"},
                            loop({Ubicacion, [{TaxiId, Pid, Ubic} | TaxisRestantes], Viajeros, Historial, Contador, ViajesActivos})
                    after 2000 ->
                        From ! {no_se_puede_eliminar, TaxiId, "No respondió"},
                        loop({Ubicacion, [{TaxiId, Pid, Ubic} | TaxisRestantes], Viajeros, Historial, Contador, ViajesActivos})
                    end
            end;

        %% ======= Cancelar taxi de viajero =======
        {cancelar_taxi, Nombre} ->
        %% Buscar al viajero
        case lists:keytake(Nombre, 1, Viajeros) of
            false ->
                io:format("Central: No se encontró al viajero ~p para cancelar~n", [Nombre]),
                loop({Ubicacion, Taxis, Viajeros, Historial, Contador, ViajesActivos});
            {value, {Nombre, PidV}, ViajerosRestantes} ->
                PidV ! cancelado,
                io:format("Central: Cancelando solicitud de ~p~n", [Nombre]),
                loop({Ubicacion, Taxis, ViajerosRestantes, Historial, Contador, ViajesActivos})
        end;

        %% ======= Mostrar lista de taxis =======
        {mostrar_taxis, _From} ->
            io:format("=== Taxis activos ===~n"),
            lists:foreach(fun({Id, Pid, Ubic}) ->
                io:format("Taxi: ~p | PID: ~p | Ubicación: ~p~n", [Id, Pid, Ubic])
            end, Taxis),
            loop({Ubicacion, Taxis, Viajeros, Historial, Contador, ViajesActivos});

        %% ======= Mostrar lista de viajeros =======
        {mostrar_viajeros, _From} ->
            io:format("=== Viajeros activos ===~n"),
            lists:foreach(fun({Nombre, Pid}) ->
                io:format("Viajero: ~p | PID: ~p~n", [Nombre, Pid])
            end, Viajeros),
            loop({Ubicacion, Taxis, Viajeros, Historial, Contador, ViajesActivos});

        %% ======= Mostrar historial de viajes =======
        {mostrar_historial, _From} ->
            io:format("=== Viajes completados ===~n"),
            lists:foreach(fun({IdViaje, TaxiId, Viajero, Origen, Destino}) ->
                io:format("Viaje ~p | Taxi: ~p | Viajero: ~p | De: ~p → A: ~p~n",
                          [IdViaje, TaxiId, Viajero, Origen, Destino])
            end, Historial),
            loop({Ubicacion, Taxis, Viajeros, Historial, Contador, ViajesActivos});

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
            loop({Ubicacion, Taxis, Viajeros, Historial, Contador, ViajesActivos})
    end.

%%% ==========================
%%% FUNCIÓN AUXILIAR: ubicacion/1
%%% ==========================
% Suppress unused warning for ubicacion/1 as it is used internally
ubicacion(aeropuerto) -> {0, 0};
ubicacion(zona_norte) -> {2, 10};
ubicacion(zona_sur) -> {2, -10};
ubicacion(zona_centro) -> {5, 5};
ubicacion(_) -> {0, 0}.  % fallback

%%% ==========================
%%% FUNCIÓN AUXILIAR: distancia/2
%%% ==========================
distancia({X1, Y1}, {X2, Y2}) ->
    math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)).
