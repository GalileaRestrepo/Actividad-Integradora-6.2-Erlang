%%% Archivo: taxi.erl
%%% Autoras: Anna Galilea Restrepo Martínez A01178273 y Regina Romero Alvarado A00840840

-module(taxi).
-export([
    registra_taxi/3,
    loop/4,
    consultar_estado/2,
    elimina_taxi/1,
    servicio_iniciado/1,
    servicio_completado/1
]).

%%% ===========================
%%% COMANDO: registra_taxi/3
%%% ===========================
%%% Este comando se usa para registrar un taxi en la central.
%%% Como estamos trabajando con nodos distribuidos, se pasa explícitamente
%%% el nodo donde está corriendo la central (ej. 'central@laptop-de-juan').
%%% Así evitamos depender de un hostname fijo y el código es portable para otros usuarios.

registra_taxi(TaxiId, UbicacionInicial, NodoCentral) 
when is_atom(TaxiId), is_atom(UbicacionInicial), is_atom(NodoCentral) ->
    spawn(?MODULE, loop, [TaxiId, disponible, UbicacionInicial, NodoCentral]).

%%% ===============================
%%% COMANDO: consultar_estado/1
%%% ===============================
consultar_estado(TaxiId, NodoCentral) when is_atom(TaxiId), is_atom(NodoCentral) ->
    {central, NodoCentral} ! {solicitar_pid, TaxiId, self()},
    receive
        {pid_taxi, undefined} ->
            io:format("Taxi ~p no está registrado.~n", [TaxiId]);
        {pid_taxi, Pid} ->
            Pid ! {consultar_estado, self()},
            receive
                {estado, Estado, Ubic} ->
                    io:format("Taxi ~p | Estado: ~p | Ubicación: ~p~n", [TaxiId, Estado, Ubic])
            after 2000 ->
                io:format("No se recibió respuesta del taxi ~p~n", [TaxiId])
            end
    after 2000 ->
        io:format("No se encontró el taxi ~p en central~n", [TaxiId])
    end.


%%% =============================
%%% COMANDO: elimina_taxi/1
%%% =============================
elimina_taxi(TaxiId) when is_atom(TaxiId) ->
    central ! {eliminar_taxi, TaxiId, self()},
    receive
        {eliminado, TaxiId} ->
            io:format("Taxi ~p eliminado correctamente.~n", [TaxiId]);
        {no_se_puede_eliminar, TaxiId, Motivo} ->
            io:format("No se pudo eliminar taxi ~p: ~s~n", [TaxiId, Motivo]);
        _ ->
            io:format("Respuesta inesperada al intentar eliminar taxi ~p~n", [TaxiId])
    after 2000 ->
        io:format("Timeout al intentar eliminar taxi ~p~n", [TaxiId])
    end.

%%% ================================
%%% COMANDO: servicio_iniciado/1
%%% ================================
servicio_iniciado(TaxiId) when is_atom(TaxiId) ->
    central ! {servicio_iniciado, TaxiId},
    io:format("Taxi ~p notifica inicio de servicio.~n", [TaxiId]).

%%% ===================================
%%% COMANDO: servicio_completado/1
%%% ===================================
servicio_completado(TaxiId) when is_atom(TaxiId) ->
    central ! {servicio_completado, TaxiId},
    io:format("Taxi ~p notifica fin de servicio.~n", [TaxiId]).

%%% ======================
%%% LOOP DEL TAXI
%%% ======================
loop(TaxiId, Estado, Ubicacion, NodoCentral) ->
    {central, NodoCentral} ! {registrar_taxi, TaxiId, self(), Ubicacion},

    io:format("Taxi ~p registrado en ~p con estado ~p~n", [TaxiId, Ubicacion, Estado]),

    receive
        {consultar_estado, From} ->
            From ! {estado, Estado, Ubicacion},
            loop(TaxiId, Estado, Ubicacion, NodoCentral);

        stop ->
            io:format("Taxi ~p: proceso terminado.~n", [TaxiId]);

        _Otro ->
            io:format("Taxi ~p recibió un mensaje desconocido~n", [TaxiId]),
            loop(TaxiId, Estado, Ubicacion, NodoCentral)
    end.
