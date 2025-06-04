%%% Archivo: taxi.erl
%%% Autoras: TuNombre y Matrícula aquí

-module(taxi).
-export([
    registra_taxi/2,
    loop/3
    consultar_estado/1
]).

%%% ===========================
%%% COMANDO: registra_taxi/2
%%% ===========================
registra_taxi(TaxiId, UbicacionInicial) when is_atom(TaxiId), is_atom(UbicacionInicial) ->
    spawn(?MODULE, loop, [TaxiId, disponible, UbicacionInicial]).

%%% ===============================
%%% COMANDO: consultar_estado/1
%%% ===============================
consultar_estado(TaxiId) when is_atom(TaxiId) ->
    central ! {solicitar_pid, TaxiId, self()},
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

%%% ======================
%%% LOOP DEL TAXI
%%% ======================
loop(TaxiId, Estado, Ubicacion) ->
    %% Al iniciar, se registra con la central
    central ! {registrar_taxi, TaxiId, self(), Ubicacion},

    io:format("Taxi ~p registrado en ~p con estado ~p~n", [TaxiId, Ubicacion, Estado]),

    receive
        %% Aquí luego se reciben comandos como servicio_asignado, etc.
            {consultar_estado, From} ->
            From ! {estado, Estado, Ubicacion},
            loop(TaxiId, Estado, Ubicacion);

        stop ->
            io:format("Taxi ~p: proceso terminado.~n", [TaxiId]);

        _Otro ->
            io:format("Taxi ~p recibió un mensaje desconocido~n", [TaxiId]),
            loop(TaxiId, Estado, Ubicacion)
        
    end.
