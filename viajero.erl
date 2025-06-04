%%% Archivo: viajero.erl
%%% Autoras: Anna Galilea Restrepo Martínez A01178273 y Regina Romero Alvarado A00840840

-module(viajero).
-export([
    solicitar_taxi/3, 
    cancelar_taxi/1, 
    loop/1
]).

%%% =============================
%%% COMANDO: solicitar_taxi/3
%%% =============================
solicitar_taxi(Nombre, aeropuerto, Destino) when is_atom(Nombre), is_atom(Destino) ->
    spawn(?MODULE, loop, [{Nombre, aeropuerto, Destino}]);

solicitar_taxi(_, Origen, _) ->
    io:format("Error: Solo se permiten solicitudes desde el aeropuerto. Origen inválido: ~p~n", [Origen]).

%%% ==========================
%%% COMANDO: cancelar_taxi/1
%%% ==========================
cancelar_taxi(Nombre) when is_atom(Nombre) ->
    central ! {cancelar_taxi, Nombre}.

%%% ======================
%%% LOOP DEL VIAJERO
%%% ======================
loop({Nombre, Origen, Destino}) ->
    io:format("Manda: ~p solicita taxi desde ~p hacia ~p~n", [Nombre, Origen, Destino]),

    %% Enviar solicitud a la central
    central ! {solicita_taxi, self(), Nombre, Origen, Destino},

    receive
        {taxi_asignado, TaxiId, ViajeId} ->
            io:format("Recibe: Taxi ~p asignado a ~p (Viaje ~p)~n", [TaxiId, Nombre, ViajeId]),
            esperar_terminar(Nombre);

        {no_disponible} ->
            io:format("Recibe: No hay taxis disponibles para ~p. Terminando proceso.~n", [Nombre]);

        cancelado ->
            io:format("Recibe: Solicitud de taxi de ~p fue cancelada. Cerrando proceso.~n", [Nombre])
    end.

%%% ============================
%%% ESPERAR HASTA TERMINAR VIAJE
%%% ============================
esperar_terminar(Nombre) ->
    receive
        terminar ->
            io:format("Recibe: ~p ha llegado a su destino. Finalizando proceso.~n", [Nombre])
    end.
