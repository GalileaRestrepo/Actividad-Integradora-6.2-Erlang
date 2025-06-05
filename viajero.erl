%%% Archivo: viajero.erl
%%% Autoras: Anna Galilea Restrepo Martínez A01178273 y Regina Romero Alvarado A00840840

-module(viajero).
-export([
    solicitar_taxi/4, 
    cancelar_taxi/2, 
    loop/2
]).

%%% =============================
%%% Zonas válidas para origen y destino
%%% =============================
zonas_validas() -> [aeropuerto, zona_norte, zona_sur, zona_centro].

%%% =============================
%%% COMANDO: solicitar_taxi/4
%%% =============================
%%% Como estamos trabajando con nodos distribuidos,
%%% pasamos explícitamente el nodo donde se ejecuta la central (NodoCentral).
%%% Así el código no depende del nombre local de la máquina.

solicitar_taxi(Nombre, Origen, Destino, NodoCentral) 
when is_atom(Nombre), is_atom(Origen), is_atom(Destino), is_atom(NodoCentral) ->
    case lists:member(Origen, zonas_validas()) andalso lists:member(Destino, zonas_validas()) of
        true -> spawn(?MODULE, loop, [{Nombre, Origen, Destino}, NodoCentral]);
        false -> io:format("Zona inválida: origen ~p o destino ~p~n", [Origen, Destino])
    end.

%%% ==========================
%%% COMANDO: cancelar_taxi/2
%%% ==========================
cancelar_taxi(Nombre, NodoCentral) when is_atom(Nombre), is_atom(NodoCentral) ->
    {central, NodoCentral} ! {cancelar_taxi, Nombre, self()},
    receive
        {cancelado_exitoso, Nombre} ->
            io:format("Se canceló correctamente la solicitud de ~p~n", [Nombre]);
        {cancelado_fallido, Nombre} ->
            io:format("No se encontró al viajero ~p para cancelar~n", [Nombre])
    after 2000 ->
        io:format("Timeout: No se recibió respuesta al cancelar ~p~n", [Nombre])
    end.


%%% ======================
%%% LOOP DEL VIAJERO
%%% ======================
loop({Nombre, Origen, Destino}, NodoCentral) ->
    io:format("Manda: ~p solicita taxi desde ~p hacia ~p~n", [Nombre, Origen, Destino]),

    %% Enviar solicitud a la central
    {central, NodoCentral} ! {solicita_taxi, self(), Nombre, Origen, Destino},

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
