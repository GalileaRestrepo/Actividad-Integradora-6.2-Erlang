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
% Devuelve la lista de zonas válidas para los viajes
zonas_validas() -> [aeropuerto, zona_norte, zona_sur, zona_centro].

%%% =============================
%%% COMANDO: solicitar_taxi/4
%%% =============================
%%% Como estamos trabajando con nodos distribuidos,
%%% pasamos explícitamente el nodo donde se ejecuta la central (NodoCentral).
%%% Así el código no depende del nombre local de la máquina.

% Solicita un taxi para un viajero, validando zonas y lanzando el proceso viajero
solicitar_taxi(Nombre, Origen, Destino, NodoCentral) 
when is_atom(Nombre), is_atom(Origen), is_atom(Destino), is_atom(NodoCentral) ->
    case lists:member(Origen, zonas_validas()) andalso lists:member(Destino, zonas_validas()) of
        true -> 
            % Si las zonas son válidas, inicia el proceso viajero
            spawn(?MODULE, loop, [{Nombre, Origen, Destino}, NodoCentral]);
        false -> 
            % Si alguna zona es inválida, muestra mensaje de error
            io:format("Zona inválida: origen ~p o destino ~p~n", [Origen, Destino])
    end.

%%% ==========================
%%% COMANDO: cancelar_taxi/2
%%% ==========================
% Solicita a la central cancelar la solicitud de taxi de un viajero
cancelar_taxi(Nombre, NodoCentral) when is_atom(Nombre), is_atom(NodoCentral) ->
    {central, NodoCentral} ! {cancelar_taxi, Nombre, self()},
    receive
        {cancelado_exitoso, Nombre} ->
            % Cancelación exitosa
            io:format("Se canceló correctamente la solicitud de ~p~n", [Nombre]);
        {cancelado_fallido, Nombre} ->
            % No se encontró al viajero para cancelar
            io:format("No se encontró al viajero ~p para cancelar~n", [Nombre])
    after 2000 ->
        % Timeout esperando respuesta de la central
        io:format("Timeout: No se recibió respuesta al cancelar ~p~n", [Nombre])
    end.

%%% ======================
%%% LOOP DEL VIAJERO
%%% ======================
% Proceso principal del viajero: solicita taxi y espera respuesta de la central
loop({Nombre, Origen, Destino}, NodoCentral) ->
    io:format("Manda: ~p solicita taxi desde ~p hacia ~p~n", [Nombre, Origen, Destino]),

    %% Enviar solicitud a la central
    {central, NodoCentral} ! {solicita_taxi, self(), Nombre, Origen, Destino},

    receive
        {taxi_asignado, TaxiId, ViajeId} ->
            % Si se asigna un taxi, lo informa y espera a terminar el viaje
            io:format("Recibe: Taxi ~p asignado a ~p (Viaje ~p)~n", [TaxiId, Nombre, ViajeId]),
            esperar_terminar(Nombre);

        {no_disponible} ->
            % Si no hay taxis disponibles, termina el proceso
            io:format("Recibe: No hay taxis disponibles para ~p. Terminando proceso.~n", [Nombre]);

        cancelado ->
            % Si la solicitud fue cancelada, termina el proceso
            io:format("Recibe: Solicitud de taxi de ~p fue cancelada. Cerrando proceso.~n", [Nombre])
    end.

%%% ============================
%%% ESPERAR HASTA TERMINAR VIAJE
%%% ============================
% Espera el mensaje de 'terminar' para finalizar el proceso del viajero
esperar_terminar(Nombre) ->
    receive
        terminar ->
            io:format("Recibe: ~p ha llegado a su destino. Finalizando proceso.~n", [Nombre])
    end.
