%%% Archivo: taxi.erl
%%% Autoras: Anna Galilea Restrepo Martínez A01178273 y Regina Romero Alvarado A00840840

-module(taxi).

% Exporta las funciones públicas del módulo
-export([
    registra_taxi/3,
    loop/4,
    consultar_estado/2,
    elimina_taxi/2,
    servicio_iniciado/2,
    servicio_completado/2
]).

%%% ===========================
%%% COMANDO: registra_taxi/3
%%% ===========================
%%% Este comando se usa para registrar un taxi en la central.
%%% Como estamos trabajando con nodos distribuidos, se pasa explícitamente
%%% el nodo donde está corriendo la central (ej. 'central@laptop-de-juan').
%%% Así evitamos depender de un hostname fijo y el código es portable para otros usuarios.

% Crea un nuevo proceso taxi con su estado inicial y lo registra en la central
registra_taxi(TaxiId, UbicacionInicial, NodoCentral) 
when is_atom(TaxiId), is_atom(UbicacionInicial), is_atom(NodoCentral) ->
    spawn(?MODULE, loop, [TaxiId, disponible, UbicacionInicial, NodoCentral]).

%%% ===============================
%%% COMANDO: consultar_estado/2
%%% ===============================
% Consulta el estado actual de un taxi registrado en la central
consultar_estado(TaxiId, NodoCentral) when is_atom(TaxiId), is_atom(NodoCentral) ->
    % Solicita el PID del taxi a la central
    {central, NodoCentral} ! {solicitar_pid, TaxiId, self()},
    receive
        {pid_taxi, undefined} ->
            % Si el taxi no está registrado
            io:format("Taxi ~p no está registrado.~n", [TaxiId]);
        {pid_taxi, Pid} ->
            % Si se encuentra el PID, consulta su estado
            Pid ! {consultar_estado, self()},
            receive
                {estado, Estado, Ubic} ->
                    io:format("Taxi ~p | Estado: ~p | Ubicación: ~p~n", [TaxiId, Estado, Ubic])
            after 2000 ->
                % Si el taxi no responde en 2 segundos
                io:format("No se recibió respuesta del taxi ~p~n", [TaxiId])
            end
    after 2000 ->
        % Si la central no responde en 2 segundos
        io:format("No se encontró el taxi ~p en central~n", [TaxiId])
    end.

%%% =============================
%%% COMANDO: elimina_taxi/2
%%% =============================
% Solicita a la central eliminar un taxi específico
elimina_taxi(TaxiId, NodoCentral) when is_atom(TaxiId), is_atom(NodoCentral) ->
    {central, NodoCentral} ! {eliminar_taxi, TaxiId, self()},
    receive
        {eliminado, TaxiId} ->
            % Eliminación exitosa
            io:format("Taxi ~p eliminado correctamente.~n", [TaxiId]);
        {no_se_puede_eliminar, TaxiId, Motivo} ->
            % No se pudo eliminar, se muestra el motivo
            io:format("No se pudo eliminar taxi ~p: ~s~n", [TaxiId, Motivo]);
        _ ->
            % Respuesta inesperada
            io:format("Respuesta inesperada al intentar eliminar taxi ~p~n", [TaxiId])
    after 2000 ->
        % Timeout esperando respuesta de la central
        io:format("Timeout al intentar eliminar taxi ~p~n", [TaxiId])
    end.

%%% ================================
%%% COMANDO: servicio_iniciado/2
%%% ================================
% Notifica a la central que el taxi ha iniciado un servicio
servicio_iniciado(TaxiId, NodoCentral) 
when is_atom(TaxiId), is_atom(NodoCentral) ->
    {central, NodoCentral} ! {servicio_iniciado, TaxiId},
    io:format("Taxi ~p notifica inicio de servicio.~n", [TaxiId]).

%%% ===================================
%%% COMANDO: servicio_completado/2
%%% ===================================
% Notifica a la central que el taxi ha completado un servicio
servicio_completado(TaxiId, NodoCentral) 
when is_atom(TaxiId), is_atom(NodoCentral) ->
    {central, NodoCentral} ! {servicio_completado, TaxiId},
    io:format("Taxi ~p notifica fin de servicio.~n", [TaxiId]).

%%% ======================
%%% LOOP DEL TAXI
%%% ======================
% Proceso principal del taxi: espera mensajes y responde según el caso
loop(TaxiId, Estado, Ubicacion, NodoCentral) ->
    % Al iniciar, se registra en la central
    {central, NodoCentral} ! {registrar_taxi, TaxiId, self(), Ubicacion},

    io:format("Taxi ~p registrado en ~p con estado ~p~n", [TaxiId, Ubicacion, Estado]),

    receive
        % Responde a la consulta de estado enviando su estado y ubicación actual
        {consultar_estado, From} ->
            From ! {estado, Estado, Ubicacion},
            loop(TaxiId, Estado, Ubicacion, NodoCentral);

        % Si recibe el mensaje de stop, termina el proceso
        stop ->
            io:format("Taxi ~p: proceso terminado.~n", [TaxiId]);

        % Para cualquier otro mensaje, lo ignora pero lo reporta
        _Otro ->
            io:format("Taxi ~p recibió un mensaje desconocido~n", [TaxiId]),
            loop(TaxiId, Estado, Ubicacion, NodoCentral)
    end.
