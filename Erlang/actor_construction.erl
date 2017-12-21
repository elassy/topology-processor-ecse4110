-module(actor_construction).
-export([run/2]).

run(SwitchTable, Configuration) ->
	Data = [SwitchTable,Configuration,[]],

	% Explicitely spawn a supervisor actor, to execute the function "supervisor"
	% located in module "actor_construction" (this module) with parameter "Data"
	Supervisor = spawn(actor_construction, supervisor, Data),

	% Send a message to the supervisor to spawn the actors
	Supervisor ! {self(), spawn_actors},

	% Send another message to the supervisor instructing it to investigate changes
	% From here, the supervisor starts its simulation, and does not report back
	% to the main process untill the islanding and energized checks are complete
	Supervisor ! {self(), start_sim},
	receive
		{From, end} ->
			io:format("Supervisor actor has signaled end of topology processing")
	end.


supervisor(Data) ->
	receive
		{From, spawn_actors} ->
			% Call the spawn_actors/1 function to create a list of actors
			ActorsSub = spawn_actors(lists:nth(1,Data), sub),
			ActorsCircuits = spawn_actors(lists:nth(2,Data), cir),
			Actors = ActorsSub ++ ActorsCircuits,

			% Recursively call supervisor with the new actors list in Data
			supervisor([lists:nth(1,Data),lists:nth(2,Data),Actors]);

		{From, start_sim} ->
			% Firstly, we investigate changes. Next, we must perform analysis on
			% substation merging and splitting, circuit conn. analysis, and 
			% islanding and energized checks...

			% This example illustrates the framework of laying out the actor
			% management system. 

		_ ->
			supervisor(Data)
	end.


% spawn_actors/2 creates a list of actors (processes) from a list of data about 
% the actors
spawn_actors([], _) -> [];				% 'Base Case' for pattern matching
spawn_actors([H|T], sub) ->
	{_,_,_,_,_,_,_,_,_,_,_} = H,	    % Extract info via pattern matching
	P = spawn(actor,actor,[H]),         % Spawn first actor, using the head H
	[P|spawn_actors(T, sub)].           % Recursively construct list of actors
spawn_actors([H|T], cir) ->
	{_,_,_,_,_,_,_,_,_,_,_} = H,	    % Extract info via pattern matching
	P = spawn(actor,actor,[H]),         % Spawn first actor, using the head H
	[P|spawn_actors(T, cir)].           % Recursively construct list of actors
