-module(change_investigator).
-export([driver/0, change_investigator/2, sub_builder/2]).


sub_builder([],[]) -> [];
sub_builder([H1|T1],[H2|T2]) ->
	OldStatus = lists:nth(5,H1),
	NewStatus = lists:nth(5,H2),
	Type = lists:nth(4,H1),

	case OldStatus == NewStatus of
		false when Type == 3 -> [lists:nth(8,H1) | sub_builder(T1,T2)];
		true -> sub_builder(T1,T2)
	end.


change_investigator(S,S) -> [];
change_investigator(O,N) ->
	sub_builder(O,N).


driver() ->
	Old = [[1,1,1,3,0,-1,-4,1],[2,1,1,3,1,-1,-2,1],[3,2,1,3,0,-2,-3,2],[4,2,1,3,1,-4,-5,2]],
	New = [[1,1,1,3,1,-1,-4,1],[2,1,1,3,1,-1,-2,1],[3,2,1,3,0,-2,-3,2],[4,2,1,3,1,-4,-5,2]],
	change_investigator(Old, New).