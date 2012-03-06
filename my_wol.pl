:- compile(war_of_life).

test_strategy(N, FirstPlayerStrategy, SecondPlayerStrategy) :-
 % Run the tests
 test(N, FirstPlayerStrategy, SecondPlayerStrategy, [0, 0, 0, 0, 250, 0, 0],
  [NumDraws, FPWins, SPWins, LongestGame, ShortestGame, TotalMoves, TotalTime]),
 % Print stats
 format('Number of draws: ~d~n', [NumDraws]),
 format('Number of wins for player 1 (blue): ~d~n', [FPWins]),
 format('Number of wins for player 2 (red): ~d~n', [SPWins]),
 format('Longest (non-exhaustive) game: ~d~n', [LongestGame]),
 format('Shortest game: ~d~n', [ShortestGame]),
 format('Average game length (including exhaustives): ~d~n', [TotalMoves / N]),
 format('Average game time: ~d~n', [TotalTime / N]).

% Base case
test(0, _, _, I, I) :- !.

% Recursive case for the test predicate
test(N, FPStrategy, SPStrategy,
  [INumDraws, IFPWins, ISPWins, ILongestGame, IShortestGame, ITotalMoves, ITotalTime],
  O) :-
 % Run and time the test
 statistics(runtime, [Start,_]),
 play(quiet, FPStrategy, SPStrategy, NumMoves, WinningPlayer),
 statistics(runtime, [End,_]),
 % Set output variables
 ((\+ (WinningPlayer = b ; WinningPlayer = r)) ->
   ONumDraws is INumDraws + 1 ;
   ONumDraws is INumDraws),
 (WinningPlayer = b ->
   OFPWins is IFPWins + 1 ;
   OFPWins is IFPWins),
 (WinningPlayer = r ->
   OSPWins is ISPWins + 1 ;
   OSPWins is ISPWins),
 ((NumMoves < 250 , NumMoves > ILongestGame) ->
   OLongestGame is NumMoves ;
   OLongestGame is ILongestGame),
 (NumMoves < IShortestGame ->
   OShortestGame is NumMoves ;
   OShortestGame is IShortestGame),
 OTotalMoves is ITotalMoves + NumMoves,
 OTotalTime is ITotalTime + End - Start,
 % Recurse
 NewN is N - 1,
 test(NewN, FPStrategy, SPStrategy,
  [ONumDraws, OFPWins, OSPWins, OLongestGame, OShortestGame, OTotalMoves, OTotalTime], O).


% Bloodlust strategy
bloodlust(b, [Blue, Red], [NewBlue, Red], Move) :-
 poss_moves(Blue, Red, PossMoves),
 bloodlust_best_move(Blue, Red, PossMoves, 100, _, Move),
 alter_board(Move, Blue, NewBlue).

bloodlust(r, [Blue, Red], [Blue, NewRed], Move) :-
 poss_moves(Red, Blue, PossMoves),
 bloodlust_best_move(Red, Blue, PossMoves, 100, _, Move),
 alter_board(Move, Red, NewRed).

bloodlust_best_move(_, _, [], _, Move, Move).

bloodlust_best_move(Alive, OtherPlayerAlive, [H|T], Score, Move, OutMove) :-
 alter_board(H, OtherPlayerAlive, NewOtherPlayerAlive),
 next_generation([Alive, NewOtherPlayerAlive], [_, NextOtherPlayerAlive]),
 length(NextOtherPlayerAlive, MoveScore),
 (MoveScore < Score ->
   (NewScore is MoveScore , NewMove = H) ;
   (NewScore is Score , NewMove = Move)),
 bloodlust_best_move(Alive, OtherPlayerAlive, T, NewScore, NewMove, OutMove).


% Self preservation strategy
self_preservation(b, [Blue, Red], [NewBlue, Red], Move) :-
 poss_moves(Blue, Red, PossMoves),
 self_preservation_best_move(Blue, Red, PossMoves, -100, _, Move),
 alter_board(Move, Blue, NewBlue).

self_preservation(r, [Blue, Red], [Blue, NewRed], Move) :-
 poss_moves(Red, Blue, PossMoves),
 self_preservation_best_move(Red, Blue, PossMoves, -100, _, Move),
 alter_board(Move, Red, NewRed).

self_preservation_best_move(_, _, [], _, Move, Move).

self_preservation_best_move(Alive, OtherPlayerAlive, [H|T], Score, Move, OutMove) :-
 alter_board(H, Alive, NewAlive),
 next_generation([NewAlive, OtherPlayerAlive], [NextAlive, _]),
 length(NextAlive, MoveScore),
 (MoveScore > Score ->
   (NewScore is MoveScore , NewMove = H) ;
   (NewScore is Score , NewMove = Move)),
 self_preservation_best_move(Alive, OtherPlayerAlive, T, NewScore, NewMove, OutMove).


% Land grab strategy
land_grab(b, [Blue, Red], [NewBlue, Red], Move) :-
 poss_moves(Blue, Red, PossMoves),
 land_grab_best_move(Blue, Red, PossMoves, -100, _, Move),
 alter_board(Move, Blue, NewBlue).

land_grab(r, [Blue, Red], [Blue, NewRed], Move) :-
 poss_moves(Red, Blue, PossMoves),
 land_grab_best_move(Red, Blue, PossMoves, -100, _, Move),
 alter_board(Move, Red, NewRed).

land_grab_best_move(_, _, [], _, Move, Move).

land_grab_best_move(Alive, OtherPlayerAlive, [H|T], Score, Move, OutMove) :-
 alter_board(H, Alive, NewAlive),
 next_generation([NewAlive, OtherPlayerAlive], [NextAlive, NextOtherPlayerAlive]),
 length(NextAlive, NextAliveLength),
 length(NextOtherPlayerAlive, NextOtherPlayerAliveLength),
 MoveScore is NextAliveLength - NextOtherPlayerAliveLength,
 (MoveScore > Score ->
   (NewScore is MoveScore , NewMove = H) ;
   (NewScore is Score , NewMove = Move)),
 land_grab_best_move(Alive, OtherPlayerAlive, T, NewScore, NewMove, OutMove).


% Minimax strategy
minimax(b, [Blue, Red], [NewBlue, Red], Move) :-
 poss_moves(Blue, Red, PossMoves),
 minimax_best_move(r, Blue, Red, PossMoves, -100, _, Move),
 alter_board(Move, Blue, NewBlue).

minimax(r, [Blue, Red], [Blue, NewRed], Move) :-
 poss_moves(Red, Blue, PossMoves),
 minimax_best_move(b, Red, Blue, PossMoves, -100, _, Move),
 alter_board(Move, Red, NewRed).

minimax_best_move(_, _, _, [], _, Move, Move).

minimax_best_move(OtherPlayer, Alive, OtherPlayerAlive, [H|T], Score, Move, OutMove) :-
 alter_board(H, Alive, NewAlive),
 next_generation([NewAlive, OtherPlayerAlive], Next),
 land_grab(OtherPlayer, Next, AfterLandGrab, _),
 next_generation(AfterLandGrab, [NextAlive, NextOtherPlayerAlive]),
 length(NextAlive, NextAliveLength),
 length(NextOtherPlayerAlive, NextOtherPlayerAliveLength),
 MoveScore is NextAliveLength - NextOtherPlayerAliveLength,
 (MoveScore > Score ->
   (NewScore is MoveScore , NewMove = H) ;
   (NewScore is Score , NewMove = Move)),
 minimax_best_move(OtherPlayer, Alive, OtherPlayerAlive, T, NewScore, NewMove, OutMove).


% Helper predicate for generating possible moves
poss_moves(Alive, OtherPlayerAlive, PossMoves) :-
 findall(
   [A,B,MA,MB],
   (member([A,B], Alive), neighbour_position(A,B,[MA,MB]),
	 \+member([MA,MB],Alive), \+member([MA,MB],OtherPlayerAlive)),
   PossMoves).
