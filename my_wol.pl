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
 bloodlust_best_move(Blue, Red, PossMoves, 100, Move),
 alter_board(Move, Blue, NewBlue).

bloodlust(r, [Blue, Red], [Blue, NewRed], Move) :-
 poss_moves(Red, Blue, PossMoves),
 bloodlust_best_move(Red, Blue, PossMoves, 100, Move),
 alter_board(Move, Red, NewRed).

bloodlust_best_move(_, _, [Move|[]], _, Move).

bloodlust_best_move(Alive, OtherPlayerAlive, [H|T], Score, Move) :-
 alter_board(H, OtherPlayerAlive, NewOtherPlayerAlive),
 length(NewOtherPlayerAlive, MoveScore),
 (MoveScore < Score ->
   (NewScore is MoveScore , NewMove = H) ;
   (NewScore is Score , NewMove = Move)),
 best_move(Alive, OtherPlayerAlive, T, NewScore, Move).


% Self preservation strategy
self_preservation(b, [Blue, Red], [NewBlue, Red], Move) :-
 poss_moves(Blue, Red, PossMoves),
 self_preservation_best_move(Blue, Red, PossMoves, 0, Move),
 alter_board(Move, Blue, NewBlue).

self_preservation(r, [Blue, Red], [Blue, NewRed], Move) :-
 poss_moves(Red, Blue, PossMoves),
 self_preservation_best_move(Red, Blue, PossMoves, 0, Move),
 alter_board(Move, Red, NewRed).

self_preservation_best_move(_, _, [Move|[]], _, Move).

self_preservation_best_move(Alive, OtherPlayerAlive, [H|T], Score, Move) :-
 alter_board(H, Alive, NewAlive),
 length(NewAlive, MoveScore),
 (MoveScore > Score ->
   (NewScore is MoveScore , NewMove = H) ;
   (NewScore is Score , NewMove = Move)),
 best_move(Alive, OtherPlayerAlive, T, NewScore, Move).


% Land grab strategy
land_grab(b, [Blue, Red], [NewBlue, Red], Move) :-
 poss_moves(Blue, Red, PossMoves),
 land_grab_best_move(Blue, Red, PossMoves, 0, Move),
 alter_board(Move, Blue, NewBlue).

land_grab(r, [Blue, Red], [Blue, NewRed], Move) :-
 poss_moves(Red, Blue, PossMoves),
 land_grab_best_move(Red, Blue, PossMoves, 0, Move),
 alter_board(Move, Red, NewRed).

land_grab_best_move(_, _, [Move|[]], _, Move).

land_grab_best_move(Alive, OtherPlayerAlive, [H|T], Score, Move) :-
 alter_board(H, Alive, NewAlive),
 length(NewAlive, NewAliveLength),
 length(OtherPlayerAlive, OtherPlayerAliveLength),
 MoveScore is NewAliveLength - OtherPlayerAliveLength,
 (MoveScore > Score ->
   (NewScore is MoveScore , NewMove = H) ;
   (NewScore is Score , NewMove = Move)),
 best_move(Alive, OtherPlayerAlive, T, NewScore, Move).


% Helper predicate for generating possible moves
poss_moves(Alive, OtherPlayerAlive, PossMoves) :-
 findall(
   [A,B,MA,MB],
   (member([A,B], Alive), neighbour_position(A,B,[MA,MB]),
	 \+member([MA,MB],Alive), \+member([MA,MB],OtherPlayerAlive)),
   PossMoves).





minimax(PlayerColour, CurrentBoardState, [Blue, Red], Move) :-
 board_after_move(PlayerColour, CurrentBoardState, [Blue, Red], Move),
 write('.'),
 (PlayerColour == 'r' -> (
     land_grab('b', [Blue, Red], [Blue2, Red2], Move3),
 \+ (board_after_move(PlayerColour, CurrentBoardState, IntermediateBoardState, Move2),
     land_grab('b', IntermediateBoardState, [Blue3, Red3], Move4),
     length(Red2, R2L), length(Red3, R3L), length(Blue2, B2L), length(Blue3, B3L), 
     (B3L - R3L) < (B2L - R2L)));
    (land_grab('r', [Blue, Red], [Blue2, Red2], Move3),
 \+ (board_after_move(PlayerColour, CurrentBoardState, IntermediateBoardState, Move2),
     land_grab('r', IntermediateBoardState, [Blue3, Red3], Move4),
     length(Red2, R2L), length(Red3, R3L), length(Blue2, B2L), length(Blue3, B3L), 
     (R3L - B3L) < (R2L - B2L)))).

board_after_move(PlayerColour, [Blue, Red], NewBoardState, Move) :-
 (PlayerColour == 'r' -> (possible_move(Red, Blue, Move),
                          alter_board(Move, Blue, NewBlue),
                          next_generation([NewBlue, Red], NewBoardState));
                          (possible_move(Blue, Red, Move)),
                          alter_board(Move, Red,  NewRed),
                          next_generation([Blue, NewRed], NewBoardState)).

possible_move(Alive, OtherPlayerAlive, Move) :-    
 findall([A,B,MA,MB],(member([A,B], Alive),
           neighbour_position(A,B,[MA,MB]),
           \+member([MA,MB],Alive),
           \+member([MA,MB],OtherPlayerAlive)),
       PossMoves),
 member(Move, PossMoves).
