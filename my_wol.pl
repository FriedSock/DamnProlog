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
 (member(WinningPlayer, ['draw', 'exhaust', 'stalemate']) ->
   NumDraws is RNumDraws + 1 ;
   NumDraws is RNumDraws),
 (WinningPlayer == 'b' -> FPWins is RFPWins + 1 ; FPWins is RFPWins),
 (WinningPlayer == 'r' -> SPWins is RSPWins + 1 ; SPWins is RSPWins),
 (NumMoves > RLongestGame ->
   (WinningPlayer == 'exhaust' ->
     LongestGame is RLongestGame ;
     LongestGame is NumMoves) ;
   LongestGame is RLongestGame),
 (NumMoves < RShortestGame ->
   ShortestGame is NumMoves ;
   ShortestGame is RShortestGame),
 TotalMoves is RTotalMoves + NumMoves,
 TotalTime is RTotalTime + End - Start.

bloodlust(PlayerColour, CurrentBoardState, [Blue, Red], Move) :-
 board_after_move(PlayerColour, CurrentBoardState, [Blue, Red], Move),
 (PlayerColour == 'r' -> (
 \+ (board_after_move(PlayerColour, CurrentBoardState, [Blue2, Red2], Move2),
     length(Blue2, B2L), length(Blue, BL), B2L < BL));
    (
 \+ (board_after_move(PlayerColour, CurrentBoardState, [Blue2, Red2], Move2),
     length(Red2, R2L), length(Red, RL), R2L < RL))).

self_preservation(PlayerColour, CurrentBoardState, [Blue, Red], Move) :-
 board_after_move(PlayerColour, CurrentBoardState, [Blue, Red], Move),
 (PlayerColour == 'r' -> (
 \+ (board_after_move(PlayerColour, CurrentBoardState, [Blue2, Red2], Move2),
     length(Blue2, B2L), length(Blue, BL), B2L > BL));
    (
 \+ (board_after_move(PlayerColour, CurrentBoardState, [Blue2, Red2], Move2),
     length(Red2, R2L), length(Red, RL), R2L > RL))).

land_grab(PlayerColour, CurrentBoardState, [Blue, Red], Move) :-
 board_after_move(PlayerColour, CurrentBoardState, [Blue, Red], Move),
 (PlayerColour == 'r' -> (
 \+ (board_after_move(PlayerColour, CurrentBoardState, [Blue2, Red2], Move2),
     length(Red2, R2L), length(Red, RL), length(Blue2, B2L), length(Blue, BL), 
     (R2L - B2L) > (RL - BL)));
    (
 \+ (board_after_move(PlayerColour, CurrentBoardState, [Blue2, Red2], Move2),
     length(Red2, R2L), length(Red, RL), length(Blue2, B2L), length(Blue, BL), 
     (B2L - R2L) > (BL - RL)))).

minimax(PlayerColour, CurrentBoardState, [Blue, Red], Move) :-
 board_after_move(PlayerColour, CurrentBoardState, [Blue, Red], Move),
 (PlayerColour == 'r' -> (
 \+ (board_after_move(PlayerColour, CurrentBoardState, IntermediateBoardState, Move2),
     board_after_move('b', IntermediateBoardState, [Blue2, Red2], Move3),
     length(Red2, R2L), length(Red, RL), length(Blue2, B2L), length(Blue, BL), 
     (B2L - R2L) < (BL - RL)));
    (
 \+ (board_after_move(PlayerColour, CurrentBoardState, IntermediateBoardState, Move2),
     board_after_move('r', IntermediateBoardState, [Blue2, Red2], Move3),
     length(Red2, R2L), length(Red, RL), length(Blue2, B2L), length(Blue, BL), 
     (R2L - B2L) < (RL - BL)))).

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
 bloodlust_best_move(b, Blue, Red, PossMoves, 100, _, Move),
 alter_board(Move, Blue, NewBlue).

bloodlust(r, [Blue, Red], [Blue, NewRed], Move) :-
 poss_moves(Red, Blue, PossMoves),
 bloodlust_best_move(r, Red, Blue, PossMoves, 100, _, Move),
 alter_board(Move, Red, NewRed).

bloodlust_best_move(_, _, _, [], _, Move, Move).

bloodlust_best_move(Player, Alive, OtherPlayerAlive, [H|T], Score, Move, OutMove) :-
 alter_board(H, OtherPlayerAlive, NewOtherPlayerAlive),
 (Player = r ->
   next_generation([NewOtherPlayerAlive, Alive], [NextOtherPlayerAlive, _]) ;
   next_generation([Alive, NewOtherPlayerAlive], [_, NextOtherPlayerAlive])),
 length(NextOtherPlayerAlive, MoveScore),
 (MoveScore < Score ->
   (NewScore is MoveScore , NewMove = H) ;
   (NewScore is Score , NewMove = Move)),
 bloodlust_best_move(Player, Alive, OtherPlayerAlive, T, NewScore, NewMove, OutMove).


% Self preservation strategy
self_preservation(b, [Blue, Red], [NewBlue, Red], Move) :-
 poss_moves(Blue, Red, PossMoves),
 self_preservation_best_move(b, Blue, Red, PossMoves, -100, _, Move),
 alter_board(Move, Blue, NewBlue).

self_preservation(r, [Blue, Red], [Blue, NewRed], Move) :-
 poss_moves(Red, Blue, PossMoves),
 self_preservation_best_move(r, Red, Blue, PossMoves, -100, _, Move),
 alter_board(Move, Red, NewRed).

self_preservation_best_move(_, _, _, [], _, Move, Move).

self_preservation_best_move(Player, Alive, OtherPlayerAlive, [H|T], Score, Move, OutMove) :-
 alter_board(H, Alive, NewAlive),
 (Player = r ->
   next_generation([OtherPlayerAlive, NewAlive], [_, NextAlive]) ;
   next_generation([NewAlive, OtherPlayerAlive], [NextAlive, _])),
 length(NextAlive, MoveScore),
 (MoveScore > Score ->
   (NewScore is MoveScore , NewMove = H) ;
   (NewScore is Score , NewMove = Move)),
 self_preservation_best_move(Player, Alive, OtherPlayerAlive, T, NewScore, NewMove, OutMove).


% Land grab strategy
land_grab(b, [Blue, Red], [NewBlue, Red], Move) :-
 poss_moves(Blue, Red, PossMoves),
 land_grab_best_move(b, Blue, Red, PossMoves, -100, _, Move),
 alter_board(Move, Blue, NewBlue).

land_grab(r, [Blue, Red], [Blue, NewRed], Move) :-
 poss_moves(Red, Blue, PossMoves),
 land_grab_best_move(r, Red, Blue, PossMoves, -100, _, Move),
 alter_board(Move, Red, NewRed).

land_grab_best_move(_, _, _, [], _, Move, Move).

land_grab_best_move(Player, Alive, OtherPlayerAlive, [H|T], Score, Move, OutMove) :-
 alter_board(H, Alive, NewAlive),
 (Player = r ->
   next_generation([OtherPlayerAlive, NewAlive], [NextOtherPlayerAlive, NextAlive]) ;
   next_generation([NewAlive, OtherPlayerAlive], [NextAlive, NextOtherPlayerAlive])),
 length(NextAlive, NextAliveLength),
 length(NextOtherPlayerAlive, NextOtherPlayerAliveLength),
 MoveScore is NextAliveLength - NextOtherPlayerAliveLength,
 (MoveScore > Score ->
   (NewScore is MoveScore , NewMove = H) ;
   (NewScore is Score , NewMove = Move)),
 land_grab_best_move(Player, Alive, OtherPlayerAlive, T, NewScore, NewMove, OutMove).


% Minimax strategy
minimax(b, [Blue, Red], [NewBlue, Red], Move) :-
 poss_moves(Blue, Red, PossMoves),
 minimax_best_move(b, r, Blue, Red, PossMoves, -100, _, Move),
 alter_board(Move, Blue, NewBlue).

minimax(r, [Blue, Red], [Blue, NewRed], Move) :-
 poss_moves(Red, Blue, PossMoves),
 minimax_best_move(r, b, Red, Blue, PossMoves, -100, _, Move),
 alter_board(Move, Red, NewRed).

minimax_best_move(_, _, _, _, [], _, Move, Move).

minimax_best_move(Player, OtherPlayer, Alive, OtherPlayerAlive, [H|T], Score, Move, OutMove) :-
 alter_board(H, Alive, NewAlive),
 (Player = r ->
   next_generation([OtherPlayerAlive, NewAlive], Next) ;
   next_generation([NewAlive, OtherPlayerAlive], Next)),
 land_grab(OtherPlayer, Next, AfterLandGrab, _),
 (Player = r ->
   next_generation(AfterLandGrab, [NextOtherPlayerAlive, NextAlive]) ;
   next_generation(AfterLandGrab, [NextAlive, NextOtherPlayerAlive])),
 length(NextAlive, NextAliveLength),
 length(NextOtherPlayerAlive, NextOtherPlayerAliveLength),
 MoveScore is NextAliveLength - NextOtherPlayerAliveLength,
 (MoveScore > Score ->
   (NewScore is MoveScore , NewMove = H) ;
   (NewScore is Score , NewMove = Move)),
 minimax_best_move(Player, OtherPlayer, Alive, OtherPlayerAlive, T, NewScore, NewMove, OutMove).


% Helper predicate for generating possible moves
poss_moves(Alive, OtherPlayerAlive, PossMoves) :-
 findall(
   [A,B,MA,MB],
   (member([A,B], Alive), neighbour_position(A,B,[MA,MB]),
	 \+member([MA,MB],Alive), \+member([MA,MB],OtherPlayerAlive)),
   PossMoves).
>>>>>>> a8e9cfa35f9dfe3bdba43ebf8c53fb6a2f303209
