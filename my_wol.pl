:- compile(war_of_life).

test_strategy(N, FirstPlayerStrategy, SecondPlayerStrategy) :-
 % Run the tests
 test(N, FirstPlayerStrategy, SecondPlayerStrategy, NumDraws, FPWins, SPWins,
     LongestGame, ShortestGame, TotalMoves, TotalTime),
 % Print stats
 format('Number of draws: ~d~n', [NumDraws]),
 format('Number of wins for player 1 (blue): ~d~n', [FPWins]),
 format('Number of wins for player 2 (red): ~d~n', [SPWins]),
 format('Longest (non-exhaustive) game: ~d~n', [LongestGame]),
 format('Shortest game: ~d~n', [ShortestGame]),
 format('Average game length (including exhaustives): ~d~n', [TotalMoves / N]),
 format('Average game time: ~d~n', [TotalTime / N]).

% Base case for the test predicate
test(1, FPStrategy, SPStrategy, NumDraws, FPWins, SPWins, LongestGame,
    ShortestGame, TotalMoves, TotalTime) :-
 !,
 % Run and time the test
 statistics(walltime, [Start,_]),
 play(quiet, FPStrategy, SPStrategy, NumMoves, WinningPlayer),
 statistics(walltime, [End,_]),
 % Set output variables
 (member(WinningPlayer, ['draw', 'exhaust', 'stalemate']) ->
   NumDraws is 1 ;
   NumDraws is 0),
 (WinningPlayer == 'b' -> FPWins is 1 ; FPWins is 0),
 (WinningPlayer == 'r' -> SPWins is 1 ; SPWins is 0),
 (WinningPlayer == 'exhaust' -> LongestGame is 0 ; LongestGame is NumMoves),
 ShortestGame is NumMoves,
 TotalMoves is NumMoves,
 TotalTime is End - Start.

% Recursive case for the test predicate
test(N, FPStrategy, SPStrategy, NumDraws, FPWins, SPWins, LongestGame,
    ShortestGame, TotalMoves, TotalTime) :-
 % Recurse
 NewN is N - 1,
 test(NewN, FPStrategy, SPStrategy, RNumDraws, RFPWins, RSPWins, RLongestGame,
     RShortestGame, RTotalMoves, RTotalTime),
 % Run and time the test
 statistics(walltime, [Start,_]),
 play(quiet, FPStrategy, SPStrategy, NumMoves, WinningPlayer),
 statistics(walltime, [End,_]),
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
     length(Red2, R2L), length(Red, RL), R2L > RL));
    (
 \+ (board_after_move(PlayerColour, CurrentBoardState, [Blue2, Red2], Move2),
     length(Blue2, B2L), length(Blue, BL), B2L > BL))).

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
 write('.'),
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
