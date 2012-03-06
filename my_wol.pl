test_strategy(N, FirstPlayerStrategy, SecondPlayerStrategy) :-
 test(N, FirstPlayerStrategy, SecondPlayerStrategy, NumDraws, FPWins, SPWins,
     LongestGame, ShortestGame, TotalMoves, TotalTime),
 format('Number of draws: ~d~n', [NumDraws]),
 format('Number of wins for player 1 (blue): ~d~n', [FPWins]),
 format('Number of wins for player 2 (red): ~d~n', [SPWins]),
 format('Longest (non-exhaustive) game: ~d~n', [LongestGame]),
 format('Shortest game: ~d~n', [ShortestGame]),
 format('Average game length (including exhaustives): ~d~n', [TotalMoves / N]),
 format('Average game time: ~d~n', [TotalTime / N]).

test(1, FPStrategy, SPStrategy, NumDraws, FPWins, SPWins, LongestGame,
    ShortestGame, TotalMoves, TotalTime) :-
 !,
 statistics(walltime, [Start,_]),
 play(quiet, FPStrategy, SPStrategy, NumMoves, WinningPlayer),
 statistics(walltime, [End,_]),
 (member(WinningPlayer, ['draw', 'exhaust', 'stalemate']) ->
   NumDraws is 1 ;
   NumDraws is 0),
 (WinningPlayer == 'b' -> FPWins is 1 ; FPWins is 0),
 (WinningPlayer == 'r' -> SPWins is 1 ; SPWins is 0),
 (WinningPlayer == 'exhaust' -> LongestGame is 0 ; LongestGame is NumMoves),
 ShortestGame is NumMoves,
 TotalMoves is NumMoves,
 TotalTime is End - Start.

test(N, FPStrategy, SPStrategy, NumDraws, FPWins, SPWins, LongestGame,
    ShortestGame, TotalMoves, TotalTime) :-
 NewN is N - 1,
 test(NewN, FPStrategy, SPStrategy, RNumDraws, RFPWins, RSPWins, RLongestGame,
     RShortestGame, RTotalMoves, RTotalTime),
 statistics(walltime, [Start,_]),
 play(quiet, FPStrategy, SPStrategy, NumMoves, WinningPlayer),
 statistics(walltime, [End,_]),
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

bloodlust(PlayerColour, CurrentBoardState, NewBoardState, Move) :-
 board_after_move(PlayerColour, CurrentBoardState, NewBoardState, Move),
 NewBoardState == [Blue, Red],
 ((PlayerColour == r) -> (
 \+ (board_after_move(PlayerColour, CurrentBoardState, [Blue2, Red2], Move2),
     Blue2 < Blue))).
 ((PlayerColour == b) -> (
 \+ (board_after_move(PlayerColour, CurrentBoardState, [Blue2, Red2], Move2),
     Red2 > Red))).

self_preservation(PlayerColour, CurrentBoardState, NewBoardState, Move) :-
 board_after_move(PlayerColour, CurrentBoardState, NewBoardState, Move),
 NewBoardState == [Blue, Red],
 ((PlayerColour == r) -> (
 \+ (board_after_move(PlayerColour, CurrentBoardState, [Blue2, Red2], Move2),
     Red2 < Red))).
 ((PlayerColour == b) -> (
 \+ (board_after_move(PlayerColour, CurrentBoardState, [Blue2, Red2], Move2),
     Blue2 > Blue))).

land_grab(PlayerColour, CurrentBoardState, NewBoardState, Move) :-
 board_after_move(PlayerColour, CurrentBoardState, NewBoardState, Move),
 NewBoardState == [Blue, Red],
 ((PlayerColour == r) -> (
 \+ (board_after_move(PlayerColour, CurrentBoardState, [Blue2, Red2], Move2),
    (Red2 - Blue2) > (Red - Blue)))).
 ((PlayerColour == b) -> (
 \+ (board_after_move(PlayerColour, CurrentBoardState, [Blue2, Red2], Move2),
    (Blue2 - Red2) > (Blue - Red)))).

minimax(PlayerColour, CurrentBoardState, NewBoardState, Move) :-
 NewBoardState == [Blue, Red],
 ((PlayerColour == r) -> (
 board_after_move(r, CurrentBoardState, IntermediateBoardState, Move),
 land_grab(b, IntermediateBoardState, [Blue, Red]),
 \+ (board_after_move(r, CurrentBoardState, IntermediateBoardState2, Move2),
     land_grab(b, IntermediateBoardState2, [Blue2, Red2]),
     (Blue2 - Red2) < (Blue - Red)))),
 ((PlayerColour == b) -> (
 board_after_move(b, CurrentBoardState, IntermediateBoardState, Move),
 land_grab(r, IntermediateBoardState, [Blue, Red]),
 \+ (board_after_move(b, CurrentBoardState, IntermediateBoardState2, Move2),
     land_grab(r, IntermediateBoardState2, [Blue2, Red2]),
     (Red2 - Blue2) < (Red - Blue)))).
 
     

board_after_move(PlayerColour, [Blue, Red], NewBoardState, Move) :-
 ((PlayerColour == r) -> random_move(Red, Blue, Move)),
 ((PlayerColour == b) -> random_move(Blue, Red, Move)),
 alter_board(Move, CurrentBoardState, IntermediateBoardState),
 next_generation(IntermediateBoardState, NewBoardState).


