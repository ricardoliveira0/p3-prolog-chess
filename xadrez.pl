:- initialization(start).

start :- argument_list(As), verify_format(As).

is_algebrica('algebrica') :- write('formato: algebrica'), nl, !. % TO DO
is_descritiva('descritiva') :- write('formato: descritiva'), nl, !. % TO DO
is_postal('postal') :- write('formato: postal'), nl, !. % TO DO
is_mostrar('mostrar') :- write('ação: mostrar'), nl, start_game, print_board. % Plays the whole game and print Board at final instance
is_estado('estado') :- write('ação: estado'), nl, start_game, verify_check. % Verify if King is in check at this Board instance

verify_format([F|T]) :- (is_algebrica(F); is_descritiva(F); is_postal(F)), verify_action(T). % Verify 1st argument (format)
verify_action([A,J|T]) :- open_game_file(J), (is_mostrar(A); is_estado(A)). % Verify 2nd argument (action)

open_game_file(J) :- % Verify Tail (last argument) from argument_list (file to open)
  file_exists(J), see(J),
  format("\n[file ~w]\n\n", [J]).

% Read char by char. If there is passed any [MoveList] continue to append on it. Otherwise clear the current [MoveList] (new movement found) 
start_game :- get0(C), check_char(C,[]).
start_game(MoveList) :- get0(C), check_char(C, MoveList).

check_char(32, List) :- make_move_handler(List, false), start_game. % space (new move) | 1 for white
check_char(10, List) :- make_move_handler(List, true), start_game. % enter (end of turn) | 0 for black
check_char(-1, List). % EOF (game finished)
check_char(C, List) :- append(List, [C], MoveList), start_game(MoveList), !. % no 'special' char found, continue adding chars to [MoveList]

make_move_handler(List, PieceColor) :- % Predicate that handles the movement lenght
  (length(List, 2), make_move_two(List, PieceColor));
  (length(List, 3), make_move_three(List, PieceColor));
  (length(List, 4), make_move_four(List, PieceColor)).

% make_move_N - predicates that handle the different types of moves, "translating" the ASCII move to coordinates
make_move_two([X,Y], PC) :- 
  X1 is X - 96, Y1 is Y - 48, chess_board(Board),
  find_pawn(Board, X1, Y1, PC, UpdatedBoard), retract(chess_board(_)), assertz(chess_board(UpdatedBoard)). %, print_board.
make_move_three([X,Y,Z], PC) :- 
  (( chess_board(Board), X = 79, Y = 45, Z = 79, move_piece(Board, X, Y, Z, PC, UpdatedBoard) ); % It's Castle
  ( chess_board(Board), Y1 is Y-96, Z1 is Z-48, move_piece(Board, X, Y1, Z1, PC, UpdatedBoard))), % Normal move
  retract(chess_board(_)), assertz(chess_board(UpdatedBoard)). %, print_board.
make_move_four([X,Y,Z,W], PC):-
  ( % The second char is 'x' (ASCII 120) so it is a take move, act like a normal move
    chess_board(Board),
    Y = 120, 
    Z1 is Z - 96, W1 is W - 48,
    move_piece(Board, X, Z1, W1, PC, UpdatedBoard), retract(chess_board(_)), assertz(chess_board(UpdatedBoard)) %, print_board
  ); 
  ( % The second char is between 'a' (ASCII 97) and 'h' (ASCII 104) so it is a move where two pieces can move to that position
    chess_board(Board),
    (Y >= 97, Y =< 104, W =\= 43 ),
    Y1 is Y - 96, Z1 is Z - 96, W1 is W - 48
    % TO DO
    /* move_piece(Board, X, Y1, Z1, W1, PC, UpdatedBoard), retract(chess_board(_)), assertz(chess_board(UpdatedBoard)) */
  );
  ( % Normal move but there is a check
    chess_board(Board),
    (Y >= 97, Y =< 104, W =:= 43 ),
    Y1 is Y - 96, Z1 is Z - 48, 
    move_piece(Board, X, Y1, Z1, PC, UpdatedBoard), retract(chess_board(_)), assertz(chess_board(UpdatedBoard)) %, print_board
  ).

find_pawn(Board, X, Y, PC, UpdatedBoard) :- 
  ( (PC = false, name(Piece, [119, 80]) ) ; (PC = true, name(Piece, [98, 80])) ),
  name(Piece, PieceList),
  chess_rules(PieceList, X, Y, FromX, FromY, Board),
  nth(FromY, Board, FromRow),
  nth(Y, Board, ToRow),
  nth(FromX, FromRow, Piece),
  new_row_handler('es', FromRow, FromX, FinalFromRow),
  new_row_handler(Piece, ToRow, X, FinalToRow),
  replace(Board, FromY, FinalFromRow, TempBoard),
  replace(TempBoard, Y, FinalToRow, UpdatedBoard).

move_piece(Board, 79, 45, 79, PC, UpdatedBoard) :- % O-O (Castle)
  ( (PC = false, name(Piece1, [119, 75]), name(Piece2, [119, 82]) ) ; (PC = true, name(Piece1, [98, 75]), name(Piece2, [98, 82])) ),
  name(Piece1, PieceList1), name(Piece2, PieceList2),
  do_castle(PieceList1, PieceList2, Y),
  nth(Y, Board, Row),
  nth(5, Row, Piece1),
  nth(8, Row, Piece2),
  new_row_handler('es', Row, 5, TempRow1),
  new_row_handler(Piece1, TempRow1, 7, TempRow2),
  new_row_handler('es', TempRow2, 8, TempRow3),
  new_row_handler(Piece2, TempRow3, 6, FinalFromRow),
  replace(Board, Y, FinalFromRow, UpdatedBoard).

move_piece(Board, P, X, Y, PC, UpdatedBoard) :- % Normal Move
  ( (PC = false, name(Piece, [119, P]) ) ; (PC = true, name(Piece, [98, P])) ),
  name(Piece, PieceList),
  chess_rules(PieceList, X, Y, FromX, FromY, Board),
  nth(FromY, Board, FromRow),
  nth(Y, Board, ToRow),
  nth(FromX, FromRow, Piece),
  new_row_handler('es', FromRow, FromX, FinalFromRow),
  new_row_handler(Piece, ToRow, X, FinalToRow),
  replace(Board, FromY, FinalFromRow, TempBoard),
  replace(TempBoard, Y, FinalToRow, UpdatedBoard).

new_row_handler(Piece, Row, Position, NewRow) :-
  PrefixLength is Position - 1,
  length(Prefix, PrefixLength),
  append(Prefix, [KP|Suffix], Row),
  append(Prefix, [Piece], Temp),
  append(Temp, Suffix, NewRow).

replace([_|T], 1, X, [X|T]).
replace([H|T], I, X, [H|R]) :- I > 1, I1 is I-1, replace(T, I1, X, R).

:- dynamic(chess_board/1).
chess_board([
        ['wR', 'wN', 'wB', 'wQ', 'wK', 'wB', 'wN', 'wR'],
        ['wP', 'wP', 'wP', 'wP', 'wP', 'wP', 'wP', 'wP'],
        ['es', 'es', 'es', 'es', 'es', 'es', 'es', 'es'],
        ['es', 'es', 'es', 'es', 'es', 'es', 'es', 'es'],
        ['es', 'es', 'es', 'es', 'es', 'es', 'es', 'es'],
        ['es', 'es', 'es', 'es', 'es', 'es', 'es', 'es'],
        ['bP', 'bP', 'bP', 'bP', 'bP', 'bP', 'bP', 'bP'],
        ['bR', 'bN', 'bB', 'bQ', 'bK', 'bB', 'bN', 'bR']
      ]).

print_board :-
  chess_board(B),
  print_board(B).
      
print_board([]) :- nl.

print_board([Row|Rest]) :-
  print_row(Row),
  print_board(Rest).
      
print_row(Row) :-
  print_row(Row, '|'), nl.
      
print_row([], _) :- !.

print_row([Piece|Rest], Separator) :-
  write(Separator), write(Piece),
  print_row(Rest, Separator).

verify_check :- write('Verifying check'), nl.

% Chess Rules for every piece
chess_rules([C, 80], X, Y, FromX, FromY, Board) :- % Pawn
  ( % Do OR between black and white (because white goes forward in the board otherwise black goes backward). Also OR for 1 or 2 piece moves
    C = 119,
    (
      (Y1 is Y - 1),
      is_inside_board(X, Y1),
      find_piece(X, Y1, Board, [C, 80]),
      FromX is X, FromY is Y1
    );
    (
      (Y1 is Y - 2),
      is_inside_board(X, Y1),
      find_piece(X, Y1, Board, [C, 80]),
      FromX is X, FromY is Y1
    )
  );
  (
    C = 98,
    (
      (Y1 is Y + 1),
      is_inside_board(X, Y1),
      find_piece(X, Y1, Board, [C, 80]),
      FromX is X, FromY is Y1
    );
    (
      (Y1 is Y + 2),
      is_inside_board(X, Y1),
      find_piece(X, Y1, Board, [C, 80]),
      FromX is X, FromY is Y1
    )
  ).

chess_rules([C, 78], X, Y, FromX, FromY, Board) :- % Knight
  ( (X1 is X + 2) ; (X1 is X - 2) ),
  ( (Y1 is Y + 1) ; (Y1 is Y - 1) ),
  is_inside_board(X1, Y1),
  find_piece(X1, Y1, Board, [C, 78]),
  FromX is X1, FromY is Y1.

chess_rules([C, 78], X, Y, FromX, FromY, Board) :- % Knight
  ( (X1 is X + 1) ; (X1 is X - 1) ),
  ( (Y1 is Y + 2) ; (Y1 is Y - 2) ),
  is_inside_board(X1, Y1),
  find_piece(X1, Y1, Board, [C, 78]),
  FromX is X1, FromY is Y1.

chess_rules([C, 66], X, Y, FromX, FromY, Board) :- % Bishop
  ( ( X1 is X + 1, Y1 is Y + 1 ); ( X1 is X + 1, Y1 is Y - 1 ); ( X1 is X - 1, Y1 is Y + 1 ); ( X1 is X - 1, Y1 is Y - 1 );
  ( X1 is X + 2, Y1 is Y + 2 ); ( X1 is X + 2, Y1 is Y - 2 ); ( X1 is X - 2, Y1 is Y + 2 ); ( X1 is X - 2, Y1 is Y - 2 );
  ( X1 is X + 3, Y1 is Y + 3 ); ( X1 is X + 3, Y1 is Y - 3 ); ( X1 is X - 3, Y1 is Y + 3 ); ( X1 is X - 3, Y1 is Y - 3 );
  ( X1 is X + 4, Y1 is Y + 4 ); ( X1 is X + 4, Y1 is Y - 4 ); ( X1 is X - 4, Y1 is Y + 4 ); ( X1 is X - 4, Y1 is Y - 4 );
  ( X1 is X + 5, Y1 is Y + 5 ); ( X1 is X + 5, Y1 is Y - 5 ); ( X1 is X - 5, Y1 is Y + 5 ); ( X1 is X - 5, Y1 is Y - 5 );
  ( X1 is X + 6, Y1 is Y + 6 ); ( X1 is X + 6, Y1 is Y - 6 ); ( X1 is X - 6, Y1 is Y + 6 ); ( X1 is X - 6, Y1 is Y - 6 );
  ( X1 is X + 7, Y1 is Y + 7 ); ( X1 is X + 7, Y1 is Y - 7 ); ( X1 is X - 7, Y1 is Y + 7 ); ( X1 is X - 7, Y1 is Y - 7 ) ),
  is_inside_board(X1, Y1),
  find_piece(X1, Y1, Board, [C, 66]),
  FromX is X1, FromY is Y1.

chess_rules([C, 82], X, Y, FromX, FromY, Board) :- % Rook
  between(1, 8, X1),
  between(1, 8, Y1),
  (X1 =:= X ; Y1 =:= Y),
  is_inside_board(X1, Y1),
  find_piece(X1, Y1, Board, [C, 82]),
  FromX is X1, FromY is Y1.

chess_rules([C, 75], X, Y, FromX, FromY, Board) :- % King
  between(-1, 1, Xaux),
  between(-1, 1, Yaux),
  (X1 is X + Xaux, Y1 is Y + Yaux),
  is_inside_board(X1, Y1),
  find_piece(X1, Y1, Board, [C, 75]),
  FromX is X1, FromY is Y1.

chess_rules([C, 81], X, Y, FromX, FromY, Board) :- % Queen
  % Bishop like move (Diagonals)
  (
    ( ( X1 is X + 1, Y1 is Y + 1 ); ( X1 is X + 1, Y1 is Y - 1 ); ( X1 is X - 1, Y1 is Y + 1 ); ( X1 is X - 1, Y1 is Y - 1 );
    ( X1 is X + 2, Y1 is Y + 2 ); ( X1 is X + 2, Y1 is Y - 2 ); ( X1 is X - 2, Y1 is Y + 2 ); ( X1 is X - 2, Y1 is Y - 2 );
    ( X1 is X + 3, Y1 is Y + 3 ); ( X1 is X + 3, Y1 is Y - 3 ); ( X1 is X - 3, Y1 is Y + 3 ); ( X1 is X - 3, Y1 is Y - 3 );
    ( X1 is X + 4, Y1 is Y + 4 ); ( X1 is X + 4, Y1 is Y - 4 ); ( X1 is X - 4, Y1 is Y + 4 ); ( X1 is X - 4, Y1 is Y - 4 );
    ( X1 is X + 5, Y1 is Y + 5 ); ( X1 is X + 5, Y1 is Y - 5 ); ( X1 is X - 5, Y1 is Y + 5 ); ( X1 is X - 5, Y1 is Y - 5 );
    ( X1 is X + 6, Y1 is Y + 6 ); ( X1 is X + 6, Y1 is Y - 6 ); ( X1 is X - 6, Y1 is Y + 6 ); ( X1 is X - 6, Y1 is Y - 6 );
    ( X1 is X + 7, Y1 is Y + 7 ); ( X1 is X + 7, Y1 is Y - 7 ); ( X1 is X - 7, Y1 is Y + 7 ); ( X1 is X - 7, Y1 is Y - 7 ) ),
    is_inside_board(X1, Y1),
    find_piece(X1, Y1, Board, [C, 81])
  );
  ( % Rook like move (Rows or Columns)
    between(1, 8, X1),
    between(1, 8, Y1),
    (X1 =:= X ; Y1 =:= Y),
    is_inside_board(X1, Y1),
    find_piece(X1, Y1, Board, [C, 81])
  ),
  FromX is X1, FromY is Y1.

find_piece(X, Y, Board, Piece) :-
  nth(Y, Board, Row),
  nth(X, Row, CurrentPiece),
  name(CurrentPiece, Piece).

is_inside_board(X, Y) :- % Verify if Piece is inside the game board
  Min is 1, Max is 8,
  (X >= Min, X =< Max), (Y >= Min, Y =< Max).

do_castle([C, 75], [C, 82], Y) :-
  (
    C = 119, 
    Y = 1 
  );
  (
    C = 98,
    Y = 8
  ).

