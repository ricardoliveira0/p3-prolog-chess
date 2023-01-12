:- initialization(start).

start :- argument_list(As), verify_format(As).

is_algebrica('algebrica') :- write('formato: algebrica'), nl, !.
is_descritiva('descritiva') :- write('formato: descritiva'), nl, !.
is_postal('postal') :- write('formato: postal'), nl, !.
is_mostrar('mostrar') :- write('ação: mostrar'), nl, !. % TEMP: remover e adicionar a função final em baixo, após os moves todos
/* is_mostrar('mostrar') :- print_board. */
is_estado('estado') :- verify_check.

verify_format([F|T]) :- (is_algebrica(F); is_descritiva(F); is_postal(F)), verify_action(T).
verify_action([A,J|T]) :-  open_game_file(J), (is_mostrar(A); is_estado(A)).

open_game_file(J) :- 
  file_exists(J),see(J), start_game,
  format("\n[file ~w]\n\n", [J]).

start_game :- write('esvaziou [current_move]'), nl, get0(C), check_char(C,[]).
start_game(MoveList) :- get0(C), check_char(C, MoveList).

check_char(32, List) :- write('SPACE'), write(List), nl, make_move_handler(List, false), start_game. % space (new move) | 1 for white
check_char(10, List) :- write('ENTER'), write(List), nl, make_move_handler(List, true), start_game. % enter (end of turn) | 0 for black
check_char(-1, List). % EOF (game finished)
check_char(C, List) :- append(List, [C], MoveList), start_game(MoveList), !.

make_move_handler(List, PieceColor) :- 
  write('-> make_move_handler args: '), write(List), write(' '), write(PieceColor), nl,
  (length(List, 2), make_move_two(List, PieceColor));
  (length(List, 3), make_move_three(List, PieceColor));
  (length(List, 4), make_move_four(List, PieceColor)).
  /* (length(List, 5), make_move_five). */

make_move_two([X,Y], PC):- 
  X1 is X-96, Y1 is Y-48, write(X1), write(' '), write(Y1), nl, chess_board(Board),
  find_pawn(Board, X1, Y1, PC, UpdatedBoard), retract(chess_board(_)), assertz(chess_board(UpdatedBoard)), print_board.
make_move_three([X,Y,Z], PC):-
  Y1 is Y-96, Z1 is Z-48, write(X), write(' '), write(Y1), write(' '), write(Z1), nl, chess_board(Board),
  move_piece(Board, X, Y1, Z1, PC, UpdatedBoard), retract(chess_board(_)), assertz(chess_board(UpdatedBoard)), print_board.
make_move_four([X,Y,Z,W], PC):- write('4 chars move'), nl, nl.
/* make_move_five:- write('5 chars move'), nl, nl. */

find_pawn(Board, X, Y, PC, UpdatedBoard) :- 
  ( (PC = false, name(Piece, [119, 80]) ) ; (PC = true, name(Piece, [98, 80])) ),
  ( (PC = false, Y1 is Y - 2) ; (PC = true, Y1 is Y + 2) ),
  nth(Y1, Board, FromRow),
  nth(Y, Board, ToRow),
  nth(X, FromRow, Piece),
  new_row_handler('es', FromRow, X, FinalFromRow),
  new_row_handler(Piece, ToRow, X, FinalToRow),
  replace(Board, Y1, FinalFromRow, TempBoard),
  replace(TempBoard, Y, FinalToRow, UpdatedBoard).
  
move_piece(Board, P, X, Y, PC, UpdatedBoard) :-
  ( (PC = false, name(Piece, [119, P]) ) ; (PC = true, name(Piece, [98, P])) ),
  write('PEÇA: '), write(Piece), nl,
  chess_rules(Piece, X, Y, X, Y).

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
        write('|'),
        print_row(Row, '|'),
        nl.
      
      print_row([], _) :- !.
      print_row([Piece|Rest], Separator) :-
        write(Separator), write(Piece),
        print_row(Rest, Separator).

verify_check :- write('teste'), nl.

chess_rules([piece_color, 80], X, Y, X1, Y1) :- % Pawn
  (Y1 is Y+1 ; Y1 is Y+2),
  X1 is X,
  (Y1 =:= 2 ; Y1 =:= 7), % First move (can move two squares)
  \+ occupied(X1, Y1).

chess_rules([piece_color, 80], X, Y, X1, Y1) :- % Pawn
  Y1 is Y+1,
  X1 is X,
  \+ occupied(X1, Y1).

chess_rules([piece_color, 80], X, Y, X1, Y1) :- % Pawn
  Y1 is Y+1,
  (X1 is X+1 ; X1 is X-1),
  occupied(X1, Y1).

chess_rules([piece_color, 82], X, Y, X1, Y1) :- % Rook
  write('Rook'), nl,
  (X1 =:= X ; Y1 =:= Y),
  \+ occupied(X1, Y1).

chess_rules([piece_color, 78], X, Y, X1, Y1) :- % Knight
  write('Knight'), nl,
  (X1 is X+2 ; X1 is X-2),
  (Y1 is Y+1 ; Y1 is Y-1),
  \+ occupied(X1, Y1).

chess_rules([piece_color, 78], X, Y, X1, Y1) :- % Knight
  (X1 is X+1 ; X1 is X-1),
  (Y1 is Y+2 ; Y1 is Y-2),
  \+ occupied(X1, Y1).

chess_rules([piece_color, 66], X, Y, X1, Y1) :- % Bishop
  write('Bishop'), nl,
  X1 is X + Y1 - Y,
  Y1 is Y + X1 - X,
  \+ occupied(X1, Y1).

chess_rules([piece_color, 81], X, Y, X1, Y1) :- % Queen
  X1 is X + Y1 - Y,
  Y1 is Y + X1 - X,
  \+ occupied(X1, Y1).

chess_rules([piece_color, 81], X, Y, X1, Y1) :- % Queen
  (X1 =:= X ; Y1 =:= Y),
  \+ occupied(X1, Y1).

chess_rules([piece_color, 75], X, Y, X1, Y1) :- % King
  write('King'), nl,
  (X1 is X+1 ; X1 is X-1 ; X1 is X),
  (Y1 is Y+1 ; Y1 is Y-1 ; Y1 is Y),
  \+ occupied(X1, Y1).

occupied(X, Y).
/* occupied(X, Y) :- occupied(board, X, Y). */
/* occupied(B, X, Y) :- . */
