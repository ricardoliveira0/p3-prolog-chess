:- initialization(start_game).

start_game :- argument_list(As), verify_format(As).

is_algebrica('algebrica') :- write('formato: algebrica'), nl, !.
is_descritiva('descritiva') :- write('formato: descritiva'), nl, !.
is_postal('postal') :- write('formato: postal'), nl, !.
is_mostrar('mostrar') :- print_board.
is_estado('estado') :- verify_check.

verify_format([F|T]) :- (is_algebrica(F); is_descritiva(F); is_postal(F)), verify_action(T).
verify_action([A|T]) :- (is_mostrar(A); is_estado(A)), start_game(T).

start_game(B) :- print_board.
/* command([]).
command([A|As]) :- arg(A), command(As).

arg('-') :- !, format("\n[standard input]\n\n", []).

arg(F) :- is_algebrica(F); is_descritiva(F); is_postal(F).

arg(B) :- is_mostrar(B); is_estado(B), !.

arg(F) :- file_exists(F), see(F), !,
		format("\n[file ~w]\n\n", [F]).

arg(U) :- format("[nao existe: ~w]\n", [U]), !, halt. */

% define the chess board with pieces at start positions
board([
        ['wR', 'wN', 'wB', 'wQ', 'wK', 'wB', 'wN', 'wR'],
        ['wP', 'wP', 'wP', 'wP', 'wP', 'wP', 'wP', 'wP'],
        ['es', 'es', 'es', 'es', 'es', 'es', 'es', 'es'],
        ['es', 'es', 'es', 'es', 'es', 'es', 'es', 'es'],
        ['es', 'es', 'es', 'es', 'es', 'es', 'es', 'es'],
        ['es', 'es', 'es', 'es', 'es', 'es', 'es', 'es'],
        ['bP', 'bP', 'bP', 'bP', 'bP', 'bP', 'bP', 'bP'],
        ['bR', 'bN', 'bB', 'bQ', 'bK', 'bB', 'bN', 'bR']
      ]).

% print the board to the console
print_board :-
        board(B),
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
(X1 =:= X ; Y1 =:= Y),
\+ occupied(X1, Y1).

chess_rules([peice_color, 78], X, Y, X1, Y1) :- % Knight
(X1 is X+2 ; X1 is X-2),
(Y1 is Y+1 ; Y1 is Y-1),
\+ occupied(X1, Y1).
chess_rules([piece_color, 78], X, Y, X1, Y1) :- % Knight
(X1 is X+1 ; X1 is X-1),
(Y1 is Y+2 ; Y1 is Y-2),
\+ occupied(X1, Y1).

chess_rules([piece_color, 66], X, Y, X1, Y1) :- % Bishop
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
(X1 is X+1 ; X1 is X-1 ; X1 is X),
(Y1 is Y+1 ; Y1 is Y-1 ; Y1 is Y),
\+ occupied(X1, Y1).

occupied(X, Y).
/* occupied(X, Y) :- occupied(board, X, Y). */
/* occupied(B, X, Y) :- . */
