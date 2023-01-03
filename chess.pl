% Pieces

pawn(X, Y, X1, Y1) :-
    (Y1 is Y+1 ; Y1 is Y+2),
    X1 is X,
    (Y1 =:= 2 ; Y1 =:= 7), % First move (can move two squares)
    \+ occupied(X1, Y1).
pawn(X, Y, X1, Y1) :-
    Y1 is Y+1,
    X1 is X,
    \+ occupied(X1, Y1).
pawn(X, Y, X1, Y1) :-
    Y1 is Y+1,
    (X1 is X+1 ; X1 is X-1),
    occupied(X1, Y1).

rook(X, Y, X1, Y1) :-
    (X1 =:= X ; Y1 =:= Y),
    \+ occupied(X1, Y1).

knight(X, Y, X1, Y1) :-
    (X1 is X+2 ; X1 is X-2),
    (Y1 is Y+1 ; Y1 is Y-1),
    \+ occupied(X1, Y1).
knight(X, Y, X1, Y1) :-
    (X1 is X+1 ; X1 is X-1),
    (Y1 is Y+2 ; Y1 is Y-2),
    \+ occupied(X1, Y1).

bishop(X, Y, X1, Y1) :-
    X1 is X + Y1 - Y,
    Y1 is Y + X1 - X,
    \+ occupied(X1, Y1).

queen(X, Y, X1, Y1) :- bishop(X, Y, X1, Y1).
queen(X, Y, X1, Y1) :- rook(X, Y, X1, Y1).

king(X, Y, X1, Y1) :-
    (X1 is X+1 ; X1 is X-1 ; X1 is X),
    (Y1 is Y+1 ; Y1 is Y-1 ; Y1 is Y),
    \+ occupied(X1, Y1).

occupied(X, Y) :- piece(X, Y, _).
