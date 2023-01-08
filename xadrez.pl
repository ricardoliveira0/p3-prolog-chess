:- initialization(command).

command :- argument_list(As), interpreter(As).

interpreter([]).
interpreter([A|As]) :- arg(A), gets(As).

arg('-') :- !, format("\n[standard input]\n\n", []).

gets(As) :- get0(C), gets([], C, As).

gets(As, 10, As).		% 10 é o newline
gets(As, -1, As).		% -1 é o end-of-file
gets(I, C, [C|O]) :- get0(CC), gets(I, CC, O).



/* xadrez :- argument_list(As), comando(As).. */

/* formato_jogadas(As) :- nth0(1, As, Formato),
    string_codes(FormatoStr, Formato),
    (FormatoStr == 'algebrica' ->
    true
    ; FormatoStr == 'descritiva' ->
    true
    ; FormatoStr == 'postal' ->
    true
    ; write('Formato de jogadas inválido'), nl, fail).
    
ações(As) :- nth0(2, As, Ação1),
    string_codes(Ação1Str, Ação1),
    (Ação1Str == 'algebrica' ->
    true
    ; Ação1Str == 'descritiva' ->
    true
    ; Ação1Str == 'postal' ->
    true
    ; Ação1Str == 'mostrar' ->
    true
    ; Ação1Str == 'estado' ->
    true
    ; write('Ação inválida'), nl, fail). */

main :- true.

