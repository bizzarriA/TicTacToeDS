:- use_module(library(mcintyre)).

:- mc.


%% predicati per determinare a chi tocca giocare %%%
count(_,CT,[],CT).
count(S,C0,[S|T],CT):-
    C1 is C0 + 1,
    count(S,C1,T,CT).
count(S,C0,[S1|T],CT):-
    S \= S1,
    count(S,C0,T,CT).

count_symbols_diff(CO,CX,o):- CO =:= CX - 1.
count_symbols_diff(CO,CX,x):- CX =:= CO - 1.

next_player(Board,NextPlayer):-
	count(o,0,Board,CO),
    count(x,0,Board,CX),
    count_symbols_diff(CO,CX,NextPlayer).

next_player(Board,x):-
    count(o,0,Board,Count),
    count(x,0,Board,Count).


% BOARD:
% (1,3) (2,3) (3,3)
% (1,2) (2,2) (3,2)
% (1,1) (2,1) (3,1)

empty_board :- 
    cell(1,3,b), cell(2,3,b), cell(3,3,b),
    cell(1,2,b), cell(2,2,b), cell(3,2,b),
    cell(1,1,b), cell(2,1,b), cell(3,1,b).
    
full_board :-
    \+cell(1,3,b), \+cell(2,3,b), \+cell(3,3,b),
    \+cell(1,2,b), \+cell(2,2,b), \+cell(3,2,b),
    \+cell(1,1,b), \+cell(2,1,b), \+cell(3,1,b).


win(P) :- cell(1,1,P), cell(2,1,P), cell(3,1,P), P \== b. % rowwin
win(P) :- cell(1,2,P), cell(2,2,P), cell(3,2,P), P \== b. % rowwin
win(P) :- cell(1,3,P), cell(2,3,P), cell(3,3,P), P \== b. % rowwin

win(P) :- cell(1,1,P), cell(1,2,P), cell(1,3,P), P \== b. % colwin
win(P) :- cell(2,1,P), cell(2,2,P), cell(2,3,P), P \== b. % colwin
win(P) :- cell(3,1,P), cell(3,2,P), cell(3,3,P), P \== b. % colwin

win(P) :- cell(1,1,P), cell(2,2,P), cell(3,3,P), P \== b. % diagwin
win(P) :- cell(1,3,P), cell(2,2,P), cell(3,1,P), P \== b. % diagwin


opponent(x,o).
opponent(o,x).


% === NEXT MOVE ==============================================================

% --- GENERAL RULE: se ci sono due simboli in fila, metti il terzo -----------
% Regola valida sia per vincere, sia per bloccare l'avversario
% line(Col,Row) restituisce la colonna e la riga in cui giocare

% BOARD:
% (1,3) (2,3) (3,3)
% (1,2) (2,2) (3,2)
% (1,1) (2,1) (3,1)

% row win/block
line(C,1,S) :- cell(C,1,b), cell(C,2,S), cell(C,3,S), S \== b.
line(C,2,S) :- cell(C,1,S), cell(C,2,b), cell(C,3,S), S \== b.
line(C,3,S) :- cell(C,1,S), cell(C,2,S), cell(C,3,b), S \== b.

line(1,R,S) :- cell(1,R,b), cell(2,R,S), cell(3,R,S), S \== b.
line(2,R,S) :- cell(1,R,S), cell(2,R,b), cell(3,R,S), S \== b.
line(3,R,S) :- cell(1,R,S), cell(2,R,S), cell(3,R,b), S \== b.

line(1,1,S) :- cell(1,1,b), cell(2,2,S), cell(3,3,S), S \== b.
line(2,2,S) :- cell(1,1,S), cell(2,2,b), cell(3,3,S), S \== b.
line(3,3,S) :- cell(1,1,S), cell(2,2,S), cell(3,3,b), S \== b.

line(1,1,S) :- cell(1,3,b), cell(2,2,S), cell(3,1,S), S \== b.
line(2,2,S) :- cell(1,3,S), cell(2,2,b), cell(3,1,S), S \== b.
line(3,3,S) :- cell(1,3,S), cell(2,2,S), cell(3,1,b), S \== b.

line :- line(_,_,_).


:- begin_lpad.
% begin_lpad contiene tutti i predicati probabilistici e i predicati che li usano

place_elements([]).
place_elements([H|T]):-
    el(_,H),
    place_elements(T).

generate_random_board(Board):-
    length(Board,9),
    place_elements(Board),
    valid(Board), !.
generate_random_board(Board):-
    generate_random_board(Board).


el(R,V) : uniform(V,[b,x,o]):- random(R).

next(C,R) :- player(Player), line(C,R,Player).
next(C,R) :- player(Player), opponent(Player,Opponent), line(C,R,Opponent).


% --- PLAYER 1 (x) -------------------------------------------------------
% NOTA: Xn, On = n-esima mossa di x/o

% 1. X1 in a corner (4 probabilistic alternatives).
next(1,1) : 0.25 ; next(3,1) : 0.25 ; next(1,3) : 0.25 ; next(3,3) : 0.25 :- empty_board.

% 2a. O1 in the CENTER.
% 2aa. X2 opposite CORNER from X1 (so there's a line going "X O X" diagonally across the board) 
% OR 
% 2ab. X2 on an EDGE square (not a corner), NOT touching your first X.  
next(A,B) : uniform([A,B],[[2,3],[3,3],[3,2]]) :- 
% next(2,3) : 0.33 ; next(3,3) : 0.33 ; next(3,2) : 0.33 :-
    \+empty_board, \+line, cell(2,2,o), cell(1,1,x), cell(2,1,b), cell(3,1,b), cell(1,2,b), cell(3,2,b), cell(1,3,b), cell(2,3,b), cell(3,3,b).

next(A,B) : uniform([A,B],[[1,1],[2,3],[1,2]]) :- 
% next(1,1) : 0.33 ; next(2,3) : 0.33 ; next(1,2) : 0.33 :-
    \+empty_board, \+line, cell(2,2,o), cell(3,1,x), cell(1,1,b), cell(2,1,b), cell(1,2,b), cell(3,2,b), cell(1,3,b), cell(2,3,b), cell(3,3,b).

next(A,B) : uniform([A,B],[[1,2],[1,1],[2,1]]) :- 
% next(1,2) : 0.33 ; next(1,1) : 0.33 ; next(2,1) : 0.33 :-
    \+empty_board, \+line, cell(2,2,o), cell(3,3,x), cell(1,1,b), cell(2,1,b), cell(3,1,b), cell(1,2,b), cell(3,2,b), cell(1,3,b), cell(2,3,b).

next(A,B) : uniform([A,B],[[3,2],[2,1],[3,1]]) :- 
% next(3,2) : 0.33 ; next(2,1) : 0.33 ; next(3,1) : 0.33 :-
    \+empty_board, \+line, cell(2,2,o), cell(1,3,x), cell(1,1,b), cell(2,1,b), cell(3,1,b), cell(1,2,b), cell(3,2,b), cell(2,3,b), cell(3,3,b).


% 3aa. O2 in one of the other CORNERS => X3 in the LAST empty corner (and win with the next move)
% NOTA: questi casi dovrebbero essere già tutti coperti: 
% - se O mette in un corner, ha una linea in diag => X blocca nell'ultimo corner rimasto (e fa doppio gioco)
% - se O mette in un edge, ha una linea in row/col => X blocca
% => IN TEORIA QUESTE REGOLE NON SERVONO
% X in (1,1) E in (3,3) 
% next(3,1) :- \+empty_board, \+line, cell(2,2,o), cell(1,3,o), cell(1,1,x), cell(3,3,x), cell(3,1,b). 
% next(1,1) :- \+empty_board, \+line, cell(2,2,o), cell(3,1,o), cell(1,1,x), cell(3,3,x), cell(1,1,b). 
% X in (1,3) E in (3,1) 
% next(1,1) :- \+empty_board, \+line, cell(2,2,o), cell(3,3,o), cell(1,3,x), cell(3,1,x), cell(1,1,b).
% next(3,3) :- \+empty_board, \+line, cell(2,2,o), cell(1,1,o), cell(1,3,x), cell(3,1,x), cell(3,3,b).

% 3ab: If your opponent puts down an O in the corner that's not next to your X, 
% you can use your third X to block their move and automatically win with your fourth X.
% IN TEORIA, CASO GIà COPERTO DA GENERAL RULE (ma O2 DEVE essere NON vicino alla X)

% 3ac [nessuna delle precedenti]: E se O2 è vicino alla X? In teoria coperto da mossa a caso || block

% ----

% 2b. O1 NOT in center.
% X2 in any other corner (NOT opposite X1 - vedi nota), with an empty space in between the two X's.

% - Caso in cui player1 inizia in (1,3)
next(1,1) : 0.5 ; next(3,3) : 0.5 :-
    \+empty_board, \+line, cell(1,3,x), cell(2,2,b), cell(1,1,b), cell(3,3,b), cell(2,3,b), cell(1,2,b), cell(3,2,o).
next(1,1) : 0.5 ; next(3,3) : 0.5 :-
    \+empty_board, \+line, cell(1,3,x), cell(2,2,b), cell(1,1,b), cell(3,3,b), cell(2,3,b), cell(1,2,b), cell(2,1,o).
next(1,1) : 0.5 ; next(3,3) : 0.5 :-
    \+empty_board, \+line, cell(1,3,x), cell(2,2,b), cell(1,1,b), cell(3,3,b), cell(2,3,b), cell(1,2,b), cell(3,1,o).
next(3,3) :- \+empty_board, \+line, cell(1,3,x), cell(2,2,b), cell(3,3,b), cell(2,3,b). 
next(1,1) :- \+empty_board, \+line, cell(1,3,x), cell(2,2,b), cell(1,1,b), cell(2,1,b). 
% next(3,1) :- \+empty_board, \+line, cell(1,3,x), cell(2,2,b), cell(3,1,b). 

% - Caso in cui player1 inizia in (3,3)
next(1,3) : 0.5 ; next(3,1) : 0.5 :-
    \+empty_board, \+line, cell(3,3,x), cell(2,2,b), cell(1,3,b), cell(3,1,b), cell(2,3,b), cell(3,2,b), cell(1,2,o).
next(1,3) : 0.5 ; next(3,1) : 0.5 :-
    \+empty_board, \+line, cell(3,3,x), cell(2,2,b), cell(1,3,b), cell(3,1,b), cell(2,3,b), cell(3,2,b), cell(1,1,o).
next(1,3) : 0.5 ; next(3,1) : 0.5 :-
    \+empty_board, \+line, cell(3,3,x), cell(2,2,b), cell(1,3,b), cell(3,1,b), cell(2,3,b), cell(3,2,b), cell(3,1,o).
next(1,3) :- \+empty_board, \+line, cell(3,3,x), cell(2,2,b), cell(1,3,b), cell(2,3,b).
next(3,1) :- \+empty_board, \+line, cell(3,3,x), cell(2,2,b), cell(3,1,b), cell(3,2,b).
% next(1,1) :- \+empty_board, \+line, cell(3,3,x), cell(2,2,b), cell(1,1,b).

% - Caso in cui player1 inizia in (1,1)
next(1,3) : 0.5 ; next(3,1) : 0.5 :-
    \+empty_board, \+line, cell(1,1,x), cell(2,2,b), cell(1,3,b), cell(3,1,b), cell(1,2,b), cell(2,1,b), cell(2,3,o).
next(1,3) : 0.5 ; next(3,1) : 0.5 :-
    \+empty_board, \+line, cell(1,1,x), cell(2,2,b), cell(1,3,b), cell(3,1,b), cell(1,2,b), cell(2,1,b), cell(3,3,o).
next(1,3) : 0.5 ; next(3,1) : 0.5 :-
    \+empty_board, \+line, cell(1,1,x), cell(2,2,b), cell(1,3,b), cell(3,1,b), cell(1,2,b), cell(2,1,b), cell(3,2,o).
next(1,3) :- \+empty_board, \+line, cell(1,1,x), cell(2,2,b), cell(1,3,b), cell(1,2,b).
next(3,1) :- \+empty_board, \+line, cell(1,1,x), cell(2,2,b), cell(3,1,b), cell(2,1,b).
% next(3,3) :- \+empty_board, \+line, cell(1,1,x), cell(2,2,b), cell(3,3,b).

% - Caso in cui player1 inizia in (3,1)
next(1,1) : 0.5 ; next(3,3) : 0.5 :-
    \+empty_board, \+line, cell(3,1,x), cell(2,2,b), cell(1,1,b), cell(3,3,b), cell(3,2,b), cell(2,1,b), cell(1,3,o).
next(1,1) : 0.5 ; next(3,3) : 0.5 :-
    \+empty_board, \+line, cell(3,1,x), cell(2,2,b), cell(1,1,b), cell(3,3,b), cell(3,2,b), cell(2,1,b), cell(2,3,o).
next(1,1) : 0.5 ; next(3,3) : 0.5 :-
    \+empty_board, \+line, cell(3,1,x), cell(2,2,b), cell(1,1,b), cell(3,3,b), cell(3,2,b), cell(2,1,b), cell(1,2,o).
next(1,1) :- \+empty_board, \+line, cell(3,1,x), cell(2,2,b), cell(1,1,b), cell(2,1,b).
next(3,3) :- \+empty_board, \+line, cell(3,1,x), cell(2,2,b), cell(3,3,b), cell(3,2,b).
% next(1,3) :- \+empty_board, \+line, cell(3,1,x), cell(2,2,b), cell(1,3,b).

% NOTA: lo spazio al centro non va bene: se O2 blocca al centro, X3 dovrà sempre bloccare (=> no doppio gioco)


% 3b. Put X3 so you have two possible winning moves (given that O2 blocks - if not, win). 
% After O2 blocks, there should be an empty square that is in line with both X1 and X2, 
% with no enemy O's blocking that line: put X3 in this square.


% BOARD:
% (1,3) (2,3) (3,3)
% (1,2) (2,2) (3,2)
% (1,1) (2,1) (3,1)

% NOTA: non può essercene solo 1 blank perché con il secondo o blocca la linea

% Caso X-O-X nella colonna 1
next(A,B) : uniform([A,B],[[3,1],[2,2],[3,3]]) :- 
% next(3,1) : 0.33 ; next(2,2) : 0.33 ; next(3,3) : 0.33 :-
    \+empty_board, \+line, cell(1,1,x), cell(1,2,o), cell(1,3,x), cell(3,1,b), cell(2,2,b), cell(3,3,b).

next(3,1) : 0.5 ; next(2,2) : 0.5 :-
    \+empty_board, \+line, cell(1,1,x), cell(1,2,o), cell(1,3,x), cell(3,1,b), cell(2,2,b), \+cell(3,3,b).

next(3,1) : 0.5 ; next(3,3) : 0.5 :-
    \+empty_board, \+line, cell(1,1,x), cell(1,2,o), cell(1,3,x), cell(3,1,b), \+cell(2,2,b), cell(3,3,b).

next(2,2) : 0.5 ; next(3,3) : 0.5 :-
    \+empty_board, \+line, cell(1,1,x), cell(1,2,o), cell(1,3,x), \+cell(3,1,b), cell(2,2,b), cell(3,3,b).


% Caso X-O-X nella colonna 3
next(A,B) : uniform([A,B],[[1,1],[2,2],[1,3]]) :- 
% next(1,1) : 0.33 ; next(2,2) : 0.33 ; next(1,3) : 0.33 :-
    \+empty_board, \+line, cell(3,1,x), cell(3,2,o), cell(3,3,x), cell(1,1,b), cell(2,2,b), cell(1,3,b).

next(1,1) : 0.5 ; next(2,2) : 0.5 :-
    \+empty_board, \+line, cell(3,1,x), cell(3,2,o), cell(3,3,x), cell(1,1,b), cell(2,2,b), \+cell(1,3,b).

next(1,1) : 0.5 ; next(1,3) : 0.5 :-
    \+empty_board, \+line, cell(3,1,x), cell(3,2,o), cell(3,3,x), cell(1,1,b), \+cell(2,2,b), cell(1,3,b).

next(2,2) : 0.5 ; next(1,3) : 0.5 :-
    \+empty_board, \+line, cell(3,1,x), cell(3,2,o), cell(3,3,x), \+cell(1,1,b), cell(2,2,b), cell(1,3,b).


% Caso X-O-X nella riga 1
next(A,B) : uniform([A,B],[[2,2],[1,3],[3,3]]) :- 
% next(2,2) : 0.33 ; next(1,3) : 0.33 ; next(3,3) : 0.33 :-
    \+empty_board, \+line, cell(1,1,x), cell(2,1,o), cell(3,1,x), cell(2,2,b), cell(1,3,b), cell(3,3,b).

next(2,2) : 0.5 ; next(1,3) : 0.5 :-
    \+empty_board, \+line, cell(1,1,x), cell(2,1,o), cell(3,1,x), cell(2,2,b), cell(1,3,b), \+cell(3,3,b).

next(2,2) : 0.5 ; next(3,3) : 0.5 :-
    \+empty_board, \+line, cell(1,1,x), cell(2,1,o), cell(3,1,x), cell(2,2,b), \+cell(1,3,b), cell(3,3,b).

next(1,3) : 0.5 ; next(3,3) : 0.5 :-
    \+empty_board, \+line, cell(1,1,x), cell(2,1,o), cell(3,1,x), \+cell(2,2,b), cell(1,3,b), cell(3,3,b).


% Caso X-O-X nella riga 3
next(A,B) : uniform([A,B],[[1,1],[3,1],[2,2]]) :- 
% next(1,1) : 0.33 ; next(3,1) : 0.33 ; next(2,2) : 0.33 :-
    \+empty_board, \+line, cell(1,3,x), cell(2,3,o), cell(3,3,x), cell(1,1,b), cell(3,1,b), cell(2,2,b).

next(1,1) : 0.5 ; next(3,1) : 0.5 :- 
    \+empty_board, \+line, cell(1,3,x), cell(2,3,o), cell(3,3,x), cell(1,1,b), cell(3,1,b), \+cell(2,2,b).

next(1,1) : 0.5 ; next(2,2) : 0.5 :-
    \+empty_board, \+line, cell(1,3,x), cell(2,3,o), cell(3,3,x), cell(1,1,b), \+cell(3,1,b), cell(2,2,b).

next(3,1) : 0.5 ; next(2,2) : 0.5 :-
    \+empty_board, \+line, cell(1,3,x), cell(2,3,o), cell(3,3,x), \+cell(1,1,b), cell(3,1,b), cell(2,2,b).


% --- METHOD B: Never Losing when Playing Second -------------------------
% 1a. Force a draw if the opponent starts in the corner: always put your first O in the center. 
% &&
% 3a. Try to win if the opponent starts at the edge: put your first O in the center. 
next(2,2) :- \+empty_board, \+line, cell(2,2,b).

% BOARD:
% (1,3) (2,3) (3,3)
% (1,2) (2,2) (3,2)
% (1,1) (2,1) (3,1)


% 1b. Your second O should be on an edge, not a corner (X NON ha giocato in edge, ha giocato in corner => tutti gli edge devono essere b)
next(2,1) : 0.25 ; next(1,2) : 0.25 ; next(3,2) : 0.25 ; next(2,3) : 0.25 :-
    \+empty_board, \+line, cell(2,1,b), cell(1,2,b), cell(3,2,b), cell(2,3,b).


% 3b. If your opponent puts the second X on the opposite edge, making a line X-O-X, put your second O in a corner. 
next(1,1) : 0.25 ; next(3,1) : 0.25 ; next(1,3) : 0.25 ; next(3,3) : 0.25 :-
    \+empty_board, \+line, cell(2,2,o), cell(2,3,x), cell(2,1,x).

next(1,1) : 0.25 ; next(3,1) : 0.25 ; next(1,3) : 0.25 ; next(3,3) : 0.25 :-
    \+empty_board, \+line, cell(2,2,o), cell(1,2,x), cell(3,2,x).


% 2. Force a draw when the opponent starts in the center: put your first X in a corner. 
% After that, just keep blocking your opponent from scoring and the game will be a draw
next(1,1) : 0.25 ; next(3,1) : 0.25 ; next(1,3) : 0.25 ; next(3,3) : 0.25 :-
    \+empty_board, \+line, cell(2,2,x).  


% ------------------------------------------------------------------------------

% % CASO in cui non esiste nessuna mossa vincente (non vincerà nessuno, ma bisogna terminare) 
% oppure è rimasta una sola cella vuota 
% => mossa casuale tra in una dell celle libere rimaste
next(C,R) : uniform((C,R),L) :- findall((C,R),cell(C,R,b),L).


:- end_lpad.

% ========================================================================

% aggiunge fatti cell al db (NOTA: assert è deprecato, assertz è equivalente)
assert_board([C11,C21,C31, C12,C22,C32, C13,C23,C33]) :- 
    % BOARD:
    % (1,3) (2,3) (3,3)
    % (1,2) (2,2) (3,2)
    % (1,1) (2,1) (3,1)

    % asserta o assertz, non cambia
    asserta(cell(1,1,C11)),
    asserta(cell(2,1,C21)),
    asserta(cell(3,1,C31)),
    
    asserta(cell(1,2,C12)),
    asserta(cell(2,2,C22)),
    asserta(cell(3,2,C32)),
    
    asserta(cell(1,3,C13)),
    asserta(cell(2,3,C23)),
    asserta(cell(3,3,C33)).


% ?- mc_sample_arg_raw(pair_(A,B),100,(A,B),L).
% pair_ per generare la stessa board
pair_(Board,L):-
    % set_random(seed(42)), % genera sempre la stessa board, per debug
    generate_random_board(Board),
    assert_board(Board),
	pair(Board,L),
    retractall(cell(_,_,_)), !.

pair_(_,_):-
    retract(player(_)),
    retractall(cell(_,_,_)).
    

pair(Board,[C,R]):-
    next_player(Board,Player),
    opponent(Player,Opponent),
    assertz(player(Player)),
    \+win(Player),\+win(Opponent),
    \+full_board,
    next(C,R),
    %display_game(Board), 
    retract(player(Player)),
    % write(Player), write(" next move in "), write([C,R]),
    write(Board), write('\t'), write(Player), write(" next move in "), write([C,R]),
    writeln("").

pair(Board,[]):-
    writeln(Board),
    win(Player),
    display_game(Board), 
    write(Player), write(" wins."),
    writeln(""),
    writeln('-----------').




% rowwin
win_([P,P,P,_,_,_,_,_,_],P) :- P \== b.
win_([_,_,_,P,P,P,_,_,_],P) :- P \== b.
win_([_,_,_,_,_,_,P,P,P],P) :- P \== b.
% colwin
win_([P,_,_,P,_,_,P,_,_],P) :- P \== b.
win_([_,P,_,_,P,_,_,P,_],P) :- P \== b.
win_([_,_,P,_,_,P,_,_,P],P) :- P \== b.
% diagwin
win_([P,_,_,_,P,_,_,_,P],P) :- P \== b.
win_([_,_,P,_,P,_,P,_,_],P) :- P \== b.


% da usare in valid se vogliamo tenere l'esempio con un Player che vince
both_win(Board):-
    win_(Board,x),
    win_(Board,o).
   

valid(Board):-
    findall(Index,nth0(Index,Board,o),L1),
    findall(Index,nth0(Index,Board,x),L2),
    length(L1,N1),
    length(L2,N2),
    V is abs(N1 - N2),
    V =< 1,
    \+ win_(Board,x),
    \+ win_(Board,o),
    member(b,Board).
    %\+ both_win(Board),
    
  

display_game([A,B,C,D,E,F,G,H,I]) :- 
    writeln([G,H,I]),
    writeln([D,E,F]),
    writeln([A,B,C]).




generate_dataset :-
    tell('tris_dataset.txt'),
    mc_sample_arg_raw(pair_(A,B),100,(A,B),_),
    told.
