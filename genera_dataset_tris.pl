:- use_module(library(lists)).
:- use_module(library(assert)).


%%% predicati per determinare a chi tocca giocare %%%
% count(_,CT,[],CT).
% count(S,C0,[S|T],CT):-
%     C1 is C0 + 1,
%     count(S,C1,T,CT).
% count(S,C0,[S1|T],CT):-
%     S \= S1,
%     count(S,C0,T,CT).

% count_symbols_diff(CO,CX,o):- CO =:= CX - 1.
% count_symbols_diff(CO,CX,x):- CX =:= CO - 1.

% next_player(_,Board,NextPlayer):-
% 	count(o,0,Board,CO),
%     count(x,0,Board,CX),
%     count_symbols_diff(CO,CX,NextPlayer).

% next_player(FirstPlayer,Board,FirstPlayer):-
%     count(o,0,Board,Count),
%     count(x,0,Board,Count).


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

win(P) :- cell(1,1,P), cell(1,2,P), cell(1,1,P), P \== b. % colwin
win(P) :- cell(2,1,P), cell(2,2,P), cell(2,1,P), P \== b. % colwin
win(P) :- cell(3,1,P), cell(3,2,P), cell(3,1,P), P \== b. % colwin

win(P) :- cell(1,1,P), cell(2,2,P), cell(3,3,P), P \== b. % diagwin
win(P) :- cell(1,1,P), cell(2,2,P), cell(3,3,P), P \== b. % diagwin


opponent(x,o).
opponent(o,x).


% === NEXT MOVE ==============================================================

% --- GENERAL RULE: se ci sono due simboli in fila, metti il terzo -----------
% Regola valida sia per vincere, sia per bloccare l'avversario
% line(Col,Row) restituisce la colonna e la riga in cui giocare

line(C,1) :- cell(C,1,b), cell(C,2,S), cell(C,3,S), S \== b.
line(C,2) :- cell(C,1,S), cell(C,2,b), cell(C,3,S), S \== b.
line(C,3) :- cell(C,1,S), cell(C,2,S), cell(C,3,b), S \== b.

line(1,R) :- cell(1,R,b), cell(2,R,S), cell(3,R,S), S \== b.
line(2,R) :- cell(1,R,S), cell(2,R,b), cell(3,R,S), S \== b.
line(3,R) :- cell(1,R,S), cell(2,R,S), cell(3,R,b), S \== b.

line(1,1) :- cell(1,1,b), cell(2,2,S), cell(3,3,S), S \== b.
line(2,2) :- cell(1,1,S), cell(2,2,b), cell(3,3,S), S \== b.
line(3,3) :- cell(1,1,S), cell(2,2,S), cell(3,3,b), S \== b.

line(1,1) :- cell(1,3,b), cell(2,2,S), cell(3,1,S), S \== b.
line(2,2) :- cell(1,3,S), cell(2,2,b), cell(3,1,S), S \== b.
line(3,3) :- cell(1,3,S), cell(2,2,S), cell(3,1,b), S \== b.

line :- line(C,R).

next(C,R) :- line(C,R).


% --- PLAYER 1 (x) -------------------------------------------------------
% NOTA: Xn, On = n-esima mossa di x/o

% 1. X1 in a corner (4 probabilistic alternatives).
0.25 :: next(1,1) ; 0.25 :: next(3,1) ; 0.25 :: next(1,3) ; 0.25 :: next(3,3) :- empty_board.

% 2a. O1 in the CENTER.
% 2aa. X2 opposite CORNER from X1 (so there's a line going "X O X" diagonally across the board) 
% OR 
% 2ab. X2 on an EDGE square (not a corner), NOT touching your first X.  
0.33 :: next(2,3); 0.33 :: next(3,3); 0.33 :: next(3,2) :- 
    \+empty_board, \+line, cell(2,2,o), cell(1,1,x), cell(2,1,b), cell(3,1,b), cell(1,2,b), cell(3,2,b), cell(1,3,b), cell(2,3,b), cell(3,3,b).

0.33 :: next(1,1); 0.33 :: next(2,3); 0.33 :: next(1,2) :- 
    \+empty_board, \+line, cell(2,2,o), cell(3,1,x), cell(1,1,b), cell(2,1,b), cell(1,2,b), cell(3,2,b), cell(1,3,b), cell(2,3,b), cell(3,3,b).

0.33 :: next(1,2); 0.33 :: next(1,1); 0.33 :: next(2,1) :- 
    \+empty_board, \+line, cell(2,2,o), cell(3,3,x), cell(1,1,b), cell(2,1,b), cell(3,1,b), cell(1,2,b), cell(3,2,b), cell(1,3,b), cell(2,3,b).

0.33 :: next(3,2); 0.33 :: next(2,1); 0.33 :: next(3,1) :- 
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
0.5 :: next(1,1); 0.5 :: next(3,3) :- 
    \+empty_board, \+line, cell(1,3,x), cell(2,2,b), cell(1,1,b), cell(3,3,b), cell(2,3,b), cell(1,2,b), cell(3,2,o).
0.5 :: next(1,1); 0.5 :: next(3,3) :- 
    \+empty_board, \+line, cell(1,3,x), cell(2,2,b), cell(1,1,b), cell(3,3,b), cell(2,3,b), cell(1,2,b), cell(2,1,o).
0.5 :: next(1,1); 0.5 :: next(3,3) :- 
    \+empty_board, \+line, cell(1,3,x), cell(2,2,b), cell(1,1,b), cell(3,3,b), cell(2,3,b), cell(1,2,b), cell(3,1,o).
next(3,3) :- \+empty_board, \+line, cell(1,3,x), cell(2,2,b), cell(3,3,b), cell(2,3,b). 
next(1,1) :- \+empty_board, \+line, cell(1,3,x), cell(2,2,b), cell(1,1,b), cell(2,1,b). 
% next(3,1) :- \+empty_board, \+line, cell(1,3,x), cell(2,2,b), cell(3,1,b). 

% - Caso in cui player1 inizia in (3,3)
0.5 :: next(1,3); 0.5 :: next(3,1) :-
    \+empty_board, \+line, cell(3,3,x), cell(2,2,b), cell(1,3,b), cell(3,1,b), cell(2,3,b), cell(3,2,b), cell(1,2,o).
0.5 :: next(1,3); 0.5 :: next(3,1) :-
    \+empty_board, \+line, cell(3,3,x), cell(2,2,b), cell(1,3,b), cell(3,1,b), cell(2,3,b), cell(3,2,b), cell(1,1,o).
0.5 :: next(1,3); 0.5 :: next(3,1) :-
    \+empty_board, \+line, cell(3,3,x), cell(2,2,b), cell(1,3,b), cell(3,1,b), cell(2,3,b), cell(3,2,b), cell(3,1,o).
next(1,3) :- \+empty_board, \+line, cell(3,3,x), cell(2,2,b), cell(1,3,b), cell(2,3,b).
next(3,1) :- \+empty_board, \+line, cell(3,3,x), cell(2,2,b), cell(3,1,b), cell(3,2,b).
% next(1,1) :- \+empty_board, \+line, cell(3,3,x), cell(2,2,b), cell(1,1,b).

% - Caso in cui player1 inizia in (1,1)
0.5 :: next(1,3); 0.5 :: next(3,1) :-
    \+empty_board, \+line, cell(1,1,x), cell(2,2,b), cell(1,3,b), cell(3,1,b), cell(1,2,b), cell(2,1,b), cell(2,3,o).
0.5 :: next(1,3); 0.5 :: next(3,1) :-
    \+empty_board, \+line, cell(1,1,x), cell(2,2,b), cell(1,3,b), cell(3,1,b), cell(1,2,b), cell(2,1,b), cell(3,3,o).
0.5 :: next(1,3); 0.5 :: next(3,1) :-
    \+empty_board, \+line, cell(1,1,x), cell(2,2,b), cell(1,3,b), cell(3,1,b), cell(1,2,b), cell(2,1,b), cell(3,2,o).
next(1,3) :- \+empty_board, \+line, cell(1,1,x), cell(2,2,b), cell(1,3,b), cell(1,2,b).
next(3,1) :- \+empty_board, \+line, cell(1,1,x), cell(2,2,b), cell(3,1,b), cell(2,1,b).
% next(3,3) :- \+empty_board, \+line, cell(1,1,x), cell(2,2,b), cell(3,3,b).

% - Caso in cui player1 inizia in (3,1)
0.5 :: next(1,1); 0.5 :: next(3,3) :- 
    \+empty_board, \+line, cell(3,1,x), cell(2,2,b), cell(1,1,b), cell(3,3,b), cell(3,2,b), cell(2,1,b), cell(1,3,o).
0.5 :: next(1,1); 0.5 :: next(3,3) :- 
    \+empty_board, \+line, cell(3,1,x), cell(2,2,b), cell(1,1,b), cell(3,3,b), cell(3,2,b), cell(2,1,b), cell(2,3,o).
0.5 :: next(1,1); 0.5 :: next(3,3) :- 
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
0.33 :: next(3,1); 0.33 :: next(2,2); 0.33 :: next(3,3) :- 
    \+empty_board, \+line, cell(1,1,x), cell(1,2,o), cell(1,3,x), cell(3,1,b), cell(2,2,b), cell(3,3,b).

0.5 :: next(3,1); 0.5 :: next(2,2) :- 
    \+empty_board, \+line, cell(1,1,x), cell(1,2,o), cell(1,3,x), cell(3,1,b), cell(2,2,b), \+cell(3,3,b).

0.5 :: next(3,1); 0.5 :: next(3,3) :- 
    \+empty_board, \+line, cell(1,1,x), cell(1,2,o), cell(1,3,x), cell(3,1,b), \+cell(2,2,b), cell(3,3,b).

0.5 :: next(2,2); 0.5 :: next(3,3) :- 
    \+empty_board, \+line, cell(1,1,x), cell(1,2,o), cell(1,3,x), \+cell(3,1,b), cell(2,2,b), cell(3,3,b).


% Caso X-O-X nella colonna 3
0.33 :: next(1,1); 0.33 :: next(2,2); 0.33 :: next(1,3) :- 
    \+empty_board, \+line, cell(3,1,x), cell(3,2,o), cell(3,3,x), cell(1,1,b), cell(2,2,b), cell(1,3).

0.5 :: next(1,1); 0.5 :: next(2,2) :- 
    \+empty_board, \+line, cell(3,1,x), cell(3,2,o), cell(3,3,x), cell(1,1,b), cell(2,2,b), \+cell(1,3).

0.5 :: next(1,1); 0.5 :: next(1,3) :- 
    \+empty_board, \+line, cell(3,1,x), cell(3,2,o), cell(3,3,x), cell(1,1,b), \+cell(2,2,b), cell(1,3).

0.5 :: next(2,2); 0.5 :: next(1,3) :- 
    \+empty_board, \+line, cell(3,1,x), cell(3,2,o), cell(3,3,x), \+cell(1,1,b), cell(2,2,b), cell(1,3).


% Caso X-O-X nella riga 1
0.33 :: next(2,2); 0.33 :: next(1,3); 0.33 ::  next(3,3) :- 
    \+empty_board, \+line, cell(1,1,x), cell(2,1,o), cell(3,1,x), cell(2,2,b), cell(1,3,b), cell(3,3,b).

0.5 :: next(2,2); 0.5 :: next(1,3) :- 
    \+empty_board, \+line, cell(1,1,x), cell(2,1,o), cell(3,1,x), cell(2,2,b), cell(1,3,b), \+cell(3,3,b).

0.5 :: next(2,2); 0.5 :: next(3,3) :- 
    \+empty_board, \+line, cell(1,1,x), cell(2,1,o), cell(3,1,x), cell(2,2,b), \+cell(1,3,b), cell(3,3,b).

0.5 :: next(1,3); 0.5 :: next(3,3) :- 
    \+empty_board, \+line, cell(1,1,x), cell(2,1,o), cell(3,1,x), \+cell(2,2,b), cell(1,3,b), cell(3,3,b).


% Caso X-O-X nella riga 3
0.33 :: next(1,1); 0.33 :: next(3,1); 0.33 :: next(2,2) :- 
    \+empty_board, \+line, cell(1,3,x), cell(2,3,o), cell(3,3,x), cell(1,1,b), cell(3,1,b), cell(2,2,b).

0.5 :: next(1,1); 0.5 :: next(3,1); :- 
    \+empty_board, \+line, cell(1,3,x), cell(2,3,o), cell(3,3,x), cell(1,1,b), cell(3,1,b), \+cell(2,2,b).

0.5 :: next(1,1); 0.5 :: next(2,2) :- 
    \+empty_board, \+line, cell(1,3,x), cell(2,3,o), cell(3,3,x), cell(1,1,b), \+cell(3,1,b), cell(2,2,b).

0.5 :: next(3,1); 0.5 :: next(2,2) :- 
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
0.25 :: next(2,1); 0.25 :: next(1,2); 0.25 :: next(3,2); 0.25 :: next(2,3) :- 
    \+empty_board, \+line, cell(2,1,b), cell(1,2,b), cell(3,2,b), cell(2,3,b).


% 3b. If your opponent puts the second X on the opposite edge, making a line X-O-X, put your second O in a corner. 
0.25 :: next(1,1) ; 0.25 :: next(3,1) ; 0.25 :: next(1,3) ; 0.25 :: next(3,3) :- 
    \+empty_board, \+line, cell(2,2,o), cell(2,3,x), cell(2,1,x).

0.25 :: next(1,1) ; 0.25 :: next(3,1) ; 0.25 :: next(1,3) ; 0.25 :: next(3,3) :- 
    \+empty_board, \+line, cell(2,2,o), cell(1,2,x), cell(3,2,x).


% 2. Force a draw when the opponent starts in the center: put your first X in a corner. 
% After that, just keep blocking your opponent from scoring and the game will be a draw
0.25 :: next(1,1) ; 0.25 :: next(3,1) ; 0.25 :: next(1,3) ; 0.25 :: next(3,3) :- 
    \+empty_board, \+line, cell(2,2,x). 


% ------------------------------------------------------------------------------

% % CASO in cui non esiste nessuna mossa vincente (non vincerà nessuno, ma bisogna terminare) 
% oppure è rimasta una sola cella vuota 
% => mossa casuale tra in una dell celle libere rimaste
next(C,R) : uniform((C,R),L) :- findall((C,R),cell(C,R,b),L).

% ========================================================================


% aggiunge fatti cell al db (NOTA: assert è deprecato, assertz è equivalente)
assert_board([C11,C12,C13,C21,C22,C23,C31,C32,C33]) :- 
    assertz(cell(1,1,C11)),
    assertz(cell(1,2,C12)),
    assertz(cell(1,3,C13)),
    assertz(cell(2,1,C21)),
    assertz(cell(2,2,C22)),
    assertz(cell(2,3,C23)),
    assertz(cell(3,1,C31)),
    assertz(cell(3,2,C32)),
    assertz(cell(3,3,C33)).

% NOTA: setarg dà problemi in problog
next_board(Board, Player, NewBoard) :-
    % assert_board(Board), IN play
    next(C,R), 
    % Metto Player in posizione C,R nella Board:
    F =..[f|Board], % crea termine f che ha 9 elem, quelli di Board
    Pos is ((3-R)*3+C),
    setarg(Pos,F,Player), % setarg(Arg,Term,NewVal): dato un termine Term, mette NewVal in Term alla posizione Arg
    F =..[f|NewBoard]. % riprende args e li mette in NewB
    % retractall(cell(_,_,_)). % cancella tutti i fatti x cell dal db


% display_game([A,B,C,D,E,F,G,H,I]) :- 
%     writeln([A,B,C]),
%     writeln([D,E,F]),
%     writeln([G,H,I]),
%     writeln('-------').


% BOARD:
% (1,3) (2,3) (3,3)
% (1,2) (2,2) (3,2)
% (1,1) (2,1) (3,1)

display_game :- 
    cell(1,3,S13),cell(2,3,S23),cell(3,3,S33),
    cell(1,2,S12),cell(2,2,S22),cell(3,2,S32),
    cell(1,1,S11),cell(2,1,S21),cell(3,1,S31),
    write(S13),write(S23),write(S33),
    write(S12),write(S22),write(S32),
    write(S11),write(S21),write(S31),
    writeln('------').


% Partita finita in parità
play(_,Board,b) :-
    assert_board(Board),
    \+member(b,Board), % full_board(Board),
    \+win(x), \+win(o),
    display_game,
    retractall(cell(_,_,_)).

% Board vincente per Player
play(_,Board,Player) :-
    assert_board(Board),
    display_game,
    win(Player),
    retractall(cell(_,_,_)).

% Player non vince, fa la sua mossa, quindi passa il turno all'Opponent
% Player1 sempre X
play(Board,Player,Winner) :-
    % serve specificare che la board non è ancora complet? member(b,Board),
    assert_board(Board),
    \+win(Player),
    next_board(Board,Player,NewBoard),
    display_game,
    retractall(cell(_,_,_)),
    opponent(Player,Opp),
    play(NewBoard,Opp,Winner).

game :-
    Board = [b,b,b,b,b,b,b,b,b],
    assert_board(Board),
    play(Board,x,Winner),
    write(Player), write(' wins.\n').


% TODO:
% - aggiungere tell, told x salvare su file


% nn(sudoku_net,[X],Y,[b,o,x])::cell(C,R,Y) :- image(C,R,X).
% %esempio descritto da 9 atomi per image + fatto game(Outcome) o mossa da fare
% game([A,B,C,D,E,F,G,H,I], Outcome):-
%     element(A,A1),
%     element(B,B1),
%     element(C,C1),

%     element(D,D1),
%     element(E,E1),
%     element(F,F1),

%     element(G,G1),
%     element(H,H1),
%     element(I,I1),

%     image(1,1,A)

%     Board1 = [A1,B1,C1,D1,E1,F1,G1,H1,I1],

%     play(x,Board1,Outcome).




