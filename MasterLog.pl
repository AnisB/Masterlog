% Projet prolog - ALIA - 4IF
% Jeremy gaillard, Benyoub Anis
% MasterLog, un jeu mastermind en prolog

couleur(1).
couleur(2).
couleur(3).
couleur(4).
couleur(5).
couleur(6).
couleur(7).
couleur(8).


% Methode d'inversion de listes
inv_pile([],X,X).
inv_pile([X|Y],Z,W) :- inv_pile(Y,[X|Z],W).
inv(X,Y):- inv_pile(X,[],Y).


% Methode de récupération de combinaison stockée dans le fichier X
getCombination(X,Y,Z):-readFile(X,Y,Z).


% Lecture du fichier File
readFile(File,Y,Z):-
        open(File, read, In),
        get_char(In, Char1),
        process_stream(Char1, In,Y,Z),
        close(In).


% Lecture des caractères
process_stream(end_of_file, _,Y,Y) :- !.
      
process_stream(' ', In,Y,Z) :-
        get_char(In, Char2),
        process_stream(Char2, In,Y,Z).      
process_stream(Char, In,Y,Z) :-
		Char \=' ',
        get_char(In, Char2),
        process_stream(Char2, In,Y,[_|Z]).
        
        
        
% Fonctions de substitution
subs_pile(_,_,[],Z,Z).
subs_pile([],Y,[X|F],S,Z):-subs_pile([],Y,F,[X|S],Z).
subs_pile(X,Y,[X|F],S,Z):-subs_pile([],Y,F,[Y|S],Z).
subs_pile(X,Y,[Q|F],S,Z):-X\=Q,X\=[],subs_pile(X,Y,F,[Q|S],Z).
subs(X,Y,Z,S):-subs_pile(X,Y,Z,[],Q),inv(Q,S).

search(A,Y,p,Ys):-member(A,Y), subs(A,p,Y,Ys).
search(A,Y,f,Y):-not(member(A,Y)).

replace2([],_,Z,Z).
replace2([v|X],Y,Z,R):-replace2(X,Y,[v|Z],R).
replace2([A|X],Y,Z,R):-A\=v,search(A,Y,f,Ys),replace2(X,Ys,[f|Z],R).
replace2([A|X],Y,Z,R):-A\=v,search(A,Y,p,Ys),replace2(X,Ys,[p|Z],R).
replace(X,Y,R):-replace2(X,Y,[],S),inv(S,R).




        
        
% Correction d'une tentative, X: Combinaison proposée, Y: Combinaison exacte, Z: correction   

% Correction exacte
testExact2([],[],Z,Z).     
testExact2([X|K],[X|O],Z,R):-testExact2(K,O,[v|Z],R).     
testExact2([X|K],[Y|O],Z,R):- X\=Y,testExact2(K,O,[X|Z],R).  
testExact(X,Y,R):-testExact2(X,Y,[],F),inv(F,R).   

% Le deuxième paramètre prend la valeur 1 si la combinaison est gagnante (que des v), 0 sinon
testVictory([], 1).
testVictory([v|R], V):-testVictory(R, V).
testVictory([X|_], 0):-X\=v.

% X : combinaison proposée, Y : combinaison juste, S : correction
testComb(X,Y,S):-testExact(X,Y,R),replace(R,Y,S).


tour(_,_,1):-write('Victory !\n').
tour(_,0,0):-write('You just lost the game\n').
tour(X,Y,0):- Y\=0,write('Try to guess the combination\n'),read(H),testComb(H,X,J),write(J),testVictory(J,V),Z is Y -1,tour(X,Z,V).



startGame(X,Y):-tour(X,Y,0).

%launch(X):-write(''),getCombination('logs',InitCombination,[]),inv(InitCombination,Patate),write(Patate),startGame(Patate,9).

% Y : nombre de tours
play(Y):-read(X),write(X),startGame(X,Y).

