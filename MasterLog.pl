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
        process_stream(Char2, In,Y,[U|Z]).
        
        
        
% Fonctions de substitution
subs_pile(X,Y,[],Z,Z).
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

verif([],ok):-write('You just won the match\n').
verif([v|J],F):-verif(J,F).

verif([X|J],notok):- X\=v,write('Not ok Continue').

testComb(X,Y,S):-testExact(X,Y,R),replace(R,Y,S).

tour(X,-1):-write('You just lost the game').
tour(X,Y):- Y\=0,read(H),testComb(H,X,J),write(J),verif(J,D),write(D), D==ok,write('Hurray!!!').


tour(X,Y):-Y \=0,read(H),testComb(H,X,J),write(J),verif(J,D),write(D),D==notok,Z is Y -1,tour(X,Z).


startGame(X,Y):-Z is Y -1,tour(X,Z).

launch(X):-write(''),getCombination('logs',InitCombination,[]),inv(InitCombination,Patate),write(Patate),startGame(Patate,9).

play(Y):-read(X),write(X),startGame(X,Y).

