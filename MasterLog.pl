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
        process_stream(Char2, In,Y,[Char|Z]).
        
        
% Correction d'une tentative, X: Combinaison proposée, Y: Combinaison exacte, Z: correction   

% Correction exacte
testExact2([],[],Z,Z).     
testExact2([X|K],[X|O],Z,R):-testExact2(K,O,[v|Z],R).     
testExact2([X|K],[Y|O],Z,R):- X\=Y,testExact2(K,O,[f|Z],R).  
testExact(X,Y,R):-testExact2(X,Y,[],F),inv(F,R).   


% Correction reste

remplacer([],[],[],X,Y,X,Y).
remplacer([D|X],[S|Y],[f|Z],Xp,Yp,Xs,Ys):-remplacer(X,Y,Z,[D|Xp],[S|Yp],Xs,Ys).
remplacer([D|X],[S|Y],[v|Z],Xp,Yp,Xs,Ys):-remplacer(X,Y,Z,[v|Xp],[v|Yp],Xs,Ys).
testReste(X,Y,Z):-remplacer(X,Y,Z,[],[],Xs,Ys),inv(Xs,Xu),inv(Ys,Yu).

test1(S):-testExact([a,b,d],[a,b,c],R),testReste([a,b,d],[a,b,c],R).

%test(X,Y,Z):-


%startGame©:-



launch(X):-write(''),getCombination('logs',InitCombination,[]),inv(InitCombination,Patate),startGame(Patate).
