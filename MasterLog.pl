% Projet prolog - ALIA - 4IF
% Jeremy gaillard, Benyoub Anis
% MasterLog, un jeu mastermind en prolog


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

% enlève l'élément X de la liste passé en second paramètre
%extract(X, [X|L], L).
%extract(X, [], X).
%extract(X, [T|R], [T|L] ):-extract(X, R, L).
% appel a subtract car celui-ci renvoie vrai même si l'élément n'appartient pas a la liste de départ
extract(X,Y,Z):-subtract(Y,[X],Z).

% Met a jour les listes Li (liste initiale comportant les couleurs non encore testées), Lp (contient pour chaque position, la liste des couleurs
% interdites), C (liste des couleurs contenues dans la combinaison)
%
% Toute les infos ont été traitées : on copie C dans NC, Li, dans NLi, et on inverse NLp
newList(NLi,NC,[],_,[],[],NLi,NLp,NLp2,NC):- inv(NLp,NLp2).

newList(Li,C,[_|Ls],[D|Lp],[f|R],[I|Sp],NLi,NLp,NLpf,NC):-extract(I,Li,Li2),newList(Li2,C,Ls,Lp,R,Sp,NLi,[D|NLp],NLpf,NC).
%newList(Li,C,[_|Ls],[D|Lp],[f|R],[I|Sp],NLi,NLp,NLpf,NC):-newList(Li,C,Ls,Lp,R,Sp,NLi,[D|NLp],NLpf,NC).

newList(Li,C,[I|Ls],[D|Lp],[v|R],[I|Sp],NLi,NLp,NLpf,NC):-extract(I,Li,Li2),newList(Li2,[I|C],Ls,Lp,R,Sp,NLi,[D|NLp],NLpf,NC).
%newList(Li,C,[I|Ls],[D|Lp],[v|R],[I|Sp],NLi,NLp,NLpf,NC):-newList(Li,[I|C],Ls,Lp,R,Sp,NLi,[D|NLp],NLpf,NC).

newList(Li,C,[_|Ls],[D|Lp],[p|R],[I|Sp],NLi,NLp,NLpf,NC):-extract(I,Li,Li2),newList(Li2,[I|C],Ls,Lp,R,Sp,NLi,[[I|D]|NLp],NLpf,NC).
%newList(Li,C,[_|Ls],[D|Lp],[p|R],[I|Sp],NLi,NLp,NLpf,NC):-newList(Li,[I|C],Ls,Lp,R,Sp,NLi,[[I|D]|NLp],NLpf,NC).

%propose(4,NLI,_,NC,S,Sk,NC,NLI):-inv(S,Sk).
%propose(Size,[],Lp,C,S,Sp,NC,NLI):- propose(Size,C,Lp,C,S,Sp,NC,NLI).
%propose(Size,[D|Li],[O|Lp],C,S,Sp,NC,NLI):-D \= [], Size1 is Size+1,not(member(D,O)),propose(Size1,Li,Lp,C,[D|S],Sp,NC,NLI).
%propose(Size,[D|Li],Lp,C,S,Sp,NC,NLI):-D \= [], propose(Size,Li,Lp,C,S,Sp,NC,NLI).
propose(4,_,_,_,_,S,Sk):-inv(S,Sk).
propose(Size,[],Lp,Ls,C,S,Sp):- propose2(Size,C,Lp,Ls,C,S,Sp).
% On dépile un élément de Li, si celui-ci n'est pas interdit à la position actuelle, on le rajoute à la solution proposée
propose(Size,[D|Li],[O|Lp],[_|Ls],C,S,Sp):-D \= [], Size1 is Size+1,not(member(D,O)),propose(Size1,Li,Lp,Ls,C,[D|S],Sp).
% Dans le cas contraire, on prend l'élément suivant de Li
propose(Size,[D|Li],Lp,Ls,C,S,Sp):-D \= [], propose(Size,Li,Lp,Ls,C,S,Sp).

% Si toutes les couleurs ont été trouvées et seules les positions doivent être devinées
propose2(4,_,_,_,_,S,Sk):-inv(S,Sk).
% Si Li est vide, on la remplie par la liste des couleurs trouvées
propose2(Size,[],Lp,Ls,C,S,Sp):-propose2(Size,C,Lp,Ls,C,S,Sp).
propose2(Size,Li,[_|Lp],[P|Ls],C,S,Sp):-nonvar(P), Size1 is Size+1,extract(P,Li,Li2),propose2(Size1,Li2,Lp,Ls,C,[P|S],Sp).
propose2(Size,[D|Li],[O|Lp],[P|Ls],C,S,Sp):-var(P),D \= [], Size1 is Size+1,not(member(D,O)),propose2(Size1,Li,Lp,Ls,C,[D|S],Sp).
propose2(Size,[D|Li],Lp,Ls,C,S,Sp):-var(P),D \= [], propose2(Size,Li,Lp,Ls,C,S,Sp).

solve(_,_,_,_,_,_,[v,v,v,v]):-write('I\'ve found it !').
solve(S,Li,Sf,Ls,Lp,C,AR):-AR \= [v,v,v,v],propose(0,Li,Lp,Ls,C,[],Sp),write('Maybe this?\n'),write( Sp ),write( '\n' ), testComb(Sp,S,R),write( R ),write('\n'	),read(OP),newList(Li,C,Ls,Lp,R,Sp,NLi,[],Nlpf,NC),solve(S,NLi,Sf,Ls,Nlpf,NC,R).

% S : solution, Li : liste des couleurs possibles, Sf : solution trouvée par l'IA
machine(S,Li,Sf):-solve(S,Li,Sf,[X,Y,Z,D],[[],[],[],[]],[],[]).

%newList([1,2,3,4,5,6,7,8],C,[X,Y,Z,D],[[],[],[],[]],[p,v,f,f],[1,2,7,6],J,[],Q,P).
