% Projet prolog - ALIA - 4IF
% Jeremy gaillard, Benyoub Anis
% MasterLog, un jeu mastermind en prolog


% Methode d'inversion de listes
inv_pile([],X,X).
inv_pile([X|Y],Z,W) :- inv_pile(Y,[X|Z],W).
inv(X,Y):- inv_pile(X,[],Y).


% Fonctions de substitution
subs_pile(_,_,[],Z,Z).
subs_pile([],Y,[X|F],S,Z):-subs_pile([],Y,F,[X|S],Z).
subs_pile(X,Y,[X|F],S,Z):-subs_pile([],Y,F,[Y|S],Z).
subs_pile(X,Y,[Q|F],S,Z):-X\=Q,X\=[],subs_pile(X,Y,F,[Q|S],Z).
subs(X,Y,Z,S):-subs_pile(X,Y,Z,[],Q),inv(Q,S).

search(A,Y,p,Ys):-member(A,Y), subs(A,p,Y,Ys).
search(A,Y,f,Y):-not(member(A,Y)).

% Correction exacte
% X combinaison proposée partiellement corrigée , Y combinaison juste partiellement corrigée , R correction finale
testPresence2([],_,Z,Z).
testPresence2([v|X],Y,Z,R):-testPresence2(X,Y,[v|Z],R).
testPresence2([A|X],Y,Z,R):-A\=v,search(A,Y,D,Ys),testPresence2(X,Ys,[D|Z],R).
testPresence(X,Y,R):-testPresence2(X,Y,[],S),inv(S,R).


        
        
% Correction exacte
% X combinaison proposée, Y combinaison juste, RX correction partielle de X, RY correction partielle de Y
testExact2([],[],ZX,ZX,ZY,ZY).     
testExact2([X|K],[X|O],ZX,RX,ZY,RY):-testExact2(K,O,[v|ZX],RX,[v|ZY],RY).     
testExact2([X|K],[Y|O],ZX,RX,ZY,RY):- X\=Y,testExact2(K,O,[X|ZX],RX,[Y|ZY],RY).  
testExact(X,Y,RX,RY):-testExact2(X,Y,[],FX,[],FY),inv(FX,RX),inv(FY,RY). 

% Le deuxième paramètre prend la valeur 1 si la combinaison est gagnante (que des v), 0 sinon
testVictory([], 1).
testVictory([v|R], V):-testVictory(R, V).
testVictory([X|_], 0):-X\=v.

% Correction d'une tentative, X: Combinaison proposée, Y: Combinaison exacte, S: correction   
testCombinaison(X,Y,S):-testExact(X,Y,RX,RY),testPresence(RX,RY,S).


tour(_,_,1):-write('Victory !\n').
tour(_,0,0):-write('You just lost the game\n').
tour(X,Y,0):- Y\=0,write('Try to guess the combination\n'),read(H),testCombinaison(H,X,J),write(J),testVictory(J,V),Z is Y -1,tour(X,Z,V).



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




% Fonction de résolution 
% S: Solution , Li liste des élèments possibles, Sf: Solution finale proposée, Ls :  Liste des valeurs trouvées, C: Variables existantes , AR : resultat précédent 
solve(_,_,_,_,_,_,[v,v,v,v]):-write('I\'ve found it !').
solve(S,Li,Sf,Ls,Lp,C,AR):-AR \= [v,v,v,v],propose(0,Li,Lp,Ls,C,[],Sp),write('Maybe this?\n'),write( Sp ),write( '\n' ), testCombinaison(Sp,S,R),write( R ),write('\n'	),read(OP),newList(Li,C,Ls,Lp,R,Sp,NLi,[],Nlpf,NC),solve(S,NLi,Sf,Ls,Nlpf,NC,R).



% S : solution, Li : liste des couleurs possibles, Sf : solution trouvée par l'IA
machine(S,Li,Sf):-solve(S,Li,Sf,[X,Y,Z,D],[[],[],[],[]],[],[]).

