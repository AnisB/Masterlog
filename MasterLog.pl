% Projet prolog - ALIA - 4IF
% Jeremy gaillard, Benyoub Anis
% MasterLog, un jeu mastermind en prolog


% Methode d'inversion de listes
% X : Liste de depart, Y liste de retour
inv_pile([],X,X).
inv_pile([X|Y],Z,W) :- inv_pile(Y,[X|Z],W).
inv(X,Y):- inv_pile(X,[],Y).


% Fonctions de substitution
% X : element à substituer, Y: substitut , Z liste de depart , S liste avec substituion
subs_pile(_,_,[],Z,Z).
subs_pile([],Y,[X|F],S,Z):-subs_pile([],Y,F,[X|S],Z).
subs_pile(X,Y,[X|F],S,Z):-subs_pile([],Y,F,[Y|S],Z).
subs_pile(X,Y,[Q|F],S,Z):-X\=Q,X\=[],subs_pile(X,Y,F,[Q|S],Z).
subs(X,Y,Z,S):-subs_pile(X,Y,Z,[],Q),inv(Q,S).



% A : element à chercher, Y : liste contenu, p : retour de la regle, Ys : avec element substitué
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


% Correction d'une tentative, X: Combinaison proposée, Y: Combinaison exacte, S: correction   
testCombinaison(X,Y,S):-testExact(X,Y,RX,RY),testPresence(RX,RY,S).


% Regle qui permet de savoir si une liste es uniquement composée de v
% Le deuxième paramètre prend la valeur 1 si la combinaison est gagnante (que des v), 0 sinon
testVictory([], 1).
testVictory([v|R], V):-testVictory(R, V).
testVictory([X|_], 0):-X\=v.



% Regle qui permet à un joueur de jouer contre un autre
% X la combinaison gagnante
% Y la le nombre de tour restants
tour(_,_,1):-write('Victory !\n').
tour(_,0,0):-write('You just lost the game\n').
tour(X,Y,0):- Y\=0,write('Try to guess the combination\n'),read(H),testCombinaison(H,X,J),write(J),testVictory(J,V),
																							Z is Y -1,tour(X,Z,V).
% Lancement d'une partie sans IA
startGame(X,Y):-tour(X,Y,0).


%IA

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
newList(Li,C,[_|Ls],[D|Lp],[f|R],[I|Sp],NLi,NLp,NLpf,NC):-extract(I,Li,Li2),newList(Li2,C,Ls,Lp,R,Sp,NLi,[[I|D]|NLp],NLpf,NC).
newList(Li,C,[I|Ls],[D|Lp],[v|R],[I|Sp],NLi,NLp,NLpf,NC):-extract(I,Li,Li2),newList(Li2,[I|C],Ls,Lp,R,Sp,NLi,[D|NLp],NLpf,NC).
newList(Li,C,[_|Ls],[D|Lp],[p|R],[I|Sp],NLi,NLp,NLpf,NC):-extract(I,Li,Li2),newList(Li2,[I|C],Ls,Lp,R,Sp,NLi,[[I|D]|NLp],NLpf,NC).


% Methode de proposiition qui est utlisée tant que le nombre de couleurs est inférieur au nombre de variables
proposeCol(0,_,_,_,_,S,Sk):-inv(S,Sk).
proposeCol(Size,[],Lp,Ls,C,S,Sp):- proposePos(Size,C,Lp,Ls,C,S,Sp).
% On dépile un élément de Li, si celui-ci n'est pas interdit à la position actuelle, on le rajoute à la solution proposée
proposeCol(Size,[D|Li],[O|Lp],[_|Ls],C,S,Sp):-D \= [], Size1 is Size-1,not(member(D,O)),proposeCol(Size1,Li,Lp,Ls,C,[D|S],Sp).
% Dans le cas contraire, on prend l'élément suivant de Li
proposeCol(Size,[D|Li],Lp,Ls,C,S,Sp):-D \= [], proposeCol(Size,Li,Lp,Ls,C,S,Sp).

% Si toutes les couleurs ont été trouvées et seules les positions doivent être devinées
proposePos(0,_,_,_,_,S,Sk):-inv(S,Sk).
% Si Li est vide, on la remplie par la liste des couleurs trouvées
proposePos(Size,[],Lp,Ls,C,S,Sp):-proposePos(Size,C,Lp,Ls,C,S,Sp).
proposePos(Size,Li,[_|Lp],[P|Ls],C,S,Sp):-nonvar(P), Size1 is Size-1,extract(P,Li,Li2),proposePos(Size1,Li2,Lp,Ls,C,[P|S],Sp).
proposePos(Size,[D|Li],[O|Lp],[P|Ls],C,S,Sp):-var(P),D \= [], Size1 is Size-1,not(member(D,O)),proposePos(Size1,Li,Lp,Ls,C,[D|S],Sp).
proposePos(Size,[D|Li],Lp,Ls,C,S,Sp):-var(P),D \= [], proposePos(Size,Li,Lp,Ls,C,S,Sp).




% Fonction de résolution IA
% S: Solution , Li liste des élèments possibles, Sf: Solution finale proposée, Ls :  Liste des valeurs trouvées, C: Variables existantes , AR : resultat précédent 
solve(_,_,_,_,_,_,V,V):-write('I\'ve found it !').
solve(S,Li,Sf,Ls,Lp,C,AR,V):-AR \= V,proper_length(S, Size),proposeCol(Size,Li,Lp,Ls,C,[],Sp),write('Maybe this?\n'),write( Sp ),write( '\n' ), testCombinaison(Sp,S,R),write( R ),write('\n'),newList(Li,C,Ls,Lp,R,Sp,NLi,[],Nlpf,NC),solve(S,NLi,Sf,Ls,Nlpf,NC,R,V).




% Construction des listes necessaires à une partie avec IA

% Liste des variables solution
buildVar(0,S,S).
buildVar(Size,S,R):-NewSize is Size - 1, buildVar(NewSize,[X|S],R).

% Liste de positions interdites initialisées a vide
buildEmpty(0,S,S).
buildEmpty(Size,S,R):-NewSize is Size - 1, buildEmpty(NewSize,[[]|S],R).

% Liste de v
buildV(0,S,S).
buildV(Size,S,R):-NewSize is Size - 1, buildV(NewSize,[v|S],R).


% Regle qui permet de faire joeur l'IA
makeIAPlay(ATrouver,ElementsPos,SolutionFin):-proper_length(ATrouver, Size), buildVar(Size, [], Var), buildEmpty(Size, [], E), buildV(Size, [], V), solve(ATrouver,ElementsPos,SolutionFin,Var,E,[],[],V).

