:- dynamic sGB/3. 
:- dynamic sG/3. 

% no:
no(a00,0,0).
no(a01,0,1).
no(a02,0,2).
no(a10,1,0).
no(a11,1,1).
no(a12,1,2).
no(a20,2,0).
no(a21,2,1).
no(a22,2,2).
% a00, a10, a20
% a01, a11, a21
% a02, a12, a22

% Obstáculo
obstaculo(a11).
obstaculo(a01).

% Nó sujo
sujeira(a22).
sujeira(a21).

% Custos de um nó para o outro
sGB(1,a00,a01).
sGB(1,a00,a10).
sGB(1,a01,a02).
sGB(1,a01,a11).
sGB(1,a02,a12).
sGB(1,a10,a11).
sGB(1,a10,a20).
sGB(1,a11,a12).
sGB(1,a11,a21).
sGB(1,a12,a22).
sGB(1,a20,a21).
sGB(1,a21,a22).
sGB(1,a00,a11).
sGB(1,a11,a22).
sGB(1,a11,a02).
sGB(1,a11,a20).
sGB(1,a10,a21).
sGB(1,a10,a01).
sGB(1,a01,a12).
sGB(1,a12,a21).

% Avaliação de um nó para o nó com sujeira
sH(No, H):- 
  no(No,X1,Y1), 
  sujeira(No2),
  no(No2,X2,Y2),
  abs(X1 - X2,Sx),
  abs(Y1 - Y2,Sy),
  H is Sx + Sy.

% Relação do grafo não orientado
% g(n)
sG(G,V1,V2):-
    sGB(G,V1,V2).
sG(G,V1,V2):-
	sGB(G,V2,V1).

% f(n) = g(n) + h(n)
sF(G,H,F,V1,V2):-
	sG(G,V1,V2),
	sH(V2,H),
	F is G + H.

% h(n)
sH(H,V1,V2):-
    sGB(_,V1,V2),
    sH(V2,H).

%% Funções auxiliares %%
concatena([],L,L).
concatena([X|L1],L,[X|L2]):-
          concatena(L1,L,L2).

imprime(Lista,Custo) :-
    write('------------------'),nl,
	write('Solucao:'),nl,
	imprimeLista(Lista),
    write(' | Custo: '),write(Custo),nl,
	write('------------------'),nl.

imprimeLista([]) :-
    !,write('Lista Vazia').
imprimeLista([Elem]) :-
    !,write(Elem).
imprimeLista([Elem|Lista]) :-
	write(Elem),
    write(' -> '),
	imprimeLista(Lista).

ordenaF(Caminhos,CaminhosOrd) :-
	quicksortF(Caminhos,CaminhosOrd).

particionarF(_,[],[],[]).
particionarF(X,[Y|Cauda],[Y|Menor],Maior) :-
	maiorF(X,Y),!, 
	particionarF(X,Cauda,Menor,Maior).
particionarF(X,[Y|Cauda],Menor,[Y|Maior]) :-
	particionarF(X,Cauda,Menor,Maior).

quicksortF([],[]).
quicksortF([X|Cauda],ListaOrd) :-
	particionarF(X,Cauda,Menor,Maior),
	quicksortF(Menor,MenorOrd),
	quicksortF(Maior,MaiorOrd),
	concatena(MenorOrd,[X|MaiorOrd],ListaOrd).

maiorF([F1|_],[F2|_]):-
	F1 > F2.

estendeH([_,No|Caminho],NovosCaminhos) :-
	findall([HNovo,NovoNo,No|Caminho],
	( 
		sH(HN,No,NovoNo),
        not(obstaculo(NovoNo)),
		not(member(NovoNo,[No|Caminho])),
		HNovo is HN),
		NovosCaminhos
	).

estendeG([GC,No|Caminho],NovosCaminhos) :-
	findall([GNovo,NovoNo,No|Caminho],
	( 
		sG(GN,No,NovoNo),
        not(obstaculo(NovoNo)),
		not(member(NovoNo,[No|Caminho])),
		GNovo is GC + GN),
		NovosCaminhos
	).

estendeF([_,GC,_,No|Caminho],NovosCaminhos) :-
	findall([FNovo,GNovo,HNovo,NovoNo,No|Caminho],
	      (
          	  sF(GN,HN,_,No,NovoNo),
        	  not(obstaculo(NovoNo)),
              not(member(NovoNo,[No|Caminho])),
              GNovo is GC + GN, 
          	  HNovo is HN, 
              FNovo is GNovo + HNovo
          ),
	      NovosCaminhos).
%% Funções auxiliares %%

% Função principal
% resolve/2
% Inicio := nó
% Estrategia c {hillClimb, bestFirst, aEstrela, bBound}
resolve(Inicio, Estrategia) :-
	buscaHeuristica(Estrategia,Inicio,Solucao,Custo),
	imprime(Solucao,Custo).

buscaHeuristica(hillClimb,Inicio,Solucao,Custo):-
      hillClimb([[_,Inicio]],Solucao,Custo).
buscaHeuristica(bestFirst,Inicio,Solucao,Custo):-
      bestFirst([[_,Inicio]],Solucao,Custo).
buscaHeuristica(bBound,Inicio,Solucao,Custo):- 
      bBound([[0,Inicio]],Solucao,Custo).
buscaHeuristica(aEstrela,Inicio,Solucao,Custo):-
      aEstrela([[0,0,0,Inicio]],Solucao,Custo).

hillClimb([[_,No|Caminho]|_],Solucao,'-') :-
	sujeira(No),
	reverse([No|Caminho],Solucao).
hillClimb([Caminho|Caminhos], Solucao, Custo) :-
	estendeH(Caminho, NovosCaminhos),
	ordenaF(NovosCaminhos, CaminhosOrd),
	concatena(CaminhosOrd, Caminhos, CaminhosTotal),
	hillClimb(CaminhosTotal, Solucao, Custo).

bestFirst([[_,No|Caminho]|_],Solucao, '-') :-
	sujeira(No),
	reverse([No|Caminho],Solucao).
bestFirst([Caminho|Caminhos], Solucao, Custo) :-
	estendeH(Caminho, NovosCaminhos),
	ordenaF(NovosCaminhos, CaminhosOrd),
	concatena(CaminhosOrd, Caminhos, CaminhosTotal),
	bestFirst(CaminhosTotal, Solucao, Custo).

bBound([[Custo,No|Caminho]|_],Solucao,Custo) :-
	sujeira(No),
	reverse([No|Caminho],Solucao).
bBound([Caminho|Caminhos], Solucao, Custo) :-
	estendeG(Caminho, NovosCaminhos),
	ordenaF(NovosCaminhos, CaminhosOrd),
	concatena(CaminhosOrd, Caminhos, CaminhosTotal),
	bBound(CaminhosTotal, Solucao, Custo).

aEstrela([[G,_,_,No|Caminho]|_],Solucao,G) :- 
	sujeira(No), 
    reverse([No|Caminho],Solucao).
aEstrela([Caminho|Caminhos], Solucao, G) :-
	estendeF(Caminho, NovosCaminhos),
	concatena(Caminhos,NovosCaminhos,CaminhosTotal),
	ordenaF(CaminhosTotal,CaminhosTotOrd),
	aEstrela(CaminhosTotOrd, Solucao, G).
