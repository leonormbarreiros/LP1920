% Leonor Barreiros, numero 95618

% ------------------------------------------------------------------------------------- %
%                                                                                       %
%                              Solucionador de Palavras Cruzadas                        %
%                                        Maio de 2020                                   %
%                                                                                       %
% ------------------------------------------------------------------------------------- %

:-[codigo_comum].

%   3.1. Predicados para a inicializacao de puzzles

% ------------------------------------------------------------------------------------- %
%                                   INICIALIZACAO DO PUZZLE                             %
% ------------------------------------------------------------------------------------- %

%   Passo 1: 
%   Obter uma lista ordenada cujos elementos sao listas com as letras de cada palavra. 


%   3.1.1. Predicado obtem_letras_palavras/2

%   obtem_letras_palavras(Lst_Pals, Letras) significa que Letras e a lista ordenada cujos
% elementos sao listas com as letras de cada palavra da lista de palavras Lst_Pals

obtem_letras_palavras(Lst_Pals, Letras) :-
    sort(Lst_Pals, Pals_Ordenadas_Juntas),
    maplist(atom_chars,Pals_Ordenadas_Juntas, Letras).

% ------------------------------------------------------------------------------------- %
%                                                                                       %
% ------------------------------------------------------------------------------------- %

%   Passo 2: 
%   Obter uma lista com os espacos disponiveis na grelha, aos quais podem ser atribuidas 
% palavras. 

%   3.1.2. Predicado espaco_fila/2

%   espaco_fila(Fila, Esp) significa que Esp e um espaco de Fila (linha/coluna)

espaco_fila(Fila, Esp) :-
    append([Antes, Esp, Depois], Fila), % o espaco e uma sublista da Fila;
    length(Esp, N), N >= 3,             % o espaco tem pelo menos 3 posicoes;
    \+ pertence(#, Esp),                % o espaco nao tem "paredes" no meio;
    % o espaco e limitado a esquerda e a direita por # ou por nada.
    (Antes == [] ; (last(Antes, Ultimo), Ultimo == #)), 
    (Depois == [] ; (nth1(1, Depois, Primeiro), Primeiro == #)).

%   pertence(E, L) significa que o elemento E pertence a lista L

pertence(P, [Q | _]) :- P == Q, !.

pertence(P, [_ | R]) :- pertence(P, R).

% ------------------------------------------------------------------------------------- %

%   3.1.3. Predicado espacos_fila/2

%   espacos_fila(Fila, Espacos) significa que Espacos e a lista dos espacos de Fila

espacos_fila(Fila, Espacos) :-          % os espacos sao o conjunto dos espacos de Fila;                                
    bagof(Espaco, espaco_fila(Fila, Espaco), Espacos), !.

espacos_fila(_, []).                    % uma fila pode nao ter espacos.

% ------------------------------------------------------------------------------------- %

%   3.1.4. Predicado espacos_puzzle/2

%   espacos_puzzle(Grelha, Espacos) significa que Espacos e a lista de espacos da Grelha

espacos_puzzle(Grelha, Espacos) :-
    mat_transposta(Grelha, Grelha_T),
    append(Grelha, Grelha_T, Puzzle),       % procuram-se os espacos nas linhas e colunas;
    espacos_puzzle(Puzzle, Espacos, []).  

espacos_puzzle([], Espacos, Espacos) :- !.

espacos_puzzle([Fila | Filas], Espacos, Ac) :-
    espacos_fila(Fila, Esp),                % percorre-se cada fila do puzzle e juntam-se
    append(Ac, Esp, N_Ac),                  % os espacos.
    espacos_puzzle(Filas, Espacos, N_Ac).

% ------------------------------------------------------------------------------------- %

%   3.1.5. Predicado espacos_com_posicoes_comuns/3

%   espacos_com_posicoes_comuns(Espacos, Esp, Esps_com) significa que Esps_com e a lista 
% de espacos com variaveis em comum com Esp, excepto ele mesmo

espacos_com_posicoes_comuns(Espacos, Esp, Esps_com) :-
    bagof(Esps,                         % e o conjunto de todos os elementos de Espacos
        (member(Esps, Espacos),         % que tem alguma variavel em comum com Esp;
        posicao_em_comum(Esp, Esps),
        Esp \== Esps),
        Esps_com).

%   posicao_em_comum(L1, L2) significa que L1 e L2 tem alguma variavel em comum

posicao_em_comum([P | R], Esp) :-
    pertence(P, Esp) ->
    !
    ;
    posicao_em_comum(R, Esp).

% ------------------------------------------------------------------------------------- %
%                                                                                       %
% ------------------------------------------------------------------------------------- %

%   Passo 3: 
%   Obter a lista de palavras possiveis para cada espaco. 


%   3.1.6 Predicado palavra_possivel_esp/4

%   palavra_possivel_esp(Pal, Esp, Espacos, Letras) significa que Pal e uma palavra pos-
% sivel para o espaco Esp, sendo ele um espaco de Espacos

palavra_possivel_esp(Pal, Esp, Espacos, Letras) :-
    member(Pal, Letras),
    Esp = Pal,                          % a unificacao de Esp com Pal nao pode impedir a 
                                        % resolucao do puzzle;
    espacos_com_posicoes_comuns(Espacos, Esp, Esps_com),
    palavra_possivel_esp(Esps_com, Letras).

palavra_possivel_esp([], _).

palavra_possivel_esp([Esp | R], Letras) :-
    ha_unificacao(Esp, Letras),         % para cada espaco procura uma unificacao com al-
    palavra_possivel_esp(R, Letras).    % guma palavra de Letras;

%   ha_unificacao(L, L1) significa que existe pelo menos um elemento de L1 que unifica 
% com cada elemento de L

ha_unificacao(Esp, [Pal | R]) :-        % se Esp unificar com Pal nao se impediu a res-
    unifiable(Esp, Pal, _) ->           % olucao desse espaco;
    !
    ;
    ha_unificacao(Esp, R).              % se nao, continua a tentar.

% ------------------------------------------------------------------------------------- %

%   3.1.7 Predicado palavras_possiveis_esp/4

%   palavras_possiveis_esp(Letras, Espacos, Esp, Pals_Possiveis) significa que Pals_Pos-
% siveis e a lista ordenada de palavras possiveis para o espaco Esp.

palavras_possiveis_esp(Letras, Espacos, Esp, Pals_Possiveis) :-
    findall(Pal, 
        palavra_possivel_esp(Pal, Esp, Espacos, Letras), 
        Pals_Possiveis).                % conjunto de todas as solucoes que palavra_pos-
                                        % sivel_esp teria para Esp

% ------------------------------------------------------------------------------------- %

%   3.1.8 Predicado palavras_possiveis/3

%   palavras_possiveis(Letras, Espacos, Pals_Possiveis) significa que Pals_Possiveis e a 
% lista de palavras possiveis.

palavras_possiveis(Letras, Espacos, Pals_Possiveis) :-  
    bagof([Esp, Pals],                 % conjunto de pares [Esp, Pals] tal que Esp e um 
        (member(Esp, Espacos),         % espaco de Espacos e Pals sao as palavras possi-
        palavras_possiveis_esp(Letras, Espacos, Esp, Pals)),            % veis para Esp.
        Pals_Possiveis).                           

% ------------------------------------------------------------------------------------- %
%                                                                                       %
% ------------------------------------------------------------------------------------- %

%   Passo 4: 
%   Simplificar a lista de palavras possiveis. 


%   3.1.9 Predicado letras_comuns/2

%   letras_comuns(Lst_Pals, Letras_comuns) significa que Letras_comuns e uma lista de pa-
% res (pos, letra), em que todas as listas de Lst_Pals contem a letra na posicao pos

letras_comuns(Lst_Pals, Letras_comuns) :-
    maplist(cria_conjunto, Lst_Pals, Conj),
    nth1(1, Conj, Primeiro),
    intersecta(Conj, Primeiro,Letras_comuns).
    
%   cria_conjunto(Pal, Conj) significa que Conj e o conjunto das letras de Pal nas respec-
% tivas posicoes

cria_conjunto(Pal, Conj) :-
    bagof((Pos, Letra), (nth1(Pos, Pal, Letra)), Conj).

%   intersecta(Conj, P, Letras_comuns) faz a interseccao dos elementos de Conj com P

intersecta([], Letras_comuns, Letras_comuns) :- !.

intersecta([Par | R], Conj, Letras_comuns) :-
    intersection(Par, Conj, N_Conj),
    intersecta(R, N_Conj, Letras_comuns).
    
% ------------------------------------------------------------------------------------- %

%   3.1.10. Predicado atribui_comuns/1

%   atribui_comuns(Pals_Possiveis) actualiza a lista de palavras possiveis, atribuindo a
% cada espaco as letras comuns a todas as palavras possiveis para ele

atribui_comuns([]) :- !.

atribui_comuns([[Esp, Pals] | R]) :-    % se ele tiver alguma letra que apareca sempre na
    letras_comuns(Pals, Letras_comuns), % mesma posicao, pode coloca-a;
    atribui_comuns(Esp, Letras_comuns),
    atribui_comuns(R).

atribui_comuns(_, []) :- !.

atribui_comuns(Esp, [(N, L) | R]) :-
    nth1(N, Esp, L),                    % coloca a letra L na posicao N de Esp.
    atribui_comuns(Esp, R).      

% ------------------------------------------------------------------------------------- %

%   3.1.11. Predicado retira_impossiveis/2

%   retira_impossiveis(Pals_Possiveis, Novas_Pals_Possiveis) significa que Novas_Pals_Pos-
% siveis e o resultado de retirar palavras impossiveis de Pals_Possiveis
    
retira_impossiveis(Pals_Possiveis, Novas_Pals_Possiveis) :-
    retira_impossiveis(Pals_Possiveis, Novas_Pals_Possiveis, []).

retira_impossiveis([], Novas_Pals_Possiveis, Novas_Pals_Possiveis) :- !.

retira_impossiveis([[Esp, Pals] | R], Novas_Pals_Possiveis, Ac) :-
    filtra_impossiveis(Esp, Pals, N_Pals),          % vai construir o resultado filtrando
    append(Ac, [[Esp, N_Pals]], N_Ac),              % as Pals_Possiveis e colocando num 
    retira_impossiveis(R, Novas_Pals_Possiveis, N_Ac),                     % acumulador;
    !.                          

%   filtra_impossiveis(Esp, Pals, N_Pals) significa que N_Pals e a lista filtrada de Pals
% que exclui as palavras impossiveis

filtra_impossiveis(Esp, Pals, N_Pals) :-
    filtra_impossiveis(Esp, Pals, N_Pals, []).

filtra_impossiveis(_, [], N_Pals, N_Pals) :- !.

filtra_impossiveis(Esp, [Pal | R], N_Pals, Ac) :-   % a filtragem das palavras tambem e 
    unifiable(Esp, Pal, _) ->                       % feita usando um acumulador: se Esp
    (append(Ac, [Pal], N_Ac),                       % e um membro de Pals forem unifica-
    filtra_impossiveis(Esp, R, N_Pals, N_Ac))       % veis, adiciona-se ao acumulador;
    ;
    filtra_impossiveis(Esp, R, N_Pals, Ac).         % se nao passa a frente.

% ------------------------------------------------------------------------------------- %

%   3.1.12. Predicado obtem_unicas/2

%   obtem_unicas(Pals_Possiveis, Unicas) significa que Unicas sao as palavras unicas de 
% Pals_Possiveis

obtem_unicas(Pals_Possiveis, Unicas) :-             % vai aceder as palavras possiveis 
    maplist(nth1(2), Pals_Possiveis, Pals),         % para cada espaco;      
    findall(Pal, (member(Pal, Pals), length(Pal, 1)), Palavras),
    findall(Pal, member([Pal], Palavras), Unicas).  % vai criar uma lista com das pala-
                                                    % lavras unicas e inseri-la no for-
                                                    % macto correcto.

% ------------------------------------------------------------------------------------- %

%   3.1.13. Predicado retira_unicas/2

%   retira_unicas(Pals_Possiveis, Novas_Pals_Possiveis) significa que Novas_Pals_Possi-
% veis e o resultado de retirar de Pals_Possiveis as palavras unicas

retira_unicas(Pals_Possiveis, Novas_Pals_Possiveis) :-  % vai reconstruir a lista de pa-
    obtem_unicas(Pals_Possiveis, Unicas),               % lavras possiveis (usando um           
    retira_unicas(Pals_Possiveis, Unicas, Novas_Pals_Possiveis, []).      % acumulador);

retira_unicas([], _, Novas_Pals_Possiveis, Novas_Pals_Possiveis) :- !.

retira_unicas([[Esp, Pals] | R], Unicas, Novas_Pals_Possiveis, Ac) :-
    length(Pals, 1) ->                                  
    (append(Ac, [[Esp, Pals]], N_Ac),                   % se so houver uma palavra para
    retira_unicas(R, Unicas, Novas_Pals_Possiveis, N_Ac))             % Esp nao a filtra;
    ;
    (filtra_unicas(Pals, N_Pals, Unicas),               % se nao, filtra a lista de pa.a-
    append(Ac, [[Esp, N_Pals]], N_Ac),                  % vras antes de adicionar ao acu-
    retira_unicas(R, Unicas, Novas_Pals_Possiveis, N_Ac)).                     % mulador;

%   filtra_unicas(Pals, N_Pals, Unicas) significa que N_Pals e a lista de palavras filtra-
% da de Pals que nao contem nenhum membro de Unicas

filtra_unicas(Pals, N_Pals, Unicas) :-
    intersection(Pals, Unicas, Int),                    % faz a intersecao e remove-a. 
    subtract(Pals, Int, N_Pals).
    
% ------------------------------------------------------------------------------------- %

%   3.1.14. Predicado simplifica/2

%   simplifica(Pals_Possiveis, Novas_Pals_Possiveis) significa que Novas_Pals_Possiveis e 
% o resultado de simplificar Pals_Possiveis

simplifica(Pals_Possiveis, Novas_Pals_Possiveis) :-
    simplifica(Pals_Possiveis, [], Novas_Pals_Possiveis, faz).

simplifica(Novas_Pals_Possiveis, _, Novas_Pals_Possiveis, naofaz) :- !.

simplifica(Pals_Possiveis, Pals_Possiveis_Ant, Novas_Pals_Possiveis, Gerador) :-
    Pals_Possiveis == Pals_Possiveis_Ant ->     % e um ciclo do-while que para quando nao
                                                % houver nenhuma alteracao de uma iteracao 
                                                % para a seguinte;
    simplifica(Pals_Possiveis, _, Novas_Pals_Possiveis, naofaz)
    ;
    (ciclo_simplifica(Pals_Possiveis, N_Pals_Possiveis_Ant),
    simplifica(N_Pals_Possiveis_Ant, Pals_Possiveis, Novas_Pals_Possiveis, Gerador)).

%   ciclo_simplifica(Pals_Possiveis, Pals) significa que Pals e o resultado de aplicar por
% ordem os predicados atribui_comuns, retira_impossiveis e retira_unicas a Pals_Possiveis

ciclo_simplifica(Pals_Possiveis, Pals) :- 
                                                % o ciclo_simplifica executa a
    atribui_comuns(Pals_Possiveis),             % sequencia que simplifica tera
    retira_impossiveis(Pals_Possiveis, Aux),    % de executar uma ou mais vezes.
    retira_unicas(Aux, Pals).

% ------------------------------------------------------------------------------------- %
%                                                                                       %
% ------------------------------------------------------------------------------------- %

%   Os 4 passos da inicializacao do puzzle culminam no predicado inicializa/2

%   3.1.15. Predicado inicializa/2

%   inicializa(Puz, Pals_Possiveis) significa que Pals_Possiveis e a lista de palavras 
% possiveis simplificada para Puz

inicializa(Puz, Pals_Possiveis) :-
    nth1(1, Puz, Lst_Pals),                                 % 1: letras das palavras
    obtem_letras_palavras(Lst_Pals, Letras),
    nth1(2, Puz, Grelha),                                   % 2: espacos disponiveis
    espacos_puzzle(Grelha, Espacos),
    palavras_possiveis(Letras, Espacos, Pals_Possiveis_Aux),% 3: palavras possiveis
    simplifica(Pals_Possiveis_Aux, Pals_Possiveis),         % 4: simplificar
    !.

% ------------------------------------------------------------------------------------- %

%   3.2. Predicados para a resolucao de listas de palavras possiveis

% ------------------------------------------------------------------------------------- %
%                                  RESOLUCAO DO PUZZLE                                  %
% ------------------------------------------------------------------------------------- %

%   Passo 1: 
%   Identificar os espacos com mais de uma palavra possivel. Deles escolher o primeiro 
% que tenha associado um numero minimo de palavras possiveis


%   3.2.1. Predicado escolhe_menos_alternativas/2

%   escolhe_menos_alternativas(Pals_Possiveis, Escolha) significa que escolha e o elemento 
% de Pals_Possiveis escolhido, como realizacao do passo 1

escolhe_menos_alternativas([[Esp, Lst_Pals] | R], Escolha) :-
    (length(Lst_Pals, N),      % o primeiro elemento que pode vir a ser escolhido tem de 
    N > 1) ->                  % ter no minimo duas palavras;
    escolhe_menos_alternativas(R, [Esp, Lst_Pals], Escolha)
    ;
    escolhe_menos_alternativas(R, Escolha).

escolhe_menos_alternativas([], Escolha, Escolha) :- !.

escolhe_menos_alternativas([[Esp, Lst_Pals] | R], [Esp_E , Lst_Pals_E], Escolha) :-               
    (length(Lst_Pals, 1)       % se so tiver uma palavra, nao se troca nada;
    ;
    (length(Lst_Pals, N),      % se tiver mais palavras que o anterior escolhido, nao se
    length(Lst_Pals_E, M),     % troca nada; 
    N >= M)) ->                
    escolhe_menos_alternativas(R, [Esp_E , Lst_Pals_E], Escolha)
    ;                          % se nao, passa a ser ele o escolhido.
    escolhe_menos_alternativas(R, [Esp, Lst_Pals], Escolha).

% ------------------------------------------------------------------------------------- %
%                                                                                       %
% ------------------------------------------------------------------------------------- %

%   Passo 2: 
%   Experimentar atribuir uma palavra ao espaco escolhido


%   3.2.2. Predicado experimenta_pal/3

%   experimenta_pal(Escolha, Pals_Possiveis, Novas_Pals_Possiveis) substitui o elemento
% Escolha (vindo do predicado anterior) pelo elemento [Esp, [Pal]] para cada palavra da
% lista de palavra de Escolha (Lst_Pals)

experimenta_pal([Esp, Lst_Pals], Pals_Possiveis, Novas_Pals_Possiveis) :-
    member(Pal, Lst_Pals),      % para cada membro da lista de palavras da Escolha, faz
    Esp = Pal,                  % unificacao do seu Esp com esse membro Pal, e substitui;
    substituti(Pals_Possiveis, Esp , Pal, [], Novas_Pals_Possiveis).


%   substitui(Pals_Possiveis, Esp, Pal, Ac, Novas_Pals_Possiveis) significa que Novas_-
% Pals_Possiveis e o resultado de substituir o elemento do espaco igual a Esp pelo ele-
% mento [Esp, [Pal]]

substituti([], _, _, Novas_Pals_Possiveis, Novas_Pals_Possiveis) :- !.

substituti([[Espaco, Pals] | R], Esp, Pal, Ac, Novas_Pals_Possiveis) :-
    Espaco == Esp ->            % se for o mesmo espaco, acrescenta ao acumulador a subs-
    (append(Ac, [[Esp, [Pal]]], N_Ac),                                        % tituicao;
    substituti(R, Esp, Pal, N_Ac, Novas_Pals_Possiveis))
    ;
    (append(Ac, [[Espaco, Pals]], N_Ac),             % se nao acrescenta o que la estava.
    substituti(R, Esp, Pal, N_Ac, Novas_Pals_Possiveis)).

% ------------------------------------------------------------------------------------- %
%                                                                                       %
% ------------------------------------------------------------------------------------- %

%   Passo 3: 
%   Simplificar a lista de palavras possiveis


%   3.2.3 Predicado resolve_aux/2

%   resolve_aux(Pals_Possiveis, Novas_Pals_Possiveis) significa que Novas_Pals_Possiveis
% e o resultado de aplicar o algoritmo da resolucao (passos 1 e 2) a Pals_Possiveis

resolve_aux(Pals_Possiveis, Novas_Pals_Possiveis) :-
    resolve_aux(Pals_Possiveis, Novas_Pals_Possiveis, faz),!.
    % e um ciclo do-while que para quando nao houver nenhum elemento com mais que uma
    % palavra possivel;

resolve_aux(Novas_Pals_Possiveis, Novas_Pals_Possiveis, naofaz) :- !.

resolve_aux(Pals_Possiveis, Novas_Pals_Possiveis, Gerador) :-
    escolhe_menos_alternativas(Pals_Possiveis, Escolha) % ve ha algo para simplificar:
    ->
    (experimenta_pal(Escolha, Pals_Possiveis, Aux),     % se sim aplica o algoritmo -
    simplifica(Aux, N_Pals_Possiveis),                  % experimentar e simplificar;
    resolve_aux(N_Pals_Possiveis, Novas_Pals_Possiveis, Gerador))
    ;                                                   % se nao, diz que e hora de parar.
    resolve_aux(Pals_Possiveis, Novas_Pals_Possiveis, naofaz).

% ------------------------------------------------------------------------------------- %

%   3.3. Predicados para a resolucao de puzzles

% ------------------------------------------------------------------------------------- %
%                                                                                       %
% ------------------------------------------------------------------------------------- %

%   A inicializacao e resolucao do puzzle e concretizada pelo predicado resolve/1


%   3.3.1 Predicado resolve/1

%   resolve(Puz) resolve o puzzle Puz 

resolve(Puz) :-
    inicializa(Puz, Pals_Possiveis),    % o puzzle resolve-se inicializando-o e resolven-
    resolve_aux(Pals_Possiveis, _).     % do-o.
    
% ------------------------------------------------------------------------------------- %
%                                                                                       %
% ------------------------------------------------------------------------------------- %

