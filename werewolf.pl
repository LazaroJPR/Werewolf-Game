% DECLARAÇÃO DOS FATOS
% -------------------------------------------------------------------------
% --> Lista de jogadores
jogadores([jogador01, jogador02, jogador03, jogador04, jogador05, jogador06, jogador07, jogador08, jogador09, jogador10, jogador11, jogador12, jogador13, jogador14, jogador15, jogador16, jogador17, jogador18, jogador19, jogador20]).

% --> Lista de grupos
grupos([curandeiro, vidente, aldeao, idiota, cacador, lobisomem]).

% --> Predicativo dinâmico do jogadorGrupo
:- dynamic jogadorGrupo/2.

% --> Açõess
acao(proteger, curandeiro).
acao(verificar, vidente).
acao(matar, lobisomem).
acao(escolherOutro, idiota).
acao(eliminar, cacador).

% --> Turnos
turno(dia).
turno(noite).

% --> Votação
votacao(iniciar).
votacao(finalizada).

% --> Status do jogador
status(vivo).
status(morto).
status(protegido).

% FUNÇÕES
% -------------------------------------------------------------------------
% --> Gera a relação entre jogador e grupo aleatoriamente
% 50% de chance de ser aldeão
% Pelo menos um lobosiomem é relacionado
gerarRelacao :-
    retractall(jogadorGrupo(_, _)), % Limpa qualquer relação anterior
    jogadores(Jogadores),
    grupos(Grupos),
    random_member(Lobisomem, Grupos), % Seleciona aleatoriamente um lobisomem
    random_permutation(Jogadores, JogadoresEmbaralhados), % Embaralha a lista de jogadores
    inserirGrupo(Lobisomem, JogadoresEmbaralhados, Grupos).

inserirGrupo(_, [], _).
inserirGrupo(Lobisomem, [Jogador|Resto], Grupos) :-
    random(0, 2, Chance), % Gera um número aleatório entre 0 e 1
    (Chance =:= 0 -> Grupo = aldeao ; random_member(Grupo, Grupos)), % 50% de chance de ser aldeão, caso contrário seleciona aleatoriamente um grupo
    assertz(jogadorGrupo(Jogador, Grupo)), % Insere a relação jogadorGrupo
    (Grupo = Lobisomem -> % Verifica se o grupo é lobisomem
        delete(Grupos, Lobisomem, NovosGrupos),
        inserirGrupo(Lobisomem, Resto, NovosGrupos)
    ; inserirGrupo(Lobisomem, Resto, Grupos)
    ).

% --> Ordena jogadores em ordem "Numérica"
imprimirJogadores :-
    findall(Jogador-Grupo, jogadorGrupo(Jogador, Grupo), Relacoes),
    sort(Relacoes, RelacoesOrdenadas),
    imprimirRelacoes(RelacoesOrdenadas).

% --> Imprime os jogadores e seu grupo
imprimirRelacoes([]).
imprimirRelacoes([Jogador-Grupo|Resto]) :-
    write(Jogador), write(' - '), write(Grupo), nl,
    imprimirRelacoes(Resto).

% Exibir o menu
exibir_menu :-
    writeln('========================================================================================'),
    write('[ 1 ] INICIAR JOGO'), nl,
    write('[ 2 ] SAIR'), nl, nl,
    write('Digite sua opcao: '), nl.

% INTERFACE
% -------------------------------------------------------------------------
% Menu
menu :-
    repeat,
    exibir_menu,
    read(Opcao),
    (Opcao = 1 ->(
            gerarRelacao,
            imprimirJogadores
        );
     Opcao = 2 -> halt;
     write('Opcao invalida!'), nl
    ),
    Opcao \= 2,
    nl,
    fail.

iniciar :-
        writeln(' __      ____________________________________      __________  .____   ___________'),
    writeln('/  \\    /  \\_   _____/\\______   \\_   _____/  \\    /  \\_____  \\ |    |  \\_   _____/'),
    writeln('\\   \\/\\/   /|    __)_  |       _/|    __)_\\   \\/\\/   //   |   \\|    |   |    __)  '),
    writeln(' \\        / |        \\ |    |   \\|        \\        //    |    \\    |___|     \\   '),
    writeln('  \\__/\\  / /_______  / |____|_  /_______  / \\__/\\  / \\_______  /_______ \\___  /   '),
    writeln('       \\/          \\/         \\/        \\/       \\/          \\/        \\/   \\  /    '),
    writeln('          .                                                  .                  '),
    writeln('               ( ,(/                                       .((. (               '),
    writeln('               ,*      /(*    ./((/.         *((/.    /(*      (                '),
    writeln('                (  ,*((.                                .((/. ,*                '),
    writeln('                 (  (            (/(.((*(/(.((*            /  (                 '),
    writeln('                 ,*  (                                    (  (                  '),
    writeln('                  (                                         ,*                  '),
    writeln('                   (                ,((.*(/                 (                   '),
    writeln('                   (             /((.      ((/              (                   '),
    writeln('                  /.        *((.                *((,        *,                  '),
    writeln('                  (         ,*  ( ,(((.  *(((..(  (          (                  '),
    writeln('                 ,* ,(.       (.    //    (,    *(       *(  (                  '),
    writeln('                 (      /(                           ,(,      /                 '),
    writeln('                 *(.      (   (        /.        (  .*      ,(,                 '),
    writeln('                     ((       *,       /.       /        (*                     '),
    writeln('                         ((    (       /.       (    (/                         '),
    writeln('                             (/(       /.      ,,(/                             '),
    writeln('                                /      /.      (                                '),
    writeln('                                (      /      ./                                '),
    writeln('                                ,*  *,    (   (                                 '),
    writeln('                                 //////////////                                 '),
    menu.