% DECLARAÇÃO DOS FATOS
% -------------------------------------------------------------------------
% --> Lista de jogadores
jogadores([jogador01, jogador02, jogador03, jogador04, jogador05, jogador06, jogador07, jogador08, jogador09, jogador10, jogador11, jogador12, jogador13, jogador14, jogador15, jogador16, jogador17, jogador18, jogador19, jogador20]).

% --> Lista de grupos
grupos([curandeiro, vidente, aldeao, idiota, cacador, lobisomem]).

% --> Predicativo dinâmico do jogadorGrupo
:- dynamic jogadorGrupo/2.

% --> Predicativo dinâmico do jogadorStatus
:- dynamic jogadorStatus/2.

% --> Açõess
acao(nada, aldeao).
acao(proteger, curandeiro).
acao(verificar, vidente).
acao(matar, lobisomem).
acao(escolherOutro, idiota).
acao(eliminar, cacador).

% --> Status
status(vivo).
status(morto).
status(protegido).

% FUNÇÕES
% -------------------------------------------------------------------------
% --> Gera a relação entre jogador e grupo aleatoriamente
gerarRelacao :-
    retractall(jogadorGrupo(_, _)), % Limpa qualquer relação anterior
    jogadores(Jogadores),
    grupos(Grupos),
    random_member(Lobisomem, Grupos),
    random_permutation(Jogadores, JogadoresEmbaralhados),
    inserirGrupo(Lobisomem, JogadoresEmbaralhados, Grupos).

% --> Relaciona os grupos com jogadores
% 50% de chance de ser aldeão e pelo menos um lobosiomem é relacionado
inserirGrupo(_, [], _).
inserirGrupo(Lobisomem, [Jogador|Resto], Grupos) :-
    random(0, 2, Chance), 
    (Chance =:= 0 -> Grupo = aldeao ; random_member(Grupo, Grupos)),
    assertz(jogadorGrupo(Jogador, Grupo)),
    (Grupo = Lobisomem ->
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

% --> Imprime os jogadores e seus status
imprimirJogadoresStatus.
imprimirJogadoresStatus :-
    jogadorStatus(Jogador, Status),
    write('Jogador: '), write(Jogador), write(' - Status: '), write(Status), nl,
    fail.

% --> Coloca todos os status dos jogadores como vivo
atualizarStatusVivo :-
    jogadores(ListaJogadores),
    atualizarStatusVivoLista(ListaJogadores).

atualizarStatusVivoLista([]).
atualizarStatusVivoLista([Jogador|Resto]) :-
    assertz(jogadorStatus(Jogador, vivo)),
    atualizarStatusVivoLista(Resto).

%  --> atualiza o status do jogador
atualizarStatusJogador(Jogador, NovoStatus) :-
    retract(jogadorStatus(Jogador, _)),
    assertz(jogadorStatus(Jogador, NovoStatus)).

%  --> atualiza o grupo do jogador
atualizarGrupoJogador(Jogador, NovoGrupo) :-
    retract(jogadorGrupo(Jogador, _)),
    assertz(jogadorGrupo(Jogador, NovoGrupo)).

%  --> executa a ação do jogador e chama recursivamente para os próximos jogadores
todosJogadoresAcaoRealizada([]).

todosJogadoresAcaoRealizada([Jogador]) :-
    jogadorGrupo(Jogador, Grupo),
    acao(Acao, Grupo),
    executarAcao(Jogador, Acao, Jogador).

todosJogadoresAcaoRealizada([Jogador | OutrosJogadores]) :-
    jogadorGrupo(Jogador, Grupo),
    acao(Acao, Grupo),
    random_permutation(OutrosJogadores, [JogadorAleatorio | NovosOutrosJogadores]),
    executarAcao(Jogador, Acao, JogadorAleatorio),
    todosJogadoresAcaoRealizada([JogadorAleatorio | NovosOutrosJogadores]).

%  --> Altera o status de todos os jogadores protegidos para vivos
retirarProtecao([]).
retirarProtecao([Jogador | OutrosJogadores]) :-
    atualizarStatusJogador(Jogador, vivo),
    retirarProtecao(OutrosJogadores).

% --> Regras para executar uma ação específica para cada jogador
%  --> Ação específica de quando um cacador morre
acaoCacador(_).
acaoCacador(Jogador):-
    jogadorGrupo(Jogador, cacador),
    findall(Jogador, jogadorStatus(Jogador, vivo), JogadoresVivos),
    random_permutation(JogadoresVivos, [JogadorEscolhido | _]),
    atualizarStatusJogador(JogadorEscolhido, morto),
    write('Jogador '), write(Jogador), write(' era um cacador e morreu levando o jogador '),  write(JogadorEscolhido), write(' junto.'), nl. 

%  --> Ação específica de quando um idiota morre
acaoIdiota(_).
acaoIdiota(Jogador) :-
    jogadorGrupo(Jogador, idiota),
    findall(Jogador, jogadorStatus(Jogador, vivo), JogadoresVivos),
    random_permutation(JogadoresVivos, [JogadorEscolhido | _]),
    atualizarGrupoJogador(JogadorEscolhido, idiota),
    write('Jogador '), write(Jogador), write(' era um idiota e morreu mudando o grupo de outro jogador para idiota'), nl. 

% --> Ação específica do aldeao
executarAcao(Jogador, nada, _) :-
    write('Jogador '), write(Jogador), write(' realizou sua acao.'), nl.

% --> Ação específica do curandeiro
executarAcao(Jogador, proteger, JogadorAleatorio) :-
    atualizarStatusJogador(JogadorAleatorio, protegido),
    write('Jogador '), write(Jogador), write(' realizou sua acao.'), nl.    

% --> Ação específica do vidente
executarAcao(Jogador, verificar, JogadorAleatorio) :-
    jogadorGrupo(JogadorAleatorio, Grupo),
    write('Jogador '), write(Jogador), write(' verificou o jogador '), write(JogadorAleatorio), write(' e ele e um '), write(Grupo), nl.

% --> Ação específica do lobisomem
executarAcao(Jogador, matar, JogadorAleatorio) :-
    jogadorStatus(JogadorAleatorio, vivo),  % Verificacao para saber se o jogador não está morto nem está protegido
    atualizarStatusJogador(JogadorAleatorio, morto),
    acaoCacador(JogadorAleatorio),
    acaoIdiota(JogadorAleatorio),
    write('Jogador '), write(Jogador), write(' realizou sua acao.'), nl.

% --> Ação específica do idiota
executarAcao(Jogador, escolherOutro, _) :-
    write('Jogador '), write(Jogador), write(' realizou sua acao.'), nl.

% --> Ação específica do caçador
executarAcao(Jogador, eliminar, _) :-
    write('Jogador '), write(Jogador), write(' realizou sua acao.'), nl.

% --> Chama a execução das ações de todos os jogadores
realizarAcoes():-
    findall(Jogador, jogadorStatus(Jogador, vivo), JogadoresVivos),
    todosJogadoresAcaoRealizada(JogadoresVivos),
    findall(Jogador, jogadorStatus(Jogador, protegido), JogadoresProtegidos),
    retirarProtecao(JogadoresProtegidos).

% INTERFACE
% -------------------------------------------------------------------------
% --> Realiza cada rodada
rodadas :-
    writeln('========================================================================================'),
    lua,
    write('A noite chegou, os aldeoes foram para suas casas, o que podera acontecer nesta noite? '), nl, nl,
    realizarAcoes,
    writeln('========================================================================================'),
    sol,
    write('A noite passou, vamos ver o que aconteceu? '), nl,
    imprimirJogadoresStatus,
    write('Com esse caos que acoteceu esta noite, precisamos fazer uma votacao para eliminar o nosso problema, o lobisomem! '), nl,
    %FAZER PARTE DA VOTACAO
    %FAZER PARTE DA FINALIZACAO DO JOGO
    %SOL É SÓ PRA FECHAR A FUNCAO
    sol.

% Exibir o menu
exibir_menu :-
    writeln('========================================================================================'),
    write('[ 1 ] INICIAR JOGO'), nl,
    write('[ 2 ] SAIR'), nl, nl,
    write('Digite sua opcao: '), nl.

% Menu
menu :-
    %repeat,
    exibir_menu,
    read(Opcao),
    (Opcao = 1 ->(
            gerarRelacao,
            atualizarStatusVivo,
            %imprimirJogadoresStatus,
            imprimirJogadores,
            rodadas
        );
     Opcao = 2 -> halt;
     write('Opcao invalida!'), nl
    ),
    Opcao \= 2,
    nl,
    fail.

lua :-
    writeln('o                     __...__     *                    '),
    writeln('              *   .--     __.=-.             o         '),
    writeln('     |          ./     .-                              '),
    writeln('    -O-        /      /                                '),
    writeln('     |        /     "/               *                 '),
    writeln('             |     (@)                                 '),
    writeln('            |        \\                         .      '),
    writeln('            |         \\                               '),
    writeln(' *          |       ___\\                  |           '),
    writeln('             |  .   /  `                 -O-           '),
    writeln('              \\  `~~\\                     |          '),
    writeln('         o     \\     \\            *                  '),
    writeln('                `\\    `-.__           .               '),
    writeln('    .             `--._    `--                         '),
    writeln('                       `---~~`                *        '),
    writeln('            *                   o                      ').

sol :-
    writeln('        \\     (      /            '),
    writeln('   `.    \\     )    /    .        '),
    writeln('     `.   \\   (    /   .          '),
    writeln('       `.  .-''''-.  .             '),
    writeln(' `~._    . /_    _\\`.    _.~      '),
    writeln('     `~ /  / \\  / \\  \\ ~        '),
    writeln('_ _ _ _|  _\\O/  \\O/_  |_ _ _ _   '),
    writeln('       | (_)  /\\  (_) |           '),
    writeln('    _.~ \\  \\      /  / ~._       '),
    writeln(' .~      `. `.__. .      `~.       '),
    writeln('       .   `-,,,,-   `.            '),
    writeln('     .    /    )   \\   `.         '),
    writeln('   .     /    (     \\    `.       '),
    writeln('        /      )     \\            '),
    writeln('              (                    ').

iniciar :-
    writeln(' __      ____________________________________      __________  .____   ___________             '),
    writeln('/  \\    /  \\_   _____/\\______   \\_   _____/  \\    /  \\_____  \\ |    |  \\_   _____/     '),
    writeln('\\   \\/\\/   /|    __)_  |       _/|    __)_\\   \\/\\/   //   |   \\|    |   |    __)        '),
    writeln(' \\        / |        \\ |    |   \\|        \\        //    |    \\    |___|     \\           '),
    writeln('  \\__/\\  / /_______  / |____|_  /_______  / \\__/\\  / \\_______  /_______ \\___  /          '),
    writeln('       \\/          \\/         \\/        \\/       \\/          \\/        \\/   \\  /       '),
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