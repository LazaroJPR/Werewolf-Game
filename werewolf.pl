% DECLARAÇÃO DOS FATOS
% -------------------------------------------------------------------------
% --> Lista de jogadores
jogadores([jogador01, jogador02, jogador03, jogador04, jogador05, jogador06, jogador07, jogador08, jogador09, jogador10, jogador11, jogador12, jogador13, jogador14, jogador15, jogador16, jogador17, jogador18, jogador19, jogador20]).

% --> Lista de grupos
grupos([curandeiro, vidente, aldeao, idiota, cacador, lobisomem]).

% --> Predicativo dinâmico do jogadorGrupo
:- dynamic jogadorGrupo/2.

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

% --> Status do jogador
jogadorStatus(jogador01, vivo).
jogadorStatus(jogador02, vivo).
jogadorStatus(jogador03, vivo).
jogadorStatus(jogador04, vivo).
jogadorStatus(jogador05, vivo).
jogadorStatus(jogador06, vivo).
jogadorStatus(jogador07, vivo).
jogadorStatus(jogador08, vivo).
jogadorStatus(jogador09, vivo).
jogadorStatus(jogador10, vivo).
jogadorStatus(jogador11, vivo).
jogadorStatus(jogador12, vivo).
jogadorStatus(jogador13, vivo).
jogadorStatus(jogador14, vivo).
jogadorStatus(jogador15, vivo).
jogadorStatus(jogador16, vivo).
jogadorStatus(jogador17, vivo).
jogadorStatus(jogador18, vivo).
jogadorStatus(jogador19, vivo).
jogadorStatus(jogador20, vivo).

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
imprimirJogadoresStatus :-
    jogadorStatus(Jogador, Status),
    write('Jogador: '), write(Jogador), write(' - Status: '), write(Status), nl,
    fail.

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
todosJogadoresAcaoRealizada([Jogador | OutrosJogadores]) :-
    jogadorStatus(Jogador, vivo),
    jogadorGrupo(Jogador, Grupo),
    acao(Acao, Grupo),
    random_permutation(OutrosJogadores, JogadoresEmbaralhados),
    select(JogadorAleatorio, JogadoresAleatorios, NovosOutrosJogadores),
    executarAcao(Jogador, Acao, JogadorAleatorio),
    todosJogadoresAcaoRealizada([JogadorAleatorio | NovosOutrosJogadores]).

%  --> Altera o status de todos os jogadores protegidos para vivos
retirarProtecao([]).
retirarProtecao([Jogador | OutrosJogadores]) :-
    atualizarStatusJogador(JogadorEscolhido, vivo),
    retirarProtecao(OutrosJogadores).

%  --> Acao específica de quando um cacador morre
acaoCacador(Jogador):-
    	jogadorGrupo(Jogador, cacador),
        findall(Jogador, jogadorStatus(Jogador, vivo), JogadoresVivos),
        random_permutation(JogadoresVivos, [JogadorEscolhido | OutrosJogadores]),
        atualizarStatusJogador(JogadorEscolhido, morto),
        write('Jogador '), write(Jogador), write(' era um cacador e morreu levando o jogador '),  write(JogadorEscolhido), write(' junto.'), nl. 

%  --> Acao específica de quando um idiota morre
acaoIdiota(Jogador) :-
    jogador(Jogador, idiota),
    findall(Jogador, jogadorStatus(Jogador, vivo), JogadoresVivos),
    random_permutation(JogadoresVivos, [JogadorEscolhido | OutrosJogadores]),
    atualizarGrupoJogador(JogadorEscolhido, idiota),
    write('Jogador '), write(Jogador), write(' era um idiota e morreu mudando o grupo de outro jogador para idiota'), nl. 

% --> Regras para executar uma ação específica para cada jogador
executarAcao(Jogador, nada, JogadorAleatorio) :-
    write('Jogador '), write(Jogador), write(' realizou sua acao.'), nl.

executarAcao(Jogador, proteger, JogadorAleatorio) :-
    atualizarStatusJogador(JogadorAleatorio, protegido),
    write('Jogador '), write(Jogador), write(' realizou sua acao.'), nl.    

executarAcao(Jogador, verificar, JogadorAleatorio) :-
    write('Jogador '), write(Jogador), write(' verificou o jogador '), write(JogadorAleatorio), write(' e ele é um '), write(jogadorGrupo(JogadorAleatorio, X)), nl.

executarAcao(Jogador, matar, JogadorAleatorio) :-
    jogadorStatus(JogadorAleatorio, vivo),  % Verificacao para saber se o jogador não está morto nem está protegido
    atualizarStatusJogador(JogadorAleatorio, morto),
    acaoCacador(JogadorAleatorio),
    acaoIdiota(JogadorAleatorio),
    write('Jogador '), write(Jogador), write(' realizou sua acao.'), nl.

executarAcao(Jogador, escolherOutro, JogadorAleatorio) :-
    write('Jogador '), write(Jogador), write(' realizou sua acao.'), nl.

executarAcao(Jogador, eliminar, JogadorAleatorio) :-
    write('Jogador '), write(Jogador), write(' realizou sua acao.'), nl.

% --> Chama a execução das ações de todos os jogadores
realizarAoes():-
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
    write('A noite chegou, os aldeões foram para suas casas, o que poderá acontecer nesta noite? '), nl,
    realizarAcoes,
    writeln('========================================================================================'),
    sol,
    write('A noite passou, vamos ver o que aconteceu? '), nl,
    imprimirJogadoresStatus,
    write('Com esse caos que acoteceu esta noite, precisamos fazer uma votação para eliminar o nosso problema, o lobisomem! '), nl,
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