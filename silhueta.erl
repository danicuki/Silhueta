
-module(silhueta).
  -export([algoritmo1/1, algoritmo2/1, uniao/2, main/1, preenche_retangulo/6]).
  -import(matrix).
  -include("util.erl").

  -include_lib("eunit/include/eunit.hrl").

  main([]) -> main(["2"]);
  main([Alg]) -> main([Alg, empty]);
  main([Alg, Entrada]) -> main([Alg, Entrada, empty]);
  main([Alg, Entrada, Saida]) -> main([Alg, Entrada, Saida, empty]);
  main([Alg, Entrada, Saida, Imagem]) ->
    io:fwrite("Executando algoritmo ~w, entrada ~w, saida ~w, imagem:~w~n", [Alg, Entrada, Saida, Imagem]),
    ArqEntrada =
      if Entrada == empty -> standard_io;
        true -> {ok, S1} = file:open(Entrada, read), S1
      end,
    ArqSaida =
      if Saida == empty -> standard_io;
        true -> {ok, S} = file:open(Saida, write), S
      end,
    Predios = le_entrada(ArqEntrada),
    Silhueta =
      if Alg == '1' -> algoritmo1(Predios);
         Alg == '2' -> algoritmo2(Predios);
         Alg == 'L' -> silhueta_com_foldl(Predios);
         Alg == 'R' -> silhueta_com_foldr(Predios);
         true -> throw ("Algoritmo Invalido!")
      end,
    if (Imagem /= empty) -> gera_imagem(Silhueta, Imagem); true -> true end,
    print_silhueta(ArqSaida, Silhueta),
    % print_silhueta(ArqSaida, silhueta()).
    % print_predios(Predios).
    init:stop().


  le_entrada(Entrada) ->
    {Linhas, "\n"} = string:to_integer(io:get_line(Entrada, '')),
    le_entrada(Entrada, Linhas, []).

  le_entrada(_, 0, Lista) -> Lista;
  le_entrada(Entrada, N, Lista) ->
    Linha = io:get_line(Entrada, ''),
    [{L,[]},{H,[]},{D,"\n"}] = lists:map(fun(X) -> string:to_integer(X) end, string:tokens(Linha, " ")),
    le_entrada(Entrada, N - 1, [{L,H,D}|Lista]).

  print_silhueta(Saida, L) ->
    io:format(Saida, "~w ~n", [length(L)]),
    lists:foreach(fun({C,A}) -> io:format(Saida, "~w ~w ~n", [C,A]) end, L),
    file:close(Saida).

  gera_imagem(ListaDePares, String) ->
    {ok, Arquivo} = file:open(String, write),
    io:format(Arquivo, "P2~n", []),
    io:format(Arquivo, "~w ~w~n", [n_cols(), n_lins()]),
    io:format(Arquivo, "~w~n", [branco()]),
    M = matrix:new(n_lins(), n_cols()),
    M1 = matriz_da_silhueta(M, ListaDePares),
    M2 = preenche_retangulo(M1, base(), base(), 0, n_cols(), preto()),
    print_matrix(Arquivo, M2),
    file:close(Arquivo).

  % calcula a matriz de uma silhueta
  matriz_da_silhueta(Matriz, []) -> Matriz;
  matriz_da_silhueta(Matriz, [_|[]]) -> Matriz;
  matriz_da_silhueta(Matriz, [{C1,A1}|[{C2,A2}|T]]) ->
    M = preenche_retangulo(Matriz, base() - A1, base(), C1, C2, cinza()),
    matriz_da_silhueta(M, [{C2,A2}|T]).

  % preenche um retangulo da matriz com um valor
  preenche_retangulo(Matriz, Lin1, Lin1, Col1, Col2, Valor) ->
    preenche_linha(Matriz, Lin1, Col1, Col2, Valor);
  preenche_retangulo(Matriz, Lin1, Lin2, Col1, Col2, Valor) ->
    M = preenche_linha(Matriz, Lin2, Col1, Col2, Valor),
    preenche_retangulo(M, Lin1, Lin2 - 1, Col1, Col2, Valor).

  % preenche uma linha da matriz com um valor
  preenche_linha(Matriz, Lin1, Col1, Col1, Valor) -> matrix:set(Lin1, Col1, Valor, Matriz);
  preenche_linha(Matriz, Lin1, Col1, Col2, Valor) ->
    M = matrix:set(Lin1, Col2, Valor, Matriz),
    preenche_linha(M, Lin1, Col1, Col2 - 1, Valor).

  % imprime a matriz no arquivo
  print_matrix(Arquivo, Matrix) ->
    for(n_lins(),
      fun(Linha) ->
        for(n_cols(), fun(Coluna) -> io:format(Arquivo, "~w ", [matrix:get(Linha, Coluna, Matrix)]) end),
        io:format(Arquivo, "~n", [])
      end).

  % Algoritmo 1
  % Depois de determinar recursivamente a silhueta de um subconjunto com m-1 edifícios do conjunto original,
  % determina a união dessa silhueta com a edifício que ficou fora do subconjunto
  algoritmo1(L) -> algoritmo1_aux(L, []).
  algoritmo1_aux([], Acum) -> Acum;
  algoritmo1_aux([H|T], Acum) -> algoritmo1_aux(T, uniao(silhueta_de_edificio(H), Acum)).

  % Algoritmo 2
  % Particiona o conjunto de edifícios em duas "metades", determina recursivamente a silhueta para cada uma das "metades",
  % e determina a união das duas silhuetas obtidas
  algoritmo2([]) -> [];
  algoritmo2([H|[]]) -> silhueta_de_edificio(H);
  algoritmo2(L) ->
    {L1, L2} = lists:split(round(length(L) / 2), L),
    uniao(algoritmo2(L1), algoritmo2(L2)).

  silhueta_com_foldl(L) -> lists:foldl(fun(Edificio, Silhueta) -> uniao(silhueta_de_edificio(Edificio), Silhueta) end, [], L).

  silhueta_com_foldr(L) -> lists:foldr(fun(Edificio, Silhueta) -> uniao(silhueta_de_edificio(Edificio), Silhueta) end, [], L).

  % Silhueta de um edificio
  silhueta_de_edificio({E, A, D}) -> [{E,A}, {D,0}].

  % Algoritmo que une duas silhuetas
  uniao(L1, L2) -> uniao_aux(L1, L2, 0, 0, []).

  uniao_aux([], [], _, _, Acum) -> lists:reverse(Acum);

  uniao_aux([{C1, A1}|[]], [], _, M2, [{AcumC, _}|AcumT]) when (AcumC == C1) ->
     uniao_aux([], [], A1, M2, [{C1, A1}|AcumT]);
  uniao_aux([{C1, A1}|T1], L2, _, M2, [{AcumC, AcumA}|AcumT]) when (AcumC == C1) and (A1 > AcumA) ->
     uniao_aux(T1, L2, A1, M2, [{C1, A1}|AcumT]);
  uniao_aux([{C1, A1}|T1], L2, _, M2, [{AcumC, AcumA}|AcumT]) when (AcumC == C1) and (A1 =< AcumA) ->
      uniao_aux(T1, L2, A1, M2, [{AcumC, AcumA}|AcumT]);

  uniao_aux([{C,A}|T], [], Max1, Max2, Acum) -> uniao_aux(T, [], Max1, Max2, [{C,A}|Acum]);
  uniao_aux([], L, M1, M2, Acum) -> uniao_aux(L, [], M2, M1, Acum);

  uniao_aux([{C1, A1}|T1], [{C2, A2}|T2], M1, M2, Acum) ->
    if C1 =< C2 ->
      % io:fwrite("C1: ~w, A1:~w, C2:~w, A2:~w, M1:~w, M2: ~w~n", [C1,A1,C2,A2,M1,M2]),
      if (A1 =< M2) and (M1 =< M2) -> uniao_aux(T1, [{C2, A2}|T2], A1, M2, Acum);
         (A1 =< M2) and (M1 > M2) -> uniao_aux(T1, [{C2, A2}|T2], A1, M2, [{C1,M2}|Acum]);
         (A1 > M2) and (M1 < M2) ->  uniao_aux(T1, [{C2, A2}|T2], A1, M2, [{C1,A1}|Acum]);
         (A1 > M2) and (M1 >= M2) -> uniao_aux(T1, [{C2, A2}|T2], A1, M2, [{C1,A1}|Acum])
      end;
      C1 > C2 -> uniao_aux([{C2, A2}|T2], [{C1, A1}|T1], M2, M1, Acum)
    end.

  % ------------------------ CONSTANTES --------------------
  n_lins() -> 600.                      % número de linhas da imagem
  n_cols() -> 800.                      % número de colunas da imagem
  borda_inf() -> n_lins() - 1.          % borda inferior (última linha da imagem)
  margem_inf() -> 20.                   % linhas do eixo base à borda inferior da imagem
  base() -> borda_inf() - margem_inf(). % linha do eixo base

  branco() -> 15.                       % valor de maxval
  cinza() -> 10.                        % cor da silhueta preenchida
  preto() -> 0.                         % cor do eixo base


  % ------------------------ TESTES ------------------------
  silhueta_de_edificio_test_() ->
    [?_assert(silhueta_de_edificio({1,1,2}) =:= [{1,1},{2,0}]),
     ?_assert(silhueta_de_edificio({10,50,21}) =:= [{10,50},{21,0}])
    ].

  silhueta_com_foldl_test_() ->
    [?_assert(silhueta_com_foldl([]) =:= []),
     ?_assert(silhueta_com_foldl([{1,1,2}]) =:= [{1,1},{2,0}]),
     ?_assert(silhueta_com_foldl([{12,7,16},{2,6,7},{1,11,5},{24,4,28},{3,13,9},{19,18,22},{23,13,29},{14,3,25}])
                       =:= [{1,11},{3,13},{9,0},{12,7},{16,3},{19,18},{22,3},{23,13},{29,0}])
    ].

  silhueta_com_foldr_test_() ->
    [?_assert(silhueta_com_foldr([]) =:= []),
     ?_assert(silhueta_com_foldr([{1,1,2}]) =:= [{1,1},{2,0}]),
     ?_assert(silhueta_com_foldr([{12,7,16},{2,6,7},{1,11,5},{24,4,28},{3,13,9},{19,18,22},{23,13,29},{14,3,25}])
                       =:= [{1,11},{3,13},{9,0},{12,7},{16,3},{19,18},{22,3},{23,13},{29,0}])
    ].

  algoritmo1_test_() ->
       [?_assert(algoritmo1([{12,7,16},{2,6,7},{1,11,5},{24,4,28},{3,13,9},{19,18,22},{23,13,29},{14,3,25}])
                        =:= [{1,11},{3,13},{9,0},{12,7},{16,3},{19,18},{22,3},{23,13},{29,0}]),
        ?_assert(algoritmo1([]) =:= [])
	     ].

  algoritmo2_test_() ->
    [?_assert(algoritmo2([{12,7,16},{2,6,7},{1,11,5},{24,4,28},{3,13,9},{19,18,22},{23,13,29},{14,3,25}])
                     =:= [{1,11},{3,13},{9,0},{12,7},{16,3},{19,18},{22,3},{23,13},{29,0}]),
     ?_assert(algoritmo2([]) =:= [])
    ].

  uniao_test_() ->
    [?_assert(uniao([],[]) =:= []),
     ?_assert(uniao([{1,2},{3,0}],[]) =:= [{1,2},{3,0}]),
     ?_assert(uniao([],[{1,2},{3,0}]) =:= [{1,2},{3,0}]),
     ?_assert(uniao([{1,2},{2,0}],[{1,2},{1,2}]) =:= [{1,2},{2,0}]),
     ?_assert(uniao([{1,2},{4,0}],[{1,2},{3,0}]) =:= [{1,2},{4,0}]),
     ?_assert(uniao([{1,2},{3,0}],[{4,5},{6,0}]) =:= [{1,2},{3,0},{4,5},{6,0}]),
     ?_assert(uniao([{4,5},{6,0}],[{1,2},{3,0}]) =:= [{1,2},{3,0},{4,5},{6,0}])
    ].





