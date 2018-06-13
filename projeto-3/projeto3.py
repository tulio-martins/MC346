import sys

def main():
    print("Ola Luis que tem 22 anos");
    print("Ola Andressa que tem 20 anos");
    ola_pessoa("Luis", 22)
    ola_pessoa("Andressa", 20)

def ola_pessoa(nome_da_pessoa, idade):
    print("Ola {} que tem {} anos".format(nome_da_pessoa, idade));

def main():
    arestas_dic = {}
    trans_public_dic = {}

    tempo_trans_public_dic = {}
    nos = set([])
    no_dic = {}

    # Lê arestas
    line = sys.stdin.readline()
    while line.strip() != "":
        # Obtem os elementos da linha
        separados = line.split()
        origem = separados[0]
        destino = separados[1]
        modo = separados[2]
        tempo = float(separados[3])

        # Adiciona a origem e o destino no conjunto de nos conhecidos
        if origem not in nos:
            nos.add(origem)
        if destino not in nos:
            nos.add(destino)

        if modo == "a-pe":
            # Adiciona a aresta no dicionario de arestas
            if origem not in arestas_dic:
                arestas_dic[origem] = {}
                arestas_dic[origem][destino] = (modo, tempo)
            else:
                arestas_dic[origem][destino] = (modo, tempo)
        else:
            # Adiciona a aresta no dicionario de transportes publicos
            if modo not in trans_public_dic:
                trans_public_dic[modo] = {}
                trans_public_dic[modo][origem] = ([(destino, tempo)], False)
                trans_public_dic[modo][destino] = ([], False)
            else:
                if origem not in trans_public_dic[modo]:
                    trans_public_dic[modo][origem] = ([(destino, tempo)], False)
                else:
                    trans_public_dic[modo][origem][0].append((destino, tempo))

                if destino not in trans_public_dic[modo]:
                    trans_public_dic[modo][destino] = ([], False)


        line = sys.stdin.readline()

    # Lê tempo para entrar no transporte publico
    line = sys.stdin.readline()
    while line.strip() != "":
        # Obtem os elementos da linha
        separados = line.split()
        modo = separados[0]
        tempo = float(separados[1])

        # Adiciona o tempo para entrar no transporte publico no dicionario
        tempo_trans_public_dic[modo] = tempo

        line = sys.stdin.readline()

    line = sys.stdin.readline()
    separados = line.split()
    origem = separados[0]
    destino = separados[1]

    #print("---------------------------------")
    #print("Nos")
    #print(nos)
    #print("---------------------------------")
    #print("Dicionario de arestas")
    #print(arestas_dic)
    #print("---------------------------------")
    #print("Dicionario de transporte publicos")
    #print(trans_public_dic)
    #print("---------------------------------")
    #print("Dicionario de tempo dos transportes publicos")
    #print(tempo_trans_public_dic)
    #print("---------------------------------")
    gera_arestas_trans_public(arestas_dic, trans_public_dic, 
            tempo_trans_public_dic)
    #print("Dicionario de arestas")
    #print(arestas_dic)
    #print("---------------------------------")
    dist, pai = dijkstra(nos, arestas_dic, origem)
    #print("Distancias")
    #print(dist)
    #print("Pais")
    #print(pai)
    #print("---------------------------------")
    imprime_caminho(arestas_dic, origem, destino, pai)
    print()
    print(dist[destino])

def imprime_caminho(arestas_dic, origem, destino, pais):
    if destino == origem:
        print(destino, end='')
        return

    imprime_caminho(arestas_dic, origem, pais[destino], pais)
    print(" {}".format(arestas_dic[pais[destino]][destino][0]), end='')
    print(" {}".format(destino), end='')
    return


def gera_arestas_trans_public(arestas_dic, trans_public_dic, 
        tempo_trans_public_dic):

    # Para todas as linhas de transporte publico
    for linha, linha_dic in trans_public_dic.items():

        # Para todos os pontos dessa linha que levam a algum lugar
        tempo_linha = tempo_trans_public_dic[linha]
        for origem in linha_dic.keys():
            # Executa um dfs encontrando todos os pontos possiveis de se chegar
            dfs_linha_trans_public(arestas_dic, origem, linha, linha_dic, 
                    tempo_linha)

def dfs_linha_trans_public(arestas_dic, origem, linha, linha_trans_pubic_dic, 
        tempo_linha):

    #print("LINHA TOP")
    #print(linha_trans_pubic_dic)
    linha_trans_pubic_dic[origem] = (linha_trans_pubic_dic[origem][0], True)

    # Para todos os pontos que esse ponto leva diretamente
    for (destino, tempo) in linha_trans_pubic_dic[origem][0]:
        dfs_linha_trans_public_aux(arestas_dic, origem, destino, linha, 
                linha_trans_pubic_dic, tempo+tempo_linha)

    linha_trans_pubic_dic[origem] = (linha_trans_pubic_dic[origem][0], False)

def dfs_linha_trans_public_aux(arestas_dic, origem, atual, linha,
        linha_trans_pubic_dic, tempo_total):

    #print("LINHA TOP TOP")
    #print(linha_trans_pubic_dic)
    #print(origem)
    #print(atual)
    if linha_trans_pubic_dic[atual][1] == True:
        return

    linha_trans_pubic_dic[atual] = (linha_trans_pubic_dic[atual][0], True)

    # Atualiza o valor da aresta origem, atual caso tenha encontrado um modo
    # mais rapido para ir de origem até atual ou adiciona a aresta caso ela nao
    # existisse
    if atual in arestas_dic[origem]:
        (modo, tempo) = arestas_dic[origem][atual]
        if tempo_total < tempo:
            arestas_dic[origem][atual] = (linha, tempo_total)
    else:
        arestas_dic[origem][atual] = (linha, tempo_total)

    # Se o no atual leva para algum outro nessa linha
    for (novo_atual, tempo) in linha_trans_pubic_dic[atual][0]:
        dfs_linha_trans_public_aux(arestas_dic, origem, novo_atual, linha, 
                linha_trans_pubic_dic, tempo_total+tempo)

    linha_trans_pubic_dic[atual] = (linha_trans_pubic_dic[atual][0], False)
    return

def extract_min(q, dist):
    it = iter(q)
    menor_no = next(it)
    menor_dist = dist[menor_no]
    for no in it:
        no_dist = dist[no]
        if no_dist < menor_dist:
            menor_no = no
            menor_dist = no_dist

    return menor_no, menor_dist


def dijkstra(nos, arestas_dic, origem):

    dist = {}
    pai = {}
    q = set()

    for no in nos:
        dist[no] = float("inf")
        q.add(no)

    dist[origem] = 0
    while len(q) > 0:
        menor_no, menor_dist = extract_min(q, dist)
        q.remove(menor_no)

        if menor_no in arestas_dic:
            for vizinho, aresta in arestas_dic[menor_no].items():
                modo = aresta[0]
                dist_aresta = aresta[1]
                possivel_dist = menor_dist + dist_aresta
                if possivel_dist < dist[vizinho]:
                    dist[vizinho] = possivel_dist
                    pai[vizinho] = menor_no

    return dist, pai


if __name__ == "__main__":
    main()
