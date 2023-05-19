/* Wilner Lucas Firmino de França - 405023 */
/* Trabalho 03 - ALGORITMO GENÉTICO - CAIXEIRO VIAJANTE */

clear;  // Limpa as variáveis armazenadas
clc;    // Limpa a tela

base = fscanfMat ('caixeiro.dat'); // Base de dados fornecida na questão - Matriz adjacente

// PASSO 01: Precisamos criar a geração inicial de cromossosmos 
geracao = 100;
individuos = zeros(100,14);
score = zeros(100,1);
taxadeCrossover = 0.9
taxaMutacao = 0.7

//Gerando aleatorimente as cidades 
for i=1:100
    individuos(i,:)=grand(1, "prm",(1:14));
end

minimosCaminhos = [];

//Temos a primeira população 
for i=1:100
    caminho = individuos(i,:);
    for j=1:14
        if j < 14 then
            score(i) = score(i) + base(caminho(j),caminho(j+1))
        else
            score(i) = score(i) + base(caminho(j),caminho (1))
        end
    end
end

individuosP = zeros(100,15);
individuosP = [individuos,score];

  
//trocam-se as suas posições para minimizar 
for contagemGerac=1:geracao
    inversaD = 1./individuosP(:,15);
    individuosP(:,15) = inversaD
    
//Uso da roleta    
    pais = zeros(100,14);
    prob = (individuosP(:,15).*100)./sum(individuosP(:,15));
    prob = cumsum(prob);
    for i=1:1:length(individuosP(:,1))
        rou = rand()*100;
        j = 1;
        if(j>length(prob)),j=j-1; rou = 101; end
        while rou>=prob(j)
            j = j+1;
            if(j>length(prob)), j=j-1;break; end
        end
        pais(i,:) = individuosP(j,1:14) ;
    end
    
    
// PASSO 02: Criando as funções de cruzamento e mutação
function [filho1 , filho2] = crossover(pai ,mae)
    pontoCorte1 = 2
    pontoCorte2 = 7
    paiMiddleCross = pai( pontoCorte1 : pontoCorte2 )
    maeMiddleCross = mae( pontoCorte1 : pontoCorte2 )
    
    temp_filho1 (1: pontoCorte1 -1) = pai (1: pontoCorte1 -1)
    temp_filho1 (pontoCorte2 + 1: 14) = pai( pontoCorte2 +1: 14)
    temp_filho1 (pontoCorte1 :pontoCorte2 ) = maeMiddleCross
    
    temp_filho2 (1: pontoCorte1 -1) = mae (1: pontoCorte1 -1)
    temp_filho2 (pontoCorte2 + 1: 14) = mae( pontoCorte2 +1: 14)
    temp_filho2 (pontoCorte1 :pontoCorte2 ) = paiMiddleCross
    
    relations = [ maeMiddleCross ; paiMiddleCross ]'
    
    filho1 = recursion1(temp_filho1,pontoCorte1,pontoCorte2,paiMiddleCross,maeMiddleCross);
    filho2 = recursion2(temp_filho2,pontoCorte1,pontoCorte2,paiMiddleCross,maeMiddleCross);

endfunction
    
function [populacaoMutante] = mutacao(populacao,taxaMutacao)
    for i=1:length(populacao (1,:))
        if(rand()<=taxaMutacao)
            ind = populacao(i,:);
            numeroAleatorio = [3,9];
            p1 = numeroAleatorio(1,1);
            p2 = numeroAleatorio(1,2);
            aux = ind(1,p1);
            ind(1,p1) = ind(1,p2);
            ind(1,p2) = aux;
            populacao(i,:) = ind;
        end
    end
    populacaoMutante = populacao;
endfunction

function child1 = recursion1(temp_filho,pontoCorte1,pontoCorte2,parent1MiddleCross,parent2MiddleCross)
    child1 = zeros (1,length(temp_filho));
    matriceNumerate = []
    tempCrossPoint = temp_filho (1:pontoCorte1-1)
    for i=1:length(tempCrossPoint)
        matriceNumerate = [matriceNumerate;i,tempCrossPoint(i)]
    end
    
    for i = 1:length( matriceNumerate(:,1))
        c=0
        for j = 1: length(relations(:,1))
            if matriceNumerate(i,2) == relations(j,1)
                child1(i) = relations(j,2)
                c=1
                break;
            end
        end
        if c==0
            child1(i) = matriceNumerate(i,2)
        end
    end
    
    j=1
    for i = pontoCorte1:pontoCorte2
        child1(i) = parent2MiddleCross (j)
        j = j + 1
    end
    
    tempCrossPoint2 = temp_filho(pontoCorte2+1:14)
    matriceNumerate2 = []
    for i=1:length(tempCrossPoint2)
        matriceNumerate2 = [matriceNumerate2;i,tempCrossPoint2(i)]
    end
    
    for i=1:length(matriceNumerate2(:,1))
        c=0
        for j = 1:length(relations(:,1))
            if matriceNumerate2(i, 2)==relations(j,1)
                child1(i+pontoCorte2)=relations(j,2)
                c=1
                break;
            end
        end
        if c==0
            child1(i+pontoCorte2)=matriceNumerate2(i,2)
        end
    end
    
    parteantes = child1(1:pontoCorte2)
    ultimaparte = child1(pontoCorte2+1:14)
    
    if length(child1)>length(unique(child1)) then
        child1=recursion1(child1,pontoCorte1,pontoCorte2,parent1MiddleCross,parent2MiddleCross)
    end
endfunction

function child2 = recursion2(temp_filho, pontoCorte1,pontoCorte2,parent1MiddleCross,parent2MiddleCross)
    child2 = zeros(1,length(temp_filho));
    matriceNumerate = []
    tempCrossPoint = temp_filho(1:pontoCorte1-1)
    for i=1:length(tempCrossPoint)
        matriceNumerate = [matriceNumerate;i,tempCrossPoint(i)]
    end
    
    for i=1:length(matriceNumerate(:,1))
        c=0
        for j=1:length(relations(:,1))
            if matriceNumerate(i,2)==relations(j,2)
                child2(i)=relations(j,1)
                c=1
                break;
            end
        end
        if c==0
            child2(i)=matriceNumerate(i,2)
        end
    end
    
    j=1
    for i=pontoCorte1:pontoCorte2
        child2(i)=parent1MiddleCross(j)
        j=j+1
    end
    
    tempCrossPoint2 = temp_filho(pontoCorte2+1:14)
    matriceNumerate2 = []
    for i=1:length(tempCrossPoint2)
        matriceNumerate2 = [matriceNumerate2;i,tempCrossPoint2(i)]
    end
    
    for i =1:length(matriceNumerate2(:,1))
        c=0
        for j=1:length(relations(:,1))
            if matriceNumerate2(i,2)==relations(j,2)
                child2(i+pontoCorte2 )=relations(j,1)
                c=1
                break;
            end
        end
        if c==0
            child2(i+pontoCorte2)=matriceNumerate2(i,2)
        end
    end
    if length(child2)>length(unique(child2)) then
        child2=recursion2(child2,pontoCorte1,pontoCorte2,parent1MiddleCross,parent2MiddleCross)
    end
endfunction


//PASSO 03: Neste passo acontece a geração dos filhos - Crossover de mapa parcial
novaPopulacao = []
    for i=1:100
        if pmodulo(i,2) == 0
            pai = pais(i-1,:)
            mae = pais(i,:)
            if(rand () <= taxadeCrossover )
                [filho1 ,filho2] = crossover(pai ,mae)
                novaPopulacao = [novaPopulacao;filho1;filho2]
            else
                novaPopulacao = [novaPopulacao;pai;mae]
            end
        end
    end

// A operação mutação é utilizada para garantir uma maior varredura do espaço
    populacaoMutante = mutacao(novaPopulacao,taxaMutacao )
    
    scoreMpopulacao = zeros(length( populacaoMutante (: ,1)), 1);
    
    for i=1: length( populacaoMutante (: ,1))
        caminho = populacaoMutante (i ,:);
        for j=1: length( populacaoMutante (i ,:))
            if j < 14 then
                scoreMpopulacao (i) = scoreMpopulacao (i) + base(caminho(j),caminho(j+1));
            else
                scoreMpopulacao (i) = scoreMpopulacao (i) + base(caminho(j),caminho (1));
            end
        end
    end
    
 //plotando o gráfico 
    plot(contagemGerac , min( scoreMpopulacao ),"bo")
    minimoG = min( scoreMpopulacao );
    for i=1: length( populacaoMutante (:,1))
        if(minimoG == scoreMpopulacao (i))
            minimosCaminhos = [ minimosCaminhos;[ populacaoMutante(i,:),scoreMpopulacao(i),contagemGerac]]
        end
    end
    
    individuosP = [populacaoMutante , scoreMpopulacao ]
end
    title(' Grafico do individuo com melhor score por populacao ', 'fontsize', 2)
minimoTG = min( minimosCaminhos (:,15))
    xlabel('Geração');
    ylabel('Distancia');
for i=1: length( minimosCaminhos (:,1))
    if(minimoTG == minimosCaminhos (i,15))
        disp("Melhor caminho encontrado : ")
        disp(minimosCaminhos (i,1:14))
        disp("A distancia total para esse caminho: ")
        disp(minimoTG)
        disp("Encontrado na geracao : ")
        disp(minimosCaminhos (i,16))
        break
    end
end
