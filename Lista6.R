# Grupo 4
# Augusto Guimarães
# Fábio Bocampagni
# Isaque Araujo
# Lucas Hélio

#Material de apoio a Lista 6
l6<-function(){
  library(MASS)
  library(triangle)
  library(Matrix)
  library(igraph)
  source('funcCpm.r')
  
  
  #1-Declaração dos parâmetros do modelo
  #Constantes
  Ns=3000
  nVar=10
  nVarDur=nVar+1
  #matriz de correlação
  A<-diag(x=1,nrow=nVar)
  A[1,2]<-0.9
  A[2,1]<-A[1,2]
  A[4,5]<-0.85
  A[5,4]<-A[4,5]
  A[6,7]<-0.90
  A[7,6]<-A[6,7]
  A[8,9]<-0.85
  A[9,10]<-A[8,9]
  A[8,10]<-A[8,9]
  A[10,9]<-A[8,9]
  A[10,8]<-A[8,9]
  A[9,8]<-A[8,9]
  #parametros das durações
  ptDur<-matrix(data=c(2,4,18,
                       5,9,19,
                       4,10,28,
                       8,13,36,
                       44,60,100,
                       30,40,74,
                       9,20,43,
                       24,30,48,
                       28,29,96,
                       10,10,12),ncol=3,byrow=T)
  
  #parametros dos itens de custo
  ptCusto<-matrix(data=c(300, 450, 600,
                         480, 600, 720,
                         3750, 4500, 5250,
                         8400, 9600, 10800,
                         300000, 312000,322500,
                         37650, 39600, 41400,
                         10500, 11550, 12600,
                         36000, 38400, 40800,
                         48750, 52500, 56250,
                         360, 450, 540), ncol=3,byrow=T)
  
  #grafo de precedencia
  #Nota:que precisamos criar um nó dummy para o grafo ficar bem formado
  Suc<-list(c(2,3),4,6,5,6,7,c(8,10),9,11,11,0)
  Pre<-list(0,1,1,2,4,c(3,5),6,7,8,7,c(9,10))
  elos<-c(1,2,1,3,2,4,4,5,5,6,3,6,6,7,7,8,7,10,8,9,9,11,10,11)
  
  genNCorrTriangsMv<-function(n=nVar,Ns=Ns,
                              A=matrix(rep(0,n*n),ncol=n),
                              pt=matrix(rep(0,3*n),ncol=n)
  )                                                         {
    m<-rep(0,times=n)
    Z<-mvrnorm(Ns,mu=m,Sigma=A)
    U<-pnorm(Z)
    Tri<-matrix(nrow=Ns,ncol=n)
    for (i in 1:n){
      Tri[,i]<-qtriangle(U[,i],pt[i,1],pt[i,3],pt[i,2])
    }
    Tri
  }
  
  #main
  
  #1-geração dos cenários
  cenarioCusto<-genNCorrTriangsMv(n=nVar,Ns=Ns,A,ptCusto)
  cenarioPrazo<-ceiling(genNCorrTriangsMv(n=nVar,Ns=Ns,A,ptDur))
  cenarioPrazo<-cbind(rep(0,Ns),cenarioPrazo)
  
  #2-risco de custo
  Custo<-apply(cenarioCusto,1,sum)
  
  #3-risco de prazo
  Agendas<-matrix(ncol=nVarDur,nrow=Ns)
  cCritico<-matrix(data=rep(0,nVarDur*Ns),ncol=nVarDur,nrow=Ns)
  Prazo<-vector(length=Ns)
  
  #4-risco  de prazo
  #4.1-gera grafo
  g<-make_graph(elos)
  #4.2-plota para verificao
  tkplot(g,vertex.color='white')
  #4.3-iteracao sobre cenarios de prazo
  for (i in 1:Ns){
    r<-funcCpm(n=nVarDur,cenarioPrazo[i,],Suc=Suc,Pre=Pre)
    #1-cronogramas de menor prazo
    Agendas[i,]<-r$est
    #2-menor prazo de realizao
    Prazo[i]<-r$est[nVar]+cenarioPrazo[i,nVarDur]
    #3-identificando e marcando atividades no CC
    indCrit<-which(r$slack==0)
    cCritico[i,indCrit]<-1
    probCC<-apply(cCritico,2,sum)/Ns
  }
  #5-retorno 
  r<-list(Custo=Custo,Agendas=Agendas,Prazo=Prazo,cCritico=cCritico,probCC=probCC)
}

q1<-function(){
  r<-l6()
  mean_costs <- mean(r$Custo)
  std_costs <- sd(r$Custo)
  # Histograma dos custos
  hist(r$Custo, breaks = 20, main = "Histograma de Custos", xlab = "Custo")
  
  cat("Média dos Custos:", mean_costs, "\n")
  cat("Desvio Padrão dos Custos:", std_costs, "\n")
  
  mean_durations <- mean(r$Prazo)
  std_durations <- sd(r$Prazo)
  # Histograma dos prazos
  hist(r$Prazo, breaks = 20, main = "Histograma de Prazos", xlab = "Prazo")
  
  cat("Média dos Prazos:", mean_durations, "\n")
  cat("Desvio Padrão dos Prazos:", std_durations, "\n")
}

q2<-function(){
  r<-l6()
  probabilidades_medias <- apply(r$cCritico, 2, mean)
  print(probabilidades_medias)
}

q3<-function(){
  resultados <- l6()
  
  par(mfrow=c(1,1))  # Defina o layout do gráfico
  plot(resultados$Prazo, resultados$Custo, 
       xlab = "Prazo", ylab = "Custo", 
       main = "Correlação entre Prazo e Custo da Obra", 
       pch = 19, col = "blue")
  
  tendencia <- lm(resultados$Custo ~ resultados$Prazo)
  abline(tendencia, col = "red")
  
  legend("topright", legend = paste("Correlação:", round(cor(resultados$Prazo, resultados$Custo), 2)),
         col = "red", lty = 1, cex = 0.8)
}

q4<-function(){
  l6_com_correlacao <- function() {
      library(MASS)
      library(triangle)
      library(Matrix)
      library(igraph)
      source('funcCpm.r')
      
      
      #1-Declaração dos parâmetros do modelo
      #Constantes
      Ns=3000
      nVar=10
      nVarDur=nVar+1
      #matriz de correlação
      A<-diag(x=1,nrow=nVar)
      A[1,2]<-0.9
      A[2,1]<-A[1,2]
      A[4,5]<-0.85
      A[5,4]<-A[4,5]
      A[6,7]<-0.90
      A[7,6]<-A[6,7]
      A[8,9]<-0.85
      A[9,10]<-A[8,9]
      A[8,10]<-A[8,9]
      A[10,9]<-A[8,9]
      A[10,8]<-A[8,9]
      A[9,8]<-A[8,9]
      #parametros das durações
      ptDur<-matrix(data=c(2,4,18,
                           5,9,19,
                           4,10,28,
                           8,13,36,
                           44,60,100,
                           30,40,74,
                           9,20,43,
                           24,30,48,
                           28,29,96,
                           10,10,12),ncol=3,byrow=T)
      
      #parametros dos itens de custo
      ptCusto<-matrix(data=c(300, 450, 600,
                             480, 600, 720,
                             3750, 4500, 5250,
                             8400, 9600, 10800,
                             300000, 312000,322500,
                             37650, 39600, 41400,
                             10500, 11550, 12600,
                             36000, 38400, 40800,
                             48750, 52500, 56250,
                             360, 450, 540), ncol=3,byrow=T)
      
      #grafo de precedencia
      #Nota:que precisamos criar um nó dummy para o grafo ficar bem formado
      Suc<-list(c(2,3),4,6,5,6,7,c(8,10),9,11,11,0)
      Pre<-list(0,1,1,2,4,c(3,5),6,7,8,7,c(9,10))
      elos<-c(1,2,1,3,2,4,4,5,5,6,3,6,6,7,7,8,7,10,8,9,9,11,10,11)
      
      genNCorrTriangsMv<-function(n=nVar,Ns=Ns,
                                  A=matrix(rep(0,n*n),ncol=n),
                                  pt=matrix(rep(0,3*n),ncol=n)
      )                                                         {
        m<-rep(0,times=n)
        Z<-mvrnorm(Ns,mu=m,Sigma=A)
        U<-pnorm(Z)
        Tri<-matrix(nrow=Ns,ncol=n)
        for (i in 1:n){
          Tri[,i]<-qtriangle(U[,i],pt[i,1],pt[i,3],pt[i,2])
        }
        Tri
      }
      
      #main
      
      #1-geração dos cenários
      cenarioCusto<-genNCorrTriangsMv(n=nVar,Ns=Ns,A,ptCusto)
      cenarioPrazo<-ceiling(genNCorrTriangsMv(n=nVar,Ns=Ns,A,ptDur))
      cenarioPrazo<-cbind(rep(0,Ns),cenarioPrazo)
      
      #2-risco de custo
      Custo<-apply(cenarioCusto,1,sum)
      
      #3-risco de prazo
      Agendas<-matrix(ncol=nVarDur,nrow=Ns)
      cCritico<-matrix(data=rep(0,nVarDur*Ns),ncol=nVarDur,nrow=Ns)
      Prazo<-vector(length=Ns)
      
      #4-risco  de prazo
      #4.1-gera grafo
      g<-make_graph(elos)
      #4.2-plota para verificao
      tkplot(g,vertex.color='white')
      #4.3-iteracao sobre cenarios de prazo
      for (i in 1:Ns){
        r<-funcCpm(n=nVarDur,cenarioPrazo[i,],Suc=Suc,Pre=Pre)
        #1-cronogramas de menor prazo
        Agendas[i,]<-r$est
        #2-menor prazo de realizao
        Prazo[i]<-r$est[nVar]+cenarioPrazo[i,nVarDur]
        #3-identificando e marcando atividades no CC
        indCrit<-which(r$slack==0)
        cCritico[i,indCrit]<-1
        probCC<-apply(cCritico,2,sum)/Ns
      }
      #5-retorno 
      r<-list(Custo=Custo,Agendas=Agendas,Prazo=Prazo,cCritico=cCritico,probCC=probCC)
  }
  
  # Função para executar o modelo sem correlação (matriz de correlação de identidade)
  l6_sem_correlacao <- function() {
      library(MASS)
      library(triangle)
      library(Matrix)
      library(igraph)
      source('funcCpm.r')
      
      
      #1-Declaração dos parâmetros do modelo
      #Constantes
      Ns=3000
      nVar=10
      nVarDur=nVar+1
      #matriz de correlação
      A<-diag(x=1,nrow=nVar)
      #parametros das durações
      ptDur<-matrix(data=c(2,4,18,
                           5,9,19,
                           4,10,28,
                           8,13,36,
                           44,60,100,
                           30,40,74,
                           9,20,43,
                           24,30,48,
                           28,29,96,
                           10,10,12),ncol=3,byrow=T)
      
      #parametros dos itens de custo
      ptCusto<-matrix(data=c(300, 450, 600,
                             480, 600, 720,
                             3750, 4500, 5250,
                             8400, 9600, 10800,
                             300000, 312000,322500,
                             37650, 39600, 41400,
                             10500, 11550, 12600,
                             36000, 38400, 40800,
                             48750, 52500, 56250,
                             360, 450, 540), ncol=3,byrow=T)
      
      #grafo de precedencia
      #Nota:que precisamos criar um nó dummy para o grafo ficar bem formado
      Suc<-list(c(2,3),4,6,5,6,7,c(8,10),9,11,11,0)
      Pre<-list(0,1,1,2,4,c(3,5),6,7,8,7,c(9,10))
      elos<-c(1,2,1,3,2,4,4,5,5,6,3,6,6,7,7,8,7,10,8,9,9,11,10,11)
      
      genNCorrTriangsMv<-function(n=nVar,Ns=Ns,
                                  A=matrix(rep(0,n*n),ncol=n),
                                  pt=matrix(rep(0,3*n),ncol=n)
      )                                                         {
        m<-rep(0,times=n)
        Z<-mvrnorm(Ns,mu=m,Sigma=A)
        U<-pnorm(Z)
        Tri<-matrix(nrow=Ns,ncol=n)
        for (i in 1:n){
          Tri[,i]<-qtriangle(U[,i],pt[i,1],pt[i,3],pt[i,2])
        }
        Tri
      }
      
      #main
      
      #1-geração dos cenários
      cenarioCusto<-genNCorrTriangsMv(n=nVar,Ns=Ns,A,ptCusto)
      cenarioPrazo<-ceiling(genNCorrTriangsMv(n=nVar,Ns=Ns,A,ptDur))
      cenarioPrazo<-cbind(rep(0,Ns),cenarioPrazo)
      
      #2-risco de custo
      Custo<-apply(cenarioCusto,1,sum)
      
      #3-risco de prazo
      Agendas<-matrix(ncol=nVarDur,nrow=Ns)
      cCritico<-matrix(data=rep(0,nVarDur*Ns),ncol=nVarDur,nrow=Ns)
      Prazo<-vector(length=Ns)
      
      #4-risco  de prazo
      #4.1-gera grafo
      g<-make_graph(elos)
      #4.2-plota para verificao
      tkplot(g,vertex.color='white')
      #4.3-iteracao sobre cenarios de prazo
      for (i in 1:Ns){
        r<-funcCpm(n=nVarDur,cenarioPrazo[i,],Suc=Suc,Pre=Pre)
        #1-cronogramas de menor prazo
        Agendas[i,]<-r$est
        #2-menor prazo de realizao
        Prazo[i]<-r$est[nVar]+cenarioPrazo[i,nVarDur]
        #3-identificando e marcando atividades no CC
        indCrit<-which(r$slack==0)
        cCritico[i,indCrit]<-1
        probCC<-apply(cCritico,2,sum)/Ns
      }
      #5-retorno 
      r<-list(Custo=Custo,Agendas=Agendas,Prazo=Prazo,cCritico=cCritico,probCC=probCC)
  }
  
  # Executar o modelo com correlação
  resultados_com_correlacao <- l6_com_correlacao()
  
  # Executar o modelo sem correlação
  resultados_sem_correlacao <- l6_sem_correlacao()
  
  # Comparar os resultados como anteriormente
  media_custo_com_correlacao <- mean(resultados_com_correlacao$Custo)
  media_prazo_com_correlacao <- mean(resultados_com_correlacao$Prazo)
  
  media_custo_sem_correlacao <- mean(resultados_sem_correlacao$Custo)
  media_prazo_sem_correlacao <- mean(resultados_sem_correlacao$Prazo)
  
  # Exibindo as médias
  cat("Média de Custo com correlação:", media_custo_com_correlacao, "\n")
  cat("Média de Prazo com correlação:", media_prazo_com_correlacao, "\n")
  cat("Média de Custo sem correlação:", media_custo_sem_correlacao, "\n")
  cat("Média de Prazo sem correlação:", media_prazo_sem_correlacao, "\n")
  
}