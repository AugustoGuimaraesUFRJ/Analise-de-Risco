#Augusto Guimarães
#Fábio Bocampagni
#Isaque Araujo
#Lucas Hélio

#Questao 1

# Analitica:
# media do tempo de uma placa: (3h45,5h30,4h15)/3 = 4.5h
# tempo total: 562 * 4.5 = 2529 horas
# custo mao de obra: = 2529 * 7.5 = 18967,5

# Monte Carlos
q1<-function(){
  Ns <-1000
  i<-1
  amostras <-vector(mode="integer",Ns)
  while(i<Ns+1){
    tempo_placas_minutos<-rtriangle(562,225, 330, 255) # Tempo convertido para minutos
    tempo_total_horas <-sum(tempo_placas_minutos)/60 # Tempo convertido novamente para horas
    amostras[i]<-tempo_total_horas*7.5 #Custos de Mão de Obra
    i<-i+1
  }
  hist(amostras, main="Histograma do Custo de Mão de Obra",
       xlab="Custo de Mão de Obra (USD)", ylab="Frequência", col="lightblue")
  plot(ecdf(amostras), main="Distribuição Cumulativa do Custo de Mão de Obra",
       xlab="Custo de Mão de Obra (USD)", ylab="Probabilidade")
  
  # Cria um gráfico Q-Q plot para comparar com a distribuição normal teorica
  qqnorm(amostras, main="Gráfico Q-Q dos Custos de Mão de Obra")
  qqline(amostras, col="red")  # Adiciona uma linha de referência
  #amostras
}

# Questao 2
q2<-function(){
  Ns <-1000
  carros_ativos <- rtriangle(Ns, 15, 20, 18)
  consumo_gasolina <- rtriangle(Ns, 40, 60, 58)
  custo_gasolina <- rtriangle(Ns, 5.7, 7.2, 6.0)
  custo_diario <- carros_ativos * consumo_gasolina * custo_gasolina
  media_custo_diario <- mean(custo_diario)
  desvio_padrao_custo_diario <- sd(custo_diario)
  percentil_10 <- quantile(custo_diario, 0.1)
  percentil_90 <- quantile(custo_diario, 0.9)
  
  cat("Média de custo diário: ", media_custo_diario, "\n")
  cat("Desvio padrão de custo diário: ", desvio_padrao_custo_diario, "\n")
  cat("Percentil 10 de custo diário: ", percentil_10, "\n")
  cat("Percentil 90 de custo diário: ", percentil_90, "\n")
  
  # Cria um gráfico Q-Q plot para comparar com a distribuição normal teorica
  qqnorm(custo_diario, main="Gráfico Q-Q dos Custo")
  qqline(custo_diario, col="red")  # Adiciona uma linha de referência
}

#Questao 3
q3<-function(){
  cat("Risco do Custo anual com 40 sextas-feiras: ", sum(rtriangle(40,16,22,18) * rtriangle(40,25,36,28)), "\n")
  cat("Risco do Custo anual com 41 sextas-feiras: ", sum(rtriangle(41,16,22,18) * rtriangle(41,25,36,28)), "\n")
  cat("Risco do Custo anual com 42 sextas-feiras: ", sum(rtriangle(42,16,22,18) * rtriangle(42,25,36,28)), "\n")
}

#Questão 4
  q4<-function(){
    i<-1
    j<-1
    Ns<-1000
    amostras_meses <-vector(mode="double",12)
    amostras<-vector(mode="double",Ns)
    while(j<Ns+1){
      while (i<12+1) {
        
        gasto_grupo<-rtriangle(30, 90, 250, 130)
        
        frequencia_diaria<-rtriangle(30, 40, 120, 60)
        
        porcentagem_lucro<-rtriangle(30, 0.15, 0.30, 0.22)
        
        
        #PRECISA TRAZER PARA O VALOR PRESENTE
        #13,25%
        
        amostras_meses[i]<-sum(gasto_grupo*frequencia_diaria*porcentagem_lucro)/(1+0.1325)
        i<-i+1
      }
      i<-1
      amostras[j]<-sum(amostras_meses)
      j<-j+1
    }
    
    
    cat("media:", mean(amostras))
    hist(amostras, main="Histograma do Lucro no Primeiro Ano",
         #xlab="Meses do ano", ylab="Lucro")
         #plot(amostras, main="Plot do Lucro Mensal",
         xlab="Lucros", ylab="Frequencia")
    
  }