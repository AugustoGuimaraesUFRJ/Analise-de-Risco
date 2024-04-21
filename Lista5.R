# Grupo 4
# Augusto Guimarães
# Fábio Bocampagni
# Isaque Araujo
# Lucas Hélio

#Material de apoio a Lista 5
library(triangle)
library(igraph)
source('funcCpm.r')

#1-Parâmetros
Ns=3000
nivelRisco=0.85
#1.1-parametros do grafo
n=14
d=c(0,6,5,3,1,6,2,1,4,3,2,3,5,0)
Suc=list(c(2,3,4),9,c(5,6,7),8,
         10,12,c(8,11),13,
         14,12,12,13,
         14,0)
Pre=list(0,1,1,1,
         3,3,3,4,
         2,5,7,c(6,10,11),
         c(8,12),c(9,13))
elos=c(1,2,1,3,1,4,2,9,3,5,3,6,3,7,4,8,5,10,6,12,
       7,8,7,11,8,13,9,14,10,12,11,12,12,13,13,14)

#1.2-verificando grafo
g<-make_graph(elos)
tkplot(g,vertex.color='white')

#2-geração simplificada dos cenários
Cenarios<-matrix(ncol=n,nrow=Ns)
Cenarios[,1]=0
Cenarios[,n]=0
#gera valores entre 0.2 d e 2d
for (i in 2:(n-1)){
  #Nota: nao sao os parametros da lista 5
  Cenarios[,i]<-ceiling(rtriangle(Ns,0.2*d[i],2*d[i],d[i]))
}
#3-avaliacao dos cenarios

#3.1-estruturas de dados
Prazo<-vector(length=Ns)
probCC<-rep(0,n)
Agendas<-matrix(nrow=Ns,ncol=n)

#3.2-iteracao sobre os cenarios
for (i in 1:Ns){
  r<-funcCpm(n,Cenarios[i,],Suc,Pre)
  est<-r$est
  slack<-r$slack
  Agendas[i,]<-est
  Prazo[i]<-est[n]+Cenarios[i,n]
  indCritico<-which(slack==0)
  probCC[indCritico]<-probCC[indCritico]+1
  
}

#4-analise dos resultados
#4.1-risco de prazo
# hist(Prazo)
#4.2-histogramam da agenda  da atividade 10
# hist(Agendas[,10])
#4.3-probabilidades do CC
probCC<-round(probCC/Ns,2)
#4.4-agenda com risco 15%
ordPrazo<-order(Agendas[,n],decreasing = T)
indRisco<-round((1-nivelRisco)*Ns)
Agendas[ordPrazo,5:14]
Agendas[indRisco,]

# .
# Uma aproximacao empirica para a duração total do projeto mostrado.
q1<-function(){
  aproximacao_empirica_duracao_total <- mean(Prazo)
  # Imprime a aproximação empírica
  print(aproximacao_empirica_duracao_total)}

# .
# Uma aproximação empirica para a probabilidade das atividades pertencerem ao caminho critico
q2 <- function() {
  # Probabilidade das atividades pertencerem ao caminho crítico
  atividades <- 1:n
  probabilidade_atividades_no_caminho_critico <- data.frame(Atividade = atividades, Probabilidade = probCC)
  print(probabilidade_atividades_no_caminho_critico)
}

# .
# Supondo a politica de agendamento ”alocar todas atividades no tempo mais
# cedo de inicio”. Obtenha uma aproximaçãoo empirica para a distribuição de
# probabilidade do agendamento de cada uma das atividades.
q3 <- function(){
  limite_minimo <- 0  # Defina o limite mínimo desejado
  # Calculando a distribuição de probabilidade do agendamento de cada atividade
  atividades <- 2:(n-1)  # Excluindo as atividades fictícias do início e do final
  
  # Inicialize uma matriz para armazenar as probabilidades de agendamento para cada atividade
  probabilidade_agendamento <- matrix(ncol = length(atividades), nrow = Ns)
  
  # Itere sobre os cenários e calcule as probabilidades de agendamento
  for (atividade in atividades) {
    for (i in 1:Ns) {
      # Verifique o tempo de início da atividade no cenário atual
      tempo_inicio <- Agendas[i, atividade]
      tempo_inicio <- max(tempo_inicio, limite_minimo)
      # Atualize a contagem de probabilidades de agendamento
      probabilidade_agendamento[i, atividade - 1] <- tempo_inicio
    }
  }
  
  # Normalize as probabilidades para obter uma distribuição de probabilidade
  probabilidade_agendamento <- probabilidade_agendamento / max(probabilidade_agendamento)

  plot(probabilidade_agendamento[, 1], 
       type = "b", pch = 19, col = "blue",
       xlab = "Atividade 2", ylab = "Probabilidade de Agendamento",
       main = "Distribuição de Probabilidade do Agendamento")
  plot( probabilidade_agendamento[, 2], 
       type = "b", pch = 19, col = "blue",
       xlab = "Atividade 3", ylab = "Probabilidade de Agendamento",
       main = "Distribuição de Probabilidade do Agendamento")
  plot(probabilidade_agendamento[, 3], 
       type = "b", pch = 19, col = "blue",
       xlab = "Atividade 4", ylab = "Probabilidade de Agendamento",
       main = "Distribuição de Probabilidade do Agendamento")
  hist(probabilidade_agendamento[, 4], main = "Distribuição de Probabilidade da Atividade 5", xlab = "Probabilidade Tempo de Início")
  hist(probabilidade_agendamento[, 5], main = "Distribuição de Probabilidade da Atividade 6", xlab = "Probabilidade Tempo de Início")
  hist(probabilidade_agendamento[, 6], main = "Distribuição de Probabilidade da Atividade 7", xlab = "Probabilidade Tempo de Início")
  hist(probabilidade_agendamento[, 7], main = "Distribuição de Probabilidade da Atividade 8", xlab = "Probabilidade Tempo de Início")
  hist(probabilidade_agendamento[, 8], main = "Distribuição de Probabilidade da Atividade 9", xlab = "Probabilidade Tempo de Início")
  hist(probabilidade_agendamento[, 9], main = "Distribuição de Probabilidade da Atividade 10", xlab = "Probabilidade Tempo de Início")
  hist(probabilidade_agendamento[, 10], main = "Distribuição de Probabilidade da Atividade 11", xlab = "Probabilidade Tempo de Início")
  hist(probabilidade_agendamento[, 11], main = "Distribuição de Probabilidade da Atividade 12", xlab = "Probabilidade Tempo de Início")
  hist(probabilidade_agendamento[, 12], main = "Distribuição de Probabilidade da Atividade 13", xlab = "Probabilidade Tempo de Início")

}

# .
# Sabendo que a definição formal de um cronograma  ́e um conjunto de triplas
#{(ai, si, di)}, (atividade, data de inicio, duração), obtenha um cronograma de
# menor duração para este projeto que tenha uma chance de 85% de ser cumprido.
q4<- function(){
  # número de simulações Monte Carlo
  Ns <- 10000
  
  # Inicialize um vetor para armazenar as durações simuladas do projeto
  duracoes_simuladas <- numeric(Ns)
  
  # Realize simulações Monte Carlo para encontrar o cronograma ideal
  for (i in 1:Ns) {
    # Gere cenários de duração para cada atividade (da mesma forma que você fez)
    Cenarios <- matrix(ncol = n, nrow = 1)
    Cenarios[, 1] = 0
    Cenarios[, n] = 0
    for (j in 2:(n - 1)) {
      Cenarios[, j] <- ceiling(rtriangle(1, 0.2 * d[j], 2 * d[j], d[j]))
    }
    
    # Calcule as datas de início das atividades usando a função funcCpm
    r <- funcCpm(n, Cenarios, Suc, Pre)
    est <- r$est
    
    # Calcule a duração total do projeto
    duracao_projeto <- est[n] + Cenarios[1, n]
    
    # Armazene a duração simulada
    duracoes_simuladas[i] <- duracao_projeto
  }
  
  # Determine o percentil que corresponde a 85% de probabilidade
  percentil_85 <- quantile(duracoes_simuladas, 0.85)
  
  # Filtre as simulações que estão dentro do percentil desejado
  simulacoes_cumprem_prazo <- duracoes_simuladas <= percentil_85
  
  # Selecione a menor duração que atende à probabilidade desejada
  cronograma_otimizado <- min(duracoes_simuladas[simulacoes_cumprem_prazo])
  
  # Imprimir o cronograma otimizado
  cat("Duração do cronograma otimizado com 85% de probabilidade de cumprimento:", cronograma_otimizado, "\n")
  
}

# . 
# Gere um diagrama de Gantt com o cronograma definido no item anterior.
q5<-function(){
  # Carregue a biblioteca plotly
  library(plotly)
  
  # Suponha que você tenha um índice válido para o cenário com risco de 15%
  indice_risco <- indRisco
  
  # Gerar cores aleatórias para cada atividade (exemplo: usando cores no espaço RGB)
  cores_aleatorias <- sample(colors(), n - 2, replace = FALSE)  # Subtrai 2 para excluir as atividades fictícias
  
  # Criar um data frame para o diagrama de Gantt
  df <- data.frame(
    Atividade = factor(2:(n-1), levels = 2:(n-1)),  # Exclua as atividades fictícias do início e do final
    Inicio = Agendas[indice_risco, 2:(n-1)],  # Use os dados do cenário com risco de 15%
    Fim = Agendas[indice_risco, 2:(n-1)] + Cenarios[indice_risco, 2:(n-1)],  # Início + Duração
    Cor = cores_aleatorias
  )
  
  # Criar o gráfico de Gantt com cores
  ggplot(df, aes(y = Atividade, x = Inicio, xend = Fim, yend = Atividade, color = Cor)) +
    geom_segment(linewidth = 10) +
    labs(x = "Tempo", y = "Atividade") +
    ggtitle("Diagrama de Gantt - Cronograma com 85% de Chances") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_color_identity()
}