# Grupo 4
# Augusto Guimarães
# Fábio Bocampagni
# Isaque Araujo
# Lucas Hélio

q1<-function(){
  Ns <-3000
  i<-1
  amostras_tempo_projeto <-vector(mode="integer",Ns)
  amostras_custo_projeto <-vector(mode="integer",Ns)
  while(i<Ns+1){
    tempo <- rtriangle(1,14,21,16)
    #Indica se houve um adicional ou não
    adicional_controle<-sample(c(0,1),1,replace = TRUE,prob=c(0.8,0.2))
    if(adicional_controle==0){
      amostras_tempo_projeto[i]<-tempo
      amostras_custo_projeto[i]<-160000
    }
    else{
      tempo_adicional<-rtriangle(1,3,6,4)
      amostras_tempo_projeto[i]<-tempo+tempo_adicional
      amostras_custo_projeto[i]<-160000+12000
    }
    i<-i+1
  }
  hist(amostras_tempo_projeto, main = "Distribuição de Tempo do Projeto", xlab = "Tempo (semanas)")
  hist(amostras_custo_projeto, main = "Distribuição de Custo do Projeto", xlab = "R$")
  
  #Grafico correlação
  par(mfrow=c(1,1))  # Defina o layout do gráfico
  plot(amostras_tempo_projeto, amostras_custo_projeto, 
       xlab = "Prazo", ylab = "Custo", 
       main = "Correlação entre Prazo e Custo do Projeto", 
       pch = 19, col = "blue")
  
  tendencia <- lm(amostras_custo_projeto ~ amostras_tempo_projeto)
  abline(tendencia, col = "red")
  
  legend("topright", legend = paste("Correlação do Projeto:", round(cor(amostras_tempo_projeto, amostras_custo_projeto), 2)),
         col = "red", lty = 1, cex = 0.8)
  
  r<-list(amostras_tempo_projeto=amostras_tempo_projeto,amostras_custo_projeto=amostras_custo_projeto)
}

q2<-function(){
  Ns <-3000
  i<-1
  amostras_tempo_terraplanagem<-vector(mode="integer",Ns)
  amostras_custo_terraplanagem <-vector(mode="integer",Ns)
  while(i<Ns+1){
    tempo <- rtriangle(1,3,7,4)
    #Indica se houve um adicional ou não
    adicional_controle<-sample(c(0,1),1,replace = TRUE,prob=c(0.7,0.3))
    if(adicional_controle==0){
      amostras_tempo_terraplanagem[i]<-tempo
      amostras_custo_terraplanagem[i]<-tempo*rtriangle(1,4200,4700,4500)
    }
    else{
      tempo_adicional<-rtriangle(1,8,14,10)
      amostras_tempo_terraplanagem[i]<-tempo+tempo_adicional
      amostras_custo_terraplanagem[i]<-(tempo+tempo_adicional)*rtriangle(1,4200,4700,4500)
    }
    i<-i+1
  }
  hist(amostras_tempo_terraplanagem, main = "Distribuição de Tempo da Terraplanagem", xlab = "Tempo (semanas)")
  hist(amostras_custo_terraplanagem, main = "Distribuição de Custo da Terraplanagem", xlab = "R$")
  
  #Grafico correlação
  par(mfrow=c(1,1))  # Defina o layout do gráfico
  plot(amostras_tempo_terraplanagem, amostras_custo_terraplanagem, 
       xlab = "Prazo", ylab = "Custo", 
       main = "Correlação entre Prazo e Custo da Terraplanagem", 
       pch = 19, col = "blue")
  
  tendencia <- lm(amostras_custo_terraplanagem ~ amostras_tempo_terraplanagem)
  abline(tendencia, col = "red")
  
  legend("topright", legend = paste("Correlação da Terraplanagem:", round(cor(amostras_tempo_terraplanagem, amostras_custo_terraplanagem), 2)),
         col = "red", lty = 1, cex = 0.8)
  
  r<-list(amostras_tempo_terraplanagem=amostras_tempo_terraplanagem,amostras_custo_terraplanagem=amostras_custo_terraplanagem)
}

q3<-function(){
  Ns<-3000
  amostras_tempo_fundacao<-rtriangle(3000,6,8,7)
  amostras_custo_fundacao<- amostras_tempo_fundacao*rtriangle(3000,2800,3300,3300) + rtriangle(3000,37000,40000,38500)
  hist(amostras_tempo_fundacao, main = "Distribuição de Tempo da Fundação", xlab = "Tempo (semanas)")
  hist(amostras_custo_fundacao, main = "Distribuição de Custo da Fundação", xlab = "R$")
  
  #Grafico correlação
  par(mfrow=c(1,1))  # Defina o layout do gráfico
  plot(amostras_tempo_fundacao, amostras_custo_fundacao, 
       xlab = "Prazo", ylab = "Custo", 
       main = "Correlação entre Prazo e Custo da Fundação", 
       pch = 19, col = "blue")
  
  tendencia <- lm(amostras_custo_fundacao ~ amostras_tempo_fundacao)
  abline(tendencia, col = "red")
  
  legend("topright", legend = paste("Correlação da Fundação:", round(cor(amostras_tempo_fundacao, amostras_custo_fundacao), 2)),
         col = "red", lty = 1, cex = 0.8)
  
  r<-list(amostras_tempo_fundacao=amostras_tempo_fundacao,amostras_custo_fundacao=amostras_custo_fundacao)
  
}

q4<-function(){
  Ns<-3000
  # Os componentes estruturais do prédio (pisos, pilares, cobertura) podem ser iniciados,
  # dependendo do tempo (3,4,6) semanas após o término do trabalho da fundação.
  tempo_primeiro_piso<-rtriangle(3000,3,6,4)+rtriangle(3000,4,6,4.5)
  tempo_segundo_piso<-rtriangle(3000,4,6,4.5)
  tempo_terceiro_piso<-rtriangle(3000,4,6,4.5)
  
  tempo_total_pisos<-tempo_primeiro_piso+tempo_segundo_piso+tempo_terceiro_piso
  
  custo_total_estrutura<-3*rtriangle(3000,4,6,4.5)*rtriangle(3000,4700,5500,5200)+3*rtriangle(3000,17200,18000,17500)+172000
  tempo_total_estrutura<-tempo_total_pisos+rtriangle(3000,7,10,8)
  
  hist(tempo_total_estrutura, main = "Distribuição de Tempo da Estrutura", xlab = "Tempo (semanas)")
  hist(custo_total_estrutura, main = "Distribuição de Custo da Estrutura", xlab = "R$")
  
  #Grafico correlação
  par(mfrow=c(1,1))  # Defina o layout do gráfico
  plot(tempo_total_estrutura, custo_total_estrutura, 
       xlab = "Prazo", ylab = "Custo", 
       main = "Correlação entre Prazo e Custo da Estrutura", 
       pch = 19, col = "blue")
  
  tendencia <- lm(custo_total_estrutura ~ tempo_total_estrutura)
  abline(tendencia, col = "red")
  
  legend("topright", legend = paste("Correlação da Estrutura:", round(cor(tempo_total_estrutura, custo_total_estrutura), 2)),
         col = "red", lty = 1, cex = 0.8)
  
  r<-list(tempo_primeiro_piso=tempo_primeiro_piso,tempo_segundo_piso=tempo_segundo_piso,
          tempo_terceiro_piso=tempo_terceiro_piso)
}

q5<-function(){
  Ns <-3000
  i<-1
  amostras_tempo_envoltoria_primeiro <-vector(mode="integer",Ns)
  amostras_tempo_envoltoria_segundo <-vector(mode="integer",Ns)
  amostras_tempo_envoltoria_terceiro <-vector(mode="integer",Ns)
  
  amostras_custo_envoltoria_primeiro <-vector(mode="integer",Ns)
  amostras_custo_envoltoria_segundo <-vector(mode="integer",Ns)
  amostras_custo_envoltoria_terceiro <-vector(mode="integer",Ns)
  
  r<-q4()

  while(i<Ns+1){
    #Indica se houve um adicional ou não
    
    adicional_controle<-sample(c(0,1),1,replace = TRUE,prob=c(0.9,0.1))
    if(adicional_controle==0){
      tempo_primeiro <- 3 + rtriangle(1,7,9,8)
      amostras_tempo_envoltoria_primeiro[i]<-tempo_primeiro  
      amostras_custo_envoltoria_primeiro[i]<-rtriangle(1,36000, 40000,37000)+9800 + 197000
      
      tempo_segundo <- 3 + rtriangle(1,7,9,8)
      amostras_tempo_envoltoria_segundo[i]<-tempo_segundo  
      amostras_custo_envoltoria_segundo[i]<-rtriangle(1,36000, 40000,37000) + 197000
      
      tempo_terceiro <- 3 + rtriangle(1,7,9,8)
      amostras_tempo_envoltoria_terceiro[i]<-tempo_terceiro  
      amostras_custo_envoltoria_terceiro[i]<-rtriangle(1,36000, 40000,37000) + 197000
    }
    else{
      tempo_primeiro <- 3 + rtriangle(1,6,11,8)
      amostras_tempo_envoltoria_primeiro[i]<-tempo_primeiro  
      amostras_custo_envoltoria_primeiro[i]<-rtriangle(1,36000, 40000,37000)+9800 + 209000
      
      tempo_segundo <- 3 + rtriangle(1,6,11,8)
      amostras_tempo_envoltoria_segundo[i]<-tempo_segundo  
      amostras_custo_envoltoria_segundo[i]<-rtriangle(1,36000, 40000,37000) + 209000
      
      tempo_terceiro <- 3 + rtriangle(1,6,11,8)
      amostras_tempo_envoltoria_terceiro[i]<-tempo_terceiro  
      amostras_custo_envoltoria_terceiro[i]<-rtriangle(1,36000, 40000,37000) + 209000
    }
    i<-i+1
  }
  
  amostras_tempo_envoltoria_primeiro <- amostras_tempo_envoltoria_primeiro+r$tempo_primeiro_piso
  amostras_tempo_envoltoria_segundo <- amostras_tempo_envoltoria_segundo+r$tempo_segundo_piso+r$tempo_primeiro_piso
  amostras_tempo_envoltoria_terceiro <-amostras_tempo_envoltoria_terceiro+r$tempo_terceiro_piso+r$tempo_segundo_piso+r$tempo_primeiro_piso
  amostras_custo_envoltoria<-amostras_custo_envoltoria_primeiro+amostras_custo_envoltoria_segundo+amostras_custo_envoltoria_terceiro
  hist(amostras_tempo_envoltoria_terceiro, main = "Distribuição de Tempo do Envoltório", xlab = "Tempo (semanas)")
  hist(amostras_custo_envoltoria, main = "Distribuição de Custo do Envoltório", xlab = "R$")
  
  #Grafico correlação
  par(mfrow=c(1,1))  # Defina o layout do gráfico
  plot(amostras_tempo_envoltoria_terceiro, amostras_custo_envoltoria, 
       xlab = "Prazo", ylab = "Custo", 
       main = "Correlação entre Prazo e Custo do Envoltorio", 
       pch = 19, col = "blue")
  
  tendencia <- lm(amostras_custo_envoltoria ~ amostras_tempo_envoltoria_terceiro)
  abline(tendencia, col = "red")
  
  legend("topright", legend = paste("Correlação do Envoltorio:", round(cor(amostras_tempo_envoltoria_terceiro, amostras_custo_envoltoria), 2)),
         col = "red", lty = 1, cex = 0.8)

  r<-list(amostras_tempo_envoltoria_primeiro=amostras_tempo_envoltoria_primeiro,amostras_tempo_envoltoria_segundo=amostras_tempo_envoltoria_segundo,
          amostras_tempo_envoltoria_terceiro=amostras_tempo_envoltoria_terceiro)  
}

q6<-function(){
  Ns <-3000
  i<-1
  amostras_tempo_acabamento_primeiro <-vector(mode="integer",Ns)
  amostras_tempo_acabamento_segundo <-vector(mode="integer",Ns)
  amostras_tempo_acabamento_terceiro <-vector(mode="integer",Ns)
  
  amostras_custo_acabamento_primeiro <-vector(mode="integer",Ns)
  amostras_custo_acabamento_segundo <-vector(mode="integer",Ns)
  amostras_custo_acabamento_terceiro <-vector(mode="integer",Ns)
  
  r<-q5()
  
  while(i<Ns+1){
    #Indica se houve um adicional ou não
    
    adicional_controle<-sample(c(0,1),1,replace = TRUE,prob=c(0.95,0.05))
    if(adicional_controle==0){
      tempo_primeiro <- rtriangle(1,8,13,10) + rtriangle(1,9,13,11)
      amostras_tempo_acabamento_primeiro[i]<-tempo_primeiro  
      amostras_custo_acabamento_primeiro[i]<-rtriangle(1,106000, 114000,11200)
      
      tempo_segundo <- rtriangle(1,8,13,10) + rtriangle(1,9,13,11)
      amostras_tempo_acabamento_segundo[i]<-tempo_segundo  
      amostras_custo_acabamento_segundo[i]<-rtriangle(1,106000, 114000,11200)
      
      tempo_terceiro <- rtriangle(1,8,13,10) + rtriangle(1,9,13,11)
      amostras_tempo_acabamento_terceiro[i]<-tempo_terceiro  
      amostras_custo_acabamento_terceiro[i]<-rtriangle(1,106000, 114000,11200)
    }
    else{
      tempo_primeiro <- rtriangle(1,8,13,10) + rtriangle(1,9,13,11)
      amostras_tempo_acabamento_primeiro[i]<-tempo_primeiro  
      amostras_custo_acabamento_primeiro[i]<-rtriangle(1,92000, 107000,95000)
      
      tempo_segundo <- rtriangle(1,8,13,10) + rtriangle(1,9,13,11)
      amostras_tempo_acabamento_segundo[i]<-tempo_segundo  
      amostras_custo_acabamento_segundo[i]<-rtriangle(1,92000, 107000,95000)
      
      tempo_terceiro <- rtriangle(1,8,13,10) + rtriangle(1,9,13,11)
      amostras_tempo_acabamento_terceiro[i]<-tempo_terceiro  
      amostras_custo_acabamento_terceiro[i]<-rtriangle(1,92000, 107000,95000)
    }
    i<-i+1
  }
  
  amostras_tempo_acabamento_primeiro <- amostras_tempo_acabamento_primeiro+r$amostras_tempo_envoltoria_primeiro
  amostras_tempo_acabamento_segundo <- amostras_tempo_acabamento_segundo+r$amostras_tempo_envoltoria_segundo
  amostras_tempo_acabamento_terceiro <-amostras_tempo_acabamento_terceiro+r$amostras_tempo_envoltoria_terceiro
  
  amostras_custo_acabamento_total<-amostras_custo_acabamento_primeiro+amostras_custo_acabamento_segundo+amostras_custo_acabamento_terceiro
  
 
  hist(amostras_tempo_acabamento_terceiro, main = "Distribuição de Tempo do Acabamento", xlab = "Tempo (semanas)")
  hist(amostras_custo_acabamento_total, main = "Distribuição de Custo do Acabamento", xlab = "R$")
  
  
  #Grafico correlação
  par(mfrow=c(1,1))  # Defina o layout do gráfico
  plot(amostras_tempo_acabamento_terceiro, amostras_custo_acabamento_total, 
       xlab = "Prazo", ylab = "Custo", 
       main = "Correlação entre Prazo e Custo do Acabamento", 
       pch = 19, col = "blue")
  
  tendencia <- lm(amostras_custo_acabamento_total ~ amostras_tempo_acabamento_terceiro)
  abline(tendencia, col = "red")
  
  legend("topright", legend = paste("Correlação do Acabamento:", round(cor(amostras_tempo_acabamento_terceiro, amostras_custo_acabamento_total), 2)),
         col = "red", lty = 1, cex = 0.8)
  
  r<-list(amostras_tempo_acabamento_terceiro=amostras_tempo_acabamento_terceiro,
          amostras_custo_acabamento_total=amostras_custo_acabamento_total)
}

q7<-function(){
  Ns <-3000
  i<-1
  amostras_tempo_finalizacao <-vector(mode="integer",Ns)
  amostras_custo_finalizacao <-vector(mode="integer",Ns)
  while(i<Ns+1){
    #Indica se houve um adicional ou não
    adicional_controle1<-sample(c(0,1),1,replace = TRUE,prob=c(0.4,0.6))
    adicional_controle2<-sample(c(0,1),1,replace = TRUE,prob=c(0.95,0.05))
    if(adicional_controle1==0){
      amostras_tempo_finalizacao[i]<-2+rtriangle(1,0.2,5,2)
      amostras_custo_finalizacao[i]<-4000
    }
    if(adicional_controle1==0 && adicional_controle2 == 1){
      amostras_tempo_finalizacao[i]<-2+rtriangle(1,0.2,5,2)+rtriangle(1,0.5,1.5,1)
      amostras_custo_finalizacao[i]<-4000
    }
    if(adicional_controle1==1){
      amostras_tempo_finalizacao[i]<-2
      amostras_custo_finalizacao[i]<-4000}
    i<-i+1
  }
  hist(amostras_tempo_finalizacao, main = "Distribuição de Tempo da Finalização", xlab = "Tempo (semanas)")
  hist(amostras_custo_finalizacao, main = "Distribuição de Custo da Finalização", xlab = "R$")
  
  #Grafico correlação
  par(mfrow=c(1,1))  # Defina o layout do gráfico
  plot(amostras_tempo_finalizacao, amostras_custo_finalizacao, 
       xlab = "Prazo", ylab = "Custo", 
       main = "Correlação entre Prazo e Custo do Finalização", 
       pch = 19, col = "blue")
  
  tendencia <- lm(amostras_custo_finalizacao ~ amostras_tempo_finalizacao)
  abline(tendencia, col = "red")
  
  legend("topright", legend = paste("Correlação da Finalização:", round(cor(amostras_tempo_finalizacao, amostras_custo_finalizacao), 2)),
         col = "red", lty = 1, cex = 0.8)
  
  r<-list(amostras_tempo_finalizacao=amostras_tempo_finalizacao,
          amostras_custo_finalizacao=amostras_custo_finalizacao)
} 

predio<-function(){
  projeto<-q1()
  terraplanagem<-q2()
  fundacao<-q3()
  acabamento<-q6()
  finalizacao<-q7()
  
  predio_tempo<-projeto$amostras_tempo_projeto+terraplanagem$amostras_tempo_terraplanagem+
    fundacao$amostras_tempo_fundacao+acabamento$amostras_tempo_acabamento_terceiro+finalizacao$amostras_tempo_finalizacao
  
  hist(predio_tempo, main = "Distribuição de Tempo do Predio", xlab = "Tempo (semanas)")
  
  predio_custo<-projeto$amostras_custo_projeto+terraplanagem$amostras_custo_terraplanagem+
    fundacao$amostras_custo_fundacao + acabamento$amostras_custo_acabamento_total + finalizacao$amostras_custo_finalizacao
  
  hist(predio_custo, main = "Distribuição de Custo do Predio", xlab = "R$")
  
}