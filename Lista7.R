# Grupo 4
# Augusto Guimarães
# Fábio Bocampagni
# Isaque Araujo
# Lucas Hélio
# 1 - Risco e Correlação:

# Um investidor deseja aplicar $10 milhões em investimentos imobiliários. 
# Ele foi oferecido a oportunidade de dividir o montante total em um conjunto de 10 aplicações distintas, 
# porém com as mesmas características de risco. O retorno de cada um dos ativos é distribuído normalmente, 
# com uma média de $1 milhão e um desvio padrão de $1.3 milhão. 
# O investidor acredita que individualmente os investimentos não são atrativos, 
# mas ao montar uma carteira com 10 investimentos idênticos, suas chances de perda diminuirão consideravelmente, 
# já que uma perda em um ativo será compensada por um ganho em outro. Será isso verdade?
  
# Calcule a probabilidade de perda no investimento da carteira quando o coeficiente de correlação entre os ativos for 
# igual a: (0, 0.25, 0.50, 0.75, 0.90), respectivamente. Este caso modela a situação em que os investimentos são 
# completamente não correlacionados, desde ρ = 0 até quando são fortemente correlacionados ρ = 0.9 

# Uma carteira diversificada, isto é, composta por títulos
# diferentes e de emissão de diferentes instituições, reduz
# o impacto do retorno de cada título no resultado global
# da carteira.  A combinação das variações individuais e das 
#relações entre os ativos desempenha um papel fundamental na 
# redução da volatilidade da carteira e, consequentemente, no 
# gerenciamento de riscos.


# Assumindo que as variaveis (X1, X2, ... , X10) são independentes:
  
# Var(X1 + X2 + ... + X10) = Var(X1) + Var(X2) + Var(X3) + ... + Var(X10) = 10 * Var (X)
# Var(10 X) = 10^2 * Var (X)
# Var(X1 + X2 + ... + Xc) é menor

# Podemos ver que a variância do investimento total
# em uma conjunto de ações é menor do que a variância do investimento em uma única ação. Assim,
# podemos afirmar que comprar um fundo de ações é menos arriscado do que comprar uma única ação,
# pois o investimento é menos suscetível a flutuações imprevisíveis do mercado.

q1<-function(){
  media_retorno_esperado <- 1
  # https://www.maxwell.vrac.puc-rio.br/8675/8675_4.PDF
  # σ = ∑i=1^10 ∑j=1^10 wi * wj * σi * σj * ρij
  desvio_padrao <- sqrt((1/100)*(1.3)*(1.3)*(10+90*c(0, 0.25,0.50,0.75,0.90)))
  #Para calcular a probabilidade de que o retorno do portfólio seja menor que zero (ou seja, uma perda), 
  # precisa se padronizar os valores usando a fórmula Z:
  # Z = (X - μ) / σ
  Z = (0 - 1) / desvio_padrao 
  p<-pnorm(Z)
  cat("Correlação 0.00 -> probabilidade: ",round(p[1],3),"\n")
  cat("Correlação 0.25 -> probabilidade: ",round(p[2],3),"\n")
  cat("Correlação 0.50 -> probabilidade: ",round(p[3],3),"\n")
  cat("Correlação 0.75 -> probabilidade: ",round(p[4],3),"\n")
  cat("Correlação 0.90 -> probabilidade: ",round(p[5],3),"\n")
}

q2<-function(){
  #install.packages("quantmod")
  #library(quantmod)
  # Símbolos das ações que você deseja importar
  symbols <- c("PETR4.SA", "VALE3.SA", "BBAS3.SA")
  
  # Defina a data inicial e final (últimos 12 meses)
  end_date <- Sys.Date()  # Data final (hoje)
  start_date <- end_date - 365  # Data inicial (1 ano atrás)
  
  # Use a função getSymbols para importar os dados
  getSymbols(symbols, src = "yahoo", from = start_date, to = end_date)
  
  valores_petrobras<-coredata(PETR4.SA$PETR4.SA.Adjusted)
  num<-length(valores_petrobras)
  rt_petrobras<-c()
  i<-1
  while (i<num){
    rt_petrobras[i]<-log(valores_petrobras[i+1])-log(valores_petrobras[i])
    i<-i+1}
  micro_petrobras<-mean(rt_petrobras)
  variancia_petrobras<-var(rt_petrobras)
  cat("Media dos retornos da Petrobras: ",micro_petrobras,"\n")
  cat("Variancia dos retornos da Petrobras: ",variancia_petrobras,"\n")
  
  valores_vale<-coredata(VALE3.SA$VALE3.SA.Adjusted)
  num<-length(valores_vale)
  rt_vale<-c()
  i<-1
  while (i<num){
    rt_vale[i]<-log(valores_vale[i+1])-log(valores_vale[i])
    i<-i+1}
  micro_vale<-mean(rt_vale)
  variancia_vale<-var(rt_vale)
  cat("Media dos retornos da Vale: ",micro_vale,"\n")
  cat("Variancia dos retornos da Vale: ",variancia_vale,"\n")
  
  valores_bbas<-coredata(BBAS3.SA$BBAS3.SA.Adjusted)
  num<-length(valores_bbas)
  rt_bbas<-c()
  i<-1
  while (i<num){
    rt_bbas[i]<-log(valores_bbas[i+1])-log(valores_bbas[i])
    i<-i+1}
  micro_bbas<-mean(rt_bbas)
  variancia_bbas<-var(rt_bbas)
  cat("Media dos retornos da BBAS: ",micro_bbas,"\n")
  cat("Variancia dos retornos da BBAS: ",variancia_bbas,"\n")
  
  correlation_A <- cor(valores_petrobras,valores_vale)
  correlation_B <- cor(valores_petrobras,valores_bbas)
  correlation_C <- cor(valores_vale,valores_bbas)
  
  cm<-diag(1,3,3)
  dimnames(cm) <- list(c("Petrobras","VALE","BB"), c("Petrobras","VALE","BB"))
  cm[1,2]<-correlation_A
  cm[2,1]<-correlation_A

  cm[1,3]<-correlation_B
  cm[3,1]<-correlation_B
  
  cm[2,3]<-correlation_C
  cm[3,2]<-correlation_C
  cat("\n")
  print("Correlações ")
  print(cm)
}