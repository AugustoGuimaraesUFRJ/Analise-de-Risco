# Instale o pacote se ainda não estiver instalado
# install.packages("quantmod")

# Carregue a biblioteca
library(quantmod)

monte_carlo_simulation <- function(S, K, T, r, sigma, option_type, num_simulations) {
  set.seed(123)  # Defina uma semente para reproduzibilidade
  
  dt <- T / 252  # Assumindo 252 dias úteis em um ano
  paths <- matrix(0, nrow = num_simulations, ncol = 252 + 1)
  
  for (i in 1:num_simulations) {
    paths[i, 1] <- S
    
    for (t in 1:252) {
      dWt <- rnorm(1) * sqrt(dt)
      paths[i, t + 1] <- paths[i, t] * exp((r - 0.5 * sigma^2) * dt + sigma * dWt)
    }
  }
  
  option_payoffs <- numeric(num_simulations)
  
  for (i in 1:num_simulations) {
    if (option_type == "call") {
      option_payoffs[i] <- pmax(paths[i, 252 + 1] - K, 0)
    } else if (option_type == "put") {
      option_payoffs[i] <- pmax(K - paths[i, 252 + 1], 0)
    } else {
      stop("Invalid option type. Use 'call' or 'put'.")
    }
  }
  
  # Ajuste: Multiplique o payoff pelo fator de desconto exp(-r * T)
  option_price <- mean(option_payoffs) * exp(-r * T)
  
  return(option_price)
}

# É preciso ajustar o tipo de opção, o preço que o titular tem dirieto de opção de comprar e a data de vencimento
criar_tabela_opcoes <- function() {
  # Defina os parâmetros
  symbol <- "AAPL"
  
  # Obtenha os dados de opções do Yahoo Finance usando quantmod
  opcoes_yahoo <- getOptionChain(symbol,Exp="2023-12-08")
  
  # Verifique se há dados de opções
  if (is.null(opcoes_yahoo$calls) | is.null(opcoes_yahoo$puts)) {
    stop("Não foi possível obter os dados de opções para o símbolo fornecido.")
  }
  
  # Defina os parâmetros adicionais
  S <- getQuote(symbol)$Last  # Preço atual da ação da Apple
  K <- c(95,100,125,130,135,140,145,150,155)  # O preço ao qual o titular da opção tem o direito de comprar
  T <- as.numeric(difftime(as.Date("2023-12-08"), Sys.Date(), units = "days")) / 252  # Tempo até a expiração em anos
  r <- 0.02  # Taxa livre de risco
  sigma <- 0.2  # Volatilidade
  option_type <- "call"  # "call" para opção de compra, "put" para opção de venda
  num_simulations <- 10000  # Número de simulações Monte Carlo
  
  # Precificação usando Monte Carlo para cada preço de exercício (K)
  monte_carlo_results <- sapply(K, function(k) {
    monte_carlo_simulation(S, k, T, r, sigma, option_type, num_simulations)
  })
  
  # Crie a tabela
  tabela <- data.frame(
    Strike = K,
    "Yahoo Finance" = opcoes_yahoo$calls$Last[1:length(K)],
    "Monte Carlo" = monte_carlo_results,
    "Diferença" = monte_carlo_results - opcoes_yahoo$calls$Last[1:length(K)]
  )
  
  # Imprima a tabela
  print(tabela)
}

