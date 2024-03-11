library(Metrics)
library(tidyr)
library(forecast)
library(dplyr)
library("openxlsx")
# Indicar o caminho dos dados
dados_series <- read.xlsx("C:/Users/renan/Desktop/dados_series.xlsx")
# Definição do período da série
dados_treino1 <- dados_series %>%
  filter(ano_inicial >= 1800 & ano_inicial <= 2000)
# Defina o assunto para ser previsto
ts_medicina <- ts(subset(dados_treino1, assunto == "PSIQUIATRIA")$qtd_total_publicacoes, start = min(dados_treino1$ano_inicial), frequency = 1)
# plot(ts_medicina)

# Parâmetros para experimentação
ordens_p <- 0:5
ordens_d <- 0:5
ordens_q <- 0:5 

melhor_modelo <- NULL
melhor_aic <- Inf
melhores_parametros <- c(0, 0, 0)

# Loop para experimentar diferentes combinações de parâmetros
for (p in ordens_p) {
  for (d in ordens_d) {
    for (q in ordens_q) {
      ordem <- c(p, d, q)
      
      tryCatch({
        modelo <- arima(ts_medicina, order = ordem, method = "ML")
        aic <- AIC(modelo)
        
        # Atualizar se a combinação atual for melhor
        if (aic < melhor_aic) {
          melhor_aic <- aic
          melhores_parametros <- ordem
          melhor_modelo <- modelo
        }
      }, error = function(e) {})
    }
  }
}



# Crie previsões para o assunto especificado nos anos de 2001 a 2010
anos_previsao <- seq(2001, 2010)
melhor_modelo <- arima(ts_medicina, order = melhores_parametros)
# Caso a seleção automática dos parâmetros não funcione, fazer a seleção manual com análise gráfica
# acf(ts_medicina)
# pacf(ts_medicina)
# melhor_modelo <- arima(ts_medicina, order = c(parametros))
previsoes_medicina <- forecast(melhor_modelo, h = length(anos_previsao))
plot(previsoes_medicina, main = "ASSUNTO X")

dados_reais <- subset(dados_series, assunto == "ASSUNTO X" & ano_inicial >= 2001 & ano_inicial <= 2010)
# Caso o data frame não tenha 10 observações (que são os 10 anos do período), rodar o código abaixo que adiciona
# zeros nos anos que não possuem informações reais

# dados_medicina_1991_2000 <- dados_medicina_1991_2000 %>%
#    complete(ano_inicial = seq(min(2001), max(2010)), fill = list(qtd_total_publicacoes = 0, assunto = "MATEMATICA"))

# Crie um DataFrame com os resultados
resultados_previsao <- data.frame(
  ano_inicial = anos_previsao,
  assunto = rep('ASSUNTO X', length(anos_previsao)),
  valor_real = dados_reais$qtd_total_publicacoes,
  valor_previsto = previsoes_medicina$mean
)

# Métricas
mae_valor <- mae(resultados_previsao$valor_real, resultados_previsao$valor_previsto)
mape_valor <- mape(resultados_previsao$valor_real, resultados_previsao$valor_previsto)
print(paste("MAE:", mae_valor))
print(paste("MAPE:", mape_valor))