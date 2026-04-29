# instalando o pacote
install.packages('remotes')
remotes::install_github('rfsaldanha/microdatasus')

# carregando os pacotes
library(microdatasus) 
library(dplyr) 
library(ggplot2)

# lendo os dados
data <- fetch_datasus(year_start = 2023, month_start = 1, year_end = 2023, month_end = 12,  information_system = "SINASC", uf = "GO")

# nrow(data) -> 91822

### Parte 1

## Questão 1

# Selecinando as colunas necessárias

# PESO: Peso ao nascer, em gramas
# IDADEMAE: Idade da mãe em anos
# ESCMAE: Escolaridade da mãe (anos de estudo concluídos) sendo 1 nenhuma; 2, 1 a 3 anos; 3, 4 a 7 anos; 4, 8 a 11 anos; 5, 12 e mais; 9, ignorado
# ref: https://rfsaldanha.github.io/sis/assets/sinasc/Estrutura_SINASC_para_CD.pdf
s_data <- data |>
  select(PESO, IDADEMAE, ESCMAE) |>
  mutate(
    PESO = as.numeric(PESO),
    IDADEMAE = as.numeric(IDADEMAE),
  ) |>
  filter(!is.na(PESO)) |>
  mutate(
    ESCMAE = factor(
      ESCMAE,
      levels = c('1', '2', '3', '4', '5', '9'),
      labels = c('Nenhuma', '1-3', '4-7', '8-11','12+', NA)  
    )
  )
glimpse(s_data)

## Questão 2

# A cobertura do SINASC não é de 100%, embora seja superior a 90% na maioria das UFs 

media <- mean(s_data$PESO, na.rm = TRUE)
desvio <- sd(s_data$PESO, na.rm = TRUE)
n <- as.numeric(nrow(s_data))
ep <-  desvio/sqrt(n)

media
desvio
ep

# Desvio padrão é a variabilidade da amostra, enquanto o erro padrão é a precisão da estimativa da média e depende do tamnho da amostra.
# Nesse caso, temos um n grande, resultando em um erro padrão de menos de 2 gramas

## Questão 3

# Considerando o valor de t para um intervalo de confiança de 95% igual a 1.96, temos

ic <- c(media - 1.96*ep, media + 1.96*ep)
ic

# O intervalo de confiança de 95% significa que se a amostragem for repetida infinitas vezes,
# 95% dos intervalos construídos conterão o verdadeiro valor da média

## Questão 4

histograma <- s_data |>
  ggplot() +
  aes(x = PESO) +
  geom_histogram(binwidth = 100)

histograma
# Distribuição em forma de sino com a cauda inferior maior

### Parte 2

## Questão 5

modelo_linear <- lm(PESO ~ IDADEMAE, data = s_data)
summary(modelo_linear)

# intercepto = 3066.43 -> Valor da variável dependente (PESO) quando a variável independente (IDADEMAE) é 0
# beta (coeficiente da idade da mãe) = 1.57 -> inclinação, quanto o incremento davariável independente
# impacta na variável dependente, ou seja, o aumento médio esperado no peso do bebê para cada ano adicional a idade da mãe
# R2 = 0.0003576 -> R2 perto de 0 mostra que a variável independente não explica quase nada da variável dependente

## Questão 6

## Estimando um modelo de regressão linear com uma subamostra de 100

amostra100 <- s_data |> slice_sample(n=100)
modelo_linear_100 <- lm(PESO ~IDADEMAE, data = amostra100)

# intercepto = 2730.693
# beta = 16.051
# R2 = 0.03894

## Repetindo o experimento

betas100 = c()
for (i in 1:1000){
  amostra <- s_data |> slice_sample(n=100)
  modelo <- lm(PESO ~ IDADEMAE, data = amostra)
  betas100 <- c(betas100, modelo$coefficients[[2]])
}

df100 <- data.frame(betas100)
df100 |> ggplot() + aes(x=betas100) + geom_histogram(binwidth = 2)

summary(df100)

# Para amostras de 100, 0 beta varia de -28 a +30, num distribuição mais ou menos normal de média 1.87

## Estimando um modelo de regressão linear com uma subamostra de 1000

betas1000 = c()
for (i in 1:1000){
  amostra <- s_data |> slice_sample(n=1000)
  modelo <- lm(PESO ~ IDADEMAE, data = amostra)
  betas1000 <- c(betas1000, modelo$coefficients[[2]])
}

df1000 <- data.frame(betas1000)
df1000 |> ggplot() + aes(x=betas1000) + geom_histogram(binwidth = 2)

summary(df1000)

# Para amostras de 1000, o beta varia de -8 a +11, num distribuição mais ou menos normal de média 1.4

## Estimando um modelo de regressão linear com uma subamostra de 10000

betas10k = c()
for (i in 1:1000){
  amostra <- s_data |> slice_sample(n=10000)
  modelo <- lm(PESO ~ IDADEMAE, data = amostra)
  betas10k <- c(betas10k, modelo$coefficients[[2]])
}

df10k <- data.frame(betas10k)
df10k |> ggplot() + aes(x=betas10k) + geom_histogram(binwidth = 0.5)

summary(df10k)

# Para amostras de 1000, o beta varia de -1.2 a +4, num distribuição mais ou menos normal de média 1.59

betas <- bind_rows(
  data.frame(beta = betas100, group = 'n_100'),
  data.frame(beta = betas1000, group = 'n_1000'),
  data.frame(beta = betas10k, group = 'n_10000'),
)

betas |> 
  ggplot() +
  aes(x = beta, fill = group) +
  geom_density(alpha = 0.3)

# a) Os valores estimados para beta são diferentes entre cada amostra, visto que a estimação depende da amsotra selecionada
# b) A medida que o tamanho da amostra aumenta, a dispersão dos valores de beta diminui
# c) Esse comportamento demonstra exatamente a relação do erro padrão com o tamanho amostral, quanto maior a amostra, menor o erro
# d) A confiabilidade de coeficiente estimado em regressão está fortemente ligado à amostra e ao seu tamanho

## Questão 7

modelo_multiplo <- lm(PESO ~ IDADEMAE + ESCMAE, data = s_data)

summary(modelo_multiplo)

# intercepto = 2808.44
# beta idade = 1.3673
# betas escolaridade
#   1-3 = 192
#   4-7 = 199
#   8-11 = 258
#   12+ = 245
#   NA = 135

# Em relação ao modelo anterior, os betas relacionados à escolaridade são bem maiores, mostrando
# que o impacto de mais anos de escolaridade é maior no peso do que a idade da mãe

## Questão 8

# Provavelmente, mães com mais escolaridade possuem renda maior e maior acesso a alimentos de boa qualidade

## Questão 9

# Muito difícil interpretar os coeficientes como causais, existem muitas variávies omitidas

## Questão 10

# Contrafactual para a influência da idade da mãe no peso do bebê
#  -> qual seria o peso do mesmo bebê para a mãe com idades diferentes? 