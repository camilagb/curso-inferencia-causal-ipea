# =============================================================================
# CONTROLE SINTÉTICO — EFEITO DA TARIFA DAS BLUSINHAS NO EMPREGO TÊXTIL
# =============================================================================
# Estratégia: comparar subsetores têxteis de ALTA exposição às remessas de
# baixo valor (confecção popular) contra subsetores de BAIXA exposição
# (têxteis técnicos, fios, cama/mesa/banho) usando Synthetic Control Method.
#
# Dados necessários:
#   - CAGED mensal por CNAE (emprego líquido ou estoque via RAIS)
#   - Tabela de exposição por CNAE (construída via Comex Stat / NCM)
#
# Pacotes principais: gsynth (Generalized Synthetic Control)
#                     Synth (método original de Abadie et al.)
#                     tidyverse, ggplot2
# =============================================================================


# -----------------------------------------------------------------------------
# 0. INSTALAÇÃO E CARREGAMENTO DE PACOTES
# -----------------------------------------------------------------------------

packages <- c("tidyverse", "gsynth", "Synth", "lubridate", "scales", "ggrepel")

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
invisible(lapply(packages, install_if_missing))
lapply(packages, library, character.only = TRUE)


# -----------------------------------------------------------------------------
# 1. PARÂMETROS GLOBAIS
# -----------------------------------------------------------------------------

# Data de implementação da tarifa das blusinhas
# Ajuste conforme a data oficial de vigência
DATA_TARIFA <- as.Date("2024-08-01")

# Janela de análise: meses antes e depois
MESES_PRE  <- 24   # meses pré-tarifa para construir o sintético
MESES_POST <- 12   # meses pós-tarifa para estimar o efeito

# CNAE do subsetor tratado principal (confecção de vestuário popular)
# 1412-6 Confecção de peças do vestuário, exceto roupas íntimas e as confeccionadas
#         sob medida — ajuste conforme granularidade disponível na RAIS/CAGED
CNAE_TRATADO <- "14126"

# CNAEs do pool de controle (subsetores têxteis menos expostos às blusinhas)
# Ajuste de acordo com os CNAEs disponíveis nos seus dados
CNAE_CONTROLES <- c(
  "13111",  # Preparação e fiação de fibras de algodão
  "13129",  # Preparação e fiação de fibras têxteis naturais (exceto algodão)
  "13200",  # Tecelagem (exceto malha)
  "13308",  # Fabricação de tecidos de malha — menos exposto ao vestuário popular
  "13405",  # Acabamentos em fios, tecidos e artefatos têxteis
  "13511",  # Fabricação de artefatos têxteis para uso doméstico — cama/mesa/banho
  "13529"   # Fabricação de artefatos de tapeçaria e outros têxteis
)

# -----------------------------------------------------------------------------
# 2. LEITURA E PREPARAÇÃO DOS DADOS
# -----------------------------------------------------------------------------
# Aqui você carrega seus dados reais do CAGED/RAIS.
# O script abaixo usa dados simulados para demonstrar a estrutura esperada.
# Substitua a seção 2.1 pela leitura dos seus arquivos reais.

# --- 2.1 SIMULAÇÃO DE DADOS (substituir pelos dados reais) ------------------

set.seed(42)

todos_cnaes <- c(CNAE_TRATADO, CNAE_CONTROLES)
n_cnaes     <- length(todos_cnaes)

# Sequência de datas mensais
datas <- seq(
  DATA_TARIFA - months(MESES_PRE),
  DATA_TARIFA + months(MESES_POST) - days(1),
  by = "month"
)

# Nomes legíveis para os subsetores (para os gráficos)
nomes_cnae <- c(
  "14126" = "Confecção popular (tratado)",
  "13111" = "Fiação de algodão",
  "13129" = "Fiação de fibras naturais",
  "13200" = "Tecelagem",
  "13308" = "Malha técnica",
  "13405" = "Acabamentos têxteis",
  "13511" = "Cama/mesa/banho",
  "13529" = "Tapeçaria e outros"
)

# Índice de exposição às remessas de baixo valor (0 a 1)
# Em dados reais: calcule como share das importações via e-commerce
# no consumo aparente de cada subsetor, usando Comex Stat pré-tarifa
exposicao <- c(
  "14126" = 0.82,   # alta — compete diretamente com Shein/Shopee
  "13111" = 0.08,
  "13129" = 0.06,
  "13200" = 0.05,
  "13308" = 0.15,   # alguma exposição (malharia popular)
  "13405" = 0.04,
  "13511" = 0.12,
  "13529" = 0.03
)

# Simular índice de emprego (base 100 em t0)
simular_emprego <- function(cnae, datas, data_tarifa) {
  n     <- length(datas)
  t0    <- which(datas == data_tarifa)
  exp_i <- exposicao[cnae]

  # Tendência pré-existente: mais expostos já declinavam antes
  tendencia_pre  <- -0.002 * exp_i
  # Efeito da tarifa: recuperação proporcional à exposição (efeito positivo esperado)
  efeito_tarifa  <-  0.008 * exp_i
  # Ruído idiossincrático
  ruido          <- rnorm(n, 0, 0.004)

  indice <- numeric(n)
  indice[1] <- 100

  for (i in 2:n) {
    if (i < t0) {
      indice[i] <- indice[i-1] * (1 + tendencia_pre) + ruido[i]
    } else {
      indice[i] <- indice[i-1] * (1 + efeito_tarifa) + ruido[i]
    }
  }

  # Normalizar para base 100 em t0
  indice / indice[t0] * 100
}

dados_brutos <- map_dfr(todos_cnaes, function(cnae) {
  tibble(
    cnae       = cnae,
    nome_cnae  = nomes_cnae[cnae],
    data       = datas,
    mes_rel    = interval(DATA_TARIFA, datas) %/% months(1),
    emprego    = simular_emprego(cnae, datas, DATA_TARIFA),
    exposicao  = exposicao[cnae],
    tratado    = as.integer(cnae == CNAE_TRATADO)
  )
})


# --- 2.2 LEITURA DE DADOS REAIS (descomentar e adaptar) ----------------------

# library(arrow)  # para arquivos parquet do CAGED
#
# # Exemplo: CAGED agregado por CNAE e mês
# dados_caged <- read_csv("dados/caged_textil_mensal.csv") |>
#   filter(cnae_2dig %in% c("13", "14")) |>
#   mutate(
#     data    = ym(paste(ano, mes, sep = "-")),
#     mes_rel = interval(DATA_TARIFA, data) %/% months(1)
#   )
#
# # Calcular índice de emprego (base 100 em t0)
# dados_brutos <- dados_caged |>
#   group_by(cnae) |>
#   mutate(
#     emprego_t0 = emprego[data == DATA_TARIFA],
#     emprego    = emprego / emprego_t0 * 100
#   ) |>
#   ungroup()


# -----------------------------------------------------------------------------
# 3. ANÁLISE DESCRITIVA — TRAJETÓRIAS PRÉ-TARIFA
# -----------------------------------------------------------------------------

p_descritivo <- dados_brutos |>
  filter(mes_rel >= -MESES_PRE, mes_rel <= MESES_POST) |>
  mutate(
    grupo = if_else(tratado == 1, "Confecção popular (tratado)", "Controles (pool)")
  ) |>
  ggplot(aes(x = data, y = emprego,
             group = cnae,
             color = grupo,
             alpha = grupo)) +
  geom_line(linewidth = 0.8) +
  geom_vline(xintercept = DATA_TARIFA, linetype = "dashed",
             color = "#BA7517", linewidth = 0.8) +
  annotate("text", x = DATA_TARIFA + days(10), y = 103,
           label = "Tarifa das\nblusinhas", hjust = 0,
           size = 3, color = "#BA7517") +
  scale_color_manual(values = c(
    "Confecção popular (tratado)" = "#185FA5",
    "Controles (pool)"            = "#B4B2A9"
  )) +
  scale_alpha_manual(values = c(
    "Confecção popular (tratado)" = 1,
    "Controles (pool)"            = 0.6
  )) +
  scale_x_date(date_labels = "%b/%Y", date_breaks = "4 months") +
  labs(
    title    = "Trajetória do emprego têxtil — tratado vs. pool de controles",
    subtitle = "Índice de emprego, base 100 = mês de implementação da tarifa",
    x        = NULL,
    y        = "Índice de emprego (base 100)",
    color    = NULL,
    alpha    = NULL,
    caption  = "Fonte: CAGED/MTE. Elaboração própria."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    axis.text.x      = element_text(angle = 30, hjust = 1),
    panel.grid.minor = element_blank()
  )

print(p_descritivo)
ggsave("output/01_trajetorias_descritivo.png", p_descritivo,
       width = 10, height = 6, dpi = 150)


# -----------------------------------------------------------------------------
# 4. CONTROLE SINTÉTICO — MÉTODO GSYNTH
# -----------------------------------------------------------------------------
# gsynth (Xu 2017) é a versão generalizada que lida com múltiplas unidades
# tratadas e é mais robusta que o Synth original para painel de subsetores.

# --- 4.1 Preparar painel no formato exigido pelo gsynth ----------------------

painel <- dados_brutos |>
  select(cnae, data, mes_rel, emprego, tratado, exposicao) |>
  mutate(
    id_num   = as.integer(factor(cnae)),   # ID numérico por subsetor
    tempo    = mes_rel                     # tempo relativo à tarifa
  )

# Verificar balanceamento do painel
painel |>
  count(cnae, nome = "n_obs") |>
  print()

# --- 4.2 Rodar o modelo gsynth -----------------------------------------------

# O gsynth estima um modelo de fatores interativos (IFE):
#   Y_it = delta_it * D_it + lambda_i' * f_t + epsilon_it
#
# onde lambda_i são cargas de fatores latentes e f_t são fatores comuns.
# Isso é mais flexível que o Synth clássico e lida melhor com
# tendências pré-existentes diferentes entre subsetores.

modelo_gsynth <- gsynth(
  emprego ~ tratado,          # fórmula: resultado ~ indicador de tratamento
  data      = painel,
  index     = c("id_num", "tempo"),   # identificadores de unidade e tempo
  force     = "two-way",              # efeitos fixos de unidade e tempo
  CV        = TRUE,                   # cross-validation para escolher nº de fatores
  r         = c(0, 5),               # buscar entre 0 e 5 fatores latentes
  se        = TRUE,                   # calcular erros-padrão por bootstrap
  nboots    = 200,                    # número de replicações bootstrap
  parallel  = FALSE,                  # TRUE se tiver múltiplos núcleos disponíveis
  min.T0    = MESES_PRE               # mínimo de períodos pré-tratamento
)

summary(modelo_gsynth)


# --- 4.3 Extrair resultados --------------------------------------------------

# ATT (Average Treatment Effect on the Treated) por período
att_periodo <- as_tibble(modelo_gsynth$att.avg) |>
  mutate(
    mes_rel    = as.integer(rownames(modelo_gsynth$att.avg)),
    att        = modelo_gsynth$att.avg[, 1],
    ci_lower   = att - 1.96 * modelo_gsynth$att.avg.se,
    ci_upper   = att + 1.96 * modelo_gsynth$att.avg.se,
    pos_tarifa = mes_rel >= 0
  )

# ATT médio pós-tarifa
att_medio <- modelo_gsynth$att.avg.se |> mean()
cat("\nATT médio pós-tarifa:", round(mean(att_periodo$att[att_periodo$pos_tarifa]), 3), "\n")


# --- 4.4 Gráfico do gap (observado − sintético) ------------------------------

p_gap <- att_periodo |>
  ggplot(aes(x = mes_rel, y = att)) +
  annotate("rect",
           xmin = -0.5, xmax = max(att_periodo$mes_rel) + 0.5,
           ymin = -Inf, ymax = Inf,
           fill = "#E6F1FB", alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#888780") +
  geom_vline(xintercept = -0.5, linetype = "dashed",
             color = "#BA7517", linewidth = 0.8) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper),
              fill = "#185FA5", alpha = 0.15) +
  geom_line(color = "#185FA5", linewidth = 1.2) +
  geom_point(color = "#185FA5", size = 2) +
  annotate("text", x = 0.2, y = max(att_periodo$ci_upper, na.rm = TRUE) * 0.9,
           label = "Pós-tarifa", hjust = 0, size = 3.5, color = "#185FA5") +
  annotate("text", x = -MESES_PRE + 0.5,
           y = max(att_periodo$ci_upper, na.rm = TRUE) * 0.9,
           label = "Pré-tarifa\n(ajuste do sintético)", hjust = 0,
           size = 3.5, color = "#888780") +
  scale_x_continuous(breaks = seq(-MESES_PRE, MESES_POST, by = 3)) +
  labs(
    title    = "Controle Sintético — Efeito da Tarifa das Blusinhas",
    subtitle = "Gap entre emprego observado e sintético (pontos do índice, base 100)\nIC 95% por bootstrap",
    x        = "Meses em relação à implementação da tarifa",
    y        = "Gap: observado − sintético",
    caption  = "Fonte: CAGED/MTE. Método: gsynth (Xu, 2017). Elaboração própria."
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

print(p_gap)
ggsave("output/02_gap_sintetico.png", p_gap,
       width = 10, height = 6, dpi = 150)


# -----------------------------------------------------------------------------
# 5. TESTE DE PLACEBO POR PERMUTAÇÃO
# -----------------------------------------------------------------------------
# Aplica o mesmo método para cada subsetor de controle como se fosse o tratado.
# O efeito real deve ser maior que a distribuição dos placebos.

placebo_resultados <- map_dfr(CNAE_CONTROLES, function(cnae_placebo) {

  painel_placebo <- painel |>
    mutate(tratado_pl = as.integer(cnae == cnae_placebo))

  tryCatch({
    modelo_pl <- gsynth(
      emprego ~ tratado_pl,
      data    = painel_placebo,
      index   = c("id_num", "tempo"),
      force   = "two-way",
      CV      = FALSE,
      r       = modelo_gsynth$r.cv,   # usar mesmo nº de fatores do modelo principal
      se      = FALSE,
      min.T0  = MESES_PRE
    )

    tibble(
      cnae    = cnae_placebo,
      mes_rel = as.integer(rownames(modelo_pl$att.avg)),
      att     = modelo_pl$att.avg[, 1],
      tipo    = "placebo"
    )
  }, error = function(e) {
    message("Erro no placebo para CNAE ", cnae_placebo, ": ", e$message)
    NULL
  })
})

# Combinar tratado e placebos para o gráfico
dados_placebo_plot <- bind_rows(
  att_periodo |>
    select(mes_rel, att) |>
    mutate(cnae = CNAE_TRATADO, tipo = "tratado"),
  placebo_resultados
)

p_placebo <- dados_placebo_plot |>
  ggplot(aes(x = mes_rel, y = att,
             group = cnae,
             color = tipo,
             linewidth = tipo,
             alpha = tipo)) +
  annotate("rect",
           xmin = -0.5, xmax = MESES_POST + 0.5,
           ymin = -Inf, ymax = Inf,
           fill = "#E6F1FB", alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#888780") +
  geom_vline(xintercept = -0.5, linetype = "dashed",
             color = "#BA7517", linewidth = 0.8) +
  geom_line() +
  scale_color_manual(
    values = c("tratado" = "#185FA5", "placebo" = "#D3D1C7"),
    labels = c("tratado" = "Confecção popular (tratado)",
               "placebo" = "Placebos (controles)")
  ) +
  scale_linewidth_manual(values = c("tratado" = 1.5, "placebo" = 0.7)) +
  scale_alpha_manual(values   = c("tratado" = 1,   "placebo" = 0.7)) +
  scale_x_continuous(breaks = seq(-MESES_PRE, MESES_POST, by = 3)) +
  labs(
    title    = "Teste de Placebo por Permutação",
    subtitle = "Gap do subsetor tratado vs. distribuição dos gaps dos placebos",
    x        = "Meses em relação à implementação da tarifa",
    y        = "Gap: observado − sintético",
    color    = NULL,
    caption  = "Fonte: CAGED/MTE. Elaboração própria."
  ) +
  guides(linewidth = "none", alpha = "none") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

print(p_placebo)
ggsave("output/03_teste_placebo.png", p_placebo,
       width = 10, height = 6, dpi = 150)


# --- P-valor do placebo -------------------------------------------------------

# Para cada período pós-tarifa, qual fração dos placebos tem gap >= tratado?
p_valor_placebo <- att_periodo |>
  filter(pos_tarifa) |>
  left_join(
    placebo_resultados |>
      group_by(mes_rel) |>
      summarise(n_placebos_maiores = sum(abs(att) >= abs(att_periodo$att[
        att_periodo$mes_rel == unique(mes_rel)][1]),
        na.rm = TRUE),
        n_total = n(),
        .groups = "drop"
      ),
    by = "mes_rel"
  ) |>
  mutate(p_valor = (n_placebos_maiores + 1) / (n_total + 1))

cat("\nP-valores do teste de placebo (pós-tarifa):\n")
print(p_valor_placebo |> select(mes_rel, att, p_valor))


# -----------------------------------------------------------------------------
# 6. ANÁLISE DE HETEROGENEIDADE — EFEITO POR MUNICÍPIO
# -----------------------------------------------------------------------------
# Se você tiver os dados de emprego por município × CNAE (RAIS),
# pode estimar se o efeito foi maior em municípios com maior especialização têxtil.

# --- 6.1 Calcular Quociente Locacional (QL) pré-tarifa -----------------------

# ql_municipio <- rais_pre_tarifa |>
#   group_by(municipio) |>
#   summarise(
#     emp_textil = sum(emprego[cnae_2dig %in% c("13","14")]),
#     emp_total  = sum(emprego),
#     .groups = "drop"
#   ) |>
#   mutate(
#     share_local   = emp_textil / emp_total,
#     share_nacional = sum(emp_textil) / sum(emp_total),
#     ql             = share_local / share_nacional
#   )

# Municípios com QL > 1.5 são candidatos a análise de caso
# (Americana SP, Blumenau SC, Ibitinga SP, Colatina ES, etc.)

# --- 6.2 DDD: Post × Exposição_setor × QL_municipio -------------------------

# modelo_ddd <- feols(
#   delta_emprego ~ i(mes_rel, exposicao, ref = -1) :ql_municipio
#                 | cnae^municipio + municipio^mes_rel + cnae^mes_rel,
#   data    = painel_municipal,
#   cluster = ~microrregiao   # clustered SE no nível de microrregião
# )
# iplot(modelo_ddd)  # event study do DDD


# -----------------------------------------------------------------------------
# 7. ROBUSTEZ — VARIAR JANELA PRÉ-TARIFA
# -----------------------------------------------------------------------------
# Testar se os resultados são sensíveis à escolha de MESES_PRE

janelas <- c(12, 18, 24)

robustez_janela <- map_dfr(janelas, function(janela) {

  painel_rob <- painel |> filter(mes_rel >= -janela)

  tryCatch({
    mod <- gsynth(
      emprego ~ tratado,
      data   = painel_rob,
      index  = c("id_num", "tempo"),
      force  = "two-way",
      CV     = FALSE,
      r      = modelo_gsynth$r.cv,
      se     = TRUE,
      nboots = 100,
      min.T0 = janela
    )

    tibble(
      janela  = janela,
      mes_rel = as.integer(rownames(mod$att.avg)),
      att     = mod$att.avg[, 1],
      se      = mod$att.avg.se
    )
  }, error = function(e) NULL)
})

p_robustez <- robustez_janela |>
  filter(mes_rel >= 0) |>
  mutate(janela = paste0(janela, " meses pré")) |>
  ggplot(aes(x = mes_rel, y = att, color = janela, fill = janela)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#888780") +
  geom_ribbon(aes(ymin = att - 1.96*se, ymax = att + 1.96*se), alpha = 0.12, color = NA) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("#185FA5", "#0F6E56", "#854F0B")) +
  scale_fill_manual(values  = c("#185FA5", "#0F6E56", "#854F0B")) +
  labs(
    title    = "Robustez — Variando a Janela Pré-Tarifa",
    subtitle = "ATT pós-tarifa sob diferentes tamanhos de janela de estimação",
    x        = "Meses pós-tarifa",
    y        = "ATT (gap observado − sintético)",
    color    = "Janela pré-tarifa",
    fill     = "Janela pré-tarifa",
    caption  = "Fonte: CAGED/MTE. Elaboração própria."
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

print(p_robustez)
ggsave("output/04_robustez_janela.png", p_robustez,
       width = 10, height = 6, dpi = 150)


# -----------------------------------------------------------------------------
# 8. TABELA DE RESULTADOS
# -----------------------------------------------------------------------------

tabela_att <- att_periodo |>
  filter(pos_tarifa) |>
  summarise(
    att_medio     = mean(att),
    att_acumulado = sum(att),
    ci_lower_med  = mean(ci_lower),
    ci_upper_med  = mean(ci_upper)
  )

cat("\n========================================================\n")
cat("RESULTADOS — EFEITO DA TARIFA DAS BLUSINHAS\n")
cat("========================================================\n")
cat("ATT médio pós-tarifa:     ", round(tabela_att$att_medio, 3), " pontos do índice\n")
cat("IC 95%:                   [",
    round(tabela_att$ci_lower_med, 3), ",",
    round(tabela_att$ci_upper_med, 3), "]\n")
cat("ATT acumulado:            ", round(tabela_att$att_acumulado, 3), "\n")
cat("Número de fatores (CV):  ", modelo_gsynth$r.cv, "\n")
cat("========================================================\n")


# -----------------------------------------------------------------------------
# 9. PRÓXIMOS PASSOS SUGERIDOS
# -----------------------------------------------------------------------------
# [ ] Substituir dados simulados pelos dados reais do CAGED/RAIS
# [ ] Construir o índice de exposição via Comex Stat (NCM × subsetor)
# [ ] Adicionar covariáveis pré-tratamento no gsynth (salário médio, tamanho firma)
# [ ] Estimar o DDD municipal (seção 6.2) para heterogeneidade regional
# [ ] Testar com pacote Synth original para comparar com gsynth
# [ ] Verificar se houve antecipação (anúncio pré-implementação) nos meses -3 a -1
# =============================================================================
