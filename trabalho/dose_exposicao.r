## https://strategicprojects.github.io/comexr/

library(tidyverse)
library(readxl)
library(readr)
# library(comexr)

## NCMs de produtos de bens de consumo final

ncm_cgce <- read_excel("trabalho/dados/PRODLIST-Industria 2010 x CGCE-IBGE x CONTAS x BEC.xls", sheet="PRODLIST 2010", skip=5)

nrow(ncm_cgce)
naniar::gg_miss_var(ncm_cgce)
unique(ncm_cgce$CGCE)

# filtrando bens de consumo final pelo código CGCE

ncm_cgce_sep <- ncm_cgce |> separate_rows(CGCE, sep='/')
ncm_bens_consumo <- ncm_cgce_sep |>
  filter(str_starts(CGCE, '3')) |>
  filter(!CGCE %in% c('323', '324')) |>
  rename (ncm = `NCM/IPI 2007`)
unique(ncm_bens_consumo$CGCE)

# recuperando lista de ncms completos

ncm_bens_consumo_completo <- ncm_bens_consumo |>
  select(`Descrição`, CGCE, ncm) |>
  separate_rows(ncm, sep = "\\+") |>
  mutate(ncm = str_trim(ncm)) |>
  mutate(
    base = !str_starts(ncm, "\\."),
    grupo = cumsum(base) # Cria um grupo lógico para cada base
  ) |>
  group_by(grupo) |>
  mutate(
    # Pega o primeiro valor do grupo que tem 4+ dígitos e junta com os pontos
    base_ncm = first(ncm[str_length(ncm) >= 4]),
    # Se for uma extensão, cola a extensão na base removendo o ponto
    ncm_completo = ifelse(
      base, 
      ncm, 
      paste0(str_sub(base_ncm, 1, str_locate(base_ncm, "\\.")[2] - 1), ncm)
    )
  ) |>
  ungroup()

ncm_bens_consumo_completo$ncm_completo <- gsub("\\.", "", ncm_bens_consumo_completo$ncm_completo)
ncm_bens_consumo_completo$ncm_completo <- gsub("\\*", "", ncm_bens_consumo_completo$ncm_completo)


# valores únicos de NCM 

ncms <- ncm_bens_consumo_completo |> distinct(ncm_completo)

write.csv2(ncms, "trabalho/dados/ncms_bens_consumo.csv")
write.csv2(ncms, "trabalho/dados/ncms_bens_consumo_sem_alimentos.csv")

## Importações de 2022

imp_22 <- read_csv2("trabalho/dados/IMP_2022.csv")

imp_bens_consumo_22 <- imp_22 |> filter(if_any(CO_NCM, ~ str_starts(., paste(ncms$ncm_completo, collapse = "|"))))


# CO_UNID	NO_UNID
# 10	QUILOGRAMA LIQUIDO
# 11	NUMERO (UNIDADE)
# 12	MILHEIRO
# 13	PARES
# 14	METRO
# 15	METRO QUADRADO
# 16	METRO CUBICO
# 17	LITRO
# 18	MIL QUILOWATT HORA
# 19	QUILATE
# 20	DUZIA
# 21	TONELADA METRICA LIQUIDA
# 22	GRAMA LIQUIDO
# 23	BILHOES DE UNIDADES INTERNACIONAIS
# 24	QUILOGRAMA BRUTO

# CO_VIA	NO_VIA
# 10	ENTRADA/SAIDA FICTA
# 99	VIA DESCONHECIDA
# 13	POR REBOQUE
# 11	COURIER
# 15	VICINAL FRONTEIRICO
# 14	DUTOS
# 12	EM MAOS
# 00	VIA NAO DECLARADA
# 01	MARITIMA
# 02	FLUVIAL
# 03	LACUSTRE
# 04	AEREA
# 05	POSTAL
# 06	FERROVIARIA
# 07	RODOVIARIA
# 08	CONDUTO/REDE DE TRANSMISSAO
# 09	MEIOS PROPRIOS

imp_bens_consumo_22 |> count(CO_UNID, sort=TRUE) |> mutate(porcentagem=n/sum(n)*100)


#   CO_UNID      n porcentagem
# 1      10 132522     53.1     # quilograma liquido
# 2      11 102065     40.9     # numero (unidade)
# 3      13   6919      2.77    # pares
# 4      17   5157      2.07    # litro
# 5      15   2287      0.917   # metro quadrado
# 6      12    277      0.111   # milheiro
# 7      21    231      0.0926  # tonelada metrica liquida

imp_bens_consumo_22 |> count(CO_VIA, sort=TRUE) |> mutate(porcentagem=n/sum(n)*100)

#   CO_VIA      n porcentagem
# 1 01     134757   54.0         # maritima
# 2 04      98622   39.5         # aerea
# 3 07       9681    3.88        # rodoviaria
# 4 10       6366    2.55        # entrada/saída ficta
# 5 09         17    0.00681     # meios proprios
# 6 05         14    0.00561     # postal
# 7 11          1    0.000401    # courier


imp_bens_consumo_22 |> group_by(CO_UNID) |> summarise(vl_fob_total = sum(VL_FOB)) |> mutate(porcentagem_fob=vl_fob_total/sum(vl_fob_total)*100)

# com alimentos
#   CO_UNID vl_fob_total porcentagem_fob
# 1      10  13865041014         60.5   
# 2      11   7388392826         32.2   
# 3      12     16223749          0.0708
# 4      13    412150584          1.80  
# 5      15     85744311          0.374 
# 6      17   1154741039          5.04  
# 7      21      6894768          0.0301

# # sem alimentos
#   CO_UNID vl_fob_total porcentagem_fob
# 1      10  10159002617         55.6   
# 2      11   7388392826         40.4   
# 3      12     16223749          0.0888
# 4      13    412150584          2.26  
# 5      15     85744311          0.469 
# 6      17    204009027          1.12