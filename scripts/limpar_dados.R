# Informação Geral --------------------------------------------------------

# Projeto: Analise do resultado eleitoral da JPT em 2020
# Alexandre Freitas
# Email: alexandrefreitas92@gmail.com

# Ler bibliotecas ---------------------------------------------------------

library(tidyverse)

# 2020 --------------------------------------------------------------------
# * Ler base de dados de 2020 e bases complementares -------------------------------------------------------

# Dados codigo TSE
brasil_tse <- read_csv(url("https://raw.githubusercontent.com/betafcc/Municipios-Brasileiros-TSE/master/municipios_brasileiros_tse.csv")) %>%
  mutate(codigo_ibge = as.character(codigo_ibge))

# Dados dos candidatos de 2020
votacao_2020 <- read.csv("data/votacao_candidato_munzona_2020_BRASIL.csv", sep = ";", fileEncoding = "Latin1")
candidatos_2020 <- read.csv("data/consulta_cand_2020_BRASIL.csv", sep = ";", fileEncoding = "Latin1")
eleitorado_2020 <- read.csv("data/eleitorado_local_votacao_2020.csv", sep = ";", fileEncoding = "Latin1")

# Lista de municipios sem informacao
municipios_sem_dados <- votacao_2020 %>%
  group_by(NM_MUNICIPIO) %>%
  summarise(n_vereadores_candidatos = sum(DS_CARGO == "Vereador")) %>%
  filter(n_vereadores_candidatos == 0)

paste(municipios_sem_dados$NM_MUNICIPIO, collapse = ", ")

# Limpar dados
eleitorado_2020 <- eleitorado_2020 %>%
  group_by(CD_MUNICIPIO) %>%
  summarise(total_eleitores = sum(QT_ELEITOR))

# Dados estimativa populacional 2020
brasil <- read_xls("data/pop_2020.xls", sheet = 2)

# * Limpar e juntar as base -------------------------------------------
brasil <- brasil %>%
  mutate(cd_ibge = paste(`COD. UF`, `COD. MUNIC`, sep = "")) %>%
  select(cd_ibge, `NOME DO MUNICÍPIO`,`POPULAÇÃO ESTIMADA`) %>%
  inner_join(brasil_tse, by = c("cd_ibge" = "codigo_ibge")) %>%
  select(codigo_tse, `NOME DO MUNICÍPIO`, `POPULAÇÃO ESTIMADA`) %>%
  mutate(codigo_tse = as.integer(codigo_tse)) %>%
  rename(`Município` = `NOME DO MUNICÍPIO`) %>%
  left_join(eleitorado_2020, by = c("codigo_tse" = "CD_MUNICIPIO"))

# Limpar base de candidatos
candidatos_2020 <- candidatos_2020 %>%
  select(SQ_CANDIDATO, CD_GENERO, DS_GENERO, CD_COR_RACA, DS_COR_RACA, NR_IDADE_DATA_POSSE)

# Juntar base de votação e das características dos candidatos
votacao_2020 <- votacao_2020 %>%
  left_join(candidatos_2020, by = "SQ_CANDIDATO") %>%
  left_join(brasil, by = c("CD_MUNICIPIO" = "codigo_tse")) %>%
  mutate(DS_COR_RACA = case_when(DS_COR_RACA %in% c("BRANCA", "AMARELA") ~ "Branca",
                                 DS_COR_RACA %in% c("PARDA", "PRETA") ~ "Negra",
                                 DS_COR_RACA == "INDÍGENA" ~ 
                                   "Indígena",
                                 TRUE ~ "Sem informação"),
         DS_GENERO = case_when(DS_GENERO == "FEMININO" ~ "Feminino",
                               DS_GENERO == "MASCULINO" ~ "Masculino"),
         porte_municipio = case_when(`POPULAÇÃO ESTIMADA` < 50000 ~ "Pequeno",
                                     `POPULAÇÃO ESTIMADA` >= 50000 & total_eleitores <= 200000 ~ "Médio",
                                     total_eleitores > 200000 ~ "Grande"))

rm(candidatos_2020)

# * Lista de vereadores do PT ---------------------------------

vereadores_pt_2020 <- votacao_2020 %>%
  filter(DS_CARGO == "Vereador" & SG_PARTIDO == "PT") %>%
  group_by(SG_UF, Município, SG_PARTIDO, NM_CANDIDATO, porte_municipio) %>%
  summarise(votos_nominais = sum(QT_VOTOS_NOMINAIS),
            situacao_eleicao = first(CD_SIT_TOT_TURNO),
            NR_IDADE_DATA_POSSE = first(NR_IDADE_DATA_POSSE),
            DS_GENERO = first(DS_GENERO),
            DS_COR_RACA = first(DS_COR_RACA)) %>%
  mutate(eleito = if_else(situacao_eleicao %in% c(1, 2, 3), "Sim", "Não")) %>%
  ungroup() %>%
  group_by(SG_UF, Município) %>%
  mutate(votos_totais = sum(votos_nominais),
         percentual = votos_nominais / votos_totais,
         ano = 2020) %>%
  arrange(-votos_nominais) %>%
  select(-c(situacao_eleicao, votos_totais))


# * Escrever csv ------------------------------------------------------------
write_csv(vereadores_pt_2020, "data/vereadores_pt_2020.csv")

# Liberar memória
rm(votacao_2020, eleitorado_2020)
gc()

# 2016 --------------------------------------------------------------------
# * Ler base de dados de 2016 -------------------------------------------------------

votacao_2016 <- read.csv("data/votacao_candidato_munzona_2016_BRASIL.csv", sep = ";", fileEncoding = "Latin1")
candidatos_2016 <- read.csv("data/consulta_cand_2016_BRASIL.csv", sep = ";", fileEncoding = "Latin1")

# * Limpar e juntar base de dados -------------------------------------------

# Limpar base de candidatos
candidatos_2016 <- candidatos_2016 %>%
  select(SQ_CANDIDATO, CD_GENERO, DS_GENERO, CD_COR_RACA, DS_COR_RACA, NR_IDADE_DATA_POSSE)

# Juntar base de votação e das características dos candidatos
votacao_2016 <- votacao_2016 %>%
  left_join(candidatos_2016, by = "SQ_CANDIDATO") %>%
  left_join(brasil, by = c("CD_MUNICIPIO" = "codigo_tse")) %>%
  mutate(DS_COR_RACA = case_when(DS_COR_RACA %in% c("BRANCA", "AMARELA") ~ "Branca",
                                 DS_COR_RACA %in% c("PARDA", "PRETA") ~ "Negra",
                                 DS_COR_RACA == "INDÍGENA" ~ "Indígena",
                                 TRUE ~ "Sem informação"),
         DS_GENERO = case_when(DS_GENERO == "FEMININO" ~ "Feminino",
                               DS_GENERO == "MASCULINO" ~ "Masculino"),
         porte_municipio = case_when(`POPULAÇÃO ESTIMADA` < 50000 ~ "Pequeno",
                                     `POPULAÇÃO ESTIMADA` >= 50000 & total_eleitores <= 200000 ~ "Médio",
                                     total_eleitores > 200000 ~ "Grande"))

rm(candidatos_2016)

# Vereadores jovens - lista geral
vereadores_pt_2016 <- votacao_2016 %>%
  filter(DS_CARGO == "Vereador" & SG_PARTIDO == "PT") %>%
  group_by(SG_UF, Município, SG_PARTIDO, NM_CANDIDATO, porte_municipio) %>%
  summarise(votos_nominais = sum(QT_VOTOS_NOMINAIS),
            situacao_eleicao = first(CD_SIT_TOT_TURNO),
            NR_IDADE_DATA_POSSE = first(NR_IDADE_DATA_POSSE),
            DS_GENERO = first(DS_GENERO),
            DS_COR_RACA = first(DS_COR_RACA)) %>%
  mutate(eleito = if_else(situacao_eleicao %in% c(1, 2, 3), "Sim", "Não")) %>%
  ungroup() %>%
  group_by(SG_UF, Município) %>%
  mutate(votos_totais = sum(votos_nominais),
         percentual = votos_nominais / votos_totais,
         ano = 2016) %>%
  arrange(-votos_nominais) %>%
  select(-c(situacao_eleicao, votos_totais))


# * Escrever csv ----------------------------------------------------------
write_csv(vereadores_pt_2016, "data/vereadores_pt_2016.csv")

# Liberar memória
rm(list=ls())
gc()
