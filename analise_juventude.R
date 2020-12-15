library(tidyverse)
library(openxlsx)
library(readxl)
library(DT)
library(plotly)

# Ler base de dados

#########################
#                       #    
#   Ler base de dados   #
#                       #
#########################

# Dados codigo TSE
brasil_tse <- read_csv(url("https://raw.githubusercontent.com/betafcc/Municipios-Brasileiros-TSE/master/municipios_brasileiros_tse.csv")) %>%
  mutate(codigo_ibge = as.character(codigo_ibge))

# Dados dos candidatos de 2016
votacao_2020 <- read.csv("data/votacao_candidato_munzona_2020_BRASIL.csv", sep = ";", fileEncoding = "Latin1")
candidatos_2020 <- read.csv("data/consulta_cand_2020_BRASIL.csv", sep = ";", fileEncoding = "Latin1")
eleitorado_2020 <- read.csv("data/eleitorado_local_votacao_2020.csv", sep = ";", fileEncoding = "Latin1")
partidos_2020 <- read.csv("data/votacao_partido_munzona_2020_BRASIL.csv", sep = ";", fileEncoding = "Latin1")

# Limpar dados
eleitorado_2020 <- eleitorado_2020 %>%
  group_by(CD_MUNICIPIO) %>%
  summarise(total_eleitores = sum(QT_ELEITOR))

# Dados estimativa populacional 2020
brasil <- read_xls("/home/xedar/Documents/Trabalho/pop_2020.xls", sheet = 2)


########################################
#                                      #    
#   Limpar e juntar as base de dados   #
#                                      #
########################################
brasil <- brasil %>%
  mutate(cd_ibge = paste(`COD. UF`, `COD. MUNIC`, sep = "")) %>%
  select(cd_ibge, `NOME DO MUNICÍPIO`,`POPULAÇÃO ESTIMADA`) %>%
  inner_join(brasil_tse, by = c("cd_ibge" = "codigo_ibge")) %>%
  select(codigo_tse, `NOME DO MUNICÍPIO`, `POPULAÇÃO ESTIMADA`) %>%
  mutate(codigo_tse = as.integer(codigo_tse)) %>%
  rename(`Município` = `NOME DO MUNICÍPIO`) %>%
  left_join(eleitorado_2020, by = c("codigo_tse" = "CD_MUNICIPIO"))

# Limpar base de candidatos
candidatos_2020_2 <- candidatos_2020 %>%
  select(SQ_CANDIDATO, CD_GENERO, DS_GENERO, CD_COR_RACA, DS_COR_RACA, NR_IDADE_DATA_POSSE)

# Limpar base de partidos
sintese_resultado_partidos_2020 <- partidos_2020 %>%
  filter(DS_CARGO == "Vereador") %>%
  group_by(SG_PARTIDO) %>%
  summarise(votos_legenda = sum(QT_VOTOS_LEGENDA),
            votos_nominais = sum(QT_VOTOS_NOMINAIS),
            votos_totais = votos_legenda + votos_nominais) %>%
  ungroup() %>%
  mutate(votos_geral = sum(votos_totais),
         percentual = votos_totais / votos_geral * 100) %>%
  select(-votos_geral) %>%
  arrange(-votos_totais)

# Juntar base de votação e das características dos candidatos
votacao_2020_2 <- votacao_2020 %>%
  left_join(candidatos_2020_2, by = "SQ_CANDIDATO") %>%
  left_join(brasil, by = c("CD_MUNICIPIO" = "codigo_tse")) %>%
  mutate(DS_COR_RACA = case_when(DS_COR_RACA %in% c("BRANCA", "AMARELA") ~ "Branca",
                                 DS_COR_RACA %in% c("PARDA", "PRETA") ~ "Negra",
                                 DS_COR_RACA == "INDÍGENA" ~ "Indígena",
                                 TRUE ~ "Sem informação"),
         DS_GENERO = case_when(DS_GENERO == "FEMININO" ~ "Feminino",
                               DS_GENERO == "MASCULINO" ~ "Masculino"),
         porte_municipio = case_when(`POPULAÇÃO ESTIMADA` < 50000 ~ "Pequeno",
                                     `POPULAÇÃO ESTIMADA` > 50000 & total_eleitores <= 200000 ~ "Médio",
                                     total_eleitores > 200000 ~ "Grande"))

###############################################
#                                             #    
#   Análise geral das votações de juventude   #
#                                             #
###############################################

# Análise geral dos dados das juventudes em 2020
vereadores_juventude <- votacao_2020_2 %>%
  ungroup() %>%
  filter(DS_CARGO == "Vereador") %>%
  group_by(Município, SG_PARTIDO, NM_CANDIDATO) %>%
  summarise(TOTAL_VOTOS = sum(QT_VOTOS_NOMINAIS),
            situacao_eleicao = first(CD_SIT_TOT_TURNO %in% c(1, 2, 3)),
            NR_IDADE_DATA_POSSE = first(NR_IDADE_DATA_POSSE)) %>%
  ungroup() %>%
  group_by(SG_PARTIDO) %>%
  summarise(votos_nominais = sum(TOTAL_VOTOS),
            votos_nominais_juventude = sum(TOTAL_VOTOS[NR_IDADE_DATA_POSSE %in% c(15:29)]),
            n_eleitos = sum(situacao_eleicao %in% c(1, 2, 3)),
            n_jovens_eleitos = sum(situacao_eleicao %in% c(1, 2, 3) & NR_IDADE_DATA_POSSE %in% c(15:29))) %>%
  mutate(SG_PARTIDO = toupper(SG_PARTIDO),
         total_eleitos = sum(n_eleitos),
         votos_totais = sum(votos_nominais),
         percentual_eleitos = n_eleitos / total_eleitos * 100,
         percentual_eleitos_jovem = n_jovens_eleitos / n_eleitos * 100,
         percentual_votos_vereador = votos_nominais / votos_totais * 100,
         percentual_votos_jovens = votos_nominais_juventude / votos_nominais * 100) %>%
  select(-c(total_eleitos, votos_totais)) %>%
  arrange(-percentual_votos_vereador)

# Vereadores jovens - lista geral
vereadores_jovens <- votacao_2020_2 %>%
  filter(DS_CARGO == "Vereador") %>%
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
         percentual = votos_nominais / votos_totais) %>%
  filter(NR_IDADE_DATA_POSSE %in% c(15:35)) %>%
  arrange(-votos_nominais) %>%
  select(-c(situacao_eleicao, NR_IDADE_DATA_POSSE, votos_totais))

# Escrever csv
write_csv(vereadores_jovens, "data/vereadores_jovens_2020.csv")

# Vereadores jovens do PT
vereadores_jovens_pt <- vereadores_jovens %>%
  filter(SG_PARTIDO == "PT")

# Salvar tabela com a lista dos vereadores eleitos pelo PT
tabela_juventude_pt <- datatable(vereadores_jovens_pt,
                                 filter = 'top',
                                 colnames = c("UF", "Município", "Partido", "Candidata(o)", "Porte do Município", "Votação",
                                              "Gênero", "Cor ou raça", "Eleito(a)?", "Percentual de votos")) %>%
  formatPercentage("percentual", 2) %>%
  formatRound("votos_nominais", mark = ".", digits = 0)

saveWidget(tabela_juventude_pt, 'tabela_juventude_pt.html')


#######################################################
#                                                     #    
#   Análise do perfil dos candidatos eleitos jovens   #
#                                                     #
#######################################################

# Análise geral

# Gênero
genero_geral <- vereadores_jovens %>%
  ungroup() %>%
  filter(eleito == "Sim") %>%
  count(DS_GENERO) %>%
  mutate(total = sum(n),
         percentual = n / total * 100,
         percentual = round(percentual, digits = 2))

fig_genero_geral <- plot_ly(genero_geral, x = ~DS_GENERO, y = ~percentual, type = 'bar', color = ~DS_GENERO) %>% 
  layout(yaxis = list(title = '%'),
         xaxis = list(title = 'Gênero'),
         title = "Gênero dos(as) jovens vereadores(as) eleitos(as) em 2020")

fig_genero_geral
saveWidget(fig_genero_geral, 'fig_genero_geral.html')
# Raca/Cor
cor_geral <- vereadores_jovens %>%
  ungroup() %>%
  filter(eleito == "Sim") %>%
  count(DS_COR_RACA) %>%
  mutate(total = sum(n),
         percentual = n / total * 100,
         percentual = round(percentual, digits = 2))

fig_raca_cor_geral <- plot_ly(cor_geral, x = ~DS_COR_RACA, y = ~percentual, type = 'bar', color = ~DS_COR_RACA,
               textposition = 'outside',
               text = ~percentual) %>%
  layout(yaxis = list(title = '%'),
         xaxis = list(title = 'Raça/Cor'),
         title = "Raça/cor dos(as) jovens vereadores(as) eleitos(as) em 2020")
fig_raca_cor_geral
saveWidget(fig_raca_cor_geral, 'fig_raca_cor_geral.html')

# Análise do PT

# Gênero
genero_pt <- vereadores_jovens %>%
  ungroup() %>%
  filter(eleito == "Sim" & SG_PARTIDO == "PT") %>%
  count(DS_GENERO) %>%
  mutate(total = sum(n),
         percentual = n / total * 100,
         percentual = round(percentual, digits = 2))

fig_genero_pt <- plot_ly(genero_pt, x = ~DS_GENERO, y = ~percentual, type = 'bar', color = ~DS_GENERO) %>%
  layout(yaxis = list(title = '%'),
         xaxis = list(title = 'Gênero'),
         title = "Gênero dos(as) jovens vereadores(as) eleitos(as) pelo PT em 2020"
         )
fig_genero_pt
saveWidget(fig_genero_pt, 'fig_genero_pt.html')

# Raca/Cor
cor_pt <- vereadores_jovens %>%
  ungroup() %>%
  filter(eleito == "Sim" & SG_PARTIDO == "PT") %>%
  count(DS_COR_RACA) %>%
  mutate(total = sum(n),
         percentual = n / total * 100,
         percentual = round(percentual, digits = 2))

fig_raca_cor_pt <- plot_ly(cor_pt, x = ~DS_COR_RACA, y = ~percentual, type = 'bar', color = ~DS_COR_RACA,
               textposition = 'outside',
               text = ~percentual) %>% 
  layout(yaxis = list(title = '%'),
         xaxis = list(title = 'Raça/Cor'),
         title = "Raça/cor dos(as) jovens vereadores(as) eleitos(as) pelo PT em 2020")
fig_raca_cor_pt
saveWidget(fig_raca_cor_pt, 'fig_raca_cor_pt.html')


#######################################################
#                                                     #    
#   Análise do perfil dos candidatos eleitos jovens   #
#                 por porte do município              #
#                                                     #
#######################################################

# Análise geral

# Gênero
genero_geral_por_porte <- vereadores_jovens %>%
  ungroup() %>%
  filter(eleito == "Sim") %>%
  group_by(porte_municipio) %>%
  count(DS_GENERO) %>%
  mutate(total = sum(n),
         percentual = n / total * 100,
         percentual = round(percentual, digits = 2)) %>%
  mutate(porte_municipio = factor(porte_municipio, levels = c("Pequeno", "Médio", "Grande")))

# Gráfico
fig_genero_geral_por_porte <- genero_geral_por_porte %>%
  group_by(DS_GENERO) %>%
  plot_ly(x=~porte_municipio, y=~percentual,
          type="scatter",color=~DS_GENERO, mode="lines+markers") %>%
  layout(yaxis = list(range = c(0,100), title = "%"),
         xaxis = list(title = "Porte do município"),
         title = "Gênero dos(as) jovens vereadores(as) eleitos(as) por porte do município em 2020",
         margin = list(t = 100))
fig_genero_geral_por_porte
saveWidget(fig_genero_geral_por_porte, 'fig_genero_geral_por_porte.html')

# Raca/Cor
raca_cor_geral_por_porte <- vereadores_jovens %>%
  ungroup() %>%
  filter(eleito == "Sim") %>%
  group_by(porte_municipio) %>%
  count(DS_COR_RACA) %>%
  mutate(total = sum(n),
         percentual = n / total * 100,
         percentual = round(percentual, digits = 2)) %>%
  mutate(porte_municipio = factor(porte_municipio, levels = c("Pequeno", "Médio", "Grande")))

# Gráfico
fig_raca_cor_geral_por_porte <- raca_cor_geral_por_porte %>%
  group_by(DS_COR_RACA) %>%
  plot_ly(x=~porte_municipio, y=~percentual,
          type="scatter",color=~DS_COR_RACA, mode="lines+markers") %>%
  layout(yaxis = list(range = c(0,100), title = "%"),
         xaxis = list(title = "Porte do município"),
         title = "Raça/cor dos(as) jovens vereadores(as) eleitos(as) por porte do município em 2020")
fig_raca_cor_geral_por_porte
saveWidget(fig_raca_cor_geral_por_porte, 'fig_raca_cor_geral_por_porte.html')

# Análise PT

# Gênero
genero_pt_por_porte <- vereadores_jovens %>%
  ungroup() %>%
  filter(SG_PARTIDO == "PT" & eleito == "Sim") %>%
  group_by(porte_municipio) %>%
  count(DS_GENERO) %>%
  mutate(total = sum(n),
         percentual = n / total * 100,
         percentual = round(percentual, digits = 2)) %>%
  mutate(porte_municipio = factor(porte_municipio, levels = c("Pequeno", "Médio", "Grande")))

# Gráfico
fig_genero_pt_por_porte <- genero_pt_por_porte %>%
  group_by(DS_GENERO) %>%
  plot_ly(x=~porte_municipio, y=~percentual,
          type="scatter",color=~DS_GENERO, mode="lines+markers") %>%
  layout(yaxis = list(range = c(0,100), title = "%"),
         xaxis = list(title = "Porte do município"),
         title = "Gênero dos(as) jovens vereadores(as) do PT eleitos(as) por porte do município em 2020")
fig_genero_pt_por_porte
saveWidget(fig_genero_pt_por_porte, 'fig_genero_pt_por_porte.html')


# Raca/Cor
raca_cor_pt_por_porte <- vereadores_jovens %>%
  ungroup() %>%
  filter(SG_PARTIDO == "PT" & eleito == "Sim") %>%
  group_by(porte_municipio) %>%
  count(DS_COR_RACA) %>%
  mutate(total = sum(n),
         percentual = n / total * 100,
         percentual = round(percentual, digits = 2)) %>%
  mutate(porte_municipio = factor(porte_municipio, levels = c("Pequeno", "Médio", "Grande")))

# Gráfico
fig_raca_cor_pt_por_porte <- raca_cor_pt_por_porte %>%
  group_by(DS_COR_RACA) %>%
  plot_ly(x=~porte_municipio, y=~percentual,
          type="scatter",color=~DS_COR_RACA, mode="lines+markers") %>%
  layout(yaxis = list(range = c(0,100), title = "%"),
         xaxis = list(title = "Porte do município"),
         title = "Raça/cor dos(as) jovens vereadores(as) do PT eleitos(as) por porte do município em 2020")
fig_raca_cor_pt_por_porte
saveWidget(fig_raca_cor_pt_por_porte, 'fig_raca_cor_pt_por_porte.html')

# Porte do município
pt_por_porte <- vereadores_jovens %>%
  ungroup() %>%
  filter(SG_PARTIDO == "PT" & eleito == "Sim") %>%
  group_by(porte_municipio) %>%
  summarise(n = n(),
            votos = sum(votos_nominais)) %>%
  ungroup() %>%
  mutate(total = sum(votos),
         percentual = votos / total * 100,
         percentual = round(percentual, digits = 2)) %>%
  mutate(porte_municipio = factor(porte_municipio, levels = c("Pequeno", "Médio", "Grande"))) %>%
  select(-total)

#############################################
#                                           #
#   Evolução do número de jovens eleitos    #
#                                           #
#############################################

# Abrir base de dados de 2016
votacao_2016 <- read.csv("data/votacao_candidato_munzona_2016_BRASIL.csv", sep = ";", fileEncoding = "Latin1")
candidatos_2016 <- read.csv("data/consulta_cand_2016_BRASIL.csv", sep = ";", fileEncoding = "Latin1")

# Limpar base de candidatos
candidatos_2016_2 <- candidatos_2016 %>%
  select(SQ_CANDIDATO, CD_GENERO, DS_GENERO, CD_COR_RACA, DS_COR_RACA, NR_IDADE_DATA_POSSE)

# Juntar base de votação e das características dos candidatos
votacao_2016_2 <- votacao_2016 %>%
  left_join(candidatos_2016_2, by = "SQ_CANDIDATO") %>%
  left_join(brasil, by = c("CD_MUNICIPIO" = "codigo_tse")) %>%
  mutate(DS_COR_RACA = case_when(DS_COR_RACA %in% c("BRANCA", "AMARELA") ~ "Branca",
                                 DS_COR_RACA %in% c("PARDA", "PRETA") ~ "Negra",
                                 DS_COR_RACA == "INDÍGENA" ~ "Indígena",
                                 TRUE ~ "Sem informação"),
         DS_GENERO = case_when(DS_GENERO == "FEMININO" ~ "Feminino",
                               DS_GENERO == "MASCULINO" ~ "Masculino"),
         porte_municipio = case_when(`POPULAÇÃO ESTIMADA` < 50000 ~ "Pequeno",
                                     `POPULAÇÃO ESTIMADA` > 50000 & total_eleitores <= 200000 ~ "Médio",
                                     total_eleitores > 200000 ~ "Grande"))

# Vereadores jovens - lista geral
vereadores_jovens_2016 <- votacao_2016_2 %>%
  filter(DS_CARGO == "Vereador") %>%
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
         percentual = votos_nominais / votos_totais) %>%
  filter(NR_IDADE_DATA_POSSE %in% c(15:35)) %>%
  arrange(-votos_nominais) %>%
  select(-c(situacao_eleicao, NR_IDADE_DATA_POSSE, votos_totais))

# Escrever csv
write_csv(vereadores_jovens_2016, "data/vereadores_jovens_2016.csv")

# Vereadores jovens do PT
vereadores_jovens_pt_2016 <- vereadores_jovens_2016 %>%
  filter(SG_PARTIDO == "PT")

# Porte do município
pt_por_porte_2016 <- vereadores_jovens_2016 %>%
  ungroup() %>%
  filter(SG_PARTIDO == "PT" & eleito == "Sim") %>%
  group_by(porte_municipio) %>%
  summarise(n = n(),
            votos = sum(votos_nominais)) %>%
  ungroup() %>%
  mutate(total = sum(votos),
         percentual = votos / total * 100,
         percentual = round(percentual, digits = 2)) %>%
  mutate(porte_municipio = factor(porte_municipio, levels = c("Pequeno", "Médio", "Grande"))) %>%
  select(-total)


# Análise PT 2016

# Gênero
genero_pt_por_porte_2016 <- vereadores_jovens_2016 %>%
  ungroup() %>%
  filter(SG_PARTIDO == "PT" & eleito == "Sim") %>%
  group_by(porte_municipio) %>%
  count(DS_GENERO) %>%
  mutate(total = sum(n),
         percentual = n / total * 100,
         percentual = round(percentual, digits = 2)) %>%
  mutate(porte_municipio = factor(porte_municipio, levels = c("Pequeno", "Médio", "Grande")))

# Gráfico
fig_genero_pt_por_porte_2016 <- genero_pt_por_porte_2016 %>%
  group_by(DS_GENERO) %>%
  plot_ly(x=~porte_municipio, y=~percentual,
          type="scatter",color=~DS_GENERO, mode="lines+markers") %>%
  layout(yaxis = list(range = c(0,100), title = "%"),
         xaxis = list(title = "Porte do município"),
         title = "Gênero dos(as) jovens vereadores(as) do PT eleitos(as) por porte do município em 2016")
fig_genero_pt_por_porte_2016
saveWidget(fig_genero_pt_por_porte_2016, 'fig_genero_pt_por_porte_2016.html')


# Raca/Cor
raca_cor_pt_por_porte_2016 <- vereadores_jovens_2016 %>%
  ungroup() %>%
  filter(SG_PARTIDO == "PT" & eleito == "Sim") %>%
  group_by(porte_municipio) %>%
  count(DS_COR_RACA) %>%
  mutate(total = sum(n),
         percentual = n / total * 100,
         percentual = round(percentual, digits = 2)) %>%
  mutate(porte_municipio = factor(porte_municipio, levels = c("Pequeno", "Médio", "Grande")))

# Gráfico
fig_raca_cor_pt_por_porte_2016 <- raca_cor_pt_por_porte_2016 %>%
  group_by(DS_COR_RACA) %>%
  plot_ly(x=~porte_municipio, y=~percentual,
          type="scatter",color=~DS_COR_RACA, mode="lines+markers") %>%
  layout(yaxis = list(range = c(0,100), title = "%"),
         xaxis = list(title = "Porte do município"),
         title = "Raça/cor dos(as) jovens vereadores(as) do PT eleitos(as) por porte do município em 2016")
fig_raca_cor_pt_por_porte_2016
saveWidget(fig_raca_cor_pt_por_porte_2016, 'fig_raca_cor_pt_por_porte_2016.html')

# Evolução do Número de jovens petistas eleitos
total_jovens_eleitos <- vereadores_jovens_pt %>%
  ungroup() %>%
  summarise(total_2020 = sum(eleito == "Sim"),
            total_2016 = sum(vereadores_jovens_pt_2016$eleito == "Sim"),
            votos_2020 = sum(votos_nominais),
            votos_2016 = sum(vereadores_jovens_pt_2016$votos_nominais))

votacao_vereadores_jovens_pt_2020 <- vereadores_jovens_pt %>%
  ungroup() %>%
  group_by(porte_municipio) %>%
  summarise(eleitos = sum(eleito == "Sim"),
            votos = sum(votos_nominais),
            ano = 2020) %>%
  ungroup() %>%
  mutate(votos_totais = sum(votos),
         percentual = votos / votos_totais * 100)

votacao_vereadores_jovens_pt_2016 <- vereadores_jovens_pt_2016 %>%
  ungroup() %>%
  group_by(porte_municipio) %>%
  summarise(eleitos = sum(eleito == "Sim"),
            votos = sum(votos_nominais),
            ano = 2016) %>%
  ungroup() %>%
  mutate(votos_totais = sum(votos),
         percentual = votos / votos_totais * 100)

votacao_vereadores_jovens_pt_2020_2016 <- votacao_vereadores_jovens_pt_2020 %>%
  bind_rows(votacao_vereadores_jovens_pt_2016)

# Gráfico com os eleitos por porte
fig_pequeno <- votacao_vereadores_jovens_pt_2020_2016 %>%
  filter(porte_municipio == "Pequeno") %>%
  plot_ly(x = ~ano, y = ~eleitos, type="scatter", mode = "lines+markers") %>%
  layout(yaxis = list(range = c(400,600), title = "Eleitos"))

fig_medio <- votacao_vereadores_jovens_pt_2020_2016 %>%
  filter(porte_municipio == "Médio") %>%
  plot_ly(x = ~ano, y = ~eleitos, type="scatter", mode = "lines+markers") %>%
  layout(yaxis = list(range = c(0,100), title = "Eleitos"))

fig_grande <- votacao_vereadores_jovens_pt_2020_2016 %>%
  filter(porte_municipio == "Grande") %>%
  plot_ly(x = ~ano, y = ~eleitos, type="scatter", mode = "lines+markers") %>%
  layout(yaxis = list(range = c(0,40), title = "Eleitos"))

fig_pequeno
fig_medio
fig_grande
saveWidget(fig_pequeno, 'fig_votacao_juv_2016_2020_porte_pequeno.html')
saveWidget(fig_medio, 'fig_votacao_juv_2016_2020_porte_medio.html')
saveWidget(fig_grande, 'fig_votacao_juv_2016_2020_porte_grande.html')
