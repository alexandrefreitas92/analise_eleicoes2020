# Informação Geral --------------------------------------------------------

# Projeto: Analise do resultado eleitoral da JPT em 2020
# Alexandre Freitas
# Email: alexandrefreitas92@gmail.com

# Ler bibliotecas ---------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(readxl)
library(DT)
library(plotly)
library(knitr)
library(ggthemes)
library(htmlwidgets)

# Ler base de dados -------------------------------------------------------

pt_2016 <- read_csv("data/vereadores_pt_2016.csv")
pt_2020 <- read_csv("data/vereadores_pt_2020.csv")
pt_geral <- bind_rows(pt_2016, pt_2020)

# Preparar cores
cores <- c("#CBAACB", "#ABDEE6", "#F3B0C3", "#8FCACA", "#FFC8A2", "#FFFFB5",  "#CCE2CB", "#ECD5E3", "#FEE1E8", "#D4F0F0")


# Análise dos dados -------------------------------------------------------

# * Votação Total ---------------------------------------------------------
votacao_juventude <- pt_geral %>%
  summarise(total_eleitos_2016 = sum(eleito == "Sim" & ano == 2016 & NR_IDADE_DATA_POSSE %in% c(18:35)),
            total_eleitos_2020 = sum(eleito == "Sim" & ano == 2020 & NR_IDADE_DATA_POSSE %in% c(18:35)),
            votacao_total_2016 = sum(votos_nominais[ano==2016 & NR_IDADE_DATA_POSSE %in% c(18:35)]),
            votacao_total_2020 = sum(votos_nominais[ano==2020 & NR_IDADE_DATA_POSSE %in% c(18:35)]),
            dif_votacao = votacao_total_2020 - votacao_total_2016,
            eleitos_mun_grande_2016 = sum(eleito == "Sim" & porte_municipio == "Grande" & ano == 2016 & NR_IDADE_DATA_POSSE %in% c(18:35)),
            eleitos_mun_grande_2020 = sum(eleito == "Sim" & porte_municipio == "Grande" & ano == 2020 & NR_IDADE_DATA_POSSE %in% c(18:35)),
            eleitos_mun_grande_mulheres_2016 = sum(eleito == "Sim" & porte_municipio == "Grande" & ano == 2016 & DS_GENERO == "Feminino" & NR_IDADE_DATA_POSSE %in% c(18:35)),
            eleitos_mun_grande_mulheres_2020 = sum(eleito == "Sim" & porte_municipio == "Grande" & ano == 2020 & DS_GENERO == "Feminino" & NR_IDADE_DATA_POSSE %in% c(18:35)),
            percentual_mulheres_eleitas_mun_grande_2016_geral = sum(eleito == "Sim" & porte_municipio == "Grande" & ano == 2016 & DS_GENERO == "Feminino" & NR_IDADE_DATA_POSSE %in% c(18:35)) / sum(eleito == "Sim" & porte_municipio == "Grande" & ano == 2016),
            percentual_mulheres_eleitas_mun_grande_2020_geral = sum(eleito == "Sim" & porte_municipio == "Grande" & ano == 2020 & DS_GENERO == "Feminino" & NR_IDADE_DATA_POSSE %in% c(18:35)) / sum(eleito == "Sim" & porte_municipio == "Grande" & ano == 2020),
            percentual_mulheres_eleitas_mun_grande_2016_jovem = sum(eleito == "Sim" & porte_municipio == "Grande" & ano == 2016 & DS_GENERO == "Feminino" & NR_IDADE_DATA_POSSE %in% c(18:35)) / sum(eleito == "Sim" & porte_municipio == "Grande" & ano == 2016 & NR_IDADE_DATA_POSSE %in% c(18:35)),
            percentual_mulheres_eleitas_mun_grande_2020_jovem = sum(eleito == "Sim" & porte_municipio == "Grande" & ano == 2020 & DS_GENERO == "Feminino" & NR_IDADE_DATA_POSSE %in% c(18:35)) / sum(eleito == "Sim" & porte_municipio == "Grande" & ano == 2020 & NR_IDADE_DATA_POSSE %in% c(18:35)),
            votacao_mulheres_jovens_2016 = sum(votos_nominais[porte_municipio == "Grande" & ano == 2016 & DS_GENERO == "Feminino" & NR_IDADE_DATA_POSSE %in% c(18:35)]),
            votacao_mulheres_jovens_2020 = sum(votos_nominais[porte_municipio == "Grande" & ano == 2020 & DS_GENERO == "Feminino" & NR_IDADE_DATA_POSSE %in% c(18:35)]),
            eleitos_mun_grande_negros_2016 = sum(eleito == "Sim" & porte_municipio == "Grande" & ano == 2016 & DS_COR_RACA == "Negra"  & NR_IDADE_DATA_POSSE %in% c(18:35)),
            eleitos_mun_grande_negros_2020 = sum(eleito == "Sim" & porte_municipio == "Grande" & ano == 2020 & DS_COR_RACA == "Negra" & NR_IDADE_DATA_POSSE %in% c(18:35)),
            percentual_negros_eleitas_mun_grande_2016_geral = sum(eleito == "Sim" & porte_municipio == "Grande" & ano == 2016 & DS_COR_RACA == "Negra" & NR_IDADE_DATA_POSSE %in% c(18:35)) / sum(eleito == "Sim" & porte_municipio == "Grande" & ano == 2016),
            percentual_negros_eleitas_mun_grande_2020_geral = sum(eleito == "Sim" & porte_municipio == "Grande" & ano == 2020 & DS_COR_RACA == "Negra" & NR_IDADE_DATA_POSSE %in% c(18:35)) / sum(eleito == "Sim" & porte_municipio == "Grande" & ano == 2020),
            percentual_negros_eleitas_mun_grande_2016_jovem = sum(eleito == "Sim" & porte_municipio == "Grande" & ano == 2016 & DS_COR_RACA == "Negra" & NR_IDADE_DATA_POSSE %in% c(18:35)) / sum(eleito == "Sim" & porte_municipio == "Grande" & ano == 2016 & NR_IDADE_DATA_POSSE %in% c(18:35)),
            percentual_negros_eleitas_mun_grande_2020_jovem = sum(eleito == "Sim" & porte_municipio == "Grande" & ano == 2020 & DS_COR_RACA == "Negra" & NR_IDADE_DATA_POSSE %in% c(18:35)) / sum(eleito == "Sim" & porte_municipio == "Grande" & ano == 2020 & NR_IDADE_DATA_POSSE %in% c(18:35)),
            votacao_negros_jovens_2016 = sum(votos_nominais[porte_municipio == "Grande" & ano == 2016 & DS_COR_RACA == "Negra" & NR_IDADE_DATA_POSSE %in% c(18:35)]),
            votacao_negros_jovens_2020 = sum(votos_nominais[porte_municipio == "Grande" & ano == 2020 & DS_COR_RACA == "Negra" & NR_IDADE_DATA_POSSE %in% c(18:35)]))


votacao_geral <- pt_geral %>%
  summarise(total_eleitos_2016 = sum(eleito == "Sim" & ano == 2016),
            total_eleitos_2020 = sum(eleito == "Sim" & ano == 2020),
            votacao_total_2016 = sum(votos_nominais[ano==2016]),
            votacao_total_2020 = sum(votos_nominais[ano==2020]),
            dif_votacao = votacao_total_2020 - votacao_total_2016,
            eleitos_mun_grande_2016 = sum(eleito == "Sim" & porte_municipio == "Grande" & ano == 2016),
            eleitos_mun_grande_2020 = sum(eleito == "Sim" & porte_municipio == "Grande" & ano == 2020),
            nao_jovens_eleitos_2016_mun_grande = sum(eleito == "Sim" & ano == 2016 & porte_municipio == "Grande" & NR_IDADE_DATA_POSSE > 35, na.rm = TRUE),
            nao_jovens_eleitos_2020_mun_grande = sum(eleito == "Sim" & ano == 2020 & porte_municipio == "Grande" & NR_IDADE_DATA_POSSE > 35, na.rm = TRUE),
            eleitos_mun_grande_mulheres_2016 = sum(eleito == "Sim" & porte_municipio == "Grande" & ano == 2016 & DS_GENERO == "Feminino"),
            eleitos_mun_grande_mulheres_2020 = sum(eleito == "Sim" & porte_municipio == "Grande" & ano == 2020 & DS_GENERO == "Feminino"),
            percentual_mulheres_eleitas_mun_grande_2016 = sum(eleito == "Sim" & porte_municipio == "Grande" & ano == 2016 & DS_GENERO == "Feminino") / sum(eleito == "Sim" & porte_municipio == "Grande" & ano == 2016),
            percentual_mulheres_eleitas_mun_grande_2020 = sum(eleito == "Sim" & porte_municipio == "Grande" & ano == 2020 & DS_GENERO == "Feminino") / sum(eleito == "Sim" & porte_municipio == "Grande" & ano == 2020),
            eleitos_mun_grande_negros_2016 = sum(eleito == "Sim" & porte_municipio == "Grande" & ano == 2016 & DS_COR_RACA == "Negra"),
            eleitos_mun_grande_negros_2020 = sum(eleito == "Sim" & porte_municipio == "Grande" & ano == 2020 & DS_COR_RACA == "Negra"),
            percentual_negros_eleitas_mun_grande_2016 = sum(eleito == "Sim" & porte_municipio == "Grande" & ano == 2016 & DS_COR_RACA == "Negra") / sum(eleito == "Sim" & porte_municipio == "Grande" & ano == 2016),
            percentual_negros_eleitas_mun_grande_2020 = sum(eleito == "Sim" & porte_municipio == "Grande" & ano == 2020 & DS_COR_RACA == "Negra") / sum(eleito == "Sim" & porte_municipio == "Grande" & ano == 2020))

# * Gênero ------------------------------------------------------------------
genero_pt_por_porte <- pt_geral %>%
  ungroup() %>%
  filter(eleito == "Sim" & NR_IDADE_DATA_POSSE <= 35) %>%
  group_by(porte_municipio, ano) %>%
  count(DS_GENERO) %>%
  mutate(total = sum(n),
         percentual = n / total * 100,
         percentual = round(percentual, digits = 2)) %>%
  mutate(porte_municipio = factor(porte_municipio, levels = c("Pequeno", "Médio", "Grande")))

fig_genero_pt_por_porte <- genero_pt_por_porte %>%
  mutate(percentual = percentual / 100) %>%
  rename("Gênero" = DS_GENERO, "Percentual" = percentual, "Porte do município" = porte_municipio) %>%
  ggplot(aes(fill=`Gênero`, y=Percentual, x=`Porte do município`,
             text = paste("Percentual", Percentual))) + 
  geom_bar(position="fill", stat="identity") +
  scale_y_continuous(labels=scales::percent, name = "") +
  labs(fill = "", title = "Vereadores(as) jovens do PT eleitos por gênero e porte do município") +
  xlab("") +
  scale_fill_manual(values = cores[c(1,7)]) +
  facet_wrap(~ano) +
  geom_text(aes(label = paste(n, "\n", "(", round(Percentual*100), "%)", sep = "")), colour = "black", position = position_stack(vjust = 0.5),
            size = 3) +
  theme_classic() +
  theme(legend.position = "bottom")


fig_genero_pt_por_porte

ggsave("produtos/genero_pt_porte.png", plot = fig_genero_pt_por_porte, width = 7, height = 6)

# * Raça/Cor ----------------------------------------------------------------
raca_pt_por_porte <- pt_geral %>%
  ungroup() %>%
  filter(eleito == "Sim" & NR_IDADE_DATA_POSSE <= 35) %>%
  group_by(porte_municipio, ano) %>%
  count(DS_COR_RACA) %>%
  mutate(total = sum(n),
         percentual = n / total * 100,
         percentual = round(percentual, digits = 2)) %>%
  mutate(porte_municipio = factor(porte_municipio, levels = c("Pequeno", "Médio", "Grande")))

fig_raca_pt_por_porte <- raca_pt_por_porte %>%
  mutate(percentual = percentual / 100) %>%
  rename("Raça/Cor" = DS_COR_RACA, "Percentual" = percentual, "Porte do município" = porte_municipio) %>%
  ggplot(aes(fill=`Raça/Cor`, y=Percentual, x=`Porte do município`,
             text = paste("Percentual", Percentual))) + 
  geom_bar(position="fill", stat="identity") +
  scale_y_continuous(labels=scales::percent, name = "") +
  labs(fill = "", title = "Vereadores(as) jovens do PT eleitos por raça/cor e porte do município") +
  xlab("") +
  scale_fill_manual(values = cores[c(7, 3, 1, 10)]) +
  facet_wrap(~ano) +
  geom_text(aes(label = paste(n, "\n", "(", round(Percentual*100), "%)", sep = "")), colour = "black", position = position_stack(vjust = 0.5),
            size = 3) +
  theme_classic() +
  theme(legend.position = "bottom")


fig_raca_pt_por_porte

ggsave("produtos/raca_pt_porte.png", plot = fig_raca_pt_por_porte, width = 7, height = 6)


# * Tabela ----------------------------------------------------------------

# Evolução do Número de jovens petistas eleitos - 2016 a 2020
votacao_vereadores_jovens_pt_2016_2020 <- pt_geral %>%
  filter(NR_IDADE_DATA_POSSE <= 35) %>%
  rename(`Porte Município` = porte_municipio) %>%
  group_by(`Porte Município`) %>%
  summarise(`Eleitos 2016` = sum(eleito == "Sim" & ano == 2016),
            `Votação 2016` = sum(votos_nominais[ano == 2016]),
            `Eleitos 2020` = sum(eleito == "Sim" & ano == 2020),
            `Votação 2020` = sum(votos_nominais[ano == 2020]))


t_votacao_vereadores_jovens_pt_2016_2020 <- datatable(votacao_vereadores_jovens_pt_2016_2020, 
      caption = "Votação dos jovens petistas para vereador(a) nos anos de 2016 e 2020"
) %>%
  formatRound(c(2:5), digits = 0, mark = ".", dec.mark = ",")

withr::with_dir('produtos',DT::saveWidget(t_votacao_vereadores_jovens_pt_2016_2020, 'tabela_votacao_juventude_pt.html'))

# Vereadores jovens do PT
vereadores_jovens_pt <- pt_2020 %>%
  filter(NR_IDADE_DATA_POSSE <= 35) %>%
  select(-c(NR_IDADE_DATA_POSSE, SG_PARTIDO, ano))

# Apresentar tabela com a lista dos vereadores eleitos pelo PT
t_vereadores_jovens_pt <- datatable(vereadores_jovens_pt,
          filter = 'top',
          colnames = c("UF", "Município","Candidata", "Porte do Município", "Votação",
                       "Gênero", "Cor/ raça", "Eleita?", "% de votos"),
          caption = "Tabela com todos(as) os(as) candidatos a vereador jovens do PT em 2020") %>%
  formatPercentage("percentual", 2) %>%
  formatRound("votos_nominais", mark = ".", digits = 0)

withr::with_dir('produtos',DT::saveWidget(t_vereadores_jovens_pt, 'tabela_juventude_eleitos_pt.html'))