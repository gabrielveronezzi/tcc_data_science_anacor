# -------------------------- PACOTES ---------------------------------

#Pacotes utilizados
pacotes <- c("plotly","tidyverse","ggrepel","sjPlot","reshape2","knitr",
             "kableExtra","FactoMineR", "tibble", "foreign")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# ----------------------- TRATAMENTO DOS DADOS --------------------------

# carregando a base de dados
df<- read.spss("04758.SAV", to.data.frame = TRUE) 

# remover colulas P4 (candidato que nao votaria)
df <- df[,-9:-21]

# remover categ. zeradas da var. RELIGIAO: Sara nossa terra (9) e Judaica (14)
levels(df$RELIGIAO)[c(9,14)] <- "Outras religiões"

# renomeando escolaridade por nível de escolaridade
levels(df$ESCOLARIDADE) <- c("Analfabeto",                               
                             "Sabe ler/ escrever, mas não cursou escola",
                             "Fundamental incompleto",                   
                             "Fundamental incompleto",                     
                             "Fundamental incompleto",                     
                             "Fundamental incompleto",                     
                             "Fundamental incompleto",                     
                             "Fundamental incompleto",                     
                             "Fundamental incompleto",                     
                             "Fundamental incompleto",                     
                             "Fundamental completo",                     
                             "Medio incompleto",                                 
                             "Medio incompleto",                                 
                             "Medio Completo",                                 
                             "Superior incompleto",                      
                             "Superior completo")

# classe
class(df$SEXO)
# tabela de frequência
table(df$SEXO)
# resumo
glimpse(df)

# ---------------- TABELAS DE CONTINGENCIAS EM FUNCAO DE P3 ----------------

# Para este estudo será utilizado a intenção de voto P3,
# que é a intenção de voto que leva em consideração
# apenas os principais candidatos na disputa presidencial.

# SEXO
tab_sexo <- table(df$P3, 
                  df$SEXO)
tab_sexo

# IDADE (variável quantitativa, não é alvo do estudo)
# FXIDADE
tab_fxidade <- table(df$P3, 
                        df$FXIDADE)
tab_fxidade

# ALFABETIZACAO
tab_alfabetizacao <- table(df$P3, 
                  df$ALFABETIZACAO)
tab_alfabetizacao

# ESCOLARIDADE
tab_escolaridade <- table(df$P3, 
                  df$ESCOLARIDADE)
tab_escolaridade

# P1, P2, P3, P4, P5, P6, P7 E PVOTO

# RACA_COR
tab_raca <- table(df$P3, 
                  df$RACA_COR)
tab_raca <- tab_raca[,1:5]
tab_raca

# RELIGIAO
tab_religiao <- table(df$P3, 
                  df$RELIGIAO)
tab_religiao

# REND1
tab_rend1 <- table(df$P3, 
                  df$REND1)
tab_rend1

# REND2
tab_rend2 <- table(df$P3, 
                  df$REND2)
tab_rend2

# REGIÃO
tab_regiao <- table(df$P3,
                  df$REGIÃO)
tab_regiao

# CONDIÇAO_MUNICIPIO
tab_cond_municipio <- table(df$P3, 
                  df$CONDIÇAO_MUNICIPIO)
tab_cond_municipio

# PORTE
tab_porte <- table(df$P3, 
                  df$PORTE)
tab_porte

# -------------------------- TESTE QUI QUADRADO -----------------------------

# Neste momento, através do teste qui², será verificado se
# a associação das categorias se da de maneira aleatória.
# Caso a associação se de de maneira ALEATORIA, não teremos análise 
# por correspondencias, pois não haverá significancia estatistica.

# H0 (hipotese nula): categorias das variáveis se associam de forma aleatória
# H1 (hipotese alternativa): associação entre categ das variaveis é 
# estatisticamente significante, sendo possível a ANACOR

# SEXO
qui2_sexo <- chisq.test(tab_sexo)
qui2_sexo

# FXIDADE
qui2_fxidade <- chisq.test(tab_fxidade)
qui2_fxidade

# ALFABETIZACAO
qui2_alfabetizacao <- chisq.test(tab_alfabetizacao)
qui2_alfabetizacao

# ESCOLARIDADE
qui2_escolaridade <- chisq.test(tab_escolaridade)
qui2_escolaridade

# RACA_COR
qui2_raca <- chisq.test(tab_raca)
qui2_raca

# RELIGIAO
qui2_religiao <- chisq.test(tab_religiao)
qui2_religiao

# REND1
qui2_rend1 <- chisq.test(tab_rend1)
qui2_rend1

# REND2
qui2_rend2 <- chisq.test(tab_rend2)
qui2_rend2

# REGIÃO
qui2_regiao <- chisq.test(tab_regiao)
qui2_regiao

# CONDIÇAO_MUNICIPIO
qui2_cond_municipio <- chisq.test(tab_cond_municipio)
qui2_cond_municipio

# PORTE
qui2_porte <- chisq.test(tab_porte)
qui2_porte

# Adotando nivel de significância de 5%, validamos H1 quando 
# o p-value é menor que 0,05

# Tabela de P-Values
lista_categorias <- c("sexo",
                      "fxidade",
                      "alfabetizacao",
                      "escolaridade",
                      "raca",
                      "religiao",
                      "rend1",
                      "rend2",
                      "regiao",
                      "cond_municipio",
                      "porte")

lista_pvalue <- c(qui2_sexo$p.value,
                  qui2_fxidade$p.value,
                  qui2_alfabetizacao$p.value,
                  qui2_escolaridade$p.value,
                  qui2_raca$p.value,
                  qui2_religiao$p.value,
                  qui2_rend1$p.value,
                  qui2_rend2$p.value,
                  qui2_regiao$p.value,
                  qui2_cond_municipio$p.value,
                  qui2_porte$p.value) 

df_pvalues <- data.frame(
  var=lista_categorias,
  pvalue=lista_pvalue,
  hipotese=ifelse(lista_pvalue < 0.05,"H1","H0"))
df_pvalues

# -------------------------- ANACOR -----------------------------

# ANACOR das variáveis que possuem associação estatisticamente significante:
# escolaridade
# raca
# religiao
# rend1
# rend2
# regiao

levels(df$ESCOLARIDADE)

# ESCOLARIDADE
# ANACOR
anacor_escolaridade <- CA(tab_escolaridade)
anacor_escolaridade

# Mapa de calor dos resíduos padronizados ajustados
data.frame(qui2_escolaridade$stdres) %>%
  rename(voto = 1,
         escolaridade = 2) %>% 
  ggplot(aes(x = fct_rev(voto), y = escolaridade, fill = Freq, label = round(Freq,3))) +
  geom_tile() +
  geom_text(size = 3) +
  scale_fill_gradient2(low = "darkblue", 
                       mid = "white", 
                       high = "red",
                       midpoint = 0) +
  labs(x = NULL, y = NULL) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.text.x = element_text(angle = 90))

# RAÇA / COR
# ANACOR
anacor_raca <- CA(tab_raca)
anacor_raca

# Mapa de calor dos resíduos padronizados ajustados
data.frame(qui2_raca$stdres) %>%
  rename(voto = 1,
         raca = 2) %>% 
  ggplot(aes(x = fct_rev(voto), y = raca, fill = Freq, label = round(Freq,3))) +
  geom_tile() +
  geom_text(size = 3) +
  scale_fill_gradient2(low = "darkblue", 
                       mid = "white", 
                       high = "red",
                       midpoint = 0) +
  labs(x = NULL, y = NULL) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.text.x = element_text(angle = 90))

# RELIGIÃO
# ANACOR
anacor_religiao <- CA(tab_religiao)
anacor_religiao

# Mapa de calor dos resíduos padronizados ajustados
data.frame(qui2_religiao$stdres) %>%
  rename(voto = 1,
         religiao = 2) %>% 
  ggplot(aes(x = fct_rev(voto), y = religiao, fill = Freq, label = round(Freq,3))) +
  geom_tile() +
  geom_text(size = 3) +
  scale_fill_gradient2(low = "darkblue", 
                       mid = "white", 
                       high = "red",
                       midpoint = 0) +
  labs(x = NULL, y = NULL) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.text.x = element_text(angle = 90))

# REND1
# ANACOR
anacor_rend1 <- CA(tab_rend1)
anacor_rend1

# Mapa de calor dos resíduos padronizados ajustados
data.frame(qui2_rend1$stdres) %>%
  rename(voto = 1,
         rend1 = 2) %>% 
  ggplot(aes(x = fct_rev(voto), y = rend1, fill = Freq, label = round(Freq,3))) +
  geom_tile() +
  geom_text(size = 3) +
  scale_fill_gradient2(low = "darkblue", 
                       mid = "white", 
                       high = "red",
                       midpoint = 0) +
  labs(x = NULL, y = NULL) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.text.x = element_text(angle = 90))

# REND2
# ANACOR
anacor_rend2 <- CA(tab_rend2)
anacor_rend2

# Mapa de calor dos resíduos padronizados ajustados
data.frame(qui2_rend2$stdres) %>%
  rename(voto = 1,
         rend2 = 2) %>% 
  ggplot(aes(x = fct_rev(voto), y = rend2, fill = Freq, label = round(Freq,3))) +
  geom_tile() +
  geom_text(size = 3) +
  scale_fill_gradient2(low = "darkblue", 
                       mid = "white", 
                       high = "red",
                       midpoint = 0) +
  labs(x = NULL, y = NULL) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.text.x = element_text(angle = 90))

# REGIAO
# ANACOR
anacor_regiao <- CA(tab_regiao)
anacor_regiao

# Mapa de calor dos resíduos padronizados ajustados
data.frame(qui2_regiao$stdres) %>%
  rename(voto = 1,
         regiao = 2) %>% 
  ggplot(aes(x = fct_rev(voto), y = regiao, fill = Freq, label = round(Freq,3))) +
  geom_tile() +
  geom_text(size = 3) +
  scale_fill_gradient2(low = "darkblue", 
                       mid = "white", 
                       high = "red",
                       midpoint = 0) +
  labs(x = NULL, y = NULL) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.text.x = element_text(angle = 90))

# Eigenvalues / Qtd de dimensoes para capturar toda variancia
anacor_escolaridade$eig
anacor_raca$eig
anacor_regiao$eig
anacor_religiao$eig
anacor_rend1$eig
anacor_rend2$eig
