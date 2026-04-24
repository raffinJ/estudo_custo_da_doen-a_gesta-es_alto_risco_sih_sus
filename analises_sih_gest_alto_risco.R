
# Código R referente à análise de dados de um estudo de custo da doença com o 
## objetivo de estimar os custos diretos da assistência hospitalar às gestações 
### de alto risco na Região Centro-Oeste do Brasil (2018 a 2021)  
#### em uma perspectiva do Sistema Único de Saúde (SUS)

#Liberar pacotes
library(tidyverse)
library(dplyr)
library(microdatasus)


options(scipen = 99)


# Lista dos diagnósticos de interesse
diag_interesse <- c(
  "O13", "O14", "O140", "O141", "O149", 
  "O15", "O150", "O151", "O152", "O159", 
  "O23", "O230", "O231", "O232", "O233", 
  "O234", "O235", "O239", "O24", "O240", 
  "O241", "O242", "O243", "O244", "O249"
)

dados <- dados %>%
  filter(DIAG_PRINC %in% diag_interesse) # extraindo apenas registros hospitalares de inetersse


# Porcessando com microdatasus
dados <- process_sih(dados) # primeira parte dos dados 

# Selecionando colunas de ineteresse
dados <- dados %>% select(munResUf, ANO_CMPT, PROC_REA, VAL_SH, VAL_SP, VAL_TOT, VAL_UTI, UTI_INT_TO, UTI_MES_TO, MARCA_UTI,
                                  DIAS_PERM, QT_DIARIAS, DIAG_PRINC, IDADE, COD_IDADE, MUNIC_MOV, MUNIC_RES, 
                                  RACA_COR, MORTE) # seleciona apenas certas colunas


# Traduzir códigos de procedimentos SIGTAP
TB_SIGTAP <- read.dbc("TB_SIGTAP.dbc") 

dados <- dados %>%
  left_join(TB_SIGTAP, c("PROC_REA"="CHAVE"))

rm(TB_SIGTAP)

# Criando variável que diz se usou UTI ou não
dados$USO_UTI=ifelse(dados$MARCA_UTI %in% c("00"), c("Não"), "Sim")


# Criar a variavel (REG_ESTAB) para as UF do local de internação
dados <- dados %>%
  mutate(REG_ESTAB = ifelse(substr(MUNIC_MOV, 1, 2) == "11", "Rondônia",
                            ifelse(substr(MUNIC_MOV, 1, 2) == "12", "Acre", 
                                   ifelse(substr(MUNIC_MOV, 1, 2) == "13", "Amazonas",
                                          ifelse(substr(MUNIC_MOV, 1, 2) == "14", "Roraima",
                                                 ifelse(substr(MUNIC_MOV, 1, 2) == "15", "Pará",
                                                        ifelse(substr(MUNIC_MOV, 1, 2) == "16", "Amapá",
                                                               ifelse(substr(MUNIC_MOV, 1, 2) == "17", "Tocantins",
                                                                      ifelse(substr(MUNIC_MOV, 1, 2) == "21", "Maranhão",
                                                                             ifelse(substr(MUNIC_MOV, 1, 2) == "22", "Piauí",
                                                                                    ifelse(substr(MUNIC_MOV, 1, 2) == "23", "Ceará",
                                                                                           ifelse(substr(MUNIC_MOV, 1, 2) == "24", "Rio Grande do Norte",
                                                                                                  ifelse(substr(MUNIC_MOV, 1, 2) == "25", "Paraíba",
                                                                                                         ifelse(substr(MUNIC_MOV, 1, 2) == "27", "Alagoas",
                                                                                                                ifelse(substr(MUNIC_MOV, 1, 2) == "28", "Sergipe",
                                                                                                                       ifelse(substr(MUNIC_MOV, 1, 2) == "29", "Bahia",
                                                                                                                              ifelse(substr(MUNIC_MOV, 1, 2) == "31", "Minas Gerais",
                                                                                                                                     ifelse(substr(MUNIC_MOV, 1, 2) == "32", "Espírito Santo",
                                                                                                                                            ifelse(substr(MUNIC_MOV, 1, 2) == "33", "Rio de Janeiro",
                                                                                                                                                   ifelse(substr(MUNIC_MOV, 1, 2) == "35", "São Paulo",
                                                                                                                                                          ifelse(substr(MUNIC_MOV, 1, 2) == "41", "Paraná",
                                                                                                                                                                 ifelse(substr(MUNIC_MOV, 1, 2) == "42", "Santa Catarina",
                                                                                                                                                                        ifelse(substr(MUNIC_MOV, 1, 2) == "43", "Rio Grande do Sul",
                                                                                                                                                                               ifelse(substr(MUNIC_MOV, 1, 2) == "50", "Mato Grosso do Sul",
                                                                                                                                                                                      ifelse(substr(MUNIC_MOV, 1, 2) == "51", "Mato Grosso",
                                                                                                                                                                                             ifelse(substr(MUNIC_MOV, 1, 2) == "52", "Goiás",
                                                                                                                                                                                                    ifelse(substr(MUNIC_MOV, 1, 2) == "53", "Distrito Federal",
                                                                                                                                                                                                           ifelse(substr(MUNIC_MOV, 1, 2) == "00", "Ignorado/Exterior", "Pernambuco"))))))))))))))))))))))))))))
# Criar a variavel (Regiao) para as regiões do local de internação a partir da UF
dados$Regiao=ifelse(dados$REG_ESTAB %in% c("Paraná", "Santa Catarina", "Rio Grande do Sul"),c("Sul"),
                          ifelse(dados$REG_ESTAB %in% c("São Paulo", "Rio de Janeiro", "Espírito Santo", "Minas Gerais"),c("Sudeste"),
                                 ifelse(dados$REG_ESTAB %in% c("Mato Grosso", "Mato Grosso do Sul", "Goiás", "Distrito Federal"),c("Centro-Oeste"),
                                        ifelse(dados$REG_ESTAB %in% c("Acre", "Amapá", "Amazonas", "Pará", "Rondônia", "Roraima", "Tocantins"),c("Norte"), "Nordeste"))))


# Refinando a população limitando a idade do paciente
dados <- dados %>%
  filter(COD_IDADE == "Anos" & IDADE >= 12 & IDADE <= 59)


# Garantindo que as variáveis estão do formato correto para a análise
dados$PROC_REA <- as.factor(dados$PROC_REA)
dados$ANO_CMPT <- as.factor(dados$ANO_CMPT)
dados$VAL_SH <- as.numeric(dados$VAL_SH)
dados$VAL_SP <- as.numeric(dados$VAL_SP)
dados$VAL_TOT <- as.numeric(dados$VAL_TOT)
dados$VAL_UTI <- as.numeric(dados$VAL_UTI)
dados$DIAS_PERM <- as.numeric(dados$DIAS_PERM)


###Ajustando os custos pela inflação (IPCA - https://www3.bcb.gov.br/CALCIDADAO/publico/corrigirPorIndice.do?method=corrigirPorIndice)

# Fatores de correção por ano para 2024
fatores_correcao <- c("2018" = 1.44884310,
                      "2019" = 1.40290030,
                      "2020" = 1.34491960,
                      "2021" = 1.21450060,
                      "2022" = 1.14683190,
                      "2023" = 1.09552270,
                      "2024" = 1.04461840,
                      "2025" = 1)

# Adicionando as colunas corrigidas no dataframe
dados <- dados %>%
  mutate(
    val_tot_corrigido = VAL_TOT * fatores_correcao[as.character(ANO_CMPT)],
    val_uti_corrigido = VAL_UTI * fatores_correcao[as.character(ANO_CMPT)],
    val_sh_corrigido = VAL_SH * fatores_correcao[as.character(ANO_CMPT)],
    val_sp_corrigido = VAL_SP * fatores_correcao[as.character(ANO_CMPT)]
  )





#_______________________________________________________________________________

################ CRIANDO FUNÇÕES PARA ANALISAR OS DADOS ########################

#_______________________________________________________________________________





###### FUNÇÃO DE ANÁLISES DESCRITIVAS ###
descritiva_cat <- function(dados, variavel){
  dados %>%
    group_by({{variavel}}) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    mutate(prop = round(prop.table(n) * 100, 1))
}



descritiva <- function(dados, vd){
  dados %>%
    summarise(soma = sum({{vd}}),
              media = mean({{vd}}),
              desvio_padrao = sd({{vd}}),
              mediana = median({{vd}}),
              q1 = quantile({{vd}}, p = 0.25),
              q3 = quantile({{vd}}, p = 0.75),
              min = min({{vd}}),
              max = max({{vd}}))
}

descritiva_grupo <- function(dados, grupo, vd){
  dados %>%
    group_by({{grupo}}) %>% 
    summarise(
      soma = sum({{vd}}, na.rm = TRUE),
      media = mean({{vd}}, na.rm = TRUE),
      desvio_padrao = sd({{vd}}, na.rm = TRUE),
      mediana = median({{vd}}, na.rm = TRUE),
      q1 = quantile({{vd}}, probs = 0.25, na.rm = TRUE),
      q3 = quantile({{vd}}, probs = 0.75, na.rm = TRUE),
      min = min({{vd}}, na.rm = TRUE),
      max = max({{vd}}, na.rm = TRUE),
      n = n()
    ) %>%
    ungroup() %>%
    mutate(prop_soma = round(soma / sum(soma) * 100, 1))
}


tab_freq_grupo <- function(dados, variavel, grupo){
  
  tab <- dados |> 
    group_by({{grupo}}, {{variavel}}) |> 
    count() |> 
    ungroup() |> 
    group_by({{grupo}}) |> 
    mutate(porc = 100*n/sum(n))
  
  return(tab)
  
}




#_______________________________________________________________________________

######################### ANALISANDO DADOS ######################################

#_______________________________________________________________________________




tabela <- descritiva(dados, IDADE) # Análise da média [...] de idade da população
tabela <- descritiva(dados, DIAS_PERM) # Análise da média [...] de dias de permanência hospitalar

tabela <- descritiva_cat(dados, ANO_CMPT) # Análise de frequências de registros ao longo dos anos
tabela <- descritiva_cat(dados, IDADE) # Análise de frequências da idade da população
tabela <- descritiva_cat(dados, RACA_COR) # Análise de frequências da raça/cor da população
tabela <- descritiva_cat(dados, USO_UTI) # Análise de frequências de registros de UTI ou não
tabela <- descritiva_cat(dados, DS_REGRA) # Análise de frequências dos procedimentos autorizados nos registros
tabela <- descritiva_cat(dados, REG_ESTAB) # Análise de frequências da UF da instituição hospitalar
tabela <- descritiva_cat(dados, DIAG_PRINC) # Análise de frequências dos diagnósticos

tabela <- tab_freq_grupo(dados, USO_UTI, DIAG_PRINC) # Análise de frequências de registros de UTI por diagnóstico
tabela <- tab_freq_grupo(dados, USO_UTI, ANO_CMPT) # Análise de frequências de registros de UTI por ano
tabela <- tab_freq_grupo(dados, MORTE, ANO_CMPT)  # Análise de frequências de óbitos por ano
tabela <- tab_freq_grupo(dados, DS_REGRA, REG_ESTAB) # Análise de frequências de procedimentos por UF
tabela <- tab_freq_grupo(dados, DS_REGRA, ANO_CMPT) # Análise de frequências de procedimentos por ano

## 

cid_ano <- tab_freq_grupo(dados, DIAG_PRINC, ANO_CMPT) # Análise de frequências de diagnósticos por ano

cid_ano <- cid_ano %>%
  mutate(code = substr(DIAG_PRINC, 1, 3)) # Agrupando os códigos de doença CID-10 em seus grupos

cid_ano <- cid_ano %>%
  group_by(ANO_CMPT, code) %>% 
  summarise(n = sum(n)) %>% # Analisando variação temporal dos grupos de CID-10
  ungroup() 

cid_ano <- cid_ano %>%
  pivot_wider(
    names_from = ANO_CMPT,   
    values_from = n      
  ) %>%
  group_by(code) %>%
  mutate(prop = (sum(`2020`, `2021`, na.rm = TRUE) - sum(`2018`, `2019`, na.rm = TRUE)) / 
           sum(`2018`, `2019`, na.rm = TRUE) * 100)

##


#_______________________________________________________________________________



### Custos


tabela <- descritiva(dados, val_tot_corrigido) # Análise de soma, mediana [...] de custo total

tabela <- dados %>%   # Análise de soma, mediana [...] de custo UTI
  filter(USO_UTI=="Sim")  %>%
  summarise(soma = sum(val_uti_corrigido),
            media = mean(val_uti_corrigido),
            desvio_padrão = sd(val_uti_corrigido),
            mediana = median(val_uti_corrigido),
            q1 = quantile(val_uti_corrigido, probs = 0.25, na.rm = TRUE),
            q3 = quantile(val_uti_corrigido, probs = 0.75, na.rm = TRUE))


tabela <- descritiva_grupo(dados, DIAG_PRINC, val_tot_corrigido) # Análise da mediana [...] de custo total por diagnóstico

tabela <- dados %>%    # Análise da mediana [...] de custo UTI por diagnóstico
  filter(USO_UTI=="Sim")  %>%
  group_by(DIAG_PRINC) %>%
  summarise(soma = sum(val_uti_corrigido),
            media = mean(val_uti_corrigido),
            desvio_padrao = sd(val_uti_corrigido),
            mediana = median(val_uti_corrigido),
            q1 = quantile(val_uti_corrigido, probs = 0.25, na.rm = TRUE),
            q3 = quantile(val_uti_corrigido, probs = 0.75, na.rm = TRUE))


tabela <- descritiva_grupo(dados, ANO_CMPT, val_tot_corrigido)  # Análise da mediana [...] de custo total por ano

tabela <- dados %>%
  filter(USO_UTI=="Sim")  %>%       # Análise da mediana [...] de custo UTI por ano
  group_by(ANO_CMPT) %>%
  summarise(soma = sum(val_uti_corrigido),
            media = mean(val_uti_corrigido),
            desvio_padrão = sd(val_uti_corrigido),
            mediana = median(val_uti_corrigido),
            q1 = quantile(val_uti_corrigido, probs = 0.25, na.rm = TRUE),
            q3 = quantile(val_uti_corrigido, probs = 0.75, na.rm = TRUE))




tabela <- descritiva_grupo(dados, DS_REGRA, val_tot_corrigido)  # Análise da mediana [...] de custo total por procedimento

tabela <- dados %>%
  filter(USO_UTI=="Sim")  %>%      # Análise da mediana [...] de custo UTI por procedimento
  group_by(DS_REGRA) %>%
  summarise(soma = sum(val_uti_corrigido),
            media = mean(val_uti_corrigido),
            desvio_padrão = sd(val_uti_corrigido))



#_______________________________________________________________________________


# UF de atendimento

## Análise da mediana [...] de custo total por UF
tabela_tot <- descritiva_grupo(dados, REG_ESTAB, val_tot_corrigido) 

## Análise da mediana [...] de custo UTI por UF
tabela_uti <- dados %>%
  filter(USO_UTI=="Sim")  %>%
  group_by(REG_ESTAB) %>%
  summarise(soma = sum(val_uti_corrigido),
            media = mean(val_uti_corrigido),
            desvio_padrão = sd(val_uti_corrigido))

## UF de atendimento por ano

tabela <- dados %>%
  group_by(ANO_CMPT, REG_ESTAB) %>%
  summarise(soma = sum(val_tot_corrigido),
            media = mean(val_tot_corrigido),
            desvio_padrão = sd(val_tot_corrigido))

tabela <- dados %>%
  filter(USO_UTI=="Sim")  %>%
  group_by(ANO_CMPT, REG_ESTAB) %>%
  summarise(soma = sum(val_uti_corrigido),
            media = mean(val_uti_corrigido),
            desvio_padrão = sd(val_uti_corrigido))




# custo por mil nascidos vivos:



      # numero de nascidos vivos
      nv <- c("Distrito Federal" = 164013,
              "Goiás" = 378713,
              "Mato Grosso" = 232379,
              "Mato Grosso do Sul" = 171451)



tabela_nv <- tabela_tot %>%
  mutate(
    val_por_mil_nv = (soma / nv[REG_ESTAB]) * 1000,
    br = sum(soma)/946556 *1000 # 946556 é a soma de nascidos vivos
  )



#_______________________________________________________________________________



# atendimento em UF diferente da de residência

tabela <- dados %>%
  filter(munResUf != REG_ESTAB) %>%
  group_by(REG_ESTAB) %>%
  summarise(total = sum(val_tot_corrigido),
            soma_uti = sum(val_uti_corrigido),
            total_n = n()) 

#local de residência da mulher

tabela <- dados %>%
  filter(munResUf != REG_ESTAB) %>%
  group_by(munResUf) %>%
  summarise(n = n())


    
    #só do DF
    tabela <- dados %>%
      filter(REG_ESTAB == "Distrito Federal" & munResUf != REG_ESTAB) %>%
      group_by(DIAG_PRINC) %>%
      summarise(total_aih = n())
    
    #só do GO
    tabela <- dados %>%
      filter(REG_ESTAB == "Goiás" & munResUf != REG_ESTAB) %>%
      group_by(DIAG_PRINC) %>%
      summarise(total_aih = n())
    
    #só do MT
    tabela <- dados %>%
      filter(REG_ESTAB == "Mato Grosso" & munResUf != REG_ESTAB) %>%
      group_by(DIAG_PRINC) %>%
      summarise(total_aih = n())
    
    #só do MS
    tabela <- dados %>%
      filter(REG_ESTAB == "Mato Grosso do Sul" & munResUf != REG_ESTAB) %>%
      group_by(DIAG_PRINC) %>%
      summarise(total_aih = n())



# Diagnósticos

tabela <- dados %>%
  filter(munResUf != REG_ESTAB) %>%
  group_by(DIAG_PRINC) %>%
  summarise(total_aih = n())



#_______________________________________________________________________________



# Análise de sensibilidade
tabela <- descritiva_grupo(dados, USO_UTI, val_tot_corrigido) # Custo total por uso ou não de UTI


# Análise excluindo os percentis extremos 

## Calculando percentil 95
p95 <- quantile(dados$val_tot_corrigido, 0.95, na.rm = TRUE)

# Eexcluindo os percentis extremos
dados_p95 <- dados %>%
  filter(val_tot_corrigido <= p95)

# Calculando estatísticas descritivas
sensibilidade_percentil <- dados_p95 %>%
  summarise(
    soma = sum(val_tot_corrigido),
    media = mean(val_tot_corrigido),
    desvio_padrao = sd(val_tot_corrigido),
    mediana = median(val_tot_corrigido),
    q1 = quantile(val_tot_corrigido, 0.25),
    q3 = quantile(val_tot_corrigido, 0.75),
    min = min(val_tot_corrigido),
    max = max(val_tot_corrigido)
  )
            