# Script para leitura de bancos de dados diversos para geração de um data frame de uma única linha referente as informações do estado do aluno

# Ao receber este script esqueleto colocá-lo no repositório LOCAL Extensao, que deve ter sido clonado do GitHub
# Enviar o script esqueleto para o repositório REMOTO com o nome extensao-esqueleto.R

# Para realizar as tarefas da ETAPA 1, ABRIR ANTES uma branch de nome SINASC no main de Extensao e ir para ela
# Após os alunos concluírem a ETAPA 1 a professora orientará fazer o merge into main e depois abrir outro branch. Aguarde...


####################################
# ETAPA 1: BANCO DE DADOS DO SINASC
####################################

# A ALTERAÇÃO DO SCRIPT ESQUELETO - ETAPA 1 - DEVERÁ SER FEITA DENTRO DA BRANCH SINASC

# Tarefa 1. Leitura do banco de dados do SINASC 2015  com 3017668 linhas e 61 colunas
# verificar se a leitura foi feita corretamente e a estrutura dos dados
# nomeie o banco de dados como dados_sinasc

dados_sinasc = read.csv("SINASC_2015.csv", sep=";", header=TRUE)
head(dados_sinasc)
str(dados_sinasc)

# Tarefa 2. Reduzir dados_sinasc apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sinasc_1
# as colunas serão 1, 4, 5, 6, 7, 12, 13, 14, 15, 19, 21, 22, 23, 24, 35, 38, 44, 46, 48, 59, 60, 61
# nomes das respectivas variáveis: CONTADOR, CODMUNNASC, LOCNASC, IDADEMAE, ESTCIVMAE, CODMUNRES, GESTACAO, GRAVIDEZ, PARTO,
# SEXO, APGAR5, RACACOR, PESO, IDANOMAL, ESCMAE2010, RACACORMAE, SEMAGESTAC, CONSPRENAT, TPAPRESENT, TPROBSON, PARIDADE, KOTELCHUCK

dados_sinasc_1 = dados_sinasc[, c(1, 4, 5, 6, 7, 12, 13, 14, 15, 19, 21, 22, 23, 24, 35, 38, 44, 46, 48, 59, 60, 61)]
summary(dados_sinasc_1)

# Tarefa 3. Reduzir dados_sinasc_1 apenas para o estado que o aluno irá trabalhar (utilizar os dois primeiros dígitos de CODMUNRES), nomeando este novo banco de dados como dados_sinasc_2
# Códigos das UF: 11: RO, 12: AC, 13: AM, 14: RR, 15: PA, 16: AP, 17: TO, 21: MA, 22: PI, 23: CE, 24: RN
# 25: PB, 26: PE, 27: AL, 28: SE, 29: BA, 31: MG, 32: ES, 33: RJ, 35: SP, 41: PR, 42: SC, 43: RS
# 50: MS, 51: MT, 52: GO, 53: DF 

# observar abaixo o número de nascimentos por UF de residência para certificar-se que seu banco de dados está correto
# 11: 27918     12: 16980     13: 80097     14: 11412     15: 143657    16: 15750      17: 25110
# 21: 117564    22: 49253     23: 132516    24: 49099     25: 59089     26: 145024     27: 52257     28: 34917     29: 206655
# 31: 268305    32: 56941     33: 236960    35: 634026     
# 41: 160947    42: 97223     43: 148359
# 50: 44142     51: 56673     52: 100672    53: 46122 


summary(dados_sinasc_1$CODMUNRES)
UF = substr(as.character(dados_sinasc_1$CODMUNRES), 1, 2)

# Considerando que a resolução é para o estado do Acre, ou seja, UF = 12
dados_sinasc_2 = dados_sinasc_1[UF == "31",]

# Exportar o arquivo com o nome dados_sinasc_2.csv e apagando arquivos não mais necessários
write.csv(dados_sinasc_2, "dados_sinasc_2.csv", row.names = FALSE)

rm(dados_sinasc, dados_sinasc_1)
gc()

# Ao concluir a Tarefa 3 da Etapa 1 commite e envie para o repositório REMOTO o script e dados_sinasc_2.csv com o comentário "Dados do estado UF (coloque o nome da UF) e script de sua obtenção"


# Tarefa 4. Verificar em dados_sinasc_2 a frequência das categorias das seguintes variáveis: LOCNASC, ESTCIVMAE, GESTACAO, GRAVIDEZ, PARTO,
# SEXO, RACACOR, IDANOMAL, ESCMAE2010, RACACORMAE, TPAPRESENT, TPROBSON, PARIDADE, KOTELCHUCK

table(dados_sinasc_2$LOCNASC)
table(dados_sinasc_2$ESTCIVMAE)
table(dados_sinasc_2$GESTACAO)
table(dados_sinasc_2$GRAVIDEZ)
table(dados_sinasc_2$PARTO)
table(dados_sinasc_2$SEXO)
table(dados_sinasc_2$RACACOR)
table(dados_sinasc_2$IDANOMAL)
table(dados_sinasc_2$ESCMAE2010)
table(dados_sinasc_2$RACACORMAE)
table(dados_sinasc_2$TPAPRESENT)
table(dados_sinasc_2$TPROBSON)
table(dados_sinasc_2$PARIDADE)
table(dados_sinasc_2$KOTELCHUCK)

# Aproveitando para ver os valores das variáveis quantitativas
unique(dados_sinasc_2$IDADEMAE)
unique(dados_sinasc_2$CONSPRENAT)
unique(dados_sinasc_2$SEMAGESTAC)
unique(dados_sinasc_2$APGAR5)
unique(dados_sinasc_2$PESO)
summary(dados_sinasc_2$PESO)

# Tarefa 5. Atribuir para cada variável de dados_sinasc_2 como sendo NA a categoria de "Não informado ou Ignorado", geralmente com código 9
# KOTELCHUCK = 9 significa "não informado"   TPROBSON = 11 significa "não classificado por falta de informação"
# veja o dicionário do SINASC para identificar qual o código das categorias de cada variável
# Em variáveis quantitativas como IDADEMAE, CONSPRENAT, APGAR5 e PESO e SEMAGESTAC verificar se existem valores como 99 para NA
dados_sinasc_2$LOCNASC[dados_sinasc_2$LOCNASC == 9] = NA
dados_sinasc_2$IDADEMAE[dados_sinasc_2$IDADEMAE == 99] = NA
dados_sinasc_2$ESTCIVMAE[dados_sinasc_2$ESTCIVMAE == 9] = NA
dados_sinasc_2$GESTACAO[dados_sinasc_2$GESTACAO == 9] = NA
dados_sinasc_2$GRAVIDEZ[dados_sinasc_2$GRAVIDEZ == 9] = NA
dados_sinasc_2$PARTO[dados_sinasc_2$PARTO == 9] = NA
dados_sinasc_2$SEXO[dados_sinasc_2$SEXO == 0] = NA
dados_sinasc_2$APGAR5[dados_sinasc_2$APGAR5 == 99] = NA
dados_sinasc_2$PESO[dados_sinasc_2$PESO == 9999] = NA
dados_sinasc_2$IDANOMAL[dados_sinasc_2$IDANOMAL == 9] = NA
dados_sinasc_2$ESCMAE2010[dados_sinasc_2$ESCMAE2010 == 9] = NA
dados_sinasc_2$CONSPRENAT[dados_sinasc_2$CONSPRENAT == 99] = NA
dados_sinasc_2$TPAPRESENT[dados_sinasc_2$TPAPRESENT == 9] = NA
dados_sinasc_2$TPROBSON[dados_sinasc_2$TPROBSON == 11] = NA
dados_sinasc_2$KOTELCHUCK[dados_sinasc_2$KOTELCHUCK == 9] = NA
summary(dados_sinasc_2)

# Por curiosidade, verificando o tamanho dos banco de dados referente ao estado e aos municípios com e sem NAs
n_total_nasc_UF = nrow(dados_sinasc_2)
n_total_nasc_UF_sem_missing = sum(complete.cases(dados_sinasc_2))
n_total_nasc_MUN = tapply(rep(1, nrow(dados_sinasc_2)), dados_sinasc_2$CODMUNRES, sum)
n_total_nasc_MUN_sem_missing = tapply(complete.cases(dados_sinasc_2), dados_sinasc_2$CODMUNRES, sum)


# Tarefa 6. Atribuir legendas para as categorias das variáveis qualitativas investigadas na tarefa 4.
# Exemplo: dados_sinasc_2$KOTELCHUCK = factor(dados_sinasc_2$KOTELCHUCK, levels = c(1,2,3,4,5), 
# labels = c("Não realizou pré-natal", "Inadequado", "Intermediário", "Adequado",  
# "Mais que adequado")

# ATENçÃO: 1. Na hora de escrever os labels, somente a primeira letra da palavra é maiúscula. Exemplo para SEXO: Feminino e Masculino
#          2. Nesta Tarefa 6 não crie novas variáveis no banco de dados

dados_sinasc_2$LOCNASC = factor(dados_sinasc_2$LOCNASC, levels = c(1,2,3,4), labels = c("Hospital", "Outros estabelecimentos de saúde", "Domicílio", "Outros"))
dados_sinasc_2$ESTCIVMAE = factor(dados_sinasc_2$ESTCIVMAE, levels = c(1,2,3,4,5), labels = c("Solteira", "Casada", "Viúva", "Separada judicialmente/divorciada", "União estável"))
dados_sinasc_2$GESTACAO = factor(dados_sinasc_2$GESTACAO, levels = c(1,2,3,4,5,6), labels = c("Menos de 22 semanas", "22 a 27 semanas", "28 a 31 semanas", "32 a 36 semanas", "32 a 36 semanas", "42 semanas e mais"))
dados_sinasc_2$GRAVIDEZ = factor(dados_sinasc_2$GRAVIDEZ, levels = c(1,2,3), labels = c("Única", "Dupla", "Tripla ou mais"))
dados_sinasc_2$PARTO = factor(dados_sinasc_2$PARTO, levels = c(1,2), labels = c("Vaginal", "Cesário"))
dados_sinasc_2$SEXO = factor(dados_sinasc_2$SEXO, levels = c(1,2), labels = c("Masculino", "Feminino"))
dados_sinasc_2$RACACOR = factor(dados_sinasc_2$RACACOR, levels = c(1,2,3,4,5), labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))
dados_sinasc_2$IDANOMAL = factor(dados_sinasc_2$IDANOMAL, levels = c(1,2), labels = c("Sim", "Não"))
dados_sinasc_2$ESCMAE2010 = factor(dados_sinasc_2$ESCMAE2010, levels = c(0,1,2,3,4,5), labels = c("Sem escolaridade", "Fundamental I (1ª a 4ª série)", "Fundamental II (5ª a 8ª série)", "Médio (antigo 2º grau)", "Superior incompleto", "Superior completo"))
dados_sinasc_2$RACACORMAE = factor(dados_sinasc_2$RACACORMAE, levels = c(1,2,3,4,5), labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))
dados_sinasc_2$TPAPRESENT = factor(dados_sinasc_2$TPAPRESENT, levels = c(1,2,3), labels = c("Cefálico", "Pélvica ou podálica", "Transversa"))
dados_sinasc_2$TPROBSON = factor(dados_sinasc_2$TPROBSON, levels = c(1,2,3,4,5,6,7,8,9,10), labels = c("Grupo 1", "Grupo 2", "Grupo 3", "Grupo 4", "Grupo 5", "Grupo 6", "Grupo 7", "Grupo 8", "Grupo 9", "Grupo 10"))
dados_sinasc_2$PARIDADE = factor(dados_sinasc_2$PARIDADE, levels = c(0,1), labels = c("Nulípara", "Multípara"))
dados_sinasc_2$KOTELCHUCK = factor(dados_sinasc_2$KOTELCHUCK, levels = c(1,2,3,4,5), labels = c("Não realizou pré-natal", "Inadequado", "Intermediário", "Adequado", "Mais que adequado"))


# Tarefa 7. Categorizar as variáveis IDADEMAE, PESO e APGAR5 e criar variáveis referentes ao deslocamento materno (peregrinação) e estado civil
# nova variável: dados_sinasc_2$F_PESO com PESO: < 2500: Baixo peso, >=2500 e < 4000: Peso normal, >= 4000: Macrossomia
# nova variável dados_sinasc_2$F_IDADE com IDADEMAE: <15, 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50+
# nova variável dados_sinasc_2$F_APGAR5 com APGAR5: < 7: Baixo, >= 7: Normal
# Atenção para casos de NA em IDADEMAE, PESO e APGAR5
# nova variável: dados_sinasc_2$PERIG: Não: CODMUNNASC igual a CODMUNRES, Sim: CODMUNNASC diferente de CODMUNRES
# nova variável: dados_sinasc_2$ESTCIV: Sem companheiro: ESTCIVMAE 1, 3 ou 4, Com companheiro: ESTCIVMAE 2 ou 5
# Ao categorizar as variáveis, garantir que sejam transformadas em tipo fator


dados_sinasc_2$F_IDADE = ifelse(dados_sinasc_2$IDADEMAE < 15, "<15",
                                ifelse(dados_sinasc_2$IDADEMAE <= 19, "15-19",
                                       ifelse(dados_sinasc_2$IDADEMAE <= 24, "20-24",
                                              ifelse(dados_sinasc_2$IDADEMAE <= 29, "25-29",
                                                     ifelse(dados_sinasc_2$IDADEMAE <= 34, "30-34",
                                                            ifelse(dados_sinasc_2$IDADEMAE <= 39, "35-39",
                                                                   ifelse(dados_sinasc_2$IDADEMAE <= 44, "40-44",
                                                                          ifelse(dados_sinasc_2$IDADEMAE <= 49, "45-49",
                                                                                 "50+"))))))))
dados_sinasc_2$F_IDADE = factor(dados_sinasc_2$F_IDADE,
                                levels = c("<15","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+"), ordered = TRUE)

dados_sinasc_2$F_PESO = ifelse(dados_sinasc_2$PESO < 2500, "Baixo peso",
                               ifelse(dados_sinasc_2$PESO < 4000, "Peso normal",
                                      "Macrossomia"))
dados_sinasc_2$F_PESO = factor(dados_sinasc_2$F_PESO, levels = c("Baixo peso","Peso normal","Macrossomia"))

dados_sinasc_2$F_APGAR5 = ifelse(dados_sinasc_2$APGAR5 < 7, "Baixo", "Normal")
dados_sinasc_2$F_APGAR5 = factor(dados_sinasc_2$F_APGAR5,levels = c("Baixo","Normal"))

dados_sinasc_2$PERIG = ifelse(is.na(dados_sinasc_2$CODMUNNASC) | is.na(dados_sinasc_2$CODMUNRES), NA,
                              ifelse(dados_sinasc_2$CODMUNNASC == dados_sinasc_2$CODMUNRES, "Não", "Sim"))
dados_sinasc_2$PERIG = factor(dados_sinasc_2$PERIG, levels = c("Não", "Sim"))

dados_sinasc_2$ESTCIV = ifelse(dados_sinasc_2$ESTCIVMAE %in% c("Solteira", "Viúva", "Separada judicialmente/divorciada"), "Sem companheiro",
                               ifelse(dados_sinasc_2$ESTCIVMAE %in% c("Casada", "União estável"), "Com companheiro", NA))
dados_sinasc_2$ESTCIV = factor(dados_sinasc_2$ESTCIV, levels = c("Sem companheiro","Com companheiro"))



# Tarefa 8. Agregar ao banco de dados_sinasc_2 as informações PESO_P10 e PESO_P90 a partir de Tabela_PIG_Brasil.csv
# a Tabela PIG informa P10 e P90 dos pesos, de acordo com a idade gestacional
# criar nova variável referente ao peso, de acordo com a idade gestacional, conforme indicado abaixo
# nova variável apenas para casos de GRAVIDEZ Única: dados_sinasc_2$F_PIG: PIG: PESO < PESO_P10, AIG: PESO_P10 <= PESO <= PESO_P90, GIG: PESO > PESO_P90
# Atenção para casos de NA em SEMAGESTAC, PESO ou SEXO. Lembre-se também que em dados_sinasc_2 SEXO está como fator com as categorias Feminino e Masculino.

tabela_pig = read.csv("Tabela_PIG_Brasil.csv", sep = ";", header = TRUE)

dados_sinasc_2 = merge(dados_sinasc_2, tabela_pig, by = c("SEMAGESTAC", "SEXO"), all.x = TRUE)

dados_sinasc_2$F_PIG = ifelse(
  dados_sinasc_2$GRAVIDEZ == "Única" & 
    !is.na(dados_sinasc_2$PESO) & 
    !is.na(dados_sinasc_2$PESO_P10) & 
    !is.na(dados_sinasc_2$PESO_P90),
  
  ifelse(dados_sinasc_2$PESO < dados_sinasc_2$PESO_P10, "PIG",
         ifelse(dados_sinasc_2$PESO > dados_sinasc_2$PESO_P90, "GIG", "AIG")),
  
  NA 
)

dados_sinasc_2$F_PIG = factor(dados_sinasc_2$F_PIG, levels = c("PIG", "AIG", "GIG"))

# Tarefa 9 & 10.

library(dplyr)

cols_base = c("CONTADOR", "CODMUNNASC", "LOCNASC", "IDADEMAE", "ESTCIVMAE", "CODMUNRES", 
              "GESTACAO", "GRAVIDEZ", "PARTO", "SEXO", "APGAR5", "RACACOR", "PESO", 
              "IDANOMAL", "ESCMAE2010", "RACACORMAE", "SEMAGESTAC", "CONSPRENAT", 
              "TPAPRESENT", "TPROBSON", "PARIDADE", "KOTELCHUCK")

dados_sinasc_2$NIVEL = "MUNICIPIO"
dados_sinasc_2$CODMUNRES = as.character(dados_sinasc_2$CODMUNRES)

dados_uf = dados_sinasc_2
dados_uf$NIVEL = "UF"
dados_uf$CODMUNRES = "31"

dados_totais = rbind(dados_uf, dados_sinasc_2)

dados_totais$REG_COMPLETO_22 = complete.cases(dados_totais[, cols_base])

SINASC_MG = dados_totais |>
  group_by(NIVEL, CODMUNRES) |>
  summarise(
    ANO = 2015,
    TN = n(),
    
    TNRC = sum(REG_COMPLETO_22, na.rm = TRUE),
    TNRCR = sum(REG_COMPLETO_22, na.rm = TRUE),
    
    TGI_15 = sum(F_IDADE == "<15", na.rm = TRUE),
    TGI_15_19 = sum(F_IDADE == "15-19", na.rm = TRUE),
    TGI_20_24 = sum(F_IDADE == "20-24", na.rm = TRUE),
    TGI_25_29 = sum(F_IDADE == "25-29", na.rm = TRUE),
    TGI_30_34 = sum(F_IDADE == "30-34", na.rm = TRUE),
    TGI_35_39 = sum(F_IDADE == "35-39", na.rm = TRUE),
    TGI_40_44 = sum(F_IDADE == "40-44", na.rm = TRUE),
    TGI_45_49 = sum(F_IDADE == "45-49", na.rm = TRUE),
    TGI_50 = sum(F_IDADE == "50+", na.rm = TRUE),
    TGIF = sum(F_IDADE %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"), na.rm = TRUE),
    
    IM_P25 = quantile(IDADEMAE, probs = 0.25, na.rm = TRUE),
    IM_P50 = quantile(IDADEMAE, probs = 0.50, na.rm = TRUE),
    IM_P75 = quantile(IDADEMAE, probs = 0.75, na.rm = TRUE),
    IM_MD = mean(IDADEMAE, na.rm = TRUE),
    IM_DP = sd(IDADEMAE, na.rm = TRUE),
    
    EM_S = sum(ESCMAE2010 == "Sem escolaridade", na.rm = TRUE),
    EM_FI = sum(ESCMAE2010 == "Fundamental I (1ª a 4ª série)", na.rm = TRUE),
    EM_FII = sum(ESCMAE2010 == "Fundamental II (5ª a 8ª série)", na.rm = TRUE),
    EM_M = sum(ESCMAE2010 == "Médio (antigo 2º grau)", na.rm = TRUE),
    EM_SI = sum(ESCMAE2010 == "Superior incompleto", na.rm = TRUE),
    EM_SC = sum(ESCMAE2010 == "Superior completo", na.rm = TRUE),
    
    TGRC_B = sum(RACACORMAE == "Branca", na.rm = TRUE),
    TGRC_PT = sum(RACACORMAE == "Preta", na.rm = TRUE),
    TGRC_A = sum(RACACORMAE == "Amarela", na.rm = TRUE),
    TGRC_PD = sum(RACACORMAE == "Parda", na.rm = TRUE),
    TGRC_I = sum(RACACORMAE == "Indígena", na.rm = TRUE),
    
    TGSC = sum(ESTCIV == "Sem companheiro", na.rm = TRUE),
    TGCC = sum(ESTCIV == "Com companheiro", na.rm = TRUE),
    TGPRI = sum(PARIDADE == "Nulípara", na.rm = TRUE), 
    TGNPRI = sum(PARIDADE == "Multípara", na.rm = TRUE),
    
    TGU = sum(GRAVIDEZ == "Única", na.rm = TRUE),
    TGG = sum(GRAVIDEZ %in% c("Dupla", "Tripla ou mais"), na.rm = TRUE),
    
    TGD_22 = sum(GESTACAO == "Menos de 22 semanas", na.rm = TRUE),
    TGD_22_27 = sum(GESTACAO == "22 a 27 semanas", na.rm = TRUE),
    TGD_28_31 = sum(GESTACAO == "28 a 31 semanas", na.rm = TRUE),
    TGD_32_36 = sum(GESTACAO == "32 a 36 semanas", na.rm = TRUE),
    TGD_37_41 = sum(GESTACAO == "37 a 41 semanas", na.rm = TRUE),
    TGD_42 = sum(GESTACAO == "42 semanas e mais", na.rm = TRUE),
    TGD_PRT = sum(SEMAGESTAC < 37, na.rm = TRUE),
    TGD_AT = sum(SEMAGESTAC >= 37 & SEMAGESTAC <= 41, na.rm = TRUE),
    TGD_PST = sum(SEMAGESTAC >= 42, na.rm = TRUE),
    
    DG_P25 = quantile(SEMAGESTAC, probs = 0.25, na.rm = TRUE),
    DG_P50 = quantile(SEMAGESTAC, probs = 0.50, na.rm = TRUE),
    DG_P75 = quantile(SEMAGESTAC, probs = 0.75, na.rm = TRUE),
    DG_MD = mean(SEMAGESTAC, na.rm = TRUE),
    DG_DP = sd(SEMAGESTAC, na.rm = TRUE),
    
    TKC_NR = sum(KOTELCHUCK == "Não realizou pré-natal", na.rm = TRUE),
    TKC_ID = sum(KOTELCHUCK == "Inadequado", na.rm = TRUE),
    TKC_IT = sum(KOTELCHUCK == "Intermediário", na.rm = TRUE),
    TKC_AD = sum(KOTELCHUCK == "Adequado", na.rm = TRUE),
    TKC_MAD = sum(KOTELCHUCK == "Mais que adequado", na.rm = TRUE),
    
    TGPRG_S = sum(PERIG == "Sim", na.rm = TRUE),
    TGPRG_N = sum(PERIG == "Não", na.rm = TRUE),
    TPV = sum(PARTO == "Vaginal", na.rm = TRUE),
    TPC = sum(PARTO == "Cesário", na.rm = TRUE), 
    
    TRAP_C = sum(TPAPRESENT == "Cefálico", na.rm = TRUE),
    TRAP_P = sum(TPAPRESENT == "Pélvica ou podálica", na.rm = TRUE),
    TRAP_T = sum(TPAPRESENT == "Transversa", na.rm = TRUE),
    
    TGROB_1 = sum(TPROBSON == "Grupo 1", na.rm = TRUE),
    TGROB_2 = sum(TPROBSON == "Grupo 2", na.rm = TRUE),
    TGROB_3 = sum(TPROBSON == "Grupo 3", na.rm = TRUE),
    TGROB_4 = sum(TPROBSON == "Grupo 4", na.rm = TRUE),
    TGROB_5 = sum(TPROBSON == "Grupo 5", na.rm = TRUE),
    TGROB_6 = sum(TPROBSON == "Grupo 6", na.rm = TRUE),
    TGROB_7 = sum(TPROBSON == "Grupo 7", na.rm = TRUE),
    TGROB_8 = sum(TPROBSON == "Grupo 8", na.rm = TRUE),
    TGROB_9 = sum(TPROBSON == "Grupo 9", na.rm = TRUE),
    TGROB_10 = sum(TPROBSON == "Grupo 10", na.rm = TRUE),
    
    TNLOC_H = sum(LOCNASC == "Hospital", na.rm = TRUE),
    TNLOC_ES = sum(LOCNASC == "Outros estabelecimentos de saúde", na.rm = TRUE),
    TNLOC_D = sum(LOCNASC == "Domicílio", na.rm = TRUE),
    TNLOC_O = sum(LOCNASC == "Outros", na.rm = TRUE),
    TNLOC_AI = sum(LOCNASC == "Aldeia Indígena", na.rm = TRUE),
    
    TRS_M = sum(SEXO == "Masculino", na.rm = TRUE),
    TRS_F = sum(SEXO == "Feminino", na.rm = TRUE),
    
    TRRC_B = sum(RACACOR == "Branca", na.rm = TRUE),
    TRRC_PT = sum(RACACOR == "Preta", na.rm = TRUE),
    TRRC_A = sum(RACACOR == "Amarela", na.rm = TRUE),
    TRRC_PD = sum(RACACOR == "Parda", na.rm = TRUE),
    TRRC_I = sum(RACACOR == "Indígena", na.rm = TRUE),
    
    TRP_BP = sum(F_PESO == "Baixo peso", na.rm = TRUE),
    TRP_N = sum(F_PESO == "Peso normal", na.rm = TRUE),
    TRP_M = sum(F_PESO == "Macrossomia", na.rm = TRUE),
    
    PESO_P25 = quantile(PESO, probs = 0.25, na.rm = TRUE),
    PESO_P50 = quantile(PESO, probs = 0.50, na.rm = TRUE),
    PESO_P75 = quantile(PESO, probs = 0.75, na.rm = TRUE),
    PESO_MD = mean(PESO, na.rm = TRUE),
    PESO_DP = sd(PESO, na.rm = TRUE),
    
    TRPIG_P = sum(F_PIG == "PIG", na.rm = TRUE),
    TRPIG_A = sum(F_PIG == "AIG", na.rm = TRUE),
    TRPIG_G = sum(F_PIG == "GIG", na.rm = TRUE),
    
    TRAPG5_B = sum(F_APGAR5 == "Baixo", na.rm = TRUE),
    TRAPG5_N = sum(F_APGAR5 == "Normal", na.rm = TRUE),
    APG5_MD = mean(APGAR5, na.rm = TRUE),
    APG5_DP = sd(APGAR5, na.rm = TRUE),
    
    TRAC = sum(IDANOMAL == "Sim", na.rm = TRUE),
    TRSAC = sum(IDANOMAL == "Não", na.rm = TRUE),
    
    .groups = "drop"
  ) |>
  arrange(desc(NIVEL == "UF"), CODMUNRES)

# Tarefa 11: Exporte o banco de dados com o nome SINASC_UF.csv

write.csv(SINASC_MG, "SINASC_MG.csv", row.names = FALSE)

# Ao terminar a ETAPA 1 commite e envie para o repositório REMOTO com o comentário "Dados da UF e Script Etapa 1"

##################################
# ETAPA 2: BANCO DE DADOS DO SIM
##################################
# Só inicie esta Etapa quando a professora orientar
# Altere o script esqueleto nas partes que se refere a ETAPA 2 e envie para o repositório Extensao tendo feito o commit "Esqueleto atualizado na Etapa 2"
# A partir de main crie a branch SIM e vá para ela
# ESTANDO NA BRANCH SIM, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 1 e só insira comandos na ETAPA 2

# Tarefa 1. Leitura do banco de dados Mortalidade_Geral_2015 do SIM 2015 com 1264175 linhas e 87 colunas
# verificar se a leitura foi feita corretamente e a estrutura dos dados
# nomeie o banco de dados como dados_sim

library(readr)
library(dplyr)

dados_sim = read_csv2('Mortalidade_Geral_2015.csv')

# Tarefa 2. Reduzir dados_sim apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sim_1
# as colunas serão: 1, 3, 4, 8, 9, 10, 11, 14, 17, 35, 36, 37, 47, 77, 84
# nomes das respectivas variáveis: CONTADOR, TIPOBITO, DTOBITO, DTNASC, IDADE, SEXO, RACACOR, ESC2010, 
# CODMUNRES, TPMORTEOCO, OBITOGRAV, OBITOPUERP, CAUSABAS, TPOBITOCOR, MORTEPARTO

dados_sim_1 = dados_sim[c(1, 3, 4, 8, 9, 10, 11, 14, 17, 35, 36, 37, 47, 77, 84)]

# Tarefa 3. Reduzir dados_sim_1 apenas para o estado que o aluno irá trabalhar (utilizar os dois primeiros dígitos de 
# CODMUNRES), nomeando este novo banco de dados como dados_sim_2
# Códigos das UF: 11: RO, 12: AC, 13: AM, 14: RR, 15: PA, 16: AP, 17: TO, 21: MA, 22: PI, 23: CE, 24: RN
# 25: PB, 26: PE, 27: AL, 28: SE, 29: BA, 31: MG, 32: ES, 33: RJ, 35: SP, 41: PR, 42: SC, 43: RS
# 50: MS, 51: MT, 52: GO, 53: DF

dados_sim_2 = dados_sim_1 |> 
  filter(substr(as.character(CODMUNRES), 1, 2) == "31")

# observar abaixo o número de óbitos por UF de residência para certificar-se que seu banco de dados está correto
# 11: 7948      12: 3517      13: 16675     14: 2091      15: 37365     16: 2946       17: 7402
# 21: 33666     22: 19366     23: 55258     24: 20153     25: 26422     26: 62556      27: 19756     28: 13453     29: 87083
# 31: 131274    32: 22332     33: 127714    35: 287645    
# 41: 70839     42: 37984     43: 82349
# 50: 15457     51: 17095     52: 38854     53: 11975

# Exportar o arquivo com o nome dados_sim_2.csv

write_csv2(dados_sim_2, "dados_sim_2.csv")

# Ao concluir a Tarefa 3 da Etapa 2 commite e envie para o repositório REMOTO o script e dados_sim_2.csv com o comentário "Dados do estado UF (coloque o nome da UF) e script de sua obtenção"


# Tarefa 4. Verificar em dados_sim_2 a frequência das categorias das seguintes variáveis: TIPOBITO, SEXO, RACACOR,
# TPMORTEOCO, OBITOGRAV, OBITOPUERP, CAUSABAS, TPOBITOCOR, MORTEPARTO


variaveis = c("TIPOBITO", "SEXO", "RACACOR", "TPMORTEOCO", "OBITOGRAV", "OBITOPUERP", "CAUSABAS", "TPOBITOCOR", "MORTEPARTO")

for (v in variaveis) {
  resultado = dados_sim_2 |> 
    count(dados_sim_2[v])
  print(resultado)
}

# Tarefa 5. Atribuir para cada variável de dados_sim_2 como sendo NA a categoria de "Não informado ou Ignorado", 
# geralmente com código 9
# veja o dicionário do SIM para identificar qual o código das categorias de cada variável
# Em variáveis quantitativas como IDADE verificar se existem valores como 9999 para NA

library(dplyr)

# Tarefa 5: Atribuindo NA para valores "Ignorados"
dados_sim_2 = dados_sim_2 |> 
  mutate(
    SEXO = ifelse(SEXO == 9 | SEXO == 0, NA, SEXO), 
    RACACOR    = ifelse(RACACOR == 9, NA, RACACOR),
    TPMORTEOCO = ifelse(TPMORTEOCO == 9, NA, TPMORTEOCO),
    OBITOGRAV  = ifelse(OBITOGRAV == 9, NA, OBITOGRAV),
    OBITOPUERP = ifelse(OBITOPUERP == 9, NA, OBITOPUERP),
    MORTEPARTO = ifelse(MORTEPARTO == 9, NA, MORTEPARTO),
    ESC2010    = ifelse(ESC2010 == 9, NA, ESC2010),
    IDADE      = ifelse(IDADE == 999, NA, IDADE))

# Tarefa 6. Atribuir legendas para as categorias das variáveis qualitativas investigadas na tarefa 4.
# Exemplo: dados_sim_2$TIPOBITO = factor(dados_sim_2$TIPOBITO, levels = c(1,2), labels = c("Fetal", "Não fetal")

# ATENçÃO: 1. Na hora de escrever os labels, somente a primeira letra da palavra é maiúscula. Exemplo para SEXO: Feminino e Masculino
#          2. Nesta Tarefa 6 não crie novas variáveis no banco de dados

dados_sim_2$TIPOBITO = factor(dados_sim_2$TIPOBITO, 
                               levels = c(1, 2), 
                               labels = c("Fetal", "Não fetal"))

dados_sim_2$SEXO = factor(dados_sim_2$SEXO, 
                           levels = c(1, 2), 
                           labels = c("Masculino", "Feminino"))

dados_sim_2$RACACOR = factor(dados_sim_2$RACACOR, 
                              levels = c(1, 2, 3, 4, 5), 
                              labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))

dados_sim_2$TPMORTEOCO = factor(dados_sim_2$TPMORTEOCO, 
                                 levels = c(1, 2, 3, 4, 5, 8), 
                                 labels = c("Na gravidez", 
                                            "No parto", 
                                            "No abortamento", 
                                            "Até 42 dias após o término do parto", 
                                            "De 43 dias a 1 ano após o término da gestação", 
                                            "Não ocorreu nestes períodos"))

dados_sim_2$OBITOGRAV = factor(dados_sim_2$OBITOGRAV, 
                                levels = c(1, 2), 
                                labels = c("Sim", "Não"))

dados_sim_2$OBITOPUERP = factor(dados_sim_2$OBITOPUERP, 
                                 levels = c(1, 2, 3), 
                                 labels = c("Sim, até 42 dias após o parto", 
                                            "Sim, de 43 dias a 1 ano", 
                                            "Não"))

dados_sim_2$TPOBITOCOR = factor(dados_sim_2$TPOBITOCOR, 
                                 levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9), 
                                 labels = c("Durante a gestação", 
                                            "Durante o abortamento", 
                                            "Após o abortamento", 
                                            "No parto ou até 1 hora após o parto", 
                                            "No puerpério - até 42 dias após o parto", 
                                            "Entre 43 dias e até 1 ano após o parto", 
                                            "A investigação não identificou o momento do óbito", 
                                            "Mais de um ano após o parto", 
                                            "O óbito não ocorreu nas circunstancias anteriores"))

dados_sim_2$MORTEPARTO = factor(dados_sim_2$MORTEPARTO, 
                                 levels = c(1, 2, 3), 
                                 labels = c("Antes", "Durante", "Após"))

# Tarefa 7. Crie um banco de dados, de nome SIM_UF.csv (Exemplo: SIM_RJ.csv), contendo as 41 variáveis listadas no arquivo “Variáveis - Projeto - Tarefa 7 da Etapa 2.pdf”
# Atenção:
# 1. Para informações gerais utilize CAUSABAS, SEXO e IDADE
# 2. Para informações fetais utilize TIPOBITO
# 3. Para informações neonatais utilize TIPOBITO não fetal e IDADE entre 0 e 27 dias e RACACOR
# 4. Para informações maternas utilize TPMORTEOCO, ESC e IDADE

library(stringr)

dados_sim_2 = dados_sim_2 |>
  mutate(IDADE = as.numeric(IDADE))

torc_calculado = dados_sim |>
  filter(substr(as.character(CODMUNRES), 1, 2) == "31") |>
  group_by(CODMUNRES) |>
  summarise(TORC = sum(complete.cases(pick(everything())))) |>
  ungroup()

SIM_MG = dados_sim_2 |>
  group_by(CODMUNRES) |>
  summarise(
    ANO = 2015,
    NIVEL = "MUNICIPIO",
    TO = n(),
    TORCR = sum(complete.cases(pick(TIPOBITO, DTOBITO, DTNASC, IDADE, SEXO, RACACOR, ESC2010, TPMORTEOCO, OBITOGRAV, OBITOPUERP, CAUSABAS, TPOBITOCOR, MORTEPARTO))),
    TO_NN = sum(substr(CAUSABAS, 1, 1) %in% c("V", "W", "X", "Y"), na.rm = TRUE),
    TO_N  = sum(!substr(CAUSABAS, 1, 1) %in% c("V", "W", "X", "Y"), na.rm = TRUE),
    TO_CB_I = sum(substr(CAUSABAS, 1, 1) %in% c("A", "B"), na.rm = TRUE),
    TO_CB_N = sum(substr(CAUSABAS, 1, 1) %in% c("C", "D"), na.rm = TRUE),
    TO_CB_C = sum(substr(CAUSABAS, 1, 1) == "I", na.rm = TRUE),
    TO_CB_R = sum(substr(CAUSABAS, 1, 1) == "J", na.rm = TRUE),
    TO_CB_O = sum(!substr(CAUSABAS, 1, 1) %in% c("A", "B", "C", "D", "I", "J", "V", "W", "X", "Y"), na.rm = TRUE),
    TO_M = sum(SEXO == "Masculino", na.rm = TRUE),
    TO_F = sum(SEXO == "Feminino", na.rm = TRUE),
    TO_F_IF = sum(SEXO == "Feminino" & IDADE >= 415 & IDADE <= 449, na.rm = TRUE),
    TO_FT = sum(TIPOBITO == "Fetal", na.rm = TRUE),
    TO_NT = sum(TIPOBITO == "Não fetal" & (IDADE < 123 | (IDADE >= 200 & IDADE <= 227)), na.rm = TRUE),
    TO_NT_P = sum(TIPOBITO == "Não fetal" & (IDADE < 123 | (IDADE >= 200 & IDADE <= 206)), na.rm = TRUE),
    TO_NT_T = sum(TIPOBITO == "Não fetal" & (IDADE >= 207 & IDADE <= 227), na.rm = TRUE),
    TO_PNT = sum(TIPOBITO == "Não fetal" & ((IDADE >= 228 & IDADE <= 229) | (IDADE >= 301 & IDADE <= 311)), na.rm = TRUE),
    TO_MT_G = sum(TPMORTEOCO == "Na gravidez", na.rm = TRUE),
    TONT_B  = sum(TIPOBITO == "Não fetal" & (IDADE < 123 | (IDADE >= 200 & IDADE <= 227)) & RACACOR == "Branca", na.rm = TRUE),
    TONT_PT = sum(TIPOBITO == "Não fetal" & (IDADE < 123 | (IDADE >= 200 & IDADE <= 227)) & RACACOR == "Preta", na.rm = TRUE),
    TONT_A  = sum(TIPOBITO == "Não fetal" & (IDADE < 123 | (IDADE >= 200 & IDADE <= 227)) & RACACOR == "Amarela", na.rm = TRUE),
    TONT_PD = sum(TIPOBITO == "Não fetal" & (IDADE < 123 | (IDADE >= 200 & IDADE <= 227)) & RACACOR == "Parda", na.rm = TRUE),
    TONT_I  = sum(TIPOBITO == "Não fetal" & (IDADE < 123 | (IDADE >= 200 & IDADE <= 227)) & RACACOR == "Indígena", na.rm = TRUE),
    TO_MT = sum(TPMORTEOCO %in% c("Na gravidez", "No parto", "No abortamento", "Até 42 dias após o término do parto", "De 43 dias a 1 ano após o término da gestação"), na.rm = TRUE),
    TO_MT_DG = sum(TPMORTEOCO == "Na gravidez", na.rm = TRUE),
    TO_MT_PT = sum(TPMORTEOCO == "No parto", na.rm = TRUE),
    TO_MT_AB = sum(TPMORTEOCO == "No abortamento", na.rm = TRUE),
    TO_MT_42 = sum(TPMORTEOCO == "Até 42 dias após o término do parto", na.rm = TRUE),
    TO_MT_43 = sum(TPMORTEOCO == "De 43 dias a 1 ano após o término da gestação", na.rm = TRUE),
    TO_MT_P = sum(TPMORTEOCO %in% c("Na gravidez", "No parto", "No abortamento", "Até 42 dias após o término do parto"), na.rm = TRUE),
    TO_MT_P_I = sum(TPMORTEOCO %in% c("Na gravidez", "No parto", "No abortamento", "Até 42 dias após o término do parto") & SEXO == "Feminino" & IDADE >= 415 & IDADE <= 449, na.rm = TRUE),
    TO_MT_P_ES   = sum(TPMORTEOCO %in% c("Na gravidez", "No parto", "No abortamento", "Até 42 dias após o término do parto") & ESC2010 == 0, na.rm = TRUE),
    TO_MT_P_EFI  = sum(TPMORTEOCO %in% c("Na gravidez", "No parto", "No abortamento", "Até 42 dias após o término do parto") & ESC2010 == 1, na.rm = TRUE),
    TO_MT_P_EFII = sum(TPMORTEOCO %in% c("Na gravidez", "No parto", "No abortamento", "Até 42 dias após o término do parto") & ESC2010 == 2, na.rm = TRUE),
    TO_MT_P_EM   = sum(TPMORTEOCO %in% c("Na gravidez", "No parto", "No abortamento", "Até 42 dias após o término do parto") & ESC2010 == 3, na.rm = TRUE),
    TO_MT_P_ESI  = sum(TPMORTEOCO %in% c("Na gravidez", "No parto", "No abortamento", "Até 42 dias após o término do parto") & ESC2010 == 4, na.rm = TRUE),
    TO_MT_P_ESC  = sum(TPMORTEOCO %in% c("Na gravidez", "No parto", "No abortamento", "Até 42 dias após o término do parto") & ESC2010 == 5, na.rm = TRUE)
  ) |>
  ungroup() |>
  left_join(torc_calculado, by = "CODMUNRES") |>
  relocate(TORC, .after = TO)

# Tarefa 8: Exporte o banco de dados com o nome SIM_UF.csv

write_csv2(SIM_MG, "SIM_MG.csv")

# Ao terminar a ETAPA 2 commite e envie para o repositório REMOTO com o comentário "Dados da UF e Script Etapa 2"
# Faça um merge de script de SIM para main

#####################################################
# ETAPA 3: OUTROS BANCOS DE DADOS: IBGE, SNIS, ...
#####################################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 3

# Tarefa 1. Acesso aos bancos de dados e obtenção da informação



#####################################################################################################
# ETAPA 4: GERAR BANCO DE DADOS FINAL DO ESTADO, BASEADO NAS ANÁLISES DE SINASC, SIM, IBGE, SNIS,...
######################################################################################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 4

# Cada aluno gerar um dataframe de uma única linha (referente ao seu estado) com as variáveis na ordem indicada pela professora



############################################################################################
# ETAPA 5: EMPILHAMENTO DOS DATAFRAMES DE CADA ESTADO, GERANDO UM DATAFRAME DE 27 LINHAS
############################################################################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 5

# 1. Enviar arquivos para as pastas do repositório da Professora no GitHUb
# 2. A professora fará o empilhamentos dos dataframes

