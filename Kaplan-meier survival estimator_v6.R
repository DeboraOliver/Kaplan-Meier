
library(redcapAPI)
library(dplyr)
library(plyr)
library(tidyr)
library(stats)
library(survival)
library(ggplot2)
library(survminer)
library(GGally)
library(utils)

rm(list=ls())


#########
#conectando ao redcapAPI
#########

source("token.txt")

rcon <- redcapConnection(url=url, token=token)

rm(token)

##########
#Chamando as variáveis
##########

#chamando os campos básicos como: data de entrada no prog, motivo de saida e data da morte

vetor_id <- c("cadast_same", "cadast_primeiro_sn", "cadast_data_programa", "contato_respondido_sn", "contato_data", "icc_termino_follow_up", "icc_term_follow_up_mot", "icc_term_follow_up_data", "icc_term_fup_obito_data")
estratificando  <- c("cadast_genero" ,"smoke", "alcohol","hyperten","diab","icc_pd_fibri_atrial_sn","icc_pd_iam_sn" ,"icc_pd_adm_dpoc_sn" ,"icc_pd_adm_irc_cronica_sn","icc_pd_adm_irc_aguda_sn"        ,"icc_pd_adm_irc_dialise_sn" ,"icc_pd_adm_disp_card_sn"        ,"icc_pd_adm_disp_card_cdi"  ,"icc_pd_adm_disp_card_trc" ,"icc_pd_adm_disp_card_marc","icc_pd_adm_cir_card_sn"   ,"icc_pd_adm_trat_percuta_sn"   ,"icc_pd_adm_trat_perc_icp" ,"icc_pd_adm_trat_perc_tavi","icc_pd_adm_trat_perc_mitra" ,"pd_adm_cl_med_aas"  ,"pd_adm_cl_med_ant_recep_aldo" ,"pd_adm_cl_med_antiarr"  ,"pd_adm_cl_med_anticoag" ,"pd_adm_cl_med_antiep"  ,"pd_adm_cl_med_arni"   ,"pd_adm_cl_med_b_bloq"     ,"pd_adm_cl_med_bloq_calcio","pd_adm_cl_med_bloq_bra","pd_adm_cl_med_clo_tica_pra","pd_adm_cl_med_dogixina"  ,"pd_adm_cl_med_diuretico"   ,"pd_adm_cl_med_hidra","pd_adm_cl_med_ieca"   ,"pd_adm_cl_med_ivabradina"  ,"pd_adm_cl_med_vaso_nit"   ,"icc_pd_adm_edem_agd_pulmao"  ,"icc_pd_adm_chq_cardiogenic" ,"icc_pd_hos_infec_hosp_sn" ,"icc_pd_hop_irc_creatinina_48h"  ,"icc_pd_hop_irc_creatinina_7dias"  ,"icc_pd_hop_irc_sn","icc_pd_hop_irc_dial_sn"     ,"icc_pd_hos_disp_card_sn" ,"icc_pd_hos_disp_card_cdi" ,"icc_pd_hos_disp_card_trc" ,"icc_pd_hos_dav_sn" ,"icc_pd_hosp_cir_card_sn"  ,"icc_pd_hosp_trat_percut_sn" ,"icc_pd_hosp_trat_perc_icp"  ,"icc_pd_hosp_ed_agd_pulmao"  ,"icc_pd_hosp_chq_cardiog","icc_pd_compldevice","pd_alta_cl_med_aas" ,"pd_alta_cl_med_ant_recep_aldo" ,"pd_alta_cl_med_antiarr"  ,"pd_alta_cl_med_anticoag"  ,"pd_alta_cl_med_antiep"     ,"pd_alta_cl_med_arni"  ,"pd_alta_cl_med_b_bloq", "pd_alta_cl_med_bloq_bra","pd_alta_cl_med_clo_tica_pra"  ,"pd_alta_cl_med_dogixina"  ,"pd_alta_cl_med_diuretico"  ,"pd_alta_cl_med_hidra"  ,"pd_alta_cl_med_ieca" ,"pd_alta_cl_med_ivabradina"  ,"pd_alta_cl_med_vaso_nit"   ,"drg_severidade_admissao"  ,"drg_risc_mortal_admissao"  ,"drg_severidade_alta")
evento1 <-  c("t0_arm_1", "72_horas_arm_1","30_dias_arm_1", "6_meses_arm_1", "1_ano_arm_1" ,"1_ano_e_meio_arm_1", "2_anos_arm_1","2_anos_e_meio_arm_1", "3_anos_arm_1","3_anos_e_meio_arm_1", "4_anos_arm_1", "4_anos_e_meio_arm_1","5_anos_arm_1" , "5_anos_e_meio_arm_1", "6_anos_arm_1","6_anos_e_meio_arm_1" ,"7_anos_arm_1" ,"7_anos_e_meio_arm_1", "8_anos_arm_1" , "8_anos_e_meio_arm_1", "9_anos_arm_1",  "9_anos_e_meio_arm_1", "10_anos_arm_1") 
evento2 <-  c("t0_arm_1")

data_base_all <- exportRecords(rcon,  factors = FALSE,
                        fields = c("record_id", paste(vetor_id)),
                        events = evento1, dates = TRUE)

#vamos selecionar apenas as colunas que vamos usar

data_base_all <- data_base_all %>% select("record_id", vetor_id)

data_base_num <- mutate_all(data_base_all, funs(as.numeric),1:length(data_base_all))

data_base_num$icc_term_follow_up_mot[is.na(data_base_num$icc_term_follow_up_mot)] <- 666

data_base_num[is.na(data_base_num)] <- 0


#vamos aplicar as regras

#REGRA1: SE cadast_primeiro_sn = 0 NÃO PEGAR a cadast_data
#REGRA2: SE contato_respondido_sn = 0 NÃO PEGAR contato_data
#REGRA3: não mapear SE contato_data=0 AND icc_follow_up_data=0 AND icc_term_fub_obito_data = 0
#REGRA4: se o paciente reentrar zeramos a follow_up_data até isso ser diferente de reentrada 20
#REGRA5: pacientes em seguimento icc_termino_follow_up


data_base_num$cadast_data_programa <- ifelse(data_base_num$cadast_primeiro_sn == 0, 0, data_base_num$cadast_data_programa)
data_base_num$icc_term_follow_up_data <- ifelse(data_base_num$icc_term_follow_up_mot!=20, data_base_num$icc_term_follow_up_data, 0)
data_base_num$contato_respondido_sn <- ifelse(data_base_num$icc_term_follow_up_mot==20, 0, data_base_num$contato_respondido_sn)
data_base_num$contato_data <- ifelse(data_base_num$contato_respondido_sn == 0, 0, data_base_num$contato_data) 
 
#vamos olhar por RECORD_ID e identificar o último contato
group_by_record <- ddply(data_base_num, ~record_id , summarise,
               cadast_same = max(cadast_same),
               cadast_data_programa = max(cadast_data_programa),
               contato_data = max(contato_data),
               icc_term_follow_up_mot = min(icc_term_follow_up_mot),
               icc_term_follow_up_data = max(icc_term_follow_up_data),
               icc_term_fup_obito_data=max(icc_term_fup_obito_data))


#pelo caso 946606
group_by_record$contato_data <- ifelse(group_by_record$icc_term_follow_up_mot==20, 0, group_by_record$contato_data)

#um mesmo same pode ter varios record_ids, então vamos filtrar por same
group_same <- ddply(group_by_record, ~cadast_same, summarise,  
                 cadast_data_programa = max(cadast_data_programa),
                 contato_data = max(contato_data),
              icc_term_follow_up_mot = min(icc_term_follow_up_mot),
                 icc_term_follow_up_data = max(icc_term_follow_up_data),
                 icc_term_fup_obito_data=max(icc_term_fup_obito_data))

#vamos criar uma coluna com o maior valor entre "contato_data", "icc_term_follow_up_data", "icc_term_fup_obito_data"
group_same$recent_date <- ifelse(group_same$icc_term_fup_obito_data != 0, group_same$icc_term_fup_obito_data, 
                            pmax(group_same$contato_data, group_same$icc_term_follow_up_data, group_same$icc_term_fup_obito_data))

#observar <- readline("O que vamos observar? 0 - óbitos ou 20 - Reinternações?")
observar <- 0
group_same$censored <- ifelse(group_same$icc_term_follow_up_mot == observar, 1, 0)

group_same_clean <- group_same %>% select(cadast_same, cadast_data_programa, recent_date, icc_term_follow_up_mot, censored)

group_same_clean$recent_date <- ifelse(group_same_clean$recent_date == 0, NA, group_same_clean$recent_date)

group_same_clean <- group_same_clean[complete.cases(group_same_clean), ]

group_same_clean$recent_date <- as.Date(as.POSIXct(group_same_clean$recent_date, tz = "GMT", origin = "1970-01-01", format = "%Y-%m-%d"), "Y%m%d")
group_same_clean$cadast_data_programa  <- as.Date(as.POSIXct(group_same_clean$cadast_data_programa , tz = "GMT", origin = "1970-01-01", format = "%Y-%m-%d"), "Y%m%d")


group_same_clean$dias_vivo <- group_same_clean$recent_date - group_same_clean$cadast_data_programa




#############
#RECOMENDAÇÃO DO ISAC
#############
#mediana da nossa trial: usar os 10 anos ou a mediana do máximo de dias vivo?
# group_same_clean <- group_same_clean[complete.cases(group_same_clean), ]
# 
# valor_max_dias <- max(group_same_clean$dias_vivo, na.rm=TRUE)

 median_days          <- median(group_same_clean$dias_vivo, na.rm=TRUE)
median_days <- as.numeric(median_days)




#criando variaveis para receber o p_valor

p_value_names            <- vector(mode = "character", length = 0)
p_value_value            <- vector(mode = "numeric", length = 0)
p_value_char             <- vector(mode = "character", length = 0)
graphs				<- list()

#########
#VARIAVEIS PARA COMPARAR
for(i in 1:length(estratificando)) {

  print(i)

  vetor_compara <- estratificando[i] 

compara <- exportRecords(rcon,  factors = FALSE,
                      fields = c("cadast_same", paste(vetor_compara)),
                      events = evento2, dates = TRUE)

compara <- compara %>% select(cadast_same, paste(vetor_compara)) %>% group_by(cadast_same) %>% slice(1L)

data_compare <- merge(group_same_clean, compara, by.x="cadast_same", by.y= "cadast_same")
data_compare <- na.omit(data_compare)

 ################
#KAPLAN-MEIER
################

icc.fit<- survfit(Surv(data_compare$dias_vivo, data_compare$censored)~data_compare[,7], data= data_compare)

p_value <- surv_pvalue(icc.fit, data= data_compare, method = "n")


if (p_value$pval < 0.001) {
  
  p_value_names            <-  append(p_value_names, paste(vetor_compara))
  p_value_value            <-  append(p_value_value, paste(p_value$pval))
  p_value_char             <-  append(p_value_char, paste(p_value$pval.txt))
  
  
  graphs[[i]]  <- ggsurvplot(icc.fit, fun = NULL, color = "strata", palette =  "hue", linetype = 1, 
                             break.time.by = NULL, surv.scale = c("percent"), 
                             conf.int = TRUE, conf.int.fill = "strata", censor = TRUE, pval = TRUE, 
                             pval.size = TRUE, pval.coord = c(NULL, NULL), main = paste(vetor_compara), xlab = "Time", 
                             ylab = "Survival probability", font.main = c(16, "plain", "black"), 
                             font.x = c(14, "plain", "black"), font.y = c(14, "plain", "black"), 
                             font.tickslab = c(12, "plain", "black"), xlim = NULL, ylim = NULL, 
                             legend = c("right"), legend.title = " ", 
                             legend.labs = NULL, font.legend = c(10, "plain", "black"), risk.table = TRUE, 
                             risk.table.title = "Number at risk by time", risk.table.col = "strata", 
                             risk.table.fontsize = 4.5, risk.table.y.text = TRUE, risk.table.y.text.col = TRUE, 
                             risk.table.height = 0.25, surv.plot.height = 0.75, ggtheme = theme_minimal())
  # arrange_ggsurvplots(splots, print = TRUE,
  #                     ncol = 2, nrow = 1, risk.table.height = 0.4)
  # 
  
}
}

final_result <- data.frame(p_value_names,p_value_value, p_value_char)




#Não consigo ver o p.value

#O Teste de Log-Rank só pode ser aplicado a taxas de mortalidade maiores de 10% abaixo disto o teste a ser aplicado é o de Breslow.
 #numero_censored <- sum(group_same_clean$censored == 1, na=TRUE)
  # proportion_censored <- (numero_censored / (numero_censored + sum(group_same_clean$censored ==0, na.rm =TRUE)))

   #Estes testes comparam a diferença ponderada entre o número de eventos observados e o n de eventos esperados em cada momento do tempo
   #É comum que todos os testes levem a mesma conlusão, mas a escolha do teste a usar deve ser baseada nas diferenças que se esperam nas distribuições de sobrevivência
   #Estes testes testam a hipótese nula de que não há diferenças nas distribuições de sobrevivencia entre os grupos na população
   # se p.value é menor do que 0.05, podemos assumir que há diferenças de eficácia nos dois tratamentos,
   #Ou seja rejeita-se a hipotese nula de o evento ocorrer no mesmo prazo quando comparados os dois tipos de tratamentos

   
#################
#LOG-RANK Test
################
   
#Distribuicao esperada de eventos igual em todos os estratos

 #teste.fit<- survdiff(Surv(group_same_clean$dias_vivo, group_same_clean$censored) ~ group_same_clean$cadast_genero, data = group_same_clean, rho=0)
 #summary(teste.fit)









#separei os dados e agora faço um csv e subo para a migração de dados
#write.table(base, "kaplan.csv",  row.names = FALSE, na = "NA", sep = ",")