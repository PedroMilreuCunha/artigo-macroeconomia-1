## -- Pedro Milreu Cunha -- ##
## -- Mestrando em Economia Aplicada pelo PPGE/UFPB -- ##

#### Bibliotecas ####

library(readxl)
library(mFilter)
library(stargazer)
library(ggplot2)
library(strucchange)
library(dplyr)
library(systemfit)
library(nlWaldTest)
library(gridExtra)
library(zoo)
library(dyn)
library(sandwich)

#### Lendo os dados ####

serie_emprego_mensal <- read.csv2("Dados iniciais/emprego_ibge_mensal.csv", sep = ",")
serie_emprego_mensal <- serie_emprego_mensal[,-3]
colnames(serie_emprego_mensal) <- c("Data", "Pessoas ocupadas")
str(serie_emprego_mensal)

serie_desemprego_mensal <- read.csv2("Dados iniciais/desemprego_ibge_mensal.csv", sep = ",")
serie_desemprego_mensal <- serie_desemprego_mensal[,-3]
colnames(serie_desemprego_mensal) <- c("Data", "% desocupação")
str(serie_desemprego_mensal)
serie_desemprego_mensal$`% desocupação` <- as.numeric(serie_desemprego_mensal$`% desocupação`)

serie_pib_trimestral <- read.csv2("Dados iniciais/pib_ibge_trimestral.csv", sep = ",")
serie_pib_trimestral <- serie_pib_trimestral[,-3]
colnames(serie_pib_trimestral) <- c("Data", "PIB")
str(serie_pib_trimestral)
serie_pib_trimestral$PIB <- as.numeric(serie_pib_trimestral$PIB)
Data <- serie_pib_trimestral$Data
serie_pib_trimestral <- ts(serie_pib_trimestral$PIB, 
                           start = c(2000,1), end = c(2020,3), freq = 4)

#### Convertendo as séries de emprego e desemprego para a frequência trimestral ####

temp1 <- ts(serie_emprego_mensal$`Pessoas ocupadas`, start = c(2012,3), end = c(2020,10),freq = 12)
temp1 <- window(temp1, start = c(2013,1), end = c(2019,12))
serie_emprego_trimestral <- aggregate(temp1, nfrequency = 4, mean)

temp2 <- ts(serie_desemprego_mensal$`% desocupação`, start = c(2012,3), end = c(2020,10),freq = 12)
temp2 <- window(temp2, start = c(2013,1), end = c(2019,12))
serie_desemprego_trimestral <- aggregate(temp2, nfrequency = 4, mean)

#### Deixando apenas a janela de tempo 2013-T1:2019-T4 nas séries  ####

serie_pib_trimestral <- window(serie_pib_trimestral,
                               start = c(2013,1), end = c(2019,4))
temp3 <- as.data.frame(serie_pib_trimestral)

Data <- Data[-c(1:52, 81:83)]

#### Criando o data.frame das séries unidas ####

serie_temporal <- as.data.frame(cbind(ur = serie_desemprego_trimestral,
                                      employ = serie_emprego_trimestral,
                                      gdp = serie_pib_trimestral)) 
serie_temporal <- cbind(Data, serie_temporal)

#write.csv(serie_temporal, "Dados trabalhados/serie_temporal_trimestral-2013-1-2019-4.csv")

#### Criando o GDP e o emprego em log x 100 ####

serie_temporal$ln_GDP_100 <- 100*log(serie_temporal$gdp)
serie_temporal$ln_EMP_100 <- 100*log(serie_temporal$employ)

stargazer(serie_temporal[,-c(1,3,4)], type = "text")


#### Criando as variáveis em diferença ####

serie_em_diferencas <- data.frame(GDP = seq(1,30), UR = seq(1,30))

i <- 1

while (i <= 27) {
  
  serie_em_diferencas$GDP[i] = serie_temporal$ln_GDP_100[i+1] - serie_temporal$ln_GDP_100[i]
  serie_em_diferencas$UR[i] = serie_temporal$ur[i+1] - serie_temporal$ur[i]
  i = i+1
}

#---- a) Tratando como série temporal #----

serie_em_diferencas <- ts(data = serie_em_diferencas,
                          start = c(2013,1),
                          end = c(2019,4), frequency = 4)

#### 1ª forma - Filtro HP ####

#---- a) Output, employment e unemployment gap #----

output_gap_1600 <- hpfilter(serie_temporal$ln_GDP_100, type = "lambda", freq = 1600)$cycle
employment_gap_1600 <- hpfilter(serie_temporal$ln_EMP_100, type = "lambda", freq = 1600)$cycle
unemployment_gap_1600 <- hpfilter(serie_temporal$ur, type = "lambda", freq = 1600)$cycle

output_gap_16000 <- hpfilter(serie_temporal$ln_GDP_100, type = "lambda", freq = 16000)$cycle
employment_gap_16000 <- hpfilter(serie_temporal$ln_EMP_100, type = "lambda", freq = 16000)$cycle
unemployment_gap_16000 <- hpfilter(serie_temporal$ur, type = "lambda", freq = 16000)$cycle

#---- b) Tratando como séries temporais #----

serie_hp_1600 <- cbind(output_gap = output_gap_1600,
                       employment_gap = employment_gap_1600,
                       unemployment_gap = unemployment_gap_1600)
serie_hp_1600 <- ts(data = serie_hp_1600,
                    start = c(2013,1),
                    end = c(2019,4), frequency = 4)

serie_hp_16000 <- cbind(output_gap = output_gap_16000,
                        employment_gap = employment_gap_16000,
                       unemployment_gap = unemployment_gap_16000)
serie_hp_16000 <- ts(data = serie_hp_16000,
                     start = c(2013,1),
                     end = c(2019,4), frequency = 4)

#---- b.1) Gráficos das decomposições para lambda = 1600 #----

#---- b.1.1) Emprego #----

temp <- with(serie_temporal,
             data.frame(Data = Data,
                        Emprego = employ,
                        Tendência = hpfilter(employ, type = "lambda", freq = 1600)$trend,
                        Cíclica = employment_gap_1600))

g1 <- ggplot(data = temp)+
  geom_line(aes(x = Data, y = Emprego), col = "blue", group = 1)+
  geom_line(data = temp, aes (x = Data, y = Tendência), col = "red", linetype = "dashed", group = 1)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

g2 <- ggplot(data = temp)+
  geom_line(data = temp, aes(x = Data, y = Cíclica), group = 1)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

grid.arrange(g1,g2, widths = c(2,2)) 


#---- b.1.2) PIB #----

temp <- with(serie_temporal,
             data.frame(Data = Data,
                        PIB = ln_GDP_100,
                        Tendência = hpfilter(serie_temporal$ln_GDP_100, type = "lambda", freq = 1600)$trend,
                        Cíclica = output_gap_1600))

g3 <- ggplot(data = temp)+
  geom_line(aes(x = Data, y = PIB), col = "blue", group = 1)+
  geom_line(data = temp, aes (x = Data, y = Tendência), col = "red", linetype = "dashed", group = 1)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

g4 <- ggplot(data = temp)+
  geom_line(data = temp, aes(x = Data, y = Cíclica), group = 1)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

grid.arrange(g3,g4, widths = c(2,2)) 

#---- b.1.3) Taxa de desemprego #----

temp_2 <- with(serie_temporal,
               data.frame(Data = Data,
                          Desemprego = ur,
                          Tendência = hpfilter(ur, type = "lambda", freq = 1600)$trend,
                          Cíclica = unemployment_gap_1600))

g5 <- ggplot(data = temp_2)+
  geom_line(aes(x = Data, y = Desemprego), col = "blue", group = 1)+
  geom_line(data = temp_2, aes (x = Data, y = Tendência), col = "red", linetype = "dashed", group = 1)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

g6 <- ggplot(data = temp_2)+
  geom_line(data = temp_2, aes(x = Data, y = Cíclica), group = 1)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

grid.arrange(g5,g6, widths = c(2,2)) 


#---- c) Estimando a primeira versão da Lei de Okun #----

 ## Criando os lags do produto e a base para estimação
  
 L1_output_gap <- lag(as.data.frame(serie_hp_1600)$output_gap, 1)
 L1_output_gap <- L1_output_gap[-c(1,2)]
 L2_output_gap <- lag(as.data.frame(serie_hp_1600)$output_gap, 2)
 L2_output_gap <- L2_output_gap[-c(1,2)]
 L1_employment_gap <- lag(as.data.frame(serie_hp_1600)$employment_gap, 1)
 L1_employment_gap <- L1_employment_gap[-c(1,2)]
 L2_employment_gap <- lag(as.data.frame(serie_hp_1600)$employment_gap, 2)
 L2_employment_gap <- L2_employment_gap[-c(1,2)]
 L1_unemployment_gap <- lag(as.data.frame(serie_hp_1600)$unemployment_gap, 1)
 L1_unemployment_gap <- L1_unemployment_gap[-c(1,2)]
 L2_unemployment_gap <- lag(as.data.frame(serie_hp_1600)$unemployment_gap, 2)
 L2_unemployment_gap <- L2_unemployment_gap[-c(1,2)] 
 base_reg <- cbind(as.data.frame(serie_hp_1600)[-c(1,2),],
                   L1_output_gap, L2_output_gap)

 ## Realizando as estimações 
 
 estimacao_1 <-lm(data = serie_hp_1600, 
                  unemployment_gap ~ output_gap+0)
 c1 <- coeftest(estimacao_1, vcov = vcovHC(estimacao_1, type = "HC0"))

 se_1_robust <- c1[,2]
 pvalue_1_robust <- c1[,4]
 
 estimacao_1_lag <- lm(data = base_reg,
                      unemployment_gap ~ output_gap + L1_output_gap
                                                    + L2_output_gap+0)
 c1_lag <- coeftest(estimacao_1_lag, vcov = vcovHC(estimacao_1_lag, type = "HC0"))
 
 se_1_lag_robust <- c1_lag[,2]
 pvalue_1_lag_robust <- c1_lag[,4]
 
stargazer(estimacao_1, estimacao_1_lag, type = "text",
          se = list(se_1_robust, se_1_lag_robust),
          p = list(pvalue_1_robust, pvalue_1_lag_robust))

estimacao_2 <-lm(data = serie_hp_16000, 
                 unemployment_gap ~ output_gap+0)

c2 <- coeftest(estimacao_2, vcov = vcovHC(estimacao_2, type = "HC0"))

se_2_robust <- c2[,2]
pvalue_2_robust <- c2[,4]

estimacao_2_lag <- dyn$lm(data = serie_hp_16000,
                          unemployment_gap ~ output_gap + lag(output_gap,1)
                          + lag(output_gap,2)+0)

c2_lag <- coeftest(estimacao_2_lag, vcov = vcovHC(estimacao_2_lag, type = "HC0"))

se_2_lag_robust <- c2_lag[,2]
pvalue_2_lag_robust <- c2_lag[,4]

stargazer(estimacao_2, estimacao_2_lag, type = "text",
          se = list(se_2_robust, se_2_lag_robust),
          p = list(pvalue_2_robust, pvalue_2_lag_robust))

#---- d) Testando a estabilidade da estimação com lambda = 1600 #----

base_reg <- ts(base_reg, start = c(2013,3), end = c(2019,4), frequency = 4)

temp_bp <- breakpoints(data = base_reg, unemployment_gap ~ output_gap + L1_output_gap +
                                                           L2_output_gap+0,
                       format.times = TRUE, h = 0.45)
summary(temp_bp)

fstats <- Fstats(data = base_reg, unemployment_gap ~ output_gap+L1_output_gap+
                                                     L2_output_gap+0)
plot(fstats)

#### Criação dos gráficos - versão sem lag  ####

df <- as.data.frame(serie_hp_1600)

ggplot(data = df)+
  geom_point(data = df, aes(y = unemployment_gap, x = output_gap))+
  geom_hline(yintercept = 0, color = "blue", linetype = "dashed", size = 0.25)+ 
  geom_vline(xintercept = 0, color = "blue", linetype = "dashed", size = 0.25)+ 
  geom_abline(slope = estimacao_1$coefficients, intercept = 0,
              show.legend = TRUE, color = "red")+
  labs(x = "Hiato do produto", y = "Hiato do desemprego")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#---- 2ª forma - Versão em diferenças #----

colnames(serie_em_diferencas) <- c("output", "unemployment")
estimacao_3_lag <- dyn$lm(data = serie_em_diferencas,
                      unemployment ~ output + lag(output,1)
                                     + lag(output,2))

stargazer(estimacao_1_lag, estimacao_3_lag, type = "text")

#### Sistema de equações - Estimação SUR - 2013 - 2019 ####

#---- a) Fórmulas #----

r1 <- employment_gap ~ output_gap + L1_output_gap + L2_output_gap + 0
r2 <- unemployment_gap ~ employment_gap + L1_employment_gap + L2_employment_gap + 0
r3 <- unemployment_gap ~ output_gap + L1_output_gap + L2_output_gap + 0

#---- b) Estimação SUR #----

fit_sur <- systemfit(list(r1 = r1, r2 = r2, r3 = r3), method = "SUR",
                       data = base_reg)
summary(fit_sur) 

#---- c) Teste da hipótese de que beta = gamma * delta #---- 

restricao <- "b[1]*b[2] = b[3]"

teste_1 <- nlWaldtest(fit_sur, 
                      texts =  restricao)


