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

#### Lendo os dados ####

serie_temporal <- read_excel("Dados iniciais/serie_temporal_brasil.xlsx")
serie_temporal$Output <- as.numeric(serie_temporal$Output)
serie_temporal$Unemployment <- as.numeric(serie_temporal$Unemployment)
serie_temporal <- serie_temporal[,-4]

colnames(serie_temporal) <- c("year", "gdp", "ur")

#### Criando o GDP em log x 100 ####

serie_temporal$ln_GDP_100 <- 100*log(serie_temporal$gdp)

#### Criando as variáveis em diferença ####

serie_em_diferencas <- data.frame(GDP = seq(1,28), UR = seq(1,28))

i <- 1

while (i <= 28) {
  
  serie_em_diferencas$GDP[i] = serie_temporal$ln_GDP_100[i+1] - serie_temporal$ln_GDP_100[i]
  serie_em_diferencas$UR[i] = serie_temporal$ur[i+1] - serie_temporal$ur[i]
  i = i+1
}

#---- a) Tratando como série temporal #----

serie_em_diferencas <- ts(data = serie_em_diferencas, start = 1992, end = 2019, frequency = 1)


#### 1ª forma - Filtro HP ####

#---- a) Output gap #----

output_gap_100 <- hpfilter(serie_temporal$ln_GDP_100, type = "lambda", freq = 100)$cycle
unemployment_gap_100 <- hpfilter(serie_temporal$ur, type = "lambda", freq = 100)$cycle

output_gap_1000 <- hpfilter(serie_temporal$ln_GDP_100, type = "lambda", freq = 1000)$cycle
unemployment_gap_1000 <- hpfilter(serie_temporal$ur, type = "lambda", freq = 1000)$cycle

output_gap_1600 <- hpfilter(serie_temporal$ln_GDP_100, type = "lambda", freq = 1600)$cycle
unemployment_gap_1600 <- hpfilter(serie_temporal$ur, type = "lambda", freq = 1600)$cycle


#---- b) Tratando como séries temporais #----

serie_hp_100 <- cbind(output_gap = output_gap_100,
                      unemployment_gap = unemployment_gap_100)
serie_hp_100 <- ts(data = serie_hp_100, start = 1991, end = 2019, frequency = 1)

serie_hp_1000 <- cbind(output_gap = output_gap_1000,
                       unemployment_gap = unemployment_gap_1000)
serie_hp_1000 <- ts(data = serie_hp_1000, start = 1991, end = 2019, frequency = 1)

serie_hp_1600 <- cbind(output_gap = output_gap_1600,
                       unemployment_gap = unemployment_gap_1600)
serie_hp_1600 <- ts(data = serie_hp_1600, start = 1991, end = 2019, frequency = 1)


#---- b.1) Gráficos das decomposições para lambda = 1000 #----

#---- b.1.1) PIB #----

temp <- with(serie_temporal,
             data.frame(Ano = year,
                        PIB = ln_GDP_100,
                        Tendência = hpfilter(ln_GDP_100, type = "lambda", freq = 1000)$trend,
                        Cíclica = output_gap_1000))

g3 <- ggplot(data = temp)+
  geom_line(aes(x = Ano, y = PIB), col = "blue")+
  geom_line(data = temp, aes (x = Ano, y = Tendência), col = "red", linetype = "dashed")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

g4 <- ggplot(data = temp)+
  geom_line(data = temp, aes(x = Ano, y = Cíclica))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

grid.arrange(g3,g4, widths = c(2,2)) 

#---- b.1.2) Taxa de desemprego #----

temp_2 <- with(serie_temporal,
               data.frame(Ano = year,
               Desemprego = ur,
               Tendência = hpfilter(ur, type = "lambda", freq = 1000)$trend,
               Cíclica = unemployment_gap_1000))

g5 <- ggplot(data = temp_2)+
  geom_line(aes(x = Ano, y = Desemprego), col = "blue")+
  geom_line(data = temp_2, aes (x = Ano, y = Tendência), col = "red", linetype = "dashed")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

g6 <- ggplot(data = temp_2)+
  geom_line(data = temp_2, aes(x = Ano, y = Cíclica))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

grid.arrange(g5,g6, widths = c(2,2)) 


#---- c) Estimando a primeira versão da Lei de Okun #----

estimacao_1 <-lm(data = serie_hp_100, 
                 unemployment_gap ~ output_gap+0)

stargazer(estimacao_1, type = "text")

estimacao_2 <-lm(data = serie_hp_1000, 
                 unemployment_gap ~ output_gap+0)

stargazer(estimacao_2, type = "text")

estimacao_3 <- lm(data = serie_hp_1600,
                  unemployment_gap ~ output_gap+0)

#---- d) Testando a estabilidade da estimação com lambda = 1000 #----

temp <- serie_hp_1000
fs <- Fstats(unemployment_gap ~ output_gap+0, from = 0, data = serie_hp_1000)
plot(fs)

temp_bp <- breakpoints(data = temp, unemployment_gap ~ output_gap+0)
summary(temp_bp)

colnames(temp) <- c("Hiato do produto", "Hiato do desemprego")

plot(temp, main = "", xlab = "Ano", xaxp = c(1990,2020, 15))
lines(temp_bp, col = "red")     

#---- d.1) Regressões separadas para os períodos (1991-1998) e (1999-2019) #----

estimacao_pre_1998 <- lm(data = window(serie_hp_1000, start = 1991, end = 1998), 
                         unemployment_gap ~ output_gap+0)

estimacao_pos_1998 <- lm(data = window(serie_hp_1000, start = 1999, end = 2019),
                        unemployment_gap ~ output_gap+0)

stargazer(estimacao_pre_1998, estimacao_pos_1998, type = "text")

#### Criação dos gráficos ####

#---- a) 1991-2019 #----

df <- as.data.frame(serie_hp_1000)
colnames(df) <- c("output_gap", "unemployment_gap")

ggplot(data = df)+
  geom_point(data = df, aes(y = unemployment_gap, x = output_gap))+
  geom_hline(yintercept = 0, color = "blue", linetype = "dashed", size = 0.25)+ 
  geom_vline(xintercept = 0, color = "blue", linetype = "dashed", size = 0.25)+ 
  geom_abline(slope = estimacao_2$coefficients, intercept = 0,
              show.legend = TRUE, color = "red")+
  labs(x = "Hiato do produto", y = "Hiato do desemprego")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#---- b) 1991-1998 #----

df2 <- as.data.frame(window(serie_hp_1000, start = 1991, end = 1998))
colnames(df2) <- c("output_gap", "unemployment_gap")

g1 <- ggplot(data = df2)+
  geom_point(data = df2, aes(y = unemployment_gap, x = output_gap))+
  geom_hline(yintercept = 0, color = "blue", linetype = "dashed", size = 0.25)+ 
  geom_vline(xintercept = 0, color = "blue", linetype = "dashed", size = 0.25)+ 
  geom_abline(slope = estimacao_pre_1998$coefficients, intercept = 0,
              show.legend = TRUE, color = "red")+
  labs(x = "Hiato do produto", y = "Hiato do desemprego", subtitle = "1991-1998")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#---- c) 1999-2019 #----

df3 <- as.data.frame(window(serie_hp_1000, start = 1999, end = 2019))
colnames(df3) <- c("output_gap", "unemployment_gap")

g2 <- ggplot(data = df3)+
  geom_point(data = df3, aes(y = unemployment_gap, x = output_gap))+
  geom_hline(yintercept = 0, color = "blue", linetype = "dashed", size = 0.25)+ 
  geom_vline(xintercept = 0, color = "blue", linetype = "dashed", size = 0.25)+ 
  geom_abline(slope = estimacao_pos_1998$coefficients, intercept = 0,
              show.legend = TRUE, color = "red")+
  labs(x = "Hiato do produto", y = "Hiato do desemprego", subtitle = "1999-2019")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

grid.arrange(g1,g2, widths = c(2,2)) ## Plotando lado-a-lado

#---- 2ª forma - Versão em diferenças #----

estimacao_4 <- lm(data = serie_em_diferencas,
                  UR ~ GDP)

stargazer(estimacao_1, estimacao_2, estimacao_3, estimacao_4, type = "text")

#### Sistema de equações - Estimação SUR - 1991-2016 ####

serie_sur <- read_xlsx("Dados iniciais/sur_brasil.xlsx")
serie_sur$Unemployment <- as.numeric(serie_sur$Unemployment)

#---- a) Criando o GDP e Employment em log x 100 #----

serie_sur$ln_GDP_100 <- 100*log(serie_sur$Output)
serie_sur$ln_Employment_100 <- 100*log(serie_sur$Employment)

serie_sur <- serie_sur[, c(3,5,6)]

#---- b) Output, employment e unemployment gap #----

output_gap_1000_sur <- hpfilter(serie_sur$ln_GDP_100, type = "lambda", freq = 1000)$cycle
unemployment_gap_1000_sur <- hpfilter(serie_sur$Unemployment, type = "lambda", freq = 1000)$cycle
employment_gap_1000_sur <- hpfilter(serie_sur$ln_Employment_100, type = "lambda", freq = 1000)$cycle

output_gap_1600_sur <- hpfilter(serie_sur$ln_GDP_100, type = "lambda", freq = 1600)$cycle
unemployment_gap_1600_sur <- hpfilter(serie_sur$Unemployment, type = "lambda", freq = 1600)$cycle
employment_gap_1600_sur <- hpfilter(serie_sur$ln_Employment_100, type = "lambda", freq = 1600)$cycle

#---- b.1) Decomposição da série de emprego #----

temp_3 <- with(serie_sur,
               data.frame(Ano = seq(1991,2016,1),
                          Emprego = ln_Employment_100,
                          Tendência = hpfilter(ln_Employment_100, type = "lambda", freq = 1000)$trend,
                          Cíclica = employment_gap_1000_sur))

g7 <- ggplot(data = temp_3)+
  geom_line(aes(x = Ano, y = Emprego), col = "blue")+
  geom_line(data = temp_3, aes (x = Ano, y = Tendência), col = "red", linetype = "dashed")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

g8 <- ggplot(data = temp_3)+
  geom_line(data = temp_3, aes(x = Ano, y = Cíclica))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

grid.arrange(g7,g8, widths = c(2,2)) 


#---- c) Criando a série temporal #----
 
serie_hp_1000_sur <- cbind(output_gap = output_gap_1000_sur,
                           unemployment_gap = unemployment_gap_1000_sur,
                           employment_gap = employment_gap_1000_sur)
serie_hp_1000_sur <- ts(data = serie_hp_1000_sur, start = 1991, end = 2016, frequency = 1)
 
serie_hp_1600_sur <- cbind(output_gap = output_gap_1600_sur,
                           unemployment_gap = unemployment_gap_1600_sur,
                           employment_gap = employment_gap_1600_sur)
serie_hp_1600_sur <- ts(data = serie_hp_1600_sur, start = 1991, end = 2016, frequency = 1)


#---- d) Sistema de equações #----
 
r1 <- employment_gap ~ output_gap + 0
r2 <- unemployment_gap ~ employment_gap + 0
r3 <- unemployment_gap ~ output_gap + 0

#---- e) Estimação SUR #----
 
fit_sur_1 <- systemfit(list(r1 = r1, r2 = r2, r3 = r3), method = "SUR", data = serie_hp_1000_sur)
summary(fit_sur_1) 

fit_sur_2 <- systemfit(list(r1 = r1, r2 = r2, r3 = r3), method = "SUR", data = serie_hp_1600_sur)
summary(fit_sur_2) 

 
#---- f) Teste da hipótese de que beta = gamma * delta #---- 
 
restricao <- "b[1]*b[2] = b[3]"

teste_1 <- nlWaldtest(fit_sur, 
                      texts =  restricao)

teste_2 <- nlWaldtest(fit_sur, 
                      texts =  restricao,
                      df2 = T)
  
