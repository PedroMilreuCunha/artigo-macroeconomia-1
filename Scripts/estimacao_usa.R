## -- Pedro Milreu Cunha -- ##
## -- Mestrando em Economia Aplicada pelo PPGE/UFPB -- ##

library(mFilter)
library(stargazer)
library(ggplot2)
library(strucchange)

#### Lendo os dados ####

serie_temporal <- read.delim("Data Appendix/data/Data_USA_FRED_updated.txt", header = TRUE, sep = "\t", dec = ".")
serie_temporal <- serie_temporal[-c(65,66),]

#### Criando o GDP em log x 100 ####

serie_temporal$ln_GDP_100 <- 100*log(serie_temporal$gdp)
serie_temporal$ln_Employment_100 <- 100*log(serie_temporal$emp1)

#### Criando as variáveis em diferença ####

serie_em_diferencas <- data.frame(GDP = seq(1,63), UR = seq(1,63))

i <- 1

while (i <= 63) {

  serie_em_diferencas$GDP[i] = serie_temporal$ln_GDP_100[i+1] - serie_temporal$ln_GDP_100[i]
  serie_em_diferencas$UR[i] = serie_temporal$ur[i+1] - serie_temporal$ur[i]
  i = i+1
  }

#### 1ª forma - Filtro HP ####

 ## Output gap ##

output_gap_100 <- hpfilter(serie_temporal$ln_GDP_100, type = "lambda", freq = 100)$cycle
unemployment_gap_100 <- hpfilter(serie_temporal$ur, type = "lambda", freq = 100)$cycle

output_gap_1000 <- hpfilter(serie_temporal$ln_GDP_100, type = "lambda", freq = 1000)$cycle
unemployment_gap_1000 <- hpfilter(serie_temporal$ur, type = "lambda", freq = 1000)$cycle


## Tratando como séries temporais ##

serie_hp_100 <- cbind(output_gap = output_gap_100,
                  unemployment_gap = unemployment_gap_100)
serie_hp_100 <- ts(data = serie_hp_100, start = 1948, end = 2011, frequency = 1)

serie_hp_1000 <- cbind(output_gap = output_gap_1000,
                      unemployment_gap = unemployment_gap_1000)
serie_hp_1000 <- ts(data = serie_hp_1000, start = 1948, end = 2011, frequency = 1)

serie_em_diferencas <- ts(data = serie_em_diferencas, start = 1949, end = 2011, frequency = 1)

## Estimando a primeira versão da Lei de Okun ##

estimacao_1 <-lm(data = serie_hp_100, 
                 unemployment_gap ~ output_gap+0)
stargazer(estimacao_1, type = "text")

estimacao_2 <-lm(data = serie_hp_1000, 
                 unemployment_gap ~ output_gap+0)
stargazer(estimacao_2, type = "text")

#### 2ª Forma - Versão em diferenças ####

estimacao_3 <- lm(data = serie_em_diferencas,
                  UR ~ GDP)
stargazer(estimacao_1, estimacao_2, estimacao_3, type = "text")

#### Testando a estabilidade da estimação ####

temp <- subset(serie_temporal, date >= 1980)

temp_output_gap <- hpfilter(temp$ln_GDP_100, type = "lambda", freq = 100)$cycle
temp_unemployment_gap <- hpfilter(temp$ur, type = "lambda", freq = 100)$cycle

serie_hp_estabilidade <- cbind(output_gap = temp_output_gap, unemployment_gap = temp_unemployment_gap)

serie_hp_estabilidade <- ts(serie_hp_estabilidade, start = 1980, end = 2011, frequency = 1)

estimacao_4 <- lm(data = serie_hp_estabilidade, unemployment_gap ~ output_gap + 0)

temp_bp <- breakpoints(data = serie_hp_estabilidade, unemployment_gap ~ output_gap+0)
summary(temp_bp)

## Isso é para o caso de haver quebra. Pelo critério de menor BIC, a melhor opção é 0 quebras.
#temp_ci <- confint(temp_bp)
#plot(estimacao_4)
#lines(temp_bp)                              
#lines(MFG_ci)

#### Criação dos gráficos ####

 ## 1980-2011 ##

ggplot(data = df, aes(y = unemployment_gap, x = output_gap))+
  geom_point(data = df, show.legend = TRUE)+
  geom_vline(xintercept = 0)+
  geom_abline(slope = estimacao_4$coefficients, intercept = 0,
              show.legend = TRUE)+
  labs(x = "Hiato do produto", y = "Hiato do desemprego")+
  theme_light()

## Sistema de equações

temp <- serie_temporal[, c(3,9,10)]

## Output, employment e unemployment gap##

output_gap_100_sur <- hpfilter(temp$ln_GDP_100, type = "lambda", freq = 100)$cycle
unemployment_gap_100_sur <- hpfilter(temp$ur, type = "lambda", freq = 100)$cycle
employment_gap_100_sur <- hpfilter(temp$ln_Employment_100, type = "lambda", freq = 100)$cycle

serie_sur <- cbind(output_gap = output_gap_100_sur, 
                   unemployment_gap = unemployment_gap_100_sur,
                   employment_gap = employment_gap_100_sur)

serie_sur <- ts(data = serie_sur, start = 1948, end = 2011, frequency = 1)

r1 <- employment_gap ~ output_gap + 0
r2 <- unemployment_gap ~ employment_gap + 0
r3 <- unemployment_gap ~ output_gap + 0

## Estimação SUR

fit_sur <- systemfit(list(r1 = r1, r2 = r2, r3 = r3), method = "SUR", data = serie_sur)
summary(fit_sur) 

## Teste da hipótese de que beta = gamma * delta 

restricao <- "b[1]*b[2] = b[3]"
teste_1 <- nlWaldtest(fit_sur, 
                    texts =  restricao)
teste_2 <- nlWaldtest(fit_sur, 
                    texts =  restricao,
                    df2 = T)
