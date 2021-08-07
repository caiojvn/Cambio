### Projeto destinado a previsão da tendência do cambio USD/BRL

url_1 <- 'http://api.bcb.gov.br/dados/serie/bcdata.sgs.10813/dados?formato=csv' #compra
url_2 <- 'http://api.bcb.gov.br/dados/serie/bcdata.sgs.1/dados?formato=csv' #venda

library(readr)
Cambio_Compra <- read_delim(url_1, ";", escape_double = FALSE, 
                            col_types = cols(data = col_date(format = "%d/%m/%Y"), 
                                             valor = col_character()), trim_ws = TRUE)
colnames(Cambio_Compra) <- c('Data', 'Nominal')
Cambio_Compra$Nominal <- as.double(gsub(',','.', Cambio_Compra$Nominal))


Cambio_Venda <- read_delim(url_2, ";", escape_double = FALSE, 
                           col_types = cols(data = col_date(format = "%d/%m/%Y"), 
                                            valor = col_character()), trim_ws = TRUE)
colnames(Cambio_Venda) <- c('Data', 'Nominal')
Cambio_Venda$Nominal <- as.double(gsub(',','.', Cambio_Venda$Nominal))


# A partir do ano 2000

paridade.df <- min(max(Cambio_Venda$Data), max(Cambio_Compra$Data))

library(dplyr)

Dados <- data.frame(filter(Cambio_Compra, Cambio_Compra>='2000-01-01' & Cambio_Compra$Data<=paridade.df),
                    filter(Cambio_Venda[,2], Cambio_Venda>='2000-01-01' & Cambio_Venda$Data<=paridade.df))
colnames(Dados) <- c('Data', 'Compra', 'Venda')
Dados <- Dados %>% mutate(Variacao_Compra = c(rep(NA, 1), diff(Dados$Compra, lag=1)),
                          Variacao_Venda = c(rep(NA, 1), diff(Dados$Venda, lag=1)),
                          MMS_Compra = TTR::SMA(Dados$Compra, 7),
                          MMS_Venda = TTR::SMA(Dados$Venda, 7))
View(Dados)

ultimos_dias <- tail(Dados, 15)
rownames(ultimos_dias) <- 1:nrow(ultimos_dias)
ultimos_dias


## Calendario Economico


calendario <- read_csv("calendarioeconomico - 2020ate2021.csv")
calendario$Start <- as.Date(as.timeDate(calendario$Start))
calendario$Impact <- as.factor(calendario$Impact)
calendario$Currency <- as.factor(calendario$Currency)

## Teste de sazonalidade
summary(seastests::wo(Dados$Compra[Dados$Data>'2019-01-01'], freq = 365)) # Não há evidencia de sazonalidade anual


### Panorama
library(ggplot2)

ggplot(Dados[Dados$Data>tail(Dados$Data,30)[1],])+theme_minimal()+theme(aspect.ratio = 2/4)+
  geom_line(aes(x=Data, y=Compra, colour='Valor nominal'))+labs(y='Dolar (R$)')+
  geom_line(aes(x=Data, y=MMS_Compra, colour='Media movel - 7 dias'), alpha=0.5, size=1)+
  scale_color_manual(values = c('Valor nominal'='black', 'Media movel - 7 dias'='steelblue'))
###


### PREVISAO M: Prophet

library(prophet)

#
remove(Dados2000)
Dados2020 <- Dados[Dados$Data>'2020-01-01',]

train_dt2020 <- Dados2020[1:(nrow(Dados2020)*2/3),]
test_dt2020 <- Dados2020[-(1:(nrow(Dados2020)*1/3)),]

#

m.prophet <- data.frame(ds=Dados2020$Data[Dados2020$Data<(max(Dados2020$Data)-(periodo+3))], 
                        y=Dados2020$Compra[Dados2020$Data<(max(Dados2020$Data)-(periodo+3))])

changepoint.m <- unique(calendario %>% filter(Impact==c("HIGH", "MEDIUM")) %>% select(Start)) 
changepoint.m <- changepoint.m$Start[changepoint.m$Start>=min(m.prophet$ds) & 
                                       changepoint.m$Start<=max(m.prophet$ds)]


holid.br <- as.Date(c('2020-01-01', '2020-02-24', '2020-02-25', '2020-04-10', '2020-04-21',
                      '2020-05-01', '2020-06-11', '2020-07-09','2020-01-01', '2020-09-07', 
                      '2020-10-12', '2020-11-02', '2020-11-20', '2020-12-24', '2020-12-25', 
                      '2020-12-31', '2021-01-01', '2021-01-25', '2021-02-15', '2021-02-16', 
                      '2021-02-17', '2021-03-02', '2021-03-21', '2021-06-03', '2021-07-09', 
                      '2021-09-07', '2021-10-12', '2021-11-02', '2021-11-15', '2021-12-22',
                      '2021-12-23', '2021-12-29', '2021-12-30', '2021-12-31'))

holid.br <- data.frame(holiday='feriado Brasil',
                       ds=holid.br,
                       lower_window=0,
                       upper_window=1)


holid.eua <- as.Date(c('2020-01-01', '2020-01-20', '2020-02-17', '2020-05-25', '2020-07-03',
                       '2020-09-07', '2020-10-12', '2020-11-11','2020-11-26', '2020-12-25',
                       '2021-01-01', '2021-01-18', '2021-02-15', '2021-02-15', '2021-05-31',
                       '2021-07-05', '2021-09-06', '2021-10-11', '2021-11-11', '2021-11-25',
                       '2021-12-24'))
holid.eua <- data.frame(holiday='feriado EUA',
                        ds=holid.eua,
                        lower_window=0,
                        upper_window=1)

holid <- arrange(rbind(holid.br, holid.eua), ds)
holid

m <- prophet(m.prophet, growth = 'linear', 
             changepoints = changepoint.m, changepoint.prior.scale = 0.8, holidays=holid,
             mcmc.samples = 0, interval.width = 0.95,
             yearly.seasonality = FALSE, weekly.seasonality = FALSE, 
             daily.seasonality = FALSE)
m.future <- make_future_dataframe(m, period=periodo)
m.forecast <- predict(m, m.future)

previsao_m <- tail(m.forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')], periodo)
colnames(previsao_m) <- c('Data', 'Previsão', 'Minimo', 'Maximo')
rownames(previsao_m) <- 1:periodo


library(timeDate)

Data <- timeSequence(from=as.Date(previsao_m$Data[1]), 
                     to=as.Date(previsao_m$Data[1])+(periodo+3))[
                       isWeekday(timeSequence(from=as.Date(previsao_m$Data[1]), 
                                              to=as.Date(previsao_m$Data[1])+(periodo+3)))]
previsao_m$Data <- as.Date(Data)
previsao_m


### Gráfico da previsão m
ggplot(prophet:::df_for_plotting(m, m.forecast), aes(x=ds, y=y))+
  theme_minimal()+theme(aspect.ratio = 2/4)+
  labs(caption = 'Intervalo de confianca: 95%', y = 'Dolar (R$)', x = 'Data')+geom_line(na.rm=TRUE)+
  geom_line(aes(y=yhat), color = "deeppink3", na.rm = TRUE, size=1, alpha=0.5)+ 
  geom_ribbon(ggplot2::aes(ymin = yhat_lower, ymax = yhat_upper), alpha = 0.1, 
              fill = "deepskyblue4", na.rm = TRUE)


#

p <- 30

df.prevbasica <- data.frame(Data = Dados2020$Data,
                            Dolar = Dados2020$Compra,
                            Previsao = m.forecast$yhat,
                            yhat_lower = m.forecast$yhat_lower,
                            yhat_upper = m.forecast$yhat_upper)
df.prevbasica <- df.prevbasica %>% filter(Data>=max(Dados$Data)-p, Data<=max(Dados$Data))

library(gganimate)

anim1 <- ggplot(df.prevbasica, aes(x=Data))+geom_line(aes(y= Dolar))+
  xlim(max(Dados$Data)-p,max(Dados$Data))+
  ylim(tail(m.forecast$yhat_lower, p) %>% min,tail(m.forecast$yhat_upper, p) %>% max)+
  theme_minimal()+
  labs(caption = 'Intervalo de confianca: 95%', y = 'Dolar (R$)', x = 'Data')+
  geom_line(aes(y= Previsao), color='slategray4', alpha=0.5, size=1)+geom_line(aes(y=Dolar))+
  geom_ribbon(aes(ymin = yhat_lower, ymax = yhat_upper, x=Data), 
              alpha = 0.1, fill = "deepskyblue4", na.rm = TRUE)+
  geom_segment(data=df.prevbasica[df.prevbasica$Data>max(Dados$Data)-(periodo+4),], 
               aes(y=Dolar, group=Data, yend= Previsao, xend=Data), col='slateblue', 
               linetype=2, alpha=0.75)+
  geom_point(data=df.prevbasica[df.prevbasica$Data>max(Dados$Data)-(periodo+4),], 
             aes(y=Dolar, group=Data), col='slateblue',alpha=0.75, size=2)+
  transition_reveal(Data)+
  shadow_mark(alpha = 0.005)

animate(anim1)

### Precisao da previsao

intseq <- zoo::as.Date(intersect(ultimos_dias$Data,previsao_m$Data))

valorreal <- ultimos_dias$Compra[ultimos_dias$Data>=intseq[1] & ultimos_dias$Data<=intseq[length(intseq)]]

prevy <- previsao_m$Previsão[previsao_m$Data>=intseq[1] & previsao_m$Data<=intseq[length(intseq)]]
erpad.y <- (valorreal-prevy)/valorreal

prevmin <- previsao_m$Minimo[previsao_m$Data>=intseq[1] & previsao_m$Data<=intseq[length(intseq)]]
erpad.min <- (valorreal-prevmin)/valorreal

prevmax <- previsao_m$Miximo[previsao_m$Data>=intseq[1] & previsao_m$Data<=intseq[length(intseq)]]
erpad.max <- (valorreal-prevmax)/valorreal


df.accuracy <- data.frame(Data= intseq, Dolar=valorreal, Previsao=round(prevy,4) ,
                          `ErroPerc_Previsão` = paste(round(erpad.y*100,3), '%', sep='')
                          #, Minimo=prevmin 
                          #,`ErroPerc_minimo` = erpad.min*100
                          #,Maximo=prevmax
                          #,`ErroPerc_maximo` = erpad.max*100 
)

precisao <- list(aTSA::accurate(df.accuracy$Dolar, df.accuracy$Previsão, k=2), Tabela=df.accuracy)



### (NOVA) PREVISAO N: Prophet


n.prophet <- data.frame(ds=Dados2020$Data, 
                        y=Dados2020$Compra)

changepoint.n <- calendario$Start
changepoint.n <- changepoint.n[changepoint.n>=min(n.prophet$ds) & 
                                 changepoint.n<=max(n.prophet$ds)] %>% unique



n <- prophet(n.prophet, growth = 'linear', 
             changepoints = changepoint.n, changepoint.range = 0.9,
             mcmc.samples = 0, interval.width = 0.95, holidays=holid,
             yearly.seasonality = FALSE, weekly.seasonality = FALSE, 
             daily.seasonality = FALSE)
n.future <- make_future_dataframe(n, period=periodo)
n.forecast <- predict(n, n.future)

previsao_n <- tail(n.forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')], periodo)
previsao_n[-1] <- round(previsao_n[,-1],4)
colnames(previsao_n) <- c('Data', 'Previsao', 'Minimo', 'Maximo')
rownames(previsao_n) <- 1:periodo

Data_futura <- timeSequence(from=as.Date(previsao_n$Data[1]), 
                            to=as.Date(previsao_n$Data[1])+(periodo+3))[
                              isWeekday(timeSequence(from=as.Date(previsao_n$Data[1]), 
                                                     to=as.Date(previsao_n$Data[1])+(periodo+3)))]
previsao_n$Data <- as.Date(Data_futura)

previsao_n

### Gráfico da previsão

df.prev_n <- prophet:::df_for_plotting(n, n.forecast)
df.prev_n$ds <- as.Date(df.prev_n$ds)

ggplot(df.prev_n, aes(x=ds, y=y))+
  xlim(max(df.prev_n$ds)-p*6,
       max(df.prev_n$ds))+
  ylim(tail(df.prev_n$yhat_lower, p*6) %>% min,tail(df.prev_n$yhat_upper, p*6) %>% max)+
  theme_minimal()+theme(aspect.ratio = 2/4)+
  labs(caption = 'Intervalo de confianca: 95%', y = 'Dolar (R$)', x = 'Data')+geom_line(na.rm=TRUE)+
  geom_line(aes(y=yhat), color = "slategray4", na.rm = TRUE, size=1.5, alpha=0.5)+ 
  geom_ribbon(ggplot2::aes(ymin = yhat_lower, ymax = yhat_upper), alpha = 0.1, 
              fill = "deepskyblue4", na.rm = TRUE)
#

plot(n, n.forecast)
prophet_plot_components(n, n.forecast)

###


### [RETROPECTO] Previsao para variacao

o.prophet <- data.frame(ds=Dados2000$Data[Dados2000$Data<(max(Dados2000$Data)-(periodo+3))], 
                        y=Dados2000$Variacao_Compra[Dados2000$Data<(max(Dados2000$Data)-(periodo+3))])

changepoint.o <- calendario$Start
changepoint.o <- changepoint.o[changepoint.o>=min(o.prophet$ds) & 
                                 changepoint.o<=max(o.prophet$ds)] %>% unique



o <- prophet(o.prophet, growth = 'linear', 
             changepoints = changepoint.o, changepoint.range = 0.9,
             mcmc.samples = 0, interval.width = 0.95, holidays=holid,
             yearly.seasonality = FALSE, weekly.seasonality = FALSE, 
             daily.seasonality = FALSE)
o.future <- make_future_dataframe(o, period=periodo)
o.forecast <- predict(o, o.future)

previsao_o <- tail(o.forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')], periodo)
previsao_o[-1] <- round(previsao_o[,-1],4)
colnames(previsao_o) <- c('Data', 'Previsão', 'Minimo', 'Maximo')
rownames(previsao_o) <- 1:periodo

Data <- timeSequence(from=as.Date(previsao_o$Data[1]), 
                     to=as.Date(previsao_o$Data[1])+(periodo+3))[
                       isWeekday(timeSequence(from=as.Date(previsao_o$Data[1]), 
                                              to=as.Date(previsao_o$Data[1])+(periodo+3)))]
previsao_o$Data <- as.Date(Data)
previsao_o

plot(o, o.forecast)  ### FAIL, consequentemente, para os dados presentes não dará certo.


library(ggplot2)
library(gganimate)


x <- matrix(rnorm(200),100,2)
y <- 0.1*x[,1] + 2*x[,2] + rnorm(100)
z <- fitted(lm(y ~ x))
library(aTSA)
A <- accurate(y,z,2)

ggplot(A, aes(x=x, y=y))+geom_point()+transition_time(z)
