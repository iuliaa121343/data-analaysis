# ANALIZA SERIEI UNIVARIATE

library(tsibble)
library(fpp3)
library(fpp2)
library(readxl)
library(vars)
library(tseries)
library(urca)
library(stats)
library(changepoint)
library(dplyr)
library(uroot)
library(forecast) 
library(TSA)
library(FinTS)
library(tidyverse)
library(fGarch)
library(fDMA)
library(lmtest)
library(lubridate)
library(nortsTest)


# Convertim setul de date intr-un obiect de tip ts
curs_ts <- ts(date_ST$Curs_schimb, start = c(2005, 1), frequency = 12)

# Graficul seriei de timp
autoplot(curs_ts) +
  labs(title = "Cursul de schimb RON/EUR",
       subtitle = "din 2005 pana in 2025",
       y = "Curs de schimb")
# Seria cursului de schimb valutar RON/EUR, date lunare din 2005 pana in 2025
# La prima vedere, seria pare ca este nestationara si ca nu are sezonalitate
# Trendul este ascendent

# Testam sezonalitatea
# Grafice de sezonalitate
ggseasonplot(curs_ts, month.labels=TRUE, month.labels.left=TRUE) +
  ylab("RON") +
  ggtitle("Grafic de sezonalitate: cursul de schimb valutar RON/EUR")

ggsubseriesplot(curs_ts) +
  ylab("RON") +
  ggtitle("Grafic de sezonalitate: cursul de schimb valutar RON/EUR")
# Deoarece mediile pe sezoane sunt similare, nu avem sezonalitate in date

#Estimarea trendului cu MM (12 luni)

trend_ma12 <- ma(curs_ts, order = 12)

autoplot(curs_ts, series = "Serie originala") +
  autolayer(trend_ma12, series = "Trend estimat (MA12)", color = "red") +
  labs(title = "Estimarea trendului cu MM",
       y = "Curs de schimb RON/EUR") +
  guides(color = guide_legend(title = "Serii"))

# Metode de netezire exponentiala

# Simple Exponential Smoothing (SES)

# Estimarea parametrilor
forecast_ses <- ses(curs_ts, h=12)

# Summary forecast
summary(forecast_ses)

# Seria valorilor fitted
forecast_ses$model$fitted

# Seria reziduurilor
forecast_ses$model$residuals

# Acuratetea modelului
round(forecast::accuracy(forecast_ses),2)

# Prognoza prin SES
autoplot(forecast_ses) +
  autolayer(fitted(forecast_ses), series="Fitted") +
  ylab("RON") + xlab("Luna") +
  theme_bw()

# Holt linear trend method

autoplot(curs_ts) +
  ylab("Cursul de schimb RON/EUR") + xlab("Luna") +
  ggtitle("Cursul de schimb RON/EUR") +
  theme_bw()

# Estimare
forecast_holt_trend <- holt(curs_ts, h=12)

# Summary forecast
summary(forecast_holt_trend)

# Seria valorilor fitted
forecast_holt_trend$model$fitted

# Seria reziduurilor
forecast_holt_trend$model$residuals

# Acuratetea modelului
round(forecast::accuracy(forecast_holt_trend),2)

# Prognoza prin HW trend
autoplot(forecast_holt_trend) +
  autolayer(fitted(forecast_holt_trend), series="Fitted") +
  ylab("Cursul de schimb RON/EUR") + xlab("Luna") +
  theme_bw()

# Prognoza in-sample
autoplot(curs_ts) +
  autolayer(forecast_holt_trend, series="Holt's method", PI=TRUE) +
  ggtitle("Forecasts from Holt's method") + xlab("Luna") +
  ylab("Cursul de schimb RON/EUR") +
  guides(colour=guide_legend(title="Forecast")) +
  theme_bw()

# Prognoza SES si HW trend
forecast2 <- ses(curs_ts, h=12)
autoplot(curs_ts) +
  autolayer(forecast_holt_trend, series="Holt's method", PI=FALSE) +
  autolayer(forecast2, series="Simple Exponential Smoothing", PI=FALSE) +
  ggtitle("Forecasts from Holt's method") + xlab("Luna") +
  ylab("Cursul de schimb RON/EUR") +
  guides(colour=guide_legend(title="Forecast")) +
  theme_bw()

# Compararea performantei modelelor prin metoda cross-validation
e1 <- tsCV(curs_ts, ses, h=1)
e2 <- tsCV(curs_ts, holt, h=1)
#MSE (mean squared error)
mean(e1^2, na.rm=TRUE)
mean(e2^2, na.rm=TRUE)
# MAE (mean absolute error)
mean(abs(e1), na.rm=TRUE)
mean(abs(e2), na.rm=TRUE)

summary(forecast2)
summary(forecast_holt_trend)

#Comparand modelele observam urmatoarele:
#SES prezinta valori mai mici pentru indicatorii AIC, AICc si BIC decat modelul Holt, deci modelul SES este mai bun conform acestor criterii
#SES are scoruri de precizie mai mici la majoritatea metodelor (erorilor de prognoza), insa Holt este putin mai bun la eroare medie (ME) si autocorelatia reziduurilor (ACF1)


#Testarea acuratetii prognozelor modelelor

# Pentru a putea compara acuratetea prognozelor vom folosi testul Diebold Mariano
# Testul Diebold Mariano
# H0: prognozele au aceeasi acuratete
# H1: prognozele au acuratete diferita

dm.test(residuals(forecast2),residuals(forecast_holt_trend))# deoarece p > 0.1 acceptam H0
# prognozele au aceeasi acuratete => nu exista diferente semnficative intre cele doua modele


forecast2[["model"]]

#Prognoza se va realiza pe metoda SES

autoplot(forecast2) +
  xlab("Luna") + ylab("Cursul de schimb RON/EUR") +
  theme_bw()

# Diagnosticul pe reziduuri
# Extragem reziduurile
res_ses <- residuals(forecast2)

# Graficul reziduurilor
autoplot(res_ses) + xlab("Luna") + ylab("") +
  ggtitle("Reziduurile din modelul SES") +
  theme_bw()

# Histograma reziduurilor
gghistogram(res_ses) + ggtitle("Histograma reziduurilor SES")

# Testarea prin Jarque-Berra a normalitatii reziduurilor
# H0: seria este normal distribuita
# H1: seria nu este normal distribuita
jarque.bera.test(res_ses) # p-value (2.2e-16) < 0.1 => seria nu este distribuita normal

# Functia de autocorelatie a reziduurilor
ggAcf(res_ses) + ggtitle("ACF of residuals")

# Testul Box-Pierce
# H0: seria reziduurilor este stationara 
# H1: seria reziduurilor este nestationara 
Box.test(res_ses, lag=1)
Box.test(res_ses, lag=2)
Box.test(res_ses, lag=3)
Box.test(res_ses, lag=4)
Box.test(res_ses, lag=5)  
Box.test(res_ses, lag=10) # => p-value < 0.1 => seria este nestationara, seria nu este un proces white noise

# Testul Ljung-Box
# H0: model doesn't show lack of fit
# H1: model shows lack of fit

Box.test(res_ses, lag=1,type="Lj")
Box.test(res_ses, lag=2, type="Lj")
Box.test(res_ses, lag=3, type="Lj")
Box.test(res_ses, lag=4, type="Lj")
Box.test(res_ses, lag=5, type="Lj")  
Box.test(res_ses, lag=10, type="Lj") # p-value < 0.1 => seria reziduurilor este nestationara

#SAU facem diagnosticarea automata:
checkresiduals(forecast2)

#Stationaritate

autoplot(curs_ts) +
  labs(title = "Cursul de schimb RON/EUR",
       subtitle = "din 2005 pana in 2025",
       y = "Curs de schimb") 

#seria este nestationara, deoarece avem schimbari de trend

#Detectarea stationaritatii prin metoda grafica

#Graficul de autocorelare
ggAcf(curs_ts)
ggAcf(diff(curs_ts))
ggAcf(curs_ts, lag=60)

# Testarea stationaritatii
# Detectarea stationaritatii cu ajutorul testelor statistice

# Testul Augmented Dickey Fuller (ADF)
# H0: seria admite o radacina unitara si este nestationara (p-value > 0.1)
# H1: seria nu admite o radacina unitara si este stationara (p-value < 0.1)

tseries::adf.test(curs_ts, k=1) # nestationara
tseries::adf.test(curs_ts, k=2) # nestationara
tseries::adf.test(curs_ts, k=3) # nestationara
tseries::adf.test(curs_ts, k=4) # nestationara
tseries::adf.test(curs_ts, k=5) # nestationara
tseries::adf.test(curs_ts, k=6) # nestationara
tseries::adf.test(curs_ts, k=7) # nestationara
tseries::adf.test(curs_ts, k=8) # nestationara
tseries::adf.test(curs_ts, k=9) # nestationara
tseries::adf.test(curs_ts, k=10) # nestationara

tseries::adf.test(diff(curs_ts), k=1) # stationara 
tseries::adf.test(diff(curs_ts), k=2) # stationara 
tseries::adf.test(diff(curs_ts), k=3) # stationara 
tseries::adf.test(diff(curs_ts), k=4) # stationara 
tseries::adf.test(diff(curs_ts), k=5) # stationara 
tseries::adf.test(diff(curs_ts), k=10) # stationara 

# Pentru o acuratete cat mai mare a testului aplicam
# none - elemente deterministe
# drift - pentru constanta
# trend - pentru constanta si trend 

# Radacina unitara pentru elementele deterministe
rw_none <- ur.df(curs_ts, type='none', selectlags = c("AIC"))
summary(rw_none)
#Valoarea testului statistics (tau) |1.33| < |-2.58|/|-1.95|/|-1.62| => seria nu este stationara
#Probabilitatea z.lag .1 > 0.1 => seria este nestationara

rw_none2 <- ur.df(diff(curs_ts), type='none', selectlags = c("AIC"))
summary(rw_none2)
#Valoarea testului (tau2) |-9.2086 | > |-2.58|/|-1.95|/|-1.62| => seria este stationara la toate pragurile de semnificatie
#Probabilitatea z.lag.1 < 0.1 => seria este stationara la un prag de 99%

# Radacina unitara pentru intercept
rw_t <- ur.df(curs_ts, type='drift', selectlags = c("AIC"))
summary(rw_t) 
#Valoarea testului statistic |-1.2334| < |tau2| => serie nestationara
#Probabilitatea z.lag.1 > 0.1 => serie nestationara 

rw_t2 <- ur.df(diff(curs_ts), type='drift', selectlags = c("AIC"))
summary(rw_t2) 
#Valoarea testului statistic |-9.3467| > |tau2|  => seria este stationara la toate pragurile de seminificatie
#Probabilitatea z.lag.1 < 0.01 deci seria este stationara la un prag de semnificatie de 99%

# Radacina unitara in trend si intercept
rw_ct <- ur.df(curs_ts, type='trend', selectlags = c("AIC"))
summary(rw_ct) 
#Valoarea testului statistic |-2.489| < |tau3| => seria este nestationara, chiar si cu trend

rw_ct2 <- ur.df(diff(curs_ts), type='trend', selectlags = c("AIC"))
summary(rw_ct2)
#Valoarea testului statistic |-9.3344| > |tau3| => seria este stationara
#Coeficientul estimat z.lag.1 are p-value < 0.1 => este semnificativ statistic
#Trendul (tt) are p-value 0.69 > 0.1 => nesemnificativ, seria diferentiata nu mai contine o componenta de trend
#Lagul diferentei anterioare (z.diff.lag) are p-value de 0.942 > 0.1 => nesemnificativ, efect intarziat slab

# Testul KPSS (Kwiatkowski-Phillips-Schmidt-Shin)
# H0: seria este stationara 
# H1: seria nu este stationara

summary(ur.kpss(curs_ts))
#Valoarea testului 4.3541 > 0.347/0.463/0.574/0.739 => seria este nestationara 
summary(ur.kpss(diff(curs_ts)))
#Valoarea testului 0.0617 < 0.347/0.463/0.574/0.739 => seria este stationara

# Testul Phillips-Perron
# H0: seria admite o radacina unitate
# H1: seria nu admite o radacina unitate

PP.test(curs_ts) # serie nestationara 
PP.test(diff(curs_ts)) # serie stationara

#Punctele de cotitura (Change points) in medie 
#Metoda segmentarii binare
m_binseg <- cpt.mean(curs_ts, penalty = "BIC", method = "BinSeg", Q = 1)
#Afisarea punctelor de cotitura
changepoint <- cpts(m_binseg) #punctul de cotitura este la 48
#Perioada de timp in care a avut loc punctul de cotitura => criza din 2008
time(curs_ts)[48]

#Graficul schimabarilor in medie a seriei

#Media segmentelor
mean_1 <- mean(curs_ts[1:changepoint])
mean_2 <- mean(curs_ts[(changepoint + 1):length(curs_ts)])

# Timpul corespunzator axei X
time_vals <- time(curs_ts)

# Grafic cu segmente
autoplot(curs_ts) +
  geom_segment(aes(x = time_vals[1], xend = time_vals[changepoint],
                   y = mean_1, yend = mean_1), colour = 'red') +
  geom_segment(aes(x = time_vals[changepoint + 1], xend = time_vals[length(curs_ts)],
                   y = mean_2, yend = mean_2), colour = 'red') +
  ggtitle("Punctul de cotitură în medie") +
  ylab("Curs RON/EUR") +
  theme_bw()

#Rupturile in structura seriei (Structural Breaks)

#Testul Quandt Likelihood Ratio (QLR)
#Pas 1:
data_qlr <- tibble(ylag0 = curs_ts,
                   ylag1 = lag(as.numeric(curs_ts)))

# Pas 2:
#Regresie falsa in care variabila dependenta este variabila cu lagul 0 (variabila originala) si variabila independenta este variabila cu 1 decalaj (lag1)
qlr <- Fstats(ylag0 ~ ylag1, data = data_qlr)

#Pas 3: Estimam punctul in care avem structural change
breakpoints(qlr)

#Pas 4: Testam semnificatia punctului
sctest(qlr, type = "supF") 
# p < 0.1 => structural change semnificativ

#Identificam perioada de timp pentru punctul in care avem structural change (43)
time(curs_ts)[43]

#Pas 5: Reprezentare grafica
autoplot(curs_ts) +
  geom_vline(xintercept = 2008.5, colour = 'red')+ 
  geom_hline(yintercept = 3.57, colour = 'red')+
  ggtitle("Ruptura in structura seriei") +
  ylab("Curs RON/EUR") +
  theme_bw()

#Testarea radacinii unitare atunci cand avem structural break

#Testul Zivot & Andrews
# H0: Serie nestationara cu un structural break
# H1: Serie stationara cu un structural break

za_test <- ur.za(curs_ts, model = "both", lag = 0)
summary(za_test)
# |-6.422| > |-5.57|/|-5.08|/|-4.82| => resping nula si acceptam alternativa 
# => seria este stationara cu structural break in nivel

plot(za_test)
#Observam ca linia coboara sub linia rosie (adica cea mai mica valoare critica = -5.57) 
#=> respingem H0 la acest nivel de semnificatie
#Deci seria este stationara cu un punct de ruptura structurala
#Linia rosie punctata indica locul exact al breakpointului (in jurul pozitiei 47-48)

#Stationarizarea seriei

#Analizam seria originala
autoplot(curs_ts)
#Trendul este ascendent, deci avem nestationaritate in medie

#Generam graficul ACF pentru seria originala
ggAcf(curs_ts)
#Lagurile sunt in afara intervalului Bartlett, indicand autocorelare
#Valorile ACF scad lent, treptat => nestationaritate in medie

#Determinam numarul de diferentieri necesare
ndiffs(curs_ts) #este necesara o singura diferentiere pentru a deveni stationara

#Generam graficul ACF pentru seria diferentiata
ggAcf(diff(curs_ts))
#Lagurile se afla in interiorul intervalului, deci nu exista autocorelare semnificativa
# => seria diferentiata este stationara

#Analizam seria diferentiata
autoplot(diff(curs_ts))
#Trendul a fost eliminat, seria a devenit stationara

#Generam graficul PACF (autocorelatie partiala) pentru seria diferentiata
ggPacf(diff(curs_ts))

#Realizam corelograma si analizam graficele ACF si PACF
ggtsdisplay(diff(curs_ts))

#Interpretare ACF:
#Primul lag este semnificativ pozitiv, iar apoi valorile scad rapid, incadrandu-se in limitele de semnificatie (liniile punctate albastre)
#=> nu exista autocorelare persistenta => nu ennevoie de multe componente MA
#Interpretare PACF:
#Primul lag este semnificativ
#Lagurile urmatoare sunt mai slabe/nesemnificative

# Din corelograma, modelul optim pare sa fie ARIMA(1,1,0)
# => MODEL ARIMA(1,1,0) - AR(1) -> primul lag din PACF este semnificativ
#                       - I(1)  -> diferenta de ordinul 1
#                       - MA(0) -> ACF nu are taiere brusca la lag 1 => nu sugereaza o componenta MA semnificativa

#Folosim functia auto.arima pentru a identifica automat modelul cu cel mai mic AIC
model_arima_auto <- auto.arima(curs_ts, seasonal=FALSE)
summary(model_arima_auto) 
coeftest(model_arima_auto)
#Modelul identificat este ARIMA(1,1,0)
# ar1 = 0.29 => fiecare valoare a seriei este influențată de aproximativ 30% din valoarea anterioară
#Coeficientul ar1 este semnificativ, deci modelul AR(1) este justificat
# drift reprezinta termenul constant din seria diferentiata si nu este semnificativ statistic, nu aduce o contributie semnificativa in model
#Erorile sunt mici si nu exista autocorelare in reziduuri
# AIC = -838.93 => negativ, foarte mic => model bine ajustat

#Vom elimina drift-ul din model pentru a nu pierde acuratete
model_arima_auto_nodrift <- auto.arima(curs_ts, allowdrift = FALSE, seasonal = FALSE)
summary(model_arima_auto_nodrift)
coeftest(model_arima_auto_nodrift)
#Coeficientul ar1 (0.3075) este semnificativ statistic, deci acest lag anterior influențează semnificativ valorile viitoare
# AIC = -839.6 => mai mic decat in cazul anterior, deci modelul este mai bun

#In continuare vom testa mai multe modele candidate
#Inainte vom diferentia seria pentru a deveni stationara:

arima210 <- Arima(curs_ts, order=c(2,1,0))
summary(arima210) #Modelul identificat automat are un AIC mai mic
coeftest(arima210)#Nu toti coeficientii sunt semnificativi

arima111 <- Arima(curs_ts, order=c(1,1,1))
summary(arima111) #AIC mai mare decat modelul automat

arima211 <- Arima(curs_ts, order=c(2,1,1))
summary(arima211) #AIC mai mare decat modelul automat

#In concluzie, modelul generat automat este cel mai potrivit

#Testam inversabilitatea
autoplot(model_arima_auto_nodrift)
#Proces stationar, deoarece punctul se afla in interiorul cercului

#Modelul optim identificat este ARIMA(1,1,0)

arima110 <- model_arima_auto_nodrift

#Diagnosticul pe reziduuri ale modelului

residuals_arima110 <- residuals(arima110)
ggtsdisplay(residuals_arima110) # nu avem autocorelare in reziduuri conform graficului ACF
Box.test(residuals_arima110, lag=1) # p > 0.1 => nu avem autocorelare (la un pas/o luna in urma) in reziduuri 
Box.test(residuals_arima110, lag=12,type="Lj") # p > 0.1 => nu avem autocorelare (pe 12 luni) in reziduuri
Box.test(residuals_arima110, lag=24,type="Lj") # p > 0.1 => nu avem autocorelare (pe 24 luni) in reziduuri
ArchTest(residuals_arima110, lags = 1) # p < 0.1 => respingem ipoteza nula, acceptam alternativa => 
#                                                  => există heteroscedasticitate condiționată în reziduuri; avem efecte ARCH
jarque.bera.test(residuals_arima110) # p < 0.1 => respingem ipoteza nula, acceptam alternativa => reziduurile nu sunt distribuite normal
checkresiduals(arima110)
#Reziduurile nu urmeaza o distributie normala
#Distributia este asimetrica la dreapta, cu o coada mai lunga in partea pozitiva (exista cateva reziduuri mari pozitive)
#Distributie leptocurtica, curba cu varf inalt si cozi mai grele 
#Barele izolate din partea dreapta indica prezenta unor reziduuri mai mari (potentiali outlieri)

#Realizam prognoze pe baza modelului identificat

#Prognoza pentru urmatorul an:

arima110 %>% 
  forecast::forecast(h=12) %>% 
  autoplot() + 
  xlab("Perioada") + 
  ylab("Cursul de schimb valutar RON/EUR") + 
  theme_minimal()
#Deoarece ipoteza de normalitate a fost incalcata, prognoza pe interval de incredere este foarte larga

#Vom aplica si modelul ETS pentru a compara prognoza pentru cele doua modele
model_ets <- ets(curs_ts) 
summary(model_ets)
# ETS: AIC = -178.9944; RMSE = 0.04328426; MAPE = 0.654153; MASE = 0.2266044
# ARIMA(1,1,0): AIC = -839.6; RMSE = 0.04255381; MAPE = 0.6403017; MASE = 0.2222451
# In ceea ce privese erorile, valorile dintre cele doua modele nu prezinta diferente foarte mari
# Insa in cazul ARIMA, AIC are o valoare mult mai mica
# Deci modelul ETS prognozeaza mai slab decat ARIMA

# Prognoza modelului ETS
model_ets %>% forecast::forecast(h=12) %>% 
  autoplot() +
  ylab("Cursul de schimb valutar RON/EUR")+
  theme_minimal()

# Compararea modelelor cu ajutorul testului Diebold Mariano
dm.test(residuals(model_ets),residuals(arima110))
# p > 0.1 => acceptam ipoteza nula: Cele două modele au performanțe egale la prognozare.
# Nu exista o diferenta semnificativa intre precizia prognozelor produse de cele doua modele

#In final, modelul optim ramane ARIMA(1,1,0), deoarece are o valoare a AIC mult mai scazuta fata de ETS

#Verificam daca avem efecte ARCH:
ArchTest(residuals_arima110, lags = 1)  #p-value = 0.0009385 → semnificativ;  efect ARCH prezent
ArchTest(residuals_arima110, lags = 12) #p-value = 0.0007374 → semnificativ;  efect ARCH prezent
ArchTest(residuals_arima110, lags = 24) #p-value = 0.01849 → semnificativ;  efect ARCH prezent
ArchTest(residuals_arima110, lags = 36) #p-value = 0.1083 → nesemnificativ; volatilitatea este instabila

residuals_arima110_squared <- residuals(arima110)^2
ggPacf(residuals_arima110_squared,12)

#Calculam randamentele logaritmice din cursul valutar
log_curs <- diff(log(curs_ts))

#Estimam modelul GARCH(1,1)
model_garch11_arma10 <- garchFit(arma(1,0)~garch(1,1), data = log_curs, trace = F)
summary(model_garch11_arma10)
#Majoritatea coeficientilor sunt semnificativi statistic, in afara de coeficientul omega
#Nu mai există autocorelare în reziduuri sau în volatilitate după modelare, deci modelul a explicat bine dependențele

#Testam si alte modele

model_garch11_arma11 <- garchFit(arma(1,1)~garch(1,1), data = log_curs, trace = F)
summary(model_garch11_arma11) #coeficientul omega nu este semnificativ

model_garch11_arma01 <- garchFit(arma(0,1)~garch(1,1), data = log_curs, trace = F)
summary(model_garch11_arma01) #coeficientul omega nu este semnificativ

model_garch21_arma10 <- garchFit(arma(1,0)~garch(2,1), data = log_curs, trace = F)
summary(model_garch21_arma10) #nu toti coeficientii sunt semnificativi

#Vom ramane la modelul GARCH(1,1)

# Ecuatia mediei
# yt = 5.261e-04 + epsilon
# Ecuatia variantei
# ht = 1.310e-10 + 6.323e-02*ht-1^2 + 9.249e-01*ht-1 + epsilon^2

#Verificare conditie
1.310e-10 + 6.323e-02 + 9.249e-01# => 0.98 - aproape de 1 => model persistent

# Serie variantei conditionate
garch_conditional_variance <- as.ts(model_garch11_arma10@h.t) 

# Graficul variantei conditionate
autoplot(garch_conditional_variance) + theme_bw() +
  ylab('Varianta conditionata') + 
  ggtitle('Estimarea modelului GARCH(1,1)') +
  theme(plot.title = element_text(hjust = 0.5))

# Impartirea setului de date in set de antrenare si set de testare
# Avand in vedere ca perioada analizata este din 2005 pana in 2025 si include 
# evenimente majore: crize financiare si pandemia, vom delimita seturile de date astfel:
# Set de antrenare - 2005-2021 
# Set de testare - 2022 - 2025

curs_ts <- ts(date_ST$Curs_schimb, start = c(2005, 1), frequency = 12)

#Training (pana in decembrie 2021)
train <- window(curs_ts, end = c(2021,12))

#Test (din ianuarie 2022)
test <- window(curs_ts, start= c(2022,1))

#Aplicam modelul ARIMA pe setul de antrenare
model <- auto.arima(train)
model
coeftest(model)

#Orizontul de prognoza (lungimea setului de test)
h <- length(test)
h

#Predictie cu interval de incredere 95%
rezultat_predictie <- forecast(model, h = h, level = 95)

#Reprezentare grafica
plot(rezultat_predictie)
lines(test, col = "red", lwd = 2)
legend("topleft", legend = c("Predicție", "Valori reale"), 
       col = c("blue", "red"), lty = 1, lwd = 2)

######################
# Analiza Multivariata
######################
install.packages("urca")
library(urca)
install.packages("vars")
library(vars)
install.packages("mFilter")
library(mFilter)
install.packages("tseries")
library(tseries)
install.packages("forecast")
library(forecast)
install.packages("tidyverse")
library(tidyverse)
install.packages("stargazer")
library(stargazer)
install.packages("Metrics")
library(Metrics)
install.packages("readxl")
library(readxl)
# Importul datelor
curs_schimb <- read_excel("D:/Desktop (STORAGE)/FAC/AN3_SEM2/SERII DE TIMP/curs_schimb.xlsx", 
                          col_names = FALSE)
View(curs_schimb)
rezerve_internationale <- read_excel("D:/Desktop (STORAGE)/FAC/AN3_SEM2/SERII DE TIMP/rezerve_internationale.xlsx", col_names = FALSE)
View(rezerve_internationale)

# Declaram variabilele de tip ts
curs <- ts(curs_schimb, start = c(2005, 5), frequency = 12)
rezerve <- ts(rezerve_internationale, start=c(2005,5), frequency = 12 )

# Normalizarea variabilelor
curs <- scale(curs)
rezerve <- scale(rezerve)

# Graficul seriilor
autoplot(cbind(curs,rezerve)) +
  ylab('') +
  ggtitle('Graficul seriei multivariate - cursul de schimb valutar si rezervele internationale') +
  theme_bw()

# Determinarea persistentei modelului
ggtsdisplay(curs)
ggtsdisplay(rezerve)

# Testarea stationaritatii seriilor
adf.curs <- ur.df(curs, type = "trend", selectlags = "AIC")
summary(adf.curs)
# |-2.46| < |-3.99|/|-3.43|/|-3.13|
# 2.83 < 6.22/4.75/4.07
# 3.12 < 8.43/6.49/5.47
# seria este nestationara

adf.rez <- ur.df(rezerve, type = "trend", selectlags = "AIC")
summary(adf.rez)
# seria este nestationara

# Corelograma primei diferente
ggtsdisplay(diff(curs)) # seria pare nestationara
ggtsdisplay(diff(rezerve)) # seria pare stationara

# Testarea stationaritatii seriilor diferentiate
adf.curs2 <- ur.df(diff(curs), type = "trend", selectlags = "AIC")
summary(adf.curs2) # serie stationara

adf.rez2 <- ur.df(diff(rezerve), type = "trend", selectlags = "AIC")
summary(adf.rez2) # serie stationara 

# Identificarea lagurilor optime
df <- cbind(curs, rezerve)
colnames(df) <- cbind('curs','rezerve')

lagselect <- VARselect(df,lag.max = 8, type = 'const')
lagselect
lagselect$selection # lagul 4 conform AIC, FPE
# 3 laguri conform AIC si FPE

# Implementarea VAR
model <- VAR(diff(df), p = 3, type = 'const', season = NULL, exog = NULL)
summary(model)

stargazer(model[['varresult']], type = 'text')

# Diagnosticul pe reziduuri

# Autocorelarea
Serial <- serial.test(model, lags.pt = 12, type = 'PT.asymptotic')
Serial # pvalue > 0.1 => nu avem autocorelare in reziduuri

# Heteroscedasticitate
Arch <- vars::arch.test(model,lags.multi = 1,multivariate.only = TRUE)
Arch # pvalue < 0.1 => avem heteroschedasticitate

# Normalitatea reziduurilor
Norm <- normality.test(model, multivariate.only = TRUE)
Norm # pvalue JB < 0.1 => reziduurile nu sunt normal distribuite

# Testarea pentru rupturi in serie
Stability <- stability(model,type = 'OLS-CUSUM')
plot(Stability) # model stabil in cazul cursului de schimb
# si putin instabil in cazul rezervelor internationale

# Cauzalitate Granger

Granger_curs <- causality(model, cause = "curs")
Granger_curs # p < 0.1 => Cursul de schimb prezinta cauzalitate Granger cu rezervele internationale
# Cursul de schimb ajută la prezicerea rezervelor internaționale

Granger_rezerve <- causality(model, cause = 'rezerve')
Granger_rezerve # p > 0.1 => Rezervele internationale nu prezinta cauzalitate Granger cu cursul de schimb
# Rezervele internaționale nu ajută la prezicerea cursului de schimb

# Functia de raspuns la impuls (IRF) 

curs_irf <- irf(model, impulse = 'curs', response = 'rezerve', 
                n.ahead = 20, boot = TRUE, ci=0.90) 
plot(curs_irf, ylab = 'curs', 
     main = 'Raspunsul cursului de schimb la socurile rezervelor internationale')

rezerve_irf <- irf(model, impulse = 'rezerve', response = 'curs', 
                   n.ahead = 20, boot = TRUE, ci=0.90)
plot(rezerve_irf, ylab = 'Vacanta', 
     main = 'Raspunsul rezervelor internationale la socurile cursului de schimb')

# Descompunerea variante

FEVD <- fevd(model, n.ahead = 12)
plot(FEVD) # aproximativ 10%
# Prognoza VAR
model_forecast <- VAR(df, p = 3, type = 'const', season = NULL, exog = NULL)
forecast <- predict(model_forecast, n.ahead = 12, ci = 0.90) # prognoza pe 1 an

plot(forecast, name = 'curs')
plot(forecast, name = 'rezerve')

fanchart(forecast, names='curs')
fanchart(forecast, names='rezerve')

# Cointegrare si VECM
library(tsDyn)
library(dynlm)
library(aTSA)

# Se va utiliza df-ul cu cele doua variabile
View(df)

# Testul Engle-Granger de detectare a cointegrarii

# H0: Seriile nu sunt cointegrate
# H1: seriile sunt cointegrate

# Pasul 1 - Testam stationaritatea seriilor
# Anterior am observat ca ambele serii sunt nestationare
# Si am observat ca dupa o diferentiere acestea devin stationare

# Pas 2: aplicam testul de cointegrare pe seriile reziduurilor

# d - operatorul de diferentiere pe care il vom seta la 1, deoarece diferentiem seriile o data pentru a deveni stationare
# Testul calculeaza o regresie simpla intre cele doua variabile si testeaza daca reziduurile sunt sau nu stationare

# H0 - seriile NU sunt cointegrate (adică nu există relație stabilă pe termen lung)
# H1 - seriile sunt cointegrate

coint.test(y = curs,X = rezerve,d = 1) # serii cointegrate
coint.test(y = rezerve,X = curs,d = 1) # serii cointegrate

# Conform testului Engle Granger, exista o relatie de cointegrare intre cele doua serii temporale (există o relație stabilă pe termen lung între ele)

# Modelul VECM
# Se va aplica metoda de estimare 2OLS (= Metoda Engle Granger), deoarece avem o singura relatie de cointegrare
model_vecm <- VECM(df,
                   lag = 2,   # la VAR nr optim de laguri a fost 3, asadar la VECM vom aveam 3-1 = 2 laguri
                   r=1, 
                   estim = ('2OLS'),
                   LRinclude = 'const')
summary(model_vecm)
# Relatia de cointegrare este data de randul r1
# Interpretare: cursul de schimb este variabila de referinta (pt ca are val 1)
# exista o relatie negativa intre rezervele internationale si cursul de schimb (-0.792)
# si constanta e pozitiva (≈ 0)
# Relatia de echilibru pe termen lung intre cele doua variabile: curs - 0.792 * rezerve = 0 => curs = 0.792 * rezerve

# ECT - error correction term - trebuie sa fie negativ si semnificativ ca sa avem 
# relatie pe termen lung

# Ecuatia cursului:
# ECT = -0.0169 => semnificatie marginala, exista o abatere pozitiva față de echilibru, cursul va scădea în perioada următoare pentru a reveni spre echilibru
# cursul la lagul 1 este semnificativ (0.291), relatie pe TS a cursului la lagul 1
# la lagul 2 este nesemnificativ
# rezervele nu au o relatie semnificativa cu cursul

# Ecuatia rezervelor:
# ECT = -0.0041 => nesemnificativ, rezervele nu au o relatie pe TL cu rezervele decalate
# coeficientii sunt nesemnificativi sau marginal semnificativi (la lagul 2 pentru curs), ceea ce inseamna ca dinamica rezervei nu este bine explicată de propriile laguri sau de lagurile cursului în acest model

# Asadar, cursul de schimb este variabila care corectează dezechilibrul pe termen lung, ajustându-se încet spre relația de echilibru cu rezervele
# Rezervele internationale nu par să reacționeze semnificativ la deviațiile de la echilibru în acest model
# Modelul indică o dinamică mai puternică și predictibilă pentru curs decât pentru rezerve

# Diagnosticul pe reziduuri

# Testul Johansen
vecm_jo <- ca.jo(df, type = 'trace', ecdet = 'const',K=2)
summary(vecm_jo) 

# Trebuie sa transformam obiectul VECM in obiect VAR
vecm_var <- vec2var(vecm_jo, r = 1)


# Autocorelarea
Serial <- serial.test(vecm_var, lags.pt = 12, type = 'PT.asymptotic')
Serial 

# Heteroscedasticitate
Arch <- vars::arch.test(vecm_var,lags.multi = 1,multivariate.only = TRUE)
Arch 

# Normalitatea reziduurilor
Norm <- normality.test(vecm_var, multivariate.only = TRUE)
Norm 

# Functia de raspuns la impuls (IRF) 

curs_irf <- irf(vecm_var, impulse = 'curs', response = 'rezerve', 
                n.ahead = 20, boot = TRUE, ci=0.90) 
plot(curs_irf, ylab = 'curs', 
     main = 'Raspunsul cursului de schimb la socurile rezervelor internationale')

# Descompunerea variante

FEVD <- fevd(vecm_var, n.ahead = 12)
plot(FEVD) 

# Prognoza VECM

forecast <- predict(vecm_var, n.ahead = 12, ci = 0.90) # prognoza pe 1 an

plot(forecast, name = 'curs')
plot(forecast, name = 'rezerve')

fanchart(forecast, names='curs')
fanchart(forecast, names='rezerve')

