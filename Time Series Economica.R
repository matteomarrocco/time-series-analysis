rm(list = ls())

#### Libraries
library(tseries)  
library(sandwich)
library(lmtest)
library(urca)     ## For unit root
library(rugarch)  ## For GARCH models
library(FinTS)    ## For ArchTest (download from RForge)
library(car)
library(forecast) 
library(xts)      ## For time stamps
library(quantmod)

library(tsoutliers)
library(forecast)
library(tseries)
#
#
#Inserimento dei dati
#
#
Table_9_10_Natural_Gas_Prices_2_ <- read_excel("C:/Users/matte/Downloads/Table_9.10_Natural_Gas_Prices (2).xlsx", 
                                               skip = 10)
View(Table_9_10_Natural_Gas_Prices_2_)
datia=Table_9_10_Natural_Gas_Prices_2_
datia
datia=datia[,c("Month","Natural Gas Price, Delivered to Consumers, Residential")]
datia=datia[-1,]
datia
str(datia)
indprova= datia$`Natural Gas Price, Delivered to Consumers, Residential`!= "Not Available" 
prova=datia[indprova, ,drop=FALSE]
prova=prova[-c(1:48),]
prova$`Natural Gas Price, Delivered to Consumers, Residential`=as.numeric(prova$`Natural Gas Price, Delivered to Consumers, Residential`)
prova$Month=as.Date(x=as.character(prova$Month),format="%Y-%m-%d")
start=as.numeric(c(format(prova$Month[1],"%Y"),format(prova$Month[1],"%m")))
y=ts(data=(prova$`Natural Gas Price, Delivered to Consumers, Residential`),start = start,frequency=12)
head(y)

######################################
#comincio con l'analisi preliminare
#################################

#prima cosa: TS Plot, ACF, PACF della serie originale
par(mfrow = c(3,1))
x <- y
plot( x, type = "l", ylab ="",main="Natural Gas Price")
range(x)
Acf(x = x, type = "correlation", na.action = na.pass, lag.max = 50,main="Natural Gas Price")
Acf(x = x, type = "partial",     na.action = na.pass, lag.max = 50,main="Natural Gas Price")

#
#ad intuito, il p.s. non mi sembra stazionario; 
# Dall' ACF: sembra anche esserci forte presenza della 
#componente stagionale e non stagionale: ha la forma onda lunga
#
#
#Guardo subito l'ACF e vedendo tutti i lag signif nei multipli di S, ne deduco causa stagionalità.
#I picchi che si ripetono ad intervalli di ampiezza regolare pari a s = 12 e ai suoi multipli,
#sono indice della presenza della componente
#stagionale della serie e contribuiscono alla non stazionarietà in media.
#
#Prima cosa da capire è se il processo è stazionario oppure no
#Per questo parto subito con l'analisi dell UR
#
#
#il test migliore da fare è l'ADF
#Per ragioni teoriche è meglio fare l'ADF 
#
##    Model: AR(1) + trend (+ other possible stationary terms))
adf.1 <- ur.df(y = y, type = "trend", lags = 24, selectlags = "AIC")
cat("\n-----\nTest1: ADF with trend\n")
print( adf.1@teststat )
print( adf.1@cval )
#per TAU3 e PHI3; accetto per entrambi
#quindi devo andare al passo due
#Per farlo, devo cambiare il type:
adf.2 <- ur.df(y = y, type = "drift", lags = 24, selectlags = "AIC")
print( adf.2@teststat )
print( adf.2@cval )
#vado a guardare TAU2; accetto l'ipotesi H0, 
#NON rifiuto, quindi c' è UR
#
#Siccome credo che ci sia UR per colpa della parte stagionale, adesso devo fare il test
#sulle differenze dodicesime e se troverò un'altra UR vorrà dire che sarà colpa della parte
#non stagionale
#Procedo subito a fare l'augmented test.

adf.1.a <- ur.df(y = diff(y,lag=12), type = "trend", lags = 24, selectlags = "AIC")
cat("\n-----\nTest1: ADF with trend\n")
print( adf.1.a@teststat )
print( adf.1.a@cval )
#Guardo TAU3 e PHI3. Accetto per entrambe
#devo andare a fare il passo due
#
adf.2.a <- ur.df(y = diff(y,lag = 12), type = "drift", lags = 24, selectlags = "AIC")
print( adf.2.a@teststat )
print( adf.2.a@cval )
#Mi concentro su PHI2
#accetto anche per phi2, c' è UR
#Concludo: UR per "colpa" della parte non stagionale
#A questo punto, provo con le differenze prime per vedere 
#se c'è UR per colpa della parte Stagionale

adf.1.b <- ur.df(y = diff(y,lags=1), type = "trend", lags = 24 , selectlags = "AIC") 
adf.1.b@teststat
adf.1.b@cval 
#Guardo TAU3 e PHI3: entrambe fra 5% e 10%
#Mi porterebbe a non accettare H0; quindi UR
#Ma siccome la situazione è un pò borderline, faccio ugualmente
#il test sul drift
adf.2.b <- ur.df(y = diff(y,lags=1) , type = "drift", lags = 24, selectlags = "AIC")
adf.2.b@teststat
adf.2.b@cval
#Guardo TAU2: anche qui borderline, 
#Per il momento accetto, c'è UR
#Concludo: Ce n'è un'altra per colpa della parte stagionale
#implica che D=1

#Adesso per vedere, faccio il grafico dell'acf e pacf 
#delle differenze prime e dodicesime

par(mfrow = c(3,1))
x <- diff(y,lags=1)
plot(x, type = "l", main = "graph diff prime", ylab = "")
Acf(x = x, type = "correlation", na.action = na.pass, lag.max = 24, main = "Unemployment")
Acf(x = x, type = "partial",     na.action = na.pass, lag.max = 24, main = "Unemployment")

#e ora sulle diff dodicesime 

par(mfrow = c(3,1))
x <- diff(y,lags=12)
plot(x, type = "l", main = "graph diff dodicesime", ylab = "")
Acf(x = x, type = "correlation", na.action = na.pass, lag.max = 24, main = "Unemployment")
Acf(x = x, type = "partial",     na.action = na.pass, lag.max = 24, main = "Unemployment")
#
#passiamo ora ad altri test per capire se vi è 
#la presenza di UR
#Siamo al caso del test KPSS

##### KPSS tests (to do)
cat("\n-----\nKPSS with tau\n")
kpss.1 <- ur.kpss(y = y, type = "tau", lags = "long", use.lag = NULL)
print( kpss.1@teststat )
print( kpss.1@cval )
#
#il coeff. è significativa contro H0: questo implica che c'è UR
#
#
cat("\n-----\nKPSS with mu\n")
kpss.2 <- ur.kpss(y = y, type = "mu", lags = "long", use.lag = NULL)
print( kpss.2@teststat )
print( kpss.2@cval )
#
#
#
#
#A seguire, vi è un altro test per testare sia UR stagionali
#che UR non stagionali
#Quindi applichiamo il test HEGY

#### HEGY tests (to do)
library(uroot)
hegy.1 <- hegy.test(x = y, deterministic = c(1, 0, 0),
                    maxlag = 12, lag.method = "AIC", # lag.method = c("fixed", "aic", "bic", "aicc"),
                    pvalue = "RS",                   # pvalue = c("rs", "bootstrap", "raw"),
                    rs.nobsreg = 15)
print(hegy.1)

#
#t_1 non è significativa, mentre F_1:12 è signif
#conclusione che d=1, mentre D=0?
#
#procediamo facendo valutazioni occhiometriche
#
#passiamo a capire gli ordini esatti e precisi del modello
# ARIMA(0,1,0)x(0,0,0)12:   AIC=1264.97   AICc=1264.98   BIC=1269.08        
# ARIMA(0,1,0)x(1,0,0)12:   AIC=1047.42   AICc=1047.45   BIC=1055.64        
# ARIMA(1,1,0)x(1,0,0)12:   AIC=983.12    AICc=983.17    BIC=995.45        
# ARIMA(1,1,1)x(1,0,0)12:   AIC=818.44    AICc=818.53    BIC=834.88        
# ARIMA(1,1,2)x(1,0,0)12:   AIC=804.46    AICc=804.59    BIC=825        
# ARIMA(2,1,1)x(1,1,0)12:   AIC=686.06    AICc=686.2     BIC=706.47        
# ARIMA(2,1,1)x(1,1,1)12:   AIC=626.77    AICc=626.96    BIC=651.26        
# ARIMA(1,1,1)x(1,1,1)12:   AIC=617.33    AICc=617.47    BIC=637.75        
        


xreg <- NULL
fit <- Arima(y = y, 
             order = c(1, 1, 1), seasonal = list(order = c(1, 1, 1)),
             xreg = xreg, include.constant = TRUE)
print(summary(fit))
fit1 <- fit
coeftest(fit1)
#parto col mettere ARIMA D=1, ovvero (0,0,0)x(0,1,0) e vado a vedere subito l'ACF
#dei residui, dove cercherò di capire se mettere d=0 o d=1
#
#
#
## Only in case of transformed variable
#llstats.adj1 <- .loglik(fit = fit1, g = "log10")
#llstats.adj1

## vado a vedere subito i residui, dovrò vedere se hanno un comportamento WN
fit <- fit1
#### Useful quantities
npar1  <- NROW(fit$coef)                            ## Number of parameters
lag1   <- npar1 + c(1, 2, 5, 10, 15, 20)
res1   <- residuals(fit)                            ## Residuals
resst1 <- ( res1 - mean(res1) ) / sqrt(fit$sigma2)  ## Standardized residuals

#### Ts plot, acf, pacf, Ljung-Box of residuals
par(mfrow = c(3,1))
main <- "residuals"
x1 <- res1
plot(x1, type = "l", main = main, ylab = "")
Acf(x = x1, type = "correlation", lag.max = 60, na.action = na.pass, main = main)
Acf(x = x1, type = "partial",     lag.max = 60, na.action = na.pass, main = main)
#
#quindi il modello sarà ARIMA (1,1,1)x(1,1,1)
#
#
#adesso parliamo degli effetti di calendario
#
#### Calendar effects

cal <- .calendarEffects(time = prova$Month ,country = "it")
#### Select
#cal <- cal[, c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun", "sh", "lh", "eh"), drop = FALSE]
#cal <- cal[, c( "Tue", "Wed", "Thu", "Fri", "Sat", "Sun", "sh", "lh", "eh"), drop = FALSE]
#cal <- cal[, c( "Wed", "Thu", "Fri", "Sat", "Sun", "sh", "lh", "eh"), drop = FALSE]
#cal <- cal[, c(  "Thu", "Fri", "Sat", "Sun", "sh", "lh", "eh"), drop = FALSE]
#cal <- cal[, c( "Fri", "Sat", "Sun", "sh", "lh", "eh"), drop = FALSE]
#cal <- cal[, c( "Sat", "Sun", "sh", "lh", "eh"), drop = FALSE]
#cal <- cal[, c( "Sun", "sh", "lh", "eh"), drop = FALSE]
#cal <- cal[, c( "sh", "lh", "eh"), drop = FALSE]
#cal <- cal[, c( "sh", "eh"), drop = FALSE]
#cal <- cal[, c( "sh" ), drop = FALSE]
cal <- as.matrix(cal)

#### Handmade drift (sometimes useful in tso())
drift <- cbind(drift = 1 : NROW(y))

#### ARIMA + external regressors
xreg <- cal
fit <- Arima(y = y,
             order = c(1, 1, 1), seasonal = list(order = c(1, 1, 1)),
             xreg = xreg, include.constant = TRUE)
print(summary(fit))
fit2 <- fit
coeftest(fit2)
#tolgo il lunedì,martedì,mercoledi,giovedì,...
#Sono tutti non significativi, quindi non ci sono nel modello
#

## Only in case of transformed variable
#llstats.adj2 <- .loglik(fit = fit, g = "log")
#llstats.adj2

#
#Ora faccio con le anomalie
### ARIMA modeling with anomalies
#####################################
## ARIMA
#####################################
#### Copy model
xreg=NULL
fit <- Arima(y = y, order = c(1, 1, 1), seasonal = list(order = c(1, 1, 1),
                                                        period = 12), xreg = xreg , include.constant = TRUE)
print(summary(fit1))
fit2 <- fit

#### Extract settings
settings <- .Arima.settings(fit = fit)

#uso del drift
drift <- cbind(drift = 1 : NROW(y))

#### Prepare xreg
xreg <- NULL
xreg <- if ( settings$include.drift ) { cbind(drift, xreg) } else { xreg }
#### Fit
fit <- tso(y = y, xreg = xreg,
           types = c("AO", "LS", "TC"), delta = 0.7, cval = 4,
           maxit = 10, maxit.iloop = 100, maxit.oloop = 10,
           # tsmethod = "auto.arima",
           # args.tsmethod = list(allowdrift = false, ic = "bic", trace = true) )
           tsmethod = "arima",
           args.tsmethod = list( order = settings$order, seasonal = settings$seasonal) )
fit1.o <- fit

#### Reporting
print(fit1.o)
plot(fit1.o)

#
#Ci sono due anaomalie: LS 249, TC 250
#
#
#
#### Extract outlier effects
oeff <- outliers.effects(mo = fit$outliers, n = NROW(y), pars = coef(fit$fit),
                         weights = FALSE)
## Plot weighted effects
# plot(x = data1$Time, y = rowSums(oeff), type = "l")
#### Estimate again
xreg <- as.matrix( oeff )
fit <- Arima(y = y,
             order = settings$order, seasonal = settings$seasonal,
             include.constant = settings$include.constant,
             xreg = xreg)


## Only in case of transformed variable
#llstats.adj3 <- .loglik(fit = fit, g = "log")


fit3 <- fit
fit3

#ottengo un modello fit3 che sembrerebbe essere uguale a quello ARIMA
#Adesso, io mi sono creato tutti i miei  modelli dove so che fit1= ARIMA "normale"
# fit3=ARIMA+Outliers,
#
#Valuto fit3,

#### Select the model
fit <- fit3

#### Useful quantities
npar1  <- NROW(fit$coef)                            ## Number of parameters
lag1   <- npar1 + c(1, 2, 5, 10, 15, 20)
res1   <- residuals(fit)                            ## Residuals
resst1 <- ( res1 - mean(res1) ) / sqrt(fit$sigma2)  ## Standardized residuals

#### Ts plot, acf, pacf, Ljung-Box of residuals
par(mfrow = c(3,1))
main <- "residuals"
x1 <- res1
plot(x1, type = "l", main = main, ylab = "")
Acf(x = x1, type = "correlation", lag.max = 60, na.action = na.pass, main = main)
Acf(x = x1, type = "partial",     lag.max = 60, na.action = na.pass, main = main)
#
#
#ACF di fit3 non è perfetto a un WN, ha dei lag significativi
#
# 
cat("\n", paste("Ljung-Box of", main, "at different lags\n") )
lb <- mapply(FUN = Box.test, lag = lag1, 
             MoreArgs = list(x = x1, type = "Ljung-Box", fitdf = npar1))[1:3, , drop = FALSE]
print(lb)
#
#LB test di fit3 mi da tutti i lag significativi
#
#### Ts plot, acf of residuals^2
par(mfrow = c(2,1))
main <- "residuals^2"
x1 <- res1^2
plot(x1, type = "l", main = main, ylab = "")
Acf(x = x1, type = "correlation", lag.max = 60, na.action = na.pass, main = main)
#
#
#I residui^2 di fit3 mi dicono che che c'è correlazione e sembrano un onda lunga
#
#### Ts plot, acf of |residuals|
par(mfrow = c(2,1))
main <- "|residuals|"
x1 <- abs(res1)
plot(x1, type = "l", main = main, ylab = "")
Acf(x = x1, type = "correlation", lag.max = 60, na.action = na.pass, main = main)
#
#
#I |resiudi| di fit3 sono al di fuori delle bande, quindi non hanno buona qualità
#
#### Another diagnostic: the ARCH test
cat("\n-----------------------------------------------------------------
  ARCH based preliminary analyses\n")
cat("ARCH test on demeaned log-returns\n")
lag <- c(1, 2, 3, 6, 12, 24)
at <- mapply(FUN = ArchTest, lags = lag, 
             MoreArgs = list(x = x1, demean = TRUE))
print(at[1:3,])
#
#anche l'ARCH test di Fit3 mi dice che c'è eteroschedasticità fra i residui
#
#### Check for transformation to stabilize variance
trt <- .trsf.test(fit = fit, msg = "Transformation check on 'fit2'\n")
#
#Questo test per fit3 i dice che pur mettendo mano ai residui non correggo niente
#
#L'ultimo passaggio da fare è la verifica di normalità
#
#### Unconditional distribution of residuals
## Plot
par(mfrow = c(1,2))
hist(x = resst1, breaks = 25, freq = FALSE, main = "residuals", xlab = "")
x1 <- seq(from = min(resst1), to = max(resst1)+1, length.out = 100) 
lines(x = x1, y = dnorm(x = x1, mean = 0, sd = 1), col = "red")
qqnorm(y = resst1, main = "Normal Q-Q Plot",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE)
abline(a = 0, b = 1, col = "red")
## Test of normality 
#cat("\nTest of normality\n")
#print( shapiro.test(x = res1 ) )
#
###########
## Ex-post forecasts: all made 1-step ahead
###########

#### Settings
J  <- 12       #quante oss. lascio fuori per calcolare le misure di errore                                 
H  <- 1        #orizzonte di previsione                                        
t1 <- .predict.t1(nobs = NROW(y), J = J, n.ahead = H) 
#
#
#### No external regressors
pred1.1 <- .predict(object = fit1, n.ahead = H, t = t1, y = y,
                    fixed.n.ahead = TRUE)
pred1.1



#### If we have outliers
newxreg <- .oeff.4.predict(object = fit1.o, n.ahead = 0) #n.ahead = 0 NON voglio aggiungere osservazioni
pred3.1 <- .predict(object = fit3, n.ahead = H, t = t1, y = y, xreg = newxreg,
                    fixed.n.ahead = TRUE)
pred3.1

#
#Modello con outliers sembra leggermente migliore
#

#### Naive-> MODELLO INGENUO CHE PARTORISCE LE PREVISIONI
predn.1 <- .predict.naive(fit = fit3, J = J, n.ahead = H)

#### Bands
x3 <- .pred.bands(pred = pred3.1, alpha = 0.05)

#### Error Measures
#
em3.1  <- .ErrorMeasures(y = y, fit = x3$mean, naive = predn.1)
emn.1  <- .ErrorMeasures(y = y, fit = predn.1, naive = predn.1)

ErrorMeas <- data.frame(
  model = c( "Arima + Outliers", "Naive"),
  h = H, rbind( em3.1, emn.1, deparse.level = 0 ) )
print( ErrorMeas )


#### Plot
ind  <- (NROW(y) - J + 1) : NROW(y)
ind1 <- 1 : NROW(ind)
par(mfrow = c(1,1))
#ylim <- range( x3$lower[ind1], x3$upper[ind1] )
ylim <- range( min(predn.1,x3$lower[ind1]), max(predn.1,x3$upper[ind1]))

time <- prova$Month[ind]
plot(x = time, y = y[ind], ylim = ylim,
     main = "(Ex-post) Forecasts of the past 12 months", xlab = "time", ylab = "index")
lines(x = time, y = x3$mean[ind1],  col = "red")
lines(x = time, y = predn.1[ind1],  col = "green", lty = "solid")
lines(x = time, y = x3$lower[ind1], col = "cyan", lty = "dotted")
lines(x = time, y = x3$upper[ind1], col = "cyan", lty = "dotted")
legend( x = "topleft",
        legend = c("Naive", "ARIMA +Ouliers", "bands"),
        col = c("green","red", "cyan"), lwd = 2, lty = c(0,0),
        pch=c(17,19))
#
#Il modello non si adatta perfettamente ai dati, in quanto alcune previsioni si trovano 
#al di fuori delle bande


############
## Ex-ante (genuine) forecasts: from 1 to H steps ahead
############

#### Settings
H  <- 12      ## Massimo orizzonte di previsione. Setto da h=1 a h=12 !
t1 <- NROW(y) ## t1=T ovvero l'ultima osservazione nell'informazione I_T

#### If we have outliers
x2 <- .oeff.4.predict(object = fit1.o, n.ahead = H)
newxreg <- x2
pred3 <- .predict(object = fit3, n.ahead = H, t = t1, y = y, xreg = newxreg,
                  fixed.n.ahead = FALSE)
#Naive
predn <- .predict.naive(fit = fit3, J = H, n.ahead = H) 

#### Bands
x3 <- .pred.bands(pred = pred3, alpha = 0.05)

#### Print
print( cbind(t = x3$t, pred3 = x3$mean, naive = predn) )

#### Plot
par(mfrow = c(1,1))
ylim <- range( x3$lower, x3$upper, predn )
time <- prova$Month[ind]
plot(x = time, y = x3$mean, type = "l",
     main = "(Ex-ante) Forecasts of the next 12 months", xlab = "time", ylab = "index",
     ylim = ylim, col= 'red')
lines(x = time, y = x3$lower, col = "cyan", lty = "dotted")
lines(x = time, y = x3$upper, col = "cyan", lty = "dotted")
lines(x = time, y = predn, col = "green")
legend( x = "topleft",
        legend = c("ARIMA +Ouliers","Naive","Bands"),
        col = c("red","green","cyan"), lwd = 2, lty = c(0,0),
        pch=c(17,19))

#
#Fatto vedere al ricevimento che ho provato anche con le trasformazioni ln e sqrt, ma non ci sono miglioramenti evidenti
#in quanto i dati analizzati sono molto molto eteroschdastici
##così ho preferito analizzare la t.s. sui dati originali

