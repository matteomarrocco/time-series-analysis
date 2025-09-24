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
library(quantmod) ## For downloading data

#### Functions: 
source("C:\\Users\\matte\\Downloads\\TSA-Finance-Functions (1).R") 
source("C:\\Users\\matte\\Downloads\\TSA-Finance-Functions.R")
source("C:\\Users\\matte\\Downloads\\TSA-Predict-Student-Functions (1).R")

## Garman-Klass
################################################################################

.garmanklass <-
  function(data, 
           sd = TRUE, currency = FALSE)
  {
    #### Auxiliary
    nobs  <- NROW(data)
    
    #### Intradaily
    ## Extract
    coef <- if ("Adjusted" %in% colnames(data))
    { 
      data$Adjusted / data$Close
    }
    else 
    { 
      1
    }
    H1 <- log( data$High * coef )
    L1 <- log( data$Low * coef )
    O1 <- log( data$Open * coef )
    C1 <- log( data$Close * coef )
    u1 <- H1 - O1
    d1 <- L1 - O1
    c1 <- C1 - O1
    ## Values
    x <- 0.511 * (u1 - d1)^2 +
      (-0.019) * (c1 * (u1 + d1) - 2 * u1 * d1) +
      (-0.383) * c1^2
    
    # x <- 0.5 * (H1 - L1)^2 - (2 * log(2) - 1) * (C1 - O1)^2
    #### Overnight adjustment
    if ( !currency[1] )
    {
      retco <- c(NA, log( data$Open[-1] / data$Close[-nobs] ) )
      retoc <- log( data$Close / data$Open )
      x1 <- sum( retco^2, na.rm = TRUE); x2 <- sum( retoc^2, na.rm = TRUE )  
      f  <- x1 / (x1 + x2)
      f[f < 0.01] <- 0.01; f[f > 0.99] <- 0.99
      a <- 0.12
      x <- a * retco^2 / f + ( (1 - a) / (1 - f) ) * x
    }
    
    #### Answer
    if ( sd ) { 1.034 * sqrt( x ) } else { x }
  }


file.data <- "C:\\Users\\matte\\OneDrive\\Desktop\\statistica economica\\TXN.csv"

#### Read
data <- read.table(file = file.data, header = TRUE, sep = "," ,  dec=".",
                   check.names = FALSE, comment.char = "")
head(data)
#### Add variables
library(data.table)

data <- data.frame(data, 
                   cc.ret = c(NA, diff(log(data$`Adj Close` ))), 
                   gkVol= .garmanklass(data=data,sd=TRUE),
                   check.names = TRUE)

head(data)
tail(data)
#### Extract period
ind   <- as.Date(x = "2018-01-01") <= as.Date(x = data$Date)
data  <- data[ind, , drop = FALSE]
NROW(data)
#### Extract variables
time  <- as.Date(x = data$Date)
yc    <- data$Close
yclog <- log(yc)
y     <- data$Adj.Close
ylog  <- log(y)


##Analysis of prices 

#### Auxiliary quantities
nobs <- NROW(y)
nobs
#### Plots
par(mfrow = c(2,2))
plot(x = time, y = yc,    main = "Close",        xlab = "", ylab = "", type = "l")
plot(x = time, y = yclog, main = "Ln(close)",    xlab = "", ylab = "", type = "l")
plot(x = time, y = y,     main = "AdjClose",     xlab = "", ylab = "", type = "l")
plot(x = time, y = ylog,  main = "Ln(AdjClose)", xlab = "", ylab = "", type = "l")


#### Serial correlation 
par(mfrow = c(2,1))
Acf(x = ylog, lag.max = 100, type = "correlation", main = "Price")
Acf(x = ylog, lag.max = 100, type = "partial", main = "Price")
#
#Spaghettone lungo 1 a lag=1 nella PACF e per il resto decadimento lineare
#Concludo: non è stazionario
#
#Non ci sono dubbi che NON è colpa della stagionalità
#Si può fare test ADF per certicare quello che vedo

### ADF tests using the Di Fonzo-Lisi procedure
adf.1 <- ur.df(y = ylog, type = "trend", lags = 20, selectlags = "AIC")
print( summary(adf.1) )
#
#
#
adf.2 <- ur.df(y = ylog, type = "drift", lags = 20, selectlags = "AIC")
print( summary(adf.2) )
#
#Accetto H0
#Conclusione: c'è UR
#
#adesso il primo passo da fare è l'analisi preliminare
#
# Preliminary analyses of log-returns
#
#### Percentage log-returns ( ho moltiplicato per 100 per renderli più leggibili)
yret <- xts(x = 100 * data$cc.ret, order.by = time) 

mean(yret) # E' pari a 0.05495199
head(yret)


######## Preliminary analysis
#### Time series
par(mfrow = c(1,1))
plot(x = time, y = yret, main = "Returns", 
     xlab = "", ylab = "", type = "l")
#
#Dal grafico notare che i log-rendimenti hanno media tendenzialmente intorno allo zero,
#e notare che con questi modelli noi cercheremo di diminuire la volatilità intorno alla media
#notare in particolare periodo di alta volatilità dal 2020(causa COVID)
#
#### Serial correlation
par(mfrow = c(2,1))
Acf(x = yret, lag.max = 100, type = "correlation", main = "Returns")
Acf(x = yret, lag.max = 100, type = "partial", main = "Returns")
#
#Normalmente i rendimenti stanno tutti dentro gli intervalli, ma può succedere(come in questo caso)
#che qua cia sia un' eccezione
#
#L-B test:
npar <- 0
lag <- c(2, 5, 10, 15, 20, 30, 50) + npar
lb <- mapply(FUN = Box.test, lag = lag, 
             MoreArgs = list(x = yret, type = "Ljung-Box", fitdf = npar))[1:3,]
print(rbind(lag = lag, lb))
#
# tutti i P-value del test sono  significativi, sono tutti zero
#Questa cosa è inusuale, in genere sono tutti NON signif.
#
#Faccio ulteriore diagnostica
#
#### Another diagnostic: the ARCH test

lag <- c(4, 8, 12, 16)
arch_test <- mapply(FUN = ArchTest, lags = lag, 
                    MoreArgs = list(x = yret, demean = TRUE))
print(arch_test[1:3,])
#Fatto a lag 4, 8, 12, 16 e non c'è verso di accettare H0.
#il test ARCH conferma che i rendimenti sono eteroschedastici.


#### ACF of residuals, abs residuals and squared residuals
par(mfrow = c(3,1))
Acf(x = yret, lag.max = 100, type = "correlation", main = "Returns")
Acf(x = abs(yret), lag.max = 100, type = "correlation", main = "|Returns|")
Acf(x = yret^2, lag.max = 100, type = "correlation", main = expression(Returns^2))
#
# Valori Fuori dagli intervalli
#
#Studio la situazione attraverso la visualizzazione di altri grafici
#
#
#### Unconditional distribution
par(mfrow = c(1,2))
.hist(x = yret, xlim = c(-10, 10), n = 200, breaks = 200, main = "Returns")
qqnorm(y = scale(yret))
abline(a = 0, b = 1, col = "red")

#print( jarque.bera.test(x = yret) )

#Vedo l'istogramma e il qq-plot dei rendimenti.
#effetto del volatility clustering, gonfia le code

## ARMA modeling 
################################################################################
## ARMA(1,0)-norm     Akaike 4.144412   Bayes 4.155741
## ARMA(0,1)-norm     Akaike 4.144412   Bayes 4.155741
## ARMA(1,1)-norm     Akaike 4.144021   Bayes 4.159126
## ARMA(0,1)-std      Akaike 4.030026   Bayes 4.045132
## ARMA(1,0)-std      Akaike 4.031529   Bayes 4.046634
spec0 <- arfimaspec(
  mean.model = list(armaOrder = c(0,1), 
                    include.mean = TRUE, external.regressors = NULL), 
  distribution.model = "std") 
fit0 <- arfimafit(spec = spec0, data = yret, 
                  solver = "solnp")

## Store the number of parameters
np0 <- NROW(fit0@fit$coef)

## Some statistics
print( infocriteria(fit0) ) #AIC/T e BIC/T
print( fit0@fit$matcoef ) #  coeff stimati
print( fit0@fit$robust.matcoef ) # coeff stimati robusti
#
#
#### ACF of residuals, abs residuals and squared residuals
res <- as.numeric( residuals(fit0) )
par(mfrow = c(3,1))
Acf(x = res, lag.max = 100, type = "correlation", main = "Returns")
Acf(x = abs(res), lag.max = 100, type = "correlation", main = "|res|")
Acf(x = res^2, lag.max = 100, type = "correlation", main = expression(res^2))

#ARIMA NON aggiusta l'eteroschedasticità.


## ARCH/GARCH modeling 
################################################################################
#Inizio a vedere: "sGARCH", garchOrder = c(1,1) è l'ordine del Garch con p=q=1. 

#### Simple GARCH
spec1 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1), 
                        submodel = NULL, external.regressors = NULL, variance.targeting = FALSE), 
  mean.model = list(armaOrder = c(0, 1), include.mean = TRUE,  
                    external.regressors = NULL), 
  distribution.model = "std")
fit1 <- ugarchfit(spec = spec1, data = yret, solver = "solnp")

## Store the number of parameters
np1 <- NROW(fit1@fit$coef)

## Some statistics
print( infocriteria(fit1) )
print( fit1@fit$matcoef )  # NON robusta
print( fit1@fit$robust.matcoef )  # robusta

#versione robusta:
#La media stimata è significativa, anche ma1 , (omega va sempre lasciato), 
#Non ci sono grosse differenze tra la versione robusta e non. 
# 


#### Diagnostics: Use standardized residuals
fit <- fit1
par(mfrow = c(3,1))
Acf(x = fit@fit$z,      lag.max = 100, type = "correlation", main = "z")
Acf(x = abs(fit@fit$z), lag.max = 100, type = "correlation", main = "|z|")
Acf(x = fit@fit$z^2,    lag.max = 100, type = "correlation", main = expression(z^2))

#### ACF of residuals, abs residuals and squared residuals
res <- as.numeric( residuals(fit1) )
par(mfrow = c(3,1))
Acf(x = res, lag.max = 100, type = "correlation", main = "Returns")
Acf(x = abs(res), lag.max = 100, type = "correlation", main = "|res|")
Acf(x = res^2, lag.max = 100, type = "correlation", main = expression(res^2))

#### ARCH test
lag <- c(4, 8, 12, 16)
at <- mapply(FUN = ArchTest, lags = lag, 
             MoreArgs = list(x = fit1@fit$z, demean = TRUE))
print(at[1:3,])
#Il modello ARCH fa bene il suo lavoro,in quanto sono tutti NON signif., ma non è ancora perfetto.
#Per i residui al quadrato vedo che non va bene


#Ora voglio guardare la distribuzione dei residui:

par(mfrow = c(1,2))
xlim <- c(-5, 5)
.hist.fit(fit = fit1, xlim = xlim, ylim = c(0,0.65), n = 200, breaks = 100, 
          plot.norm = TRUE, main = "")
.qqplot.fit(fit = fit1)
#COMMENTO L'ISTORAMMA E IL QQ-PLOT:
#Vediamo come la student-t rispetto alla normale funziona meglio, ma non ultra perfetto

#### Leverage check-> TEST DEI SEGNI
cat("\nSign bias test\n")
print( signbias(fit1) )
##non c'è niente di significativo
#
#ma per essere sicuro, faccio ugualmente GJR-GARCH

spec2 <- ugarchspec(
  variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1), 
                        submodel = NULL, external.regressors = NULL, variance.targeting = FALSE), 
  mean.model = list(armaOrder = c(0,1), include.mean = TRUE, 
                    external.regressors = NULL), distribution.model = "std")
fit2 <- ugarchfit(spec = spec2, data = yret, solver = "solnp")

## Store the number of parameters
np2 <- NROW(fit2@fit$coef)

## Some statistics
print( infocriteria(fit2) )
print( fit2@fit$matcoef )
print( fit2@fit$robust.matcoef )
#
#Posso fare anche il grafico
#
#
#### Compare the News Impact Curves (NIC)
par(mar=c(1,1,1,1))
ni1 <- newsimpact(z = NULL, fit1)
ni2 <- newsimpact(z = NULL, fit2)
legend <- c("Simple-GARCH", "GJR-GARCH")
col  <- c("black", "red")
ylim <- range( ni1$zy, ni2$zy)
par(mfrow = c(1,1), mar = c(4, 4.5, 3, 1) + 0.1)
plot(x = ni1$zx, y = ni1$zy, ylab = ni1$yexpr, xlab = ni1$xexpr, type = "l", 
     ylim = ylim, main = "News Impact Curve", col = col[1])
lines(x = ni2$zx, y = ni2$zy, col = col[2])
legend(x = "topright", y = NULL, legend = legend, border = FALSE, col = col, 
       lty = 1, text.col = col)
#
#Dal grafico si nota che nel S-Garch non c'è asimmetria,
#mentre nel GJR vi è una asimmetria ma non tanto esagerata
#
#

#### Stability check: nyblom()

print( nyblom(fit2) )
NROW(y)
#Noto che dovrei accorciare di qualche anno ancora
# Quindi ho rifatto tutto e come data di inizio avro 2018-01-01



## Alternative GARCH formulations via fGARCH: GJRGARCH and T-GARCH
################################################################################

# fit2 -> 'gjrGarch'

#### TGARCH (using fGARCH) 
spec5 <- ugarchspec(
  variance.model = list(model = "fGARCH", garchOrder = c(1, 1), 
                        submodel = "TGARCH", external.regressors = NULL, variance.targeting = FALSE),  
  mean.model = list(armaOrder = c(0, 1), include.mean = TRUE, 
                    external.regressors = NULL), 
  distribution.model = "std")
fit5 <- ugarchfit(spec = spec5, data = yret, solver = "solnp")

## Store the number of parameters
np5 <- NROW(fit5@fit$coef)
#### Conversion to the "traditional" GJR form
fit5c <- .fgarch.2.gjr(fit = fit5)

print( infocriteria(fit5) )
print(fit5c$robust.matcoef)
#
#Ora vedo di nuovo la Impact CURVE, con tutti e tre i modelli diversi sopra
#
#### Compare the News Impact Curves (NIC)
par(mar=c(1,1,1,1))
ni1 <- newsimpact(z = NULL, fit1)
ni2 <- newsimpact(z = NULL, fit2)
ni5 <- newsimpact(z = NULL, fit5)
legend <- c("Simple-GARCH", "GJR-GARCH", "T-GARCH")
col  <- c("black", "red", "blue")
ylim <- range( ni1$zy, ni2$zy, ni5$zy)
par(mfrow = c(1,1), mar = c(4, 4.5, 3, 1) + 0.1)
plot(x = ni1$zx, y = ni1$zy, ylab = ni1$yexpr, xlab = ni1$xexpr, type = "l", 
     ylim = ylim, main = "News Impact Curve", col = col[1])
lines(x = ni2$zx, y = ni2$zy, col = col[2])
lines(x = ni5$zx, y = ni5$zy, col = col[3])
legend(x = "topright", y = NULL, legend = legend, border = FALSE, col = col, 
       lty = 1, text.col = col)

#### CONFRONTO S-GARCH, GJR-GARCH e T-GARCH
print( infocriteria(fit1) )
print( infocriteria(fit2) )
print( infocriteria(fit5) )
#
#Guardo i valori e commento quale dovebbe essere migliore
#
#### Use standardized residuals!
fit <- fit5
par(mfrow = c(3,1))
Acf(x = fit@fit$z, lag.max = 100, type = "correlation", main = "z")
Acf(x = abs(fit@fit$z), lag.max = 100, type = "correlation", main = "|z|")
Acf(x = fit@fit$z^2, lag.max = 100, type = "correlation", main = expression(z^2))
#
#I residui^2 stand. non sono ancora perfetti
#

## Alternative GARCH specifications: iGARCH 

#### IGARCH 
spec6 <- ugarchspec(variance.model = list(model = "iGARCH", garchOrder = c(1, 1), 
                                          submodel = NULL, external.regressors = NULL, variance.targeting = FALSE),  
                    mean.model = list(armaOrder = c(0, 1), include.mean = TRUE,  
                                      external.regressors = NULL), distribution.model = "std")
fit6 <- ugarchfit(spec = spec6, data = yret, solver = "solnp")
## Store the number of parameters
np6 <- NROW(fit6@fit$coef)
## Some statistics
print( infocriteria(fit6) )
print( fit6@fit$matcoef )
print( fit6@fit$robust.matcoef )
#
#
#
#CONCLUSIONE:
#GJRGARCH (fit2) E T-GARCH (fit5) sono i modelli che porto in previsione poichè mi sembrano 
#meglio rispetto agli altri
#

#Da qui inizia la parte di PREVISIONE:attraverso un'analisi EX-POST



#Ho calcolato 'gkVol' (scritto sopra all'inizio pagina) prendendo come riferimento 
#la deviazione standard: sd = TRUE così facendo sto calcolando la volatilità, 
#se metto sd = FALSE calcolo la varianza. 
#gkVol lo uso come benchmark
#
#
#
#### External benchmark
y  <- data$gkVol * 100
mediagkVol=mean(data$gkVol)
mediagkVol
mean(fit2@fit$sigma)
par(mfrow = c(1,1), lwd = 1)
plot(x = time, y = y, type = "l", ylab = "Garman-Klass volatility measure")
lines(x = time, y = fit2@fit$sigma, col = "red")
lines(x = time, y = fit5@fit$sigma, col = "blue")
legend( x = "topleft",
        legend = c( "GJR-GARCH", "T-GARCH"),
        col = c("red", "blue"), lwd = 2, lty = c(0,0),
        pch=c(17,19))
#
#Sia T-Garch che GJR-GARCH producono stime tendenzialmente minori di volatilità, 
#rispetto a quelle effettuate da Garman-Klass
#
#

#### Set naive 

naive.vol <- sd(yret)  		
naive.var <- naive.vol^2  	
#
#
#
#### MISURE DI ERRORE

ErrorMeas <- data.frame(
  measure = c("Volatility", "Volatility", "Variance", "Variance"), 
  model = c("GJR-GARCH", "T-GARCH", "GJR-GARCH", "T-GARCH" ), 
  rbind(  
    .ErrorMeasures(y = y,   fit = fit2@fit$sigma,   naive = naive.vol), 
    .ErrorMeasures(y = y,   fit = fit5@fit$sigma,   naive = naive.vol),  
    .ErrorMeasures(y = y^2, fit = fit2@fit$sigma^2, naive = naive.var), 
    .ErrorMeasures(y = y^2, fit = fit5@fit$sigma^2, naive = naive.var) ) ) 
print( ErrorMeas )



#vediamo le misure scalate: ScMAE e ScRMSE. Sulla volatilità generalmente è meglio il T-Garch,
#proprio perchè è un modello costruito sulla volatilità; 
#esso è leggermente migliore anche sulla varianza.
##comunque la scelta non è tanto faicle perche i valori dei due modelli sono molto vicini

#Facciamo altre diagnostiche sulle previsioni:

#### Mincer-Zarnowitz forecasting diagnostics
x1 <- .MincerZarnowitz(y = y, fit = fit2@fit$sigma, msg = "GJR-GARCH\n")
x1 <- .MincerZarnowitz(y = y, fit = fit5@fit$sigma, msg = "T-GARCH\n")
#
#GUardo HAC p.value e sono tutti altamente significativi
#
#Faccio altri controlli
#
#### Diebold-Mariano forecasting comparison
## Volatility
cat("Volatility\n")
h <- 1
e2 <- y - fit2@fit$sigma # GJR-GARCH
e5 <- y - fit5@fit$sigma # T-Garch
.DieboldMariano(e1 = e2, e2 = e5, h = h, power = 1, msg = "GJR-GARCH vs T-GARCH   ->")
.DieboldMariano(e1 = e2, e2 = e5, h = h, power = 2, msg = "GJR-GARCH vs T-GARCH   ->")
#
#GJR con power=1 è peggiore di T-garch ma fornisconoprevisioni simili,percè p-value non è sign
#GJR con power=2 è significativa contro H0,e mi dice che GJR-Garch fa previsioni migliori
#
#Siccome sono insicuro, li porto entrambi in previsione
#
## Forecasts using rugarch
#### Settings
H <- 10

#### 1) ex-ante, h = 1:H

forc1.1 <- ugarchforecast(fitORspec = fit2, n.ahead = H, 
                          data = NULL, out.sample = 0, n.roll = 0)
forc1.5 <- ugarchforecast(fitORspec = fit5, n.ahead = H, 
                          data = NULL, out.sample = 0, n.roll = 0)
forc1.1
forc1.5
#
#Commenti sulle previsioni...
# 