install.packages("tseries")
install.packages("forecast")
install.packages("forecastHybrid")
library(tseries)
library(TSA)
library(stats)
library(forecast)
library(forecastHybrid)

##Träningsdata:

SMHI <- read_excel("SMHI_träning.xlsx")

str(SMHI)

SMHI$Temperatur <- as.numeric(SMHI$Temperatur)
  
plot(SMHI$Temperatur)

dataset_test <- SMHI %>% 
  select(!c(Datum))

anyNA(dataset)

SMHI$Datum <- as.Date(SMHI$Datum, format="%Y-%m-%d")

dagliga_medelvärden_test <- SMHI %>%
  group_by(Datum) %>%
  summarise(medel_temperatur = mean(Temperatur))

print(dagliga_medelvärden_test)

print(dagliga_medelvärden_test)

ts_temperatur <- ts(dagliga_medelvärden_test)

##Test:

SMHI_test$Temperatur <- as.numeric(SMHI_test$Temperatur)

SMHI_test$Datum <- as.Date(SMHI_test$Datum)

plot(SMHI_test$Temperatur)

dataset_test1 <- SMHI_test %>% select(!c(Datum))

str(SMHI_test)

SMHI_test$Datum <- as.Date(dataset_test1$Datum, format="%Y-%m-%d")

dagliga_medelvärden_test1 <- SMHI_test %>%
  group_by(Datum) %>%
  summarise(medel_temperatur = mean(Temperatur))

dagliga_medelvärden_test1

print(dagliga_medelvärden_test1)

str(dagliga_medelvärden_test1)

dagliga_medelvärden_test1 <- head(dagliga_medelvärden_test1, -1)

data <- dagliga_medelvärden_test1

anyNA(data)

str(data)

##Modeller

plot(data)

acf <- acf(ts_temperatur)
pacf <- pacf(ts_temperatur)
eacf <- eacf(ts_temperatur)

plot(acf)
plot(pacf)

#Eftersom modellen har en svagt avtagande PACF tyder detta på att det är en AR(p)-process.

#Enbart lag 1 som är signifikant skilt från 0 och därav är det en AR(1):a

#Test för stationäritet MHA Augmented Dickey Fuller test
#H0 = Non-stationarity
#H1 = Staionarity

Dickey_fuller <- adf.test(ts_temperatur, k = 1)
Dickey_fuller


#Enligt Dickey Fuller testet är det en stationär process.

ljung_box_test <- Box.test(ts_temperatur, lag = 1, type = "Ljung-Box")
ljung_box_test

#Hur som helst, när vi utför ett Ljung Box test förkastar vi H0 vilket tyder på att våra Residualerna inte är oberoende fördelade; de uppvisar seriell korrelation.

# Vi testar att differentiera tidsserien för att se ifall vi får några bättre resultat

diff1 <- diff(ts_temperatur)

plot(diff1)

ljung_box_test <- Box.test(diff1, lag = 1, type = "Ljung-Box")
ljung_box_test

Dickey_fuller_diff <- adf.test(diff1, k = 1)
Dickey_fuller_diff

auto.arima(ts_temperatur)

acf(diff1)
pacf(diff1)
eacf(diff1)

#Efter att ha differentierat den en gång är det tydligt att den är både stationär och att residualerna är oberoende fördelade; de uppvisar ingen seriell korrelation

### Prediktioner:

##För AR(1):


model<-Arima(ts_temperatur, order = c(1,0,0))
model

checkresiduals(model)

prediktioner <- forecast(model, h = 14,)
prediktioner

##För ARIMA(0,1,0)


model1 <- Arima(ts_temperatur,order=c(0,1,0))
model1
checkresiduals(model1)

prediktioner1 <- forecast(model1, h = 14)
prediktioner1

##Plottar:

#AR(1):

#Arima(0,1,0):


### Press-värden:

press_ar1 <- sum((dagliga_medelvärden_test$medel_temperatur - prediktioner$mean)^2)
print(press_ar1)

press_arima <- sum((dagliga_medelvärden_test$medel_temperatur - prediktioner1$mean)^2)
print(press_arima)
