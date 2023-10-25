# Charger ou installer les packages nécessaires
required_packages <- c("quantmod", "ggplot2", "tseries", "forecast")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(required_packages, library, character.only = TRUE)

# Définir les périodes d'intérêt
start_date <- as.Date("2020-01-01")
end_date <- as.Date("2022-12-31")

# Obtenir les données du prix de l'or
getSymbols("GOLD", src = "yahoo", from = start_date, to = end_date)

# Accéder aux données de prix de clôture quotidiennes
gold_prices <- data.frame(Date = index(GOLD), GOLD.Close = Cl(GOLD))

# Tracer le prix de clôture
ggplot(gold_prices, aes(x = Date, y = GOLD.Close)) +
  geom_line() +
  labs(title = "Gold Price Over Time", x = "Date", y = "Close Price") +
  theme_minimal()

# Afficher des statistiques descriptives
summary_stats <- summary(gold_prices$GOLD.Close)
cat(paste("Mean: ", mean(gold_prices$GOLD.Close), "\n",
          "Median: ", median(gold_prices$GOLD.Close), "\n",
          "Min: ", min(gold_prices$GOLD.Close), "\n",
          "Max: ", max(gold_prices$GOLD.Close), "\n",
          "Standard Deviation: ", sd(gold_prices$GOLD.Close), "\n"))
print(summary_stats)

# Définir la première partie des données
end_date_1erpartie <- as.Date("2022-10-01")
gold_price_1erpartie <- gold_prices[gold_prices$Date <= end_date_1erpartie,]

# Tracer le prix de clôture pour la 1ère partie
ggplot(gold_price_1erpartie, aes(x = Date, y = GOLD.Close)) +
  geom_line() +
  labs(title = "Gold Price (Jan 2020 - Oct 2022)", x = "Date", y = "Close Price") +
  theme_minimal()

# Test de stationnarité avec le test de Dickey-Fuller augmenté
adf_test <- adf.test(gold_price_1erpartie$GOLD.Close, alternative = "stationary")

# Afficher le résultat du test
print(adf_test)

#on rejete l'hyposthese de stationnarité

# Calculer la série différenciée
gold_price_diff <- diff(gold_price_1erpartie$GOLD.Close)

# Créer un nouveau data.frame avec les dates (en omettant la première) et les valeurs différenciées
gold_price_diff_df <- data.frame(
  Date = gold_price_1erpartie$Date[-1],  # Omettre la première date
  GOLD_Diff = gold_price_diff  # Utiliser les valeurs différenciées
)

# Tracer la série différenciée
ggplot(gold_price_diff_df, aes(x = Date, y = GOLD_Diff)) +
  geom_line() +
  labs(title = "Differenced Gold Price Over Time",
       x = "Date",
       y = "Differenced Close Price") +
  theme_minimal()

adf_test_diff <- adf.test(gold_price_diff, alternative = "stationary")
print(adf_test_diff)

#la serie differnetié est stationnaire

# Calculer et tracer la fonction d'autocorrélation (ACF)
acf(gold_price_diff, main="Autocorrelation Function for Differenced Gold Price")

# Calculer les valeurs d'ACF sans les tracer
acf_values <- acf(gold_price_diff, plot=FALSE)

# Afficher les valeurs d'ACF
print(acf_values)


# a titre de compariason,on va calculer et tracer la fonction d'autocorrélation (ACF) pour la série non différenciée
acf(gold_price_1erpartie$GOLD.Close, main="Autocorrelation Function for Gold Price")

# Calculer les valeurs d'ACF sans les tracer
acf_values_non_diff <- acf(gold_price_1erpartie$GOLD.Close, plot=FALSE)

# Afficher les valeurs d'ACF
print(acf_values_non_diff)


#Série non-différenciée
#ACF: Les autocorrélations sont très élevées et diminuent lentement avec le décalage, ce qui est une indication claire d'une série non-stationnaire. Cela est dû à la forte tendance visible dans les données brutes, qui se manifeste sous forme d'autocorrélations élevées à des décalages plus grands.
#Série différenciée
#ACF: Les autocorrélations ne sont pas systématiquement élevées, mais il existe certains décalages (9, 17, 26) où les autocorrélations sont statistiquement significatives. Cela pourrait indiquer une sorte de saisonnalité ou de cycle dans les données qui n'est pas capturé par la différenciation simple.
#Analyse et interprétation :
#La tendance forte dans la série originale a été abordée par la différenciation, comme le montrent les autocorrélations proches de zéro dans la série différenciée, en comparaison avec l'ACF de la série non-différenciée.

#Les pics significatifs dans l'ACF de la série différenciée suggèrent une saisonnalité ou un autre type de structure périodique dans les données que vous pourriez explorer plus avant. Cela pourrait nécessiter un modèle SARIMA (Seasonal ARIMA) plutôt qu'un modèle ARIMA non saisonnier.


# Calculer et tracer la fonction d'autocorrélation partielle (PACF) pour la série différenciée
pacf(gold_price_diff, main="Partial Autocorrelation Function for Differenced Gold Price")

# Calculer les valeurs de PACF sans les tracer
pacf_values_diff <- pacf(gold_price_diff, plot=FALSE)

# Afficher les valeurs de PACF
print(pacf_values_diff)


#Lag 9: Le pic significatif à un décalage de 9 dans le PACF (et aussi dans l'ACF) pourrait indiquer une saisonnalité ou un motif répétitif tous les 9 jours.

#Lag 17 et 26: De même, les pics significatifs aux décalages 17 et 26 suggèrent également une possible saisonnalité ou un motif répétitif à ces intervalles.


# Utiliser auto.arima pour identifier automatiquement le meilleur modèle ARIMA
best_model <- auto.arima(gold_price_1erpartie$GOLD.Close, 
                         seasonal = TRUE, 
                         stepwise = TRUE, 
                         approximation = TRUE, 
                         trace = TRUE)

# Afficher un résumé du modèle
summary(best_model)

# Tracer le diagnostic des résidus
checkresiduals(best_model)

#nous ne rejetons pas l'hypothèse nulle que les résidus sont indépendants les uns des autres (c'est-à-dire, pas d'autocorrélation à des lags jusqu'à 10)
#le meilleur model est ARIMA(0,1,0), un modèle simple qui n'a pas de termes AR ou MA

# Définir la deuxième partie des données
start_date_2erpartie <- as.Date("2022-10-02")
gold_price_2erpartie <- gold_prices[gold_prices$Date > end_date_1erpartie,]

# Prédire les valeurs futures avec le modèle
forecast_results <- forecast(best_model, h = nrow(gold_price_2erpartie))

# Fusionner les prédictions avec les données réelles
results_comparison <- data.frame(
  Date = gold_price_2erpartie$Date,
  Actual = gold_price_2erpartie$GOLD.Close,
  Forecast = forecast_results$mean
)

# Calculer les erreurs de prévision
results_comparison$Error <- results_comparison$Actual - results_comparison$Forecast

# Calculer les métriques d'évaluation
MSE <- mean(results_comparison$Error^2)  # Mean Squared Error
RMSE <- sqrt(MSE)  # Root Mean Squared Error
MAE <- mean(abs(results_comparison$Error))  # Mean Absolute Error
MAPE <- mean(abs(results_comparison$Error) / results_comparison$Actual) * 100  # Mean Absolute Percentage Error

cat("Mean Squared Error (MSE):", MSE, "\n",
    "Root Mean Squared Error (RMSE):", RMSE, "\n",
    "Mean Absolute Error (MAE):", MAE, "\n",
    "Mean Absolute Percentage Error (MAPE):", MAPE, "%\n"
)


# Tracer les données réelles et les prédictions
ggplot() +
  geom_line(data = gold_price_2erpartie, aes(x = Date, y = GOLD.Close), color = "blue") +
  geom_line(data = results_comparison, aes(x = Date, y = Forecast), color = "red") +
  labs(title = "Actual vs Predicted Gold Price",
       x = "Date",
       y = "Close Price") +
  theme_minimal() +
  scale_colour_manual(values = c("blue", "red"), name = "",
                      breaks = c("Actual", "Predicted"),
                      labels = c("Actual", "Predicted")) +
  guides(colour = guide_legend(order = 1))
