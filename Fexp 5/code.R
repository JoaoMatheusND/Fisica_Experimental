

######################### Obenção de dados e pre-processamento #########################
df<- read.csv("C:\\jmnd\\Fexp 5\\Fexp 5.csv", header=TRUE, dec = ",")
df <- 

######################### Separação dos dados uteis ####################################

tempoc <- df$tempoc
carga <- df$cargar

tempod <- df$tempod
descarga <- df$descarga

testeTmp <- c(tempoc, tempod)
testeaux <- c(carga, descarga)

######################### Modelo #######################################################

modelo <- function(params, x) {
  a <- params[1]
  b <- params[2]
  return(a * (1 - exp(-b * x)))
}

soma_quadrados_residuos <- function(params, x, y) {
  y_pred <- modelo(params, x)
  res <- y - y_pred
  return(sum(res^2))
}

######################### Plotagem do gráfico ##########################################

plot(testeTmp, testeaux, main="Comportamento do capacitor", xlab = "Tempo (s)", ylab="DDP (V)")
points(tempoc, carga, col="blue")
points(tempod, descarga, col="red")
legend("center", legend = c("Carga", "Descarga", "Ajuste teórico"), col = c("blue", "red", "black"), pch = 19)

######################## Linhas de curvas teóricas ######################################
# Dados de carga
x_carga <- tempoc
y_carga <- carga

# Dados de descarga
x_descarga <- tempod
y_descarga <- descarga

# Ajuste aos dados de carga
resultado_carga <- optim(par = c(1, 1), fn = soma_quadrados_residuos, x = x_carga, y = y_carga)
# Ajuste aos dados de descarga
resultado_descarga <- optim(par = c(1, 1), fn = soma_quadrados_residuos, x = x_descarga, y = y_descarga)

# Parâmetros estimados para carga e descarga
parametros_carga <- resultado_carga$par
parametros_descarga <- resultado_descarga$par

# Curva de ajuste para carga
curve(modelo(parametros_carga, x), from = min(x_carga), to = max(x_carga), col = "black", add = TRUE, lwd=2)

# Curva de ajuste para descarga
curve(modelo(parametros_descarga, x), from = max(x_descarga), to = min(x_carga), col = "black", add = TRUE, lwd=2)
