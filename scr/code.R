# Desenvolvido por João Matheus Nascimento Dias em 2023

######################### Obenção de dados e pre-processamento #########################
resistencias <- read.csv("C:\\jmnd\\Resistencias.csv", header=TRUE, dec = ",")
resistencias[is.na(resistencias)] <- 0

######################### Função para plotar os gráficos ###############################

plot_grafic <- function(x, y, z){
  plot(x, y, main="Corrente em função da DDP", xlab="Corrente (mA)", ylab = "DDP (V)", lwd=2, col="blue")
  
  # Adicionando a linha de regressão
  fit <- lsfit(x, y)
  abline(fit, col = "red")
  
  # Coeficientes da regressão
  coef <- coef(fit)
  
  # Cálculo do ângulo da inclinação
  angle_rad <- atan(coef[2]) * 10^3  # coef[2] é o coeficiente angular, ou seja, valor do resistor.
  legend("top", legend = paste("Ângulo de Inclinação:", round(angle_rad, 2), paste(z, "°")), col = "black", lty = 1, cex = 0.8) # Legenda com o ângulo
  # Legenda com o angulo
}

######################### Gráfico do resistor de 47 ohns ###############################
x_47 <- resistencias$X_47
y_47 <- resistencias$Y_47

plot_grafic(x_47, y_47, "")

######################### Gráfico do resistor de 100 ohns ###############################
x_100 <- resistencias$X_100
y_100 <- resistencias$Y_100

plot_grafic(x_100, y_100, "")

######################### Gráfico do resistor de 200 ohns ###############################
x_200 <- resistencias$X_200
y_200 <- resistencias$Y_200

plot_grafic(x_200, y_200, "")

######################### Gráfico do resistor de 2k ohns ###############################
x_2k <- resistencias$X_2k
y_2k <- resistencias$Y_2k

plot_grafic(x_2k, y_2k,"")

######################### Gráfico do resistor de 560k ohns ###############################
x_560k <- resistencias$X_560k
y_560k <- resistencias$Y_560k

plot_grafic(x_560k, y_560k, "k")
