# Obtenção dos dados
df <- read.csv("Dataset.csv", header = TRUE, dec = ",")

# Separação dos dados
corrente <- df$Corrente
espira12.5 <- df$Espira12.5.g.
espira25 <- df$Espira25.g.
espira50.1 <- df$Espira50.1.g.
espira50.2 <- df$Espira50.2.g.

######################### Função para plotar os gráficos ###############################

plot_grafic <- function(x, y, z) {
  plot(x, y, main = "Força (mN) em função da corrente (A)", xlab = "Corrente (A)", ylab = "Força (mN)", lwd = 2, col = "blue")
  
  fit <- lsfit(x, y)
  abline(fit, col = "red")
  
  coef <- coef(fit)
  
  # Cálculo do ângulo da inclinação
  angle_rad <- coef[2]  # coef[2] é o coeficiente angular, ou seja, o valor do campo
  angle_deg <- angle_rad * (180 / pi)  # Convertendo para graus
  
  legend("top", legend = paste("B = ", round(angle_deg, 2), paste("mN/(A*",z,"mm)")), col = "black", lty = 1, cex = 0.8)
}

######################## Ajuste de força #############################################

force <- function(x, L, d) {
  x <- (x - x[1]) * 9.8  # Ajuste da força conforme seu código original
  
  # Ajuste de força
  plot_grafic(corrente, x, d)
  
  # Cálculo do ângulo de inclinação por unidade de comprimento (L)
  angle_per_length <- (x / (corrente * L)) * (180 / pi)  # Convertendo para graus
  
  return(angle_per_length)
}

# Define o comprimento L
length_12.5 <- 12.5
length_25 <- 25
length_50.1 <- 50
length_50.2 <- 100 
length <- c(12.5, 25, 50, 100)

# Chamada de função

# Gráfico da espira de 12.5mm
angle_per_length_12.5 <- force(espira12.5, length_12.5, '12,5')

# Gráfico da espira de 25mm
angle_per_length_25 <- force(espira25, length_25, '25')

# Gráfico da espira de 50.1mm
angle_per_length_50.1 <- force(espira50.1, length_50.1, '50')

# Gráfico da espira de 50.2mm
angle_per_length_50.2 <- force(espira50.2, length_50.2, '100')


# função para plotar os gráficos de 
plot_grafic2 <- function(x, y, z) {
  plot(x, y, main = "Força (mN) em função do Comprimemto (mm)", xlab = "Comprimento (mm)", ylab = "Força (mN)", lwd = 2, col = "blue")
  
  fit <- lsfit(x, y)
  abline(fit, col = "red")
  
  coef <- coef(fit)
  
  # Cálculo do ângulo da inclinação
  angle_rad <- coef[2]  # coef[2] é o coeficiente angular, ou seja, o valor do campo
  angle_deg <- angle_rad * (180 / pi)  # Convertendo para graus
  
  legend("top", legend = paste("B = ", round(angle_deg, 2), paste(z, "mN/(4[A]*mm)")), col = "black", lty = 1, cex = 0.8)
}

# Ajuste de força em função do comprimento para corrente constante
force_vs_length <- function() {
  a <- ((espira12.5 - espira12.5[1]) * 9.8)[9]
  b <- ((espira25 - espira25[1]) * 9.8)[9]
  c <- ((espira50.1 - espira50.1[1]) * 9.8)[9]
  d <- ((espira50.2 - espira50.2[1]) * 9.8)[9]
  
  # Ajuste de força
  plot_grafic2(length, c(a, b, c, d), '')
}

# Gráfico das espiras
force_vs_length()
