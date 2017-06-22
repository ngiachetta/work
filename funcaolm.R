# Função para calcular o Slope (b) e o Intercepto (a)
b <- function(x, y, a = TRUE){
  # Verificar se os pares ordenados contém NA
  ## Verificar se os vetores são do mesmo tamanho
  if (length(x) == length(y)){
    for (i in 1:length(x)){
      if (is.na(x[i]) == TRUE){
        y[i] <- NA
      }else if(is.na(y[i]) == TRUE){
        x[i] <- NA
      }
    }
  }
  
  mean_x <- mean(x, na.rm = T) # Media da variavel x
  mean_y <- mean(y, na.rm = T) # Media da variavel y
  
  x <- x[!is.na(x)]
  y <- y[!is.na(y)]
  
  # Calcular o somatorio x - X
  somaX <- c()
  for (i in 1:length(x)){
    somaX[i] <- (x[i] - mean_x)
  }
  
  # Calcular o somatorio y - Y
  somaY <- c()
  for (i in 1:length(y)){
    somaY[i] <- (y[i])
  }
  
  # Calcular o somatorio (x - X)^2
  somaX2 <- somaX^2
  
  # Calcular S(x-X)(y-Y)
  somaXY <- c()
  for (i in 1:length(somaX)){
    somaXY[i] <- somaX[i]*somaY[i]
  }
  
  # Slope (b)
  b <- (sum(somaXY, na.rm = T))/(sum(somaX2, na.rm = T))
  
  # Intercepto (a)
  if (a == TRUE){
    a <- mean_y - (b*mean_x) 
  }else{
    a <- NA
  }
  return(list(Intercepto = a, Slope = b))
}
