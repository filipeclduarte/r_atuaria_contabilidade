##### Função de Estatísticas Descritivas #####

estatisticas <- function(x){
  x <- x[!is.na(x)] # isso serve para que ele omita as observações com NA
  me <- mean(x)
  med <- median(x)
  n <- length(x)
  s <- sd(x)
  mi <- min(x)
  ma <- max(x)
  q25 <- quantile(x, probs = 0.25)
  q75 <- quantile(x, probs = 0.75)
  return(c(n = n, media = me, mediana = med, desvio = s, 
           Q = q25, Q = q75, min = mi, max = ma))
}

apply_estatisticas <- function(x){
  apply(x, 2, estatisticas)
}