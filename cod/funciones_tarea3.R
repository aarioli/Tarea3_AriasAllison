#--------------------------------
# Función: Alerta por ventana móvil
# Recibe: un dataframe con transacciones diaras (df) o transacciones individuales, un número entero de días retorspectivos (v), un factor que multiplica la desviación estándar (k).
# Devuelve: un dataframe con fecha, numero de transacciones (n.tx), la media, la desviación estándar, y la alerta.
# --------------------------------

library(dplyr)
library(zoo)

alerta.movil <- function(df, ventana, k.alerta) {
  resumen <- df %>% 
    count( fecha, name = "n.tx") %>% 
    arrange(fecha) %>% 
    mutate(
      dias.anteriores = lag(n.tx),
      media = rollapply( dias.anteriores, width = ventana, FUN = mean, fill = NA, align = "right"),
      desv = rollapply( dias.anteriores, width = ventana, FUN = sd, fill = NA, align = "right"),
      alerta = if_else( is.na(media), FALSE, n.tx > media + k.alerta * desv)
    ) %>% 
    select(-dias.anteriores)
  return(resumen)
}