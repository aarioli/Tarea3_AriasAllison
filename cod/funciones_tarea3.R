#--------------------------------
# Función: Alerta por ventana móvil
# Recibe: un dataframe con transacciones diaras (df) o transacciones individuales, un número entero de días retorspectivos (v), un factor que multiplica la desviación estándar (k).
# Devuelve: un dataframe con fecha, numero de transacciones (n.tx), la media, la desviación estándar, y la alerta.
# --------------------------------

alerta.movil <- function(df, ventana, k.alerta) {
  resumen <- df %>%
    count(fecha, name = "n.tx") %>%
    arrange(fecha) %>%
    mutate(
      dias.anteriores = lag(n.tx),
      media = rollapply(
        dias.anteriores,
        width = ventana,
        FUN = mean,
        fill = NA,
        align = "right"
      ),
      desv = rollapply(
        dias.anteriores,
        width = ventana,
        FUN = sd,
        fill = NA,
        align = "right"
      ),
      alerta = if_else(is.na(media), FALSE, n.tx > media + k.alerta * desv)
    ) %>%
    select(-dias.anteriores)
  return(resumen)
}


#--------------------------------
# Función: Desviación Absoluta Media (MAD) de la Ley de Benford
# Recibe: un vector numérico de montos (x).
# Devuelve: un valor numérico único correspondiente a la MAD respecto a las proporciones teóricas de Benford.
#--------------------------------

benford.mad <- function(x) {
  x <- x[!is.na(x) & x > 0]
  primer.digito <- x %/% 10^floor(log10(x))
  proporciones.observ <- tabulate(primer.digito, nbins = 9) / length(primer.digito)
  proporciones.benford <- log10(1 + 1 / (1:9))
  
  mad <- mean(abs(proporciones.observ - proporciones.benford))
  return(mad)
}


#--------------------------------
# Función: Ley de Benford por cantón
# Recibe: un dataframe con transacciones (df), el nombre del canal a filtrar (canal) y el dataframe catálogo de cantones (cantones).
# Devuelve: un dataframe con cod_canton, canton, n, mad y clase (Nigrini), ordenado de mayor a menor MAD.
#--------------------------------

benford.canton <- function(df, canal, cantones) {
  df <- df %>%
    filter(canal == .env$canal) %>%
    summarise(n = n(),
              mad = benford.mad(monto_crc),
              .by = cod_canton) %>%
    left_join(cantones, by = "cod_canton", relationship = "many-to-one") %>%
    mutate(
      clase = case_when(
        mad <= 0.006 ~ "conformidad cercana",
        mad <= 0.012 ~ "conformidad aceptable",
        mad < 0.015 ~ "conformidad marginal",
        .default = "no conformidad"
      )
    ) %>%
    select(cod_canton, canton, n, mad, clase) %>%
    arrange(desc(mad))
  
  return(df)
}


#--------------------------------
# Función: Valor acumulado con interés compuesto
# Recibe: un dataframe con transacciones (df), la tasa efectiva anual (tasa.anual) y el nombre del canal a analizar (canal).
# Devuelve: una lista con tabla (mes, monto, factor), tasa.mensual, valor acumulado e interes.
#--------------------------------

valor.acumulado <- function(df, tasa.anual, canal) {
  tasa.mensual <- (1 + tasa.anual)^(1 / 12) - 1
  tabla <- df %>%
    filter(canal == .env$canal) %>%
    mutate(mes = as.integer(format(as.Date(fecha), "%m"))) %>%
    summarise(monto = sum(monto_crc), .by = mes) %>%
    arrange(mes) %>%
    mutate(factor = (1 + tasa.mensual)^(12 - mes))
  
  VA <- sum(tabla$monto * tabla$factor)
  interes <- VA - sum(tabla$monto)
  
  return(list(
    tabla = tabla,
    tasa.mensual = tasa.mensual,
    va = VA,
    interes = interes
  ))
}