#' Distribucion Exponencial
#'
#' Realiza una Distribucion Exponencial
#'
#' @param y (vector) dato de la respuesta
#' @param x (vector) datos de la variable explicativa
#' @return Una grafica
#' @export
frecuencia_mortalidad <- function(x, tiempo) {
  # Calcula el lambda estimado solo si no se proporciona
  lambda <- mean(x, na.rm = TRUE)

  dexponencial <- function(x, lambda) {
    if (!is.numeric(x) || any(x %% 1 != 0) || any(x < 0))
      stop("x debe ser un número entero no negativo.")

    if (!is.numeric(lambda) || any(lambda <= 0)) {
      stop("lambda debe ser un número positivo.")
    }

    result <- exp(-lambda) * lambda^x / factorial(x)
    return(result)
  }

  max_event_mort <- max(dexponencial(0:tiempo, lambda * tiempo))
  porcentaje_max_event_mort <- max_event_mort
  mensaje <- paste("En", tiempo, "días:", max_event_mort)
  mensaje_porcentaje <- data.frame(tiempo = 0:tiempo, probabilidad = dexponencial(0:tiempo, lambda) * 100)

  # Crea un data frame con los valores del poisson
  df <- data.frame(x = 0:tiempo, probabilidad = dexponencial(0:tiempo, lambda) * 100)

  # Crea el gráfico utilizando ggplot2
  p <- ggplot(df, aes(x = x, y = probabilidad)) +
    geom_bar(stat = "identity", fill = "blue", width = 0.5) +
    labs(title = "Frecuencia de la mortalidad",
         x = "Tiempo",
         y = "Probabilidad (%)") +
    theme_minimal()

  print(p)

  return(df)
}

