# Gráfica de serie de tiempo ----------------------------------------------
# Insumos:
library(forecast)
library(tseries)
library(tidyverse)
library(reshape2)
library(ggfortify)


# Lectura de datos --------------------------------------------------------
data <- read.csv('Data/data.csv')

# Acumulado anual ---------------------------------------------------------
acumulado <- data
acumulado[is.na(acumulado)] <- 0
acumulado$MeanPrecipitation <- apply(acumulado[,2:ncol(data)],1,mean)

# Desviación  -------------------------------------------------------------

acumulado$desviacion <- apply(acumulado[,2:ncol(data)],1,sd)
write_csv2(acumulado,'Data/newdata.csv')


# Modelado  ---------------------------------------------------------------

data <- data %>% melt(id.vars = "ano")
data <- data$value %>% ts(start = c(1944,1), frequency = 12)

# Plot --------------------------------------------------------------------
p  <- autoplot(data, colour = "blue") +
  scale_x_continuous(breaks = seq(1944,1967, by = 1))
p2 <- p + labs(title ='TITULO',
               x = 'ejex',
               y = 'ejey',
               caption = 'Elaboración propia con datos de semanhi') 
p3 <- p2 + theme_minimal() 
p3

# To export plot ----------------------------------------------------------
ggsave('Plot/timseries.png',last_plot(),width = 10,height = 5)



