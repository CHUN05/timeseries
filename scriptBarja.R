# Gráfica de serie de tiempo ----------------------------------------------
# Insumos:
library(forecast)
library(tseries)
library(tidyverse)
library(reshape2)
library(ggfortify)


# Lectura de datos --------------------------------------------------------
data <- read.csv('Data/5001.csv')

# Acumulado anual ---------------------------------------------------------
acumulado <- data
acumulado[is.na(acumulado)] <- 0
acumulado$MeanPrecipitation <- apply(acumulado[,2:ncol(data)],1,mean)

# Desviación  -------------------------------------------------------------

acumulado$desviacion <- apply(acumulado[,2:ncol(data)],1,sd)
write_csv2(acumulado,'Data/newdata.csv')


# Modelado  ---------------------------------------------------------------

data <- data %>% melt(id.vars = "ano")
newdata <- data[order(data$ano),]
newdata <- newdata$value %>% ts(start = c(1944,1), frequency = 12) # Modifica el rango de star

# Plot --------------------------------------------------------------------
p  <- autoplot(newdata, colour = "blue") +
  scale_x_continuous(breaks = seq(1944,1969, by = 1)) # Modifica el rango de secuencia
p2 <- p + labs(title ='TITULO',
               x = 'ejex',
               y = 'ejey',
               caption = 'Elaboración propia con datos de semanhi') 
p3 <- p2 + theme_minimal() 
p3

# To export plot ----------------------------------------------------------
ggsave('Plot/timseries.png',last_plot(),width = 10,height = 5)



