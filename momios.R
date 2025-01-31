#install.packages("remotes")
#library(remotes)
#install_github("cran/fbRanks")

library(fbRanks)
library(dplyr)
library(ggplot2)

#No es necesario cambiar el directorio de trabajo en RStudioCloud
#Como los csv ya están en el repositorio del proyecto no es necesario descargarlos nuevamente
# Descarga de archivos
#u1011 <- "https://www.football-data.co.uk/mmz4281/1011/SP1.csv"
#u1112 <- "https://www.football-data.co.uk/mmz4281/1112/SP1.csv"
#u1213 <- "https://www.football-data.co.uk/mmz4281/1213/SP1.csv"
#u1314 <- "https://www.football-data.co.uk/mmz4281/1314/SP1.csv"
#u1415 <- "https://www.football-data.co.uk/mmz4281/1415/SP1.csv"
#u1516 <- "https://www.football-data.co.uk/mmz4281/1516/SP1.csv"
#u1617 <- "https://www.football-data.co.uk/mmz4281/1617/SP1.csv"
#u1718 <- "https://www.football-data.co.uk/mmz4281/1718/SP1.csv"
#u1819 <- "https://www.football-data.co.uk/mmz4281/1819/SP1.csv"
#u1920 <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"

#download.file(url = u1011, destfile ="SP1-1011.csv", mode = "wb")
#download.file(url = u1112, destfile ="SP1-1112.csv", mode = "wb")
#download.file(url = u1213, destfile ="SP1-1213.csv", mode = "wb")
#download.file(url = u1314, destfile ="SP1-1314.csv", mode = "wb")
#download.file(url = u1415, destfile ="SP1-1415.csv", mode = "wb")
#download.file(url = u1516, destfile ="SP1-1516.csv", mode = "wb")
#download.file(url = u1617, destfile ="SP1-1617.csv", mode = "wb")
#download.file(url = u1718, destfile ="SP1-1718.csv", mode = "wb")
#download.file(url = u1819, destfile ="SP1-1819.csv", mode = "wb")
#download.file(url = u1920, destfile ="SP1-1920.csv", mode = "wb")

# Lectura de datos

csv_s <- list("SP1-1011.csv", "SP1-1112.csv", "SP1-1213.csv", "SP1-1314.csv",
             "SP1-1415.csv", "SP1-1516.csv", "SP1-1617.csv", "SP1-1718.csv",
             "SP1-1819.csv", "SP1-1920.csv")
lista <- lapply(csv_s, read.csv)

# Procesamiento de datos

lapply(lista, colnames) 
#notamos que cada csv tiene diferente cantidad de columnas y con diferentes nombres
#y queremos seleccionar las columnas Date, HomeTeam, AwayTeam, FTHG, FTAG, 
#BbMx.2.5, BbAv.2.5, BbMx.2.5.1 y BbAv.2.5.1.
#Observamos que los primeros nueve dataframes tienen las columnas 
#Date, HomeTeam, AwayTeam, FTHG, FTAG juntas pero el último
#dataframe tiene una columna extra llamada Time entre la columna "Data" y "HomeTeam"
#además que las columnas BbMx.2.5, BbAv.2.5, BbMx.2.5.1 y BbAv.2.5.1 tienen
#otros nombres, a saber Max.2.5, Max.2.5.1, Avg.2.5, Avg.2.5.1.
#Para facilitar la manipulación simultánea de los DataFrames, modificaremos los
#detalles mencionados en el último dataframe

#eliminamos columna extra
lista[[10]]<-select(lista[[10]], -Time)
#renombramos columnas
lista[[10]]<-rename(lista[[10]], BbMx.2.5=Max.2.5, BbAv.2.5=Avg.2.5,
                    BbMx.2.5.1=Max.2.5.1, BbAv.2.5.1=Avg.2.5.1)

lista <- lapply(lista, select, Date:FTAG, BbMx.2.5:BbAv.2.5.1)
lapply(lista, colnames)
#observamos que se seleccionaron las columnas deseadas de todos los dataframes
#pero en el último df no están en el mismo orden que en los primeros 9, por lo 
#que ordenamos sus columnas
lista[[10]] <- select(lista[[10]], colnames(lista[[9]]))

# Arreglamos las fechas
lista <- lapply(lista,mutate, Date = as.Date(Date, format = "%d/%m/%y"))

# Unimos los dataframes
d1020s <- do.call(rbind,lista)

# Renombramos las columnas
d1020s <- rename(d1020s, date = Date, home.team = HomeTeam, home.score = FTHG, 
                 away.team = AwayTeam, away.score = FTAG, Max.2.5.O = BbMx.2.5, 
                 Avg.2.5.O = BbAv.2.5, Max.2.5.U = BbMx.2.5.1, Avg.2.5.U = BbAv.2.5.1)
names(d1020s)

# Ordenamos columnas
data <- select(d1020s, date, home.team, home.score, away.team, away.score:Avg.2.5.U)

#Visualizamos algunos registros del dataframe
head(data, n = 2L); tail(data, n = 2L)

# Data frames de partidos y equipos
md <- select(data, date:away.score)
write.csv(md, "match.data.csv", row.names = FALSE)
df <- create.fbRanks.dataframes(scores.file = "match.data.csv")
teams <- df$teams; scores <- df$scores
head(teams, n = 2L); dim(teams); head(scores, n = 2L); dim(scores)

# Conjuntos iniciales de entrenamiento y de prueba
Ym <- format(scores$date, "%Y-%m")
Ym <- unique(Ym)
places <- which(Ym[15]==format(scores$date, "%Y-%m")) # Consideramos partidos de
                            #15 meses para comenzar a ajustar el modelo
ffe <- scores$date[max(places)] # Fecha final conjunto de entrenamiento

# Consideraremos partidos de 15 meses para comenzar a ajustar el modelo. Así, nuestro primer conjunto de entrenamiento consiste de datos de partidos hasta el `r ffe` 
train <- scores %>% filter(date <= ffe)
test <- scores %>% filter(date > ffe)

head(train, n = 1); tail(train, n = 1)
head(test, n = 1); tail(test, n = 1)

# Primer ajuste del modelo

traindate <- unique(train$date)
testdate <- unique(test$date)

ranks <- rank.teams(scores = scores, teams = teams, 
                    min.date = traindate[1], 
                    max.date = traindate[length(traindate)])

# Primera predicción

pred <- predict(ranks, date = testdate[1])

phs <- pred$scores$pred.home.score # predicted home score
pas <- pred$scores$pred.away.score # predicted away score
pht <- pred$scores$home.team # home team in predictions
pat <- pred$scores$away.team # away team in predictions

# Continuar ajustando y prediciendo

phs <- NULL; pas <- NULL; pht <- NULL; pat <- NULL
for(i in 1:(length(unique(scores$date))-170)){
  ranks <- rank.teams(scores = scores, teams = teams, 
                      min.date = unique(scores$date)[i], 
                      max.date = unique(scores$date)[i+170-1], 
                      silent = TRUE,
                      time.weight.eta = 0.0005)
  pred <- predict(ranks, date = unique(scores$date)[i+170],
                  silent = TRUE)
  
  phs <- c(phs, pred$scores$pred.home.score) # predicted home score
  pas <- c(pas, pred$scores$pred.away.score) # predicted away score
  pht <- c(pht, pred$scores$home.team) # home team in predictions
  pat <- c(pat, pred$scores$away.team) # away team in predictions
}

# Eliminamos NA's
buenos <- !(is.na(phs) | is.na(pas))
phs <- phs[buenos] # predicted home score
pas <- pas[buenos] # predicted away score
pht <- pht[buenos] # home team in predictions
pat <- pat[buenos] # away team in predictions
momio <- data %>% filter(date >= unique(scores$date)[171]) # momios conjunto de prueba
momio <- momio[buenos,]
mean(pht == momio$home.team); mean(pat == momio$away.team)
mean(phs + pas > 2.5 & momio$home.score + momio$away.score > 2.5)
mean(phs + pas < 2.5 & momio$home.score + momio$away.score < 2.5)
hs <- momio$home.score
as <- momio$away.score

# Probabilidades condicionales

# proporción de partidos con más de tres goles según el modelo
mean(phs + pas > 3)
# probabilidad condicional estimada de ganar en over 2.5
mean(phs + pas > 3 & hs + as > 2.5)/mean(phs + pas > 3) 
# proporción de partidos con menos de 2.1 goles según el modelo
mean(phs + pas < 2.1)
# probabilidad condicional estimada de ganar en under 2.5
mean(phs + pas < 2.1 & hs + as < 2.5)/mean(phs + pas < 2.1) 

# Juegos con momios máximos

cap <- 50000; g <- NULL

for(j in 1:length(phs)){
  if(((phs[j] + pas[j]) > 3) & (0.64/(momio$Max.2.5.O[j]^-1) > 1)){
    if((hs[j] + as[j]) > 2.5) cap <- cap + 1000*(momio$Max.2.5.O[j]-1)
    else cap <- cap - 1000
    g <- c(g, cap)
  }
  
  if(((phs[j] + pas[j]) < 2.1) & (0.58/(momio$Max.2.5.U[j]^-1) > 1)){
    if((hs[j] + as[j]) < 2.5) cap <- cap + 1000*(momio$Max.2.5.U[j]-1)
    else cap <- cap - 1000
    g <- c(g, cap)
  }
}

# Escenario con momios máximos

g <- data.frame(Num_Ap = 1:length(g), Capital = g)
(p <- ggplot(g, aes(x=Num_Ap, y=Capital)) + geom_line(color="pink", size=1.5) + geom_point(color="red",size=0.3) +
  labs(x = "Número de juego", 
       y = "Capital",
       title = "Realizando una secuencia de juegos") +
  theme(plot.title = element_text(size=12))  +
  theme(axis.text.x = element_text(color="red" , size = 10, angle = 0, hjust = 1),
        axis.text.y = element_text(color="red" , size = 10, angle = 0, hjust = 1))) 
        # color, ángulo y estilo de las abcisas y ordenadas 

# Escenario con momios promedio

cap <- 50000; g <- NULL

for(j in 1:length(phs)){
  if(((phs[j] + pas[j]) > 3) & (0.64/(momio$Avg.2.5.O[j]^-1) > 1)){
    if((hs[j] + as[j]) > 2.5) cap <- cap + 1000*(momio$Avg.2.5.O[j]-1)
    else cap <- cap - 1000
    g <- c(g, cap)
  }
  
  if(((phs[j] + pas[j]) < 2.1) & (0.58/(momio$Avg.2.5.U[j]^-1) > 1)){
    if((hs[j] + as[j]) < 2.5) cap <- cap + 1000*(momio$Avg.2.5.U[j]-1)
    else cap <- cap - 1000
    g <- c(g, cap)
  }
}

g <- data.frame(Num_Ap = 1:length(g), Capital = g)
(p <- ggplot(g, aes(x=Num_Ap, y=Capital)) + geom_line(color="cyan", size=1.5) + geom_point(color="blue",size=0.3) +
  labs(x = "Número de juego", 
       y = "Capital",
       title = "Realizando una secuencia de juegos") +
  theme(plot.title = element_text(size=12))  +
  theme(axis.text.x = element_text(face = "bold", color="blue" , size = 10, angle = 0, hjust = 1),
        axis.text.y = element_text(face = "bold", color="blue" , size = 10, angle = 0, hjust = 1)))