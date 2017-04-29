#Constantes
Gravidade <- 9.81 #m.s^-2
ThetaPorCosseno <- 9.83 #em graus
ThetaPorTransf <- 11 #em graus
ImprecisaoReflexoHumano <- 0.1 #em s
NumeroDeOscilacoes <- 10

#Parte 1
#Formula 1: calcula o quadrado do Periodo, recebendo o comprimento e g
GetSquarePeriod <- function(L, g = Gravidade) (4.0 * pi^2)/g * L

#Cria uma tabela com os valores colhidos em laboratorio
TabelaPeriodoPorL <- data.frame(
	c(79.7, 109.6, 138.9, 140.6, 171.0, 200.5, 233.5),
	c(17.84, 20.89, 23.49, 23.73, 26.02, 28.21, 30.63),
	c(17.63, 20.75, 23.34, 23.70, 25.91, 28.21, 30.54),
	c(17.86, 20.91, 23.63, 23.63, 26.13, 28.13, 30.81))

#Trunca o valor com o devido arredondamento
TabelaPeriodoPorL <- round(TabelaPeriodoPorL, 1)

#Recebe a media dos valores numa nova coluna
TabelaPeriodoPorL[5] <- round(rowMeans(TabelaPeriodoPorL[ , 2:4]), 1)

#Calcula o desvio padrao dos valores obtidos
TabelaPeriodoPorL[6] <- round(apply(TabelaPeriodoPorL[, 2:4], 1, sd), 1)

#Calcula o Periodo ao Quadrado
TabelaPeriodoPorL[7] <- round((TabelaPeriodoPorL[[5]] /
	NumeroDeOscilacoes)^2, 1)

#Atribui um nome significativo as colunas da tabela
colnames(TabelaPeriodoPorL) <- c("L", "T1", "T2", "T3", 
	"Mean", "StdDev", "T^2")

#Calcula uma tabela de Periodo Teorico usando a formula (1)
TabelaPeriodoTeorico <- data.frame(round(TabelaPeriodoPorL[, 1]/100, 1))
TabelaPeriodoTeorico[2] <- round(apply(TabelaPeriodoTeorico, 1, 
	GetSquarePeriod), 1)
colnames(TabelaPeriodoTeorico) = c("L", "T^2")

#Calcula a distancia entre o periodo teorico e o experimental
diffTTeoricoPratico <- abs(TabelaPeriodoPorL[[7]] - 
	TabelaPeriodoTeorico[[2]])

#Plota o grafico Temp X Tteorico
plot(diffTTeoricoPratico * 10.0, 
	xlab = "Amostragem", 
	ylab = "|T(emp)² - T(teorico)²| x 10 (s²) +- 1.0e-1 s²", 
	type = 'b', 
	ylim=c(0.0, 2.0), 
	yaxp=c(0.0, 1.0, 2))
title('|T empírico² - T teórico²| x 10')

#Parte 2: Plano inclinado e diagrama de forcas