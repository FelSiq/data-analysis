#Constantes
Gravidade <- 9.81 #m.s^-2
ThetaPorCosseno <- 9.83 #em graus
ThetaPorTransf <- 11 #em graus
ImprecisaoReflexoHumano <- 0.1 #em s
NumeroDeOscilacoes <- 10

#Parte 0
#Esta parte e dedicada para construir as ferramentas
#necessarias para a regressao linear (ou metodo dos minimos)
#quadrados, que devera ser utilizado nas partes 1 e 2 do 
#experimento.
aLinearRegression <- function(x, y){
	xbar <- sum(x)/length(x)
	a <- sum((x - xbar)*y)/sum((x - xbar)^2)
	a
}

bLinearRegression <- function(x, y){
	xbar <- sum(x)/length(x)
	ybar <- sum(y)/length(y)
	b <- (ybar - aLinearRegression(x, y)*xbar)
	b
}

delY <- function(a, b, x, y){
	(sqrt(sum((a * x + b - y)^2))/(length(x) - 2))
}

aUnsurelr <- function(a, b, x, y){
	xbar <- sum(x)/length(x)
	delY(a, b, x, y)/sqrt(sum((x - xbar)^2))
}

bUnsurelr <- function(a, b, x, y){
	xbar <- sum(x)/length(x)
	sqrt(sum(x^2)/(length(x) * sum((x - xbar)^2))) * delY(a, b, x, y)
}

#Parte 1
#Formula 1: calcula o quadrado do Periodo, recebendo o comprimento e g
GetSquarePeriod <- function(L, g = Gravidade) 
	(4.0 * pi^2)/g * L

#Cria uma tabela com os valores colhidos em laboratorio
TabelaPeriodoPorL <- 
	data.frame(
	c(79.7, 109.6, 138.9, 140.6, 171.0, 200.5, 233.5),
	c(17.84, 20.89, 23.49, 23.73, 26.02, 28.21, 30.63),
	c(17.63, 20.75, 23.34, 23.70, 25.91, 28.21, 30.54),
	c(17.86, 20.91, 23.63, 23.63, 26.13, 28.13, 30.81))

#Trunca o valor com o devido arredondamento
TabelaPeriodoPorL <- 
	round(TabelaPeriodoPorL, 1)

#Recebe a media dos valores numa nova coluna
TabelaPeriodoPorL[5] <- 
	round(rowMeans(TabelaPeriodoPorL[ , 2:4]), 1)

#Calcula o desvio padrao dos valores obtidos
TabelaPeriodoPorL[6] <- 
	round(apply(TabelaPeriodoPorL[, 2:4], 1, sd), 1)

#Calcula o Periodo ao Quadrado
TabelaPeriodoPorL[7] <- 
	round((TabelaPeriodoPorL[[5]] /
	NumeroDeOscilacoes)^2, 1)

#Atribui um nome significativo as colunas da tabela
colnames(TabelaPeriodoPorL) <- 
	c("L", "T1", "T2", "T3", 
	"Mean", "StdDev", "T^2")

#Calcula uma tabela de Periodo Teorico usando a formula (1)
TabelaPeriodoTeorico <- 
	data.frame(round(TabelaPeriodoPorL[, 1]/100, 1))
TabelaPeriodoTeorico[2] <- 
	round(apply(TabelaPeriodoTeorico, 1, 
	GetSquarePeriod), 1)
colnames(TabelaPeriodoTeorico) = c("L", "T^2")

#Calcula a distancia entre o periodo teorico e o experimental
diffTTeoricoPratico <- 
	abs(TabelaPeriodoPorL[[7]] - 
	TabelaPeriodoTeorico[[2]])

#Plota o grafico Temp X Tteorico
plot(diffTTeoricoPratico * 10.0, 
	xlab = "Amostragem", 
	ylab = "|T(emp)^2 - T(teorico)^2| x 10 (s²) +- 1.0e-1 s^2", 
	type = 'b', 
	ylim=c(0.0, 2.0), 
	yaxp=c(0.0, 1.0, 2))
title('|T empirico^2 - T teórico^2| x 10')

#Calcular a inclinacao da reta utilizando o metodo
#dos minimos quadrados + o erro
a1 <- aLinearRegression(
	TabelaPeriodoPorL[[1]]/100, 
	TabelaPeriodoPorL[[7]])

b1 <- bLinearRegression(
	TabelaPeriodoPorL[[1]]/100, 
	TabelaPeriodoPorL[[7]])

aUnsure1 <- aUnsurelr(
	a1, 
	b1, 
	TabelaPeriodoPorL[[1]]/100, 
	TabelaPeriodoPorL[[7]])

bUnsure1 <- bUnsurelr(
	a1, 
	b1, 
	TabelaPeriodoPorL[[1]]/100, 
	TabelaPeriodoPorL[[7]])

yDelta1 <- delY(
	a1,
	b1,
	TabelaPeriodoPorL[[1]]/100, 
	TabelaPeriodoPorL[[7]])

plot(TabelaPeriodoPorL[[1]]/100, TabelaPeriodoPorL[[7]],
	xlab = "L (m)", ylab = "T^2 (s^2)")
abline(b1, a1)
abline(b1-yDelta1, a1)
abline(b1+yDelta1, a1)

#Usar o coef. angular para calcular o valor da gravidade
g1 <- (4.0 * pi^2)/a1
gUnsure1 <- abs((-4 * pi^2)/(a1^2))*aUnsure1

#Parte 2: Plano inclinado e diagrama de forcas
#Valores obtidos em laboratiorio
PlanoInclinadoComprimento <- 160.0 #cm
PlanoInclinadoComprimentoIncerteza <- 0.1 #cm
PlanoInclinadoAltura <- 6.27 #cm
PlanoInclinadoAlturaIncerteza <- 0.005 #cm
FitaX0 <- 10.0 #em cm
FrequenciaFaisca <- 5.0 #em Hz
PeriodoFaisca <- 1.0/FrequenciaFaisca #em s

#Formulas
#Formula 2: Erro do angulo theta
unsureTheta <- function(x, h, dx, dh)
	abs(-(h^2)/(x^2 + h^2))*dx + abs(x^2/(x^2 + h^2))*dh

#Formula 3: Relacao linear entre y/t e t
sorvetao <- function(v0, a, t)
	v0 + t * a/2

#Formula 4: Erro da gravidade
unsureGravity <-function(a, da, theta, dtheta)
	da/abs(sin(theta)) + a*dtheta*abs(-cos(theta)/(sin(theta)^2))

AnguloTheta <- 
	(180/pi) * atan(PlanoInclinadoAltura / PlanoInclinadoComprimento)

AnguloThetaIncerteza <- 
	(180/pi) * 
	unsureTheta(PlanoInclinadoComprimento,
		PlanoInclinadoAltura,
		PlanoInclinadoComprimentoIncerteza,
		PlanoInclinadoAlturaIncerteza)

#Construir a tabela {t (s), y (m))
TabelaPlanoInclinado <- 
	data.frame(
		round((c(10.6, 12.8, 16.4, 21.5, 28.1, 36.2, 45.9, 
			57.0, 69.6, 83.7, 99.2, 116.2, 134.7) - FitaX0), 1),
		seq(from = PeriodoFaisca, 
			to = 13 * PeriodoFaisca, 
			by = PeriodoFaisca)
	)

#Calcular a coluna y/t (m/s)
TabelaPlanoInclinado[3] <- 
	round(TabelaPlanoInclinado[1]/(100 * (TabelaPlanoInclinado[2])), 1)

#Alterar o nome das colunas
colnames(TabelaPlanoInclinado) <- 
	c("y (cm)", "t (s)", "y/t (m.s^-1)")

#Plotar e verificar se ha uma relacao linear entre t e y/t
plot(TabelaPlanoInclinado[[2]], TabelaPlanoInclinado[[3]],
	xlab = "t (s)", ylab = "y/t (m.s^-1)")

#Usar o metodo dos minimos quadrados para obter a inclinacao
#e o coef. linear da relacao (6), e os erros.
#Usar esta reta no plot.
TabelaMinimosQuadrados <- 
	dataFrameLinearRegression(
		TabelaPlanoInclinado[[2]], 
		TabelaPlanoInclinado[[3]])

a2 <- aLinearRegression(
	TabelaPlanoInclinado[[2]], 
	TabelaPlanoInclinado[[3]])

b2 <- bLinearRegression(
	TabelaPlanoInclinado[[2]], 
	TabelaPlanoInclinado[[3]])


aUnsure2 <- aUnsurelr(
	a2,
	b2,
	TabelaPlanoInclinado[[2]], 
	TabelaPlanoInclinado[[3]])

bUnsure2 <- bUnsurelr(
	a2,
	b2,
	TabelaPlanoInclinado[[2]], 
	TabelaPlanoInclinado[[3]])

unsureY2 <- delY(
	a2, 
	b2, 
	TabelaPlanoInclinado[[2]], 
	TabelaPlanoInclinado[[3]])

abline(b2, a2)
abline(b2+unsureY2, a2)
abline(b2-unsureY2, a2)

#Calcular a aceleracao da gravidade, comparar com 9.81 e o valor
#obtido no pendulo simples.
g2 <- 
	(2 * a2)/sin(AnguloTheta * pi/180)

gUnsure2 <- 
	unsureGravity(
	a2, 
	aUnsure2, 
	AnguloTheta, 
	AnguloThetaIncerteza)

abs(g2 - Gravidade)
abs(g2 - g1)

#Fim do experimento!