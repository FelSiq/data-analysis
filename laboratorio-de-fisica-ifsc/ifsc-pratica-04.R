#Inicio dos experimentos
#Constantes
IncertezaTransferidor <- 1 #Em graus
IncertezaBalanca <- 0.1 #Em gramas
IncertezaRegua <- 0.1 #Em centimetros
Gravidade <- 9.81 #em m/s^2

#Parte 1: Sistemas de polias
#Dados coletados ----------------------------
Massas <- 
	c(58.9, 78.5, 68.6) #Em gramas
AnguloAlfa <- 
	abs(c(214, 213, 213) - 270) #Em graus
AnguloGamma <- 
	abs(c(317, 316, 316) - 270) #Em graus

#Formula que calcula a tracao do fio, baseado
#no Peso suspenso, e em L e CB.
FormulaTracao <- 
	function(m, g, L, CB) {
	return ((m * g * L) / CB)
}
FormulaTracaoIncerteza <- 
	function(m, g, L, CB, dm, dg, dL, dCB) {
	return ((g*L*dm)/CB + 
		(m*L*dg)/CB + 
		(m*g*dL)/CB + 
		abs(-m*g*L*dCB)/(CB^2))
}

#Fim da parte 1.
#--------------------------------------------
#Parte 2: Tensao de ruptura de uma corda
#Dados coletados ----------------------------
#Subparte 1: Calculo do CB
MassaFixa <- 600.5 #Em g
LVariavel <- 
	c(24.4, 16.1, 18.0, 17.5, 17.6, 16.4) #Em cm
CB <-
	c(17.4, 18.0, 13.5, 14.5, 14.9, 14.3) #Em cm
#Subparte 2: Calculo da massa de ruptura 
LFixo <- 
	c(20.2, 17.1, 18.0, 17.5, 17.5) #Em cm
MassaDeRuptura <-
	c(961.2, 944.7, 767.8, 721.9, 990.8) #Em gramas

#Experimento (subparte 1):
#Calculo da Tracao media
Tmedia1 <- FormulaTracao (
	(MassaFixa)/1000, 
	Gravidade, 
	(LVariavel)/100, 
	(CB)/100)

mean(Tmedia1)
#Media: 7.035 (N)
sd(Tmedia1)
#Desvio padrao: 1.037 (N)
median(Tmedia1)
#Mediana: 7.034 (N)
mean(FormulaTracaoIncerteza(
	(MassaFixa)/1000, 
	Gravidade, 
	(LVariavel)/100, 
	(CB)/100, 
	IncertezaBalanca/1000, 
	0.0, 
	IncertezaRegua/100, 
	IncertezaRegua/100))
#Incerteza: 0.09 (N)

#Fim da subparte 1.
#Experimento (subparte 2):
Tmedia2 <- 
	FormulaTracao(
		(MassaDeRuptura)/1000, 
		Gravidade, 
		(LFixo)/100, 
		(LFixo)/100)

mean(Tmedia2)
#Media: 8.606117 (N)
sd(Tmedia2)
#Desvio padrao: 1.207503 (N)
median(Tmedia2)
#Mediana: 9.267507 (N)
mean(FormulaTracaoIncerteza(
	(MassaDeRuptura)/1000, 
	Gravidade, 
	(LFixo)/100, 
	(LFixo)/100,
	IncertezaBalanca/1000, 
	0.0, 
	IncertezaRegua/100, 
	IncertezaRegua/100))
#Incerteza: 0.1N

#Resultado: 1.55 (N)
#Fim da subparte 2.
#Fim da parte 2.
#--------------------------------------------
#Parte 3: Coeficiente de atrito estatico
#Dados coletados ----------------------------
GrauMaxFaceLisa <- 
	c(107, 106, 106, 108, 107, 108) #Em graus
GrauMaxFaceRugosa <-
	c(114, 116, 116, 115, 116, 116) #Em graus
#--------------------------------------------
#Formulas
#Calcula o Atrito estatico baseado em sua inclinacao
#maxima no dado plano inclinado
FormulaAtritoEstatico <- function(theta) {
	#Transforma o angulo theta, dado em graus, 
	#para radianos, por padrao da linguagem R.
	return (tan(pi * ((theta - 90.0) / 180.0)))
}

FormulaAtritoEstaticoIncerteza <- function(theta, dtheta) {
	return ((1.0 / cos(pi * ((theta - 90.0) / 180.0))) * 
		(pi * ((dtheta) / 180.0)))
}

mean(FormulaAtritoEstatico(GrauMaxFaceLisa))
#Resultado: 0.31
mean(FormulaAtritoEstaticoIncerteza(GrauMaxFaceLisa, 
	IncertezaTransferidor))
#Resultado: 0.02

mean(FormulaAtritoEstatico(GrauMaxFaceRugosa))
#Resultado: 0.48
mean(FormulaAtritoEstaticoIncerteza(GrauMaxFaceRugosa, 
	IncertezaTransferidor))
#Resultado: 0.02

#Fim da parte 3
#Fim dos Experimentos