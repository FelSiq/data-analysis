#Resumo:
#Parte 1: modulo de Young:
#Calculo final: 16.2 10^10 Pa (por coef. angular) +- 0.3 10^10 Pa 
#(por P e x)
#Material mais próximo, segundo a tabela da apostila: Aco

#Constantes adotadas
ImprecisaoMicrometro <- 0.01	#em mm
ImprecisaoPaquimetro <- 0.05	#em mm
ImprecisaoBalanca <- 0.1		#em g
ImprecisaoProveta <- 0.1		#em cm
ImprecisaoRegua	<- 0.1			#em cm
Gravidade <- 9.81				#em m.s^-2

#Dados base, colhidos em Laboratório
BarraEspessura <- c(1.00, 1.10, 1.10) #em mm, com o micrômetro
BarraLargura <- c(25.50, 25.55, 25.45) #em mm, com o paquímetro

MassaObjetosMetalicos <- c(56.50, 109.7, 160.7, 209.4, 
	255.9, 294.3, 341.1, 383.6)	#em g

MedidaInicialDaProveta <- 5.0 #em cm
DeslocamentoReguaLFixoPVar <- c(5.8, 6.5, 7.3, 8.0, 8.7,
	9.1, 9.8, 10.4)	#em cm

LInicial <- 27.0 #L inicial da régua p/ medições de deformação com 
#L variável, em cm
LVariacao <- 3.0 #Variamos o L de aproximadamente 3.0 em 3.0 cm
DeslocamentoReguaLVarPFixo <- c(10.4, 8.6, 7.2, 6.2, 5.4,
	4.9, 4.6, 4.4, 4.4)	#em cm

#Formulas
UnsureYoung <- function(F, L, d, b, x, dF, dL, dd, db, dx){
	(4.0 *(L^3)*dF)/((d^3)*b*x) +
	(12.0*F*(L^2)*dL)/((d^3)*b*x) +
	(12.0*F*(L^3)*dd)/((d^4)*b*x) +
	(8.0 *(L^3)*F*db)/((d^3)*(b^2)*x) +
	(8.0 *(L^3)*F*dx)/((d^3)*b*(x^2))
}

AngularCoef <- function(x0, x1, y0, y1) 
	(y1 - y0)/(x1 - x0)
YoungModulus <- function(F, L, d, b, x) 
	(F * 4.0 * (L^3))/((d^3) * b * x)
YoungModulusByCoef <- function(m, L, d, b) 
	(m * 4.0 * (L^3))/((d^3) * b)
AngularCoefByYoung <- function(E, d, b, L) 
	(E*(d^3)*b)/(4.0*(L^3))
Equivalency <- function(x1, x2, y1, y2) 
	abs(x1 - x2) < (2*(y1 + y2))

#Parte 1: Determinação do módulo de Yong
PesoObjeto <- round((MassaObjetosMetalicos / 1000.0) * Gravidade, 2)
TabelaPesoXDeformacao <- data.frame(PesoObjeto, 
	DeslocamentoReguaLFixoPVar - MedidaInicialDaProveta)
colnames(TabelaPesoXDeformacao) = c("Peso (N) +- 9.8*10^(-4)N", 
	"Deformacao (cm) +- 0.1cm")

TabelaPesoXDeformacao #Imprime a tabela
plot(TabelaPesoXDeformacao) #Plota a tabela (evidencia relação linear entre as grandezas)
#Adiciona a reta que representa o conjunto de dados
abline(0, AngularCoef(0.55, 3.76, 0.8, 5.4), col="blue") 
title("Gráfico 1 - Relação linear entre Peso (N) e a Deformação (cm) da barra de metal")

#Parâmetros para o cálculo do Modulo de Young
F <- PesoObjeto
x <- (DeslocamentoReguaLFixoPVar - MedidaInicialDaProveta) * 10^(-2)
L <- LInicial * 10^(-2)
b <- mean(BarraLargura) * 10^(-3)
d <- mean(BarraEspessura) * 10^(-3)
m <- AngularCoef(0.8/10^2, 5.4/10^2, 0.55, 3.76)

#Modulo de Young atraves de coeficiente angular
YModulus2 <- YoungModulusByCoef(m, L, d, b) 

#Modulo de Young atraves de Peso e Deslocamento
YModulus <- YoungModulus(F, L, d, b, x) 

#Concreto foi desconsiderado, por nao ter o modulo de Young 
#expresso na dada tabela da apostila
TabelaComparacaoYoung <- data.frame(c(7.0, 11.0, 9.0, 20.0, 21.0, 1.6, 6.0))
rownames(TabelaComparacaoYoung) = c("Aluminio", "Cobre", 
	"Bronze", "Aco", "Ferro", "Chumbo", "Vidro Crown")
TabelaComparacaoYoung[2] <- round(abs(TabelaComparacaoYoung[[1]] 
	- mean(YModulus2)/10^10), 1)

colnames(TabelaComparacaoYoung) = c("Modulo de Young (10^10 Pa)", 
	"Diferença absoluta (10^10 Pa)")
TabelaComparacaoYoung[order(-TabelaComparacaoYoung[,2], 
	TabelaComparacaoYoung[, 1], decreasing=TRUE),]

b <- median(BarraLargura) * 10^(-3)
d <- median(BarraEspessura) * 10^(-3)
YModulus2 <- YoungModulusByCoef(m, L, d, b)
colnames(TabelaComparacaoYoung) = c("Modulo de Young (10^10 Pa)", 
	"Diferença absoluta (10^10 Pa)")
TabelaComparacaoYoung[2] <- round(abs(TabelaComparacaoYoung[[1]] 
	- mean(YModulus2)/10^10), 1)

TabelaComparacaoYoung[order(-TabelaComparacaoYoung[,2], 
	TabelaComparacaoYoung[, 1], decreasing=TRUE),]

UnsureYoung(PesoObjeto, L, d, b, DeslocamentoReguaLFixoPVar, 
	ImprecisaoBalanca/10^3,
	ImprecisaoRegua/10^2,
	ImprecisaoMicrometro/10^3,
	ImprecisaoPaquimetro/10^3,
	ImprecisaoProveta/10^3)/10^10
#[1] 0.0009353592 0.0016353242 0.0021285955 0.0025191240 0.0028354886
#[6] 0.0031207992 0.0033586387 0.0035518418
#Incertezas muito pequenas, devido a conversão de unidades
#Usando o desvio padrao:
sd(YModulus/10^10) #[1] 0.3116274 (+- 10^10 Pa)


#Parte 2: Análise da relação comprimento-deformação
TabelaLVariavel <- data.frame(seq(from = LInicial, 
	to = 3.0, by = -LVariacao))
TabelaLVariavel[2] <- DeslocamentoReguaLVarPFixo - MedidaInicialDaProveta
TabelaLVariavel[3] <- round((TabelaLVariavel[[1]]^3)/100.0, 1)
colnames(TabelaLVariavel) = c("L (cm +- 0.1cm)", 
	"x (cm +- 0.1cm)", "L^3 (m +- 10^-5m)")

TabelaLVariavel
plot(TabelaLVariavel)

plot(TabelaLVariavel[[3]], TabelaLVariavel[[2]], 
	type = 'b', xlab = "L^3 (m +- 10^-5m)", ylab = "x (cm +-0.1cm)")
title("Gráfico 2: Relação entre L^3 (m) e x (cm)")
arrows(TabelaLVariavel[[3]], TabelaLVariavel[[2]]-0.1, 
	TabelaLVariavel[[3]], TabelaLVariavel[[2]]+0.1, 
	length=0.05, angle=90, code=3)
#Comprova a relacao linear entre L^3 e x 

m3 <- AngularCoef((TabelaLVariavel[7, 3])/10^2, 
	(TabelaLVariavel[3, 3])/10^2, 
	TabelaLVariavel[7, 2], 
	(TabelaLVariavel[3, 2]))

YModulus3 <- YoungModulusByCoef(m3, 
	(TabelaLVariavel[3, 3] - TabelaLVariavel[7, 3])/10^2, 
	d, b)/10^10
#22.30575 10^10 Pa

TabelaComparacaoYoung[3] <- round(abs(YModulus3 - TabelaComparacaoYoung[[1]]), 1)

