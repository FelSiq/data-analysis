#Parte 1: Volumes
#(RESUMO) Resultados obtidos:
#1. OBJETO CILINDRICO
#   1.1 VOLUME INDIRETO COM PAQUIMETRO (em cm^3, delta = +- 0.71):
#       Felipe: 9.30
#       Bruno: 9.26
#       Rafael: 9.25
#       Medio: 9.27
#   1.2 VOLUME INDIRETO COM MICROMETRO (em cm^3), delta = +- 0.04)
#       Felipe: 9.22 
#       Bruno: 9.24 
#       Rafael: 9.25 
#       Media: 9.24

#Parte 2: Densidade e identificacao do material
#   RESUMO:
#   DensidadePaqObjetoIndiretoFioSuspenso   : 2.696423
#   DensidadeMicObjetoIndiretoFioSuspenso   : 2.706111
#   DensidadePaqObjetoIndiretoFioRelaxado   : 2.69912
#   DensidadeMicObjetoIndiretoFioRelaxado   : 2.708818
#   DensidadeObjetoDiretoFioSuspenso        : 2.499 e 2.500
#   DensidadeObjetoDiretoFioRelaxado        : 2.502
#   Media de todas as densidades obtidas (com truncamento): 2.696
#   Material identificado (ref. na dada tabela): Aluminio

#Parte 3: 
#   DiametroMedioFioCobre: 2.21
#   DesvioAbsolutoMedio: 0.036

#A. Formulas (medidas + deltas/incertezas)
volume <- function(D, H) (D^2)*H*(pi/4.0)
volumeDelta <- function (D, H, DELTA_D, DELTA_H) (pi/2.0)*D*D*DELTA_D + (D^2)*(pi/4.0)*DELTA_H
Densidade <- function (m, V) m/V
DensidadeDelta <- function (m, V, DELTA_m, DELTA_v) (V*DELTA_m + m*DELTA_v)/(V^2)

#B. Precisao de Equipamento
PrecPaquimetro <- 0.05  #em mm
PrecMicrometro <- 0.01  #em mm
PrecBalanca <- 0.01     #em g
PrecProveta <- 1.0      #em ml

#C. Resultados obtidos em laboratorio (constantes)
VolumeDiretoObjeto <- 10.0          #em ml -> cm^3
VolumeDiretoObjetoDelta <- 1.0      #em ml
MassaFioSuspenso <- c(24.99, 25.00)  #em g
MassaFioRelaxado <- 25.02            #em g
DiametroFioCobre <- c(2.17, 2.20, 2.21, 2.18, 2.27, 2.17, 2.21, 2.33, 2.19, 2.17) #em mm, medidos com o micrometro.

#PARTE 1: VOLUMES
#Medidas (dados obtidos em laboratorio) + Medias aritmeticas
MedidasExtPaqD <- c(21.50, 21.45, 21.45)
MedidasExtPaqD[4] <- mean(MedidasExtPaqD)

MedidasExtPaqH <- c(29.55, 29.55, 29.55)
MedidasExtPaqH[4] <- mean(MedidasExtPaqH)

MedidasIntPaqD <- c(12.00, 12.00, 12.05)
MedidasIntPaqD[4] <- mean(MedidasIntPaqD)

MedidasIntPaqH <- c(12.65, 12.55, 12.50)
MedidasIntPaqH[4] <- mean(MedidasIntPaqH)

MedidasExtMicD <- c(21.42, 21.43, 21.45)
MedidasExtMicD[4] <- mean(MedidasExtMicD)

#Calculos somente com o uso do Paquimetro
#1.a Volume do cilindro macico + incerteza
VolExtPaq <- volume (MedidasExtPaqD, MedidasExtPaqH)
VolExtPaqDelta <- volumeDelta(MedidasExtPaqD, MedidasExtPaqH, PrecPaquimetro, PrecPaquimetro)

#2.a Volume "faltando" do cilindro macico + incerteza
VolIntPaq <- volume (MedidasIntPaqD, MedidasIntPaqH)
VolIntPaqDelta <- volumeDelta(MedidasIntPaqD, MedidasIntPaqH, PrecPaquimetro, PrecPaquimetro)

#3.a Volume do cilindro real + incertezas finais
VolFinalPaq <- (VolExtPaq - VolIntPaq)
VolFinalPaqDelta <- (VolExtPaqDelta + VolIntPaqDelta)

#Imprime os resultados, em mm^3.
                    #NOTA: TAIS MEDIDAS FORAM CONVERTIDAS PARA cm^3.
#VolFinalPaq*(1.0/1000.0)         #Felipe: 9.297455 Bruno: 9.258925 Rafael: 9.252774 Medio: 9.269468
#VolFinalPaqDelta*(1.0/1000.0)    #Felipe: 0.7142215 Bruno: 0.7116915 Rafael: 0.7131082 Medio: 0.7135521
                    
                    #Com truncamento:
                    #Volume indireto:
                    #Felipe: 9.30 Bruno: 9.26 Rafael: 9.25 Medio: 9.27
                    
                    #Incerteza
                    #Felipe: 0.71 Bruno: 0.71 Rafael: 0.71 Media: 0.71

#Calcula o desvio padrao dos dados obtidos, em cm^3.
DesvioPadraoVolPaq <- sd(VolFinalPaq[c(1,2,3)]*(1.0/10.0))

#Verifica a equivalencia dos Volumes Indireto e Direto (medido em laboratorio)
#abs(VolumeDiretoObjeto - VolFinalPaq[4]*(1.0/1000.0)) < 2*(max(DesvioPadraoVolPaq, PrecPaquimetro)) #VERDADEIRO, portanto sao equivalentes

#Fim dos calculos somente o Paquimetro

#Calculos do Paquimetro + Micrometro
#1.b Volume do cilindro macico + incerteza
VolExtMic <- volume (MedidasExtMicD, MedidasExtPaqH)
VolExtMicDelta <- volumeDelta(MedidasExtMicD, MedidasExtPaqH, PrecMicrometro, PrecPaquimetro)

#2.b Volume "faltando" do cilindro macico + incerteza
#As mesmas medidas de 2.a (VolIntPaq e VolIntPaqDelta)

#3.b Volume do cilindro real + incertezas finais
VolFinalMic <- (VolExtMic - VolIntPaq)
VolFinalMicDelta <- (VolExtMicDelta + VolIntPaqDelta)

#Imprime os resultados, em mm^3.
                    #NOTA: TAIS MEDIDAS FORAM CONVERTIDAS PARA cm^3.
#VolFinalMic*(1.0/1000.0)         #Felipe: 9.217766 Bruno: 9.239021 Rafael: 9.252774 Media: 9.236501
#VolFinalMicDelta*(1.0/1000.0)    #Felipe: 0.04218935 Bruno 0.04221291 Rafael: 0.04240172 Media: 0.04226792

                    #Com trucamento:
                    #Volume indireto:
                    #Felipe: 9.22 Bruno: 9.24 Rafael: 9.25 Media: 9.24
                    
                    #Incerteza:
                    #Felipe: 0.04 Bruno: 0.04 Rafael: 0.04 Media: 0.04

#Calcula o desvio padrao dos dados obtidos, em cm^3.
DesvioPadraoVolMic <- sd(VolFinalMic[c(1,2,3)]*(1.0/10.0))

#Verifica a equivalencia dos Volumes Indireto e Direto (medido em laboratorio)
#abs(VolumeDiretoObjeto - VolFinalMic[4]*(1.0/1000.0)) < 2*(max(DesvioPadraoVolMic, PrecMicrometro))

#Fim dos calculos com o Paquimetro + Micrometro

#Parte 2: Densidade e identificacao do material
#Valores obtidos em Laboratorio (revisao)
#VolumeDiretoObjeto <- 10.0          #em ml -> cm^3
#VolumeDiretoObjetoDelta <- 1.0      #em ml
#PesoFioSuspenso <- c(24.99, 25.00)  #em g
#PesoFioRelaxado <- 25.02            #em g

DensidadePaqObjetoIndiretoFioSuspenso <- mean(Densidade(MassaFioSuspenso, VolFinalPaq[4]*(1.0/1000.0)))
DensidadeMicObjetoIndiretoFioSuspenso <- mean(Densidade(MassaFioSuspenso, VolFinalMic[4]*(1.0/1000.0)))
DensidadePaqObjetoIndiretoFioRelaxado <- Densidade(MassaFioRelaxado, VolFinalPaq[4]*(1.0/1000.0))
DensidadeMicObjetoIndiretoFioRelaxado <- Densidade(MassaFioRelaxado, VolFinalMic[4]*(1.0/1000.0))
DensidadeObjetoDiretoFioSuspenso <- Densidade(MassaFioSuspenso, VolumeDiretoObjeto)
DensidadeObjetoDiretoFioRelaxado <- Densidade(MassaFioRelaxado, VolumeDiretoObjeto)

DensidadePaqObjetoIndiretoFioSuspenso   #2.696423
DensidadeMicObjetoIndiretoFioSuspenso   #2.706111
DensidadePaqObjetoIndiretoFioRelaxado   #2.69912
DensidadeMicObjetoIndiretoFioRelaxado   #2.708818
DensidadeObjetoDiretoFioSuspenso        #2.499 e 2.500
DensidadeObjetoDiretoFioRelaxado        #2.502

DensidadePaqIndFioSusDelta <- mean(DensidadeDelta(MassaFioSuspenso, 
    VolFinalPaq[4]*(1.0/1000.0), PrecBalanca, 
    VolFinalPaqDelta[4]*(1.0/1000.0)))
DensidadeMicIndFioSusDelta <- mean(DensidadeDelta(MassaFioSuspenso, 
    VolFinalMic[4]*(1.0/1000.0), PrecBalanca, 
    VolFinalMicDelta[4]*(1.0/1000.0)))
DensidadePaqIndFioRelDelta <- DensidadeDelta(MassaFioRelaxado, 
    VolFinalPaq[4]*(1.0/1000.0), PrecBalanca, 
    VolFinalPaqDelta[4]*(1.0/1000.0))
DensidadeMicIndFioRelDelta <- DensidadeDelta(MassaFioRelaxado, 
    VolFinalMic[4]*(1.0/1000.0), PrecBalanca, 
    VolFinalMicDelta[4]*(1.0/1000.0))
DensidadeDirFioSusDelta <- DensidadeDelta(MassaFioSuspenso, 
    VolumeDiretoObjeto, PrecBalanca, PrecProveta)
DensidadeDirFioRelDelta <- DensidadeDelta(MassaFioRelaxado, 
    VolumeDiretoObjeto, PrecBalanca, PrecProveta)

DensidadePaqIndFioSusDelta <- max(DensidadePaqIndFioSusDelta, PrecPaquimetro)
DensidadeMicIndFioSusDelta <- max(DensidadeMicIndFioSusDelta, PrecPaquimetro)
DensidadePaqIndFioRelDelta <- max(DensidadePaqIndFioRelDelta, PrecMicrometro)
DensidadeMicIndFioRelDelta <- max(DensidadeMicIndFioRelDelta, PrecMicrometro)
DensidadeDirFioSusDelta <- max(DensidadeDirFioSusDelta, PrecProveta)
DensidadeDirFioRelDelta <- max(DensidadeDirFioRelDelta, PrecProveta)

DensidadePaqIndFioSusDelta  # 0.05
DensidadeMicIndFioSusDelta  # 0.05
DensidadePaqIndFioRelDelta  # 0.02183987
DensidadeMicIndFioRelDelta  # 0.01347871
DensidadeDirFioSusDelta     # 1
DensidadeDirFioRelDelta     # 1

Densidades <- c(DensidadePaqObjetoIndiretoFioSuspenso,
    DensidadeMicObjetoIndiretoFioSuspenso,
    DensidadePaqObjetoIndiretoFioRelaxado,
    DensidadeMicObjetoIndiretoFioRelaxado)

#Com o Aluminio, Aluminio liqueido, Boro e Boro Amorfo.
DensidadeMedia <- mean(Densidades)
#2.615924 -> Aluminio (?)

abs(DensidadeMedia - 2.699) < 2 * (max(sd(Densidades),
    PrecMicrometro, PrecPaquimetro)) #Verdadeiro, portanto os dados sao equivalentes.

#Parte 3:
DiametroMedioFioCobre <- mean(DiametroFioCobre)
DiametroMedioFioCobre #2.21

DesvioAbsolutoMedio <- sum(abs(DiametroFioCobre - DiametroMedioFioCobre))/length(DiametroFioCobre)
DesvioAbsolutoMedio #0.036

#FIM