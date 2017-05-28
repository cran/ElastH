### R code from vignette source 'ElastH.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: options
###################################################
options(width=70, max.print=50)


###################################################
### code chunk number 2: Instalar (eval = FALSE)
###################################################
## install.packages("ElastH")


###################################################
### code chunk number 3: carregar
###################################################
library(ElastH)


###################################################
### code chunk number 4: data
###################################################
data(Exemplo)


###################################################
### code chunk number 5: Exemplo.y
###################################################
Exemplo$y
Exemplo$Hpib


###################################################
### code chunk number 6: define.criar.dlm1
###################################################
modelo <- Exemplo$modelo


###################################################
### code chunk number 7: criar.dlm1 (eval = FALSE)
###################################################
## modelo <- criar.dlm(Exemplo$y)


###################################################
### code chunk number 8: classExemplo
###################################################
class(Exemplo$y)


###################################################
### code chunk number 9: modelo1
###################################################
modelo$comp


###################################################
### code chunk number 10: testes
###################################################
modelo$q
modelo$h
modelo$nt


###################################################
### code chunk number 11: define.criar.dlm2
###################################################
modelo <- Exemplo$modelo2


###################################################
### code chunk number 12: criar.dlm2 (eval = FALSE)
###################################################
## modelo <- criar.dlm(Exemplo$y, X=Exemplo$Hpib)


###################################################
### code chunk number 13: criar.dlm2.print
###################################################
modelo$comp


###################################################
### code chunk number 14: definir.criardlm3
###################################################
modelo <- Exemplo$modelo3


###################################################
### code chunk number 15: criar.dlm3.noteval (eval = FALSE)
###################################################
## modelo <- criar.dlm(Exemplo$y, X=Exemplo$Hpib, inclinacao="F")


###################################################
### code chunk number 16: criar.dlm3.print
###################################################
modelo$comp


###################################################
### code chunk number 17: criar.dlm4 (eval = FALSE)
###################################################
## modelo <- criar.dlm(Exemplo$y, X=Exemplo$Hpib, comeco=2007, 
##                     fim=c(2014,4))


###################################################
### code chunk number 18: todas.dlms (eval = FALSE)
###################################################
## lista.dlm <- todas.dlms(Exemplo$y, X=Exemplo$Hpib, 
##                         comeco=2005, fim=c(2014,4))
## str(lista.dlm,1)
## lista.dlm[[1]]$comp


###################################################
### code chunk number 19: todas.dlms.print
###################################################
str(Exemplo$lista.y,1)
Exemplo$lista.y[[1]]$comp


###################################################
### code chunk number 20: classeigual (eval = FALSE)
###################################################
## class(lista.dlm[[1]]) == class(modelo)


###################################################
### code chunk number 21: classeigual.print
###################################################
class(Exemplo$lista.y[[1]]) == class(modelo)


###################################################
### code chunk number 22: elasticidade (eval = FALSE)
###################################################
## resultado <- calcular.elasticidades(Exemplo$receitas, 
##              Hpib=Exemplo$Hpib, Hpet=Exemplo$Hpet, fim=c(2015,4))


###################################################
### code chunk number 23: exemplo
###################################################
Exemplo$receitas


###################################################
### code chunk number 24: define.resultado
###################################################
resultado <- Exemplo$resultado


###################################################
### code chunk number 25: resultado.print
###################################################
str(resultado,1)


###################################################
### code chunk number 26: resultado.trt4
###################################################
resultado$trt[[4]]$comp


###################################################
### code chunk number 27: resultado.tgc7
###################################################
resultado$tgc[[7]]$comp


###################################################
### code chunk number 28: exportarcao.show (eval = FALSE)
###################################################
## resultado.exportacao <- exportar(resultado)


###################################################
### code chunk number 29: salvando (eval = FALSE)
###################################################
## write.csv2(resultado.exportacao, file="Caminho/Desejado/Nome.csv")


###################################################
### code chunk number 30: Script (eval = FALSE)
###################################################
## arquivo   <- "elasticidades.csv"
## Receitas  <- NULL
## Hpib      <- NULL
## Hpet      <- NULL
## fim       <- c(2016,4)
## tce     <- NULL
## comeco <- list(
##   trt  = 2000,
##   tfp  = 1997,
##   trc  = 1997,
##   ti   = 2001,
##   tm   = 2005,
##   tgc  = 2005,
##   roy  = 2000,
##   pe   = 2000,
##   tran = 2002,
##   icms = 1997,
##   iss  = 2002)
## 
## 
## resultado <- list(
##   trt  = todas.dlms(Receitas[, "TRT"] , X = Hpib,
##                     comeco = comeco$trt , fim = fim),
##   trt1 = todas.dlms(Receitas[, "TRT"] , X = lag(Hpib,-1),
##                     comeco = comeco$trt , fim = fim),
##   trt2 = todas.dlms(Receitas[, "TRT"] , X = cbind(Hpib, lag(Hpib, -1)),
##                     comeco = comeco$trt , fim = fim),
##   
##   tfp  = todas.dlms(Receitas[, "TFP"] , X = Hpib,
##                     comeco = comeco$tfp , fim = fim),
##   tfp1 = todas.dlms(Receitas[, "TFP"] , X = lag(Hpib,-1),
##                     comeco = comeco$tfp , fim = fim),
##   tfp2 = todas.dlms(Receitas[, "TFP"] , X = cbind(Hpib, lag(Hpib, -1)),
##                     comeco = comeco$tfp , fim = fim),
## 
##   trc  = todas.dlms(Receitas[, "TRC"] , X = Hpib,
##                     comeco = comeco$trc , fim = fim),
##   trc1 = todas.dlms(Receitas[, "TRC"] , X = lag(Hpib,-1),
##                     comeco = comeco$trc , fim = fim),
##   trc2 = todas.dlms(Receitas[, "TRC"] , X = cbind(Hpib, lag(Hpib, -1)),
##                     comeco = comeco$trc , fim = fim),
## 
##   ti   = todas.dlms(Receitas[, "TI"]  , X = Hpib,
##                     comeco = comeco$ti  , fim = fim),
##   ti1  = todas.dlms(Receitas[, "TI"]  , X = lag(Hpib,-1),
##                     comeco = comeco$ti  , fim = fim),
##   ti2  = todas.dlms(Receitas[, "TI"]  , X = cbind(Hpib, lag(Hpib, -1)),
##                     comeco = comeco$ti  , fim = fim),
## 
##   tm   = todas.dlms(Receitas[, "TM"]  , X = cbind(Hpib, log(tce)),
##                     comeco = comeco$tm, fim = fim),
##   tm1  = todas.dlms(Receitas[, "TM"]  , X = cbind(lag(Hpib,-1), log(tce)),
##                     comeco = comeco$tm, fim = fim),
##   tm2  = todas.dlms(Receitas[, "TM"], 
##                     X = cbind(Hpib, lag(Hpib, -1), log(tce)),
##                     comeco = comeco$tm, fim = fim),
## 
##   tgc  = todas.dlms(Receitas[, "TGC"] , X = Hpib,
##                     comeco = comeco$tgc , fim = fim),
##   tgc1 = todas.dlms(Receitas[, "TGC"] , X = lag(Hpib, -1),
##                     comeco = comeco$tgc , fim = fim),
##   tgc2 = todas.dlms(Receitas[, "TGC"] , X = cbind(Hpib, lag(Hpib, -1)),
##                     comeco = comeco$tgc , fim = fim),
## 
##   roy  = todas.dlms(Receitas[, "ROY"] , X = Hpet,
##                     comeco = comeco$roy , fim = fim),
##   roy1 = todas.dlms(Receitas[, "ROY"] , X = lag(Hpet, -1),
##                     comeco = comeco$roy , fim = fim),
##   roy2 = todas.dlms(Receitas[, "ROY"] , X = cbind(Hpet, lag(Hpet, -1)),
##                     comeco = comeco$roy , fim = fim),
##   
##   pe   = todas.dlms(Receitas[, "PE"]  , X = Hpet,
##                     comeco = comeco$pe  , fim = fim),
##   pe1  = todas.dlms(Receitas[, "PE"]  , X = lag(Hpet, -1),
##                     comeco = comeco$pe  , fim = fim),
##   pe2  = todas.dlms(Receitas[, "PE"]  , X = cbind(Hpet, lag(Hpet, -1)),
##                     comeco = comeco$pe  , fim = fim),
## 
##   tran = todas.dlms(Receitas[, "TRAN"], X = Hpib,
##                     comeco = comeco$tran, fim = fim),
##   tran1= todas.dlms(Receitas[, "TRAN"], X = lag(Hpib, -1),
##                     comeco = comeco$tran, fim = fim),
##   tran2= todas.dlms(Receitas[, "TRAN"], X = cbind(Hpib, lag(Hpib, -1)),
##                     comeco = comeco$tran, fim = fim),
## 
##   icms = todas.dlms(Receitas[, "ICMS"], X = Hpib,
##                     comeco = comeco$icms, fim = fim),
##   icms1= todas.dlms(Receitas[, "ICMS"], X = lag(Hpib,-1),
##                     comeco = comeco$icms, fim = fim),
##   icms2= todas.dlms(Receitas[, "ICMS"], X = cbind(Hpib, lag(Hpib, -1)),
##                     comeco = comeco$icms, fim = fim),
## 
##   iss  = todas.dlms(Receitas[, "ISS"] , X = Hpib,
##                     comeco = comeco$iss , fim = fim),
##   iss1 = todas.dlms(Receitas[, "ISS"] , X = lag(Hpib, -1),
##                     comeco = comeco$iss , fim = fim),
##   iss2 = todas.dlms(Receitas[, "ISS"] , X = cbind(Hpib, lag(Hpib, -1)),
##                     comeco = comeco$iss , fim = fim)
## )
## 
## ret <- exportar(resultado)
## write.csv2(ret, file=arquivo)


