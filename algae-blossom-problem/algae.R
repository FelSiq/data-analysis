#Data free avaliable on 'http://www.dcc.fc.up.pt/~ltorgo/DataMiningWithR/datasets.html'
#The training data is in 'Analysis.txt' file.
#All code atributed to:
#	Data Mining with R:	learning by case studies
#	Luis Torgo
#	LIACC-FEP, University of Porto
#	R. Campo Alegre, 823 - 4150 Porto, Portugal
#	email: ltorgo@liacc.up.pt
#	http://www.liacc.up.pt/∼ltorgo
#	August 10, 2005
#

algae.train <- read.table('./data/Analysis.txt',
	header=FALSE,
	dec='.',
	col.names=c('season','size','speed','mxPH','mnO2','Cl',
	'NO3','NH4','oPO4','PO4','Chla','a1','a2','a3','a4',
	'a5','a6','a7'),
	na.strings=c('XXXXXXX'));

#Show the first 5 samples on the data frame
algae.train[1:5,];

#Load 'ggplot2' for ggplot.
library(ggplot2)

#Playing a little with the data
#1. Verify a possible correlation between season and mxPH 
ggplot(algae.train, aes(x = mxPH, fill = factor(season))) +
  geom_bar() +
  facet_wrap(~season) + 
  xlab("Season") +
  ylab("Total Count") +
labs(fill = "Season") 

#Conclusion: Winter presents higher values than the other seasons.
#2. Take a closer look in mxPH data.	
algae.train$mxPH;

#One missing value? Where is it?
which(is.na(algae.train$mxPH));

#Take a look in mean and median values.
mean(algae.train$mxPH[!is.na(algae.train$mxPH)])
median(algae.train$mxPH[!is.na(algae.train$mxPH)])

#They're pretty close of each other. Assume that the
#missing value is = median.
algae.train$mxPH[which(is.na(algae.train$mxPH))] <- median(algae.train$mxPH[!is.na(algae.train$mxPH)]);

#More data correction: check all missing values
algae.train[!complete.cases(algae.train),]
#Get rid of the worst cases (index 62 and 199)
algae.train <- algae.train[-c(62,199)]

#Show the correlation between the variables, in order to pick the best choice in filling the others NA values
symnum(cor(algae.train[,4:18],use="complete.obs"))
#Found a great correlation between PO4 and oPO4 (kinda expected)
lm(PO4 ~ oPO4,data=algae.train)
which(is.na(algae.train$PO4))
#Fill the NA gap on PO4 data, using the values given by the lm() func
algae.train[28,'PO4'] <- 42.897 + 1.293 * algae.train[28,'oPO4']

#Let's get rid of the others NA values.
which(!complete.cases(algae.train))
library(cluster)
dist.mtx <- as.matrix(daisy(algae,stand=T))

#See the most similar ones and get a vector of their indeces
sort(dist.mtx[,38])[2:11]
as.integer(names(sort(dist.mtx[38,])[2:11]))

#Fill the gap with the median of the 10 most similiar values
algae.train[38, 'mnO2'] <- median(algae.train[as.integer(names(sort(dist.mtx[38,])[2:11])),'mnO2'])
algae.train[38,]

#Next. Now with the case 55
apply(algae.train[as.integer(names(sort(dist.mtx[55,])[2:11])), which(is.na(algae.train[55,]))], 2, median, na.rm=T)
algae.train[55,]

#Much work to do, better create a function to automate the process
central.value <- function(x) {
	if (is.numeric(x)) 
		median(x,na.rm=T)
	else if (is.factor(x)) 
		levels(x)[which.max(table(x))]
	else {
		f <- as.factor(x);
		levels(f)[which.max(table(f))]
	}
}

#Smash the function in everyplace it's needed.
for(r in which(!complete.cases(algae.train)))
	algae.train[r,which(is.na(algae.train[r,]))] <-
		apply(algae.train[as.integer(names(sort(dist.mtx[r,])[2:11])),
		which(is.na(algae.train[r,])),drop=F],
		2,
		central.value);

#Load 'Hmisc' for histogram
library(Hmisc)
#Study the correlation between the size of river, mxPH and the speed.
histogram(~ mxPH | size*speed,data=algae.train)
stripplot(size ~ mxPH | speed, data=algae.train, jitter=T)

#Take a look between the relation of river size and the algaes frequency.
bwplot(size ~ a1 + a2 + a3 + a4 + a5 + a6 + a7, data=algae.train,panel=panel.bpplot,
probs=seq(.01,.49,by=.01), datadensity=TRUE,
ylab='River Size',xlab='Algaes')
#In general, looks like smaller rivers benefits more the algaes.

#Verify if, in general, there's a "season preference".
bwplot(season ~ a1 + a2 + a3 + a4 + a5 + a6 + a7, data=algae.train,panel=panel.bpplot,
probs=seq(.01,.49,by=.01), datadensity=TRUE,
ylab='Season',xlab='Algaes')
#Conclusion: "Autumn" shows higher frequencies, while "summer" shows smaller frequencies.

# Okay. Let's get an in-depth analysis, using the lm() function.
lm.a1 <- lm(a1 ~ .,data=algae.train[,1:12])
summary(lm.a1)
anova(lm.a1)
#'season' seens to contribute less. Let's take it off.
lm2.a1 <- update(lm.a1, . ~ ., ~season)
summary(lm2.a1)
anova(lm2.a1)

anova(lm.a1, lm2.a1)

#And then...
final.lm <- step(lm.a1)
summary(final.lm)

library(rpart)
#Don't seen a very good aproach, tho.
# Let's take a look in tree regression method.
#Load the data once again, and remove the worst samples 62 and 199.
algae <- read.table('./data/Analysis.txt',
	header=F,
	dec='.',
	col.names=c('season','size','speed','mxPH','mnO2','Cl','NO3',
	'NH4','oPO4','PO4','Chla','a1','a2','a3','a4','a5','a6','a7'),
	na.strings=c('XXXXXXX'))
	algae <- algae[-c(62,199),]
	rt.a1 <- rpart(a1 ~ .,data=algae[,1:12])
algae <- algae[-c(62,199),]

central.value <- function(x) {
	if (is.numeric(x)) 
		median(x,na.rm=T)
	else if (is.factor(x)) 
		levels(x)[which.max(table(x))]
	else {
		f <- as.factor(x);
		levels(f)[which.max(table(f))]
	}
}

#Smash the function in everyplace it's needed.
for(r in which(!complete.cases(algae)))
	algae[r,which(is.na(algae[r,]))] <-
		apply(algae[as.integer(names(sort(dist.mtx[r,])[2:11])),
		which(is.na(algae[r,])),drop=F],
		2,
		central.value);

rt.a1 <- rpart(a1 ~ .,data=algae[,1:12])

#Plot the tree and take a look
rt.a1
plot(rt.a1,uniform=T,branch=1, margin=0.1, cex=0.9)
text(rt.a1,cex=0.75)

reliable.rpart <- function(form,data,se=1,cp=0,verbose=T,...) {
	tree <- rpart(form,data,cp=cp,...)
	if (verbose && ncol(tree$cptable) < 5)
	warning("No pruning will be carried out because no estimates were obtained.")
	rt.prune(tree,se,verbose)
	}
rt.prune <- function(tree,se=1,verbose=T,...) {
	if (ncol(tree$cptable) < 5) tree
		else {
		lin.min.err <- which.min(tree$cptable[,4])

		if (verbose && lin.min.err == nrow(tree$cptable))
			warning("Minimal Cross Validation Error is obtained
			at the largest tree.\n Further tree growth
			(achievable through smaller 'cp' parameter value),\n
			could produce more accurate tree.\n")
		tol.err <- tree$cptable[lin.min.err,4] + se * tree$cptable[lin.min.err,5]
		se.lin <- which(tree$cptable[,4] <= tol.err)[1]
		prune.rpart(tree,cp=tree$cptable[se.lin,1]+1e-9)
	}
}


first.tree <- rpart(a1 ~ .,data=algae[,1:12])
plot(first.tree)
text(first.tree)

#Let's set predict()
lm.predictions.a1 <- predict(final.lm,algae)
rt.predictions.a1 <- predict(rt.a1,algae)

#MAE (mean absolute error) of lm.a1 and rt.a1
(mae.a1.lm <- mean(abs(lm.predictions.a1-algae[,'a1'])))
(mae.a1.rt <- mean(abs(rt.predictions.a1-algae[,'a1'])))

#MSE (mean squared error) of lm.a1 and rt.a1
(mse.a1.lm <- mean((lm.predictions.a1-algae[,'a1'])^2))
(mse.a1.rt <- mean((rt.predictions.a1-algae[,'a1'])^2))

#Well... all I can say is that numbers exists. What's the meaning, after all?
#Let's calculate de NMSE (Normalized Mean Squared Error)
#"This statistic calculates a ratio between the per formance of our models 
#and that of a baseline predictor, usually taken as the mean value of the target variable"
#"The NMSE is a unit-less error measure with values usually ranging from 0
#to 1. If your model is performing better than this very simple baseline predictor
#then the NMSE should be clearly below 1. The smaller the NMSE, the better.
#Values above 1 mean that your model is performing worse than simply predicting
#always the average for all cases!" - Torgo, Luis, "Data Mining with R: learning by case studies".

(nmse.a1.lm <- mean((lm.predictions.a1-algae[,'a1'])^2)/mean((mean(algae[,'a1'])-algae[,'a1'])^2))
(nmse.a1.rt <- mean((rt.predictions.a1-algae[,'a1'])^2)/mean((mean(algae[,'a1'])-algae[,'a1'])^2))

#Seens like we are going the right way. Let's take a real look.
old.par <- par(mfrow=c(2,1))
plot(lm.predictions.a1,algae[,'a1'],main="Linear Model",
	xlab="Predictions",ylab="True Values")
	abline(0,1,lty=2)

plot(rt.predictions.a1,algae[,'a1'],main="Regression Tree",
	xlab="Predictions",ylab="True Values")
	abline(0,1,lty=2)
	par(old.par)

plot(lm.predictions.a1,algae[,'a1'],main="Linear Model",
	xlab="Predictions",ylab="True Values")
abline(0,1,lty=2)
algae[identify(lm.predictions.a1,algae[,'a1']),]


#Negative values? Get rid of then.
sensible.lm.predictions.a1 <- ifelse(lm.predictions.a1 < 0, 0, lm.predictions.a1)

(mae.a1.lm <- mean(abs(sensible.lm.predictions.a1-algae[,'a1'])))
(nmse.a1.lm <- mean((sensible.lm.predictions.a1-algae[,'a1'])^2)/mean((mean(algae[,'a1'])-algae[,'a1'])^2))

#K-Fold cross validation
cross.validation <- function(all.data,clean.data,n.folds=10) {
	n <- nrow(all.data)
	idx <- sample(n,n)
	all.data <- all.data[idx,]
	clean.data <- clean.data[idx,]
	n.each.part <- n %/% n.folds
	perf.lm <- vector()
	perf.rt <- vector()
	for(i in 1:n.folds) {
		cat('Fold ',i,'\n')
		out.fold <- ((i-1)*n.each.part+1):(i*n.each.part)
		l.model <- lm(a1 ~ .,clean.data[-out.fold,1:12])
		l.model <- step(l.model)
		l.model.preds <- predict(l.model,clean.data[out.fold,1:12])
		l.model.preds <- ifelse(l.model.preds < 0,0,l.model.preds)
		r.model <- reliable.rpart(a1 ~ .,all.data[-out.fold,1:12])
		r.model.preds <- predict(r.model,all.data[out.fold,1:12])
		perf.lm[i] <- mean((l.model.preds-all.data[out.fold,'a1'])^2) /mean((mean(all.data[-out.fold,'a1'])-all.data[out.fold,'a1'])^2)
		perf.rt[i] <- mean((r.model.preds-all.data[out.fold,'a1'])^2) /mean((mean(all.data[-out.fold,'a1'])-all.data[out.fold,'a1'])^2)
	}
	list(lm=list(avg=mean(perf.lm),std=sd(perf.lm),fold.res=perf.lm),
		rt=list(avg=mean(perf.rt),std=sd(perf.rt),fold.res=perf.rt))
}


algae.dirty <- read.table('./data/Analysis.txt',
	header=FALSE,
	dec='.',
	col.names=c('season','size','speed','mxPH','mnO2','Cl',
	'NO3','NH4','oPO4','PO4','Chla','a1','a2','a3','a4',
	'a5','a6','a7'),
	na.strings=c('XXXXXXX'));
cv10.res <- cross.validation(algae.dirty, algae)
cv10.res

wilcox.test(cv10.res$lm$fold.res,cv10.res$rt$fold.res,paired=T)

