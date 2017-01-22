#Disclaimer:
#This code was made following the guide:
#'Data Mining with R: learning by case studies' - Torgo, Luis (2005)

#Get the training data
algae <- read.table('./data/Analysis.txt', 
	header = FALSE, 
	dec = '.', 
	col.names=c('season','size','speed','mxPH','mnO2','Cl', 'NO3','NH4','oPO4','PO4','Chla','a1','a2','a3','a4','a5','a6','a7'), 
	na.strings=c('XXXXXXX'));

#Take a look in the first five rows of the training data
head(algae); #Equivalent of algae[1:5,]

#Summarize.
summary(algae);

#First things first. Missing values?
msg <- which(!complete.cases(algae));
msg;
length(msg);
#16 NA values. Take a better close on then.
algae[msg,];

#cases 62 and 199 seens not very healpful. Get rid of then.
algae <- algae[-c(62,199),];

#Make sure this works.
msg <- which(!complete.cases(algae));
length(msg);

#OK, but there's more. Let's create a function that applies the median of 
#the values of each column, on the NA's values of that same column.
set.median <- function(input){
	clean.input <- input;
	for(i in 1:length(clean.input)){
		if(is.numeric(clean.input[,i])){
			auxvec <- is.na(clean.input[,i]);
			clean.input[auxvec,i] <- median(sort(clean.input[,i]));
		}
	}
	return (clean.input);
};

#Apply to a new dataset
clean.algae <- set.median(algae);

#Verify if it works
which(!complete.cases(clean.algae));

#OK. Now lets take a look at the covariance between the variables
symnum(cor(clean.algae[,4:length(clean.algae)]));

#Foud a great relation between PO4 and oPO4 (kinda expected, tho).
#Let's work with the a1 for instance.
#Check its variance in relation of other variables.
anova(lm(a1 ~ ., data=clean.algae[,1:12]));
#The size of the river seens to contribute more, and the season less. Take it off.
algae.lm.a1 <- step(lm(a1 ~ ., data=clean.algae[,1:12]));
anova(algae.lm.a1);

#Give a try.
algae.predict.a1 <- predict(algae.lm.a1, clean.algae);
algae.predict.a1;
#Set negative frequencies to zero.
algae.predict.a1[algae.predict.a1 < 0] <- 0;

#Calculate NMSE
mean((algae.predict.a1 - clean.algae[,'a1'])^2)/mean((mean(clean.algae[,'a1']) - clean.algae[,'a1'])^2);
#0.6343095

#Construct a tree
library(rpart);
algae.tree <- rpart(a1 ~ ., data=clean.algae);
plot(algae.tree);
text(algae.tree);

#Make a prediction
algae.predict.tree <- predict(algae.tree, clean.algae);

#NMSE
mean((algae.tree.predict - clean.algae[,'a1'])^2)/mean((mean(clean.algae[,'a1']) - clean.algae[,'a1'])^2);
#0.3871729



#Let's check between the two models.
#k-fold validation
##****************************************************************************
#DISCLAIMER: those functions belown are not mine. 
#source: 'Data Mining with R: learning by case studies' - Torgo, Luis (2005)
##****************************************************************************

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
cross.validation <- function(algae.dirty,algae,n.folds=10) {
	n <- nrow(algae.dirty)
	idx <- sample(n,n)
	algae.dirty <- algae.dirty[idx,]
	algae <- algae[idx,]
	n.each.part <- n %/% n.folds
	perf.lm <- vector()
	perf.rt <- vector()
	for(i in 1:n.folds) {
		cat('Fold ',i,'\n')
		out.fold <- ((i-1)*n.each.part+1):(i*n.each.part)
		l.model <- lm(a1 ~ .,algae[-out.fold,1:12])
		l.model <- step(l.model)
		l.model.preds <- predict(l.model,algae[out.fold,1:12])
		l.model.preds <- ifelse(l.model.preds < 0,0,l.model.preds)
		r.model <- reliable.rpart(a1 ~ .,algae.dirty[-out.fold,1:12])
		r.model.preds <- predict(r.model,algae.dirty[out.fold,1:12])
		perf.lm[i] <- mean((l.model.preds-algae.dirty[out.fold,'a1'])^2) /
			mean((mean(algae.dirty[-out.fold,'a1'])-algae.dirty[out.fold,'a1'])^2)
		perf.rt[i] <- mean((r.model.preds-algae.dirty[out.fold,'a1'])^2) /
			mean((mean(algae.dirty[-out.fold,'a1'])-algae.dirty[out.fold,'a1'])^2)
	}
	list(lm=list(avg=mean(perf.lm),std=sd(perf.lm),fold.res=perf.lm),
		rt=list(avg=mean(perf.rt),std=sd(perf.rt),fold.res=perf.rt))
}

#Make k-fold validation
algae.kf10v <- cross.validation(algae, clean.algae);
algae.kf10v;

#Wilcox.test verify if there's a significant difference between the two models.
wilcox.test(algae.kf10v$lm$fold.res, algae.kf10v$rt$fold.res, paired=T);
#Negative, there's no reason to prefer a model over another.

#Let's get the test data now.
algae.test <- read.table('./data/Eval.txt',
	dec='.',
	header=FALSE,
	col.names=c('season','size','speed','mxPH','mnO2','Cl',
	'NO3','NH4','oPO4','PO4','Chla'),
	na.strings=c('XXXXXXX'));

#NA values?
which(!complete.cases(algae.test));

#Get rid of then using the same method of the training data.
clean.algae.test <- set.median(algae.test);

#Verify if it works.
which(!complete.cases(clean.algae.test));

#OK. Now time to predict some data.
#First into the linear model
lm.all <- function(train, test){
	#Create the base of the output
	res <- list();
	res.models <- list();
	res.predics <- list();
	#Loops for every type of algae
	for(k in 1:7){
		#Create a optmized linear model for each algae
		res$models[[k]] <- step(lm(as.formula(paste(names(train)[11 + k],'~ .')), 
			data=train[, c(1:11, 11 + k)]));
		#Predict for every algae
		p <- predict(res$models[[k]], test);
		#Set negative frequencies to zero
		res$predics[[k]] <- ifelse(p < 0, 0, p);
	};
	return (res);
};

algae.lm.result <- lm.all(clean.algae, clean.algae.test);

#Now with the tree
rt.all <- function(train, test){
	#Create the base of the output
	res <- list();
	res.models <- list();
	res.predics <- list();
	#Loops for every type of algae
	for(k in 1:7){
		#Create a optmized linear model for each algae
		res$models[[k]] <- reliable.rpart(as.formula(paste(names(train)[11 + k],'~ .')), 
			data=train[, c(1:11, 11 + k)]);
		#Predict for every algae
		res$predics[[k]] <- predict(res$models[[k]], test);
	};
	return (res);
};

algae.tree.result <- rt.all(clean.algae, clean.algae.test);

#K-fold Cross-validation in everydata
##****************************************************************************
#DISCLAIMER: this function belown are not mine. 
#source: 'Data Mining with R: learning by case studies' - Torgo, Luis (2005)
##****************************************************************************
cv.all <- function(algae.dirty, algae,n.folds=10) {
	n <- nrow(algae.dirty)
	idx <- sample(n,n)
	algae.dirty <- algae.dirty[idx,]
	algae <- algae[idx,]
	n.each.part <- n %/% n.folds
	perf.lm <- matrix(nrow=n.folds,ncol=7)
	perf.rt <- matrix(nrow=n.folds,ncol=7)
	perf.comb <- matrix(nrow=n.folds,ncol=7)

	for(i in 1:n.folds) {
		cat('Fold ',i,'\n')
		out.fold <- ((i-1)*n.each.part+1):(i*n.each.part)
		for(a in 1:7) {
			form <- as.formula(paste(names(algae.dirty)[11+a],"~."))
			l.model <- lm(form,algae[-out.fold,c(1:11,11+a)])
			l.model <- step(l.model)
			l.model.preds <- predict(l.model,algae[out.fold,c(1:11,11+a)])
			l.model.preds <- ifelse(l.model.preds < 0,0,l.model.preds)

			r.model <- reliable.rpart(form,algae.dirty[-out.fold,c(1:11,11+a)])
			r.model.preds <- predict(r.model,algae.dirty[out.fold,c(1:11,11+a)])

			perf.lm[i,a] <- mean((l.model.preds-algae.dirty[out.fold,11+a])^2) /
				mean((mean(algae.dirty[-out.fold,11+a])-algae.dirty[out.fold,11+a])^2)
			perf.rt[i,a] <- mean((r.model.preds-algae.dirty[out.fold,11+a])^2) /
				mean((mean(algae.dirty[-out.fold,11+a])-algae.dirty[out.fold,11+a])^2)

			wl <- 1-perf.lm[i,a]/(perf.lm[i,a]+perf.rt[i,a])
			wr <- 1-wl

			comb.preds <- wl*l.model.preds + wr*r.model.preds
			perf.comb[i,a] <- mean((comb.preds-algae.dirty[out.fold,11+a])^2) /
				mean((mean(algae.dirty[-out.fold,11+a])-algae.dirty[out.fold,11+a])^2)
			cat(paste("Algal a",a,sep=""),"\tlm=",perf.lm[i,a],"\trt=",
				perf.rt[i,a],"\tcomb=",perf.comb[i,a],"\n")
		}
	}
	lm.res <- apply(perf.lm,2,mean)
	names(lm.res) <- paste("a",1:7,sep="")
	rt.res <- apply(perf.rt,2,mean)
	names(rt.res) <- paste("a",1:7,sep="")
	comb.res <- apply(perf.comb,2,mean)
	names(comb.res) <- paste("a",1:7,sep="")
	list(lm=lm.res,rt=rt.res,comb=comb.res)
};

algae.kf10v.all <- cv.all(algae, clean.algae);

#Ok. Now lets combine the two solutions, alongside its respectives weights.
combine.sol <- function(lm.res, tree.res, weight.sol) {
	#create final result structure
	final <- matrix(nrow=nrow(clean.algae.test), ncol=7);
	for(i in 1:7){
		weight.lm <- (1 - (weight.sol$lm[i]/(weight.sol$lm[i] + weight.sol$rt[i])));
		weight.rt <- (1 - weight.lm);
		final[,i] <- weight.lm*lm.res[[i]] + weight.rt*tree.res[[i]];
	};
	colnames(final) <- paste('a', 1:7, sep='');
	return(final);
};

#Combine the two solutions
algae.final <- combine.sol(algae.lm.result$predics, algae.tree.result$predics, algae.kf10v.all);

#Check results
summary(algae.final);
head(algae.final);

#Now time to use the answers to check the accuracy of the results.
algae.spoiler <- read.table('./data/Sols.txt',
	dec ='.',
	header=FALSE,
	col.names=c('a1', 'a2', 'a3', 'a4', 'a5', 'a6', 'a7'),
	na.strings=c('XXXXXXX'));

#Statistical numeric results
#Squared errors
sq.errs <- (algae.final - algae.spoiler)^2;
#Absolute error (modulus)
abs.errs <- abs(algae.final - algae.spoiler);

#MSE (Mean Squared Error)
apply(sq.errs, 2, mean);

#MAD (Mean Absolute Error)
apply(abs.errs, 2, mean);

#NMSE (Normalized Mean Squared Error)
baseline.preds <- apply(algae[,paste('a', 1:7, sep='')], 2, mean);
base.sq.errs <- ((matrix(rep(baseline.preds,nrow(algae.spoiler)), byrow=T, ncol=T) - algae.spoiler)^2);
apply(sq.errs, 2, mean)/apply(base.sq.errs, 2, mean);


#Ok. We got some numbers, but they're nothing without a comparator.
#Let's calculate the NMSE with both bare strategies.
#First for linear model
aux.lm.preds <- matrix(ncol=7, nrow=140);
for(i in 1:7)
	aux.lm.preds[,i] <- algae.lm.result$predics[[i]];
lm.sq.errs <- (aux.lm.preds -algae.spoiler)^2;
apply(lm.sq.errs,2,mean)/apply(sq.errs,2,mean);
#In general, we got considerable success on a4, but pay a little bit in a5 and a6.

#Now with tree model
aux.tree.preds <- matrix(ncol=7, nrow=140);
for(i in 1:7)
	aux.tree.preds[,i] <- algae.tree.result$predics[[i]];
tree.sq.errs <- (aux.tree.preds -algae.spoiler)^2;
apply(tree.sq.errs,2,mean)/apply(sq.errs,2,mean);
#In this case, we went so much better combining both models than using a bare regression tree.

#Conclusion: we got some success on certain algaes (specially in a4 and a7), but
#still have plenty rooms for improvements.
sort(apply(sq.errs, 2, mean)/apply(base.sq.errs, 2, mean));