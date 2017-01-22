#Personal setup
#setwd('./Dropbox/study/AMtries/data-analysis/data-analysis/algae-blossom-problem');

#Get the training data
algae <- read.table('./data/Analysis.txt', 
	header = FALSE, 
	dec = '.', 
	col.names=c('season','size','speed','mxPH','mnO2','Cl', 'NO3','NH4','oPO4','PO4','Chla','a1','a2','a3','a4','a5','a6','a7'), 
	na.strings=c('XXXXXXX'));

#Take a look in the first five rows of the training data
head(algae) #Equivalent of algae[1:5,]

#Summarize.
summary(algae)

#First things first. Missing values?
msg <- which(!complete.cases(algae));
msg;
length(msg);
#16 NA values. Take a better close on then.
algae[msg,]

#cases 62 and 199 seens not very healpful. Get rid of then.
algae <- algae[-c(62,199),]

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
