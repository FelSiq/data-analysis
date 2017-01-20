#Data free avaliable on 'http://www.dcc.fc.up.pt/~ltorgo/DataMiningWithR/datasets.html'
#The training data is in 'Analysis.txt' file.

algae.train <- read.table('./data/Analysis.txt',
	header=FALSE,
	dec='.',
	col.names=c('season','size','speed','mxPH','mnO2','Cl',
	'NO3','NH4','oPO4','PO4','Chla','a1','a2','a3','a4',
	'a5','a6','a7'),
	na.strings=c('XXXXXXX'));

#Show the first 5 samples on the data frame
algae.train[1:5,];

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
symnum(cor(algae[,4:18],use="complete.obs"))
#Found a great correlation between PO4 and oPO4 (kinda expected)
lm(PO4 ~ oPO4,data=algae.train)
which(is.na(algae.train$PO4))
#Fill the NA gap on PO4 data, using the values given by the lm() func
algae.train[28,'PO4'] <- 42.897 + 1.293 * algae.train[28,'oPO4']

#Study the correlation between the size of river, mxPH and the speed.
histogram(~ mxPH | size*speed,data=algae.train)

#Take a look between the relation of river size and the algaes frequency.
library(Hmisc)
bwplot(size ~ a1 + a2 + a3 + a4 + a5 + a6 + a7, data=algae.train,panel=panel.bpplot,
probs=seq(.01,.49,by=.01), datadensity=TRUE,
ylab='River Size',xlab='Algaes')
#In general, looks like smaller rivers benefits more the algaes.

#Verify if, in general, there's a "season preference".
bwplot(season ~ a1 + a2 + a3 + a4 + a5 + a6 + a7, data=algae.train,panel=panel.bpplot,
probs=seq(.01,.49,by=.01), datadensity=TRUE,
ylab='Season',xlab='Algaes')
#Conclusion: "Autumn" shows higher frequencies, while "summer" shows smaller frequencies.

