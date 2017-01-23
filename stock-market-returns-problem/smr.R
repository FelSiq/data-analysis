#Note: 'its' was removed from CRAN, so you need to install it from source (still in CRAN database) using
#install.packages(my_little_path, repos = NULL, type="source"); 

#Just to train
#1. Read Multivariable Time Series from CSV files (ok)
library(its);
sp500 <- readcsvIts('./sp500.csv',
	header=TRUE,
	sep=' ',
	col.names=c("Index","Open","High","Low","Close","Volume","AdjClose"),
	quote='"',
	as.is=TRUE,
	dec='.');

head(sp500);

#2. Read from MySQL data frame (ok)
library(RMySQL);

db.driver <- dbDriver("MySQL");
ch <- dbConnect(db.driver, 
	dbname="stocks", 
	user="root", 
	password="not_even_close_mah_boi ;)", 
	unix.socket='/var/run/mysqld/mysqld.sock');

sp500 <- dbGetQuery(ch, "select * from gspc");

dbDisconnect(ch);
dbUnloadDriver(db.driver);

sp500 <- its(as.matrix(sp500[,2:5]),dates=as.POSIXct(sp500[,1]));

head(sp500);

#3. Read directly from the yahoo online database (ok)
library(xts);
library(tseries);
GSPC <- as.xts(get.hist.quote("^GSPC",
          start="1970-01-02",
          end='2009-09-15',
          quote=c("Open", "High", "Low", "Close","Volume","AdjClose")));

head(GSPC);

##****************************************************
#Now data pre-processing
