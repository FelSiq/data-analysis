#Data is already avaliable
load("sales.RData")

#Split the "fraud", "unkn" and "ok" cases. 
sales.known.fraud <- sales[sales[ , 'Insp'] == "fraud", ]
sales.known.ok <- sales[sales[ , 'Insp'] == "ok", ]

(nrow(sales.known.ok) + nrow(sales.known.fraud))/nrow(sales) * 100
#Only 3.92% of the total data is known.

#Check for NA values
nrow(sales.known.fraud[!complete.cases(sales.known.fraud), ])
nrow(sales.known.ok[!complete.cases(sales.known.ok), ])
#71 and 115, respectively.
rm (sales.known.fraud, sales.known.ok)

#Let's get a deeper analysis in those, before filling then up.

#1) Data that misses both Quant and Val
100 * (nrow(sales[is.na(sales$Val) & is.na(sales$Quant), ])/nrow(sales))
#< 0.3%. Those values does not seens to contribute much. Remove then.
sales <- sales[-which(is.na(sales$Val) & is.na(sales$Quant)), ]

#2) Search for "problematic" vendors (high value missing rate)
#Top 10 vendors with highest "Quant" data missing 
prop <- 100 * (table(sales[is.na(sales$Quant), 'ID'])/table(sales$ID))
prop[order(prop, decreasing = T)[1:10]]
#v2925     v4356     v5537     v5836     v6044     v6058     v6065     v2923 
#100.00000 100.00000 100.00000 100.00000 100.00000 100.00000 100.00000  90.00000 
#    v4368     v2920 
# 88.88889  85.71429 
#There's 7 vendors that never give any information about their sold Quantities.
#However, that's not a "must remove" cases, because those informations
#may could be retrieved from another vendors of the same product.

#Top 10 vendors with highest "Value" data missing 
prop <- 100 * (table(sales[is.na(sales$Val), 'ID'])/table(sales$ID))
prop[order(prop, decreasing = T)[1:10]]
#    v5647       v74     v5946     v5290     v4472     v4022      v975     v2814 
#37.500000 22.222222 20.000000 15.384615 12.500000  9.756098  9.574468  9.090909 
#    v2892     v3739 
# 9.090909  8.333333 

#3) Search for "problematic" products (high value missing rate)
prop <- 100 * (table(sales[is.na(sales$Quant), 'Prod'])/table(sales$Prod))
prop[order(prop, decreasing = T)[1:10]]
#    p2442     p2443     p1653     p4101     p4243      p903     p3678     p3955 
#100.00000 100.00000  90.90909  85.71429  68.42105  66.66667  66.66667  64.28571 
#    p4464     p1261 
# 63.63636  63.33333 
#There's two products that never show up their values, a information that could not
#ever be get from now nor predicted. Better remove those cases.

prop <- 100 * (table(sales[is.na(sales$Val), 'Prod'])/table(sales$Prod))
prop[order(prop, decreasing = T)[1:10]]
#    p1110     p1022     p4491     p1462       p80     p4307     p4471     p2821 
#25.000000 17.647059 10.000000  7.500000  6.250000  5.882353  5.882353  5.389222 
#    p1017     p4287 
# 5.263158  5.263158 

sales <- sales[!(sales$Prod %in% c('p2442', 'p2443')), ]

#Update de number of levels on sales$Prod
nlevels(sales$Prod) #4548
sales$Prod <- factor(sales$Prod)
nlevels(sales$Prod) #4546

#No need anymore of prop table.
rm(prop)

#Now, time to fill up the remaining NA values.
#Each product has a habitual price. Let's assume his true price is the median of each one
#on the dataset. To enforce the vality of data, let's ignore fraud transactions for a moment.

#Get the unitary value to each product
unitary.value <-function(my.data){
	my.data$Val/my.data$Quant
}

#The idea below works because all cases, when both "Val" and "Quant" are unknown, are already removed.
sales["Uprice"] <- unitary.value(sales)
tPrice <- tapply(sales[sales$Insp != "fraud", "Uprice"], list(sales[sales$Insp != "fraud", "Prod"]), median, na.rm = T)

missing_Quant <- which(is.na(sales$Quant))
sales[missing_Quant, 'Quant'] <- sales[missing_Quant, 'Val'] / tPrice[sales[missing_Quant, 'Prod']]

missing_Val <- which(is.na(sales$Val))
sales[missing_Val, 'Val'] <- sales[missing_Val, 'Quant'] * tPrice[sales[missing_Val, 'Prod']]

rm (missing_Quant, missing_Val)
#Verify if works
sum(!complete.cases(sales$Val))
sum(!complete.cases(sales$Quant))
#All seens ok.

#Update the "Uprice" column
sales["Uprice"] <- unitary.value(sales)

#Now there's no NA values on the dataset:
sum(!complete.cases(sales))
rm (tPrice)

#Now some statistics from the data.
#Find the 5 most expensive and 5 cheaper products on the list
highlight.prods <- function(my.data, n = 5){
	if (n <= 0) n <- 5
	prods.val <- aggregate(my.data$Uprice, list(my.data$Prod), median, na.rm = T)
	prods <- matrix(c(as.character(prods.val[order(prods.val[,2])[1:n], 1]), 
		as.character(prods.val[order(prods.val[,2], decreasing = T)[1:n], 1])), n, 2)
	colnames(prods) <- c('Cheap', 'Expensive')
	rownames(prods) <- paste(1:n, '.', sep='')
	prods
}

highlight.prods(sales)

#Find the 5 top responsibles of the most (least) income of the company
highlight.vendors <- function(my.data, n = 5, prec = 5){
	prods.val <- aggregate(my.data$Val, list(my.data$ID), sum, na.rm = T)
	prods <- matrix(c(as.character(prods.val[order(prods.val[,2])[1:n], 1]), 
		as.character(prods.val[order(prods.val[,2], decreasing = T)[1:n], 1])), n, 2)

	#Percentage of income by the top n vendors
	cat('Top', as.character(n), 'vendors income:\t\t', 
		as.character(round(100 * (sum(prods.val[order(prods.val[,2], decreasing = T)[1:n], 2])/
			sum(my.data$Val, na.rm = T)), prec)), '\n', sep = ' ')

	# ""	""		""			least n vendors
	cat('Bottom', as.character(n), 'vendors income:\t\t', 
		as.character(round(100 * (sum(prods.val[order(prods.val[,2], decreasing = F)[1:n], 2])/
			sum(my.data$Val, na.rm = T)), prec)), '\n', sep = ' ')

	colnames(prods) <- c('Least', 'Most')
	rownames(prods) <- paste(1:n, '.', sep='')
	prods
}

highlight.vendors(sales, 100)
#top 100: ~38.3%, bottom 100: < 0.1%

percentage.sold <- function(my.data, n = 5, prec = 5){
	prods.val <- aggregate(my.data$Quant, list(my.data$Prod), sum, na.rm = T)
	#Percentage of income by the top n vendors
	cat('Top', as.character(n), 'vendors sold products:', 
		as.character(round(sum(as.double(prods.val[order(prods.val$x,decreasing=T)[1:n],2]))/
			sum(as.double(my.data$Quant),na.rm=T)*100, prec)), '\n', sep = ' ')

	# ""	""		""			least n vendors
	cat('Bottom', as.character(n), 'vendors sold products:', 
		as.character(round(sum(as.double(prods.val[order(prods.val$x,decreasing=F)[1:n],2]))/
			sum(as.double(my.data$Quant),na.rm=T)*100, prec)), '\n', sep = ' ')
}

percentage.sold(sales, 100)
# Top 100 vendors sold products: 74.63478 
# Bottom 100 vendors sold products: 0.00373 

######