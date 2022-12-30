#ceci est un exemples sur les manipulation qu'on peut faire sur des cubes


# initialisation des tables

state_table <- 
  data.frame(key=c("CA", "NY", "WA", "ON", "QU"),
             name=c("California", "new York", "Washington", "Ontario", "Quebec"),
             country=c("USA", "USA", "USA", "Canada", "Canada"))

month_table <- 
  data.frame(key=1:12,
             desc=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
             quarter=c("Q1","Q1","Q1","Q2","Q2","Q2","Q3","Q3","Q3","Q4","Q4","Q4"))

prod_table <- 
  data.frame(key=c("Printer", "Tablet", "Laptop"),
             price=c(225, 570, 1120))

# fonction qui genere Sales table
gen_sales <- function(no_of_recs) {
  
  # Generer transaction data randomly
  loc <- sample(state_table$key, no_of_recs, 
                replace=T, prob=c(2,2,1,1,1))
  time_month <- sample(month_table$key, no_of_recs, replace=T)
  time_year <- sample(c(2012, 2013), no_of_recs, replace=T)
  prod <- sample(prod_table$key, no_of_recs, replace=T, prob=c(1, 3, 2))
  unit <- sample(c(1,2), no_of_recs, replace=T, prob=c(10, 3))
  amount <- unit*prod_table$price
  
  sales <- data.frame(month=time_month,
                      year=time_year,
                      loc=loc,
                      prod=prod,
                      unit=unit,
                      amount=amount)
  
  # Sort the records by time order (tri)
  sales <- sales[order(sales$year, sales$month),]
  row.names(sales) <- NULL
  return(sales)
}

# create the sales fact table
sales_fact <- gen_sales(500)

# exemple...
head(sales_fact)


# construire un cube
revenue_cube <- 
  tapply(sales_fact$amount, 
         sales_fact[,c("prod", "month", "year", "loc")], 
         FUN=function(x){return(sum(x))})

# montrer les cellules des cubes
revenue_cube


dimnames(revenue_cube)
 

# LE Slice consiste à fixer certaines dimensions pour analyser les dimensions restantes. Par exemple, nous pouvons nous concentrer sur les ventes qui se déroulent en "2012", "Jan", ou nous pouvons nous concentrer sur les ventes qui se déroulent en "2012", "Jan", "Tablette".
# cube data in Jan, 2012
revenue_cube[, "1", "2012",]

# cube data in Jan, 2012
revenue_cube["Tablet", "1", "2012",]
 #LE dice consiste à limiter chaque dimension à une certaine plage de valeurs, tout en gardant le même nombre de dimensions dans le cube résultant. Par exemple, nous pouvons nous concentrer sur les ventes qui se déroulent en [janvier/février/mars, ordinateur portable/tablette, CA/NY].
revenue_cube[c("Tablet","Laptop"), 
             c("1","2","3"), 
             ,
             c("CA","NY")]

#le rollup consiste à appliquer une fonction d'agrégation pour réduire un certain nombre de dimensions. Par exemple, nous voulons nous concentrer sur le chiffre d'affaires annuel de chaque produit et réduire la dimension géographique (c'est-à-dire : nous ne nous soucions pas de l'endroit où nous avons vendu notre produit).

apply(revenue_cube, c("year", "prod"),
      FUN=function(x) {return(sum(x, na.rm=TRUE))})
#le drilldown est l'inverse du "rollup" et de l'application d'une fonction d'agrégation à un niveau de granularité plus fin. Par exemple, nous voulons nous concentrer sur les revenus annuels et mensuels de chaque produit et réduire la dimension géographique (c'est-à-dire : nous ne nous soucions pas de l'endroit où nous avons vendu notre produit).
apply(revenue_cube, c("year", "month", "prod"), 
      FUN=function(x) {return(sum(x, na.rm=TRUE))})

#le pivot consiste à analyser la combinaison d'une paire de dimensions sélectionnées. Par exemple, nous voulons analyser les revenus par année et par mois. Ou nous voulons analyser les revenus par produit et par emplacement.

apply(revenue_cube, c("year", "month"), 
      FUN=function(x) {return(sum(x, na.rm=TRUE))})
install.packages("rgl") #package du 3d
library("rgl")

#visualisation
plot3d(x=sales_fact$year,y=sales_fact$unit,z=sales_fact$amount)
