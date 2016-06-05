# GraphsPlots
Rajeev  
June 4, 2016  


```r
require(gdata)
```

```
## Loading required package: gdata
```

```
## gdata: read.xls support for 'XLS' (Excel 97-2004) files ENABLED.
```

```
## 
```

```
## gdata: read.xls support for 'XLSX' (Excel 2007+) files ENABLED.
```

```
## 
## Attaching package: 'gdata'
```

```
## The following object is masked from 'package:stats':
## 
##     nobs
```

```
## The following object is masked from 'package:utils':
## 
##     object.size
```

```
## The following object is masked from 'package:base':
## 
##     startsWith
```

```r
require(plyr) #Added by Monnie McGee
```

```
## Loading required package: plyr
```

```r
#install the gdata and plyr packages and load in to R.
library(plyr)
library(gdata)
```


```r
setwd("../Paper")
#setwd("/Users/rajeevkumar/GitHub/LiveSessionHomeWork4Bronx/Data")
## You need a perl interpreter to do this on Windows.
## It's automatic in Mac
bk <- read.xls("../Data/rollingsales_bronx.xls",pattern="BOROUGH")
# So, save the file as a csv and use read.csv instead
# bk <- read.csv("rollingsales_bronx.csv",skip=4,header=TRUE)
```





```r
## clean/format the data with regular expressions
# We create a new variable that is a "clean' version of sale.price.
bk$SALE.PRICE.N <- as.numeric(gsub("[^[:digit:]]","", bk$SALE.PRICE))
count(is.na(bk$SALE.PRICE.N))
```

```
##       x freq
## 1 FALSE 6508
```


```r
names(bk) <- tolower(names(bk)) # make all variable names lower case
## Get rid of leading digits
bk$gross.sqft <- as.numeric(gsub("[^[:digit:]]","", bk$gross.square.feet))
bk$land.sqft <- as.numeric(gsub("[^[:digit:]]","", bk$land.square.feet))
bk$year.built <- as.numeric(as.character(bk$year.built))
```


```r
## keep only the actual sales
bk.sale <- bk[bk$sale.price.n!=0,]
plot(bk.sale$gross.sqft,bk.sale$sale.price.n)
```

![](GraphsPlots_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
plot(log10(bk.sale$gross.sqft),log10(bk.sale$sale.price.n))
```

![](GraphsPlots_files/figure-html/unnamed-chunk-5-2.png)<!-- -->


```r
## for now, let's look at 1-, 2-, and 3-family homes
bk.homes <- bk.sale[which(grepl("FAMILY",bk.sale$building.class.category)),]
dim(bk.homes)
```

```
## [1] 2589   24
```

```r
plot(log10(bk.homes$gross.sqft),log10(bk.homes$sale.price.n))
```

![](GraphsPlots_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
summary(bk.homes[which(bk.homes$sale.price.n<100000),])
```

```
##     borough                     neighborhood
##  Min.   :2   BAYCHESTER               :14   
##  1st Qu.:2   MORRISANIA/LONGWOOD      :12   
##  Median :2   SOUNDVIEW                :12   
##  Mean   :2   WAKEFIELD                : 8   
##  3rd Qu.:2   BRONXDALE                : 7   
##  Max.   :2   THROGS NECK              : 7   
##              (Other)                  :68   
##                                  building.class.category
##  02  TWO FAMILY DWELLINGS                    :62        
##  01  ONE FAMILY DWELLINGS                    :42        
##  03  THREE FAMILY DWELLINGS                  :24        
##  04  TAX CLASS 1 CONDOS                      : 0        
##  05  TAX CLASS 1 VACANT LAND                 : 0        
##  06  TAX CLASS 1 - OTHER                     : 0        
##  (Other)                                     : 0        
##  tax.class.at.present     block           lot          ease.ment     
##  1      :128          Min.   :2278   Min.   :   1.00   Mode:logical  
##  1A     :  0          1st Qu.:3226   1st Qu.:  20.75   NA's:128      
##  1B     :  0          Median :4072   Median :  46.00                 
##  1D     :  0          Mean   :4113   Mean   :  92.44                 
##  2      :  0          3rd Qu.:4980   3rd Qu.:  82.50                 
##  2A     :  0          Max.   :5870   Max.   :2294.00                 
##  (Other):  0                                                         
##  building.class.at.present                                      address   
##  B1     :30                1281 UNION AVE                           :  4  
##  C0     :24                1774 CROSS BRONX EXPRESSW                :  2  
##  A1     :19                3230 JOHNSON AVENUE                      :  2  
##  B2     :14                4465 PARK AVE                            :  2  
##  B3     :13                1010 QUINCY AVENUE                       :  1  
##  A5     : 8                1041 EAST 223 STREET                     :  1  
##  (Other):20                (Other)                                  :116  
##      apartment.number    zip.code     residential.units commercial.units 
##              :128     Min.   :10453   Min.   :1.000     Min.   :0.00000  
##  1           :  0     1st Qu.:10459   1st Qu.:1.000     1st Qu.:0.00000  
##  1-Jan       :  0     Median :10465   Median :2.000     Median :0.00000  
##  1-Mar       :  0     Mean   :10464   Mean   :1.859     Mean   :0.03125  
##  1-Nov       :  0     3rd Qu.:10469   3rd Qu.:2.000     3rd Qu.:0.00000  
##  1-Sep       :  0     Max.   :10475   Max.   :3.000     Max.   :1.00000  
##  (Other)     :  0                                                        
##   total.units    land.square.feet gross.square.feet   year.built  
##  Min.   :1.000   Min.   :  297    Min.   : 458      Min.   :1800  
##  1st Qu.:1.000   1st Qu.: 1939    1st Qu.:1479      1st Qu.:1910  
##  Median :2.000   Median : 2500    Median :2150      Median :1928  
##  Mean   :1.891   Mean   : 2651    Mean   :2137      Mean   :1933  
##  3rd Qu.:2.000   3rd Qu.: 2889    3rd Qu.:2709      3rd Qu.:1950  
##  Max.   :3.000   Max.   :10690    Max.   :4320      Max.   :2012  
##                                                                   
##  tax.class.at.time.of.sale building.class.at.time.of.sale    sale.price
##  Min.   :1                 B1     :30                     $ 10    :47  
##  1st Qu.:1                 C0     :24                     $ 100   :10  
##  Median :1                 A1     :19                     $ 1     : 7  
##  Mean   :1                 B2     :14                     $ 1,000 : 5  
##  3rd Qu.:1                 B3     :13                     $ 500   : 5  
##  Max.   :1                 A5     : 8                     $ 50,000: 4  
##                            (Other):20                     (Other) :50  
##       sale.date    sale.price.n     gross.sqft     land.sqft    
##  2015-09-01:  4   Min.   :    1   Min.   : 458   Min.   :  297  
##  2015-12-11:  4   1st Qu.:   10   1st Qu.:1479   1st Qu.: 1939  
##  2015-05-08:  3   Median :  300   Median :2150   Median : 2500  
##  2015-07-22:  3   Mean   :19036   Mean   :2137   Mean   : 2651  
##  2015-10-30:  3   3rd Qu.:30000   3rd Qu.:2709   3rd Qu.: 2889  
##  2015-05-15:  2   Max.   :97000   Max.   :4320   Max.   :10690  
##  (Other)   :109
```


```r
## remove outliers that seem like they weren't actual sales
bk.homes$outliers <- (log10(bk.homes$sale.price.n) <=5) + 0
bk.homes <- bk.homes[which(bk.homes$outliers==0),]
plot(log(bk.homes$gross.sqft),log(bk.homes$sale.price.n))
```

![](GraphsPlots_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


```r
## for now, let's look at 1-, 2-, and 3-family homes at BAYCHESTER                                         
bk.homes.baychester <- bk.homes[which(grepl("BAYCHESTER",bk.homes$neighborhood)),]

plot(log(bk.homes.baychester$gross.sqft), log(bk.homes.baychester$sale.price.n))
```

![](GraphsPlots_files/figure-html/unnamed-chunk-8-1.png)<!-- -->


```r
## for now, let's look at Rental Buildings
bk.rentals <- bk.sale[which(grepl("RENTALS",bk.sale$building.class.category)),]
dim(bk.rentals)
```

```
## [1] 528  24
```

```r
plot(log10(bk.rentals$gross.sqft),log10(bk.rentals$sale.price.n))
```

![](GraphsPlots_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
summary(bk.rentals[which(bk.rentals$sale.price.n<100000),])
```

```
##     borough                     neighborhood
##  Min.   :2   MORRISANIA/LONGWOOD      :3    
##  1st Qu.:2   KINGSBRIDGE/JEROME PARK  :2    
##  Median :2   SOUNDVIEW                :2    
##  Mean   :2   HIGHBRIDGE/MORRIS HEIGHTS:1    
##  3rd Qu.:2   KINGSBRIDGE HTS/UNIV HTS :1    
##  Max.   :2   MELROSE/CONCOURSE        :1    
##              (Other)                  :4    
##                                  building.class.category
##  07  RENTALS - WALKUP APARTMENTS             :11        
##  08  RENTALS - ELEVATOR APARTMENTS           : 1        
##  11A CONDO-RENTALS                           : 1        
##  14  RENTALS - 4-10 UNIT                     : 1        
##  01  ONE FAMILY DWELLINGS                    : 0        
##  02  TWO FAMILY DWELLINGS                    : 0        
##  (Other)                                     : 0        
##  tax.class.at.present     block           lot          ease.ment     
##  2A     :9            Min.   :2296   Min.   :   8.00   Mode:logical  
##  2      :4            1st Qu.:2708   1st Qu.:  25.75   NA's:14       
##  2B     :1            Median :3242   Median :  46.50                 
##  1      :0            Mean   :3442   Mean   : 138.71                 
##  1A     :0            3rd Qu.:3870   3rd Qu.: 142.50                 
##  1B     :0            Max.   :5306   Max.   :1001.00                 
##  (Other):0                                                           
##  building.class.at.present                                      address 
##  C3     :7                 1033 EAST 232                            :1  
##  C4     :2                 1129 MORRIS AVENUE                       :1  
##  C2     :1                 1359 ROSEDALE AVE                        :1  
##  C7     :1                 1379 BRONX RIVER AVENUE                  :1  
##  D1     :1                 233 LANDING ROADE                        :1  
##  RR     :1                 2749 GIFFORD AVENUE                      :1  
##  (Other):1                 (Other)                                  :8  
##      apartment.number    zip.code     residential.units commercial.units
##              :13      Min.   :10453   Min.   : 1.00     Min.   :0.0000  
##  1           : 1      1st Qu.:10456   1st Qu.: 4.00     1st Qu.:0.0000  
##  1-Jan       : 0      Median :10464   Median : 4.00     Median :0.0000  
##  1-Mar       : 0      Mean   :10463   Mean   :14.07     Mean   :0.2857  
##  1-Nov       : 0      3rd Qu.:10468   3rd Qu.: 7.50     3rd Qu.:0.0000  
##  1-Sep       : 0      Max.   :10472   Max.   :86.00     Max.   :3.0000  
##  (Other)     : 0                                                        
##   total.units    land.square.feet gross.square.feet   year.built  
##  Min.   : 1.00   Min.   : 2000    Min.   : 2948     Min.   :1907  
##  1st Qu.: 4.00   1st Qu.: 3083    1st Qu.: 3633     1st Qu.:1925  
##  Median : 4.00   Median : 3841    Median : 5866     Median :1927  
##  Mean   :14.36   Mean   : 7547    Mean   :17244     Mean   :1945  
##  3rd Qu.: 7.50   3rd Qu.: 6880    3rd Qu.:14040     3rd Qu.:1964  
##  Max.   :89.00   Max.   :33933    Max.   :97500     Max.   :2015  
##                                                                   
##  tax.class.at.time.of.sale building.class.at.time.of.sale    sale.price
##  Min.   :2                 C3     :7                      $ 1     :5   
##  1st Qu.:2                 C4     :2                      $ 10    :3   
##  Median :2                 C2     :1                      $ 25,000:2   
##  Mean   :2                 C7     :1                      $ 20,000:1   
##  3rd Qu.:2                 D1     :1                      $ 30,000:1   
##  Max.   :2                 RR     :1                      $ 50,000:1   
##                            (Other):1                      (Other) :1   
##       sale.date  sale.price.n     gross.sqft      land.sqft    
##  2015-06-30:2   Min.   :    1   Min.   : 2948   Min.   : 2000  
##  2015-08-06:2   1st Qu.:    1   1st Qu.: 3633   1st Qu.: 3083  
##  2015-11-04:2   Median :   10   Median : 5866   Median : 3841  
##  2015-06-09:1   Mean   :10717   Mean   :17244   Mean   : 7547  
##  2015-06-29:1   3rd Qu.:23750   3rd Qu.:14040   3rd Qu.: 6880  
##  2015-07-16:1   Max.   :50000   Max.   :97500   Max.   :33933  
##  (Other)   :5
```


```r
## remove outliers that seem like they weren't actual sales
bk.rentals$outliers <- (log10(bk.rentals$sale.price.n) <=5) + 0
bk.rentals <- bk.homes[which(bk.rentals$outliers==0),]
plot(log(bk.rentals$gross.sqft),log(bk.rentals$sale.price.n))
```

![](GraphsPlots_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
