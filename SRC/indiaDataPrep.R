library(dplyr, exclude = c("filter","lag"))
library(tidyr)
library(zoo)
library(seasonal)
library(urca)
library(strucchange)


## Function to exctract .txt files and rename them
extract_rename <- function () {

## List of all CMIE files downloaded (.zip extention)
zip_files <- as.list(list.files(path = "DATA", 
                        pattern = "\\.zip$", 
                        full.names = TRUE)
                     )
## Extract .txt files from the .zip and rename to name of .zip file (for identification)
file_names = list()
for(i in 1:length(zip_files)) {
    unzip(zip_files[[i]], exdir = "DATA")
    old_name = unzip(zip_files[[i]], list = TRUE)$Name
    new_name <- gsub(".zip", ".txt", zip_files[[i]])
    file.rename(from = paste0("DATA/", old_name),
                to = new_name)
    }

## List all .txt files
txt_files <- as.list(list.files(path = "DATA",
                                pattern = "\\.txt",
                                full.names = TRUE)
                     )
    return(txt_files)
}

yoy_growth <- function(x) {
    y <- ((x - lag(x, -12)) / lag(x, -12)) * 100
    return(y)
}
                      
##############################################################

txt_files_list <- extract_rename()

### CPI Industrial Workers (levels)
cpi_iw <- read.csv(txt_files_list[[1]], header = FALSE, sep = "|")
cpi_iw <- cpi_iw[-c(1:4),c(1:3)]
colnames(cpi_iw) <- c("Time", "Freq", "Value")
cpi_iw$Time <- as.Date(cpi_iw$Time, format = "%Y%m%d")
cpi_iw$Value <- as.numeric(cpi_iw$Value)
cpi_iw <- cpi_iw[cpi_iw$Freq=="M",]
cpi_iw <- cpi_iw[,-2]
cpi_iw <- read.zoo(cpi_iw)
cpi_iw <- window(cpi_iw, start = as.Date("1996-04-30")) ##FY 1996

### Real GDP - Base 2011-12 
gdp_raw <- read.csv(txt_files_list[[2]], header = FALSE, sep = "|")
gdp_raw <- gdp_raw[-c(1:3),c(1:3)]
colnames(gdp_raw) <- c("Time", "Freq", "Value")
gdp_raw$Time <- as.Date(gdp_raw$Time, format = "%Y%m%d")
gdp_raw$Value <- as.numeric(gdp_raw$Value)
gdp_raw <- gdp_raw[gdp_raw$Freq=="Q",-2]
gdp_raw <- read.zoo(na.omit(gdp_raw))
index(gdp_raw) <- as.yearqtr(index(gdp_raw)) - 3/12 + 1
gdp_raw <- window(gdp_raw, start = as.yearqtr("1997 Q1"))

### 3 month (~91-day) T-Bill Yields
tbill <- read.csv(txt_files_list[[3]], header = FALSE, sep = "|")
tbill <- tbill[-c(1:3),c(1,2,4)]
colnames(tbill) <- c("Time", "Freq", "Value")
tbill$Time <- as.Date(tbill$Time, format = "%Y%m%d")
tbill$Value <- as.numeric(tbill$Value)
tbill <- tbill[,-2]
tbill <- read.zoo(na.omit(tbill))
index(tbill) <- as.yearqtr(index(tbill)) - 3/12 + 1
tbill <- window(tbill, start = as.yearqtr("1998 Q1"))

##############################################################

## Transformations

### Seasonally Adjust GDP data

gdp_raw_ts <- ts(coredata(gdp_raw), start = c(1997, 1), frequency = 4)
s_adjust <- seas(gdp_raw_ts)
gdp_sa <- final(s_adjust)
gdp_sa <- as.zoo(gdp_sa)

#### TBill - Not Needed
#### CPI-IW - Monthly to Quarterly

cpi_iw_q <- aggregate(cpi_iw, as.yearqtr, mean)
index(cpi_iw_q) <- index(cpi_iw_q) - 3/12 + 1

### 2 - YoY growth rates

## GDP - Y-o-Y change
gdp_sa_g <- ((gdp_sa - lag(gdp_sa,-4)) / lag(gdp_sa,-4)) * 100

## CPI - quarterly indexed, Y-o-Y change 
cpi_iw_q_g <- ((cpi_iw_q - lag(cpi_iw_q,-4)) / lag(cpi_iw_q,-4)) * 100

### Alternative specification - Y-o-Y inflation averaged
cpi_iw_g1 <- ((cpi_iw - lag(cpi_iw, -12)) / lag(cpi_iw, -12)) * 100 
cpi_iw_g1_q <- aggregate(cpi_iw_g1, as.yearqtr, mean)
index(cpi_iw_g1_q) <- index(cpi_iw_g1_q) - 3/12 + 1

## T-Bill 
# No SA for now 

################################################################
## Econometric Tests

## Test for stationarity

########################## Real GDP, SA #############################
ur.df(gdp_sa, type = "trend", selectlags = "AIC") |> summary() #ph3<cv, so ur + tt

## Real GDP SA, growth rates
ur.df(gdp_sa_g, type = "trend", selectlags = "AIC") |> summary() #ph3>cv, so no ur + tt
ur.kpss(gdp_sa_g, type = "tau", lags = "short") |> summary() #ts<cv; fail to reject, i.e. stationary

######################### CPI Inflation Index ########################
## Levels
ur.df(cpi_iw, type = "trend", selectlags = "AIC") |> summary() #phi3<cv, so ur + tt

## Quarterly Growth
ur.df(cpi_iw_g1_q, type = "trend", selectlags = "AIC") |> summary() #phi3<cv, so ur + tt
ur.kpss(cpi_iw_g1_q, type = "tau", lags = "short") |> summary() #ts>cv, reject null, non-stationary

## Trend + a stochastic unit root

## Quarterly Growth, diff 
ur.df(diff(cpi_iw_q_g), type = "trend", selectlags = "AIC") |> summary() # phi3>cv, stationary
ur.kpss(diff(cpi_iw_q_g), type = "tau", lags = "short") |> summary() #ts<cv, stationary

###################### 3 Month Treasury Bill Yields ####################
## Levels
ur.df(tbill, type = "trend", selectlags = "AIC") |> summary() #ph3<cv, ur + tt
ur.kpss(tbill, type = "tau", lags = "short") |> summary() #ts<cv, don't reject null; stationary

## This means that the series does not have a true stochastic root but has a predictable trend.

ur.ers(tbill, type = "DF-GLS", model = "trend") |> summary() ##ts<cv; stationary


## Test for structural breaks

## BP Test
# GDP
bp_gdp <- breakpoints(gdp_sa ~ 1)
index(gdp_sa)[c(29,46,63,80,101)]
confint(bp_gdp)
summary(bp_gdp)


# GDP Growth 
bp_gdp_g <- breakpoints(gdp_sa_g ~ 1)
index(gdp_sa)[c(79,96)]
confint(bp_gdp_g)
summary(bp_gdp_g)


#CPI IW YoY change
bp_cpi_g <- breakpoints(cpi_iw_g1_q ~ 1)
index(cpi_iw_g1_q)[c(45,67)]
confint(bp_cpi_g)
summary(bp_cpi_g)

## CPI IW YoY change diff
bp_cpi_g_d <- breakpoints(diff(cpi_iw_g1_q) ~ 1) 
try(confint(bp_cpi_g_d))
summary(bp_cpi_g_d) |> try()


##TBill
bp_tbill <- breakpoints(tbill ~ 1)
index(tbill)[c(17,56,76)]
confint(bp_tbill)
summary(bp_tbill)



