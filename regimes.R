library(dplyr, exclude = c("filter","lag"))
library(tidyr)
library(zoo)
library(seasonal)

zip_files <- as.list(list.files(path = "DATA", 
                        pattern = "\\.zip$", 
                        full.names = TRUE)
                     )

file_names = list()

for(i in 1:length(zip_files)) {
    file_names[i] = unzip(zip_files[[i]], list = TRUE)$Name
}

lapply(zip_files, unzip, exdir = "DATA")


### CPI Industrial Workers (levels)
cpi_iw <- read.csv(paste0("DATA/",file_names[[1]]),
                   header = FALSE, sep = "|")
cpi_iw <- cpi_iw[-c(1:4),c(1:3)]
colnames(cpi_iw) <- c("Time", "Freq", "Value")
cpi_iw$Time <- as.Date(cpi_iw$Time, format = "%Y%m%d")
cpi_iw$Value <- as.numeric(cpi_iw$Value)
cpi_iw <- cpi_iw[cpi_iw$Freq=="M",]
cpi_iw <- cpi_iw[,-2]
cpi_iw <- read.zoo(cpi_iw)

### Real GDP - Base 2011-12 
gdp_raw <- read.csv(paste0("DATA/",file_names[[2]]),
                   header = FALSE, sep = "|")
gdp_raw <- gdp_raw[-c(1:3),c(1:3)]
colnames(gdp_raw) <- c("Time", "Freq", "Value")
gdp_raw$Time <- as.Date(gdp_raw$Time, format = "%Y%m%d")
gdp_raw$Value <- as.numeric(gdp_raw$Value)
gdp_raw <- gdp_raw[gdp_raw$Freq=="Q",-2]
gdp_raw <- read.zoo(na.omit(gdp_raw))
index(gdp_raw) <- as.yearqtr(index(gdp_raw)) - 3/12

### 3 month (~91-day) T-Bill Yields
tbill <- read.csv(paste0("DATA/",file_names[[3]]),
                   header = FALSE, sep = "|")
tbill <- tbill[-c(1:3),c(1,2,4)]
colnames(tbill) <- c("Time", "Freq", "Value")
tbill$Time <- as.Date(tbill$Time, format = "%Y%m%d")
tbill$Value <- as.numeric(tbill$Value)
tbill <- tbill[,-2]
tbill <- read.zoo(na.omit(tbill))
index(tbill) <- as.yearqtr(index(tbill)) - 3/12
############################################################

## 1 - Seasonally adjust GDP data

gdp_raw_ts <- ts(coredata(gdp_raw), start = c(1996, 1), frequency = 4)
s_adjust <- seas(gdp_raw_ts)
gdp_sa <- final(s_adjust)

### Tbill -Not Needed
### CPI-IW - Monthly to Quarterly

cpi_iw_q <- aggregate(cpi_iw, as.yearqtr, mean)
index(cpi_iw_q) <- index(cpi_iw_q) 

### 2 - YoY growth rates

## GDP
gdp_sa_g <- as.zoo(((gdp_sa - lag(gdp_sa,-4)) / lag(gdp_sa,-4)) * 100)

## CPI
cpi_iw_q_g <- ((cpi_iw_q - lag(cpi_iw_q,-4)) / lag(cpi_iw_q,-4)) * 100

### 3 - Rolling/Smoothing
## Not doing this just yet

### -> Single object dataframe

dat <- na.omit(cbind(gdp_sa_g, cpi_iw_q_g, tbill))

### 4 z-scores
scale_dat <- as.data.frame(scale(dat))

scale_dat_nCovid <- scale_dat[!(rownames(scale_dat) %in% c("2020 Q1","2021 Q1","2021 Q2")),]

### 5 PCA (optional)
## Not doing this since we only have three factors

### 6 K-means

library(factoextra)

## Witn COVID
fviz_nbclust(scale_dat, kmeans, method = "wss")
fviz_nbclust(scale_dat, kmeans, method = "silhouette")
fviz_nbclust(scale_dat, kmeans, method = "gap_stat")

km_res <- kmeans(scale_dat, centers = 4, nstart = 25)

## Without COVID outliers
fviz_nbclust(scale_dat_nCovid, kmeans, method = "wss")
fviz_nbclust(scale_dat_nCovid, kmeans, method = "silhouette")
fviz_nbclust(scale_dat_nCovid, kmeans, method = "gap_stat")

km_res2 <- kmeans(scale_dat_nCovid, centers = 3, nstart = 25)

## Map it to the original dataset
scale_dat$Cluster <- as.factor(km_res$cluster)
summary(scale_dat$Cluster)

scale_dat_nCovid$Cluster <- as.factor(km_res2$cluster)
summary(scale_dat_nCovid$Cluster)

## Seeing what these clusters look like 

### Non-scaled data w/o COVID
dat_nCovid <- as.data.frame(dat[!(rownames(scale_dat) %in% c("2020 Q1","2021 Q1","2021 Q2")),])
dat_nCovid$Cluster <- as.factor(km_res2$cluster)

dat_nCovid |>
    group_by(Cluster) |>
    summarise(across(c(gdp_sa_g, cpi_iw_q_g, tbill), mean))

dat_nCovid

### Some more specifications

## 4 clusters

scale_dat_nCovid$Cluster <- NULL

km_res3 <- kmeans(scale_dat_nCovid, centers = 4, nstart = 25)
dat_nCovid2 <- dat_nCovid

dat_nCovid2$Cluster <- as.factor(km_res3$cluster)

dat_nCovid2 |>
    group_by(Cluster) |>
    summarise(across(c(gdp_sa_g, cpi_iw_q_g, tbill), mean))


## Moving to GMM
## Using the non-COVID version of our dataset
library(mclust)

scale_dat_nCovid$Cluster <- NULL

fit_all <- Mclust(scale_dat_nCovid, G = 2:4)

summary(fit_all)

plot(fit_all, what = "BIC")

dat_nCovid$gmm_cluster <- fit_all$classification
dat_nCovid$gmm_prob <- apply(fit_all$z, 1, max)

aggregate(dat_nCovid[, c("gdp_sa_g", "cpi_iw_q_g", "tbill")],
          by = list(cluster = dat_nCovid$gmm_cluster),
          mean)

table(dat_nCovid$gmm_cluster)
head(round(fit_all$z, 3))

#### 
library(mclust)

X <-as.data.frame( scale(dat_nCovid[, c("gdp_sa_g", "cpi_iw_q_g", "tbill")]))

bic_grid <- mclustBIC(X, G = 2:4)
plot(bic_grid)

fit_best <- Mclust(X, x = bic_grid)
summary(fit_best)

dat_nCovid$gmm_cluster <- fit_best$classification

aggregate(dat_nCovid[, c("gdp_sa_g", "cpi_iw_q_g", "tbill")],
          by = list(cluster = dat_nCovid$gmm_cluster),
          mean)



###
fit3 <- Mclust(X, G = 3)
summary(fit3)
fit3$parameters$pro
fit3$parameters$mean
table(fit3$classification)

X$GMM_cluster <- fit3$classification


#########################################
#HMM
#########################################

library(depmixS4)

set.seed(123)

hmm_data_sc <- as.data.frame(scale(dat_nCovid[, c("gdp_sa_g", "cpi_iw_q_g", "tbill")]))

mod2 <- depmix(list(gdp_sa_g ~ 1, cpi_iw_q_g ~ 1, tbill ~ 1),
               data = hmm_data_sc, nstates = 2,
               family = list(gaussian(), gaussian(), gaussian()))
fit2 <- fit(mod2)

mod3 <- depmix(list(gdp_sa_g ~ 1, cpi_iw_q_g ~ 1, tbill ~ 1),
               data = hmm_data_sc, nstates = 3,
               family = list(gaussian(), gaussian(), gaussian()))
fit3 <- fit(mod3)

BIC(fit2); BIC(fit3)

post3 <- posterior(fit3)
dat_nCovid$hmm3_state <- post3$state

aggregate(dat_nCovid[, c("gdp_sa_g", "cpi_iw_q_g", "tbill")],
          by = list(state = dat_nCovid$hmm3_state),
          mean)

table(dat_nCovid$hmm3_state)
