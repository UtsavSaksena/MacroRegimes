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

km_res3 <- kmeans(scale_dat_nCovid, centers = 4, nstart = 25)








## This cluster perfectly maps to India's notorious overheating periods characterized by very high inflation and the RBI fighting it with high interest rates.

##     Historical Footprint: It dominates 1998–1999 (following the Asian Financial Crisis fallout) and completely monopolizes the 2010–2014 era.

##     Economic Reality: The 2010–2014 period was infamous in India for double-digit CPI (often 9% to 15% in your data), driven by massive fiscal stimulus post-2008 and supply-side constraints. The RBI was forced to hold T-bill rates aggressively high (8% to 12%) to tame it.

## Cluster 2: The Stagflationary Slowdown / Easing Regime

## This cluster represents periods of severe growth slowdowns where the RBI slashed interest rates to absolute rock-bottom levels to rescue the economy, even though inflation remained elevated.

##     Historical Footprint: It appears exclusively during global crisis periods: Q4 2008 to Q4 2009 (the immediate aftermath of the Global Financial Crisis) and 2019 to 2021 (the pre-COVID shadow banking crisis and the COVID recovery phase).

##     Economic Reality: In this regime, T-bill rates collapse to the 3%–5% range. Growth is heavily impaired (even turning negative in 2020 Q2), but inflation often remains sticky and high (8%–13% during the GFC aftermath).

## Cluster 3: The "Goldilocks" Structural Expansion

## This is the dominant regime, representing India's ideal state of structural growth. It captures periods of strong real GDP growth coupled with well-anchored, low inflation and moderate interest rates.

##     Historical Footprint: This cluster perfectly captures the famous 2003–2007 pre-GFC boom, the 2015–2018 macro-stabilization under the new RBI inflation-targeting framework, and the current robust post-COVID expansion (2022–2025).

##     Economic Reality: During these stretches, GDP routinely prints between 7% and 10%. Crucially, CPI is kept well under control (usually between 2% and 6%), allowing the RBI to maintain a neutral T-bill rate (roughly 5.5% to 7%).

## Conclusion

## Your model makes perfect historical sense. The data shows exactly what macroeconomists know about India: it spends most of its time in a high-growth expansion (Cluster 3), occasionally overheats into a high-inflation/high-rate environment (Cluster 1), and requires aggressive low-rate bailouts during global shocks (Cluster 2).

## Given that the historical mapping is this clean, what is the ultimate goal for this dataset? Are you using this to build an investment strategy, or is it for macroeconomic policy analysis?


## From GPT: 
## With COVID included, k=4k=4 produces a singleton cluster, indicating an outlier-driven partition.

## Excluding the flagged quarters yields more persistent and economically interpretable regimes.

## Therefore, the baseline analysis focuses on recurring regimes, with full-sample results treated as a robustness check
