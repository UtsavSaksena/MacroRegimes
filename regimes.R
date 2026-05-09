library(dplyr, exclude = c("filter","lag"))
library(quantmod)
library(tidyr)
library(xts)

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
cpi_iw <- cpi_iw[cpi_iw$Freq=="M",]
cpi_iw <- cpi_iw[,-2]

### Real GDP - Base 2011-12 
gdp_raw <- read.csv(paste0("DATA/",file_names[[2]]),
                   header = FALSE, sep = "|")
gdp_raw <- gdp_raw[-c(1:3),c(1:3)]
colnames(gdp_raw) <- c("Time", "Freq", "Value")
gdp_raw$Time <- as.Date(gdp_raw$Time, format = "%Y%m%d")
gdp_raw <- gdp_raw[gdp_raw$Freq=="Q",]
gdp_raw <- gdp_raw[,-2]

### 3 month (~91-day) T-Bill Yields
tbill <- read.csv(paste0("DATA/",file_names[[3]]),
                   header = FALSE, sep = "|")
tbill <- tbill[-c(1:3),c(1,2,4)]
colnames(tbill) <- c("Time", "Freq", "Value")
tbill$Time <- as.Date(tbill$Time, format = "%Y%m%d")
tbill <- tbill[,-2]

############################################################

## 1 seasonally adjusted data,
## 2 YoY transforms,
## 3 rolling smoothing,
## 4 z-scoring,

scale.data.g <- scale(data.g)

## 5 PCA (optional),



## 6 K-means

library(factoextra)
fviz_nbclust(scale.data.g, kmeans, method = "wss")

km.res <- kmeans(scale.data.g, centers = 4, nstart = 25)

## Map it to the original dataset
data.g$Cluster <- as.factor(km.res$cluster)

data.g |>
    group_by(Cluster) |>
    summarise(across(c(GDP, CPI, TBILL), mean))

summary(data.g$Cluster)

data.g$Cluster
