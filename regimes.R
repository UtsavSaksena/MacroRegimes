source("dataPrep.R")
library(factoextra)

#############################################
############  MACHINE LEARNING ##############
#############################################

### Single Dataframe

dat <- na.omit(cbind(gdp_sa_g, cpi_iw_g1_q, tbill)) |> as.data.frame()

dat$covid <- ifelse(rownames(dat) %in% c("2021 Q1","2021 Q2","2021 Q2", "2021 Q3",
                                         "2021 Q4", "2022 Q1", "2022 Q2"),1,0)

### Z-scores
scale_dat <- scale(dat)

scale_dat_nCovid <- scale_dat[!(rownames(scale_dat) %in% c("2020 Q1","2021 Q1","2021 Q2")),]

### 6 K-means
set.seed(300)

## Witn COVID
fviz_nbclust(scale_dat, kmeans, method = "wss")
fviz_nbclust(scale_dat, kmeans, method = "silhouette")
fviz_nbclust(scale_dat, kmeans, method = "gap_stat")

km_res <- kmeans(scale_dat, centers = 3, nstart = 50)

fviz_cluster(km_res, scale_dat) 


library(dbscan)

kNNdistplot(scale_dat, k = 5)
abline(h = 1.5, col = "red", lty = 2) 


db_result <- dbscan(scale_dat, eps = 1, minPts = 4) 
db_result

hdb_result <- hdbscan(scale_dat, minPts = 4)

viz_dat <- dat

viz_dat$HDBCluster <- as.factor(hdb_result$cluster)

summary(viz_dat$HDBCluster)


as.data.frame(viz_dat) |>
    group_by(HDBCluster) |>
    summarise(across(c(gdp_sa_g, cpi_iw_g1_q, tbill), mean))
















## Without COVID outliers
fviz_nbclust(scale_dat_nCovid, kmeans, method = "wss")
fviz_nbclust(scale_dat_nCovid, kmeans, method = "silhouette")
fviz_nbclust(scale_dat_nCovid, kmeans, method = "gap_stat")

km_res2 <- kmeans(scale_dat_nCovid, centers = 3, nstart = 25)

## Map it to the original dataset
viz_dat <- dat
viz_dat$Cluster <- as.factor(km_res$cluster)
summary(viz_dat$Cluster)

as.data.frame(viz_dat) |>
    group_by(Cluster) |>
    summarise(across(c(gdp_sa_g, cpi_iw_g1_q, tbill), mean))

scale_dat_nCovid$Cluster <- as.factor(km_res2$cluster)
summary(scale_dat_nCovid$Cluster)

## Seeing what these clusters look like 

### Non-scaled data w/o COVID
dat_nCovid <- as.data.frame(dat[!(rownames(scale_dat) %in% c("2020 Q1","2021 Q1","2021 Q2")),])
dat_nCovid$Cluster <- as.factor(km_res2$cluster)

dat_nCovid |>
    group_by(Cluster) |>
    summarise(across(c(gdp_sa_g, cpi_iw_q_g, tbill), mean))

### Some more specifications

#### 4 clusters

km_res4clus <- kmeans(scale_dat, centers = 4, nstart = 25)
viz_dat <- as.data.frame(dat)
viz_dat$Cluster <- as.factor(km_res4clus$cluster)

viz_dat |>
    group_by(Cluster) |>
    summarise(across(c(gdp_sa_g, cpi_iw_g1_q, tbill), mean))
summary(viz_dat$Cluster)

#### Non Covid
scale_dat_nCovid$Cluster <- NULL

km_res3 <- kmeans(scale_dat_nCovid, centers = 4, nstart = 25)
dat_nCovid2 <- dat_nCovid

dat_nCovid2$Cluster <- as.factor(km_res3$cluster)

dat_nCovid2 |>
    group_by(Cluster) |>
    summarise(across(c(gdp_sa_g, cpi_iw_q_g, tbill), mean))


## 7 Moving to GMM
library(mclust)

fit_all <- Mclust(scale_dat)
summary(fit_all)

plot(fit_all, what = "BIC")

dat_viz <- as.data.frame(dat)
dat_viz$gmm_cluster <- fit_all$classification
dat_viz$gmm_prob <- apply(fit_all$z, 1, max)

dat_viz |>
    group_by(gmm_cluster) |>
    summarise(across(c(gdp_sa_g, cpi_iw_g1_q, tbill), mean))

table(dat_$gmm_cluster)
head(round(fit_all$z, 3))

#### 
## library(mclust)

## X <- as.data.frame( scale(dat_nCovid[, c("gdp_sa_g", "cpi_iw_q_g", "tbill")]))

## bic_grid <- mclustBIC(X, G = 2:4)
## plot(bic_grid)

## fit_best <- Mclust(X, x = bic_grid)
## summary(fit_best)

## dat_nCovid$gmm_cluster <- fit_best$classification

## aggregate(dat_nCovid[, c("gdp_sa_g", "cpi_iw_q_g", "tbill")],
##           by = list(cluster = dat_nCovid$gmm_cluster),
##           mean)



## ###
## fit3 <- Mclust(X, G = 3)
## summary(fit3)
## fit3$parameters$pro
## fit3$parameters$mean
## table(fit3$classification)

## X$GMM_cluster <- fit3$classification


## HMM
library(depmixS4)

hmm_data <- as.data.frame(scale(dat))

mod2 <- depmix(list(na.omit(gdp_sa_g) ~ 1, cpi_iw_q_g ~ 1, tbill ~ 1),
               data = hmm_data, nstates = 3,
               family = list(gaussian(), gaussian(), gaussian())
               )

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
