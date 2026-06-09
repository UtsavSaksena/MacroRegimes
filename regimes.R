source("dataPrep.R")
library(factoextra)
library(DescTools)

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

### K-means
set.seed(300)

## Witn COVID
fviz_nbclust(scale_dat, kmeans, method = "wss")
fviz_nbclust(scale_dat, kmeans, method = "silhouette")
fviz_nbclust(scale_dat, kmeans, method = "gap_stat")

km_res <- kmeans(scale_dat, centers = 5, nstart = 50)

fviz_cluster(km_res, scale_dat) 

library(dbscan)

kNNdistplot(scale_dat, k = 5)
abline(h = 1.5, col = "red", lty = 2) 

db_result <- dbscan(scale_dat, eps = 1.5, minPts = 5) 
db_result |> summary()

hdb_result <- hdbscan(scale_dat, minPts = 5)

viz_dat <- dat

viz_dat$HDBCluster <- as.factor(hdb_result$cluster)

summary(viz_dat$HDBCluster)


as.data.frame(viz_dat) |>
    group_by(HDBCluster) |>
    summarise(across(c(gdp_sa_g, cpi_iw_g1_q, tbill), mean))

## Moving to GMM
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

table(dat_viz$gmm_cluster)
head(round(fit_all$z, 3), 100)

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

hmm_data <- as.data.frame(scale(dat))[1:114,]

mod2 <- depmix(list(hmm_data$gdp_sa_g ~ 1, hmm_data$cpi_iw_g1_q ~ 1,
                    hmm_data$tbill ~ 1),
               data = hmm_data, nstates = 4,
               family = list(gaussian(), gaussian(), gaussian())
               )

fit2 <- fit(mod2)
BIC(fit2)

hmm <- posterior(fit2)

viz_dat$hmm <- as.factor(hmm$state)

summary(viz_dat$hmm)

viz_dat |>
    group_by(hmm) |>
    summarise(across(c(gdp_sa_g, cpi_iw_g1_q, tbill), mean))



###########################################################################

library(plotly)
library(scatterplot3d)

fig <- plot_ly(viz_dat, x = ~gdp_sa_g, y = ~cpi_iw_g1_q, z = ~tbill,
               color = ~Cluster) |>
    add_markers() |>
    layout(scene = list(xaxis = list(title = 'GDP'),
                     yaxis = list(title = 'Inflation'),
                     zaxis = list(title = 'T-Bill Yield'))
           )


scatterplot3d(x = viz_dat$gdp_sa_g,
              y = viz_dat$cpi_iw_g1_q,
              z = viz_dat$tbill, pch = 10,
              highlight.3d = TRUE)

