source("dataPrep.R")
library(factoextra)
library(DescTools)
library(plotly)
library(scatterplot3d)

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

###
mah <- mahalanobis(scale_dat, colMeans(scale_dat), cov(scale_dat))
plot(mah, type="l")
abline(h = qchisq(0.999, df = ncol(scale_dat)), col="red", lty=2)

# Any observation beyond the chi-sq threshold is a multivariate outlier
which(mah > qchisq(0.999, df = ncol(scale_dat)))


### K-means
set.seed(300)

## Witn COVID
fviz_nbclust(scale_dat, kmeans, method = "wss", k.max = 10)
fviz_nbclust(scale_dat, kmeans, method = "silhouette")
fviz_nbclust(scale_dat, kmeans, method = "gap_stat")

km_res <- kmeans(scale_dat, centers = 3, nstart = 50)

fviz_cluster(km_res, scale_dat) 

library(dbscan)

kNNdistplot(scale_dat, k = 5)
abline(h = 1.8, col = "red", lty = 2) 

db_result <- dbscan(scale_dat, eps = 1.8, minPts = 5) 
db_result |> summary()

hdb_result <- hdbscan(scale_dat, minPts = 5)

## Moving to GMM
library(mclust)

fit_all <- Mclust(scale_dat)
summary(fit_all)

plot(fit_all, what = "BIC")
plot(fit_all, what = "classification")

icl_gmm <- mclustICL(scale_dat)
plot(icl_gmm)

dat_viz <- as.data.frame(dat)
dat_viz$gmm_cluster <- fit_all$classification

plot_ly(dat_viz, x = ~gdp_sa_g, y = ~cpi_iw_g1_q, z = ~tbill,
        color = ~gmm_cluster,
        mode = "markers+text",
        text = rownames(dat_viz)
        ) |>
    add_markers() |>
    layout(scene = list(xaxis = list(title = 'GDP'),
                        yaxis = list(title = 'Inflation'),
                        zaxis = list(title = 'T-Bill Yield'))
           )

dat_viz$gmm_prob <- apply(fit_all$z, 1, max)

dat_viz |>
    group_by(gmm_cluster) |>
    summarise(across(c(gdp_sa_g, cpi_iw_g1_q, tbill), mean))

table(dat_viz$gmm_cluster)
head(round(fit_all$z, 3), 100)

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

scatterplot3d(x = dat_viz$gdp_sa_g,
              y = dat_viz$cpi_iw_g1_q,
              z = dat_viz$tbill, pch = 10,
              highlight.3d = TRUE)

