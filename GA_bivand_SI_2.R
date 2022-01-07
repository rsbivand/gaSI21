### R code from vignette source 'GA_bivand_SI_2.Rnw'
zz <- file("script_output.Rout", open="wb")
sink(zz)
sink(zz, type = "message")

###################################################
### code chunk number 10: GA_bivand_SI_2.Rnw:128-162 (eval = FALSE)
###################################################
## BCrepos <- BiocManager::repositories()
## bioc <- available.packages(repo = BCrepos[1])
## bioc_ann <- available.packages(repo = BCrepos[2])
## bioc_exp <- available.packages(repo = BCrepos[3])
## cran <- available.packages()
## saveRDS(cran, file="cran_211124.rds")
## pdb <- rbind(cran, bioc, bioc_ann, bioc_exp)
## saveRDS(pdb, file="pdb_211124.rds")
## pdb <- readRDS("pdb_211124.rds")
## suppressPackageStartupMessages(library(miniCRAN))
## suppressPackageStartupMessages(library(igraph))
## suppressPackageStartupMessages(library(magrittr))
## pg <- makeDepGraph(pdb[, "Package"], availPkgs = pdb, suggests=TRUE, enhances=TRUE, includeBasePkgs = FALSE)
## pr <- pg %>%
##   page.rank(directed = FALSE) %>%
##   use_series("vector") %>%
##   sort(decreasing = TRUE) %>%
##   as.matrix %>%
##   set_colnames("page.rank")
## cutoff <- quantile(pr[, "page.rank"], probs = 0.1)
## popular <- pr[pr[, "page.rank"] >= cutoff, ]
## toKeep <- names(popular)
## vids <- V(pg)[toKeep]
## gs <- induced.subgraph(pg, vids = toKeep)
## cl <- walktrap.community(gs, steps = 3)
## topClusters <- table(cl$membership) %>%
##   sort(decreasing = TRUE) %>%
##   head(25)
## cluster <- function(i, clusters, pagerank, n=10){
##   group <- clusters$names[clusters$membership == i]
##   pagerank[group, ] %>% sort(decreasing = TRUE) %>% head(n)
## }
## z <- lapply(names(topClusters)[1:15], cluster, clusters=cl, pagerank=pr, n=50)
## saveRDS(z, file="all_z_211124.rds")


###################################################
### code chunk number 11: GA_bivand_SI_2.Rnw:166-180
###################################################
suppressPackageStartupMessages(library(wordcloud))
z <- readRDS("all_z_211124.rds")
oopar <- par(mfrow=c(1,2))
par(mar=c(0,0,0,0)+0.1)
sc <- c(4, 0.4)
is_sp <- which(sapply(z, function(x) "sp" %in% names(x)))
wordcloud(names(z[[is_sp]]), freq=unname(z[[is_sp]]), scale=sc) 
par(mar=c(5,6,4,2)+0.1, las=1)
cols <- rep("gray90", 19)
cols[c(4,6,7)] <- "gray60"
barplot(z[[is_sp]][1:19], horiz=TRUE, xlab="pagerank", col=cols)
par(oopar)


###################################################
### code chunk number 12: GA_bivand_SI_2.Rnw:198-201
###################################################
library(sf)
df_tracts <- st_read("df_tracts.gpkg", quiet=TRUE)
dim(df_tracts)


###################################################
### code chunk number 13: GA_bivand_SI_2.Rnw:208-210
###################################################
st_crs(df_tracts)$Name
st_crs(df_tracts)$IsGeographic


###################################################
### code chunk number 14: GA_bivand_SI_2.Rnw:218-222
###################################################
sf_use_s2(FALSE)
x <- st_area(df_tracts)
xx <- df_tracts$area - as.numeric(NISTunits::NISTsqrMeterTOacre(x))
sf_use_s2(TRUE)


###################################################
### code chunk number 15: GA_bivand_SI_2.Rnw:229-231
###################################################
library(modelsummary)
datasummary(s2 + lwgeom + percent_diff ~ P0 + P25 + Median + Mean + P75 + P100, data=data.frame(s2=df_tracts$area, lwgeom=as.numeric(NISTunits::NISTsqrMeterTOacre(x)), percent_diff=(xx*100)/df_tracts$area, check.names=FALSE), output="latex_tabular")


###################################################
### code chunk number 16: GA_bivand_SI_2.Rnw:236-237
###################################################
pop_lt_gt <- addNA(factor(ifelse(df_tracts$tot_pop < (df_tracts$tot_pop_acs - df_tracts$tot_pop_moe), 1, ifelse(df_tracts$tot_pop > (df_tracts$tot_pop_acs + df_tracts$tot_pop_moe), 3, 2)), levels=1:3, labels=c("below", "within", "above"), ordered=TRUE))


###################################################
### code chunk number 17: GA_bivand_SI_2.Rnw:244-245
###################################################
datasummary(1 ~ pop_lt_gt, data=data.frame(pop_lt_gt=pop_lt_gt, check.names=FALSE), output="latex_tabular", fmt=0L)


###################################################
### code chunk number 18: GA_bivand_SI_2.Rnw:255-261
###################################################
ESRI_cuts <- c(0, 0.12, 0.40, Inf)
ESRI_labels <- c("High", "Medium", "Low")
df_tracts$mi_cv_esri <- cut(df_tracts$med_inc_cv, ESRI_cuts, 
    labels=ESRI_labels, right=TRUE, include.lowest=TRUE, ordered_result=TRUE)
pop_cv_esri <- cut(df_tracts$tot_pop_cv, ESRI_cuts, labels=ESRI_labels, 
    right=TRUE, include.lowest=TRUE, ordered_result=TRUE)


###################################################
### code chunk number 19: GA_bivand_SI_2.Rnw:270-272
###################################################
o <- datasummary_crosstab(med_inc ~ population, 1 ~ 1 + N, data.frame(med_inc=df_tracts$mi_cv_esri, population=pop_cv_esri), output="latex_tabular")
cat(append(strsplit(o, "\\n")[[1]], c("& & \\multicolumn{4}{c}{population}\\\\", "\\cmidrule{3-6}"), 3), sep="\n")


###################################################
### code chunk number 20: GA_bivand_SI_2.Rnw:282-286
###################################################
chicago_MA <- read.table("Chicago_MA.txt", colClasses=c("character", "character"))
chicago_MA_tracts <- !is.na(match(substring(df_tracts$GEOID, 1, 5), chicago_MA$V2))
chicago_MA_sf <- df_tracts[chicago_MA_tracts,]
library(tmap)


###################################################
### code chunk number 21: GA_bivand_SI_2.Rnw:291-298
###################################################
tm_a0 <- tm_shape(chicago_MA_sf, projection=st_crs("EPSG:6455"))
tm_a1 <- tm_a0 + tm_fill("med_inc_cv", style="fisher", n=7, title="Coefficient of Variation") + tm_legend(outside=TRUE, outside.position="bottom")
tm_a2 <- tm_a0 + tm_fill("mi_cv_esri", title="Reliability") + tm_legend(outside=TRUE, outside.position="bottom")
tmap_arrange(tm_a1, tm_a2, nrow=1, ncol=2)


###################################################
### code chunk number 23: GA_bivand_SI_2.Rnw:312-320 (eval = FALSE)
###################################################
## lbls <- c("Coefficient of Variation", "Reliability")
## tm_shape(chicago_MA_sf, crs=6455) + 
## 	tm_polygons(fill=c("med_inc_cv", "mi_cv_esri"), 
## 		fill.scale=list(tm_scale_intervals(style="fisher", n=7),
## 			tm_scale_ordinal()), lwd=0, col="transparent", 
## 		fill.legend=tm_legend(title="")) + 
## 	tm_layout(panel.labels=lbls) +
## 	tm_facets(nrows=1, ncols=2)


###################################################
### code chunk number 24: GA_bivand_SI_2.Rnw:327-334
###################################################
library(mapsf)
oopar <- par(mfrow=c(1,2))
mf_choro(chicago_MA_sf, "med_inc_cv", breaks="fisher", nbreaks=7, border="transparent", lwd=0.01)
mf_typo(chicago_MA_sf, "mi_cv_esri", border="transparent", lwd=0.01)
par(oopar)


###################################################
### code chunk number 25: GA_bivand_SI_2.Rnw:343-348 (eval = FALSE)
###################################################
## library(mapsf)
## oopar <- par(mfrow=c(1,2))
## mf_choro(chicago_MA_sf, "med_inc_cv", breaks="fisher", nbreaks=7, border="transparent", lwd=0.01)
## mf_typo(chicago_MA_sf, "mi_cv_esri", border="transparent", lwd=0.01)
## par(oopar)


###################################################
### code chunk number 26: GA_bivand_SI_2.Rnw:357-358
###################################################
library(spdep)


###################################################
### code chunk number 27: GA_bivand_SI_2.Rnw:360-363 (eval = FALSE)
###################################################
(t0_s2 <- system.time(nb_subset_s2 <- poly2nb(df_tracts, queen=TRUE, row.names=df_tracts$GEOID)))
## #   user  system elapsed 
## # 32.875   0.425  33.713


###################################################
### code chunk number 28: GA_bivand_SI_2.Rnw:365-367 (eval = FALSE)
###################################################
## saveRDS(t0_s2, file="nb_subset_s2_t0.rds")
## saveRDS(nb_subset_s2, file="nb_subset_s2.rds")


###################################################
### code chunk number 29: GA_bivand_SI_2.Rnw:369-371
###################################################
#readRDS("nb_subset_s2_t0.rds")
#nb_subset_s2 <- readRDS("nb_subset_s2.rds")


###################################################
### code chunk number 30: GA_bivand_SI_2.Rnw:378-379
###################################################
sf_use_s2(FALSE)


###################################################
### code chunk number 31: GA_bivand_SI_2.Rnw:381-385 (eval = FALSE)
###################################################
(t0 <- system.time(nb_subset <- poly2nb(df_tracts, queen=TRUE, row.names=df_tracts$GEOID)))
## # although coordinates are longitude/latitude, st_intersects assumes that they are planar
## #   user  system elapsed 
## # 17.524   0.000  17.819 


###################################################
### code chunk number 32: GA_bivand_SI_2.Rnw:387-389 (eval = FALSE)
###################################################
## saveRDS(t0, file="nb_subset_t0.rds")
## saveRDS(nb_subset, file="nb_subset.rds")


###################################################
### code chunk number 33: GA_bivand_SI_2.Rnw:391-393
###################################################
#readRDS("nb_subset_t0.rds")
#nb_subset <- readRDS("nb_subset.rds")


###################################################
### code chunk number 34: GA_bivand_SI_2.Rnw:395-397
###################################################
all.equal(nb_subset, nb_subset_s2, check.attributes=FALSE)
sf_use_s2(TRUE)


###################################################
### code chunk number 35: GA_bivand_SI_2.Rnw:404-405
###################################################
nb_subset_s2


###################################################
### code chunk number 36: GA_bivand_SI_2.Rnw:412-415
###################################################
nc_nb_subset_s2 <- n.comp.nb(nb_subset_s2)
nc_nb_subset_s2$nc
table(table(nc_nb_subset_s2$comp.id))


###################################################
### code chunk number 37: GA_bivand_SI_2.Rnw:422-426
###################################################
not_0 <- card(nb_subset_s2) != 0L
nb_subset_s2_no0 <- subset(nb_subset_s2, subset=not_0)
library(rgeoda)
system.time(rgeoda_nb_subset_no0 <- queen_weights(df_tracts[which(not_0),], precision_threshold=sqrt(.Machine$double.eps)))


###################################################
### code chunk number 38: GA_bivand_SI_2.Rnw:431-432
###################################################
summary(rgeoda_nb_subset_no0)


###################################################
### code chunk number 39: GA_bivand_SI_2.Rnw:439-442 (eval = FALSE)
###################################################
rgeoda_nb <- lapply(1:nrow(df_tracts[which(not_0),]), function(i) {
    sort(as.integer(get_neighbors(rgeoda_nb_subset_no0, i)))
})


###################################################
### code chunk number 40: GA_bivand_SI_2.Rnw:444-445 (eval = FALSE)
###################################################
## saveRDS(rgeoda_nb, "rgeoda_nb.rds")


###################################################
### code chunk number 41: GA_bivand_SI_2.Rnw:447-448
###################################################
#rgeoda_nb <- readRDS("rgeoda_nb.rds")


###################################################
### code chunk number 42: GA_bivand_SI_2.Rnw:450-451
###################################################
all.equal(nb_subset_s2_no0, rgeoda_nb, check.attributes=FALSE)


###################################################
### code chunk number 43: GA_bivand_SI_2.Rnw:458-459 (eval = FALSE)
###################################################
st_write(df_tracts[which(not_0),], "df_tracts_no0.gpkg", append=FALSE)


###################################################
### code chunk number 44: GA_bivand_SI_2.Rnw:464-470
###################################################
library(reticulate)
use_python("/usr/bin/python", required = TRUE)
gp <- import("geopandas")
geodf <- gp$read_file('df_tracts_no0.gpkg')
ps <- import("libpysal")
system.time(nb_q <- ps$weights$Queen$from_dataframe(geodf))


###################################################
### code chunk number 45: GA_bivand_SI_2.Rnw:477-479 (eval = FALSE)
###################################################
nb_q_nb <- nb_q$neighbor_offsets
nb_q_nb <- lapply(nb_q_nb, function(x) sort(x + 1))


###################################################
### code chunk number 46: GA_bivand_SI_2.Rnw:481-482 (eval = FALSE)
###################################################
## saveRDS(nb_q_nb, "nb_q_nb.rds")


###################################################
### code chunk number 47: GA_bivand_SI_2.Rnw:484-485
###################################################
#nb_q_nb <- readRDS("nb_q_nb.rds")


###################################################
### code chunk number 48: GA_bivand_SI_2.Rnw:487-489
###################################################
all.equal(nb_q_nb, nb_subset_s2_no0, check.attributes=FALSE)
nb_q_diffs <- c(48566, 66941)


###################################################
### code chunk number 49: GA_bivand_SI_2.Rnw:502-505
###################################################
set.ZeroPolicyOption(TRUE)
lw <- nb2listw(nb_subset_s2, style="W")
lw_no0 <- nb2listw(nb_subset_s2_no0, style="W")


###################################################
### code chunk number 50: GA_bivand_SI_2.Rnw:516-517
###################################################
system.time(localI_med_inc_cv_cond_no0 <- localmoran(df_tracts[not_0,]$med_inc_cv, lw_no0, mlvar=FALSE))


###################################################
### code chunk number 51: GA_bivand_SI_2.Rnw:524-527
###################################################
library(parallel)
ncpus <- detectCores()-1L
set.coresOption(ncpus)


###################################################
### code chunk number 52: GA_bivand_SI_2.Rnw:529-532 (eval = FALSE)
###################################################
(t0 <- system.time(localI_med_inc_cv_perm_no0 <- localmoran_perm(df_tracts[not_0,]$med_inc_cv, lw_no0, nsim=499, iseed=1, mlvar=FALSE)))
## #    user  system elapsed 
## # 106.373   3.027  25.257 


###################################################
### code chunk number 53: GA_bivand_SI_2.Rnw:534-536 (eval = FALSE)
###################################################
## saveRDS(t0, file="perm_no0_t0.rds")
## saveRDS(localI_med_inc_cv_perm_no0, "localI_med_inc_cv_perm_no0.rds")


###################################################
### code chunk number 54: GA_bivand_SI_2.Rnw:538-540
###################################################
#readRDS("perm_no0_t0.rds")
#localI_med_inc_cv_perm_no0 <- readRDS("localI_med_inc_cv_perm_no0.rds")


###################################################
### code chunk number 55: GA_bivand_SI_2.Rnw:547-548
###################################################
system.time(localI_med_inc_cv_perm_rgeoda <- local_moran(rgeoda_nb_subset_no0, df_tracts[not_0, "med_inc_cv"], permutations=499, cpu_threads=ncpus))


###################################################
### code chunk number 56: GA_bivand_SI_2.Rnw:555-557
###################################################
a <- lisa_values(localI_med_inc_cv_perm_rgeoda)
all.equal(a, unname(localI_med_inc_cv_cond_no0[,1]))


###################################################
### code chunk number 57: GA_bivand_SI_2.Rnw:564-566
###################################################
esda <- import("esda")
system.time(localI_med_inc_cv_ps <- esda$Moran_Local(geodf["med_inc_cv"], nb_q, transformation="R", permutations=499L, n_jobs=ncpus, geoda_quads=TRUE, seed=1L))


###################################################
### code chunk number 58: GA_bivand_SI_2.Rnw:573-576
###################################################
v <- c("Is", "EIc", "VIc")
x <- sapply(v, function(x) localI_med_inc_cv_ps[[x]])[-nb_q_diffs,]
all.equal(x, localI_med_inc_cv_cond_no0[-nb_q_diffs, 1:3], check.attributes=FALSE)


###################################################
### code chunk number 59: GA_bivand_SI_2.Rnw:588-595
###################################################
spdep_perm_z <- localI_med_inc_cv_perm_no0[-nb_q_diffs,5]/2
py_perm_z <- localI_med_inc_cv_ps$p_z_sim[-nb_q_diffs]
spdep_perm_prob <- localI_med_inc_cv_perm_no0[-nb_q_diffs,6]/2
spdep_perm_rank <- localI_med_inc_cv_perm_no0[-nb_q_diffs,7]
py_perm <- localI_med_inc_cv_ps$p_sim[-nb_q_diffs]
geoda_perm <- lisa_pvalues(localI_med_inc_cv_perm_rgeoda)[-nb_q_diffs]
datasummary_correlation(data.frame("spdep Z"=spdep_perm_z, "PySAL Z"=py_perm_z, "spdep rank"=spdep_perm_rank, "PySAL rank"=py_perm, "rgeoda rank"=geoda_perm, check.names=FALSE), output="latex_tabular", fmt=4)


###################################################
### code chunk number 60: GA_bivand_SI_2.Rnw:608-616
###################################################
labs <- lisa_labels(localI_med_inc_cv_perm_rgeoda)[1:5]
g0 <- lisa_clusters(localI_med_inc_cv_perm_rgeoda)[-nb_q_diffs]
is.na(g0) <- g0 == 0L
g <- addNA(factor(g0, levels=c(3,5,4,2)-1, labels=labs[c(3,5,4,2)]))
p0 <- ifelse(localI_med_inc_cv_ps$p_sim[-nb_q_diffs] <= 0.05, localI_med_inc_cv_ps$q[-nb_q_diffs], as.integer(NA))
p <- addNA(factor(p0, levels=c(3,5,4,2)-1, labels=labs[c(3,5,4,2)]))
o <-datasummary_crosstab(rgeoda ~ pysal, 1 ~ 1 + N, data.frame(rgeoda=g, pysal=p), output="latex_tabular")
cat(append(strsplit(o, "\\n")[[1]], c("& & \\multicolumn{6}{c}{PySAL}\\\\", "\\cmidrule{3-8}"), 3), sep="\n")


###################################################
### code chunk number 61: GA_bivand_SI_2.Rnw:627-635
###################################################
quadrs <- attr(localI_med_inc_cv_perm_no0, "quadr")
a <- quadrs$mean
a[localI_med_inc_cv_perm_no0[, 6] > 0.05] <- as.integer(NA)
g0 <- lisa_clusters(localI_med_inc_cv_perm_rgeoda)
is.na(g0) <- g0 == 0L
g <- addNA(factor(g0, levels=c(3,5,4,2)-1, labels=labs[c(3,5,4,2)]))
o <- datasummary_crosstab(rgeoda ~ spdep, 1 ~ 1 + N, data.frame(rgeoda=g, spdep=addNA(a)), output="latex_tabular")
cat(append(strsplit(o, "\\n")[[1]], c("& & \\multicolumn{6}{c}{spdep}\\\\", "\\cmidrule{3-8}"), 3), sep="\n")


###################################################
### code chunk number 62: GA_bivand_SI_2.Rnw:651-657
###################################################
a_2 <- quadrs$mean[-nb_q_diffs]
a_2[localI_med_inc_cv_perm_no0[-nb_q_diffs, 6] > 0.05] <- as.integer(NA)
p0a <- ifelse(localI_med_inc_cv_ps$p_sim[-nb_q_diffs] <= 0.025, localI_med_inc_cv_ps$q[-nb_q_diffs], as.integer(NA))
pa <- addNA(factor(p0a, levels=c(3,5,4,2)-1, labels=labs[c(3,5,4,2)]))
o <- datasummary_crosstab(spdep ~ pysal, 1 ~ 1 + N, data.frame(spdep=addNA(a_2), pysal=pa), output="latex_tabular")
cat(append(strsplit(o, "\\n")[[1]], c("& & \\multicolumn{6}{c}{PySAL}\\\\", "\\cmidrule{3-8}"), 3), sep="\n")


###################################################
### code chunk number 63: GA_bivand_SI_2.Rnw:665-666
###################################################
localI_med_inc_cv_perm_rgeoda_025 <- local_moran(rgeoda_nb_subset_no0, df_tracts[not_0, "med_inc_cv"], permutations=499, significance_cutoff=0.025, cpu_threads=ncpus)


###################################################
### code chunk number 64: GA_bivand_SI_2.Rnw:676-683
###################################################
a <- quadrs$mean
a[localI_med_inc_cv_perm_no0[, 6] > 0.05] <- as.integer(NA)
g025 <- lisa_clusters(localI_med_inc_cv_perm_rgeoda_025)
is.na(g025) <- g025 == 0L
g_025 <- addNA(factor(g025, levels=c(3,5,4,2)-1, labels=labs[c(3,5,4,2)]))
o <- datasummary_crosstab(rgeoda ~ spdep, 1 ~ 1 + N, data.frame(rgeoda=g_025, spdep=addNA(a)), output="latex_tabular")
cat(append(strsplit(o, "\\n")[[1]], c("& & \\multicolumn{6}{c}{spdep}\\\\", "\\cmidrule{3-8}"), 3), sep="\n")


###################################################
### code chunk number 65: GA_bivand_SI_2.Rnw:697-704
###################################################
pysal <- table(factor(localI_med_inc_cv_ps$q, levels=c(2,4,3,1), labels=labs[c(3,5,4,2)]))
zero <- table(quadrs$pysal)
xmean <- table(quadrs$mean)
xmedian <- table(quadrs$median)
df_split <- data.frame(quadrant=levels(quadrs$pysal), PySAL=c(pysal), zero=c(zero), mean=c(xmean), median=c(xmedian))
o <- datasummary_df(df_split, output="latex_tabular", fmt=0)
cat(append(strsplit(o, "\\n")[[1]], c("& \\multicolumn{4}{c}{splitting rule}\\\\", "\\cmidrule{2-5}"), 3), sep="\n")


###################################################
### code chunk number 66: GA_bivand_SI_2.Rnw:717-722
###################################################
a <- b <- quadrs$mean
a[localI_med_inc_cv_perm_no0[, 6] > 0.05] <- as.integer(NA)
b[localI_med_inc_cv_perm_no0[,5] > 0.05] <- as.integer(NA)
o <-datasummary_crosstab(rank ~ Z, 1 ~ 1 + N, data.frame(rank=addNA(a), Z=addNA(b)), output="latex_tabular")
cat(append(strsplit(o, "\\n")[[1]], c("& & \\multicolumn{6}{c}{standard deviate}\\\\", "\\cmidrule{3-8}"), 3), sep="\n")


###################################################
### code chunk number 67: GA_bivand_SI_2.Rnw:731-732
###################################################
datasummary_df(data.frame(statistic=c("Skewness", "Kurtosis"), untransformed=c(e1071::skewness(df_tracts[not_0,]$med_inc_cv), e1071::kurtosis(df_tracts[not_0,]$med_inc_cv)), "log transformed"=c(e1071::skewness(log(df_tracts[not_0,]$med_inc_cv)), e1071::kurtosis(log(df_tracts[not_0,]$med_inc_cv))), check.names=FALSE), output="latex_tabular")


###################################################
### code chunk number 68: GA_bivand_SI_2.Rnw:744-745 (eval = FALSE)
###################################################
log_localI_med_inc_cv_no0 <- localmoran_perm(log(df_tracts[not_0,]$med_inc_cv), lw_no0, nsim=499, iseed=1, mlvar=FALSE)


###################################################
### code chunk number 69: GA_bivand_SI_2.Rnw:747-748 (eval = FALSE)
###################################################
## saveRDS(log_localI_med_inc_cv_no0, "log_localI_med_inc_cv_no0.rds")


###################################################
### code chunk number 70: GA_bivand_SI_2.Rnw:750-751
###################################################
#log_localI_med_inc_cv_no0 <- readRDS("log_localI_med_inc_cv_no0.rds")


###################################################
### code chunk number 71: GA_bivand_SI_2.Rnw:759-766
###################################################
df1 <- as.data.frame(localI_med_inc_cv_perm_no0[,8:9])
df2 <- as.data.frame(log_localI_med_inc_cv_no0[,8:9])
df1$transform <- "none"
df2$transform <- "log"
df3 <- rbind(df1, df2)
df3$transform <- factor(df3$transform, levels=c("none", "log"))
datasummary((Skewness + Kurtosis)*transform ~ P0 + P25 + Median + Mean + P75 + P100, df3, output="latex_tabular")


###################################################
### code chunk number 72: GA_bivand_SI_2.Rnw:775-780
###################################################
c <- d <- attr(log_localI_med_inc_cv_no0, "quadr")$mean
c[log_localI_med_inc_cv_no0[,6] > 0.05] <- as.integer(NA)
d[log_localI_med_inc_cv_no0[,5] > 0.05] <- as.integer(NA)
o <- datasummary_crosstab(rank ~ Z, 1 ~ 1 + N, data.frame(rank=addNA(c), Z=addNA(d)), output="latex_tabular")
cat(append(strsplit(o, "\\n")[[1]], c("& & \\multicolumn{6}{c}{standard deviate}\\\\", "\\cmidrule{3-8}"), 3), sep="\n")


###################################################
### code chunk number 73: GA_bivand_SI_2.Rnw:792-795
###################################################
nb_chicago_MA_tracts <- subset(nb_subset_s2, chicago_MA_tracts)
chicago_MA_lw <- nb2listw(nb_chicago_MA_tracts, style="W")
nb_chicago_MA_tracts


###################################################
### code chunk number 74: GA_bivand_SI_2.Rnw:802-803
###################################################
chicago_MA_localI_med_inc_cv_cond <- localmoran(chicago_MA_sf$med_inc_cv, chicago_MA_lw)


###################################################
### code chunk number 75: GA_bivand_SI_2.Rnw:810-811
###################################################
chicago_MA_localI_med_inc_cv_perm <- localmoran_perm(chicago_MA_sf$med_inc_cv, chicago_MA_lw, nsim=999, iseed=1)


###################################################
### code chunk number 76: GA_bivand_SI_2.Rnw:818-822
###################################################
q <- attr(chicago_MA_localI_med_inc_cv_perm, "quadr")$mean
chicago_MA_sf$hs_ac_q <- chicago_MA_sf$hs_sad_q <- chicago_MA_sf$hs_pr1_q <- q
is.na(chicago_MA_sf$hs_ac_q) <- chicago_MA_localI_med_inc_cv_cond[,5] > 0.1
is.na(chicago_MA_sf$hs_pr1_q) <- chicago_MA_localI_med_inc_cv_perm[,6] > 0.1


###################################################
### code chunk number 77: GA_bivand_SI_2.Rnw:829-837
###################################################
fig6 <- rep(as.integer(NA), nrow(chicago_MA_sf))
fig6_0 <- unclass(chicago_MA_sf$hs_pr1_q)
fig6_1 <- which(fig6_0 == 1L)
fig6_4 <- which(fig6_0 == 4L)
fig6[unique(c(fig6_1, unlist(nb_chicago_MA_tracts[fig6_1])))] <- 1L
fig6[unique(c(fig6_4, unlist(nb_chicago_MA_tracts[fig6_4])))] <- 4L
lbs <- levels(chicago_MA_sf$hs_pr1_q)
chicago_MA_sf$hs_fig6_q <- factor(fig6, levels=1:4, labels=lbs)


###################################################
### code chunk number 78: GA_bivand_SI_2.Rnw:844-849
###################################################
system.time({
    lm_obj <- lm(med_inc_cv ~ 1, chicago_MA_sf)
    sad <- localmoran.sad(lm_obj, nb=nb_chicago_MA_tracts, style="W", alternative="two.sided")
    chicago_MA_localI_med_inc_cv_sad <- as.data.frame(sad)
})


###################################################
### code chunk number 79: GA_bivand_SI_2.Rnw:854-855
###################################################
is.na(chicago_MA_sf$hs_sad_q) <- chicago_MA_localI_med_inc_cv_sad[,5] > 0.1


###################################################
### code chunk number 80: GA_bivand_SI_2.Rnw:862-866
###################################################
tm_shape(chicago_MA_sf, projection=st_crs("EPSG:6455")) + tm_fill(c("hs_ac_q", "hs_sad_q", "hs_pr1_q", "hs_fig6_q"), textNA="Insignificant", colorNA="gray95", palette="Set3", title="\nCluster\ncategory\n") + tm_facets(free.scales=FALSE, ncol=2, nrow=2) + tm_layout(panel.labels=c("Analytical std. dev. 0.1", "Saddlepoint 0.1", "Permutation ranks 0.1", "Folch et al. Figure 6."))


###################################################
### code chunk number 82: GA_bivand_SI_2.Rnw:880-883
###################################################
log_cv <- log(chicago_MA_sf$med_inc_cv)
log_chicago_MA_localI_med_inc_cv_cond <- localmoran(log_cv, chicago_MA_lw)
log_chicago_MA_localI_med_inc_cv_perm <- localmoran_perm(log_cv, chicago_MA_lw, nsim=999, iseed=1)


###################################################
### code chunk number 83: GA_bivand_SI_2.Rnw:891-892
###################################################
datasummary_df(data.frame(statistic=c("Skewness", "Kurtosis"), untransformed=c(e1071::skewness(chicago_MA_sf$med_inc_cv), e1071::kurtosis(chicago_MA_sf$med_inc_cv)), "log transformed"=c(e1071::skewness(log(chicago_MA_sf$med_inc_cv)), e1071::kurtosis(log(chicago_MA_sf$med_inc_cv))), check.names=FALSE), output="latex_tabular")


###################################################
### code chunk number 84: GA_bivand_SI_2.Rnw:902-909
###################################################
df1 <- as.data.frame(chicago_MA_localI_med_inc_cv_perm[,8:9])
df2 <- as.data.frame(log_chicago_MA_localI_med_inc_cv_perm[,8:9])
df1$transform <- "none"
df2$transform <- "log"
df3 <- rbind(df1, df2)
df3$transform <- factor(df3$transform, levels=c("none", "log"))
datasummary((Skewness + Kurtosis)*transform ~ P0 + P25 + Median + Mean + P75 + P100, df3, output="latex_tabular")


###################################################
### code chunk number 85: GA_bivand_SI_2.Rnw:916-922
###################################################
sd1 <- chicago_MA_localI_med_inc_cv_cond[,5]
sd2 <- chicago_MA_localI_med_inc_cv_perm[,5]
pr3 <- chicago_MA_localI_med_inc_cv_perm[,6]
sd4 <- log_chicago_MA_localI_med_inc_cv_cond[,5]
sd5 <- log_chicago_MA_localI_med_inc_cv_perm[,5]
pr6 <- log_chicago_MA_localI_med_inc_cv_perm[,6]


###################################################
### code chunk number 86: GA_bivand_SI_2.Rnw:930-931
###################################################
datasummary_correlation(data.frame("Z"=sd1, "Perm. Z"=sd2, "Rank"=pr3, "Log Z"=sd4, "Log perm. Z"=sd5, "Log rank"=pr6, check.names=FALSE), output="latex_tabular", fmt=4)


###################################################
### code chunk number 87: GA_bivand_SI_2.Rnw:942-950
###################################################
oopar <- par(mfrow=c(2,2))
plot(density(chicago_MA_sf$med_inc_cv), main="Med. income CV", xlab="CV")
plot(density(log(chicago_MA_sf$med_inc_cv)), main="Log med. income CV", xlab="log CV")
plot(sd2, pr3, main="", xlab="Std. dev. p-values", ylab="Rank p-values")
plot(sd5, pr6, main="", xlab="Std. dev. p-values", ylab="Rank p-values")
par(oopar)


###################################################
### code chunk number 88: GA_bivand_SI_2.Rnw:959-962
###################################################
lm_obj <- lm(log(med_inc_cv) ~ 1, chicago_MA_sf)
sad <- localmoran.sad(lm_obj, nb=nb_chicago_MA_tracts, style="W", alternative="two.sided")
log_chicago_MA_localI_med_inc_cv_sad <- as.data.frame(sad)


###################################################
### code chunk number 89: GA_bivand_SI_2.Rnw:969-976
###################################################
q <- attr(log_chicago_MA_localI_med_inc_cv_perm, "quadr")$mean
chicago_MA_sf$lhs_pr_q <- chicago_MA_sf$lhs_psd_q <- q
chicago_MA_sf$lhs_sd_q <- chicago_MA_sf$lhs_sad_q <- q
is.na(chicago_MA_sf$lhs_sd_q) <- log_chicago_MA_localI_med_inc_cv_cond[,5] > 0.05
is.na(chicago_MA_sf$lhs_sad_q) <- log_chicago_MA_localI_med_inc_cv_sad[,5] > 0.05
is.na(chicago_MA_sf$lhs_psd_q) <- log_chicago_MA_localI_med_inc_cv_perm[,5] > 0.05
is.na(chicago_MA_sf$lhs_pr_q) <- log_chicago_MA_localI_med_inc_cv_perm[,6] > 0.05


###################################################
### code chunk number 90: GA_bivand_SI_2.Rnw:982-986
###################################################
tm_shape(chicago_MA_sf, projection=st_crs("EPSG:6455")) + tm_fill(c("lhs_sd_q", "lhs_sad_q", "lhs_psd_q", "lhs_pr_q"), textNA="Insignificant", colorNA="gray95", palette="Set3", title="\nLog cluster\ncategory\n") + tm_facets(free.scales=FALSE, ncol=2, nrow=2) + tm_layout(panel.labels=c("Analytical std. dev. 0.05", "Saddlepoint 0.05", "Permutation std. dev. 0.05", "Permutation ranks 0.05"))


###################################################
### code chunk number 92: GA_bivand_SI_2.Rnw:1013-1014
###################################################
datasummary((log1p(med_inc_acs) + log1p(vacancy_rate) + log1p(old_rate) + log1p(black_rate) + log1p(hisp_rate) + log1p(group_pop) + log1p(dens))*mi_cv_esri ~ P0 + P25 + Median + Mean + P75 + P100, data=df_tracts, output = "latex_tabular")


###################################################
### code chunk number 93: GA_bivand_SI_2.Rnw:1026-1027
###################################################
form <- log(med_inc_cv) ~ log1p(med_inc_acs) + log1p(vacancy_rate) + log1p(old_rate) + log1p(black_rate) + log1p(hisp_rate) + log1p(group_pop) + log1p(dens)


###################################################
### code chunk number 94: GA_bivand_SI_2.Rnw:1034-1036
###################################################
lm_mod <- lm(form, data=df_tracts)
lm_modI <- lm.morantest(lm_mod, lw, alternative="two.sided")


###################################################
### code chunk number 95: GA_bivand_SI_2.Rnw:1043-1051
###################################################
geodf0 <- gp$read_file('df_tracts.gpkg')
nb_q0 <- ps$weights$Queen$from_dataframe(geodf0)
nb_q0$transform <- "r"
np <- import("numpy")
y <- np$array(model.response(model.frame(form, data=df_tracts)))
X <- np$array(model.matrix(form, data=df_tracts)[,-1])
spr <- import("spreg")
py_ols <- spr$OLS(y, X, w=nb_q0, spat_diag=TRUE, moran=TRUE)


###################################################
### code chunk number 96: GA_bivand_SI_2.Rnw:1055-1060
###################################################
ti <- data.frame(term=names(coef(lm_mod)), estimate=c(py_ols$betas), std.error=c(py_ols$std_err))
library(tibble)
gl <- as_tibble(data.frame("r.squared"=py_ols$r2, "adj.r.squared"=py_ols$ar2, "sigma"=sqrt(py_ols$sig2), "logLik"=py_ols$logll, "AIC"=py_ols$aic, "nobs"=py_ols$n, "F"=py_ols$f_stat[[1]], "Moran's I"=py_ols$moran_res[[1]], "Pr (|I| > 0)"=py_ols$moran_res[[3]], check.names=FALSE))
py_OLS_ms <- list(tidy = ti, glance = gl)
class(py_OLS_ms) <- "modelsummary_list"


###################################################
### code chunk number 97: GA_bivand_SI_2.Rnw:1067-1069
###################################################
lm_modw <- lm(form, data=df_tracts, weights=tot_pop)
lm_modwI <- lm.morantest(lm_modw, lw, alternative="two.sided")


###################################################
### code chunk number 98: GA_bivand_SI_2.Rnw:1075-1079
###################################################
mod_list <- modelsummary(list("OLS"=lm_mod, "Weighted OLS"=lm_modw), output = "modelsummary_list")
mod_list[[1]][[2]] <- as_tibble(cbind(mod_list[[1]][[2]][,-9], data.frame("Moran's I"=unname(lm_modI$estimate[1]), "Pr (|I| > 0)"=lm_modI$p.value, check.names=FALSE)))
mod_list[[2]][[2]] <- as_tibble(cbind(mod_list[[2]][[2]][,-9], data.frame("Moran's I"=unname(lm_modwI$estimate[1]), "Pr (|I| > 0)"=lm_modwI$p.value, check.names=FALSE)))
mod_list <- c(mod_list, list("PySAL OLS"=py_OLS_ms))


###################################################
### code chunk number 99: GA_bivand_SI_2.Rnw:1086-1087
###################################################
modelsummary(mod_list, output = "latex_tabular")


###################################################
### code chunk number 100: GA_bivand_SI_2.Rnw:1097-1101
###################################################
library(spatialreg)
lwCS <- as(lw, "CsparseMatrix")
library(sphet)
system.time(SEM_GMM <- spreg(form, data=df_tracts, listw=lwCS, model="error", het=FALSE))


###################################################
### code chunk number 101: GA_bivand_SI_2.Rnw:1108-1109
###################################################
system.time(SEM_GMMh <- spreg(form, data=df_tracts, listw=lwCS, model="error", het=TRUE))


###################################################
### code chunk number 102: GA_bivand_SI_2.Rnw:1116-1118
###################################################
system.time(py_SEM_GMM <- spr$GM_Error_Hom(y, X, w=nb_q0))
system.time(py_SEM_GMMh <- spr$GM_Error_Het(y, X, w=nb_q0))


###################################################
### code chunk number 103: GA_bivand_SI_2.Rnw:1123-1138
###################################################
x <- summary(SEM_GMM)$CoefTable
ti <- data.frame(term=rownames(x), estimate=x[,1], std.error=x[,2])
gl <- NULL
SEM_GMM_ms <- list(tidy = ti, glance = gl)
class(SEM_GMM_ms) <- "modelsummary_list"
x <- summary(SEM_GMMh)$CoefTable
ti <- data.frame(term=rownames(x), estimate=x[,1], std.error=x[,2])
SEM_GMMh_ms <- list(tidy = ti, glance = gl)
class(SEM_GMMh_ms) <- "modelsummary_list"
ti <- data.frame(term=rownames(x), estimate=c(py_SEM_GMM$betas), std.error=sqrt(diag(py_SEM_GMM$vm)))
py_SEM_GMM_ms <- list(tidy = ti, glance = gl)
class(py_SEM_GMM_ms) <- "modelsummary_list"
ti <- data.frame(term=rownames(x), estimate=c(py_SEM_GMMh$betas), std.error=sqrt(diag(py_SEM_GMMh$vm)))
py_SEM_GMMh_ms <- list(tidy = ti, glance = gl)
class(py_SEM_GMMh_ms) <- "modelsummary_list"


###################################################
### code chunk number 104: GA_bivand_SI_2.Rnw:1146-1147
###################################################
suppressWarnings(modelsummary(list("sphet Hom"=SEM_GMM_ms, "sphet Het"=SEM_GMMh_ms, "PySAL Hom"=py_SEM_GMM_ms, "PySAL Het"=py_SEM_GMMh_ms), output = "latex_tabular" ))


###################################################
### code chunk number 105: GA_bivand_SI_2.Rnw:1157-1160
###################################################
SEM <- errorsarlm(form, data=df_tracts, listw=lw, method="Matrix", zero.policy=TRUE)
SEM_Haus <- Hausman.test(SEM)
apply(SEM$timings, 2, sum)


###################################################
### code chunk number 106: GA_bivand_SI_2.Rnw:1167-1169
###################################################
SEMw <- errorsarlm(form, weights=tot_pop, data=df_tracts, listw=lw, method="Matrix", zero.policy=TRUE)
apply(SEMw$timings, 2, sum)


###################################################
### code chunk number 107: GA_bivand_SI_2.Rnw:1176-1178
###################################################
mod_list <- modelsummary(list("SEM"=SEM, "Weighted SEM"=SEMw), output="modelsummary_list")
mod_list[[1]][[2]] <- as_tibble(cbind(mod_list[[1]][[2]], data.frame("Spatial Hausman (approx.)"=unname(SEM_Haus$statistic), "Spatial Hausman Pr (> 0)"=SEM_Haus$p.value, check.names=FALSE)))


###################################################
### code chunk number 108: GA_bivand_SI_2.Rnw:1186-1187
###################################################
modelsummary(mod_list, output = "latex_tabular")


###################################################
### code chunk number 109: GA_bivand_SI_2.Rnw:1208-1211
###################################################
library(sf)
library(tidycensus)
options(tigris_use_cache=TRUE)


###################################################
### code chunk number 110: GA_bivand_SI_2.Rnw:1213-1214 (eval = FALSE)
###################################################
## census_api_key("MY_KEY")


###################################################
### code chunk number 111: GA_bivand_SI_2.Rnw:1221-1223 (eval = FALSE)
###################################################
## us_drop <- c("AK", "HI", "AS", "GU", "MP", "PR", "UM", "VI")
## us <- setdiff(unique(fips_codes$state), us_drop)


###################################################
### code chunk number 112: GA_bivand_SI_2.Rnw:1230-1234 (eval = FALSE)
###################################################
## map10 <- get_acs(geography="tract", variables="B01003_001", year=2010, 
##     moe_level=90, state=us, geometry=TRUE)
## # Getting data from the 2006-2010 5-year ACS
## # Fetching tract data by state and combining the result.


###################################################
### code chunk number 113: GA_bivand_SI_2.Rnw:1241-1260 (eval = FALSE)
###################################################
## st_crs(map10)
## # Coordinate Reference System:
## #   User input: NAD83 
## #   wkt:
## # GEOGCRS["NAD83",
## #     DATUM["North American Datum 1983",
## #         ELLIPSOID["GRS 1980",6378137,298.257222101,
## #             LENGTHUNIT["metre",1]]],
## #     PRIMEM["Greenwich",0,
## #         ANGLEUNIT["degree",0.0174532925199433]],
## #     CS[ellipsoidal,2],
## #         AXIS["latitude",north,
## #             ORDER[1],
## #             ANGLEUNIT["degree",0.0174532925199433]],
## #         AXIS["longitude",east,
## #             ORDER[2],
## #             ANGLEUNIT["degree",0.0174532925199433]],
## #     ID["EPSG",4269]]
## st_crs(map10) <- "EPSG:4269"


###################################################
### code chunk number 114: GA_bivand_SI_2.Rnw:1267-1271 (eval = FALSE)
###################################################
## med_inc_acs10 <- get_acs(geography="tract", variables="B19013_001", year=2010, 
##     moe_level=90, state=us)
## # Getting data from the 2006-2010 5-year ACS
## # Fetching tract data by state and combining the result.


###################################################
### code chunk number 115: GA_bivand_SI_2.Rnw:1278-1284 (eval = FALSE)
###################################################
## cen10 <- get_decennial(geography="tract", variables=
##     c(tot_pop="P001001", tot_hu="H001001", vacant="H003003", group_pop="P042001",
##     black_tot="P008004", hisp_tot="P004003", m70_74="P012022", m75_79="P012023", 
##     m80_84="P012024", m85p="P012025", f70_74="P012046", f75_79="P012047", 
##     f80_84="P012048", f85p="P012049"), year=2010, state=us, output="wide")
## # Getting data from the 2010 decennial Census


###################################################
### code chunk number 116: GA_bivand_SI_2.Rnw:1291-1295 (eval = FALSE)
###################################################
## df <- merge(map10[, -c(2, 3)], med_inc_acs10[, -c(2, 3)], by="GEOID")
## names(df) <- c("GEOID", "tot_pop_acs", "tot_pop_moe", "med_inc_acs", "med_inc_moe",
##     "geometry")
## names(attr(df, "agr")) <- names(df)[-6]


###################################################
### code chunk number 117: GA_bivand_SI_2.Rnw:1302-1307 (eval = FALSE)
###################################################
## df_tracts_a <- merge(df, cen10, by="GEOID")
## df_tracts <- df_tracts_a[df_tracts_a$tot_pop > 500 & df_tracts_a$tot_hu > 200,]
## df_tracts <- df_tracts[!is.na(df_tracts$med_inc_moe),]
## dim(df_tracts)
## # [1] 71353    21


###################################################
### code chunk number 118: GA_bivand_SI_2.Rnw:1314-1318 (eval = FALSE)
###################################################
## df_tracts$tot_pop_cv <- (df_tracts$tot_pop_moe/1.645) / 
##     df_tracts$tot_pop_acs
## df_tracts$med_inc_cv <- (df_tracts$med_inc_moe/1.645) /
##     df_tracts$med_inc_acs


###################################################
### code chunk number 119: GA_bivand_SI_2.Rnw:1325-1332 (eval = FALSE)
###################################################
## df_tracts$old_rate <- unname(apply(as.data.frame(df_tracts)[,13:20], 1, 
##     sum))/df_tracts$tot_pop
## df_tracts$black_rate <- df_tracts$black_tot/df_tracts$tot_pop
## df_tracts$hisp_rate <- df_tracts$hisp_tot/df_tracts$tot_pop
## df_tracts$vacancy_rate <- df_tracts$vacant/df_tracts$tot_hu
## df_tracts$area <- NISTunits::NISTsqrMeterTOacre(st_area(df_tracts))
## df_tracts$dens <- df_tracts$tot_pop/df_tracts$area


###################################################
### code chunk number 120: GA_bivand_SI_2.Rnw:1339-1340 (eval = FALSE)
###################################################
## st_write(df_tracts, "df_tracts.gpkg", append=FALSE)


sessionInfo()
sf_extSoftVersion()
sink(type = "message")
sink()
