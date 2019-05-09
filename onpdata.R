require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)
require(MCMCglmm )
require(mvtnorm)
require(beepr)
require(mcmcplots)
require("Zelig")
require(statmod)
require(MCMCpack)

onp <- read.csv("onp_train.csv",header = T, stringsAsFactors = F)
vec.remove <- c('X.1','X','url','timedelta','shares','ID','dt',
                "n_non_stop_unique_tokens","n_non_stop_words","kw_avg_min","self_reference_max_shares", 
                "kw_min_min","self_reference_min_shares","kw_max_avg","rate_negative_words",
                "avg_negative_polarity","rate_positive_words","title_subjectivity","max_positive_polarity",
                "avg_positive_polarity","average_token_length","kw_max_min","global_rate_positive_words",
                "kw_max_max")
onp <- onp[,!(colnames(onp) %in% vec.remove)]

## normalize, for penalty
onp$n_tokens_title <- onp$n_tokens_title/max(onp$n_tokens_title)
onp$n_tokens_content <- onp$n_tokens_content/max(onp$n_tokens_content)
onp$n_unique_tokens <- onp$n_unique_tokens/max(onp$n_unique_tokens)
onp$num_hrefs <- onp$num_hrefs/max(onp$num_hrefs)
onp$num_self_hrefs <- onp$num_self_hrefs/max(onp$num_self_hrefs)
onp$num_imgs <- onp$num_imgs/max(onp$num_imgs)
onp$num_videos <- onp$num_videos/max(onp$num_videos)
onp$num_keywords <- onp$num_keywords/max(onp$num_keywords)
onp$kw_min_max <- onp$kw_min_max/max(onp$kw_min_max)
onp$kw_avg_max <- onp$kw_avg_max/max(onp$kw_avg_max)
onp$kw_min_avg <- onp$kw_min_avg/max(onp$kw_min_avg)
onp$kw_avg_avg <- onp$kw_avg_avg/max(onp$kw_avg_avg)
onp$self_reference_avg_sharess <- onp$self_reference_avg_sharess/max(onp$self_reference_avg_sharess)


X <- as.matrix( onp[,-ncol(onp)] )

onpfit <- polr( as.factor(share_cat)  ~ . ,  Hess=TRUE , method = "probit" , data = onp )
# fit1 <- polr( as.factor(share_cat) ~ 1   ,  Hess=TRUE , method = "probit", data = onp )
X <- model.matrix( onpfit )
Y <- onp$share_cat
uy <- length( unique(Y) )

onp.train <- onp

onp <- read.csv("onp_test.csv",header = T, stringsAsFactors = F)
vec.remove <- c('X.1','X','url','timedelta','shares','ID','dt',
                "n_non_stop_unique_tokens","n_non_stop_words","kw_avg_min","self_reference_max_shares", 
                "kw_min_min","self_reference_min_shares","kw_max_avg","rate_negative_words",
                "avg_negative_polarity","rate_positive_words","title_subjectivity","max_positive_polarity",
                "avg_positive_polarity","average_token_length","kw_max_min","global_rate_positive_words",
                "kw_max_max")
onp <- onp[,!(colnames(onp) %in% vec.remove)]

## normalize, for penalty
onp$n_tokens_title <- onp$n_tokens_title/max(onp$n_tokens_title)
onp$n_tokens_content <- onp$n_tokens_content/max(onp$n_tokens_content)
onp$n_unique_tokens <- onp$n_unique_tokens/max(onp$n_unique_tokens)
onp$num_hrefs <- onp$num_hrefs/max(onp$num_hrefs)
onp$num_self_hrefs <- onp$num_self_hrefs/max(onp$num_self_hrefs)
onp$num_imgs <- onp$num_imgs/max(onp$num_imgs)
onp$num_videos <- onp$num_videos/max(onp$num_videos)
onp$num_keywords <- onp$num_keywords/max(onp$num_keywords)
onp$kw_min_max <- onp$kw_min_max/max(onp$kw_min_max)
onp$kw_avg_max <- onp$kw_avg_max/max(onp$kw_avg_max)
onp$kw_min_avg <- onp$kw_min_avg/max(onp$kw_min_avg)
onp$kw_avg_avg <- onp$kw_avg_avg/max(onp$kw_avg_avg)
onp$self_reference_avg_sharess <- onp$self_reference_avg_sharess/max(onp$self_reference_avg_sharess)



onp.test <- onp
onp.test$topic <- as.factor(onp.test$topic )
rm(onp)

