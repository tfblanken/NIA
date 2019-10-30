#-----------------------------------------------------------------------------
# Load packages and data
#-----------------------------------------------------------------------------

require(foreign)
require(qgraph)
require(mgm)
require(igraph)
require(bootnet)

setwd('~/surfdrive/NIN/01_Projects/07_Lancee/Data')
data_all <- read.spss("DatasetTESSA_final.sav", to.data.frame = TRUE, use.value.labels = FALSE)


#-----------------------------------------------------------------------------
# Preprocess data
#-----------------------------------------------------------------------------

# Solve inconsistencies in column names
names(data_all)[which(names(data) == "Post_PHQ_tot")]    = "post_PHQ_total"
names(data_all)[which(names(data) == "POST_ISI_totaal")] = "post_ISI_total"


# Create vector with column names for PHQ items at each week (phq_idx) and ISI items at each week (isi_idx)
weken     <- c("pre", paste("week", 1:8, sep = ""), "post") # vector indicating the different weeks
phq_items <- paste("PHQ", c(1,2, 4:9), sep = "") # vector indicating PHQ items (excluding the 3rd sleep item)
phq_idx   <- paste(rep(weken, each = length(phq_items)), phq_items , sep = "_") # vector indicating all PHQ items at each week
isi_items <- paste("ISI", c(1:7), sep = "") # vector indicating ISI items
isi_idx   <- paste(rep(weken, each = length(isi_items)), isi_items, sep = "_") # vector indicating all ISI items at each week


# Select relevant columns: participant, condition, PHQ items for each week, ISI items for each week
sel       <- c('proefpersoon',
               'conditie',
               phq_idx,
               isi_idx)
data      <- data_all[, sel]


# Create data frames for each week that contains both PHQ and ISI items for that week
chunk        <- function(x,n) split(x, factor(sort(rank(x)%%n))) 
phq_chunks   <- chunk(phq_idx, length(phq_idx) %/% length(phq_items)) # list containing relevant columns for each week
isi_chunks   <- chunk(isi_idx, length(isi_idx) %/% length(isi_items))

pre_isi_phq  <- data[, c("conditie", isi_chunks[[1]], phq_chunks[[1]])] # PHQ and ISI items for T0
w1_isi_phq   <- data[, c("conditie", isi_chunks[[2]], phq_chunks[[2]])] # PHQ and ISI items for T1
w2_isi_phq   <- data[, c("conditie", isi_chunks[[3]], phq_chunks[[3]])] # PHQ and ISI items for T2
w3_isi_phq   <- data[, c("conditie", isi_chunks[[4]], phq_chunks[[4]])] # PHQ and ISI items for T3
w4_isi_phq   <- data[, c("conditie", isi_chunks[[5]], phq_chunks[[5]])] # PHQ and ISI items for T4
w5_isi_phq   <- data[, c("conditie", isi_chunks[[6]], phq_chunks[[6]])] # PHQ and ISI items for T5
w6_isi_phq   <- data[, c("conditie", isi_chunks[[7]], phq_chunks[[7]])] # PHQ and ISI items for T6
w7_isi_phq   <- data[, c("conditie", isi_chunks[[8]], phq_chunks[[8]])] # PHQ and ISI items for T7
w8_isi_phq   <- data[, c("conditie", isi_chunks[[9]], phq_chunks[[9]])] # PHQ and ISI items for T8
post_isi_phq <- data[, c("conditie", isi_chunks[[10]], phq_chunks[[10]])] # PHQ and ISI items for T9


# Delete missing cases (as mgm cannot handle missing)
pre_isi_phq  <- pre_isi_phq[complete.cases(pre_isi_phq), ]  # N=104
w1_isi_phq   <- w1_isi_phq[complete.cases(w1_isi_phq), ]    # N=100
w2_isi_phq   <- w2_isi_phq[complete.cases(w2_isi_phq), ]    # N=97
w3_isi_phq   <- w3_isi_phq[complete.cases(w3_isi_phq), ]    # N=92
w4_isi_phq   <- w4_isi_phq[complete.cases(w4_isi_phq), ]    # N=90
w5_isi_phq   <- w5_isi_phq[complete.cases(w5_isi_phq), ]    # N=84
w6_isi_phq   <- w6_isi_phq[complete.cases(w6_isi_phq), ]    # N=90
w7_isi_phq   <- w7_isi_phq[complete.cases(w7_isi_phq), ]    # N=87
w8_isi_phq   <- w8_isi_phq[complete.cases(w8_isi_phq), ]    # N=86
post_isi_phq <- post_isi_phq[complete.cases(post_isi_phq), ]# N=92

keep = c('pre_isi_phq', paste0('w', 1:8, '_isi_phq'), 'post_isi_phq')

rm(list=setdiff(ls(), keep))
