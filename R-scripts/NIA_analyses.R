#-----------------------------------------------------------------------------
# Load preprocessed data
#-----------------------------------------------------------------------------

setwd("~/surfdrive/NIN/01_Projects/07_Lancee/R-scripts")
source('data_preparation.R')


#-----------------------------------------------------------------------------
# Run mgm analyses
#-----------------------------------------------------------------------------

# All dataframes include 16 variables:
#   variable 1: condition (0 = no treatment; 1 = treatment)
#   variable 2-8: insomnia symptoms (7 items; range 0-4)
#   variable 9-16: depression symptoms (8 items*; range 0-3) *The 3rd item of the PHQ was excluded because it is a sleep item.

set.seed(1)                                # make results reproducable: make sure folds used for cross-validation are the same
premgm <- mgm(data = pre_isi_phq,          # dataset of T0 (pre_isi_phq)
              type = c("c", rep("g", 15)), # condition variable is included as categorical "c", all symptoms as continuous "g"
              level = c(2, rep(1, 15)),    # condition variable has two levels "2", all symptoms included as continuous "1"
              lambdaSel = "CV",            # we used cross validation to select optimal tuning parameter 
              lambdaFolds = 10,            # using 10 folds
              k = 2,                       # we only include second order interactions 
              binarySign = TRUE,           
              pbar = FALSE, signInfo = FALSE)

set.seed(1)
w1mgm <- mgm(data = w1_isi_phq, type = c("c", rep("g", 15)), 
             level = c(2, rep(1, 15)), 
             lambdaSel = "CV", lambdaFolds = 10,
             k = 2, binarySign = TRUE,
             pbar = FALSE, signInfo = FALSE)

set.seed(1)
w2mgm <- mgm(data = w2_isi_phq, type = c("c", rep("g", 15)), 
             level = c(2, rep(1, 15)), 
             lambdaSel = "CV", lambdaFolds = 10,
             k = 2, binarySign = TRUE,
             pbar = FALSE, signInfo = FALSE)

set.seed(1)
w3mgm <- mgm(data = w3_isi_phq, type = c("c", rep("g", 15)), 
             level = c(2, rep(1, 15)), 
             lambdaSel = "CV", lambdaFolds = 10,
             k = 2, binarySign = TRUE,
             pbar = FALSE, signInfo = FALSE)

set.seed(1)
w4mgm <- mgm(data = w4_isi_phq, type = c("c", rep("g", 15)), 
             level = c(2, rep(1, 15)), 
             lambdaSel = "CV", lambdaFolds = 10,
             k = 2, binarySign = TRUE,
             pbar = FALSE, signInfo = FALSE)

set.seed(1)
w5mgm <- mgm(data = w5_isi_phq, type = c("c", rep("g", 15)), 
             level = c(2, rep(1, 15)), 
             lambdaSel = "CV", lambdaFolds = 10,
             k = 2, binarySign = TRUE,
             pbar = FALSE, signInfo = FALSE)

set.seed(1)
w6mgm <- mgm(data = w6_isi_phq, type = c("c", rep("g", 15)), 
             level = c(2, rep(1, 15)), 
             lambdaSel = "CV", lambdaFolds = 10, 
             k = 2, binarySign = TRUE,
             pbar = FALSE, signInfo = FALSE)

set.seed(1)
w7mgm <- mgm(data = w7_isi_phq, type = c("c", rep("g", 15)), 
             level = c(2, rep(1, 15)), 
             lambdaSel = "CV", lambdaFolds = 10, 
             k = 2, binarySign = TRUE, 
             pbar = FALSE, signInfo = FALSE)

set.seed(1)
w8mgm <- mgm(data = w8_isi_phq, type = c("c", rep("g", 15)), 
             level = c(2, rep(1, 15)), 
             lambdaSel = "CV", lambdaFolds = 10,
             k = 2, binarySign = TRUE,
             pbar = FALSE, signInfo = FALSE)

set.seed(1)
postmgm <- mgm(data = post_isi_phq, type = c("c", rep("g", 15)), 
               level = c(2, rep(1, 15)), 
               lambdaSel = "CV", lambdaFolds = 10, 
               k = 2, binarySign = TRUE,
               pbar = FALSE, signInfo = FALSE)


#-----------------------------------------------------------------------------
# Compute week averages for visualizating treatment-control differences
#-----------------------------------------------------------------------------

weeks      <- c("pre_isi_phq", paste0("w", 1:8, "_isi_phq"), "post_isi_phq") # names of the data frames (10) corresponding containing the data of the different weeks (T0-T9)
nodeLabels <- c("T", "DIS", "DMS", "EMA", "Dissat", "IDF", "NIQoL", "Worry", 
                "LoI", "DepMood", "Fatigue", "Appet", "Worth", "Con", "PsychMot", "Sui")

baselineM  <- apply(get(weeks[1]), 2, mean)[-1] # baseline mean for symptoms (omit first variable = condition variable)
baselineSD <- apply(get(weeks[1]), 2, sd)[-1]   # baseline sd for symptoms (omit first variable = condition variable)

varMeans   <- matrix(ncol = 15, nrow = 20)      # matrix to store the mean of each symptom (columns) for each week (rows) separate for control group (rows 1-10) and treatment group (rows 11-20)
names      <- paste(rep(paste("week", 1:10),  2), rep(c("control", "treat"),  each = 10))

for(i in 1:length(weeks)){
  dat     = get(weeks[i])
  
  control_mean = apply(dat[dat$conditie == 0, -1], 2, mean) # omit first condition variable (-1) and compute means across individuals in control group
  treat_mean   = apply(dat[dat$conditie == 1, -1], 2, mean) # omit first condition variable (-1) and compute means across individuals in treatment group
  
  control_sd = apply(dat[dat$conditie == 0, -1], 2, sd)
  treat_sd   = apply(dat[dat$conditie == 1, -1], 2, sd)
  
  varMeans[i, ] <- control_mean
  varMeans[(i+10), ] <- treat_mean
}

colnames(varMeans) = nodeLabels[-1]
rownames(varMeans) = names

# standardize scores to baseline mean and SD: 
#   negative = decrease in severity compared to baseline; 
#   positive = increase in severity
standardizeScores  = matrix(ncol = ncol(varMeans), nrow = nrow(varMeans)) 

for(j in 1:ncol(varMeans)){
  standardizeScores[, j] = (varMeans[, j] - baselineM[j]) / baselineSD[j] # standardize means to overall baseline mean±SD for each variable
}

colnames(standardizeScores) = nodeLabels[-1]
rownames(standardizeScores) = names

# difference in standardized severity between treatment and controls (treatment - controls)
#   negative = control group has higher severity than treatment group
#   positive = control group has lower severity than treatment group
differenceTable = matrix(nrow = 10, ncol = ncol(standardizeScores))

for(i in 1:nrow(differenceTable)){
  differenceTable[i, ] = standardizeScores[i+10, ] - standardizeScores[i, ] # compute difference in standardized mean between treatment group - control group
}

colnames(differenceTable) = nodeLabels[-1]
rownames(differenceTable) = paste("week", 1:10) 


#-----------------------------------------------------------------------------
# Compute predictability of symptoms per week
#-----------------------------------------------------------------------------

pred       <- predict(premgm, data = pre_isi_phq) # compute predictability for T0
R2premgm   <- pred$errors[,"R2"]                  # store R2 (predictability) in a vector
R2premgm[1] <- 0                                  # set predictaability of condition node from NA to 0

pred       <- predict(w1mgm, data = w1_isi_phq)
R2w1mgm    <- pred$errors[,"R2"]
R2w1mgm[1] <- 0

pred       <- predict(w2mgm, data = w2_isi_phq)
R2w2mgm    <- pred$errors[,"R2"]
R2w2mgm[1] <- 0

pred       <- predict(w3mgm, data = w3_isi_phq)
R2w3mgm    <- pred$errors[,"R2"]
R2w3mgm[1] <- 0

pred       <- predict(w4mgm, data = w4_isi_phq)
R2w4mgm    <- pred$errors[,"R2"]
R2w4mgm[1] <- 0

pred       <- predict(w5mgm, data = w5_isi_phq)
R2w5mgm    <- pred$errors[,"R2"]
R2w5mgm[1] <- 0

pred       <- predict(w6mgm, data = w6_isi_phq)
R2w6mgm    <- pred$errors[,"R2"]
R2w6mgm[1] <- 0

pred       <- predict(w7mgm, data = w7_isi_phq)
R2w7mgm    <- pred$errors[,"R2"]
R2w7mgm[1] <- 0

pred       <- predict(w8mgm, data = w8_isi_phq)
R2w8mgm    <- pred$errors[,"R2"]
R2w8mgm[1] <- 0

pred         <- predict(postmgm, data = post_isi_phq)
R2postmgm    <- pred$errors[,"R2"]
R2postmgm[1] <- 0

predictability <- data.frame(R2premgm, R2w1mgm, R2w2mgm, R2w3mgm,
                             R2w4mgm, R2w5mgm, R2w6mgm, R2w7mgm,
                             R2w8mgm, R2postmgm)                  # store all vectors in one data frame
rownames(predictability) <- nodeLabels



#-----------------------------------------------------------------------------
# Visualize networks 
#-----------------------------------------------------------------------------

size      = differenceTable                 # matrix capturing standardized differences in mean between groups (treatment - control)
condition = matrix(0, ncol = 1, nrow = 10)  # column representing condition node
size      = cbind(condition,size)           
size      = 8 + (size*2)                    # node sizes scaled to differences between groups (smaller node size when treatment mean < control mean)

nodeLabels = c("Treat", 
               "DIS", "DMS", "EMA", "Dissat", "IDF", "NIQoL", "Worry",                  # insomnia items
               "LoI", "DepMood", "Fatigue", "Appet", "Worth", "Con", "PsychMot", "Sui") # depression items

col        = c("white", rep("#9A8822", 7), rep("#F5CDB4", 8)) 


nodeGroups = list(Treatment = 1,
                  Insomnia = 2:8, 
                  Depression = 9:16) # define groups in the network

### "plot" networks, but without showing, to first average layout
pregraph  <- qgraph(premgm$pairwise$wadj, layout = "spring", DoNotPlot = TRUE)
w1graph   <- qgraph(w1mgm$pairwise$wadj, layout = "spring", DoNotPlot = TRUE)
w2graph   <- qgraph(w2mgm$pairwise$wadj, layout = "spring", DoNotPlot = TRUE)
w3graph   <- qgraph(w3mgm$pairwise$wadj, layout = "spring", DoNotPlot = TRUE)
w4graph   <- qgraph(w4mgm$pairwise$wadj, layout = "spring", DoNotPlot = TRUE)
w5graph   <- qgraph(w5mgm$pairwise$wadj, layout = "spring", DoNotPlot = TRUE)
w6graph   <- qgraph(w6mgm$pairwise$wadj, layout = "spring", DoNotPlot = TRUE)
w7graph   <- qgraph(w7mgm$pairwise$wadj, layout = "spring", DoNotPlot = TRUE)
w8graph   <- qgraph(w8mgm$pairwise$wadj, layout = "spring", DoNotPlot = TRUE)
postgraph <- qgraph(postmgm$pairwise$wadj, layout = "spring", DoNotPlot = TRUE)

Lay       <- qgraph::averageLayout(pregraph, w1graph, w2graph, w3graph, w4graph,
                                   w5graph, w6graph, w7graph, w8graph, postgraph)

pregraph <- qgraph(premgm$pairwise$wadj, layout = Lay,
                   shape = c("square", rep("circle", 15)), vsize = size[1,],
                   edge.color = premgm$pairwise$edgecolor, color = col, 
                   labels = nodeLabels, pie = R2premgm,  pieColor = '#377EB8')

w1graph <- qgraph(w1mgm$pairwise$wadj, layout = Lay,
                  shape = c("square", rep("circle", 15)), vsize = size[2, ],
                  edge.color = w1mgm$pairwise$edgecolor, color = col, 
                  labels = nodeLabels, pie = R2w1mgm,  pieColor = '#377EB8')

w2graph <- qgraph(w2mgm$pairwise$wadj, layout = Lay,
                  edge.color = w2mgm$pairwise$edgecolor, color = col,
                  shape = c("square", rep("circle", 15)), vsize = size[3, ],
                  labels = nodeLabels, pie = R2w2mgm,  pieColor = '#377EB8')

w3graph <- qgraph(w3mgm$pairwise$wadj, layout = Lay,
                  edge.color = w3mgm$pairwise$edgecolor, color = col,
                  shape = c("square", rep("circle", 15)), vsize = size[4, ],
                  labels = nodeLabels, pie = R2w3mgm,  pieColor = '#377EB8')

w4graph <- qgraph(w4mgm$pairwise$wadj, layout = Lay,
                  edge.color = w4mgm$pairwise$edgecolor, color = col,
                  shape = c("square", rep("circle", 15)), vsize = size[5, ],
                  labels = nodeLabels, pie = R2w4mgm,  pieColor = '#377EB8')

w5graph <- qgraph(w5mgm$pairwise$wadj, layout = Lay,
                  edge.color = w5mgm$pairwise$edgecolor, color = col,
                  shape = c("square", rep("circle", 15)), vsize = size[6, ],
                  labels = nodeLabels, pie = R2w5mgm,  pieColor = '#377EB8')

w6graph <- qgraph(w6mgm$pairwise$wadj, layout = Lay,
                  edge.color = w6mgm$pairwise$edgecolor, color = col,
                  shape = c("square", rep("circle", 15)), vsize = size[7, ],
                  labels = nodeLabels, pie = R2w6mgm,  pieColor = '#377EB8')

w7graph <- qgraph(w7mgm$pairwise$wadj, layout = Lay,
                  edge.color = w7mgm$pairwise$edgecolor, color = col,
                  shape = c("square", rep("circle", 15)), vsize = size[8, ],
                  labels = nodeLabels, pie = R2w7mgm,  pieColor = '#377EB8')

w8graph <- qgraph(w8mgm$pairwise$wadj, layout = Lay,
                  edge.color = w8mgm$pairwise$edgecolor, color = col,
                  shape = c("square", rep("circle", 15)), vsize = size[9, ],
                  labels = nodeLabels, pie = R2w8mgm,  pieColor = '#377EB8')

postgraph <- qgraph(postmgm$pairwise$wadj, layout = Lay,
                    edge.color = postmgm$pairwise$edgecolor, color = col,
                    shape = c("square", rep("circle", 15)), vsize = size[10, ],
                    labels = nodeLabels, pie = R2postmgm,  pieColor = '#377EB8')