# PACKAGES
sessionInfo()

library('readr')
library('dplyr')

# SET WORKING DIRECTORY
setwd(' ')
MAIN_DIR <- getwd()

# DATA 
dat <- as.data.frame(read_tsv('Data_extraction_table_20240606_articles.tsv')) # read in data 
dat <- dat[,c("sample_size", "network_size_nodes", "association", "modelling", "inclusion_strategy", "weighted", "deconfounding_level", "exclude")] # keep only columns that don't contain comments 
dat <- dat[dat$exclude == "no",] # keep only rows with papers that were not excluded 

# Ensure columns are numeric
dat$sample_size <- as.numeric(dat$sample_size)
dat$network_size_nodes <- as.numeric(dat$network_size_nodes)

# CHECK
head(dat)
nrow(dat)
str(dat)


## RECODE VARIABLES AND DEFINE CATEGORIES FOR THE FUNDAMENTAL BUILDING BLOCKS

# ASSOCIATION TYPE
dat$association <- ifelse((dat$association == "full correlation"), 'pairwise correlation', 
                          ifelse((dat$association == 'partial correlation'), 'partial correlation',
                                 ifelse((dat$association == 'correlation'), 'not specified',
                                        ifelse((dat$association == 'not specified'), "not specified",
                                               "other"))))

cat_association <- unique(dat$association)

# EDGE INCLUSION STRATEGY 
dat$inclusion_strategy <- ifelse((dat$`inclusion_strategy` == "yes (negative edges set to 0)"|
                           dat$`inclusion_strategy` == "yes (multiple)"|
                           dat$`inclusion_strategy` == "yes (cross validation)"|
                           dat$`inclusion_strategy` == "yes (minimum spanning tree)"|
                           dat$`inclusion_strategy` == "yes (normalised entropy and high amplitude co-fluctuations)"|
                           dat$`inclusion_strategy` == "yes (averages of multiple runs for a single participant)"|
                           dat$`inclusion_strategy` == "yes (correlation with behavioral outcomes)"|
                           dat$`inclusion_strategy` == "yes (edges involving nodes of interest)"),
                        'yes (other)', 
                    ifelse((dat$`inclusion_strategy` == "yes (regularisation)"), 
                           "yes (regularisation)", 
                           ifelse((dat$`inclusion_strategy` == "yes (thresholding)"), 
                                  "yes (thresholding)", 
                                  ifelse((dat$`inclusion_strategy` == "both used and not used"), 
                                  "multiple", 
                                      ifelse((dat$`inclusion_strategy` == "no" |
                                              dat$`inclusion_strategy` == "not applicable"), 
                                              "no", "not specified")))))

cat_inclusion <- unique(dat$inclusion_strategy)

# EDGE WEIGHTS 
dat$weighted <- ifelse((dat$weighted == "yes (absolutised)" | dat$weighted == "yes"), 'weighted', 
                     ifelse((dat$weighted == 'both'), 'multiple','unweighted'))

cat_weight <- unique(dat$weighted)

# MODELLING LEVEL
cat_modelling <- unique(dat$modelling)

# CHECK
cat_association
cat_inclusion
cat_weight
cat_modelling



## CALCULATE NR OF STUDIES USING CERTAIN COMBINATIONS

# ASSOCIATION TYPE & EDGE INCLUSION STRATEGIES/EDGE WEIGHTS/MODELLING
contingency_table_CAT1_CAT2 <- data.frame()
contingency_table_CAT1_CAT3 <- data.frame()
contingency_table_CAT1_CAT4 <- data.frame()

for(CAT1 in cat_association) {
  
  print("ASSOCIATION TYPE & EDGE INCLUSION STRATEGIES")
  for(CAT2 in cat_inclusion) {
    n_studies <- length(which(dat$association == CAT1 & dat$inclusion_strategy == CAT2))
    print(paste0("Combination of ", CAT1, " and ", CAT2, ": ", n_studies))
    
    # save in contingency table
    contingency_table_CAT1_CAT2 <- rbind(contingency_table_CAT1_CAT2, data.frame(association = CAT1, inclusion_strategy = CAT2, n_studies = n_studies))
  }
  
  print("ASSOCIATION TYPE & EDGE WEIGHTS")
  for(CAT3 in cat_weight) {
    n_studies <- length(which(dat$association == CAT1 & dat$weighted == CAT3))
    print(paste0("Combination of ", CAT1, " and ", CAT3, ": ", n_studies))
    
    # save in contingency table
    contingency_table_CAT1_CAT3 <- rbind(contingency_table_CAT1_CAT3, data.frame(association = CAT1, weighted = CAT3, n_studies = n_studies))
  }
  
  print("ASSOCIATION TYPE & MODELLING")
  for(CAT4 in cat_modelling) {
    n_studies <- length(which(dat$association == CAT1 & dat$modelling == CAT4))
    print(paste0("Combination of ", CAT1, " and ", CAT4, ": ", n_studies))
    
    # save in contingency table
    contingency_table_CAT1_CAT4 <- rbind(contingency_table_CAT1_CAT4, data.frame(association = CAT1, modelling = CAT4, n_studies = n_studies))
  }
}

# EDGE INCLUSION STRATEGIES & EDGE WEIGHTS/MODELLING
contingency_table_CAT2_CAT3 <- data.frame()
contingency_table_CAT2_CAT4 <- data.frame()

for(CAT2 in cat_inclusion) {

  print("EDGE INCLUSION STRATEGIES & EDGE WEIGHTS")
  for(CAT3 in cat_weight) {
    n_studies <- length(which(dat$inclusion_strategy == CAT2 & dat$weighted == CAT3))
    print(paste0("Combination of ", CAT2, " and ", CAT3, ": ", n_studies))
    
    # save in contingency table
    contingency_table_CAT2_CAT3 <- rbind(contingency_table_CAT2_CAT3, data.frame(inclusion_strategy = CAT2, weighted = CAT3, n_studies = n_studies))
  }
  
  print("EDGE INCLUSION STRATEGIES & MODELLING")
  for(CAT4 in cat_modelling) {
    n_studies <- length(which(dat$inclusion_strategy == CAT2 & dat$modelling == CAT4))
    print(paste0("Combination of ", CAT2, " and ", CAT4, ": ", n_studies))
    
    # save in contingency table
    contingency_table_CAT2_CAT4 <- rbind(contingency_table_CAT2_CAT4, data.frame(inclusion_strategy = CAT2, modelling = CAT4, n_studies = n_studies))
  }
}

# EDGE WEIGHTS & MODELLING
contingency_table_CAT3_CAT4 <- data.frame()

for(CAT3 in cat_weight) {
  
  print("EDGE WEIGHTS & MODELLING")
  for(CAT4 in cat_modelling) {
    n_studies <- length(which(dat$weighted == CAT3 & dat$modelling == CAT4))
    print(paste0("Combination of ", CAT3, " and ", CAT4, ": ", n_studies))
    
    # save in contingency table
    contingency_table_CAT3_CAT4 <- rbind(contingency_table_CAT3_CAT4, data.frame(weighted = CAT3, modelling = CAT4, n_studies = n_studies))
  }
}


## COMBINE INTO ONE CONTINGENCY TABLE
contingency_table_CAT1_CAT2
contingency_table_CAT1_CAT3
contingency_table_CAT1_CAT4
contingency_table_CAT2_CAT3
contingency_table_CAT2_CAT4
contingency_table_CAT3_CAT4

# save separate contingency tables and combine manually in excel
write.csv(contingency_table_CAT1_CAT2, "contingency_table_association_inclusion", row.names = FALSE)
write.csv(contingency_table_CAT1_CAT3, "contingency_table_association_weighted", row.names = FALSE)
write.csv(contingency_table_CAT1_CAT4, "contingency_table_association_modelling", row.names = FALSE)
write.csv(contingency_table_CAT2_CAT3, "contingency_table_inclusion_weighted", row.names = FALSE)
write.csv(contingency_table_CAT2_CAT4, "contingency_table_inclusion_modelling", row.names = FALSE)
write.csv(contingency_table_CAT3_CAT4, "contingency_table_weighted_modelling", row.names = FALSE)
