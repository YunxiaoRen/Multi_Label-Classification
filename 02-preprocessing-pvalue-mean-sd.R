library(ggpubr)
library(ggplot2)
library(ggsci)
library(plyr)
lr_data <- read.table("01_LR_metric_out.txt",stringsAsFactors = F,header = T)
dim(lr_data)
head(lr_data)

subdata1 <- subset(lr_data,Drug=="CIP")
subdata2 <- subset(lr_data,Drug=="CTX")
subdata3 <- subset(lr_data,Drug=="CTZ")
subdata4 <- subset(lr_data,Drug=="GEN")

my_comparisons <- list(c("BR","CC"),c("BR","ECC"),c("BR","LP"),c("BR","RD"),
                       c("CC","ECC"),c("CC","LP"),c("CC","RD"),
                       c("ECC","LP"),c("ECC","RD"),c("LP","RD"))

# pvalue_ctz <- compare_means(F_score ~ MLC, data = subdata3,comparisons = my_comparisons)
# pvalue_ctz <- compare_means(F_score ~ MLC, data = subdata3,ref.group = ".all.")
#pvalue_ctz <- compare_means(F_score ~ MLC, data = subdata3,method = "t.test",comparisons = my_comparisons)
#pvalue_ctz <- compare_means(F_score ~ MLC, data = subdata3,method = "t.test",ref.group = ".all.")

# pvalue_cip <- compare_means(F_score ~ MLC, data = subdata1,method = "t.test",comparisons = my_comparisons)
# pvalue_cip$drug = "CIP"
# pvalue_ctx <- compare_means(F_score ~ MLC, data = subdata2,method = "t.test",comparisons = my_comparisons)
# pvalue_ctx$drug = "CTX"
# pvalue_ctz <- compare_means(F_score ~ MLC, data = subdata3,method = "t.test",comparisons = my_comparisons)
# pvalue_ctz$drug = "CTZ"
# pvalue_gen <- compare_means(F_score ~ MLC, data = subdata4,method = "t.test",comparisons = my_comparisons)
# pvalue_gen$drug = "GEN"

pvalue_cip <- compare_means(F_score ~ MLC, data = subdata1,method = "t.test",ref.group = ".all.")
pvalue_cip$drug = "CIP"
pvalue_ctx <- compare_means(F_score ~ MLC, data = subdata2,method = "t.test",ref.group = ".all.")
pvalue_ctx$drug = "CTX"
pvalue_ctz <- compare_means(F_score ~ MLC, data = subdata3,method = "t.test",ref.group = ".all.")
pvalue_ctz$drug = "CTZ"
pvalue_gen <- compare_means(F_score ~ MLC, data = subdata4,method = "t.test",ref.group = ".all.")
pvalue_gen$drug = "GEN"

pvalue_two_compa <- rbind(pvalue_cip,pvalue_ctx,pvalue_ctz,pvalue_gen)
pvalue_all_compa <- rbind(pvalue_cip,pvalue_ctx,pvalue_ctz,pvalue_gen)
pvalue_all_t_compa <- rbind(pvalue_cip,pvalue_ctx,pvalue_ctz,pvalue_gen)
pvalue_two_t_compa <- rbind(pvalue_cip,pvalue_ctx,pvalue_ctz,pvalue_gen)
lr_pvalue_all <- rbind(pvalue_two_compa,pvalue_all_compa,pvalue_all_t_compa,pvalue_two_t_compa)

write.table(lr_pvalue_all,file = "02_LR_pvalue_all.txt",quote = F,row.names = F)


#calculate mean and sd by group 


# function to calculate the mean and standard deviation for each group
# data: a data frame 
# varname: the name of a column containing the variable to be summarized
# groupnames: vector of column names to be used as grouping variable
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm = T),
      sd = sd(x[[col]], na.rm = T))
  }
  data_sum <- ddply(data, groupnames, .fun = summary_func, varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

lr_data2 <- data_summary(lr_data, varname = "F_score", groupnames = c("MLC", "Drug"))
lr_data3 <- data_summary(lr_data, varname = "Precision", groupnames = c("MLC", "Drug"))
lr_data4 <- data_summary(lr_data, varname = "Recall", groupnames = c("MLC", "Drug"))
lr_data5 <- data_summary(lr_data, varname = "Jaccard_score", groupnames = c("MLC", "Drug"))

lr_data_mean_sd <- cbind(lr_data2,lr_data3,lr_data4,lr_data5)
lr_data_mean_sd = lr_data_mean_sd[,-c(5,6,9,10,13,14)]
names(lr_data_mean_sd)=c( "MLC","Drug","F_score_mean", "F_sd", "Precision_mean","P_sd", "Recall_mean","R_sd","Jaccard_score_mean","J_sd")
write.table(lr_data_mean_sd,file = "03_LR_data_mean_sd.txt",quote = F,row.names = F)

# ref: https://www.jianshu.com/p/634f6061a9cb

#########################################################################################################
########## SVM_preprocessing #########
svm_data <- read.table("01_SVM_metric_out.txt",stringsAsFactors = F,header = T)
dim(svm_data)
head(svm_data)

subdata1 <- subset(svm_data,Drug=="CIP")
subdata2 <- subset(svm_data,Drug=="CTX")
subdata3 <- subset(svm_data,Drug=="CTZ")
subdata4 <- subset(svm_data,Drug=="GEN")

my_comparisons <- list(c("BR","CC"),c("BR","ECC"),c("BR","LP"),c("BR","RD"),
                       c("CC","ECC"),c("CC","LP"),c("CC","RD"),
                       c("ECC","LP"),c("ECC","RD"),c("LP","RD"))

# pvalue_ctz <- compare_means(F_score ~ MLC, data = subdata3,comparisons = my_comparisons)
# pvalue_ctz <- compare_means(F_score ~ MLC, data = subdata3,ref.group = ".all.")
#pvalue_ctz <- compare_means(F_score ~ MLC, data = subdata3,method = "t.test",comparisons = my_comparisons)
#pvalue_ctz <- compare_means(F_score ~ MLC, data = subdata3,method = "t.test",ref.group = ".all.")

pvalue_cip <- compare_means(F_score ~ MLC, data = subdata1,method = "t.test",comparisons = my_comparisons)
pvalue_cip$drug = "CIP"
pvalue_ctx <- compare_means(F_score ~ MLC, data = subdata2,method = "t.test",comparisons = my_comparisons)
pvalue_ctx$drug = "CTX"
pvalue_ctz <- compare_means(F_score ~ MLC, data = subdata3,method = "t.test",comparisons = my_comparisons)
pvalue_ctz$drug = "CTZ"
pvalue_gen <- compare_means(F_score ~ MLC, data = subdata4,method = "t.test",comparisons = my_comparisons)
pvalue_gen$drug = "GEN"

pvalue_cip <- compare_means(F_score ~ MLC, data = subdata1,method = "t.test",ref.group = ".all.")
pvalue_cip$drug = "CIP"
pvalue_ctx <- compare_means(F_score ~ MLC, data = subdata2,method = "t.test",ref.group = ".all.")
pvalue_ctx$drug = "CTX"
pvalue_ctz <- compare_means(F_score ~ MLC, data = subdata3,method = "t.test",ref.group = ".all.")
pvalue_ctz$drug = "CTZ"
pvalue_gen <- compare_means(F_score ~ MLC, data = subdata4,method = "t.test",ref.group = ".all.")
pvalue_gen$drug = "GEN"

pvalue_two_compa <- rbind(pvalue_cip,pvalue_ctx,pvalue_ctz,pvalue_gen)
pvalue_all_compa <- rbind(pvalue_cip,pvalue_ctx,pvalue_ctz,pvalue_gen)
pvalue_all_t_compa <- rbind(pvalue_cip,pvalue_ctx,pvalue_ctz,pvalue_gen)
pvalue_two_t_compa <- rbind(pvalue_cip,pvalue_ctx,pvalue_ctz,pvalue_gen)
svm_pvalue_all <- rbind(pvalue_two_compa,pvalue_all_compa,pvalue_all_t_compa,pvalue_two_t_compa)

write.table(svm_pvalue_all,file = "02_SVM_pvalue_all.txt",quote = F,row.names = F)


#calculate mean and sd by group 


# function to calculate the mean and standard deviation for each group
# data: a data frame 
# varname: the name of a column containing the variable to be summarized
# groupnames: vector of column names to be used as grouping variable
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm = T),
      sd = sd(x[[col]], na.rm = T))
  }
  data_sum <- ddply(data, groupnames, .fun = summary_func, varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

svm_data2 <- data_summary(svm_data, varname = "F_score", groupnames = c("MLC", "Drug"))
svm_data3 <- data_summary(svm_data, varname = "Precision", groupnames = c("MLC", "Drug"))
svm_data4 <- data_summary(svm_data, varname = "Recall", groupnames = c("MLC", "Drug"))
svm_data5 <- data_summary(svm_data, varname = "Jaccard_score", groupnames = c("MLC", "Drug"))

svm_data_mean_sd <- cbind(svm_data2,svm_data3,svm_data4,svm_data5)
svm_data_mean_sd = svm_data_mean_sd[,-c(5,6,9,10,13,14)]
names(svm_data_mean_sd)=c( "MLC","Drug","F_score_mean", "F_sd", "Precision_mean","P_sd", "Recall_mean","R_sd","Jaccard_score_mean","J_sd")
write.table(svm_data_mean_sd,file = "03_SVM_data_mean_sd.txt",quote = F,row.names = F)


##############################################################################
### CNN


cnn_data <- read.table("01_CNN_metric_out.txt",stringsAsFactors = F,header = T)
dim(cnn_data)
head(cnn_data)

my_comparisons <- list(c("CIP","CTX"),c("CIP","CTZ"),c("CIP","GEN"),c("CTX","CTZ"),
                       c("CTX","GEN"),c("CTZ","GEN"))


pvalue1 <- compare_means(F_score ~ Drug, data = cnn_data,method = "t.test",comparisons = my_comparisons)
pvalue2 <- compare_means(F_score ~ Drug, data = cnn_data,method = "t.test",ref.group = ".all.")
pvalue3 <- compare_means(F_score ~ Drug, data = cnn_data,comparisons = my_comparisons)
pvalue4 <- compare_means(F_score ~ Drug, data = cnn_data,ref.group = ".all.")
cnn_pvalue_all <- rbind(pvalue1,pvalue2,pvalue3,pvalue4)

write.table(cnn_pvalue_all,file = "02_CNN_pvalue_all.txt",quote = F,row.names = F)

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm = T),
      sd = sd(x[[col]], na.rm = T))
  }
  data_sum <- ddply(data, groupnames, .fun = summary_func, varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

cnn_data2 <- data_summary(cnn_data, varname = "F_score", groupnames = "Drug")
cnn_data3 <- data_summary(cnn_data, varname = "Precision", groupnames = "Drug")
cnn_data4 <- data_summary(cnn_data, varname = "Recall", groupnames = "Drug")
cnn_data5 <- data_summary(cnn_data, varname = "Jaccard_score", groupnames = "Drug")

cnn_data_mean_sd <- cbind(cnn_data2,cnn_data3,cnn_data4,cnn_data5)
cnn_data_mean_sd = cnn_data_mean_sd[,-c(4,7,10)]
names(cnn_data_mean_sd)=c( "Drug","F_score_mean", "F_sd", "Precision_mean","P_sd", "Recall_mean","R_sd","Jaccard_score_mean","J_sd")
write.table(cnn_data_mean_sd,file = "03_CNN_data_mean_sd.txt",quote = F,row.names = F)



