# UML Final individual report
# Code for main part:
#-------------------------------------------------------------------------------
# -------------------------------------------Install packages and import dataset
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               readr,
               vtable,
               naniar,
               ggplot2,
               corrplot,
               caret,
               GGally,
               kernlab,
               factoextra,
               PMA,
               patchwork,
               gridExtra)

# import dataset and reduce useless column
df<- read_csv("Breast Cancer.csv")
df<- df %>% select(-id)

# create a dataframe keep B and M
df_BM <- df

# set Malignant = 1
df$diagnosis <- ifelse(df$diagnosis =="M", 1, -1)
table(df$diagnosis)
# '1'=212, '-1'=357

# check summary statistics of data set
sumtable(df, out = 'latex')

# make correlation plots
pdf("3 correlation.pdf")
# Cancer Mean
ggcorr(df[,c(2:11)], name = "corr", label = TRUE)+
  theme(legend.position="none")+
  labs(title="Cancer Mean")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))

# Cancer SE
ggcorr(df[,c(12:21)], name = "corr", label = TRUE)+
  theme(legend.position="none")+
  labs(title="Cancer SE")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))

# Cancer Worst
ggcorr(df[,c(22:31)], name = "corr", label = TRUE)+
  theme(legend.position="none")+
  labs(title="Cancer Worst")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))
dev.off()

#-------------------------------------------------------------------------------
# --------------------------------------------Principal Component Analysis (PCA)
PCA <- prcomp(df[, -1], scale=TRUE)
summary(PCA)

# plot the variance explained by the first few principal components
fviz_eig(PCA, addlabels = TRUE, barfill = "pink", barcolor = "deepskyblue3", 
         linecolor = "red",ggtheme = theme_minimal(base_size = 12)) +
  labs(title = "Scree plot of PCA", x = "Principal Components", y = "% of explained variances")

# plot contributions of variables to PC1 & PC2
p1 <- fviz_contrib(PCA, choice="var", axes=1, fill="pink", top=10)
p2 <- fviz_contrib(PCA, choice="var", axes=2, fill="skyblue", top=10)
grid.arrange(p1, p2, ncol=2)

# calculate first 6 PCs sums
cat("--- Proportion variance explained by first 6 PCs --- \n")
con = (PCA$sdev)^2/sum((PCA$sdev)^2)
sum(con[1:6])
# 0.887588 -> first 6 PCs explain 88.8% of variance

# plot diagnosis scatter
PCA_df <- as.data.frame(PCA$x)
Diagnosis <- df$diagnosis
PC_plot<-ggplot(PCA_df, aes(x=PC1, y=PC2,col=Diagnosis)) + geom_point(alpha=0.5)
PC_plot+scale_color_gradient(low="deepskyblue2", high="brown1")

#-------------------------------------------------------------------------------
# --------------------------------------------------------------Data preparation
# make new dataframe, each column of x is the kth PC score vector
df_pca <- PCA$x[,1:6]
df_pca <- as.data.frame(df_pca)
head(df_pca)

# make a new df combining PCA values and original diagnosis as response variable
diagnosis <- df$diagnosis
df_pca <- cbind(diagnosis,df_pca)

# partition data set into train and test for SVM
set.seed(2022)
train_idx <- sample(1:nrow(df_pca),floor(0.7*nrow(df_pca)))
df_train <- df_pca[train_idx,] %>% as.matrix
df_test <- df_pca[-train_idx,] %>% as.matrix

#-------------------------------------------------------------------------------
# ------------------------Linear Support Vector Machine (SVM) with Linear kernel
SVM_Lin <- ksvm(diagnosis~ ., data = df_train, scale = FALSE, kernel = "vanilladot")
# predict
Eval_Lin <- predict(SVM_Lin, df_test[,-1])
Eval_Lin <- ifelse(Eval_Lin > 0, 1, -1)
df.test <- as.matrix(df_test[,1])
table(df.test)

# confusion matrix - RBF kernel
confusionMatrix(as.factor(Eval_Lin),as.factor(df.test))

#-------------------------------------------------------------------------------
# ---------------------------Linear Support Vector Machine (SVM) with RBF kernel
SVM_RBF <- ksvm(diagnosis~ ., data = df_train, scale = FALSE, kernel = "rbfdot")
# predict
Eval_RBF <- predict(SVM_RBF, df_test[,-1])
Eval_RBF <- ifelse(Eval_RBF > 0, 1, -1)

# confusion matrix - RBF kernel
confusionMatrix(as.factor(Eval_RBF),as.factor(df.test))

#-------------------------------------------------------------------------------
# --------------------Linear Support Vector Machine (SVM) with polynomial Kernel
SVM_Poly <- ksvm(diagnosis~ ., data = df_train, scale = FALSE, kernel = "polydot")
# predict
Eval_Poly <- predict(SVM_Poly, df_test[,-1])
Eval_Poly <- ifelse(Eval_Poly > 0, 1, -1)

#confusion matrix - Polynomial kernel
confusionMatrix(as.factor(Eval_Poly),as.factor(df.test))

