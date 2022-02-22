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
               SVMMaj,
               kernlab,
               factoextra,
               PMA,
               e1071,
               patchwork,
               gridExtra
               )
df<- read_csv("Breast Cancer.csv")
df<- df %>% select(-id)
table(df$diagnosis)
# Malignant = 1
df$diagnosis <- ifelse(df$diagnosis =="M", 1, -1)
sumtable(df, out = 'latex')

# make correlation plot
corr = cor(df)
corrplot(corr,type="lower",col = COL1('Reds'),title="correlation of variables",
         tl.col=1,tl.cex=0.6) 

#-------------------------------------------------------------------------------
# --------------------------------------------Principal Component Analysis (PCA)
PCA <- prcomp(df[, -1], scale=TRUE)
summary(PCA)

# the first M principal component loading vectors provide the best M -dimensional
# approximation (in terms of Euclidean distance) to the ith observation xij
fviz_eig(PCA, addlabels = TRUE, barfill = "pink", barcolor = "pink", 
         linecolor = "black",ggtheme = theme_minimal(base_size = 12)) +
  labs(title = "Scree plot of PCA", x = "Principal Components", y = "% of explained variances")

# plot contribution
p1 <- fviz_contrib(PCA, choice="var", axes=1, fill="pink", top=10)
p2 <- fviz_contrib(PCA, choice="var", axes=2, fill="skyblue", top=10)
grid.arrange(p1, p2, ncol=2)

con = (PCA$sdev)^2/sum((PCA$sdev)^2)
sum(con[1:6])
#0.887588 -> choose first 6 components

#-------------------------------------------------------------------------------
# --------------------------------------------------------------Data preparation
# make new dataframe
df_pca <- PCA$x[,1:6]
df_pca <- as.data.frame(df_pca)
head(df_pca)

diagnosis <- df$diagnosis
df_pca <- cbind(diagnosis,df_pca)

set.seed(2022)
train_idx <- sample(1:nrow(df_pca),floor(0.75*nrow(df_pca)))
df_train <- df_pca[train_idx,] %>% as.matrix
df_test <- df_pca[-train_idx,] %>% as.matrix

#y_train <- df_train %>% pull(diagnosis) %>% as.matrix
#y_test <- df_test %>% pull(diagnosis) %>% as.matrix
#X_train<- df_train[,-1] %>% as.matrix
#X_test<- df_test[,-1] %>% as.matrix
#-------------------------------------------------------------------------------
# --------------------------------------------------Support Vector Machine (SVM)
#SVM_linear_abs <- svmmaj(X_train, y_train, hinge='absolute')
#SVM_linear_qua <- svmmaj(df_train, y_train, hinge='quadratic')
#SVM_RBF_abs <- svmmaj(df_train, y_train, hinge = 'absolute',kernel = rbfdot)
#test <- X_test %*% SVM_linear_abs$beta

SVM_RBF <- ksvm(diagnosis~ ., data = df_train, scale = FALSE, kernel = "rbfdot")
# predict
Eval_RBF <- predict(SVM_RBF, df_test)
Eval_RBF <- ifelse(Eval_RBF > 0, 1, -1)
Eval_RBF %>% as.factor
df.test <- as.matrix(df_test[,1])
# confusion matrix
confusionMatrix(Eval_RBF,df.test)
dim(df.test)
dim(Eval_RBF)
