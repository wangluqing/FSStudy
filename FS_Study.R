# 特征选择研究和应用
# 特征选择:从原始特征空间选择与目标相关的，有作用的特征子集。
# 从数据链条，认识特征选择价值和意义
# 数据获取的难易程度和成本
# 数据存储和处理的成本，金钱成本和时间成本
# 数据应用的便捷性和理解性
# 奥卡姆剃刀原理，如无必要，勿增实体。
# 约1285年至1349年在《箴言书注》2卷15题所说
# 切勿浪费较多东西去做用较少的东西同样可以做好的事情
# 简单有效原理 

# 特征选择认识
# 关键问题：
# 设计和构建一个预测模型，应该使用那些特征??
# 很有挑战性和难度的问题
# 解决好这个问题，需要这个问题领域很深的知识库。
# 特征选择所要解决的问题
# 1）研究的问题领域
# 2）自动化地从数据集中选择有用的或者相关的特征集

# 属于特征选择研究和应用的范畴

# 1 特征选择是什么？
# 特征选择又叫变量选择，属性选择
# 2 为什么要特征选择？
# 特征选择的目标3点
# 1）改进模型性能
# 2）节约成本
# 3）更好地理解
# 3 特征选择方法
# 1）Filter
# 概率论+统计论+信息论  Score
# 2) Wrapper
# 搜索论+组合论+度量论 Score
# 3) Embedded
# 构建论+度量论 

# 特征选择方法
# 移除冗余的特征集
# 采用相关性分析和度量
library(caret)
library(mlbench)
library(tidyverse)

# 加载数据集
data("PimaIndiansDiabetes")
PimaIndiansDiabetes %>% glimpse()

# 计算预测变量集的相关系数矩阵
corr_mat <- cor(PimaIndiansDiabetes[,1:8])
corr_mat

# 变量集相关系数高
highly_cor <- findCorrelation(corr_mat, cutoff = 0.5)
highly_cor

print(highly_cor)


# 特征重要性分析
control <- trainControl(method="repeatedcv", 
                        number=10, 
                        repeats=3)

# 使用LVQ算法（Learning Vector Quantization）模型
model <- train(diabetes~., 
               data=PimaIndiansDiabetes, 
               method="lvq", 
               preProcess="scale", 
               trControl=control)
# 特征重要性分析
importance <- varImp(model, scale=FALSE)
print(importance)
# 特征重要性可视化
plot(importance)

# RFE算法
library(randomForest)
control <- rfeControl(functions=rfFuncs, 
                      method="cv", 
                      number=10)

results <- rfe(PimaIndiansDiabetes[,1:8], 
               PimaIndiansDiabetes[,9], 
               sizes=c(1:8), 
               rfeControl=control)
print(results)
# 选择的变量集
predictors(results)
# 打印结果集
plot(results, type=c("g", "o"))







# test_df <- PimaIndiansDiabetes[,1:8]











# findCorrelation <- function(x, cutoff=.9, verbose=FALSE, names=FALSE) {
#   # x是要处理的数据集
#   # cutoff是相关系数的阈值
#   # verbose为TRUE时，会输出每一步操作
#   # names为TRUE时，输出结果为特征变量名
#   y <- 1 - cor(x)
#   n <- ncol(y)
#   y[diag(n)] <- Inf
#   absCor <- apply(y, 2, min)
#   if(verbose)
#     cat("  -- removed the following correlated variables:\n")
#   highCor <- which(absCor < cutoff)
#   while(length(highCor) > 0) {
#     place <- which.min(absCor)
#     temp <- paste(names(x)[highCor], collapse=", ")
#     if(verbose)
#       cat(sprintf("        %s %6.2f\n", temp, absCor[place]))
#     x <- x[, -highCor[place]]
#     y <- y[-highCor[place], -highCor[place]]
#     n <- ncol(y)
#     y[diag(n)] <- Inf
#     absCor <- apply(y, 2, min)
#     highCor <- which(absCor < cutoff)
#   }
#   if(!names) {
#     return(as.vector(colnames(x)))
#   } else {
#     return(x)
#   }
# }






























# 参考资料：
# 1 https://machinelearningmastery.com/an-introduction-to-feature-selection/
# 2 https://machinelearningmastery.com/feature-selection-with-the-caret-r-package/
# 3 https://blog.datascienceheroes.com/feature-selection-using-genetic-algorithms-in-r/
# 4 https://www.datacamp.com/tutorial/feature-selection-R-boruta
# 5 https://www.r-bloggers.com/2021/05/lasso-regression-model-with-r-code/

