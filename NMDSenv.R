setwd("C:/Users/xupeng/Desktop/HNDenv")
#清理环境
#install.packages("ggeffects")
rm(list=ls())
#加载R包
# 加载包

# 读取数据
env_data <- read.csv("SEM_env.csv")
index_data <- read.csv("SEM_index.csv")

# 合并数据
combined_data <- merge(env_data, index_data, by = "Site")

# 提取环境变量和生态指标
env_vars <- combined_data[, c("Kd", "Par", "Sst", "Sal", "Chl_a", "Pic", "Poc")]
index_vars <- combined_data[, c("Tc", "mTlc", "Tst", "Tb", "mTE", "SdI", "Sp", "TPP.TR", "FCI", "MPL", "CI", "SOI")]

# 计算环境因素与生态指标的相关性矩阵
cor_matrix <- cor(env_vars, index_vars)

# 创建相关性热图
library(ggplot2)
library(reshape2)

cor_melted <- melt(cor_matrix)
colnames(cor_melted) <- c("Environment", "Index", "Correlation")

# 绘制热图
ggplot(cor_melted, aes(x = Environment, y = Index, fill = Correlation)) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = round(Correlation, 2)), color = "black", size = 3) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "环境因素与生态指标的相关性热图",
       x = "环境因素",
       y = "生态指标")

# 计算每个环境因素的平均绝对相关性并绘制条形图
avg_abs_cor <- apply(abs(cor_matrix), 1, mean)
avg_abs_cor <- sort(avg_abs_cor, decreasing = TRUE)

avg_cor_df <- data.frame(
  Environment = names(avg_abs_cor),
  Avg_Abs_Correlation = avg_abs_cor
)

ggplot(avg_cor_df, aes(x = reorder(Environment, Avg_Abs_Correlation), y = Avg_Abs_Correlation)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
  coord_flip() +
  theme_minimal() +
  labs(title = "环境因素与生态指标的平均绝对相关性",
       x = "环境因素",
       y = "平均绝对相关性") +
  geom_text(aes(label = round(Avg_Abs_Correlation, 3)), hjust = -0.1, size = 3.5)

# 对每个生态指标进行多元回归分析并提取标准化系数
coef_list <- list()

for (i in 1:ncol(index_vars)) {
  index_name <- colnames(index_vars)[i]
  formula <- as.formula(paste(index_name, "~ Kd + Par + Sst + Sal + Chl_a + Pic + Poc"))
  model <- lm(formula, data = combined_data)
  
  # 提取标准化系数（去除截距）
  coef_df <- data.frame(
    Environment = names(coef(model))[-1],
    Coefficient = coef(model)[-1],
    Index = index_name
  )
  coef_list[[i]] <- coef_df
}

# 合并所有系数
all_coef <- do.call(rbind, coef_list)

# 计算每个环境因素的平均绝对系数
avg_abs_coef <- aggregate(abs(Coefficient) ~ Environment, data = all_coef, mean)
avg_abs_coef <- avg_abs_coef[order(-avg_abs_coef$`abs(Coefficient)`), ]

# 绘制平均绝对系数条形图
ggplot(avg_abs_coef, aes(x = reorder(Environment, `abs(Coefficient)`), y = `abs(Coefficient)`)) +
  geom_bar(stat = "identity", fill = "coral", alpha = 0.7) +
  coord_flip() +
  theme_minimal() +
  labs(title = "环境因素对生态指标的平均影响系数（绝对值）",
       x = "环境因素",
       y = "平均绝对回归系数") +
  geom_text(aes(label = round(`abs(Coefficient)`, 3)), hjust = -0.1, size = 3.5)

# 绘制系数热图（显示正负影响）
coef_matrix <- dcast(all_coef, Environment ~ Index, value.var = "Coefficient")
coef_matrix_melted <- melt(coef_matrix, id.vars = "Environment")

ggplot(coef_matrix_melted, aes(x = Environment, y = variable, fill = value)) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Coefficient") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "环境因素对生态指标的回归系数热图",
       x = "环境因素",
       y = "生态指标")

# 对环境因素进行PCA
pca_env <- prcomp(env_vars, scale. = TRUE)

# 提取主成分载荷
loadings <- pca_env$rotation[, 1:2]  # 前两个主成分

# 计算每个环境因素的平均绝对载荷
avg_abs_loading <- apply(abs(loadings), 1, mean)
avg_abs_loading <- sort(avg_abs_loading, decreasing = TRUE)

# 绘制环境因素重要性条形图
loading_df <- data.frame(
  Environment = names(avg_abs_loading),
  Avg_Abs_Loading = avg_abs_loading
)

ggplot(loading_df, aes(x = reorder(Environment, Avg_Abs_Loading), y = Avg_Abs_Loading)) +
  geom_bar(stat = "identity", fill = "purple", alpha = 0.7) +
  coord_flip() +
  theme_minimal() +
  labs(title = "环境因素在主成分中的平均绝对载荷",
       x = "环境因素",
       y = "平均绝对载荷") +
  geom_text(aes(label = round(Avg_Abs_Loading, 3)), hjust = -0.1, size = 3.5)

# 绘制PCA双标图
library(ggfortify)

autoplot(pca_env, data = combined_data, 
         loadings = TRUE, loadings.label = TRUE,
         loadings.label.size = 4, loadings.label.colour = 'blue',
         scale = 0) +
  theme_minimal() +
  labs(title = "环境因素的PCA双标图")

# 计算主成分与生态指标的相关性并绘制热图
pca_scores <- as.data.frame(pca_env$x)
pca_cor <- cor(pca_scores[, 1:2], index_vars)

pca_cor_melted <- melt(pca_cor)
colnames(pca_cor_melted) <- c("PC", "Index", "Correlation")

ggplot(pca_cor_melted, aes(x = PC, y = Index, fill = Correlation)) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = round(Correlation, 2)), color = "black", size = 3) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "PCA主成分与生态指标的相关性",
       x = "主成分",
       y = "生态指标")

# 创建综合比较图
# 准备数据
cor_df <- data.frame(
  Environment = names(avg_abs_cor),
  Correlation = avg_abs_cor,
  Method = "Correlation"
)

reg_df <- data.frame(
  Environment = avg_abs_coef$Environment,
  Value = avg_abs_coef$`abs(Coefficient)`,
  Method = "Regression"
)

pca_df <- data.frame(
  Environment = names(avg_abs_loading),
  Value = avg_abs_loading,
  Method = "PCA"
)

# 标准化值
cor_df$Std_Value <- cor_df$Correlation / max(cor_df$Correlation)
reg_df$Std_Value <- reg_df$Value / max(reg_df$Value)
pca_df$Std_Value <- pca_df$Value / max(pca_df$Value)

# 合并数据
combined_methods <- rbind(cor_df[, c("Environment", "Std_Value", "Method")],
                          reg_df[, c("Environment", "Std_Value", "Method")],
                          pca_df[, c("Environment", "Std_Value", "Method")])

# 绘制综合比较图
ggplot(combined_methods, aes(x = Environment, y = Std_Value, fill = Method)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "不同方法评估的环境因素重要性比较",
       x = "环境因素",
       y = "标准化重要性得分") +
  scale_fill_brewer(palette = "Set1")

# 计算综合得分
composite_score <- aggregate(Std_Value ~ Environment, data = combined_methods, mean)
composite_score <- composite_score[order(-composite_score$Std_Value), ]

# 绘制综合得分条形图
ggplot(composite_score, aes(x = reorder(Environment, Std_Value), y = Std_Value)) +
  geom_bar(stat = "identity", fill = "darkgreen", alpha = 0.7) +
  coord_flip() +
  theme_minimal() +
  labs(title = "环境因素对生态指标影响的综合评分",
       x = "环境因素",
       y = "综合重要性得分") +
  geom_text(aes(label = round(Std_Value, 3)), hjust = -0.1, size = 3.5)
