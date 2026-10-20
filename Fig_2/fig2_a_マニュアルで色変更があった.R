setwd("/Users/gizen/Documents/AF/20250810/")
library(ggplot2)
library(dplyr)

# 读取beta diversity结果
beta_results <- readRDS("beta_diversity_10factor_results.rds")
names(beta_results)=gsub("Age","Age>=65",names(beta_results))
names(beta_results)=gsub("BMI","BMI>=25",names(beta_results))
names(beta_results)=gsub("newHTN","Hypertension",names(beta_results))
names(beta_results)=gsub("cur.ever.smoking","Smoking",names(beta_results))

# 只保留metadata变量（前10个）
vars <- c(
  "Age>=65", "sex", "BMI>=25", "Hypertension", "Smoking",
  "Diabetes.mellitus", "Dyslipidemia", "Alcohol","Antibiotics",
  "Proton.pump.inhibitors"
)

# 提取R²值
r2_values <- sapply(vars, function(v) beta_results[[v]]$adonis$R2[1])

beta_r2_df <- data.frame(
  Variable = vars,
  R2 = r2_values,
  row.names = NULL
)

# 创建垂直柱状图
beta_r2_df$Variable <- factor(beta_r2_df$Variable, levels = vars)

p_barplot <- ggplot(beta_r2_df, aes(x = Variable, y = R2)) +
  geom_bar(stat = "identity", fill = "gray", width = 0.7) +
  theme_minimal() +
  labs(x = "", y = expression(R^2), title = "Adonis R² per Variable") +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10, angle = 90, hjust = 1, vjust = 0.5),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    plot.title = element_text(size = 10, hjust = 0.5),
    panel.grid.minor = element_blank()
  )

# 保存柱状图
ggsave("adonis_r2_barplot.svg", 
       plot = p_barplot, 
       width = 2, height = 3, 
       units = "in",
       device = "svg")