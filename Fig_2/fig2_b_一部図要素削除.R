setwd("/Users/gizen/Documents/AF/20250810/")
library(MatchIt)
library(openxlsx)
library(tableone)
library(Maaslin2)
library(ComplexHeatmap)
library(circlize)
library(tidyverse)
library(reshape2)
library(viridis)
library(ggpubr)



data=as.data.frame(read.csv("./7cohort_genus_MAA_20250810.csv"))
data=data[data$metadata %in% c("AF"),]
data$metadata=data$cate

adddata=as.data.frame(read.csv("./7cohort_genus_singlefactor_20250806.csv"))
adddata=adddata[!adddata$metadata %in% c("AF","Non_PAF","PAF","IHD","CHF","PAD","Stroke"),]
adddata=adddata[,c(-3,-7)]
data=rbind(data,adddata)
data$metadata=gsub("Age","Age>=65",data$metadata)
data$metadata=gsub("BMI","BMI>=25",data$metadata)
data$metadata=gsub("newHTN","Hypertension",data$metadata)
data$metadata=gsub("cur.ever.smoking","Smoking",data$metadata)

datakey=unique(data[(data$qval<0.1)&(abs(data$coef)>0.25),]$feature)

coef=as.data.frame(pivot_wider(data,id_cols = feature,names_from = metadata,values_from = coef))
pval=as.data.frame(pivot_wider(data,id_cols = feature,names_from = metadata,values_from = pval))
qval=as.data.frame(pivot_wider(data,id_cols = feature,names_from = metadata,values_from = qval))

rownames(coef)=coef$feature
rownames(pval)=pval$feature
rownames(qval)=qval$feature


coef=coef[,-1,drop=F]
pval=pval[,-1,drop=F]
qval=qval[,-1,drop=F]


coef=coef[datakey,,drop=F]
pval=pval[datakey,,drop=F]
qval=qval[datakey,,drop=F]
data=data[data$feature %in% datakey,]
col_fun=colorRamp2(c(-(max(coef,na.rm = T)/4),0,(max(coef,na.rm = T)/4)), c("skyblue", "white", "pink"))

pval=pval[order(coef[,1]),]
qval=qval[order(coef[,1]),]
coef=coef[order(coef[,1]),]
orderkey=c(
           "Age>=65",
           "sex",
           "BMI>=25",
           "Alcohol",
           "Smoking",
           "Diabetes.mellitus",
           "Dyslipidemia",
           "Hypertension",
           "Antibiotics",
           "Proton.pump.inhibitors",
           "AFvsHealth",
           "PSM",
           "5137"
           
)
pval=pval[,orderkey]
qval=qval[,orderkey]
coef=coef[,orderkey]

beta_results <- readRDS("beta_diversity_10factor_results.rds")
names(beta_results)=gsub("Age","Age>=65",names(beta_results))
names(beta_results)=gsub("BMI","BMI>=25",names(beta_results))
names(beta_results)=gsub("newHTN","Hypertension",names(beta_results))
names(beta_results)=gsub("cur.ever.smoking","Smoking",names(beta_results))
vars <- c(
  "Age>=65", "sex", "BMI>=25", "Hypertension", "Smoking",
  "Diabetes.mellitus", "Dyslipidemia", "Alcohol","Antibiotics",
  "Proton.pump.inhibitors",
  "5137", "PSM", "AFvsHealth"
)

# 前 10 个变量来自 beta_results，后 3 个直接设为 0
r2_values <- c(
  sapply(vars[1:10], function(v) beta_results[[v]]$adonis$R2[1]),
  rep(0, 3)
)

beta_r2_df <- data.frame(
  Variable = vars,
  R2 = r2_values,
  row.names = NULL
)
# 
# ggplot(beta_r2_df, aes(x = Variable, y = R2)) +
#   geom_bar(stat = "identity", fill = "steelblue") +
#   coord_flip() +  # 横向
#   theme_minimal(base_size = 14) +
#   labs(x = "Variable", y = "R²", title = "Adonis R² per Variable") +
#   theme(
#     axis.text.y = element_text(face = "bold"),
#     plot.title = element_text(hjust = 0.5, face = "bold")
#   )
mat_r2 <- as.matrix(beta_r2_df$R2)
rownames(mat_r2) <- beta_r2_df$Variable
rowha <- HeatmapAnnotation(
  "R^2" = anno_barplot(
    as.matrix(beta_r2_df$R2),  # 只取数值列
    gp = gpar(fill = "gray")
  ),
  annotation_name_rot = 0,
  annotation_name_side = "right",
  gap = unit(2, "mm")
)

p1=Heatmap(as.matrix((coef)),
           col = col_fun,
           show_heatmap_legend = TRUE,
           top_annotation = rowha,
           # right_annotation = rowha2,
           row_names_gp = gpar(fontsize=10, fontface="bold"),
           column_names_gp = gpar(fontsize=10, fontface="bold"),
           show_column_names = TRUE,show_row_names = TRUE,
           height = ncol(as.matrix(t(coef)))*unit(3, "mm"),
           width = nrow(as.matrix(t(coef)))*unit(3, "mm"),column_split = factor(c(rep("metadata",10),rep("group",3)),levels = c("metadata","group")),
           row_title_side = "left",
           row_dend_side = "left",
           row_names_side = "left",
           cluster_rows = T,
           cluster_columns = F,
           name = "trt",
           column_names_rot = 90,
           heatmap_legend_param = list(title = "MAA coef\n* FDR < 0.1\n** FDR < 0.05" ),
           cell_fun = function(j, i, x, y, width, height, fill) {
             if(!is.na((qval)[i, j])){if((qval)[i, j] < 0.1) {if((qval)[i, j]<0.05){grid.text("**", x, y, vjust = 0.8)}else(grid.text("*", x, y, vjust = 0.8))}}
           })  


pdf("motu_MAA_AF_5137PSMHealth_fdr01.pdf", width = 8.27, height = 11.69)
draw(p1,column_title="MAA genus 5137+PSM+Health+10 factor abs coef >0.25 FDR < 0.1")
dev.off()











