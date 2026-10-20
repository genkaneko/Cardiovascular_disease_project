# setwd("/Users/gizen/Documents/AF/20250813/")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())

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
library(dplyr)
library(stringr)

data=as.data.frame(read.csv("oral_7cohort_motusp_MAA_20250810.csv"))
data=data[data$cate=="5137",]
data=data[data$metadata %in% c("AF","IHD","CHF" ),]
data <- data %>%
  mutate(
    id = ifelse(
      str_detect(feature, "\\[ID:[^\\]]+\\]"),
      str_extract(feature, "(?<=\\[ID:)[^\\]]+"),
      feature  # 匹配不到则保留原值
    )
  )

fdata=as.data.frame(read.csv("7cohort_motusp_MAA_20250810.csv"))
fdata=fdata[fdata$metadata %in% c("AF","IHD","CHF" ),]
fdata=fdata[fdata$cate=="5137",]
fdata <- fdata %>%
  mutate(
    id = ifelse(
      str_detect(feature, "\\[ID:[^\\]]+\\]"),
      str_extract(feature, "(?<=\\[ID:)[^\\]]+"),
      feature  # 匹配不到则保留原值
    )
  )



datakey=unique(data[(data$pval<0.05),]$feature)


data$metadata=paste0("oral_",data$metadata)
fdata$metadata=paste0("fecal_",fdata$metadata)


fdata$feature=fdata$old
fdata=fdata[,-11]
fdata=fdata[fdata$id %in% data$id,]
data=rbind(data,fdata)


oldtonew=as.data.frame(read.xlsx("菌.xlsx"))
oldtonew=oldtonew[!is.na(oldtonew$old),]
rownames(oldtonew)=oldtonew$old

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
# col_fun=colorRamp2(c(-(max(coef,na.rm = T)/2),0,(max(coef,na.rm = T)/2)), c("blue", "white", "red"))

pval=pval[order(coef[,1]),]
qval=qval[order(coef[,1]),]
coef=coef[order(coef[,1]),]
key=c(1,4,2,5,3,6)

pval=pval[,key]
qval=qval[,key]
coef=coef[,key]


rownames(coef)=oldtonew[rownames(coef),]$new
library(ComplexHeatmap)
library(circlize)

# 定义两种配色
col_fun_bluered <- colorRamp2(
  c(-(max(coef, na.rm = TRUE)), 0, (max(coef, na.rm = TRUE) )),
  c("navyblue", "white", "salmon3")
)
col_fun_greenorange <- colorRamp2(
  c(-(max(coef, na.rm = TRUE)), 0, (max(coef, na.rm = TRUE) )),
  c("blue", "white", "red")
)
# col_fun_greenorange <- colorRamp2(
#   c(-(max(coef, na.rm = TRUE)),-(max(coef, na.rm = TRUE))/2, 0,(max(coef, na.rm = TRUE))/2, (max(coef, na.rm = TRUE) )),
#   c("#08306B", "#4292C6", "#FFFFFF", "#FB6A4A", "#67000D")
# )
# 每列的配色规则
color_map_list <- list(
  col_fun_greenorange,
  col_fun_bluered,
  col_fun_greenorange,
  col_fun_bluered,
  col_fun_greenorange,
  col_fun_bluered
)

# 创建 Heatmap 列表
ht_list <- list()

for (k in 1:6) {
  this_col_fun <- color_map_list[[k]]
  
  ht_list[[k]] <- Heatmap(
    as.matrix(coef[, k, drop = FALSE]),
    col = this_col_fun,
    cluster_rows = FALSE,
    cluster_columns = FALSE,row_names_side = "left" ,
    row_names_gp = gpar(fontsize = 10, fontface = "bold") ,
    column_names_gp = gpar(fontsize = 10, fontface = "bold"),
    column_names_rot = 90,
    height = ncol(as.matrix(t(coef)))*unit(3, "mm"),
    width = 1*unit(3, "mm"),
    name = paste0("trt", k),
    heatmap_legend_param = list(title = "MAA coef\n* P < 0.05\n** FDR < 0.1"),
    cell_fun = function(j, i, x, y, width, height, fill) {
      if (!is.na(pval[i, k])) {
        if (pval[i, k] < 0.05) {
          if (qval[i, k] < 0.1) {
            grid.text("**", x, y, vjust = 0.8)
          } else {
            grid.text("*", x, y, vjust = 0.8)
          }
        }
      }
    }
  )
}

# 按顺序拼接
final_ht <- ht_list[[1]] + ht_list[[2]] + ht_list[[3]] +
  ht_list[[4]] + ht_list[[5]] + ht_list[[6]]

draw(final_ht)


pdf("oral_motu_MAA_5137_5cohort_fdr01_new.pdf",width = 30,height = 20)
draw(final_ht,column_title="5137 MAA 5cohort oral fdr < 0.1",gap = unit(c(0,1,0,1,0), "mm"))
dev.off()








# 
# 
# rownames(coef)=paste0(rownames(coef),"_", oldtonew[rownames(coef),]$old)
# p1=Heatmap(as.matrix(t(coef)),
#            col = col_fun,
#            show_heatmap_legend = TRUE,
#            # top_annotation = rowha,
#            # right_annotation = rowha2,
#            row_names_gp = gpar(fontsize=10, fontface="bold"),
#            column_names_gp = gpar(fontsize=10, fontface="bold"),
#            show_column_names = TRUE,show_row_names = TRUE,
#            height = ncol(as.matrix((coef)))*unit(3, "mm"),
#            width = nrow(as.matrix((coef)))*unit(3, "mm"),
#            row_title_side = "left",
#            row_dend_side = "left",
#            row_names_side = "left",
#            cluster_rows = F,
#            cluster_columns = F,
#            name = "trt",
#            column_names_rot = 90,
#            heatmap_legend_param = list(title = "MAA coef\n* P < 0.05\n** FDR < 0.1" ),
#            cell_fun = function(j, i, x, y, width, height, fill) {
#              if(!is.na(t(pval)[i, j])){if(t(pval)[i, j] < 0.05) {if(t(qval)[i, j]<0.1){grid.text("**", x, y, vjust = 0.8)}else(grid.text("*", x, y, vjust = 0.8))}}
#            })  
# 
# 
# pdf("motu_MAA_5137_5cohort_fdr01.pdf",width = 30,height = 20)
# draw(p1,column_title="5137 MAA 5cohort oral pval < 0.1")
# dev.off()
# 



