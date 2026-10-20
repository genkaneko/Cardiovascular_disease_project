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

data_all=as.data.frame(read.csv("7cohort_motusp_MAA_20250810.csv"))
data_all=data_all[data_all$metadata %in% c("AF","Non_PAF","PAF","IHD","CHF"),]
data_all=data_all[data_all$cate=="5137",]
data_all$feature=gsub("\\[ID:(\\d+)\\]", "\\1", data_all$feature)

check=read.xlsx("../特異菌株.xlsx")
check$`Bacterial.species.(GTDB.taxonomy)`=gsub("\\[ID:(\\d+)\\]", "\\1", check$`Bacterial.species.(GTDB.taxonomy)`)

data_all=data_all[data_all$feature %in% check$`Bacterial.species.(GTDB.taxonomy)`,]

col_fun <- colorRamp2(
  c(-(max(data_all$coef, na.rm = TRUE)), 0, (max(data_all$coef, na.rm = TRUE) )),
  c("navyblue", "white", "salmon3")
)
# datakey=unique(data[(data$qval<0.1)&(abs(data$coef)>=0.1)&(data$metadata %in% c("AF","IHD","CHF")),]$feature)
# temp=table(data[(data$qval<0.1)&(abs(data$coef)>=0.1)&(data$metadata %in% c("AF","IHD","CHF")),]$feature)
# 3 coef 0.1 fdr0.1 44 ok
# 5 coef 0.1 fdr0.1 51 
# 5 coef 0.075 fdr0.1 95
# 3 fdr0.1 178
# 3 coef 0.075 fdr0.1 83
# 1 fdr0.1 56

# 
# 
# oldtonew=as.data.frame(read.csv("/Users/gizen/Documents/smoking/motu3/菌.csv"))
# oldtonew=oldtonew[!is.na(oldtonew$old),]
# rownames(oldtonew)=oldtonew$new
# oldtonew$new=gsub("\\[ID:(\\d+)\\]", "\\1", oldtonew$new)
# oldtonew$old=gsub("\\[ID:(\\d+)\\]", "\\1", oldtonew$old)

###common
data=data_all[data_all$feature %in% check[check$Disease=="Common",]$`Bacterial.species.(GTDB.taxonomy)`,]
datakey=data[data$metadata=="AF",]$feature[order(data[data$metadata=="AF",]$coef,decreasing = T)]
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


p1=Heatmap(as.matrix((coef)),
           col = col_fun,
           show_heatmap_legend = TRUE,
           # top_annotation = rowha,
           # right_annotation = rowha2,
           row_names_gp = gpar(fontsize=10, fontface="bold"),
           column_names_gp = gpar(fontsize=10, fontface="bold"),
           show_column_names = TRUE,show_row_names = TRUE,
           height = ncol(as.matrix(t(coef)))*unit(3, "mm"),
           width = nrow(as.matrix(t(coef)))*unit(3, "mm"),
           row_title_side = "left",
           row_dend_side = "left",
           row_names_side = "left",
           cluster_rows = F,
           cluster_columns = F,
           name = "Common",
           column_names_rot = 90,
           heatmap_legend_param = list(title = "MAA coef\n* FDR < 0.1\n** FDR < 0.05" ),
           cell_fun = function(j, i, x, y, width, height, fill) {
             if(!is.na((qval)[i, j])){if((qval)[i, j] < 0.1) {if((qval)[i, j]<0.05){grid.text("**", x, y, vjust = 0.8)}else(grid.text("*", x, y, vjust = 0.8))}}
           })  



####AF-sp
data=data_all[data_all$feature %in% check[check$Disease=="AF_specific",]$`Bacterial.species.(GTDB.taxonomy)`,]
datakey=data[data$metadata=="AF",]$feature[order(data[data$metadata=="AF",]$coef,decreasing = T)]
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

p2=Heatmap(as.matrix((coef)),
           col = col_fun,
           show_heatmap_legend = TRUE,
           # top_annotation = rowha,
           # right_annotation = rowha2,
           row_names_gp = gpar(fontsize=10, fontface="bold"),
           column_names_gp = gpar(fontsize=10, fontface="bold"),
           show_column_names = TRUE,show_row_names = TRUE,
           height = ncol(as.matrix(t(coef)))*unit(3, "mm"),
           width = nrow(as.matrix(t(coef)))*unit(3, "mm"),
           row_title_side = "left",
           row_dend_side = "left",
           row_names_side = "left",
           cluster_rows = F,
           cluster_columns = F,
           name = "AF",
           column_names_rot = 90,
           heatmap_legend_param = list(title = "MAA coef\n* FDR < 0.1\n** FDR < 0.05" ),
           cell_fun = function(j, i, x, y, width, height, fill) {
             if(!is.na((qval)[i, j])){if((qval)[i, j] < 0.1) {if((qval)[i, j]<0.05){grid.text("**", x, y, vjust = 0.8)}else(grid.text("*", x, y, vjust = 0.8))}}
           })  




####IHD-sp
data=data_all[data_all$feature %in% check[check$Disease=="IHD_specific",]$`Bacterial.species.(GTDB.taxonomy)`,]
datakey=data[data$metadata=="AF",]$feature[order(data[data$metadata=="IHD",]$coef,decreasing = T)]
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

p3=Heatmap(as.matrix((coef)),
           col = col_fun,
           show_heatmap_legend = TRUE,
           # top_annotation = rowha,
           # right_annotation = rowha2,
           row_names_gp = gpar(fontsize=10, fontface="bold"),
           column_names_gp = gpar(fontsize=10, fontface="bold"),
           show_column_names = TRUE,show_row_names = TRUE,
           height = ncol(as.matrix(t(coef)))*unit(3, "mm"),
           width = nrow(as.matrix(t(coef)))*unit(3, "mm"),
           row_title_side = "left",
           row_dend_side = "left",
           row_names_side = "left",
           cluster_rows = F,
           cluster_columns = F,
           name = "IHD",
           column_names_rot = 90,
           heatmap_legend_param = list(title = "MAA coef\n* FDR < 0.1\n** FDR < 0.05" ),
           cell_fun = function(j, i, x, y, width, height, fill) {
             if(!is.na((qval)[i, j])){if((qval)[i, j] < 0.1) {if((qval)[i, j]<0.05){grid.text("**", x, y, vjust = 0.8)}else(grid.text("*", x, y, vjust = 0.8))}}
           })  





####CHF-sp
data=data_all[data_all$feature %in% check[check$Disease=="HF_specific",]$`Bacterial.species.(GTDB.taxonomy)`,]
datakey=data[data$metadata=="AF",]$feature[order(data[data$metadata=="CHF",]$coef,decreasing = T)]
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

p4=Heatmap(as.matrix((coef)),
           col = col_fun,
           show_heatmap_legend = TRUE,
           # top_annotation = rowha,
           # right_annotation = rowha2,
           row_names_gp = gpar(fontsize=10, fontface="bold"),
           column_names_gp = gpar(fontsize=10, fontface="bold"),
           show_column_names = TRUE,show_row_names = TRUE,
           height = ncol(as.matrix(t(coef)))*unit(3, "mm"),
           width = nrow(as.matrix(t(coef)))*unit(3, "mm"),
           row_title_side = "left",
           row_dend_side = "left",
           row_names_side = "left",
           cluster_rows = F,
           cluster_columns = F,
           name = "HF",
           column_names_rot = 90,
           heatmap_legend_param = list(title = "MAA coef\n* FDR < 0.1\n** FDR < 0.05" ),
           cell_fun = function(j, i, x, y, width, height, fill) {
             if(!is.na((qval)[i, j])){if((qval)[i, j] < 0.1) {if((qval)[i, j]<0.05){grid.text("**", x, y, vjust = 0.8)}else(grid.text("*", x, y, vjust = 0.8))}}
           })  






pdf("motu_MAA_5137_5disease_AFfdr01_new.pdf",width = 30,height = 20)
draw(p1 %v% p2 %v% p3 %v% p4,column_title="5137 MAA AF fdr < 0.1")
dev.off()












# 
# 
# rownames(coef)=paste0(rownames(coef),"_", oldtonew[rownames(coef),]$old)
# p1=Heatmap(as.matrix((coef)),
#            col = col_fun,
#            show_heatmap_legend = TRUE,
#            # top_annotation = rowha,
#            # right_annotation = rowha2,
#            row_names_gp = gpar(fontsize=10, fontface="bold"),
#            column_names_gp = gpar(fontsize=10, fontface="bold"),
#            show_column_names = TRUE,show_row_names = TRUE,
#            height = ncol(as.matrix(t(coef)))*unit(3, "mm"),
#            width = nrow(as.matrix(t(coef)))*unit(3, "mm"),
#            row_title_side = "left",
#            row_dend_side = "left",
#            row_names_side = "left",
#            cluster_rows = F,
#            cluster_columns = F,
#            name = "trt",
#            column_names_rot = 90,
#            heatmap_legend_param = list(title = "MAA coef\n* FDR < 0.1\n** FDR < 0.05" ),
#            cell_fun = function(j, i, x, y, width, height, fill) {
#              if(!is.na((qval)[i, j])){if((qval)[i, j] < 0.1) {if((qval)[i, j]<0.05){grid.text("**", x, y, vjust = 0.8)}else(grid.text("*", x, y, vjust = 0.8))}}
#            })  
# 
# 
# pdf("motu_MAA_5137_5disease_AFfdr01coefabs01.pdf",width = 30,height = 20)
# draw(p1,column_title="5137 MAA AF fdr < 0.1")
# dev.off()
# 
# 


