# setwd("/Users/gizen/Documents/AF/fig_cytokine_box")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())

library(ggplot2)
library(tidyverse)
library(ComplexHeatmap)
library(circlize)
library(grid)

theme_set(theme_bw(base_family = "Helvetica"))

maa=read.csv("7cohort_motusp_MAA_20250810.csv")
# maa=read.csv("../20250810/7cohort_motusp_MAA_20250810.csv")
maa=maa[maa$cate=="5137",]
key=c("AF","IHD","CHF")
maa=maa[maa$metadata %in% key,]

df=NULL
cytokine=read.csv("cyt_MB_motu_Oral_spearman_260430.csv")
# cytokine=read.csv("../cytokine20260428/cyt_MB_motu_Oral_spearman_260430.csv")
cyt=c(
  "TNF_alpha",
  "IL8_CXCL8",
  "CCL2",
  "CXCL9",
  "IP10_CXCL10",
  "CXCL11"
)
cytokine_order=cyt
disease_order=key
cytokine=cytokine[cytokine$cyto_or_metab %in% cyt,]
r=NULL
rlist=NULL

for ( i in cyt){
  for (j in key){
    tempmaa=maa[maa$metadata==j,]
    tempcyto=cytokine[cytokine$cyto_or_metab==i,]
    tempmaa=tempmaa[tempmaa$qval<0.1,]
    tempcyto=tempcyto[tempcyto$motu %in% tempmaa$feature,]
    tempcyto=tempcyto[tempcyto$p_v1<0.05,]
    
    tempmaaup=tempmaa[tempmaa$coef>0,]
    tempmaadown=tempmaa[tempmaa$coef<0,]
    tempr1=data.frame(
      Cytokine=i,
      Disease=j,
      n_up=nrow(tempcyto[((tempcyto$rho_v1<0)&(tempcyto$motu %in% tempmaaup$feature))|((tempcyto$rho_v1>0)&(tempcyto$motu %in% tempmaadown$feature)),]),
      n_down=nrow(tempmaa),
      ratio=nrow(tempcyto[((tempcyto$rho_v1<0)&(tempcyto$motu %in% tempmaaup$feature))|((tempcyto$rho_v1>0)&(tempcyto$motu %in% tempmaadown$feature)),])/nrow(tempmaa),
      group="Inflammation_opposed_taxa"
    )
    if(nrow(tempcyto[((tempcyto$rho_v1<0)&(tempcyto$motu %in% tempmaaup$feature))|((tempcyto$rho_v1>0)&(tempcyto$motu %in% tempmaadown$feature)),])==0){
      rlist_tempr1=NULL
    }else{
      
      rlist_tempr1=data.frame(
        motu=tempcyto[((tempcyto$rho_v1<0)&(tempcyto$motu %in% tempmaaup$feature))|((tempcyto$rho_v1>0)&(tempcyto$motu %in% tempmaadown$feature)),]$motu,
        group="Inflammation_opposed_taxa",
        disease=j,
        cytokine=i
      )
    }
    tempr2=data.frame(
      Cytokine=i,
      Disease=j,
      n_up=nrow(tempcyto[((tempcyto$rho_v1>0)&(tempcyto$motu %in% tempmaaup$feature))|((tempcyto$rho_v1<0)&(tempcyto$motu %in% tempmaadown$feature)),]),
      n_down=nrow(tempmaa),
      ratio=nrow(tempcyto[((tempcyto$rho_v1>0)&(tempcyto$motu %in% tempmaaup$feature))|((tempcyto$rho_v1<0)&(tempcyto$motu %in% tempmaadown$feature)),])/nrow(tempmaa),
      group="Inflammation_aligned_taxa"
    )
    if(nrow(tempcyto[((tempcyto$rho_v1>0)&(tempcyto$motu %in% tempmaaup$feature))|((tempcyto$rho_v1<0)&(tempcyto$motu %in% tempmaadown$feature)),])==0){
      rlist_tempr2=NULL
    }else{
      rlist_tempr2=data.frame(
        motu=tempcyto[((tempcyto$rho_v1>0)&(tempcyto$motu %in% tempmaaup$feature))|((tempcyto$rho_v1<0)&(tempcyto$motu %in% tempmaadown$feature)),]$motu,
        group="Inflammation_aligned_taxa",
        disease=j,
        cytokine=i
      )
    }
    tempr3=data.frame(
      Cytokine=i,
      Disease=j,
      n_up=nrow(tempcyto),
      n_down=nrow(tempmaa),
      ratio=nrow(tempcyto)/nrow(tempmaa),
      group="Cytokine_sig_taxa"
    )
    r=rbind(r,tempr1,tempr2,tempr3)
    rlist=rbind(rlist,rlist_tempr1,rlist_tempr2)
  }
}

rownames(maa)=paste0(maa$feature,maa$metadata)
rlist$coef=maa[paste0(rlist$motu,rlist$disease),]$coef

cols <- c(
  "Inflammation_opposed_taxa" = "#74AFC7",  # 水蓝
  "Inflammation_aligned_taxa" = "#F06292",  # 桃粉
  "Cytokine_sig_taxa"         = "#BDBDBD"   # 灰
)



# ===== 字号 =====
pt_size <- 5
gg_size <- pt_size / 2.845



df_plot <- r %>%
  mutate(
    group = factor(group,
                   levels = c("Inflammation_opposed_taxa",
                              "Inflammation_aligned_taxa",
                              "Cytokine_sig_taxa")),
    
    label = paste0(n_up, "/", n_down, "\n(",
                   sprintf("%.1f", ratio * 100), "%)")
  )

######################################################
## Fisher test
## 只比较 opposed 和 aligned
######################################################

fisher_df <- df_plot %>%
  filter(group %in% c("Inflammation_opposed_taxa",
                      "Inflammation_aligned_taxa")) %>%
  select(Cytokine, Disease, group, n_up, n_down, ratio) %>%
  tidyr::pivot_wider(
    names_from = group,
    values_from = c(n_up, n_down, ratio)
  ) %>%
  rowwise() %>%
  mutate(
    fisher_p = fisher.test(
      matrix(
        c(
          n_up_Inflammation_opposed_taxa,
          n_down_Inflammation_opposed_taxa - n_up_Inflammation_opposed_taxa,
          n_up_Inflammation_aligned_taxa,
          n_down_Inflammation_aligned_taxa - n_up_Inflammation_aligned_taxa
        ),
        nrow = 2,
        byrow = TRUE
      )
    )$p.value,
    
    fisher_label = paste0(
      "P = ",
      ifelse(fisher_p < 0.001, "<0.001", sprintf("%.3f", fisher_p))
    )
  ) %>%
  ungroup()

######################################################
## facet title に Fisher 結果を追加
######################################################

df_plot <- df_plot %>%
  left_join(
    fisher_df %>%
      select(Cytokine, Disease, fisher_label),
    by = c("Cytokine", "Disease")
  )

df_plot$Cytokine <- factor(df_plot$Cytokine, levels = cytokine_order)
df_plot$Disease  <- factor(df_plot$Disease,  levels = disease_order)

fisher_label_df <- df_plot %>%
  distinct(Cytokine, Disease, fisher_label)

# ===== 作图 =====
p <- ggplot(df_plot, aes(x = group, y = ratio, fill = group)) +
  
  geom_col(width = 0.6, color = "black") +
  
  geom_text(aes(label = label),
            vjust = -0.3,
            size = gg_size) +
  
  geom_text(
    data = fisher_label_df,
    aes(x = -Inf, y = Inf, label = fisher_label),
    inherit.aes = FALSE,
    hjust = -0.05,
    vjust = 1.3,
    size = gg_size
  )+
  
  scale_fill_manual(values = cols) +
  
  facet_grid(
    rows = vars(Cytokine),
    cols = vars(Disease),
    switch = "y",
    scales = "free_y"
  ) +
  
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.5))
  ) +
  
  labs(x = NULL, y = "Ratio") +
  
  theme_bw(base_size = pt_size) +
  theme(
    text = element_text(size = pt_size),
    strip.text = element_text(size = pt_size),
    axis.text = element_text(size = pt_size),
    axis.title = element_text(size = pt_size),
    
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    
    strip.placement = "outside",
    strip.background = element_rect(fill = "white", color = "black"),
    
    legend.position = "none"
  )

# ===== 保存 PDF =====
ggsave("cytokine_barplot_fisher.pdf",
       plot = p,
       width = 2.75, height = 4.5)   

library(grid)
par(family = "Helvetica")

######################################################
## 0. setup
######################################################
library(dplyr)
library(grid)
library(ComplexHeatmap)
library(circlize)

par(family = "Helvetica")

######################################################
## 0. setup
######################################################

fs <- 5

# 这里可以手动指定顺序
# disease_order <- c("AF", "IHD", "CHF")

# 或者自动使用 rlist 中已有顺序
disease_order <- unique(rlist$disease)

# ===== cytokine数据统一字段 =====
cytokine2 <- cytokine %>%
  filter(cyto_or_metab %in% cytokine_order) %>%
  rename(
    cytokine = cyto_or_metab,
    rho = rho_v1,
    p = p_v1
  )

# ===== 主heatmap颜色函数 =====
col_fun <- colorRamp2(
  c(-0.1, 0, 0.1),
  c("skyblue", "white", "pink")
)

# ===== disease annotation颜色 =====
anno_col <- c(
  "maaNegative" = "#74AFC7",
  "maaPositive" = "#F06292"
)

######################################################
## 1. 每个疾病分别输出 heatmap
######################################################

for (dis in disease_order) {
  set.seed(123)
  ####################################################
  ## 1-1. 当前疾病对应的 rlist
  ####################################################
  
  rlist_use <- rlist %>%
    filter(
      disease == dis,
      group == "Inflammation_aligned_taxa"
    ) %>%
    mutate(
      maa_group = ifelse(coef > 0, "maaPositive", "maaNegative")
    )
  
  # 当前疾病对应的菌顺序
  motu_order <- unique(rlist_use$motu)
  
  ####################################################
  ## 1-2. 构建 rho matrix
  ##      行：motu
  ##      列：6个 cytokine
  ####################################################
  
  mat <- matrix(
    NA,
    nrow = length(motu_order),
    ncol = length(cytokine_order)
  )
  
  rownames(mat) <- motu_order
  colnames(mat) <- cytokine_order
  
  mat_p <- mat
  
  for (cyto in cytokine_order) {
    
    sub <- cytokine2 %>%
      filter(
        cytokine == cyto,
        motu %in% motu_order
      )
    
    mat[sub$motu, cyto] <- sub$rho
    mat_p[sub$motu, cyto] <- sub$p
  }
  
  ####################################################
  ## 1-3. 左侧 disease square annotation
  ##      每个 motu 一格，根据 coef 正负上色
  ####################################################
  
  anno_df <- rlist_use %>%
    select(motu, maa_group) %>%
    distinct(motu, .keep_all = TRUE)
  
  anno_vec <- anno_df$maa_group
  names(anno_vec) <- anno_df$motu
  
  anno_mat <- matrix(
    anno_vec[motu_order],
    ncol = 1
  )
  
  rownames(anno_mat) <- motu_order
  colnames(anno_mat) <- dis
  
  ha <- rowAnnotation(
    df = data.frame(
      disease_coef = anno_vec[motu_order]
    ),
    
    col = list(
      disease_coef = anno_col
    ),
    
    simple_anno_size = unit(2.5, "mm"),
    
    show_annotation_name = TRUE,
    annotation_name_gp = gpar(fontsize = fs),
    annotation_name_rot = 90
  )
  
  names(ha@anno_list) <- dis
  
  ####################################################
  ## 1-4. 主 heatmap：6个 cytokine 合并在一个图里
  ####################################################
  
  ht <- Heatmap(
    mat,
    name = "rho",
    col = col_fun,
    
    cluster_rows = T,
    cluster_columns = FALSE,right_annotation = ha,
      
    show_row_names = TRUE,
    show_column_names = TRUE,
    
    row_names_gp = gpar(fontsize = fs),
    column_names_gp = gpar(fontsize = fs),
    
    column_title = dis,
    column_title_gp = gpar(fontsize = fs),
    
    width = unit(2.5, "mm") * ncol(mat),
    height = unit(2.5, "mm") * nrow(mat),
    
    cell_fun = function(j, i, x, y, w, h, fill) {
      if (!is.na(mat_p[i, j]) && mat_p[i, j] < 0.05) {
        grid.text("*", x, y, gp = gpar(fontsize = fs))
      }
    }
  )
  
  ####################################################
  ## 1-5. PDF出力と描画順の取得
  ####################################################
  
  ht_all <- ht
  
  pdf(
    paste0("cytokine_hm_aligned_", dis, ".pdf"),
    width = 6,
    height = 6
  )
  
  # 描画後のオブジェクトを保存
  ht_drawn <- draw(
    ht_all,
    heatmap_legend_side = "right"
  )
  
  # PDFでの行順・列順を取得
  row_idx <- ComplexHeatmap::row_order(ht_drawn, name = "rho")
  col_idx <- ComplexHeatmap::column_order(ht_drawn, name = "rho")
  
  dev.off()
  
  # 行・列が分割されている場合にも対応
  row_idx <- unlist(row_idx, use.names = FALSE)
  col_idx <- unlist(col_idx, use.names = FALSE)
  
  ####################################################
  ## 1-6. Excel用データを作成
  ####################################################
  
  # PDFと同じ順序に並べ替える
  mat_export <- mat[row_idx, col_idx, drop = FALSE]
  p_export   <- mat_p[row_idx, col_idx, drop = FALSE]
  
  # 行名のmotuを通常の列に変換
  rho_df <- data.frame(
    motu = rownames(mat_export),
    mat_export,
    row.names = NULL,
    check.names = FALSE
  )
  
  p_df <- data.frame(
    motu = rownames(p_export),
    p_export,
    row.names = NULL,
    check.names = FALSE
  )
  
  # ヒートマップ上の * に対応するデータ
  star_mat <- matrix(
    "",
    nrow = nrow(p_export),
    ncol = ncol(p_export),
    dimnames = dimnames(p_export)
  )
  
  star_mat[!is.na(p_export) & p_export < 0.05] <- "*"
  
  star_df <- data.frame(
    motu = rownames(star_mat),
    star_mat,
    row.names = NULL,
    check.names = FALSE
  )
  
  # 疾患アノテーションと、その元のcoef
  annotation_df <- data.frame(
    display_order = seq_len(nrow(mat_export)),
    motu = rownames(mat_export),
    disease = dis,
    row.names = NULL
  ) %>%
    left_join(
      rlist_use %>%
        select(motu, coef, maa_group) %>%
        distinct(motu, .keep_all = TRUE),
      by = "motu"
    )
  
  ####################################################
  ## 1-7. 疾患ごとにExcelファイルを書き出す
  ####################################################
  
  writexl::write_xlsx(
    x = list(
      rho = rho_df,
      p_value = p_df,
      significance = star_df,
      annotation = annotation_df
    ),
    path = paste0("cytokine_hm_aligned_", dis, "_source.xlsx")
  )
}
