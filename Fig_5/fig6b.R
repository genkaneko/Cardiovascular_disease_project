setwd("/Users/gizen/Documents/AF/20250810/")
library(tidyverse)
library(readxl)
# library(showtext)  # 注释掉showtext
library(openxlsx)
library(parallel)
# library(clusterProfiler)
library(data.table)
library(microViz)
library(patchwork)
enrich_qvalue_thr=0.1

# 注释掉showtext相关代码
# font_add_google("Noto Sans JP", "noto_jp")
# showtext_auto()
# read data ----
max_l=0.05
times=1


enrich_mas <- read.csv("./KEGGID_enrichfdr01_7cohort_250811.csv",row.names = 1)
# enrich_mas=enrich_mas[enrich_mas$metadata!="Paf",]
enrich_mas=enrich_mas[enrich_mas$metadata %in% c("AF",
                                                 "Non_PAF","PAF","IHD","CHF"),]

enrich_mas$metadata=gsub("Non_PAF","Non-PAF",enrich_mas$metadata)
enrich_mas$metadata=gsub("CHF","HF",enrich_mas$metadata)
metakey=unique(enrich_mas$metadata)
enrich_mas=na.omit(enrich_mas)
passlist=unique(enrich_mas[,c("category","subcategory","Description","ID")])

enrich=enrich_mas[enrich_mas$p.adjust<enrich_qvalue_thr,]
metakey=unique(enrich$metadata)
# enrich=enrich[enrich$subcategory!="Global and overview maps",]
enrich=enrich[!is.na(enrich$Description),]
enrich_mas_expose=enrich


colnames(enrich_mas_expose)[c(4,6)]=c("B","C")
enrich_mas_expose$GeneRatio_val <- sapply(enrich_mas_expose$GeneRatio, function(x) {
  parts <- unlist(strsplit(x, "/"))
  as.numeric(parts[1]) / as.numeric(parts[2])
})

final=enrich_mas_expose
tbl_plt <- final %>% 
  mutate(`Enrichment%` = GeneRatio_val, .before = p.adjust) %>% 
  mutate(Input_KOs = if_else(KO_list == "Enrich", "UP", "Down")) %>% 
  arrange(B) %>% 
  mutate(Input_KOs = factor(Input_KOs, levels = c("UP", "Down", "NO"))) %>% 
  rename(Category = B) %>% 
  filter(p.adjust < enrich_qvalue_thr) %>% 
  mutate(Category = fct_relevel(Category, "Global and overview maps", after = 0)) %>%  # Move "Global and overview maps" to the top
  arrange(Category) %>%  # Arrange after releveling to maintain the desired order
  mutate(C = fct_inorder(C) %>% fct_rev(),
         metadata = metadata)
tbl_plt$metadata=factor(tbl_plt$metadata,levels =metakey )
tbl_plt$new_enrich=ifelse(tbl_plt$`Enrichment%`>max_l,max_l,tbl_plt$`Enrichment%`)
tbl_plt$`Enrichment%`=tbl_plt$new_enrich*times
passlist=passlist[passlist$Description %in% tbl_plt$C,]
# pass_order=read.csv("/Users/gizen/Documents/smoking/motu3/path_order.csv")
# pass_order=pass_order[pass_order$Description %in% tbl_plt$C,]
# tbl_plt$C=factor(tbl_plt$C,levels =pass_order$Description[nrow(pass_order):1] )
# tbl_plt$Category=factor(tbl_plt$Category,levels =unique(pass_order$subcategory)[length(unique(pass_order$subcategory)):1] )

brewerPlus <- distinct_palette()
tbl_plt$new_enrich=ifelse(tbl_plt$`Enrichment%`>max_l,max_l,tbl_plt$`Enrichment%`)
tbl_plt$`Enrichment%`=tbl_plt$new_enrich*times
shared_size_scale <- scale_size(range = c(0, 5), limits = c(0, 0.05))
color=c("firebrick", "dodgerblue","white")
names(color)=c("UP","Down","NO")

plt_bubble <- tbl_plt %>%
  ggplot(aes(x = metadata, y = C, size = `Enrichment%`, group = Input_KOs, color = Input_KOs)) +
  geom_point(position = position_dodge(0.75), alpha = 0.6) +
  shared_size_scale+
  # facet_wrap(~outcome, ncol = length(outcome_list)) +
  scale_color_manual(values = color) +
  labs(title = str_c(paste0(length(unique(tbl_plt$C)),"pathC\n  MAA FDR<0.1 selected enrichment,\n enrich p.adjust<",enrich_qvalue_thr))) +
  theme_bw(base_family = "Helvetica") +  # 使用Helvetica字体
  theme(
    strip.text = element_text(size = 10, face = "bold", angle = 90),  # 标题字体10号
    axis.text = element_text(color = "black", size = 10, face = "bold"),  # 轴文本字体10号
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 10, face = "bold"),  # x轴文本10号
    axis.text.y = element_blank(),
    plot.title = element_text(size = 10),  # 标题字体10号
    legend.text = element_text(size = 10),  # 图例文字10号
    legend.title = element_text(size = 10),  # 图例标题10号
    plot.margin = margin(0, 0, 0, 0, "pt"),
    axis.line.x = element_line(size = 1.5, color = "black"),
    axis.ticks.y = element_blank(),
    axis.title = element_blank()
  )+guides(fill = guide_legend(reverse = TRUE))

plt_B_out <- tbl_plt %>%
  ggplot(aes(x="a", y=C, fill=Category))+
  geom_tile(color="white")+
  scale_fill_manual(values=brewerPlus)+
  theme_minimal(base_family = "Helvetica") +scale_y_discrete(position = "right")+
  theme(
    axis.text.y = element_blank(),  # 隐藏 y 轴文本
    axis.text.x = element_blank(),  # 隐藏 x 轴文本
    legend.text = element_text(size = 10),  # 图例文字10号
    legend.title = element_text(size = 10),  # 图例标题10号
    plot.margin = margin(0, 0, 0, 0, "pt"),  # 移除图的边距
    axis.title = element_blank() )+guides(fill = guide_legend(reverse = F))


plt_B <- tbl_plt %>%
  ggplot(aes(x = "a", y = C, fill = Category)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = brewerPlus) +
  theme_minimal(base_family = "Helvetica") +scale_y_discrete(position = "right")+
  theme(
    axis.text = element_text(color = "black", size = 10, face = "bold"),  # 轴文本字体10号
    axis.text.x = element_blank(),  # 隐藏 x 轴文本
    axis.text.y.right = element_text(size = 10),  # y轴文本10号
    axis.title.y.right = element_text(size = 10),  # y轴标题10号
    legend.text = element_text(size = 10),  # 图例文字10号
    legend.title = element_text(size = 10),  # 图例标题10号
    plot.margin = margin(0, 0, 0, 0, "pt"),
    axis.title = element_blank()
  )+guides(fill = guide_legend(reverse = F))


library(Cairo)
cairo_pdf("250811_7cohort_FDR005_bubble_plot.pdf", height=12, width=9)
# wrap_plots(list(plt_B_out, plt_bubble,plt_B,plt_bubble_out,plt_bubble_cytokine), nrow = 1, widths = c(1,28,1,15,6))+
wrap_plots(list(plt_B_out, plt_bubble,plt_B), nrow = 1, widths = c(1,12,1))+
  plot_layout(guides = "collect")
dev.off()