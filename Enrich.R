library(clusterProfiler)
library(tidyverse)

mas_all_metadata=as.data.frame(read.csv("./7cohort_KO_MAA.csv"))
mas_all_metadata=mas_all_metadata[mas_all_metadata$cate=="5137",]
#https://rest.kegg.jp/link/ko/pathway
#https://rest.kegg.jp/list/pathway/ko

resultp=NULL
resultfdr=NULL
for (i in unique(mas_all_metadata$metadata)){
  temp=mas_all_metadata
  KO_up <- temp %>% filter(qval<0.1,coef>0, metadata==i) %>% pull(feature)
  KO_down <- temp %>% filter(qval<0.1,coef<0, metadata==i) %>% pull(feature)
  
  ko_up_en <- try(
    enrichKEGG(gene = KO_up,
               organism = "ko",
               keyType = "kegg",
               pAdjustMethod = "BH",
               pvalueCutoff = 1) %>%
      .@result %>% as_tibble() %>%
      mutate(metadata=i,KO_list="Enrich", .before=1)
  )
  ko_up_en$N=temp$N[1]
  ko_down_en <- try(
    enrichKEGG(gene = KO_down,
               organism = "ko",
               keyType = "kegg",
               pAdjustMethod = "BH",
               pvalueCutoff = 1) %>%
      .@result %>% as_tibble() %>%
      mutate(metadata=i,KO_list="Deplete", .before=1)
  )
  ko_down_en$N=temp$N[1]
  resultp=rbind(resultp,ko_up_en,ko_down_en)
  
}

write.csv(resultp,"KEGGID_enrichfdr01_7cohort.csv")
