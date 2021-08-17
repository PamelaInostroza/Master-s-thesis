#------Add label to variables----------
VarClass <- function(lc, orden = c(1:length(levels(factor(lc$param))))){
  b <- levels(factor(lc$param))
  b <- b[order(b)[orden]]
  labels <- NULL
  i = 0
  for (each in b){
    i = i + 1
    labels[each] <- paste0(b[i]," - ",attr(data_model[[each]], "variable.label"))
  }
  lc$param <- factor(lc$param, levels = b, labels = labels)
  return(lc)
}

#------Class barplot----------
graphclass <- function(cmodel = NULL, nclass = NULL, 
                       orden = c(1:length(levels(factor(cmodel$param)))), 
                       title = NULL, leg = FALSE){
  a <- levels(factor(cmodel$param))
  a <- a[order(a)[orden]]
  labels <- NULL
  for (each in a){
    labels[each] <- attr(data_model[[each]], "variable.label")
  }
  
  labels2 <- NULL
  n <- 0
  for (each in levels(cmodel$category)){
    n <- n + 1
    labels2[each] <- paste(each, "-", attr(data_model[[a[1]]], "levels")[n])
  }
  
  cmodel$paramf <- factor(cmodel$param, levels = a, labels = labels)
  cmodel$categoryf <- factor(cmodel$category, levels = levels(cmodel$category), 
                             labels = labels2)
  
  zp1 <- ggplot(data = subset(cmodel),
         aes(x = paramf, y = value, fill = categoryf)) + 
    geom_bar(stat = "identity", position = "stack") + 
    ggtitle(title) +
    labs(x = "Items", y = "Response probabilities", fill ="Response category") +
    scale_fill_grey() + theme_bw() +
    theme(legend.position = "top", 
          title = element_text(size=9),
          strip.text.y = element_text(size = 7), 
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0, size = 6),
          axis.text.y = element_text(size = 7),
          axis.title = element_text(size = 7),
          axis.ticks.y=element_blank(),         
          panel.grid.major.y=element_blank(), legend.title = element_text(size = 8), 
          legend.key.size = unit(0.3, "cm"),
          legend.text = element_text(size = 8)) +
    scale_x_discrete(label = function(x) str_wrap(x,25)) +
    guides(fill=guide_legend(nrow=1,byrow=TRUE)) +
    scale_y_continuous(breaks = c(0.25,0.5,0.75)) +
    geom_hline(yintercept=c(0.25,0.5,0.75), linetype = "dashed", size = 0.3, 
               color = "gray") +
    facet_grid(. ~ Class, labeller = label_wrap_gen(20)) 
  
  print(zp1)
}

#------Models fit summary----------
Modelfit <- function(Modellist, title ="", fontn = 9){
  resultsbyallo <- mixtureSummaryTable(eval(parse(text=paste0(Modellist))),
                  keepCols = c("Title", "NLatentClasses", "Parameters", "LL", 
                               "AIC", "BIC", "aBIC","Entropy", 
                               "T11_VLMR_2xLLDiff", "T11_VLMR_PValue",  
                               "T11_LMR_Value", "T11_LMR_PValue"))
  
  resultsbyallo <- resultsbyallo %>% 
    mutate(Type = Title,
      Cycle = substr(Title,str_locate(Title, "C[0-9]+")[1], 
                     str_locate(Title, "C[0-9]+")[2]+1),
      Year = ifelse(Cycle == "C1", 1999, ifelse(Cycle == "C2", 2009, 
                                ifelse(Cycle == "C3", 2016, NA)))) %>% 
    arrange(Year)
  

  resultsbyall <- resultsbyallo %>% dplyr::arrange(Year) %>% 
    dplyr::group_by(Year) %>%  
    dplyr::mutate(
      Reduction = scales::percent(ifelse(is.na(lag(LL)), NA, 
                                    (lag(LL)-LL)/lag(LL)),accuracy = 0.1),
      LL = cell_spec(round(LL,0), italic = ifelse(LL == min(LL), 
                                                  TRUE, FALSE)),
      AIC = cell_spec(round(AIC,0), italic = ifelse(AIC == min(AIC), 
                                                    TRUE, FALSE)),
      BIC = cell_spec(round(BIC,0), italic = ifelse(BIC == min(BIC), 
                                                    TRUE, FALSE)),
      aBIC = cell_spec(round(aBIC,0), italic = ifelse(aBIC == min(aBIC), 
                                                      TRUE, FALSE)),
      Entropy = ifelse(is.na(Entropy), "", 
                        cell_spec(scales::percent(Entropy,accuracy = 0.1), 
                          italic = ifelse(Entropy == max(Entropy, na.rm = T), 
                                                 TRUE, FALSE))),
      Reduction = ifelse(is.na(Reduction),"",cell_spec(Reduction, 
                        italic = ifelse(Reduction == max(Reduction, na.rm = T), 
                                              TRUE, FALSE))),
      T11_VLMR_2xLLDiff = ifelse(is.na(T11_VLMR_2xLLDiff), "", 
                                 cell_spec(round(T11_VLMR_2xLLDiff,0), 
                                  italic = ifelse(T11_VLMR_PValue > 0.05, 
                                                  TRUE, FALSE))),
      T11_VLMR_PValue = ifelse(is.na(T11_VLMR_PValue), "", 
                               cell_spec(round(T11_VLMR_PValue,3), 
                                  italic = ifelse(T11_VLMR_PValue > 0.05, 
                                                  TRUE, FALSE))),
      T11_LMR_Value = ifelse(is.na(T11_LMR_Value), "", 
                             cell_spec(round(T11_LMR_Value,0), 
                                  italic = ifelse(T11_LMR_PValue > 0.05, 
                                                  TRUE, FALSE))),
      T11_LMR_PValue = ifelse(is.na(T11_LMR_PValue), "", 
                              cell_spec(round(T11_LMR_PValue,3), 
                                        italic = ifelse(T11_LMR_PValue > 0.05, 
                                                        TRUE, FALSE)))) %>% 
    ungroup()
  
  resultsbyall <- resultsbyall[,c("Year", "NLatentClasses", "Parameters", "LL", 
                                "AIC",	"BIC",	"aBIC",	"Entropy", "Reduction",
                                "T11_VLMR_2xLLDiff", "T11_VLMR_PValue",  
                                "T11_LMR_Value", "T11_LMR_PValue")] %>% 
    setNames(c("Year", "N Latent\n Classes", "Param", "Log-Likelihood", 
               "AIC",	"BIC",	"aBIC",	"Entropy", "LL\n Reduction",
               "VLMR\n 2*LL Dif", "VLMR\n PValue", "LMR\n Value", "LMR\n PValue"))
  tableSumm <- resultsbyall %>% select(-Year) %>% 
    kbl(caption = paste0(title), 
        booktabs = TRUE, longtable = TRUE, row.names = FALSE, escape = FALSE) %>%
    kable_classic_2(full_width = F) %>% 
    kable_styling(latex_options = c("repeat_header", "HOLD_position"), 
                  font_size = fontn) %>% 
    column_spec(c(1,2), width = "3em") %>% 
    column_spec(c(3:12), width = "4em") %>% 
  
  return(tableSumm)
}


#------Class highest probabilities----------
HighProb <- function(lc5, siz5, title = NULL, 
                     orden = c(1:length(levels(factor(lc5$param)))), 
                     longsize = 11){

  labels_x <- NULL
  for (each in levels(lc5$param)){
    labels_x[each] <- paste0(each," - ",attr(data_model[[each]], "variable.label"))
  }
  labels_x <- unlist(labels_x)
  
  lc5f <- lc5 %>%  mutate(Class = str_remove(Class,"\n"))
  siz5f <- siz5 %>%
    mutate(Class = str_remove(Class,"\n"),
           param = levels(lc5$param)[orden][length(levels(lc5$param))], 
           category = "2")
  siz <- left_join(lc5f, siz5f, by = c("param", "category", "Class")) %>% 
    arrange(Class, param) %>% 
    group_by(param) %>% mutate(dif = abs(value - lag(value))) %>% ungroup() %>% 
    mutate(Class = factor(Class, levels = str_remove(levels(lc5$Class),"\n")),
           param = factor(param, levels = levels(lc5$param), labels = labels_x))
  
  siz$paramf <- factor(siz$param, levels = levels(siz$param)[orden])
  pc5 <- siz %>% 
    ggplot() +
    geom_point(aes(x = paramf, y = value, group = category, color = category), 
               size = 2) +
    geom_line(aes(paramf, value, group = category, linetype = category, 
                  color = category)) +
    scale_fill_grey() + theme_bw() #+
    #ggtitle(title) 
    
  pc5 <- pc5 + 
    geom_text(aes(x = paramf, y = 0.5, label = scales::percent(per, accuracy = 0.1)), 
              size = 2.5, nudge_x = -0.15, nudge_y = 0.1) +
    facet_grid(Class ~ ., switch = "y", labeller = label_wrap_gen(6)) +
    theme(legend.position = "top", legend.box="vertical",
          strip.text.y = element_text(size = 7),
          legend.spacing.y = unit(-0.2, 'cm'),
          title = element_text(size = 9),
          axis.title.x = element_blank(), 
          axis.text.y = element_text(size = 8),
          legend.title = element_text(size = 8), 
          legend.text =  element_text(size = 8), 
          axis.text.x = element_text(angle = 90, size = 7, 
                                     vjust = 0.5, hjust = 0)) +
    scale_y_continuous(breaks = c(0.25,0.5,0.75)) +
    labs(y="Response probabilities", linetype = "Response category", 
         color = "Response category")  +
    scale_linetype_discrete(labels = c("Agree", "Disagree")) +
    scale_color_brewer(type = "qual", palette = "Dark2", 
                       labels = c("Agree", "Disagree")) +
    scale_shape(solid = FALSE, guide = FALSE) +
    scale_x_discrete(label = function(x) str_wrap(x,15))
  return(pc5)
}

#------Comparative highest probabilities----------
ClassGraph <- function(lc5f, siz5, title = NULL, 
                       orden = c(1:length(levels(factor(lc5f$param)))), 
                       selected = c(1:length(levels(factor(siz5$Class))))){
  
  labels_x <- NULL
  for (each in levels(lc5f$param)){
    labels_x[each] <- paste0(each," - ",attr(data_model[[each]], 
                                             "variable.label"))
  }
  labels_x <- unlist(labels_x)
  
  siz5f <- siz5 %>% arrange(desc(per)) %>% cbind(row = c(1:nrow(siz5))) %>% 
    mutate(Class = factor(as.numeric(Class)),
           ClassesSizes = paste("Class", row,":", 
                                scales::percent(per, accuracy = 0.1))) 
  
  siz <- lc5f %>% 
    arrange(Class, param) %>% 
    group_by(param) %>% mutate(dif = abs(value - lag(value))) %>% 
    ungroup() 
  
  siz$paramf <- factor(siz$param, levels = levels(siz$param)[orden])
  
  pc5 <- siz %>% filter(category == 1) %>% 
    ggplot() +
    geom_point(aes(x = paramf, y = value, group = Class, color = Class), 
               size = 1.5) +
    geom_line(aes(paramf, value, group = Class, linetype = Class, color = Class)) +
    scale_fill_grey() + theme_bw() +
    ggtitle(title) +
    theme(legend.position = "top", 
          legend.direction = "vertical",
          strip.text.y = element_text(size = 8),
          legend.spacing.x = unit(0.1, 'cm'),
          legend.spacing.y = unit(-0.2, 'cm'),
          legend.margin=margin(t = 0, unit='cm'),
          title = element_text(size = 9), 
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(), 
          legend.title = element_blank(),
          legend.text = element_text(size = 6),
          axis.text.x = element_text(angle = 90, size = 8, 
                                     vjust = 0.5, hjust = 0)) +
    guides(linetype = guide_legend(nrow = 2)) +
    scale_y_continuous(breaks = c(0.25,0.5,0.75), limits = c(0,1)) +
    labs(y="Response probabilities", linetype = "Latent Classes", 
         color = "Latent Classes")  +
    scale_linetype_discrete(labels = siz5f$ClassesSizes) +
    scale_colour_manual(values=cbPalette[selected], 
                        labels = siz5f$ClassesSizes) +
    scale_shape(solid = FALSE, guide = FALSE) +
    scale_x_discrete(label = function(x) str_wrap(x,25))
  return(pc5)
}


#------Models fit By country summary----------
ModelfitByContry <- function(Modellist, title = " ", 
                             filterval = FALSE, fontn = 9){
  resultsbyallo <- mixtureSummaryTable(eval(parse(text=paste0(Modellist))), 
                                       keepCols = c("Title", "NLatentClasses", "Parameters", "LL", 
                                                    "AIC", "BIC", "aBIC","Entropy", 
                                                    "T11_VLMR_2xLLDiff", "T11_VLMR_PValue",  "T11_LMR_Value", "T11_LMR_PValue"))
  
  resultsbyallo <- resultsbyallo %>% mutate(CNT = substr(Title,1,3),
                                            Cycle = substr(Title,str_locate(Title, "C[0-9]+")[1], str_locate(Title, "C[0-9]+")[2]+1))
  
  resultsbyall <- resultsbyallo %>% dplyr::arrange(Cycle, CNT) %>% dplyr::group_by(Cycle, CNT) %>% 
  dplyr::mutate(Reduction = scales::percent(ifelse(is.na(lag(LL)), NA, (lag(LL)-LL)/lag(LL)),accuracy = 0.1),
                LL = cell_spec(round(LL,0), italic = ifelse(LL == min(LL), TRUE, FALSE)),
                AIC = cell_spec(round(AIC,0), italic = ifelse(AIC == min(AIC), TRUE, FALSE)),
                s = ifelse(BIC == min(BIC), TRUE, FALSE),
                BIC = cell_spec(round(BIC,0), italic = ifelse(BIC == min(BIC), TRUE, FALSE)),
                aBIC = cell_spec(round(aBIC,0), italic = ifelse(aBIC == min(aBIC), TRUE, FALSE)),
                Entropy = ifelse(is.na(Entropy), "", cell_spec(scales::percent(Entropy,accuracy = 0.1), italic = ifelse(Entropy == max(Entropy, na.rm = T), TRUE, FALSE))),
                Reduction = ifelse(is.na(Reduction),"",cell_spec(Reduction, italic = ifelse(Reduction == max(Reduction, na.rm = T), TRUE, FALSE))),
                T11_VLMR_2xLLDiff = ifelse(is.na(T11_VLMR_2xLLDiff), "", cell_spec(round(T11_VLMR_2xLLDiff,0), italic = ifelse(T11_VLMR_PValue > 0.05, TRUE, FALSE))),
                T11_VLMR_PValue = ifelse(is.na(T11_VLMR_PValue), "", cell_spec(round(T11_VLMR_PValue,3), italic = ifelse(T11_VLMR_PValue > 0.05, TRUE, FALSE))),
                T11_LMR_Value = ifelse(is.na(T11_LMR_Value), "", cell_spec(round(T11_LMR_Value,0), italic = ifelse(T11_LMR_PValue > 0.05, TRUE, FALSE))),
                T11_LMR_PValue = ifelse(is.na(T11_LMR_PValue), "", cell_spec(round(T11_LMR_PValue,3), italic = ifelse(T11_LMR_PValue > 0.05, TRUE, FALSE)))) %>% ungroup()
resultsbyall <- resultsbyall[,c("s","Cycle", "CNT","NLatentClasses", "Parameters", "LL", 
                                "AIC",	"BIC",	"aBIC",	"Entropy", "Reduction",
                                "T11_VLMR_2xLLDiff", "T11_VLMR_PValue",  "T11_LMR_Value", "T11_LMR_PValue")] %>% 
  setNames(c("s","Year", "Country", "N Latent\n Classes", "Param", "Log-Likelihood", "AIC",	"BIC",	"aBIC",	"Entropy", "LL\n Reduction",
             "VLMR\n 2*LL Dif", "VLMR\n PValue", "LMR\n Value", "LMR\n PValue"))

resultsbyall <- resultsbyall %>% 
  mutate(Country = case_when(Country == "BFL" ~ "Belgium (Flemish)",
                             Country == "BGR" ~ "Bulgaria",
                             Country == "DNK" ~ "Denmark",
                             Country == "EST" ~ "Estonia",
                             Country == "FIN" ~ "Finland",
                             Country == "HRV" ~ "Croatia",
                             Country == "ITA" ~ "Italy",
                             Country == "LTU" ~ "Lithuania",
                             Country == "LVA" ~ "Latvia",
                             Country == "MLT" ~ "Malta",
                             Country == "NLD" ~ "Netherlands",
                             Country == "NOR" ~ "Norway",
                             Country == "SVN" ~ "Slovenia",
                             Country == "SWE" ~ "Sweden")) 

tablebyg <- resultsbyall %>% arrange(Country) %>% 
 {if(filterval) filter(., s) %>% select(-Year, -s) else filter(., `N Latent\n Classes` <= max(`N Latent\n Classes`)) %>% select(-Year, -Country, -s) }%>% 
  kbl(caption = paste0(title), 
      booktabs = TRUE, longtable = TRUE, row.names = FALSE, escape = FALSE) 

return(tablebyg)
}
#------Models fit multigroup summary----------
ModelfitMGCntry <- function(Modellist, title = " ", 
                            filterval = NA, fontn = 9){
  
  resultsbyallo <- mixtureSummaryTable(eval(parse(text=paste0(Modellist))), 
                                       keepCols = c("Title", "NLatentClasses", "Parameters", "LL", 
                                                    "AIC", "BIC", "aBIC","Entropy", "ChiSqCategoricalPearson_DF"))
  
  
  resultsbyallo <- resultsbyallo %>% 
    mutate(Ngroups = as.numeric(str_extract(str_extract(eval(parse(text=paste0(Modellist,"[[1]]$input$variable$classes"))),"g\\([0-9]{1,2}\\)"),"[1-9]{1,2}")),
           NLatentClasses = NLatentClasses - Ngroups,
           Cycle = ifelse(is.na(str_extract(Title,"C[0-9]+")), "All", str_extract(Title,"C[0-9]+")), 
           Year = ifelse(Cycle == "C1", 1999, ifelse(Cycle == "C2", 2009, ifelse(Cycle == "C3", 2016, "All"))),
           Type = str_extract(Title,"[1-9]{0,1}[A-Z]{1}\\.[A-Za-z]{3,4}+"),
           Type = ifelse(Type == "C.Het", "Complete heterogeneity", 
                      #   ifelse(Type == "1P.Hom", "Partial homogeneity 1 class",
                      #   ifelse(Type == "P.Hom", "Freely estimated",
                      #   ifelse(Type == "2P.Hom", "With constrained values in 2 classes",
                      #   ifelse(Type == "2P.HomR", "With starting values in 2 classes",
                      #   ifelse(Type == "3P.Hom", "Partial homogeneity 3 classes",
                      #   ifelse(Type == "4P.Hom", "Partial homogeneity 4 classes",
                                ifelse(Type == "P.Hom", "Partial homogeneity",
                                       ifelse(Type == "C.Hom", "Complete homogeneity", NA)))) %>% select(-Title, -Cycle)
  
  resultsbyall <- resultsbyallo %>% 
    dplyr::arrange(Year, NLatentClasses, desc(Parameters)) %>% dplyr::group_by(Year, NLatentClasses) %>% 
    dplyr::mutate(Parameters = ifelse(is.na(Parameters), 0, Parameters),
                  Reduction = scales::percent((first(LL)-LL)/first(LL),accuracy = 0.01),
                  deltaLL = ifelse(LL-first(LL) !=0, round(LL-first(LL),0),NA),
                  deltadf = ifelse(first(ChiSqCategoricalPearson_DF)-ChiSqCategoricalPearson_DF !=0, first(ChiSqCategoricalPearson_DF)-ChiSqCategoricalPearson_DF, NA),
                  pvaluedelta = ifelse(abs(deltadf) !=0, round(1-pchisq(abs(deltaLL), abs(deltadf)),3),  NA),
                  LL = ifelse(is.na(LL), "", cell_spec(round(LL,0), italic = ifelse(LL == min(LL), TRUE, FALSE))),
                  AIC = ifelse(is.na(AIC), "", cell_spec(round(AIC,0), italic = ifelse(AIC == min(AIC), TRUE, FALSE))),
                  BIC = ifelse(is.na(BIC), "", cell_spec(round(BIC,0), italic = ifelse(BIC == min(BIC), TRUE, FALSE))),
                  aBIC = ifelse(is.na(aBIC), "", cell_spec(round(aBIC,0), italic = ifelse(aBIC == min(aBIC), TRUE, FALSE))),
                  Entropy = ifelse(is.na(Entropy), "", cell_spec(scales::percent(Entropy,accuracy = 0.1), italic = ifelse(Entropy == max(Entropy, na.rm = T), TRUE, FALSE))),
                  Reduction = ifelse(is.na(Reduction),"",cell_spec(Reduction, italic = ifelse(Reduction == max(Reduction, na.rm = T), TRUE, FALSE)))) %>% ungroup()
  resultsbyall <- resultsbyall[,c("Year", "NLatentClasses","Type", "Ngroups", "Parameters", "LL", 
                                  "AIC",	"BIC",	"aBIC",	"Entropy", "Reduction",
                                  "deltaLL", "deltadf", "pvaluedelta")] %>% 
    setNames(c("Year", "NLatentClasses","Type", "Ngroups", "Param", "Log-Likelihood", "AIC",	"BIC",	"aBIC",	
               "Entropy", "LL\n Reduction", "$\\Delta$ LL", "$\\Delta$ DF", "pvalue $\\Delta$"))

  tablemgcnt <- resultsbyall %>% arrange(NLatentClasses, Param) %>%
    {if(!is.na(filterval[1])) filter(., NLatentClasses %in% filterval)  else filter(., NLatentClasses <= max(NLatentClasses))} %>%
        select(-Year, -NLatentClasses, -Type) %>%
    kbl(caption = paste0(title), align = c("l", rep("r",11)),
        booktabs = TRUE, longtable = TRUE, row.names = FALSE, escape = FALSE) %>%
    kable_classic_2(full_width = F) %>% 
    kable_styling(latex_options = c("repeat_header", "HOLD_position"), font_size = fontn) %>% 
    column_spec(c(2,11), width = "2em") %>% 
    column_spec(c(4,5,6,7,9,10), width = "3em")  %>% 
    column_spec(c(1,3,8), width = "4em") 
  
  return(tablemgcnt)
    
}
#------Models fit Confirmatory summary----------
ModelfitConf <- function(Modellist, title =""){
  resultsbyallo <- mixtureSummaryTable(eval(parse(text=paste0(Modellist))), 
                                       keepCols = c("Title", "NLatentClasses", "Parameters", "LL", 
                                                    "AIC", "BIC", "aBIC","Entropy", 
                                                    "T11_VLMR_2xLLDiff", "T11_VLMR_PValue",  
                                                    "T11_LMR_Value", "T11_LMR_PValue"))
  
  resultsbyallo <- resultsbyallo %>% 
    mutate(Type = str_extract(Title,"[A-Za-z]{0,4}[A-Z]{3}+"),
           Type = ifelse(Type != "ConfLCA", "Exploratory LCA", 
                         "Confirmatory LCA")) %>% 
    arrange(NLatentClasses)
  
  
  resultsbyall <- resultsbyallo %>% dplyr::arrange(desc(Parameters)) %>% 
    dplyr::group_by(NLatentClasses) %>%  
    dplyr::mutate(Reduction = scales::percent(ifelse(is.na(lag(LL)), NA, 
                                                     (lag(LL)-LL)/lag(LL)),accuracy = 0.1),
                  LL = cell_spec(round(LL,0), italic = ifelse(LL == min(LL), 
                                                              TRUE, FALSE)),
                  AIC = cell_spec(round(AIC,0), italic = ifelse(AIC == min(AIC), 
                                                                TRUE, FALSE)),
                  BIC = cell_spec(round(BIC,0), italic = ifelse(BIC == min(BIC), 
                                                                TRUE, FALSE)),
                  aBIC = cell_spec(round(aBIC,0), italic = ifelse(aBIC == min(aBIC), 
                                                                  TRUE, FALSE)),
                  Entropy = ifelse(is.na(Entropy), "", 
                                   cell_spec(scales::percent(Entropy,accuracy = 0.1), 
                                             italic = ifelse(Entropy == max(Entropy, na.rm = T), 
                                                             TRUE, FALSE))),
                  Reduction = ifelse(is.na(Reduction),"",
                                     cell_spec(Reduction, 
                                               italic = ifelse(Reduction == max(Reduction, na.rm = T),
                                                               TRUE, FALSE))),
                  T11_VLMR_2xLLDiff = ifelse(is.na(T11_VLMR_2xLLDiff), "", 
                                             cell_spec(round(T11_VLMR_2xLLDiff,0), 
                                                       italic = ifelse(T11_VLMR_PValue > 0.05, 
                                                                       TRUE, FALSE))),
                  T11_VLMR_PValue = ifelse(is.na(T11_VLMR_PValue), "", 
                                           cell_spec(round(T11_VLMR_PValue,3), 
                                                     italic = ifelse(T11_VLMR_PValue > 0.05, 
                                                                     TRUE, FALSE))),
                  T11_LMR_Value = ifelse(is.na(T11_LMR_Value), "", 
                                         cell_spec(round(T11_LMR_Value,0), 
                                                   italic = ifelse(T11_LMR_PValue > 0.05, 
                                                                   TRUE, FALSE))),
                  T11_LMR_PValue = ifelse(is.na(T11_LMR_PValue), "", 
                                          cell_spec(round(T11_LMR_PValue,3), 
                                                    italic = ifelse(T11_LMR_PValue > 0.05, 
                                                                    TRUE, FALSE)))) %>% 
    ungroup()
  
  resultsbyall <- resultsbyall[,c("Type", "NLatentClasses", "Parameters", "LL", 
                                  "AIC",	"BIC",	"aBIC",	"Entropy", "Reduction")] %>% 
    setNames(c("Type", "N Latent\n Classes", "Param", "Log-Likelihood",
               "AIC",	"BIC",	"aBIC",	"Entropy", "LL\n Reduction"))
  tableSumm <- resultsbyall %>% 
    kbl(caption = paste0(title), 
        booktabs = TRUE, longtable = TRUE, row.names = FALSE, escape = FALSE) %>%
    kable_classic_2(full_width = F) %>% 
    kable_styling(latex_options = c("repeat_header", "HOLD_position"), 
                  font_size = 9) %>% 
    column_spec(c(1), width = "9em") %>% 
    column_spec(c(2:3), width = "3em") %>% 
    column_spec(c(4:9), width = "4em") 
  
  return(tableSumm)
}
