library(scales)

#------Graph Mplus models classes----------

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

graphclass <- function(cmodel = NULL, nclass = NULL, orden = c(1:length(levels(factor(cmodel$param)))), title = NULL, leg = FALSE, mg = TRUE){
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
  cmodel$categoryf <- factor(cmodel$category, levels = levels(cmodel$category), labels = labels2)
  
  zp1 <- ggplot(data = subset(cmodel),
         aes(x = paramf, y = value, fill = categoryf)) + 
    geom_bar(stat = "identity", position = "stack") + 
    ggtitle(title) +
    labs(x = "Items", y = "Response probabilities", fill ="Response category") +
    scale_fill_grey() + theme_bw() +
    theme(legend.position = "bottom", 
          title = element_text(size=9),
          strip.text.y = element_text(size = 7), 
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0, size = 6),
          axis.text.y = element_text(size = 7),
          axis.title = element_text(size = 7),
          axis.ticks.y=element_blank(),         
          panel.grid.major.y=element_blank(), legend.title = element_text(size = 8), 
          legend.key.size = unit(0.3, "cm"),
          legend.text = element_text(size = 8)) +
    scale_x_discrete(label = function(x) str_wrap(x,15)) +
    guides(fill=guide_legend(nrow=1,byrow=TRUE)) +
    scale_y_continuous(breaks = c(0.25,0.5,0.75)) +
    geom_hline(yintercept=c(0.25,0.5,0.75), linetype = "dashed", size = 0.3, color = "gray")
  if(mg){
    zp1 <- zp1 +
      facet_grid(Cycle ~ Group + ClassN, labeller = label_wrap_gen(20)) +
      theme(axis.text.x = element_blank())
  } else {
    zp1 <- zp1 +
      facet_grid(Cycle ~ Class, labeller = label_wrap_gen(20)) 
  }
  
  print(zp1)
}

Modelfit <- function(Modellist, title =""){
  resultsbyallo <- mixtureSummaryTable(eval(parse(text=paste0(Modellist))), 
                                       keepCols = c("Title", "NLatentClasses", "Parameters", "LL", 
                                                    "AIC", "BIC", "aBIC","Entropy", 
                                                    "T11_VLMR_2xLLDiff", "T11_VLMR_PValue",  "T11_LMR_Value", "T11_LMR_PValue"))
  
  resultsbyallo <- resultsbyallo %>% 
    mutate(Type = Title,
      Cycle = substr(Title,str_locate(Title, "C[0-9]+")[1], str_locate(Title, "C[0-9]+")[2]+1),
           Year = ifelse(Cycle == "C1", 1999, ifelse(Cycle == "C2", 2009, ifelse(Cycle == "C3", 2016, NA)))) %>% 
    arrange(Year)
  

  resultsbyall <- resultsbyallo %>% dplyr::arrange(Year) %>% dplyr::group_by(Year) %>%  
    dplyr::mutate(Reduction = scales::percent(ifelse(is.na(lag(LL)), NA, (lag(LL)-LL)/lag(LL)),accuracy = 0.1),
                  LL = cell_spec(round(LL,0), italic = ifelse(LL == min(LL), TRUE, FALSE)),
                  AIC = cell_spec(round(AIC,0), italic = ifelse(AIC == min(AIC), TRUE, FALSE)),
                  BIC = cell_spec(round(BIC,0), italic = ifelse(BIC == min(BIC), TRUE, FALSE)),
                  aBIC = cell_spec(round(aBIC,0), italic = ifelse(aBIC == min(aBIC), TRUE, FALSE)),
                  Entropy = ifelse(is.na(Entropy), "", cell_spec(scales::percent(Entropy,accuracy = 0.1), italic = ifelse(Entropy == max(Entropy, na.rm = T), TRUE, FALSE))),
                  Reduction = ifelse(is.na(Reduction),"",cell_spec(Reduction, italic = ifelse(Reduction == max(Reduction, na.rm = T), TRUE, FALSE))),
                  T11_VLMR_2xLLDiff = ifelse(is.na(T11_VLMR_2xLLDiff), "", cell_spec(round(T11_VLMR_2xLLDiff,0), italic = ifelse(T11_VLMR_PValue > 0.05, TRUE, FALSE))),
                  T11_VLMR_PValue = ifelse(is.na(T11_VLMR_PValue), "", cell_spec(round(T11_VLMR_PValue,3), italic = ifelse(T11_VLMR_PValue > 0.05, TRUE, FALSE))),
                  T11_LMR_Value = ifelse(is.na(T11_LMR_Value), "", cell_spec(round(T11_LMR_Value,0), italic = ifelse(T11_LMR_PValue > 0.05, TRUE, FALSE))),
                  T11_LMR_PValue = ifelse(is.na(T11_LMR_PValue), "", cell_spec(round(T11_LMR_PValue,3), italic = ifelse(T11_LMR_PValue > 0.05, TRUE, FALSE)))) %>% ungroup()
  resultsbyall <- resultsbyall[,c("Year", "NLatentClasses", "Parameters", "LL", 
                                    "AIC",	"BIC",	"aBIC",	"Entropy", "Reduction",
                                    "T11_VLMR_2xLLDiff", "T11_VLMR_PValue",  "T11_LMR_Value", "T11_LMR_PValue")] %>% 
    setNames(c("Year", "N Latent\n Classes", "Param", "Log-Likelihood", "AIC",	"BIC",	"aBIC",	"Entropy", "LL\n Reduction",
               "VLMR\n 2*LL Dif", "VLMR\n PValue", "LMR\n Value", "LMR\n PValue"))
  tableSumm <- resultsbyall %>% select(-Year) %>% 
    kbl(caption = paste0(title), 
        booktabs = TRUE, longtable = TRUE, row.names = FALSE, escape = FALSE) %>%
    kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"), 
                  position = "left", font_size = 9, full_width = TRUE) %>% 
    column_spec(c(1,2), width = "3em") %>% 
    column_spec(c(3:12), width = "4em") %>% 
  
  return(tableSumm)
}

HighProb <- function(lc5, siz5, orden = c(1:length(levels(factor(lc5$param)))), title = NULL, longsize = 15){

  #lc5$param <- factor(str_replace(lc5$param,"GND","bT_GNDREQ"))
  #lc5$param <- factor(lc5$param, levels = levels(lc5$param)[orden])
  labels_x <- NULL
  for (each in levels(lc5$param)){
    labels_x[each] <- attr(data_model[[each]], "label")
  }
  labels_x <- unlist(labels_x)
  
  lc5f <- lc5 %>%  mutate(Class = str_remove(Class,"\n"))
  siz5f <- siz5 %>%
    mutate(Class = str_remove(Class,"\n"),
           param = "GND6", category = "2")
  siz <- left_join(lc5f, siz5f, by = c("Cycle", "param", "category", "Class")) %>% 
    arrange(Cycle, Class, param) %>% 
    group_by(Cycle, param) %>% mutate(dif = abs(value - lag(value))) %>% ungroup() %>% 
    mutate(Class = factor(Class, levels = str_remove(levels(lc5$Class),"\n")[c(1,2,3,4,5,6)]),
           param = factor(param, levels = levels(lc5$param)[orden]))
  
  pc5 <- siz %>% 
    ggplot() +
    geom_point(aes(x = param, y = value, group = category, color = category, shape = Cycle), size = 2) +
    geom_line(aes(param, value, group = interaction(category,Cycle), linetype = category, color = category)) +
    scale_fill_grey() + theme_bw() +
    ggtitle(title) 
    
  pc5 <- pc5 + 
    geom_text(aes(x = param, y = 0.5, label = scales::percent(per, accuracy = 0.1)), size = 2.5, 
              nudge_x = -0.15, nudge_y = 0.1) +
    facet_grid(Class ~ Cycle, switch = "y", labeller = label_wrap_gen(10)) +
    theme(legend.position = "top", legend.box="vertical",
          strip.text.y = element_text(size = 8),
          legend.spacing.y = unit(-0.2, 'cm'),
          title = element_text(size = 9),
          axis.title.x = element_blank(), 
          axis.text.y = element_text(size = 8),
          legend.title = element_text(size = 8), 
          axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5, hjust = 0)) +
    scale_y_continuous(breaks = c(0.25,0.5,0.75)) +
    labs(y="Response probabilities", linetype = "Response category", color = "Response category")  +
    scale_linetype_discrete(labels = c("Agree", "Disagree")) +
    scale_color_brewer(type = "qual", palette = "Dark2", labels = c("Agree", "Disagree")) +
    scale_shape(solid = FALSE, guide = FALSE) #+
    #scale_x_discrete(labels = str_wrap(labels_x, longsize))
  print(pc5)
  cat('\n')
  cat('\n')
}

ByGroupHighProbMG <- function(lcg5, sizg5, orden = c(1:length(levels(factor(lcg5$param)))), title = NULL, n = 1:2){
  
  lcg5f <- lcg5 %>% group_by(Cycle, CNT, Class, param) %>% 
    mutate(ClassN = str_remove(ClassN,"\n"))
  sizg5f <- sizg5 %>%
    mutate(ClassN = str_remove(ClassN,"\n"),
           param = "GND6", category = factor(1:2))
  siz <- left_join(lcg5f, sizg5f, by = c("CNT", "Cycle", "Group","param", "category","ClassN")) %>% 
    arrange(CNT, Cycle, ClassN, param) %>% ungroup() %>% 
    group_by(Group,ClassN) %>% 
    mutate(dif = ifelse(category == 1 & abs(per - mean(per, na.rm = TRUE)) < 0.05, value + 0.15, value-0.15)) %>% 
    ungroup() %>% 
    mutate(ClassN = factor(ClassN, levels = levels(lcg5$ClassN)[c(1,2,3,4,5,6)]),
           param = factor(param, levels = levels(lcg5$param)[orden]))
  
  pc5 <- siz %>% 
    ggplot() +
    geom_point(aes(x = param, y = value, color = CNT), size = 0.5) +
    geom_line(aes(param, value, group = interaction(Cycle, CNT, category), color = CNT, linetype = category)) +
    geom_text(x = "GND6", aes(color = CNT, y = dif, 
                              label = ifelse(!is.na(per), paste(CNT, "=",scales::percent(per, accuracy = 0.1)), "")), size = 2.5) +
    theme_bw() +
    ggtitle(paste(title)) +
    labs(y="Response probabilities", color = "Response category") +
    facet_grid(ClassN ~ Group, switch = "y", labeller = label_wrap_gen(15)) +
    theme(legend.position = "top", legend.box="vertical",
          strip.text.y = element_text(size = 8),
          legend.spacing.y = unit(-0.2, 'cm'),
          title = element_text(size = 9),
          axis.title.x = element_blank(), 
          axis.text.y = element_text(size = 8),
          legend.title = element_text(size = 8), 
          legend.text = element_text(size = 8),
          axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5, hjust = 0)) +
    scale_y_continuous(breaks = c(0.25,0.5,0.75)) +
    labs(y="Response probabilities", linetype = "Response category", color = "Country")  +
    scale_linetype_manual(values=c("solid","dotdash"), labels = c("Agree", "Disagree")) +
    scale_color_brewer(type = "qual", palette = "Dark2") 
  print(pc5)
  cat('\n')
  cat('\n')
}

ModelfitByContry <- function(Modellist, title = " "){

  resultsbyallo <- Modellist[,c("CNT", "Cycle", "Title", "NLatentClasses", "Parameters", "LL", 
                                "AIC", "BIC", "aBIC","Entropy", "T11_VLMR_2xLLDiff", "T11_VLMR_PValue",  
                                "T11_LMR_Value", "T11_LMR_PValue")]

resultsbyall <- resultsbyallo %>% dplyr::arrange(Cycle, CNT) %>% dplyr::group_by(Cycle, CNT) %>% 
  dplyr::mutate(Reduction = scales::percent(ifelse(is.na(lag(LL)), NA, (lag(LL)-LL)/lag(LL)),accuracy = 0.1),
                LL = cell_spec(round(LL,0), italic = ifelse(LL == min(LL), TRUE, FALSE)),
                AIC = cell_spec(round(AIC,0), italic = ifelse(AIC == min(AIC), TRUE, FALSE)),
                BIC = cell_spec(round(BIC,0), italic = ifelse(BIC == min(BIC), TRUE, FALSE)),
                aBIC = cell_spec(round(aBIC,0), italic = ifelse(aBIC == min(aBIC), TRUE, FALSE)),
                Entropy = ifelse(is.na(Entropy), "", cell_spec(scales::percent(Entropy,accuracy = 0.1), italic = ifelse(Entropy == max(Entropy, na.rm = T), TRUE, FALSE))),
                Reduction = ifelse(is.na(Reduction),"",cell_spec(Reduction, italic = ifelse(Reduction == max(Reduction, na.rm = T), TRUE, FALSE))),
                T11_VLMR_2xLLDiff = ifelse(is.na(T11_VLMR_2xLLDiff), "", cell_spec(round(T11_VLMR_2xLLDiff,0), italic = ifelse(T11_VLMR_PValue > 0.05, TRUE, FALSE))),
                T11_VLMR_PValue = ifelse(is.na(T11_VLMR_PValue), "", cell_spec(round(T11_VLMR_PValue,3), italic = ifelse(T11_VLMR_PValue > 0.05, TRUE, FALSE))),
                T11_LMR_Value = ifelse(is.na(T11_LMR_Value), "", cell_spec(round(T11_LMR_Value,0), italic = ifelse(T11_LMR_PValue > 0.05, TRUE, FALSE))),
                T11_LMR_PValue = ifelse(is.na(T11_LMR_PValue), "", cell_spec(round(T11_LMR_PValue,3), italic = ifelse(T11_LMR_PValue > 0.05, TRUE, FALSE)))) %>% ungroup()
resultsbyall <- resultsbyall[,c("Cycle", "CNT","NLatentClasses", "Parameters", "LL", 
                                "AIC",	"BIC",	"aBIC",	"Entropy", "Reduction",
                                "T11_VLMR_2xLLDiff", "T11_VLMR_PValue",  "T11_LMR_Value", "T11_LMR_PValue")] %>% 
  setNames(c("Year", "Country", "N Latent\n Classes", "Param", "Log-Likelihood", "AIC",	"BIC",	"aBIC",	"Entropy", "LL\n Reduction",
             "VLMR\n 2*LL Dif", "VLMR\n PValue", "LMR\n Value", "LMR\n PValue"))
tablebyg <- resultsbyall %>% select(-Year,-Country) %>% 
  kbl(caption = paste0(title), 
      booktabs = TRUE, longtable = TRUE, row.names = FALSE, escape = FALSE) %>%
  kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"), 
                position = "left", font_size = 9, full_width = TRUE) %>% 
  column_spec(c(2,3,11), width = "3em") %>% 
  column_spec(c(4:10,12), width = "4em") %>% 
  column_spec(c(1), width = "6em") 

return(tablebyg)
}

ModelfitMGRegion <- function(Modellist, title = " "){
  resultsbyallo <- mixtureSummaryTable(eval(parse(text=paste0(Modellist))), 
                                       keepCols = c("Title", "NLatentClasses", "Parameters", "LL", 
                                                    "AIC", "BIC", "aBIC","Entropy"))
  
  resultsbyallo <- resultsbyallo %>% 
    mutate(Ngroups = as.numeric(str_extract(str_extract(eval(parse(text=paste0(Modellist,"[[1]]$input$variable$classes"))),"g\\([0-9]{1,2}\\)"),"[1-9]{1,2}")),
           NLatentClasses = NLatentClasses - Ngroups,
           Cycle = ifelse(is.na(str_extract(Title,"C[0-9]+")), "All", str_extract(Title,"C[0-9]+")), 
           Year = ifelse(Cycle == "C1", 1999, ifelse(Cycle == "C2", 2009, ifelse(Cycle == "C3", 2016, "All"))),
           Type = str_extract(Title,"[A-Z]{1}\\.[A-Za-z]{3}+"),
           Type = ifelse(Type == "C.Het", "1-Complete heterogeneity", 
                         ifelse(Type == "P.Hom", "3-Partial homogeneity",
                                ifelse(Type == "C.Hom", "4-Complete homogeneity", 
                                       ifelse(Type == "P.Het", paste("2-Partial homogeneity -", NLatentClasses - 1, "classes equal"), NA))))) %>% 
                                         select(-Title, -Cycle) %>% arrange(Type)
  resultsbyall <- resultsbyallo %>% 
    dplyr::arrange(Year, NLatentClasses) %>% dplyr::group_by(Year, NLatentClasses) %>% 
    dplyr::mutate(Reduction = scales::percent((LL-lag(LL))/lag(LL),accuracy = 0.1),
                  deltaLL = ifelse(Type != "3-Partial homogeneity",
                                   round(lag(LL)-LL,0), round(LL[(row_number() == 1)]-LL,0)),
                  deltadf = ifelse(Type != "3-Partial homogeneity",
                                   lag(Parameters)-Parameters, Parameters[(row_number() == 1)]-Parameters),
                  pvaluedelta = round(1-pchisq(abs(deltaLL),deltadf),3),
                  LL = cell_spec(round(LL,0), italic = ifelse(LL == min(LL), TRUE, FALSE)),
                  AIC = cell_spec(round(AIC,0), italic = ifelse(AIC == min(AIC), TRUE, FALSE)),
                  BIC = cell_spec(round(BIC,0), italic = ifelse(BIC == min(BIC), TRUE, FALSE)),
                  aBIC = cell_spec(round(aBIC,0), italic = ifelse(aBIC == min(aBIC), TRUE, FALSE)),
                  Entropy = ifelse(is.na(Entropy), "", cell_spec(scales::percent(Entropy,accuracy = 0.1), italic = ifelse(Entropy == max(Entropy, na.rm = T), TRUE, FALSE))),
                  Reduction = ifelse(is.na(Reduction),"",cell_spec(Reduction, italic = ifelse(Reduction == max(Reduction, na.rm = T), TRUE, FALSE)))
                  ) %>% ungroup()
  resultsbyall <- resultsbyall[,c("Year", "Type", "NLatentClasses","Ngroups", "Parameters", 
                                  "LL", 
                                  "AIC",	"BIC",	"aBIC",	"Entropy",
                                  "Reduction", 
                                  "deltaLL", "deltadf", "pvaluedelta"
                                  )] %>% 
    setNames(c("Year", "Type", "N Latent Classes","Ngroups", "Param", "Log-Likelihood", "AIC",	"BIC",	"aBIC",	
               "Entropy", 
               "LL\n Reduction", 
               "$\\Delta$ LL", "$\\Delta$ DF", "pvalue $\\Delta$"))
  resultsbyall %>% select(-Year) %>% 
    kbl(caption = paste0(title), 
        booktabs = TRUE, longtable = TRUE, row.names = FALSE, escape = FALSE) %>%
    kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"), 
                  position = "left", font_size = 9, full_width = TRUE) %>% 
    column_spec(1, width = "12em") %>% 
    column_spec(c(2,3,4,11,12), width = "3em") %>% 
    column_spec(c(5:8,9,10,13), width = "4em") 
}

ModelfitMGCntry <- function(Modellist, title = " "){
  resultsbyallo <- mixtureSummaryTable(eval(parse(text=paste0(Modellist))), 
                                       keepCols = c("Title", "NLatentClasses", "Parameters", "LL", 
                                                    "AIC", "BIC", "aBIC","Entropy"))
  
  
  resultsbyallo <- resultsbyallo %>% 
    mutate(Ngroups = as.numeric(str_extract(str_extract(eval(parse(text=paste0(Modellist,"[[1]]$input$variable$classes"))),"g\\([0-9]{1,2}\\)"),"[1-9]{1,2}")),
           NLatentClasses = NLatentClasses - Ngroups,
           Cycle = ifelse(is.na(str_extract(Title,"C[0-9]+")), "All", str_extract(Title,"C[0-9]+")), 
           Year = ifelse(Cycle == "C1", 1999, ifelse(Cycle == "C2", 2009, ifelse(Cycle == "C3", 2016, "All"))),
           Type = str_extract(Title,"[A-Z]{1}\\.[A-Za-z]{3}+"),
           Type = ifelse(Type == "C.Het", "Complete heterogeneity", 
                         ifelse(Type == "P.Hom", "Partial homogeneity",
                                ifelse(Type == "C.Hom", "Complete homogeneity", NA)))) %>% select(-Title, -Cycle)
  
  resultsbyall <- resultsbyallo %>% 
    dplyr::arrange(Year, NLatentClasses) %>% dplyr::group_by(Year, NLatentClasses) %>% 
    dplyr::mutate(Reduction = scales::percent(ifelse(is.na(lag(LL)), NA, (LL-lag(LL))/lag(LL)),accuracy = 0.1),
                  deltaLL = round(LL-lag(LL),0),
                  deltadf = lag(Parameters)-Parameters,
                  pvaluedelta = round(1-pchisq(abs(deltaLL),deltadf),3),
                  LL = cell_spec(round(LL,0), italic = ifelse(LL == min(LL), TRUE, FALSE)),
                  AIC = cell_spec(round(AIC,0), italic = ifelse(AIC == min(AIC), TRUE, FALSE)),
                  BIC = cell_spec(round(BIC,0), italic = ifelse(BIC == min(BIC), TRUE, FALSE)),
                  aBIC = cell_spec(round(aBIC,0), italic = ifelse(aBIC == min(aBIC), TRUE, FALSE)),
                  Entropy = ifelse(is.na(Entropy), "", cell_spec(scales::percent(Entropy,accuracy = 0.1), italic = ifelse(Entropy == max(Entropy, na.rm = T), TRUE, FALSE))),
                  Reduction = ifelse(is.na(Reduction),"",cell_spec(Reduction, italic = ifelse(Reduction == max(Reduction, na.rm = T), TRUE, FALSE)))) %>% ungroup()
  resultsbyall <- resultsbyall[,c("Year", "NLatentClasses","Type", "Ngroups", "Parameters", "LL", 
                                  "AIC",	"BIC",	"aBIC",	"Entropy", "Reduction",
                                  "deltaLL", "deltadf", "pvaluedelta")] %>% 
    setNames(c("Year", "NLatentClasses","Type", "Ngroups", "Param", "Log-Likelihood", "AIC",	"BIC",	"aBIC",	
               "Entropy", "LL\n Reduction", "$\\Delta$ LL", "$\\Delta$ DF", "pvalue $\\Delta$"))

  tablemgcnt <- resultsbyall %>% select(-Year, -NLatentClasses) %>% 
    kbl(caption = paste0(title), 
        booktabs = TRUE, longtable = TRUE, row.names = FALSE, escape = FALSE) %>%
    kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"), 
                  position = "left", font_size = 9, full_width = TRUE) %>% 
    column_spec(1, width = "14em") %>% 
    column_spec(c(2,3,10,11), width = "3em") %>% 
    column_spec(c(5:8,12), width = "4em") %>% 
    column_spec(c(4,9), width = "5em")
  
  return(tablemgcnt)
    
}

ByGroupgraphclass <- function(cmodel = NULL, nclass = NULL, orden = c(1:length(levels(factor(cmodel$param)))), title = NULL, leg = FALSE, mg = TRUE){
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
  cmodel$categoryf <- factor(cmodel$category, levels = levels(cmodel$category), labels = labels2)
  for(j in sort(as.character(unique(cmodel$CNT)))) {
    zp1 <- ggplot(data = subset(cmodel, CNT == j),
                  aes(x = paramf, y = value, fill = categoryf)) + 
      geom_bar(stat = "identity", position = "stack") + 
      ggtitle(paste(title, "-", j)) +
      labs(x = "Items", y = "Response probabilities", fill ="Response category") +
      scale_fill_grey() + theme_bw() +
      theme(legend.position = "bottom", 
            title = element_text(size=9),
            strip.text.y = element_text(size = 7), 
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0, size = 6),
            axis.text.y = element_text(size = 7),
            axis.title = element_text(size = 7),
            axis.ticks.y=element_blank(),         
            panel.grid.major.y=element_blank(), legend.title = element_text(size = 8), 
            legend.key.size = unit(0.3, "cm"),
            legend.text = element_text(size = 8)) +
      scale_x_discrete(label = function(x) str_wrap(x,15)) +
      guides(fill=guide_legend(nrow=1,byrow=TRUE)) +
      scale_y_continuous(breaks = c(0.25,0.5,0.75)) +
      geom_hline(yintercept=c(0.25,0.5,0.75), linetype = "dashed", size = 0.3, color = "gray")
    if(mg){
      zp1 <- zp1 +
        facet_grid(Cycle ~ Group + Class, labeller = label_wrap_gen(20)) +
        theme(axis.text.x = element_blank())
    } else {
      zp1 <- zp1 +
        facet_grid(Cycle ~ Class, labeller = label_wrap_gen(20)) 
    }
    
    print(zp1)
  }
}

HighProbMG <- function(lcg5, sizg5, orden = c(1:length(levels(factor(lcg5$param)))), title = NULL, n = 1:2){
  
  lcg5f <- lcg5 %>% group_by(Cycle, Class, param) %>% filter(category == 1) %>% #Probabilities for agree
    mutate(ClassN = str_remove(ClassN,"\n"))
  sizg5 <- sizg5 %>%
    mutate(ClassN = str_remove(ClassN,"\n"),
           param = "GND6")
  siz <- left_join(lcg5f, sizg5, by = c("Cycle", "param", "Group", "ClassN")) %>% 
    #arrange(Cycle, Group, ClassN, param) %>% 
    group_by(Cycle, Group, param) %>% mutate(dif = abs(value - lag(value))) %>% ungroup() %>% 
    mutate(rudge = ifelse(!is.na(per) & (dif >= 0.1 | is.na(dif)), 0.1, ifelse(!is.na(per) & dif < 0.1, -0.1, NA)),
           param = factor(param, levels = levels(lcg5$param)[orden]))
  
  pc5 <- siz %>% filter(Group %in% sort(as.character(unique(siz$Group)))[n]) %>% 
    ggplot() +
    geom_point(aes(x = param, y = value, group = ClassN, color = ClassN), size = 0.5) +
    geom_line(aes(param, value, group = interaction(ClassN, Cycle), color = ClassN)) +
    theme_bw() +
    ggtitle(title) +
    labs(y="Response probabilities", color = "Response category") 
  
  if (length(n) < 4) {
    pc5 <- pc5 + 
      geom_text(aes(x = param, y = value, label = scales::percent(per, accuracy = 0.1), color = ClassN), size = 2.5, 
                nudge_x = -0.15, nudge_y = 0.1) +
      facet_grid(Cycle ~ Group, switch = "y", labeller = label_wrap_gen(20)) +
      scale_y_continuous(breaks = c(0.25,0.5,0.75))  +
      scale_color_brewer(type = "qual", palette = "Dark2") 
    
  } else {
    pc5 <- pc5 +
      geom_text(aes(x = param, y = value, label = scales::percent(per, accuracy = 0.1), color = ClassN), size = 2.5, 
                nudge_x = -0.15, nudge_y = siz$rudge) +
      facet_wrap(. ~ Cycle + Group, labeller = label_wrap_gen(20)) +
      scale_y_continuous(breaks = c(0.25,0.5,0.75))  +
      scale_color_brewer(type = "qual", palette = "Dark2") 
    
  }
  
  if(any(n %in% 1)) {
    pc5 <- pc5 +
      theme(legend.position = "top", legend.box="vertical",
           strip.text.y = element_text(size = 8),
           legend.spacing.y = unit(-0.2, 'cm'),
           axis.title.x = element_blank(), 
           axis.text.y = element_text(size = 8),  
           title = element_text(size = 9),
           legend.title = element_blank(), 
           axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5, hjust = 0)) +
      guides(color=guide_legend(ncol=2,byrow=TRUE)) 
  } else {
    pc5 <- pc5 +
      theme(legend.position = "none", legend.box="vertical",
            strip.text.y = element_text(size = 8),
            legend.spacing.y = unit(-0.2, 'cm'),
            axis.title.x = element_blank(), 
            axis.text.y = element_text(size = 8),
            legend.title = element_blank(), 
            axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5, hjust = 0))
  }
  print(pc5)
  cat('\n')
  cat('\n')
}

