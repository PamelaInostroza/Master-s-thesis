library(plyr)
library(scales)

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}


#------Graph Mplus models classes----------
graphclass <- function(cmodel = NULL, nclass = NULL, orden = c(1:length(levels(factor(cmodel$param)))), title = NULL, leg = FALSE, mg = TRUE){
  cmodel$param <- str_replace(cmodel$param,"GND","bT_GNDREQ")
  
  a <- levels(factor(cmodel$param))
  a <- a[order(a)[orden]]
  labels <- NULL
  for (each in a){
    labels[each] <- str_remove(attr(ISC_lvRlca[[each]], "label"), 
                               "Rights and Responsibilities/Rights and responsibilities/|Rights and Responsibilities/Roles women and men/|Moving/<Immigrants> |Moving/<Immigrant> ")
  }
  
  labels2 <- NULL
  n <- 0
  for (each in levels(cmodel$category)){
    n <- n + 1
    labels2[each] <- paste(each, "-", levels(ISC_lvRlca[[cmodel[cmodel$category == each, "param"][1]]])[n])
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

ByGroupgraphclass <- function(cmodel = NULL, nclass = NULL, orden = c(1:length(levels(factor(cmodel$param)))), title = NULL, leg = FALSE, mg = TRUE){
  cmodel$param <- str_replace(cmodel$param,"GND","bT_GNDREQ")
  
  a <- levels(factor(cmodel$param))
  a <- a[order(a)[orden]]
  labels <- NULL
  for (each in a){
    labels[each] <- str_remove(attr(ISC_lvRlca[[each]], "label"), 
                               "Rights and Responsibilities/Rights and responsibilities/|Rights and Responsibilities/Roles women and men/|Moving/<Immigrants> |Moving/<Immigrant> ")
  }
  
  labels2 <- NULL
  n <- 0
  for (each in levels(cmodel$category)){
    n <- n + 1
    labels2[each] <- paste(each, "-", levels(ISC_lvRlca[[cmodel[cmodel$category == each, "param"][1]]])[n])
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
      facet_grid(Cycle ~ Group + ClassN, labeller = label_wrap_gen(20)) +
      theme(axis.text.x = element_blank())
  } else {
    zp1 <- zp1 +
      facet_grid(Cycle ~ Class, labeller = label_wrap_gen(20)) 
  }
  
  print(zp1)
}
}


Graph_modelfit <- function(Modellist, mg = TRUE){
  resultsbyallo <- mixtureSummaryTable(eval(parse(text=paste0(Modellist))), 
                                       keepCols = c("Title", "NLatentClasses", "Parameters", "LL", "ChiSqCategoricalPearson_Value", "ChiSqCategoricalPearson_DF", "AIC", "BIC", "aBIC","Entropy", 
                                                    "min_N", "max_N", "min_prob", "max_prob"))
  
  resultsbyallo <- resultsbyallo %>% 
    mutate(Type = Title,
      Cycle = substr(Title,str_locate(Title, "C[0-9]+")[1], str_locate(Title, "C[0-9]+")[2]+1),
           Year = ifelse(Cycle == "C1", 1999, ifelse(Cycle == "C2", 2009, ifelse(Cycle == "C3", 2016, NA)))) %>% 
    arrange(Year)
  
  resultsbyallo <- resultsbyallo[,c("Year", "NLatentClasses", "Parameters", "LL", "ChiSqCategoricalPearson_Value", "ChiSqCategoricalPearson_DF", "AIC",	"BIC",	"aBIC",	"Entropy")] %>% 
    setNames(c("Year", "NLatentClasses", "Parameters", "LL", "ChiSq", "DF", "AIC",	"BIC",	"aBIC",	"Entropy"))
  
  resultsbyallo <- resultsbyallo %>%  group_by(Year) %>% mutate(G2 = -2 * (LL-lag(LL)),
                                               dftest = Parameters - lag(Parameters),
                                               p.val = 1-pchisq(G2, df = dftest, lower.tail = FALSE),
                                               per_red = ifelse(is.na(lag(LL)), 0, round((lag(LL)-LL)/lag(LL)*100,1)),
                                               per_red = per_red + lag(per_red))
  
  resultsbyallo %>% 
    kbl(caption = "Model fit statistics", booktabs = TRUE, longtable = TRUE, row.names = FALSE) %>%
    kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"), 
                  position = "left", font_size = 11, full_width = FALSE) %>% 
    #column_spec(3:5, width = "8em") %>%  
    collapse_rows(1) %>% 
    row_spec(c(3,6,9), bold = TRUE) %>% print()
  cat('\n')
  cat('\n')
  
  
}
Graph_modelfitMG <- function(Modellist, mg = TRUE, title = " "){
  resultsbyallo <- mixtureSummaryTable(eval(parse(text=paste0(Modellist))), 
                                       keepCols = c("Title", "NLatentClasses", "Parameters", "LL", "ChiSqCategoricalPearson_Value", "ChiSqCategoricalPearson_DF", "AIC", "BIC", "aBIC","Entropy"))
  
  
  resultsbyallo <- resultsbyallo %>% 
    mutate(Ngroups = as.numeric(str_extract(str_extract(eval(parse(text=paste0(Modellist,"[[1]]$input$variable$classes"))),"g\\([0-9]{2}\\)"),"[1-9]{2}")),
           NLatentClasses = NLatentClasses - Ngroups,
           Cycle = str_extract(Title,"C[0-9]+"),
           Type = str_extract(Title,"PH[1-3]+"),
           Year = ifelse(Cycle == "C1", 1999, ifelse(Cycle == "C2", 2009, ifelse(Cycle == "C3", 2016, NA))),
           Type = ifelse(is.na(Type), "Complete heterogeneity",
                         ifelse(Type == "PH1", "Partial homogeneity - one class", 
                                ifelse(Type == "PH3" | (Type == "PH2" & NLatentClasses == 2), "Complete homogeneity",
                                       ifelse(Type == "PH2", "Partial homogeneity - two classes", NA))))) %>% 
    arrange(Year)
  
  resultsbyallo <- resultsbyallo[,c("Year", "NLatentClasses", "Type", "Parameters", "LL", "ChiSqCategoricalPearson_Value", "ChiSqCategoricalPearson_DF", "AIC",	"BIC",	"aBIC",	"Entropy")] %>% 
    setNames(c("Year", "NLatentClasses", "Model", "Parameters", "LL", "ChiSq", "DF", "AIC",	"BIC",	"aBIC",	"Entropy"))
  resultsbyallo <- resultsbyallo %>%  group_by(Year,NLatentClasses) %>% mutate(G2 = -2 * (LL-lag(LL)),
                                                                dftest = abs(Parameters - lag(Parameters)),
                                                                p.val = round(1-pchisq(G2, df = dftest, lower.tail = FALSE),3))
  
  resultsbyallo %>%
    kbl(caption = paste("Model fit statistics", title), booktabs = TRUE, longtable = TRUE, row.names = FALSE) %>%
    kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"), 
                  position = "left", font_size = 11, full_width = FALSE) %>% 
    #column_spec(3:5, width = "8em") %>%  
    collapse_rows(c(1,2)) %>% print()
  cat('\n')
  cat('\n')
 
}

Graph_modelfitByGroup <- function(Modellist, mg = TRUE){
  resultsbyallo <- NULL
  for(j in 1:3) {
    resultsbyallo0 <- mixtureSummaryTable(eval(parse(text=paste0(Modellist)))[[j]])
    resultsbyallo <- rbind(resultsbyallo,resultsbyallo0)
  }
  
  resultsbyallo <- resultsbyallo %>% 
    mutate(Cycle = substr(Title,str_locate(Title, "C[0-9]+")[1], str_locate(Title, "C[0-9]+")[2]+1),
           Year = ifelse(Cycle == "C1", 1999, ifelse(Cycle == "C2", 2009, ifelse(Cycle == "C3", 2016, NA))),
           Group = substr(Title, 1,3)) %>% 
    arrange(Group, Classes, Year) 
  
  if(mg) resultsbyallo <- resultsbyallo[,c("Group", "Year", "Classes", "AIC",	"BIC",	"aBIC",	"Entropy")]
  else resultsbyallo <- resultsbyallo[,c("Group", "Year", "Classes", "AIC",	"BIC",	"aBIC",	"Entropy", "min_N",	"max_N", "min_prob", "max_prob")] 
  
  resultsbyallo %>% arrange(as.character(Group), Classes, Year) %>% 
    mutate(Year = cell_spec(Year, color = ifelse(Entropy > 0.8, "black", "blue")),
           Classes = cell_spec(Classes, color = ifelse(Entropy > 0.8, "black", "blue")),
           AIC = cell_spec(AIC, color = ifelse(Entropy > 0.8, "black", "blue")),
           BIC = cell_spec(BIC, color = ifelse(Entropy > 0.8, "black", "blue")),
           aBIC = cell_spec(aBIC, color = ifelse(Entropy > 0.8, "black", "blue")),
           Entropy = cell_spec(Entropy, color = ifelse(Entropy > 0.8, "black", "blue"))) %>% 
    kbl(caption = "Model fit statistics", booktabs = TRUE, longtable = TRUE, row.names = FALSE, escape = FALSE) %>%
    kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"), 
                  position = "left", font_size = 11, full_width = FALSE) %>% 
    #column_spec(3:5, width = "8em") %>%  
    collapse_rows(1) %>% print()
  cat('\n')
  cat('\n')
  # if(mg) {
  #   resultsbyall<-tidyr::gather(resultsbyallo, "Fit", "Value", 3:6)
  #   dummy = data.frame(Classes = NA, Fit=rep(c("AIC", "BIC", "aBIC", "Entropy"), each=2), 
  #                      Value=c(150000,350000, 150000,350000, 150000,350000, 0.5 , 1))
  # } else {
  #   resultsbyall<-tidyr::gather(resultsbyallo, "Fit", "Value", 3:10)
  #   dummy = data.frame(Classes = NA, Fit=rep(c("AIC", "BIC", "aBIC", "Entropy", "min_N",	"max_N",	"min_prob",	"max_prob"), each=2), 
  #                      Value=c(150000,350000, 150000,350000, 150000,350000, 0.5 , 1, 0,20000, 0,100000,0.5,0.9,0.8,1))
  #   
  # }
  # 
  #   fit.plot <- ggplot(resultsbyall) + 
  #   geom_point(aes(x = factor(Classes), y = Value), size = 2) +
  #   geom_line(aes(factor(Classes), Value, group = 1)) + 
  #   facet_grid(Fit ~ Year, scales = "free") +
  #   geom_blank(data = dummy, aes(Classes, Value)) +
  #   scale_fill_grey() + theme_bw() +
  #     theme(axis.text.y = element_text(size = 7)) +
  #   scale_y_continuous(n.breaks = 3) +
  #   labs(x = "Classes", y = "", title = "Model fit for all scales") 
  # print(fit.plot)
  # cat('\n')
  # cat('\n')
}


HighProb <- function(lc5, orden = c(1:length(levels(factor(lc5$param)))), title = NULL){
  lc5$param <- factor(str_replace(lc5$param,"GND","bT_GNDREQ"))
  lc5$param <- factor(lc5$param, levels = levels(lc5$param)[orden])
  
  lc5$Cycle <- factor(lc5$Cycle, labels = c("1999", "2009", "2016"))
  labels_x <- NULL
  for (each in levels(lc5$param)){
    labels_x[each] <- attr(ISC_lvRlca[[each]], "label")
  }
  labels_x <- unlist(labels_x)
  pc5 <- lc5 %>%
    ggplot() +
    geom_point(aes(x = param, y = value, group = category, color = category, shape = Cycle), size = 2) +
    geom_line(aes(param, value, group = interaction(category,Cycle), color = category)) +
    scale_fill_grey() + theme_bw() +
    ggtitle(title) +
    labs(y="Response probabilities", color = "Response category")  +
    facet_grid(Class ~ ., switch = "y", labeller = label_wrap_gen(20)) +
    theme(legend.position = "top", legend.box="vertical",
          strip.text.y = element_text(size = 8),
          legend.spacing.y = unit(-0.2, 'cm'),
          axis.title.x = element_blank(), 
          axis.text.y = element_text(size = 8),
          legend.title = element_text(size = 8), 
          axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5, hjust = 0)) +
    scale_y_continuous(breaks = c(0.25,0.5,0.75))  +
    scale_color_brewer(type = "qual", palette = "Dark2", labels = c("Disagree", "Agree")) +
    scale_x_discrete(labels = str_wrap(labels_x,15))
  print(pc5)
  cat('\n')
  cat('\n')
}

HighProbMG <- function(lcg5, sizg5, orden = c(1:length(levels(factor(lcg5$param)))), title = NULL, n = 1:2){
  
  lcg5f <- lcg5 %>% group_by(Cycle, Class, param) %>% filter(category == 2) %>% #Probabilities for agree
    mutate(ClassN = str_remove(ClassN,"\n"))
  sizg5 <- sizg5 %>%
    mutate(ClassN = str_remove(ClassN,"\n"),
           param = "GND6")
  siz <- left_join(lcg5f, sizg5, by = c("Cycle", "param", "Group", "ClassN")) %>% 
    arrange(Cycle, Group, ClassN, param) %>% 
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
      facet_grid(Cycle ~ Group, switch = "y", labeller = label_wrap_gen(20)) +
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
           legend.title = element_blank(), 
           axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5, hjust = 0)) +
      guides(color=guide_legend(ncol=1,byrow=TRUE)) 
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

ByGroupHighProbMG <- function(lcg5, sizg5, orden = c(1:length(levels(factor(lcg5$param)))), title = NULL, n = 1:2){
  
  lcg5f <- lcg5 %>% group_by(Cycle, CNT, Class, param) %>% filter(category == 2) %>% #Probabilities for agree
    mutate(ClassN = str_remove(ClassN,"\n"))
  sizg5 <- sizg5 %>%
    mutate(ClassN = str_remove(ClassN,"\n"),
           param = "GND6")
  siz <- left_join(lcg5f, sizg5, by = c("CNT", "Cycle", "Group","param", "ClassN")) %>% 
    arrange(CNT, Cycle, ClassN, param) %>% 
    group_by(CNT, Cycle, param) %>% mutate(dif = abs(value - lag(value))) %>% ungroup() %>% 
    mutate(rudge = ifelse(!is.na(per) & (dif >= 0.1 | is.na(dif)), 0.1, ifelse(!is.na(per) & dif < 0.1, -0.2, NA)),
           param = factor(param, levels = levels(lcg5$param)[orden]))
  
  for(j in sort(as.character(unique(lcg5f$CNT)))) {
  pc5 <- siz %>% filter(CNT  == j) %>% 
    ggplot() +
    geom_point(aes(x = param, y = value, group = ClassN, color = ClassN), size = 0.5) +
    geom_line(aes(param, value, group = interaction(ClassN, Cycle), color = ClassN)) +
    theme_bw() +
    ggtitle(paste(title," - ", j)) +
    labs(y="Response probabilities", color = "Response category") 
  
  if (length(n) < 3) {
    pc5 <- pc5 + 
      geom_text(aes(x = param, y = value, label = scales::percent(per, accuracy = 0.1), color = ClassN), size = 2.5, 
                nudge_x = 0.25, nudge_y = 0) +
      facet_grid(Cycle ~ Group, switch = "y", labeller = label_wrap_gen(20)) +
      scale_y_continuous(breaks = c(0.25,0.5,0.75))  +
      scale_color_brewer(type = "qual", palette = "Dark2") 
    
  } else {
    pc5 <- pc5 +  
    geom_text(aes(x = param, y = value, label = scales::percent(per, accuracy = 0.1), color = ClassN), size = 3, 
              nudge_x = -0.25, nudge_y = siz[siz$CNT  == j, "rudge"]$rudge) +
    facet_grid(Cycle ~ Group, switch = "y", labeller = label_wrap_gen(20)) +
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
            legend.title = element_blank(), 
            axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5, hjust = 0)) +
      guides(color=guide_legend(ncol=1,byrow=TRUE)) 
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
}


VarClass <- function(lc){
  lc$param <- str_replace(lc$param,"GND","bT_GNDREQ")
  a <- levels(factor(lc$param))
  a <- a[order(a)[c(5,2,1,3,4,6)]]
  labels <- NULL
  for (each in a){
    labels[each] <- str_remove(attr(ISC_lvRlca[[each]], "label"), 
                               "Rights and Responsibilities/Rights and responsibilities/|Rights and Responsibilities/Roles women and men/|Moving/<Immigrants> |Moving/<Immigrant> ")
  }
  lc$param <- factor(lc$param, levels = a, labels = labels)
  return(lc)
}
