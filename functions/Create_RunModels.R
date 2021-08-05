library(MplusAutomation)
ds_lc <- data_model %>% 
  dplyr::select(all_of(sample), all_of(Scales), IDSTUD, COUNTRY, CYCLE) 

remlabclass <- function(ces){
  for (each in colnames(ces)){
    if ("labelled" %in% class(ces[[each]])){
      class(ces[[each]]) = c("numeric")
      attr(ces[[each]], "levels") <- NULL
    }
    attr(ces[[each]], "label") <- NULL
  }
  return(ces)
}
ds_lc0 <- remlabclass(ds_lc)

#----------------By country scales together by CYCLE ----------------------
for (j in 3:3) { #input file for each CYCLE 1:3
  data1 <- ds_lc0 %>%  filter(CYCLE == paste0("C",j)) %>% 
    dplyr::select(all_of(sample), all_of(ScalesGND), IDSTUD, COUNTRY) %>%
    mutate_if(is.factor, ~ as.numeric(.x)) %>%
    data.frame()
  
  cnt <- unique(data1[,c("COUNTRY","id_k")]) %>% 
    arrange(as.character(COUNTRY))
  
  data1 <- data1  %>% dplyr::select(-COUNTRY)
  prepareMplusData(df = data1,
             filename = paste0("data/MplusModels/ByCountry/GNDDtaC",j,".dat"), 
             interactive =FALSE)
  
  for(c in 1:nrow(cnt)){
    data <- data1 %>%  filter(id_k == cnt$id_k[c])
    
    lapply(1:6, function(k) { #input file for different number of classes
      fileConn <- file(paste0("data/MplusModels/ByCountry/GNDlca_",
                              cnt$COUNTRY[c],"_C",j,"cl",
                              sprintf("%d", k),".inp"))
      writeLines(c(
        paste0("TITLE: ", cnt$COUNTRY[c], "GND LCA - C", j,
               " with ", k ," classes;"),
        "DATA: ",
        paste0("FILE = GNDDtaC",j,".dat;"),
        "",
        "VARIABLE: ",
        paste0("NAMES = ", paste(colnames(data), collapse = "\n"),";"),
        "IDVARIABLE = IDSTUD;",
        paste0("USEVARIABLES = ", 
               paste(colnames(data)[grepl('^GND', colnames(data))], 
                     collapse = "\n"),";"),
        paste0("USEOBSERVATIONS ARE id_k EQ ", cnt$id_k[c], ";"),
        paste0("CATEGORICAL = ", 
               paste(colnames(data)[grepl('^GND', colnames(data))], 
                     collapse = "\n"),";"),
        "MISSING = .;",
        paste0("CLASSES = ",sprintf("c(%d);", k)),
        "WEIGHT = ws;",
        "STRATIFICATION = id_s;",
        "CLUSTER = id_j;",
        " ",
        "ANALYSIS:",
        "TYPE = COMPLEX MIXTURE;",
        "PROCESSORS = 4;",
        "STARTS = 1000 250;",
        "STITERATIONS = 20;",
        "STSEED = 288;",
        "",
        "MODEL:",
        "%OVERALL%",
        " ",
        "OUTPUT: ",
        "TECH10",
        "TECH11",
        "SVALUES",
        ";",
        "",
        "SAVEDATA:",
        paste0("FILE = Prob_", cnt$COUNTRY[c] ,
               "_GNDlca_C", j,"cl", k,".dat;"),
        "SAVE = CPROBABILITIES;"
        
      ), fileConn)
      close(fileConn)
    })
  }
}

runModels(target = "data/MplusModels/ByCountry", recursive = TRUE, 
          replaceOutfile = "never") #modifiedDate
ByCountry_GND <- readModels(target = "data/MplusModels/ByCountry", 
                            recursive = TRUE, 
                            filefilter = "GNDlca_[A-Z]{3}_C3cl")
ByCountry_ETH <- readModels(target = "data/MplusModels/ByCountry", 
                            recursive = TRUE, 
                            filefilter = "ETHlca_[A-Z]{3}_C3cl")

save(ByCountry_GND,
     ByCountry_ETH,
     file = "data/MplusModels_ByCountry.RData")

for (j in 3:3) { #input file for each CYCLE 1:3
  data1 <- ds_lc0 %>%  filter(CYCLE == paste0("C",j)) %>% 
    dplyr::select(all_of(sample), all_of(ScalesETH), IDSTUD, COUNTRY) %>%
    mutate_if(is.factor, ~ as.numeric(.x)) %>%
    data.frame()
  
  cnt <- unique(data1[,c("COUNTRY","id_k")]) %>% arrange(as.character(COUNTRY))
  data1 <- data1  %>% dplyr::select(-COUNTRY)
  prepareMplusData(df = data1,
                   filename = paste0("data/MplusModels/ByCountry/ETHDtaC",j,".dat"), interactive =FALSE)
  
  for(c in 1:nrow(cnt)){
    data <- data1 %>%  filter(id_k == cnt$id_k[c])
    
    lapply(1:6, function(k) { #input file for different number of classes
      fileConn <- file(paste0("data/MplusModels/ByCountry/ETHlca_",cnt$COUNTRY[c],"_C",j,"cl",sprintf("%d", k),".inp"))
      writeLines(c(
        paste0("TITLE: ", cnt$COUNTRY[c], "ETH LCA - C", j," with ", k ," classes;"),
        "DATA: ",
        paste0("FILE = ETHDtaC",j,".dat;"),
        "",
        "VARIABLE: ",
        paste0("NAMES = ", paste(colnames(data), collapse = "\n"),";"),
        "IDVARIABLE = IDSTUD;",
        paste0("USEVARIABLES = ", paste(colnames(data)[grepl('^ETH', colnames(data))], collapse = "\n"),";"),
        paste0("USEOBSERVATIONS ARE id_k EQ ", cnt$id_k[c], ";"),
        paste0("CATEGORICAL = ", paste(colnames(data)[grepl('^ETH', colnames(data))], collapse = "\n"),";"),
        "MISSING = .;",
        paste0("CLASSES = ",sprintf("c(%d);", k)),
        "WEIGHT = ws;",
        "STRATIFICATION = id_s;",
        "CLUSTER = id_j;",
        " ",
        "ANALYSIS:",
        "TYPE = COMPLEX MIXTURE;",
        "PROCESSORS = 4;",
        "STARTS = 1000 250;",
        "STITERATIONS = 20;",
        "STSEED = 288;",
        "",
        "MODEL:",
        "%OVERALL%",
        " ",
        "OUTPUT: ",
        "TECH10",#for bivariate model fit information
        "TECH11",
        #"SAMP",
        #"TECH14;", Time consuming tests
        #"CINTERVAL",
        "SVALUES",
        ";",
        "",
        "SAVEDATA:",
        paste0("FILE = Prob_", cnt$COUNTRY[c] ,"_ETHlca_C", j,"cl", k,".dat;"),
        "SAVE = CPROBABILITIES;"
        
      ), fileConn)
      close(fileConn)
    })
  }
}
for (j in 3:3) { #input file for each CYCLE 1:3
  data1 <- ds_lc0 %>%  filter(CYCLE == paste0("C",j)) %>% 
    dplyr::select(all_of(sample), all_of(ScalesGND), all_of(ScalesETH), IDSTUD, COUNTRY) %>%
    mutate_if(is.factor, ~ as.numeric(.x)) %>%
    data.frame()
  
  cnt <- unique(data1[,c("COUNTRY","id_k")]) %>% arrange(as.character(COUNTRY))
  data1 <- data1  %>% dplyr::select(-COUNTRY)
  prepareMplusData(df = data1,
                   filename = paste0("data/MplusModels/ByCountry/GNDETHDtaC",j,".dat"), interactive =FALSE)
  
  for(c in 1:nrow(cnt)){
    data <- data1 %>%  filter(id_k == cnt$id_k[c])
    
    lapply(1:6, function(k) { #input file for different number of classes
      fileConn <- file(paste0("data/MplusModels/ByCountry/GNDETHlca_",cnt$COUNTRY[c],"_C",j,"cl",sprintf("%d", k),".inp"))
      writeLines(c(
        paste0("TITLE: ", cnt$COUNTRY[c], "GNDETH LCA - C", j," with ", k ," classes;"),
        "DATA: ",
        paste0("FILE = GNDETHDtaC",j,".dat;"),
        "",
        "VARIABLE: ",
        paste0("NAMES = ", paste(colnames(data), collapse = "\n"),";"),
        "IDVARIABLE = IDSTUD;",
        paste0("USEVARIABLES = ", paste(colnames(data)[grepl('^ETH|GND', colnames(data))], collapse = "\n"),";"),
        paste0("USEOBSERVATIONS ARE id_k EQ ", cnt$id_k[c], ";"),
        paste0("CATEGORICAL = ", paste(colnames(data)[grepl('^ETH|GND', colnames(data))], collapse = "\n"),";"),
        "MISSING = .;",
        paste0("CLASSES = ",sprintf("c(%d);", k)),
        "WEIGHT = ws;",
        "STRATIFICATION = id_s;",
        "CLUSTER = id_j;",
        " ",
        "ANALYSIS:",
        "TYPE = COMPLEX MIXTURE;",
        "PROCESSORS = 4;",
        "STARTS = 1000 250;",
        "STITERATIONS = 20;",
        "STSEED = 288;",
        "",
        "MODEL:",
        "%OVERALL%",
        " ",
        "OUTPUT: ",
        "TECH10",#for bivariate model fit information
        "TECH11",
        #"SAMP",
        #"TECH14;", Time consuming tests
        #"CINTERVAL",
        "SVALUES",
        ";",
        "",
        "SAVEDATA:",
        paste0("FILE = Prob_", cnt$COUNTRY[c] ,"_GNDETHlca_C", j,"cl", k,".dat;"),
        "SAVE = CPROBABILITIES;"
        
      ), fileConn)
      close(fileConn)
    })
  }
}
#------


#----------------All scales together by CYCLE--------------------------------------
for (j in 3:3) { #input file for each CYCLE 1:3
  data <- ds_lc0 %>% filter(CYCLE == paste0("C",j)) %>%
    dplyr::select(all_of(sample), all_of(ScalesGND), IDSTUD) %>%
    mutate_if(is.factor, ~ as.numeric(.x)) %>%
    data.frame()
  prepareMplusData(df = data,
                   filename = paste0("data/MplusModels/LCA/GND_Dta_C",j,".dat"), interactive =FALSE)
  cas <- list()
  lapply(1:7, function(k) { #input file for different number of classes
    for(c in 1:k){
      cas[c] <- paste0("%c#",c,"%")
    }
    fileConn <- file(paste0("data/MplusModels/LCA/GND_lca_C",j,"cl",sprintf("%d", k),".inp"))
    writeLines(c(
      paste0("TITLE: LCA C", j," GND with ", k ," classes;"),
      "DATA: ",
      paste0("FILE = GND_Dta_C",j,".dat;"),
      "",
      "VARIABLE: ",
      paste0("NAMES = ", paste(colnames(data), collapse = "\n"),";"),
      "IDVARIABLE = IDSTUD;",
      paste0("USEVARIABLES = ", paste(colnames(data)[grepl('^GND', colnames(data))], collapse = "\n"),";"),
      paste0("CATEGORICAL = ", paste(colnames(data)[grepl('^GND', colnames(data))], collapse = "\n"),";"),
      "MISSING = .;",
      paste0("CLASSES = ",sprintf("c(%d);", k)),
      "WEIGHT = ws;",
      "STRATIFICATION = id_s;",
      "CLUSTER = id_j;",
      " ",
      "ANALYSIS:",
      "TYPE = COMPLEX MIXTURE;",
      "PROCESSORS = 4;",
      "STARTS = 1000 250;",
      "STITERATIONS = 20;",
      "STSEED = 288;",
      " ",
      "OUTPUT: ",
      "TECH10",#for bivariate model fit information
      "TECH11",
      #"SAMP",
      "TECH14;", #Time consuming tests
      #"CINTERVAL",
      "SVALUES",
      ";",
      "",
      "SAVEDATA:",
      paste0("FILE = GND_Prob_C", j,"cl", k,".dat;"),
      "SAVE = CPROBABILITIES;"
      
    ), fileConn)
    close(fileConn)
  })
}
for (j in 3:3) { #input file for each CYCLE 1:3
  data <- ds_lc0 %>% filter(CYCLE == paste0("C",j)) %>%
    dplyr::select(all_of(sample), all_of(ScalesETH), IDSTUD) %>%
    mutate_if(is.factor, ~ as.numeric(.x)) %>%
    data.frame()
  prepareMplusData(df = data,
                   filename = paste0("data/MplusModels/LCA/ETH_Dta_C",j,".dat"), interactive =FALSE)
  cas <- list()
  lapply(1:7, function(k) { #input file for different number of classes
    for(c in 1:k){
      cas[c] <- paste0("%c#",c,"%")
    }
    fileConn <- file(paste0("data/MplusModels/LCA/ETH_lca_C",j,"cl",sprintf("%d", k),".inp"))
    writeLines(c(
      paste0("TITLE: LCA C", j," ETH with ", k ," classes;"),
      "DATA: ",
      paste0("FILE = ETH_Dta_C",j,".dat;"),
      "",
      "VARIABLE: ",
      paste0("NAMES = ", paste(colnames(data), collapse = "\n"),";"),
      "IDVARIABLE = IDSTUD;",
      paste0("USEVARIABLES = ", paste(colnames(data)[grepl('^ETH', colnames(data))], collapse = "\n"),";"),
      paste0("CATEGORICAL = ", paste(colnames(data)[grepl('^ETH', colnames(data))], collapse = "\n"),";"),
      "MISSING = .;",
      paste0("CLASSES = ",sprintf("c(%d);", k)),
      "WEIGHT = ws;",
      "STRATIFICATION = id_s;",
      "CLUSTER = id_j;",
      " ",
      "ANALYSIS:",
      "TYPE = COMPLEX MIXTURE;",
      "PROCESSORS = 4;",
      "STARTS = 1000 250;",
      "STITERATIONS = 20;",
      "STSEED = 288;",
      " ",
      "OUTPUT: ",
      "TECH10",#for bivariate model fit information
      "TECH11",
      #"SAMP",
      "TECH14;", #Time consuming tests
      #"CINTERVAL",
      "SVALUES",
      ";",
      "",
      "SAVEDATA:",
      paste0("FILE = ETH_Prob_C", j,"cl", k,".dat;"),
      "SAVE = CPROBABILITIES;"
      
    ), fileConn)
    close(fileConn)
  })
}
for (j in 3:3) { #input file for each CYCLE 1:3
  data <- ds_lc0 %>% filter(CYCLE == paste0("C",j)) %>%
    dplyr::select(all_of(sample), all_of(ScalesGND), all_of(ScalesETH), IDSTUD) %>%
    mutate_if(is.factor, ~ as.numeric(.x)) %>%
    data.frame()
  prepareMplusData(df = data,
                   filename = paste0("data/MplusModels/LCA/GNDETH_Dta_C",j,".dat"), interactive =FALSE)
  cas <- list()
  lapply(1:9, function(k) { #input file for different number of classes
    for(c in 1:k){
      cas[c] <- paste0("%c#",c,"%")
    }
    fileConn <- file(paste0("data/MplusModels/LCA/GNDETH_lca_C",j,"cl",sprintf("%d", k),".inp"))
    writeLines(c(
      paste0("TITLE: LCA C", j," GNDETH with ", k ," classes;"),
      "DATA: ",
      paste0("FILE = GNDETH_Dta_C",j,".dat;"),
      "",
      "VARIABLE: ",
      paste0("NAMES = ", paste(colnames(data), collapse = "\n"),";"),
      "IDVARIABLE = IDSTUD;",
      paste0("USEVARIABLES = ", paste(colnames(data)[grepl('^ETH|GND', colnames(data))], collapse = "\n"),";"),
      paste0("CATEGORICAL = ", paste(colnames(data)[grepl('^ETH|GND', colnames(data))], collapse = "\n"),";"),
      "MISSING = .;",
      paste0("CLASSES = ",sprintf("c(%d);", k)),
      "WEIGHT = ws;",
      "STRATIFICATION = id_s;",
      "CLUSTER = id_j;",
      " ",
      "ANALYSIS:",
      "TYPE = COMPLEX MIXTURE;",
      "PROCESSORS = 4;",
      "STARTS = 1000 250;",
      "STITERATIONS = 20;",
      "STSEED = 288;",
      " ",
      "OUTPUT: ",
      "TECH10",#for bivariate model fit information
      "TECH11",
      #"SAMP",
      "TECH14;", #Time consuming tests
      #"CINTERVAL",
      "SVALUES",
      ";",
      "",
      "SAVEDATA:",
      paste0("FILE = GNDETH_Prob_C", j,"cl", k,".dat;"),
      "SAVE = CPROBABILITIES;"
      
    ), fileConn)
    close(fileConn)
  })
}
#----------------C.Hom Multigroup COUNTRY by CYCLE--------------------------------------
for (j in 3:3) { #input file for each CYCLE 1:3
  
  data <- ds_lc0 %>%  filter(CYCLE == paste0("C",j)) %>%
    dplyr::select(all_of(sample), all_of(ScalesGND), IDSTUD, COUNTRY) %>%
    mutate_if(is.factor, ~ as.numeric(.x)) %>%
    data.frame()
  
  # list of countries for multigroup model
  t <- cbind(unique(data[, "id_k"]),"!", unique(ds_lc0$COUNTRY), 
             g = rownames(unique(ds_lc0$COUNTRY)))
  tc <- textConnection("str", "w")
  sink(tc)   # divert output to tc connection
  print(t, quote = FALSE)  # print in str string instead of console
  sink()     # set the output back to console
  close(tc)
  str <- substr(str,floor(length(str)/10)+5,nchar(str[1])) # we get rid of the row numbers that come with print
  str <- paste0(str[-1],collapse="\n")        # we build a proper unique string with your pipes and new lines
  data<- data %>% select(-COUNTRY)
  cas <- data.frame(NULL)
  lapply(4:6, function(k) { #input file for different number of classes
    
    id = 0
    for(c in 1:k){
      
      mat <- data.frame(NULL)
      id = id + 1
      cast<- data.frame(paste0("%c#",c,"%"))
      vars<-sort(colnames(data)[grepl('^GND', colnames(data))])
      mat[1,1] <- id
      mat[1,2] <- ifelse(c == 1, paste0("[",vars[1],"$1-",vars[length(vars)],"$1] (91-9",length(vars),");"),
                         paste0("[",vars[1],"$1-",vars[length(vars)],"$1];"))
      cas <- rbind(cas,cbind(t(cast),mat))
    }
    
    casr <- reshape2::melt(cas, id.vars = "V1") %>% arrange(V1) 
    casr <- data.frame(casr[,c(-1,-2)])
    
    casc <- textConnection("castxPH", "w")
    sink(casc)   # divert output to tc connection
    print(casr)  # print in str string instead of console
    sink()     # set the output back to console
    close(casc)
    castxPH <- substr(castxPH,2,nchar(castxPH[1])) # we get rid of the row numbers that come with print
    castxPH <- paste0(castxPH[-1],collapse="\n")
    
    fileConn <- file(paste0("data/MplusModels/CountryMG/GND_MGCnty_C",j,"cl",sprintf("%d", k),"_3CHom.inp"))
    writeLines(c(
      paste0("TITLE:C.Hom MG Country LCA GND C", j," with ", k ," classes;"),
      "DATA: ",
      paste0("FILE = GND_DtaC",j,".dat;"),
      "",
      "VARIABLE: ",
      paste0("NAMES = ", paste(colnames(data), collapse = "\n"),";"),
      "IDVARIABLE = IDSTUD;",
      paste0("USEVARIABLES = ", paste(colnames(data)[grepl('^GND', colnames(data))], collapse = "\n"),";"),
      paste0("CATEGORICAL = ", paste(colnames(data)[grepl('^GND', colnames(data))], collapse = "\n"),";"),
      "MISSING = .;",
      paste0("CLASSES = g(",length(unique(data$id_k)),") ",sprintf("c(%d);", k)),
      paste0("KNOWNCLASS = g(id_k ="), 
      paste(str, collapse = "\n"),
      ");",
      "WEIGHT = ws;",
      "STRATIFICATION = id_s;",
      "CLUSTER = id_j;",
      " ",
      "ANALYSIS:",
      "TYPE = COMPLEX MIXTURE;",
      "PROCESSORS = 4;",
      "STARTS = 1000 250;",
      "STITERATIONS = 20;",
      "STSEED = 288;",
      "",
      "MODEL:",
      "%OVERALL%",
      "Model c:",
      " ",
      castxPH,
      " ",
      "OUTPUT: ",
      "TECH10",#for bivariate model fit information
      #"TECH11",
      #"SAMP",
      #"TECH14;", Time consuming tests
      #"CINTERVAL",
      "SVALUES",
      ";",
      "",
      "SAVEDATA:",
      paste0("FILE = GND_Prob_MGCntry_C", j,"cl", k,"_3CHom.dat;"),
      "SAVE = CPROBABILITIES;"
      
    ), fileConn)
    close(fileConn)
  })
}
for (j in 3:3) { #input file for each CYCLE 1:3
  
  data <- ds_lc0 %>%  filter(CYCLE == paste0("C",j)) %>%
    dplyr::select(all_of(sample), all_of(ScalesETH), IDSTUD, COUNTRY) %>%
    mutate_if(is.factor, ~ as.numeric(.x)) %>%
    data.frame()
  
  # list of countries for multigroup model
  t <- cbind(unique(data[, "id_k"]),"!", unique(ds_lc0$COUNTRY), 
             g = rownames(unique(ds_lc0$COUNTRY)))
  tc <- textConnection("str", "w")
  sink(tc)   # divert output to tc connection
  print(t, quote = FALSE)  # print in str string instead of console
  sink()     # set the output back to console
  close(tc)
  str <- substr(str,floor(length(str)/10)+5,nchar(str[1])) # we get rid of the row numbers that come with print
  str <- paste0(str[-1],collapse="\n")        # we build a proper unique string with your pipes and new lines
  data<- data %>% select(-COUNTRY)
  cas <- data.frame(NULL)
  lapply(3:5, function(k) { #input file for different number of classes
    
    id = 0
    for(c in 1:k){
      
      mat <- data.frame(NULL)
      id = id + 1
      cast<- data.frame(paste0("%c#",c,"%"))
      vars<-sort(colnames(data)[grepl('^ETH', colnames(data))])
      mat[1,1] <- id
      mat[1,2] <- ifelse(c == 1, paste0("[",vars[1],"$1-",vars[length(vars)],"$1] (91-9",length(vars),");"),
                         paste0("[",vars[1],"$1-",vars[length(vars)],"$1];"))
      cas <- rbind(cas,cbind(t(cast),mat))
    }
    
    casr <- reshape2::melt(cas, id.vars = "V1") %>% arrange(V1) 
    casr <- data.frame(casr[,c(-1,-2)])
    
    casc <- textConnection("castxPH", "w")
    sink(casc)   # divert output to tc connection
    print(casr)  # print in str string instead of console
    sink()     # set the output back to console
    close(casc)
    castxPH <- substr(castxPH,2,nchar(castxPH[1])) # we get rid of the row numbers that come with print
    castxPH <- paste0(castxPH[-1],collapse="\n")
    
    fileConn <- file(paste0("data/MplusModels/CountryMG/ETH_MGCnty_C",j,"cl",sprintf("%d", k),"_3CHom.inp"))
    writeLines(c(
      paste0("TITLE:C.Hom MG Country LCA ETH C", j," with ", k ," classes;"),
      "DATA: ",
      paste0("FILE = ETH_DtaC",j,".dat;"),
      "",
      "VARIABLE: ",
      paste0("NAMES = ", paste(colnames(data), collapse = "\n"),";"),
      "IDVARIABLE = IDSTUD;",
      paste0("USEVARIABLES = ", paste(colnames(data)[grepl('^ETH', colnames(data))], collapse = "\n"),";"),
      paste0("CATEGORICAL = ", paste(colnames(data)[grepl('^ETH', colnames(data))], collapse = "\n"),";"),
      "MISSING = .;",
      paste0("CLASSES = g(",length(unique(data$id_k)),") ",sprintf("c(%d);", k)),
      paste0("KNOWNCLASS = g(id_k ="), 
      paste(str, collapse = "\n"),
      ");",
      "WEIGHT = ws;",
      "STRATIFICATION = id_s;",
      "CLUSTER = id_j;",
      " ",
      "ANALYSIS:",
      "TYPE = COMPLEX MIXTURE;",
      "PROCESSORS = 4;",
      "STARTS = 1000 250;",
      "STITERATIONS = 20;",
      "STSEED = 288;",
      "",
      "MODEL:",
      "%OVERALL%",
      "Model c:",
      " ",
      castxPH,
      " ",
      "OUTPUT: ",
      "TECH10",#for bivariate model fit information
      #"TECH11",
      #"SAMP",
      #"TECH14;", Time consuming tests
      #"CINTERVAL",
      "SVALUES",
      ";",
      "",
      "SAVEDATA:",
      paste0("FILE = ETH_Prob_MGCntry_C", j,"cl", k,"_3CHom.dat;"),
      "SAVE = CPROBABILITIES;"
      
    ), fileConn)
    close(fileConn)
  })
}
for (j in 3:3) { #input file for each CYCLE 1:3
  
  data <- ds_lc0 %>%  filter(CYCLE == paste0("C",j)) %>%
    dplyr::select(all_of(sample), all_of(ScalesGND), all_of(ScalesETH), IDSTUD, COUNTRY) %>%
    mutate_if(is.factor, ~ as.numeric(.x)) %>%
    data.frame()
  
  # list of countries for multigroup model
  t <- cbind(unique(data[, "id_k"]),"!", unique(ds_lc0$COUNTRY), 
             g = rownames(unique(ds_lc0$COUNTRY)))
  tc <- textConnection("str", "w")
  sink(tc)   # divert output to tc connection
  print(t, quote = FALSE)  # print in str string instead of console
  sink()     # set the output back to console
  close(tc)
  str <- substr(str,floor(length(str)/10)+5,nchar(str[1])) # we get rid of the row numbers that come with print
  str <- paste0(str[-1],collapse="\n")        # we build a proper unique string with your pipes and new lines
  data<- data %>% select(-COUNTRY)
  cas <- data.frame(NULL)
  lapply(7:7, function(k) { #input file for different number of classes
    
    id = 0
    for(c in 1:k){
      
      mat <- data.frame(NULL)
      id = id + 1
      cast<- data.frame(paste0("%c#",c,"%"))
      vars<-colnames(data)[grepl('^ETH|GND', colnames(data))]
      mat[1,1] <- id
      mat[1,2] <- ifelse(c == 1, paste0("[",vars[1],"$1-",vars[length(vars)],"$1] (91-9",length(vars),");"),
                         paste0("[",vars[1],"$1-",vars[length(vars)],"$1];"))
      cas <- rbind(cas,cbind(t(cast),mat))
    }
    
    casr <- reshape2::melt(cas, id.vars = "V1") %>% arrange(V1) 
    casr <- data.frame(casr[,c(-1,-2)])
    
    casc <- textConnection("castxPH", "w")
    sink(casc)   # divert output to tc connection
    print(casr)  # print in str string instead of console
    sink()     # set the output back to console
    close(casc)
    castxPH <- substr(castxPH,3,nchar(castxPH[1])) # we get rid of the row numbers that come with print
    castxPH <- paste0(castxPH[-1],collapse="\n")
    
    fileConn <- file(paste0("data/MplusModels/CountryMG/GNDETH_MGCnty_C",j,"cl",sprintf("%d", k),"_3CHom.inp"))
    writeLines(c(
      paste0("TITLE:C.Hom MG Country LCA GNDETH C", j," with ", k ," classes;"),
      "DATA: ",
      paste0("FILE = GNDETH_DtaC",j,".dat;"),
      "",
      "VARIABLE: ",
      paste0("NAMES = ", paste(colnames(data), collapse = "\n"),";"),
      "IDVARIABLE = IDSTUD;",
      paste0("USEVARIABLES = ", paste(colnames(data)[grepl('^ETH|GND', colnames(data))], collapse = "\n"),";"),
      paste0("CATEGORICAL = ", paste(colnames(data)[grepl('^ETH|GND', colnames(data))], collapse = "\n"),";"),
      "MISSING = .;",
      paste0("CLASSES = g(",length(unique(data$id_k)),") ",sprintf("c(%d);", k)),
      paste0("KNOWNCLASS = g(id_k ="), 
      paste(str, collapse = "\n"),
      ");",
      "WEIGHT = ws;",
      "STRATIFICATION = id_s;",
      "CLUSTER = id_j;",
      " ",
      "ANALYSIS:",
      "TYPE = COMPLEX MIXTURE;",
      "PROCESSORS = 4;",
      "STARTS = 1000 250;",
      "STITERATIONS = 20;",
      "STSEED = 288;",
      "",
      "MODEL:",
      "%OVERALL%",
      "Model c:",
      " ",
      castxPH,
      " ",
      "OUTPUT: ",
      "TECH10",#for bivariate model fit information
      #"TECH11",
      #"SAMP",
      #"TECH14;", Time consuming tests
      #"CINTERVAL",
      "SVALUES",
      ";",
      "",
      "SAVEDATA:",
      paste0("FILE = GNDETH_Prob_MGCntry_C", j,"cl", k,"_3CHom.dat;"),
      "SAVE = CPROBABILITIES;"
      
    ), fileConn)
    close(fileConn)
  })
}


#----------------P.Hom Multigroup COUNTRY by CYCLE--------------------------------------
for (j in 3:3) { #input file for each CYCLE 1:3
  data <- ds_lc0 %>%  filter(CYCLE == paste0("C",j)) %>%
    dplyr::select(all_of(sample), all_of(ScalesGND), IDSTUD, COUNTRY) %>%
    mutate_if(is.factor, ~ as.numeric(.x)) %>%
    data.frame()
  
  # list of countries for multigroup model
  t <- cbind(unique(data[, "id_k"]),"!", unique(ds_lc0$COUNTRY), 
             g = rownames(unique(ds_lc0$COUNTRY)))
  tc <- textConnection("str", "w")
  sink(tc)   # divert output to tc connection
  print(t, quote = FALSE)  # print in str string instead of console
  sink()     # set the output back to console
  close(tc)
  str <- substr(str,floor(length(str)/10)+5,nchar(str[1])) # we get rid of the row numbers that come with print
  str <- paste0(str[-1],collapse="\n")        # we build a proper unique string with your pipes and new lines
  data<- data %>% select(-COUNTRY)
  # Identify restrictions for partial homogeneity model 
  cas <- data.frame(NULL)
  lapply(4:6, function(k) { #input file for different number of classes
    partial = 1:k 
    id = 0
    mat <- data.frame(NULL)
    for(g in 1:length(unique(data$id_k))){
      
      cast<- data.frame(outer(paste0("%g#",g), paste0(".c#",1:k,"%"), paste, sep=""))
      vars<-sort(colnames(data)[grepl('^GND', colnames(data))])
      for (c in 1:k) {
        n = length(vars)*id
        id = id + 1  
        for(v in 1:length(vars)){
          if (!is.na(partial[c])  & g != 1 & c %in% partial) {
            mat[c,1] <- id
            mat[c,v+1] <- cas[partial[c],v+2]
          } else {
            mat[c,1] <- id
            mat[c,v+1] <- paste0("[",vars[v],"$1] (",n + v,");")
          }
        }
      }
      cas <- rbind(cas,cbind(t(cast),mat))
    }
    casr <- reshape2::melt(cas, id.vars = "V1") %>% arrange(V1) 
    casr <- data.frame(casr[,c(-1,-2)])
    
    casc <- textConnection("castxPH", "w")
    sink(casc)   # divert output to tc connection
    print(casr)  # print in str string instead of console
    sink()     # set the output back to console
    close(casc)
    castxPH <- substr(castxPH,5+2,nchar(castxPH[1])) # we get rid of the row numbers that come with print
    castxPH <- paste0(castxPH[-1],collapse="\n")
    
    fileConn <- file(paste0("data/MplusModels/CountryMG/GND_MGCnty_C",j,"cl",sprintf("%d", k),"_2PHom.inp"))
    writeLines(c(
      paste0("TITLE: P.Hom MG Country LCA GND C", j," with ", k ," classes;"),
      "DATA: ",
      paste0("FILE = GND_DtaC",j,".dat;"),
      "",
      "VARIABLE: ",
      paste0("NAMES = ", paste(colnames(data), collapse = "\n"),";"),
      "IDVARIABLE = IDSTUD;",
      paste0("USEVARIABLES = ", paste(colnames(data)[grepl('^GND', colnames(data))], collapse = "\n"),";"),
      paste0("CATEGORICAL = ", paste(colnames(data)[grepl('^GND', colnames(data))], collapse = "\n"),";"),
      "MISSING = .;",
      paste0("CLASSES = g(",length(unique(data$id_k)),") ",sprintf("c(%d);", k)),
      paste0("KNOWNCLASS = g(id_k ="), 
      paste(str, collapse = "\n"),
      ");",
      "WEIGHT = ws;",
      "STRATIFICATION = id_s;",
      "CLUSTER = id_j;",
      " ",
      "ANALYSIS:",
      "TYPE = COMPLEX MIXTURE;",
      "PROCESSORS = 4;",
      "STARTS = 1000 250;",
      "STITERATIONS = 20;",
      "STSEED = 288;",
      "",
      "MODEL:",
      "%OVERALL%",
      "c ON g;",
      " ",
      castxPH,
      " ",
      "OUTPUT: ",
      "TECH10",#for bivariate model fit information
      #"TECH11",
      #"SAMP",
      #"TECH14;", Time consuming tests
      #"CINTERVAL",
      "SVALUES",
      ";",
      "",
      "SAVEDATA:",
      paste0("FILE = GND_Prob_MGCntry_C", j,"cl", k,"_2PHom.dat;"),
      "SAVE = CPROBABILITIES;"
      
    ), fileConn)
    close(fileConn)
  })
}
for (j in 3:3) { #input file for each CYCLE 1:3
  data <- ds_lc0 %>%  filter(CYCLE == paste0("C",j)) %>%
    dplyr::select(all_of(sample), all_of(ScalesETH), IDSTUD, COUNTRY) %>%
    mutate_if(is.factor, ~ as.numeric(.x)) %>%
    data.frame()
  
  # list of countries for multigroup model
  t <- cbind(unique(data[, "id_k"]),"!", unique(ds_lc0$COUNTRY), 
             g = rownames(unique(ds_lc0$COUNTRY)))
  tc <- textConnection("str", "w")
  sink(tc)   # divert output to tc connection
  print(t, quote = FALSE)  # print in str string instead of console
  sink()     # set the output back to console
  close(tc)
  str <- substr(str,floor(length(str)/10)+5,nchar(str[1])) # we get rid of the row numbers that come with print
  str <- paste0(str[-1],collapse="\n")        # we build a proper unique string with your pipes and new lines
  data<- data %>% select(-COUNTRY)
  # Identify restrictions for partial homogeneity model 
  cas <- data.frame(NULL)
  lapply(3:5, function(k) { #input file for different number of classes
    partial = 1:k 
    id = 0
    mat <- data.frame(NULL)
    for(g in 1:length(unique(data$id_k))){
      
      cast<- data.frame(outer(paste0("%g#",g), paste0(".c#",1:k,"%"), paste, sep=""))
      vars<-sort(colnames(data)[grepl('^ETH', colnames(data))])
      for (c in 1:k) {
        n = length(vars)*id
        id = id + 1  
        for(v in 1:length(vars)){
          if (!is.na(partial[c])  & g != 1 & c %in% partial) {
            mat[c,1] <- id
            mat[c,v+1] <- cas[partial[c],v+2]
          } else {
            mat[c,1] <- id
            mat[c,v+1] <- paste0("[",vars[v],"$1] (",n + v,");")
          }
        }
      }
      cas <- rbind(cas,cbind(t(cast),mat))
    }
    casr <- reshape2::melt(cas, id.vars = "V1") %>% arrange(V1) 
    casr <- data.frame(casr[,c(-1,-2)])
    
    casc <- textConnection("castxPH", "w")
    sink(casc)   # divert output to tc connection
    print(casr)  # print in str string instead of console
    sink()     # set the output back to console
    close(casc)
    castxPH <- substr(castxPH,5+2,nchar(castxPH[1])) # we get rid of the row numbers that come with print
    castxPH <- paste0(castxPH[-1],collapse="\n")
    
    fileConn <- file(paste0("data/MplusModels/CountryMG/ETH_MGCnty_C",j,"cl",sprintf("%d", k),"_2PHom.inp"))
    writeLines(c(
      paste0("TITLE: P.Hom MG Country LCA ETH C", j," with ", k ," classes;"),
      "DATA: ",
      paste0("FILE = ETH_DtaC",j,".dat;"),
      "",
      "VARIABLE: ",
      paste0("NAMES = ", paste(colnames(data), collapse = "\n"),";"),
      "IDVARIABLE = IDSTUD;",
      paste0("USEVARIABLES = ", paste(colnames(data)[grepl('^ETH', colnames(data))], collapse = "\n"),";"),
      paste0("CATEGORICAL = ", paste(colnames(data)[grepl('^ETH', colnames(data))], collapse = "\n"),";"),
      "MISSING = .;",
      paste0("CLASSES = g(",length(unique(data$id_k)),") ",sprintf("c(%d);", k)),
      paste0("KNOWNCLASS = g(id_k ="), 
      paste(str, collapse = "\n"),
      ");",
      "WEIGHT = ws;",
      "STRATIFICATION = id_s;",
      "CLUSTER = id_j;",
      " ",
      "ANALYSIS:",
      "TYPE = COMPLEX MIXTURE;",
      "PROCESSORS = 4;",
      "STARTS = 1000 250;",
      "STITERATIONS = 20;",
      "STSEED = 288;",
      "",
      "MODEL:",
      "%OVERALL%",
      "c ON g;",
      " ",
      castxPH,
      " ",
      "OUTPUT: ",
      "TECH10",#for bivariate model fit information
      #"TECH11",
      #"SAMP",
      #"TECH14;", Time consuming tests
      #"CINTERVAL",
      "SVALUES",
      ";",
      "",
      "SAVEDATA:",
      paste0("FILE = ETH_Prob_MGCntry_C", j,"cl", k,"_2PHom.dat;"),
      "SAVE = CPROBABILITIES;"
      
    ), fileConn)
    close(fileConn)
  })
}
for (j in 3:3) { #input file for each CYCLE 1:3
  data <- ds_lc0 %>%  filter(CYCLE == paste0("C",j)) %>%
    dplyr::select(all_of(sample), all_of(ScalesGND), all_of(ScalesETH), IDSTUD, COUNTRY) %>%
    mutate_if(is.factor, ~ as.numeric(.x)) %>%
    data.frame()
  
  # list of countries for multigroup model
  t <- cbind(unique(data[, "id_k"]),"!", unique(ds_lc0$COUNTRY), 
             g = rownames(unique(ds_lc0$COUNTRY)))
  tc <- textConnection("str", "w")
  sink(tc)   # divert output to tc connection
  print(t, quote = FALSE)  # print in str string instead of console
  sink()     # set the output back to console
  close(tc)
  str <- substr(str,floor(length(str)/10)+5,nchar(str[1])) # we get rid of the row numbers that come with print
  str <- paste0(str[-1],collapse="\n")        # we build a proper unique string with your pipes and new lines
  data<- data %>% select(-COUNTRY)
  # Identify restrictions for partial homogeneity model 
  cas <- data.frame(NULL)
  lapply(7:7, function(k) { #input file for different number of classes
    partial = 1:k 
    id = 0
    mat <- data.frame(NULL)
    for(g in 1:length(unique(data$id_k))){
      
      cast<- data.frame(outer(paste0("%g#",g), paste0(".c#",1:k,"%"), paste, sep=""))
      vars<-sort(colnames(data)[grepl('^ETH|GND', colnames(data))])
      for (c in 1:k) {
        n = length(vars)*id
        id = id + 1  
        for(v in 1:length(vars)){
          if (!is.na(partial[c])  & g != 1 & c %in% partial) {
            mat[c,1] <- id
            mat[c,v+1] <- cas[partial[c],v+2]
          } else {
            mat[c,1] <- id
            mat[c,v+1] <- paste0("[",vars[v],"$1] (",n + v,");")
          }
        }
      }
      cas <- rbind(cas,cbind(t(cast),mat))
    }
    casr <- reshape2::melt(cas, id.vars = "V1") %>% arrange(V1) 
    casr <- data.frame(casr[,c(-1,-2)])
    options(max.print = 2000)
    casc <- textConnection("castxPH", "w")
    sink(casc)   # divert output to tc connection
    print(casr)  # print in str string instead of console
    sink()     # set the output back to console
    close(casc)
    castxPH <- substr(castxPH,5+2,nchar(castxPH[1])) # we get rid of the row numbers that come with print
    castxPH <- paste0(castxPH[-1],collapse="\n")
    
    fileConn <- file(paste0("data/MplusModels/CountryMG/GNDETH_MGCnty_C",j,"cl",sprintf("%d", k),"_2PHom.inp"))
    writeLines(c(
      paste0("TITLE: P.Hom MG Country LCA GNDETH C", j," with ", k ," classes;"),
      "DATA: ",
      paste0("FILE = GNDETH_DtaC",j,".dat;"),
      "",
      "VARIABLE: ",
      paste0("NAMES = ", paste(colnames(data), collapse = "\n"),";"),
      "IDVARIABLE = IDSTUD;",
      paste0("USEVARIABLES = ", paste(colnames(data)[grepl('^ETH|GND', colnames(data))], collapse = "\n"),";"),
      paste0("CATEGORICAL = ", paste(colnames(data)[grepl('^ETH|GND', colnames(data))], collapse = "\n"),";"),
      "MISSING = .;",
      paste0("CLASSES = g(",length(unique(data$id_k)),") ",sprintf("c(%d);", k)),
      paste0("KNOWNCLASS = g(id_k ="), 
      paste(str, collapse = "\n"),
      ");",
      "WEIGHT = ws;",
      "STRATIFICATION = id_s;",
      "CLUSTER = id_j;",
      " ",
      "ANALYSIS:",
      "TYPE = COMPLEX MIXTURE;",
      "PROCESSORS = 4;",
      "STARTS = 1000 250;",
      "STITERATIONS = 20;",
      "STSEED = 288;",
      "",
      "MODEL:",
      "%OVERALL%",
      "c ON g;",
      " ",
      castxPH,
      " ",
      "OUTPUT: ",
      "TECH10",#for bivariate model fit information
      #"TECH11",
      #"SAMP",
      #"TECH14;", Time consuming tests
      #"CINTERVAL",
      "SVALUES",
      ";",
      "",
      "SAVEDATA:",
      paste0("FILE = GNDETH_Prob_MGCntry_C", j,"cl", k,"_2PHom.dat;"),
      "SAVE = CPROBABILITIES;"
      
    ), fileConn)
    close(fileConn)
  })
}

#----------------2classes P.Hom Multigroup COUNTRY by CYCLE --------------------------------------

for (j in 3:3) { #input file for each CYCLE 1:3
  
  data <- ds_lc0 %>%  filter(CYCLE == paste0("C",j)) %>%
    dplyr::select(all_of(sample), all_of(ScalesGND), IDSTUD, COUNTRY) %>%
    mutate_if(is.factor, ~ as.numeric(.x)) %>%
    data.frame()
  
  # list of countries for multigroup model
  t <- cbind(unique(data[, "id_k"]),"!", unique(ds_lc0$COUNTRY), 
             g = rownames(unique(ds_lc0$COUNTRY)))
  tc <- textConnection("str", "w")
  sink(tc)   # divert output to tc connection
  print(t, quote = FALSE)  # print in str string instead of console
  sink()     # set the output back to console
  close(tc)
  str <- substr(str,floor(length(str)/10)+5,nchar(str[1])) # we get rid of the row numbers that come with print
  str <- paste0(str[-1],collapse="\n")        # we build a proper unique string with your pipes and new lines
  data<- data %>% select(-COUNTRY)
  
  # Identify restrictions for partial homogeneity model
  lapply(4:4, function(k) { #input file for different number of classes
      cas <- data.frame(NULL)
      partial = 1:(2) 
      id = 0
      mat <- data.frame(NULL)
      for(g in 1:length(unique(data$id_k))){
        
        cast<- data.frame(outer(paste0("%g#",g), paste0(".c#",1:k,"%"), paste, sep=""))
        vars<-sort(colnames(data)[grepl('^GND', colnames(data))])
        for (c in 1:k) {
          n = length(vars)*id
          id = id + 1  
          if(c == 1){
            for(v in 1:length(vars)){
              if (!is.na(partial[c])  & g != 1 & c %in% partial) {
                mat[c,1] <- id
                mat[c,v+1] <- cas[partial[c],v+2]
              } else {
                mat[c,1] <- id
                mat[c,v+1] <- paste0("[",vars[v],"$1*6] (",n + v,");")
              }
            }  
          } else if(c == 2) {
            for(v in 1:length(vars)){
              if (!is.na(partial[c])  & g != 1 & c %in% partial) {
                mat[c,1] <- id
                mat[c,v+1] <- cas[partial[c],v+2]
              } else {
                if(vars[v] %in% c("GND1", "GND2", "GND5")){
                  mat[c,1] <- id
                  mat[c,v+1] <- paste0("[",vars[v],"$1*6] (",n + v,");")  
                } else{
                  mat[c,1] <- id
                  mat[c,v+1] <- paste0("[",vars[v],"$1*-2] (",n + v,");")
                }
                
              }
            }
            } else{
              for(v in 1:length(vars)){
                if (!is.na(partial[c])  & g != 1 & c %in% partial) {
                  mat[c,1] <- id
                  mat[c,v+1] <- cas[partial[c],v+2]
                } else {
                  mat[c,1] <- id
                  mat[c,v+1] <- paste0("[",vars[v],"$1] (",n + v,");")
                }
              }
            }
        }
        cas <- rbind(cas,cbind(t(cast),mat))
      }
      casr <- reshape2::melt(cas, id.vars = "V1") %>% arrange(V1) 
      casr <- data.frame(casr[,c(-1,-2)])
      
      casc <- textConnection("castxPH", "w")
      sink(casc)   # divert output to tc connection
      print(casr)  # print in str string instead of console
      sink()     # set the output back to console
      close(casc)
      castxPH <- substr(castxPH,5,nchar(castxPH[1])) # we get rid of the row numbers that come with print
      castxPH <- paste0(castxPH[-1],collapse="\n")
      
      fileConn <- file(paste0("data/MplusModels/CountryMG/GND_MGCnty_C",j,"cl",sprintf("%d", k),"_2.",p,"PHomR.inp"))
      writeLines(c(
        paste0("TITLE: 2.", p, "P.Hom MG Country LCA GND C", j," with ", k ," classes;"),
        "DATA: ",
        paste0("FILE = GND_DtaC",j,".dat;"),
        "",
        "VARIABLE: ",
        paste0("NAMES = ", paste(colnames(data), collapse = "\n"),";"),
        "IDVARIABLE = IDSTUD;",
        paste0("USEVARIABLES = ", paste(colnames(data)[grepl('^GND', colnames(data))], collapse = "\n"),";"),
        paste0("CATEGORICAL = ", paste(colnames(data)[grepl('^GND', colnames(data))], collapse = "\n"),";"),
        "MISSING = .;",
        paste0("CLASSES = g(",length(unique(data$id_k)),") ",sprintf("c(%d);", k)),
        paste0("KNOWNCLASS = g(id_k ="), 
        paste(str, collapse = "\n"),
        ");",
        "WEIGHT = ws;",
        "STRATIFICATION = id_s;",
        "CLUSTER = id_j;",
        " ",
        "ANALYSIS:",
        "TYPE = COMPLEX MIXTURE;",
        "PROCESSORS = 4;",
        "STARTS = 1000 250;",
        "STITERATIONS = 20;",
        "STSEED = 288;",
        "",
        "MODEL:",
        "%OVERALL%",
        "c ON g;",
        " ",
        castxPH,
        " ",
        "OUTPUT: ",
        "TECH10",#for bivariate model fit information
        #"TECH11",
        #"SAMP",
        #"TECH14;", Time consuming tests
        #"CINTERVAL",
        "SVALUES",
        ";",
        "",
        "SAVEDATA:",
        paste0("FILE = GND_Prob_MGCntry_C", j,"cl", k,"_2.","2PHomR.dat.dat;"),
        "SAVE = CPROBABILITIES;"
        
      ), fileConn)
      close(fileConn)
  })
}
for (j in 3:3) { #input file for each CYCLE 1:3
  
  data <- ds_lc0 %>%  filter(CYCLE == paste0("C",j)) %>%
    dplyr::select(all_of(sample), all_of(ScalesGND), IDSTUD, COUNTRY) %>%
    mutate_if(is.factor, ~ as.numeric(.x)) %>%
    data.frame()
  
  # list of countries for multigroup model
  t <- cbind(unique(data[, "id_k"]),"!", unique(ds_lc0$COUNTRY), 
             g = rownames(unique(ds_lc0$COUNTRY)))
  tc <- textConnection("str", "w")
  sink(tc)   # divert output to tc connection
  print(t, quote = FALSE)  # print in str string instead of console
  sink()     # set the output back to console
  close(tc)
  str <- substr(str,floor(length(str)/10)+5,nchar(str[1])) # we get rid of the row numbers that come with print
  str <- paste0(str[-1],collapse="\n")        # we build a proper unique string with your pipes and new lines
  data<- data %>% select(-COUNTRY)
  
  # Identify restrictions for partial homogeneity model
  lapply(4:4, function(k) { #input file for different number of classes
    cas <- data.frame(NULL)
    partial = 1:(2) 
    id = 0
    mat <- data.frame(NULL)
    for(g in 1:length(unique(data$id_k))){
      
      cast<- data.frame(outer(paste0("%g#",g), paste0(".c#",1:k,"%"), paste, sep=""))
      vars<-sort(colnames(data)[grepl('^GND', colnames(data))])
      for (c in 1:k) {
        n = length(vars)*id
        id = id + 1  
        if(c == 1){
          mat[c,1] <- id
          mat[c,1+1] <- paste0("[",vars[1],"$1@6] (1);")
          mat[c,2+1] <- paste0("[",vars[2],"$1@4] (2);")
          mat[c,3+1] <- paste0("[",vars[3],"$1@4] (3);")
          mat[c,4+1] <- paste0("[",vars[4],"$1@3] (4);")
          mat[c,5+1] <- paste0("[",vars[5],"$1@3] (5);")
          mat[c,6+1] <- paste0("[",vars[6],"$1@2] (6);")
        } else if(c == 2) {
          mat[c,1] <- id
          mat[c,1+1] <- paste0("[",vars[1],"$1@4] (7);")
          mat[c,2+1] <- paste0("[",vars[2],"$1@3] (8);")
          mat[c,3+1] <- paste0("[",vars[3],"$1@0] (9);")
          mat[c,4+1] <- paste0("[",vars[4],"$1@-1] (10);")
          mat[c,5+1] <- paste0("[",vars[5],"$1@2] (11);")
          mat[c,6+1] <- paste0("[",vars[6],"$1@-2] (12);")
        } else{
          for(v in 1:length(vars)){
            if (!is.na(partial[c])  & g != 1 & c %in% partial) {
              mat[c,1] <- id
              mat[c,v+1] <- cas[partial[c],v+2]
            } else {
              mat[c,1] <- id
              mat[c,v+1] <- paste0("[",vars[v],"$1] (",n + v,");")
            }
          }
        }
      }
      cas <- rbind(cas,cbind(t(cast),mat))
    }
    casr <- reshape2::melt(cas, id.vars = "V1") %>% arrange(V1) 
    casr <- data.frame(casr[,c(-1,-2)])
    
    casc <- textConnection("castxPH", "w")
    sink(casc)   # divert output to tc connection
    print(casr)  # print in str string instead of console
    sink()     # set the output back to console
    close(casc)
    castxPH <- substr(castxPH,5,nchar(castxPH[1])) # we get rid of the row numbers that come with print
    castxPH <- paste0(castxPH[-1],collapse="\n")
    
    fileConn <- file(paste0("data/MplusModels/CountryMG/GND_MGCnty_C",j,"cl",sprintf("%d", k),"_2.",p,"PHom.inp"))
    writeLines(c(
      paste0("TITLE: 2.", p, "P.Hom MG Country LCA GND C", j," with ", k ," classes;"),
      "DATA: ",
      paste0("FILE = GND_DtaC",j,".dat;"),
      "",
      "VARIABLE: ",
      paste0("NAMES = ", paste(colnames(data), collapse = "\n"),";"),
      "IDVARIABLE = IDSTUD;",
      paste0("USEVARIABLES = ", paste(colnames(data)[grepl('^GND', colnames(data))], collapse = "\n"),";"),
      paste0("CATEGORICAL = ", paste(colnames(data)[grepl('^GND', colnames(data))], collapse = "\n"),";"),
      "MISSING = .;",
      paste0("CLASSES = g(",length(unique(data$id_k)),") ",sprintf("c(%d);", k)),
      paste0("KNOWNCLASS = g(id_k ="), 
      paste(str, collapse = "\n"),
      ");",
      "WEIGHT = ws;",
      "STRATIFICATION = id_s;",
      "CLUSTER = id_j;",
      " ",
      "ANALYSIS:",
      "TYPE = COMPLEX MIXTURE;",
      "PROCESSORS = 4;",
      "STARTS = 1000 250;",
      "STITERATIONS = 20;",
      "STSEED = 288;",
      "",
      "MODEL:",
      "%OVERALL%",
      "c ON g;",
      " ",
      castxPH,
      " ",
      "OUTPUT: ",
      "TECH10",#for bivariate model fit information
      #"TECH11",
      #"SAMP",
      #"TECH14;", Time consuming tests
      #"CINTERVAL",
      "SVALUES",
      ";",
      "",
      "SAVEDATA:",
      paste0("FILE = GND_Prob_MGCntry_C", j,"cl", k,"_2.","2PHom.dat.dat;"),
      "SAVE = CPROBABILITIES;"
      
    ), fileConn)
    close(fileConn)
  })
}


for (j in 3:3) { #input file for each CYCLE 1:3
  
  data <- ds_lc0 %>%  filter(CYCLE == paste0("C",j)) %>%
    dplyr::select(all_of(sample), all_of(ScalesETH), IDSTUD, COUNTRY) %>%
    mutate_if(is.factor, ~ as.numeric(.x)) %>%
    data.frame()
  
  # list of countries for multigroup model
  t <- cbind(unique(data[, "id_k"]),"!", unique(ds_lc0$COUNTRY), 
             g = rownames(unique(ds_lc0$COUNTRY)))
  tc <- textConnection("str", "w")
  sink(tc)   # divert output to tc connection
  print(t, quote = FALSE)  # print in str string instead of console
  sink()     # set the output back to console
  close(tc)
  str <- substr(str,floor(length(str)/10)+5,nchar(str[1])) # we get rid of the row numbers that come with print
  str <- paste0(str[-1],collapse="\n")        # we build a proper unique string with your pipes and new lines
  data<- data %>% select(-COUNTRY)
  
  # Identify restrictions for partial homogeneity model
  lapply(4:4, function(k) { #input file for different number of classes
    cas <- data.frame(NULL)
    partial = 1:(3) 
    id = 0
    mat <- data.frame(NULL)
    for(g in 1:length(unique(data$id_k))){
      
      cast<- data.frame(outer(paste0("%g#",g), paste0(".c#",1:k,"%"), paste, sep=""))
      vars<-sort(colnames(data)[grepl('^ETH', colnames(data))])
      for (c in 1:k) {
        n = length(vars)*id
        id = id + 1  
        if(c == 1){
          for(v in 1:length(vars)){
            if (!is.na(partial[c])  & g != 1 & c %in% partial) {
              mat[c,1] <- id
              mat[c,v+1] <- cas[partial[c],v+2]
            } else {
              mat[c,1] <- id
              mat[c,v+1] <- paste0("[",vars[v],"$1*6] (",n + v,");")
            }
          }  
        } else if(c == 2) {
          for(v in 1:length(vars)){
            if (!is.na(partial[c])  & g != 1 & c %in% partial) {
              mat[c,1] <- id
              mat[c,v+1] <- cas[partial[c],v+2]
            } else {
              if(!vars[v] %in% c("ETH4")){
                mat[c,1] <- id
                mat[c,v+1] <- paste0("[",vars[v],"$1*6] (",n + v,");")  
              } else{
                mat[c,1] <- id
                mat[c,v+1] <- paste0("[",vars[v],"$1*0] (",n + v,");")
              }
              
            }
          }
        } else if(c == 3) {
          for(v in 1:length(vars)){
            if (!is.na(partial[c])  & g != 1 & c %in% partial) {
              mat[c,1] <- id
              mat[c,v+1] <- cas[partial[c],v+2]
            } else {
                mat[c,1] <- id
                mat[c,v+1] <- paste0("[",vars[v],"$1*0] (",n + v,");")
            }
          }
          } else{
          for(v in 1:length(vars)){
            if (!is.na(partial[c])  & g != 1 & c %in% partial) {
              mat[c,1] <- id
              mat[c,v+1] <- cas[partial[c],v+2]
            } else {
              mat[c,1] <- id
              mat[c,v+1] <- paste0("[",vars[v],"$1] (",n + v,");")
            }
          }
        }
      }
      cas <- rbind(cas,cbind(t(cast),mat))
    }
    casr <- reshape2::melt(cas, id.vars = "V1") %>% arrange(V1) 
    casr <- data.frame(casr[,c(-1,-2)])
    
    casc <- textConnection("castxPH", "w")
    sink(casc)   # divert output to tc connection
    print(casr)  # print in str string instead of console
    sink()     # set the output back to console
    close(casc)
    castxPH <- substr(castxPH,5,nchar(castxPH[1])) # we get rid of the row numbers that come with print
    castxPH <- paste0(castxPH[-1],collapse="\n")
    
    fileConn <- file(paste0("data/MplusModels/CountryMG/ETH_MGCnty_C",j,"cl",sprintf("%d", k),"_2.",p,"PHomR.inp"))
    writeLines(c(
      paste0("TITLE: 2.", p, "P.Hom MG Country LCA ETH C", j," with ", k ," classes;"),
      "DATA: ",
      paste0("FILE = ETH_DtaC",j,".dat;"),
      "",
      "VARIABLE: ",
      paste0("NAMES = ", paste(colnames(data), collapse = "\n"),";"),
      "IDVARIABLE = IDSTUD;",
      paste0("USEVARIABLES = ", paste(colnames(data)[grepl('^ETH', colnames(data))], collapse = "\n"),";"),
      paste0("CATEGORICAL = ", paste(colnames(data)[grepl('^ETH', colnames(data))], collapse = "\n"),";"),
      "MISSING = .;",
      paste0("CLASSES = g(",length(unique(data$id_k)),") ",sprintf("c(%d);", k)),
      paste0("KNOWNCLASS = g(id_k ="), 
      paste(str, collapse = "\n"),
      ");",
      "WEIGHT = ws;",
      "STRATIFICATION = id_s;",
      "CLUSTER = id_j;",
      " ",
      "ANALYSIS:",
      "TYPE = COMPLEX MIXTURE;",
      "PROCESSORS = 4;",
      "STARTS = 1000 250;",
      "STITERATIONS = 20;",
      "STSEED = 288;",
      "",
      "MODEL:",
      "%OVERALL%",
      "c ON g;",
      " ",
      castxPH,
      " ",
      "OUTPUT: ",
      "TECH10",#for bivariate model fit information
      #"TECH11",
      #"SAMP",
      #"TECH14;", Time consuming tests
      #"CINTERVAL",
      "SVALUES",
      ";",
      "",
      "SAVEDATA:",
      paste0("FILE = ETH_Prob_MGCntry_C", j,"cl", k,"_2.","2PHomR.dat.dat;"),
      "SAVE = CPROBABILITIES;"
      
    ), fileConn)
    close(fileConn)
  })
}
for (j in 3:3) { #input file for each CYCLE 1:3
  
  data <- ds_lc0 %>%  filter(CYCLE == paste0("C",j)) %>%
    dplyr::select(all_of(sample), all_of(ScalesETH), IDSTUD, COUNTRY) %>%
    mutate_if(is.factor, ~ as.numeric(.x)) %>%
    data.frame()
  
  # list of countries for multigroup model
  t <- cbind(unique(data[, "id_k"]),"!", unique(ds_lc0$COUNTRY), 
             g = rownames(unique(ds_lc0$COUNTRY)))
  tc <- textConnection("str", "w")
  sink(tc)   # divert output to tc connection
  print(t, quote = FALSE)  # print in str string instead of console
  sink()     # set the output back to console
  close(tc)
  str <- substr(str,floor(length(str)/10)+5,nchar(str[1])) # we get rid of the row numbers that come with print
  str <- paste0(str[-1],collapse="\n")        # we build a proper unique string with your pipes and new lines
  data<- data %>% select(-COUNTRY)
  
  # Identify restrictions for partial homogeneity model
  lapply(4:4, function(k) { #input file for different number of classes
    cas <- data.frame(NULL)
    partial = 1:(2) 
    id = 0
    mat <- data.frame(NULL)
    for(g in 1:length(unique(data$id_k))){
      
      cast<- data.frame(outer(paste0("%g#",g), paste0(".c#",1:k,"%"), paste, sep=""))
      vars<-sort(colnames(data)[grepl('^ETH', colnames(data))])
      for (c in 1:k) {
        n = length(vars)*id
        id = id + 1  
        if(c == 1){
          mat[c,1] <- id
          mat[c,1+1] <- paste0("[",vars[1],"$1@6] (1);")
          mat[c,2+1] <- paste0("[",vars[2],"$1@6] (2);")
          mat[c,3+1] <- paste0("[",vars[3],"$1@6] (3);")
          mat[c,4+1] <- paste0("[",vars[4],"$1@4] (4);")
          mat[c,5+1] <- paste0("[",vars[5],"$1@4] (5);")
        } else if(c == 2) {
          mat[c,1] <- id
          mat[c,1+1] <- paste0("[",vars[1],"$1@4] (6);")
          mat[c,2+1] <- paste0("[",vars[2],"$1@3] (7);")
          mat[c,3+1] <- paste0("[",vars[3],"$1@3] (8);")
          mat[c,4+1] <- paste0("[",vars[4],"$1@0] (9);")
          mat[c,5+1] <- paste0("[",vars[5],"$1@3] (10);")
        } else if(c == 3) {
          mat[c,1] <- id
          mat[c,1+1] <- paste0("[",vars[1],"$1@-3] (11);")
          mat[c,2+1] <- paste0("[",vars[2],"$1@-3] (12);")
          mat[c,3+1] <- paste0("[",vars[3],"$1@-3] (13);")
          mat[c,4+1] <- paste0("[",vars[4],"$1@-3] (14);")
          mat[c,5+1] <- paste0("[",vars[5],"$1@-3] (15);")
        } else{
          for(v in 1:length(vars)){
            if (!is.na(partial[c])  & g != 1 & c %in% partial) {
              mat[c,1] <- id
              mat[c,v+1] <- cas[partial[c],v+2]
            } else {
              mat[c,1] <- id
              mat[c,v+1] <- paste0("[",vars[v],"$1] (",n + v,");")
            }
          }
        }
      }
      cas <- rbind(cas,cbind(t(cast),mat))
    }
    casr <- reshape2::melt(cas, id.vars = "V1") %>% arrange(V1) 
    casr <- data.frame(casr[,c(-1,-2)])
    
    casc <- textConnection("castxPH", "w")
    sink(casc)   # divert output to tc connection
    print(casr)  # print in str string instead of console
    sink()     # set the output back to console
    close(casc)
    castxPH <- substr(castxPH,5,nchar(castxPH[1])) # we get rid of the row numbers that come with print
    castxPH <- paste0(castxPH[-1],collapse="\n")
    
    fileConn <- file(paste0("data/MplusModels/CountryMG/ETH_MGCnty_C",j,"cl",sprintf("%d", k),"_2.",p,"PHom.inp"))
    writeLines(c(
      paste0("TITLE: 2.", p, "P.Hom MG Country LCA ETH C", j," with ", k ," classes;"),
      "DATA: ",
      paste0("FILE = ETH_DtaC",j,".dat;"),
      "",
      "VARIABLE: ",
      paste0("NAMES = ", paste(colnames(data), collapse = "\n"),";"),
      "IDVARIABLE = IDSTUD;",
      paste0("USEVARIABLES = ", paste(colnames(data)[grepl('^ETH', colnames(data))], collapse = "\n"),";"),
      paste0("CATEGORICAL = ", paste(colnames(data)[grepl('^ETH', colnames(data))], collapse = "\n"),";"),
      "MISSING = .;",
      paste0("CLASSES = g(",length(unique(data$id_k)),") ",sprintf("c(%d);", k)),
      paste0("KNOWNCLASS = g(id_k ="), 
      paste(str, collapse = "\n"),
      ");",
      "WEIGHT = ws;",
      "STRATIFICATION = id_s;",
      "CLUSTER = id_j;",
      " ",
      "ANALYSIS:",
      "TYPE = COMPLEX MIXTURE;",
      "PROCESSORS = 4;",
      "STARTS = 1000 250;",
      "STITERATIONS = 20;",
      "STSEED = 288;",
      "",
      "MODEL:",
      "%OVERALL%",
      "c ON g;",
      " ",
      castxPH,
      " ",
      "OUTPUT: ",
      "TECH10",#for bivariate model fit information
      #"TECH11",
      #"SAMP",
      #"TECH14;", Time consuming tests
      #"CINTERVAL",
      "SVALUES",
      ";",
      "",
      "SAVEDATA:",
      paste0("FILE = ETH_Prob_MGCntry_C", j,"cl", k,"_2.","2PHom.dat.dat;"),
      "SAVE = CPROBABILITIES;"
      
    ), fileConn)
    close(fileConn)
  })
}

#----------------2P.Hom Multigroup COUNTRY by CYCLE --------------------------------------

for (j in 3:3) { #input file for each CYCLE 1:3
  
  data <- ds_lc0 %>%  filter(CYCLE == paste0("C",j)) %>%
    dplyr::select(all_of(sample), all_of(ScalesGND), IDSTUD, COUNTRY) %>%
    mutate_if(is.factor, ~ as.numeric(.x)) %>%
    data.frame()
  
  # list of countries for multigroup model
  t <- cbind(unique(data[, "id_k"]),"!", unique(ds_lc0$COUNTRY), 
             g = rownames(unique(ds_lc0$COUNTRY)))
  tc <- textConnection("str", "w")
  sink(tc)   # divert output to tc connection
  print(t, quote = FALSE)  # print in str string instead of console
  sink()     # set the output back to console
  close(tc)
  str <- substr(str,floor(length(str)/10)+5,nchar(str[1])) # we get rid of the row numbers that come with print
  str <- paste0(str[-1],collapse="\n")        # we build a proper unique string with your pipes and new lines
  data<- data %>% select(-COUNTRY)
  
  # Identify restrictions for partial homogeneity model
  lapply(5:5, function(k) { #input file for different number of classes
    for (p in (k-1):1){
      cas <- data.frame(NULL)
      partial = 1:(p) 
      id = 0
      mat <- data.frame(NULL)
      for(g in 1:length(unique(data$id_k))){
        
        cast<- data.frame(outer(paste0("%g#",g), paste0(".c#",1:k,"%"), paste, sep=""))
        vars<-sort(colnames(data)[grepl('^GND', colnames(data))])
        for (c in 1:k) {
          n = length(vars)*id
          id = id + 1  
          for(v in 1:length(vars)){
            if (!is.na(partial[c])  & g != 1 & c %in% partial) {
              mat[c,1] <- id
              mat[c,v+1] <- cas[partial[c],v+2]
            } else {
              mat[c,1] <- id
              mat[c,v+1] <- paste0("[",vars[v],"$1] (",n + v,");")
            }
          }
        }
        cas <- rbind(cas,cbind(t(cast),mat))
      }
      casr <- reshape2::melt(cas, id.vars = "V1") %>% arrange(V1) 
      casr <- data.frame(casr[,c(-1,-2)])
      
      casc <- textConnection("castxPH", "w")
      sink(casc)   # divert output to tc connection
      print(casr)  # print in str string instead of console
      sink()     # set the output back to console
      close(casc)
      castxPH <- substr(castxPH,5+2,nchar(castxPH[1])) # we get rid of the row numbers that come with print
      castxPH <- paste0(castxPH[-1],collapse="\n")
      
      fileConn <- file(paste0("data/MplusModels/CountryMG/GND_MGCnty_C",j,"cl",sprintf("%d", k),"_2.",p,"PHom.inp"))
      writeLines(c(
        paste0("TITLE: 2.", p, "P.Hom MG Country LCA GND C", j," with ", k ," classes;"),
        "DATA: ",
        paste0("FILE = GND_DtaC",j,".dat;"),
        "",
        "VARIABLE: ",
        paste0("NAMES = ", paste(colnames(data), collapse = "\n"),";"),
        "IDVARIABLE = IDSTUD;",
        paste0("USEVARIABLES = ", paste(colnames(data)[grepl('^GND', colnames(data))], collapse = "\n"),";"),
        paste0("CATEGORICAL = ", paste(colnames(data)[grepl('^GND', colnames(data))], collapse = "\n"),";"),
        "MISSING = .;",
        paste0("CLASSES = g(",length(unique(data$id_k)),") ",sprintf("c(%d);", k)),
        paste0("KNOWNCLASS = g(id_k ="), 
        paste(str, collapse = "\n"),
        ");",
        "WEIGHT = ws;",
        "STRATIFICATION = id_s;",
        "CLUSTER = id_j;",
        " ",
        "ANALYSIS:",
        "TYPE = COMPLEX MIXTURE;",
        "PROCESSORS = 4;",
        "STARTS = 1000 250;",
        "STITERATIONS = 20;",
        "STSEED = 288;",
        "",
        "MODEL:",
        "%OVERALL%",
        "c ON g;",
        " ",
        castxPH,
        " ",
        "OUTPUT: ",
        "TECH10",#for bivariate model fit information
        #"TECH11",
        #"SAMP",
        #"TECH14;", Time consuming tests
        #"CINTERVAL",
        "SVALUES",
        ";",
        "",
        "SAVEDATA:",
        paste0("FILE = GND_Prob_MGCntry_C", j,"cl", k,"_2.",p,"PHom.dat.dat;"),
        "SAVE = CPROBABILITIES;"
        
      ), fileConn)
      close(fileConn)
    }
  })
}
for (j in 3:3) { #input file for each CYCLE 1:3
  
  data <- ds_lc0 %>%  filter(CYCLE == paste0("C",j)) %>%
    dplyr::select(all_of(sample), all_of(ScalesETH), IDSTUD, COUNTRY) %>%
    mutate_if(is.factor, ~ as.numeric(.x)) %>%
    data.frame()
  
  # list of countries for multigroup model
  t <- cbind(unique(data[, "id_k"]),"!", unique(ds_lc0$COUNTRY), 
             g = rownames(unique(ds_lc0$COUNTRY)))
  tc <- textConnection("str", "w")
  sink(tc)   # divert output to tc connection
  print(t, quote = FALSE)  # print in str string instead of console
  sink()     # set the output back to console
  close(tc)
  str <- substr(str,floor(length(str)/10)+5,nchar(str[1])) # we get rid of the row numbers that come with print
  str <- paste0(str[-1],collapse="\n")        # we build a proper unique string with your pipes and new lines
  data<- data %>% select(-COUNTRY)
  
  # Identify restrictions for partial homogeneity model
  lapply(3:3, function(k) { #input file for different number of classes
    for (p in (k-1):1){
      cas <- data.frame(NULL)
      partial = 1:(p) 
      id = 0
      mat <- data.frame(NULL)
      for(g in 1:length(unique(data$id_k))){
        
        cast<- data.frame(outer(paste0("%g#",g), paste0(".c#",1:k,"%"), paste, sep=""))
        vars<-sort(colnames(data)[grepl('^ETH', colnames(data))])
        for (c in 1:k) {
          n = length(vars)*id
          id = id + 1  
          for(v in 1:length(vars)){
            if (!is.na(partial[c])  & g != 1 & c %in% partial) {
              mat[c,1] <- id
              mat[c,v+1] <- cas[partial[c],v+2]
            } else {
              mat[c,1] <- id
              mat[c,v+1] <- paste0("[",vars[v],"$1] (",n + v,");")
            }
          }
        }
        cas <- rbind(cas,cbind(t(cast),mat))
      }
      casr <- reshape2::melt(cas, id.vars = "V1") %>% arrange(V1) 
      casr <- data.frame(casr[,c(-1,-2)])
      
      casc <- textConnection("castxPH", "w")
      sink(casc)   # divert output to tc connection
      print(casr)  # print in str string instead of console
      sink()     # set the output back to console
      close(casc)
      castxPH <- substr(castxPH,5+2,nchar(castxPH[1])) # we get rid of the row numbers that come with print
      castxPH <- paste0(castxPH[-1],collapse="\n")
      
      fileConn <- file(paste0("data/MplusModels/CountryMG/ETH_MGCnty_C",j,"cl",sprintf("%d", k),"_2.",p,"PHom.inp"))
      writeLines(c(
        paste0("TITLE: 2.", p, "P.Hom MG Country LCA ETH C", j," with ", k ," classes;"),
        "DATA: ",
        paste0("FILE = ETH_DtaC",j,".dat;"),
        "",
        "VARIABLE: ",
        paste0("NAMES = ", paste(colnames(data), collapse = "\n"),";"),
        "IDVARIABLE = IDSTUD;",
        paste0("USEVARIABLES = ", paste(colnames(data)[grepl('^ETH', colnames(data))], collapse = "\n"),";"),
        paste0("CATEGORICAL = ", paste(colnames(data)[grepl('^ETH', colnames(data))], collapse = "\n"),";"),
        "MISSING = .;",
        paste0("CLASSES = g(",length(unique(data$id_k)),") ",sprintf("c(%d);", k)),
        paste0("KNOWNCLASS = g(id_k ="), 
        paste(str, collapse = "\n"),
        ");",
        "WEIGHT = ws;",
        "STRATIFICATION = id_s;",
        "CLUSTER = id_j;",
        " ",
        "ANALYSIS:",
        "TYPE = COMPLEX MIXTURE;",
        "PROCESSORS = 4;",
        "STARTS = 1000 250;",
        "STITERATIONS = 20;",
        "STSEED = 288;",
        "",
        "MODEL:",
        "%OVERALL%",
        "c ON g;",
        " ",
        castxPH,
        " ",
        "OUTPUT: ",
        "TECH10",#for bivariate model fit information
        #"TECH11",
        #"SAMP",
        #"TECH14;", Time consuming tests
        #"CINTERVAL",
        "SVALUES",
        ";",
        "",
        "SAVEDATA:",
        paste0("FILE = ETH_Prob_MGCntry_C", j,"cl", k,"_2.",p,"PHom.dat.dat;"),
        "SAVE = CPROBABILITIES;"
        
      ), fileConn)
      close(fileConn)
    }
  })
}
for (j in 3:3) { #input file for each CYCLE 1:3
  
  data <- ds_lc0 %>%  filter(CYCLE == paste0("C",j)) %>%
    dplyr::select(all_of(sample), all_of(ScalesETH), IDSTUD, COUNTRY) %>%
    mutate_if(is.factor, ~ as.numeric(.x)) %>%
    data.frame()
  
  # list of countries for multigroup model
  t <- cbind(unique(data[, "id_k"]),"!", unique(ds_lc0$COUNTRY), 
             g = rownames(unique(ds_lc0$COUNTRY)))
  tc <- textConnection("str", "w")
  sink(tc)   # divert output to tc connection
  print(t, quote = FALSE)  # print in str string instead of console
  sink()     # set the output back to console
  close(tc)
  str <- substr(str,floor(length(str)/10)+5,nchar(str[1])) # we get rid of the row numbers that come with print
  str <- paste0(str[-1],collapse="\n")        # we build a proper unique string with your pipes and new lines
  data<- data %>% select(-COUNTRY)
  
  # Identify restrictions for partial homogeneity model
  lapply(3:3, function(k) { #input file for different number of classes
    for (p in (k-1):1){
      cas <- data.frame(NULL)
      partial = 1:(p) 
      id = 0
      mat <- data.frame(NULL)
      for(g in 1:length(unique(data$id_k))){
        
        cast<- data.frame(outer(paste0("%g#",g), paste0(".c#",1:k,"%"), paste, sep=""))
        vars<-sort(colnames(data)[grepl('^ETH', colnames(data))])
        for (c in 1:k) {
          n = length(vars)*id
          id = id + 1  
          for(v in 1:length(vars)){
            if (!is.na(partial[c])  & g != 1 & c %in% partial) {
              mat[c,1] <- id
              mat[c,v+1] <- cas[partial[c],v+2]
            } else {
              mat[c,1] <- id
              mat[c,v+1] <- paste0("[",vars[v],"$1] (",n + v,");")
            }
          }
        }
        cas <- rbind(cas,cbind(t(cast),mat))
      }
      casr <- reshape2::melt(cas, id.vars = "V1") %>% arrange(V1) 
      casr <- data.frame(casr[,c(-1,-2)])
      
      casc <- textConnection("castxPH", "w")
      sink(casc)   # divert output to tc connection
      print(casr)  # print in str string instead of console
      sink()     # set the output back to console
      close(casc)
      castxPH <- substr(castxPH,5+2,nchar(castxPH[1])) # we get rid of the row numbers that come with print
      castxPH <- paste0(castxPH[-1],collapse="\n")
      
      fileConn <- file(paste0("data/MplusModels/CountryMG/ETH_MGCnty_C",j,"cl",sprintf("%d", k),"_2.",p,"PHom.inp"))
      writeLines(c(
        paste0("TITLE: 2.", p, "P.Hom MG Country LCA ETH C", j," with ", k ," classes;"),
        "DATA: ",
        paste0("FILE = ETH_DtaC",j,".dat;"),
        "",
        "VARIABLE: ",
        paste0("NAMES = ", paste(colnames(data), collapse = "\n"),";"),
        "IDVARIABLE = IDSTUD;",
        paste0("USEVARIABLES = ", paste(colnames(data)[grepl('^ETH', colnames(data))], collapse = "\n"),";"),
        paste0("CATEGORICAL = ", paste(colnames(data)[grepl('^ETH', colnames(data))], collapse = "\n"),";"),
        "MISSING = .;",
        paste0("CLASSES = g(",length(unique(data$id_k)),") ",sprintf("c(%d);", k)),
        paste0("KNOWNCLASS = g(id_k ="), 
        paste(str, collapse = "\n"),
        ");",
        "WEIGHT = ws;",
        "STRATIFICATION = id_s;",
        "CLUSTER = id_j;",
        " ",
        "ANALYSIS:",
        "TYPE = COMPLEX MIXTURE;",
        "PROCESSORS = 4;",
        "STARTS = 1000 250;",
        "STITERATIONS = 20;",
        "STSEED = 288;",
        "",
        "MODEL:",
        "%OVERALL%",
        "c ON g;",
        " ",
        castxPH,
        " ",
        "OUTPUT: ",
        "TECH10",#for bivariate model fit information
        #"TECH11",
        #"SAMP",
        #"TECH14;", Time consuming tests
        #"CINTERVAL",
        "SVALUES",
        ";",
        "",
        "SAVEDATA:",
        paste0("FILE = ETH_Prob_MGCntry_C", j,"cl", k,"_2.",p,"PHom.dat.dat;"),
        "SAVE = CPROBABILITIES;"
        
      ), fileConn)
      close(fileConn)
    }
  })
}



#----------------C.Het Multigroup COUNTRY by CYCLE--------------------------------------
for (j in 3:3) { #input file for each CYCLE 1:3
  data <- ds_lc0 %>%  filter(CYCLE == paste0("C",j)) %>%
    dplyr::select(all_of(sample), all_of(ScalesGND), IDSTUD, COUNTRY) %>%
    mutate_if(is.factor, ~ as.numeric(.x)) %>%
    data.frame()
  # list of countries for multigroup model
  t <- cbind(unique(data[, "id_k"]),"!", unique(data$COUNTRY), 
             g = rownames(unique(data$COUNTRY)))
  tc <- textConnection("str", "w")
  sink(tc)   # divert output to tc connection
  print(t, quote = FALSE)  # print in str string instead of console
  sink()     # set the output back to console
  close(tc)
  str <- substr(str,floor(length(str)/10)+5,nchar(str[1])) # we get rid of the row numbers that come with print
  str <- paste0(str[-1],collapse="\n")        # we build a proper unique string with your pipes and new lines
  data<- data %>% select(-COUNTRY)
  prepareMplusData(df = data,
                   filename = paste0("data/MplusModels/CountryMG/GND_DtaC",j,".dat"), interactive =FALSE)
  
  # Identify restrictions for partial homogeneity model
  cas <- data.frame(NULL)
  lapply(4:6, function(k) { #input file for different number of classes
    partial = 0 
    id = 0
    mat <- data.frame(NULL)
    for(g in 1:length(unique(data$id_k))){
      
      cast<- data.frame(outer(paste0("%g#",g), paste0(".c#",1:k,"%"), paste, sep=""))
      vars<-sort(colnames(data)[grepl('^GND', colnames(data))])
      for (c in 1:k) {
        n = length(vars)*id
        id = id + 1  
        for(v in 1:length(vars)){
          if (!is.na(partial[c])  & g != 1 & c %in% partial) {
            mat[c,1] <- id
            mat[c,v+1] <- cas[partial[c],v+2]
          } else {
            mat[c,1] <- id
            mat[c,v+1] <- paste0("[",vars[v],"$1] (",n + v,");")
          }
        }
      }
      cas <- rbind(cas,cbind(t(cast),mat))
    }
    casr <- reshape2::melt(cas, id.vars = "V1") %>% arrange(V1) 
    casr <- data.frame(casr[,c(-1,-2)])
    
    casc <- textConnection("castxPH", "w")
    sink(casc)   # divert output to tc connection
    print(casr)  # print in str string instead of console
    sink()     # set the output back to console
    close(casc)
    castxPH <- substr(castxPH,5+2,nchar(castxPH[1])) # we get rid of the row numbers that come with print
    castxPH <- paste0(castxPH[-1],collapse="\n")
    
    fileConn <- file(paste0("data/MplusModels/CountryMG/GND_MGCnty_C",j,"cl",sprintf("%d", k),"_1CHet.inp"))
    writeLines(c(
      paste0("TITLE: C.Het MG Country LCA GND C", j," with ", k ," classes;"),
      "DATA: ",
      paste0("FILE = GND_DtaC",j,".dat;"),
      "",
      "VARIABLE: ",
      paste0("NAMES = ", paste(colnames(data), collapse = "\n"),";"),
      "IDVARIABLE = IDSTUD;",
      paste0("USEVARIABLES = ", paste(colnames(data)[grepl('^GND', colnames(data))], collapse = "\n"),";"),
      paste0("CATEGORICAL = ", paste(colnames(data)[grepl('^GND', colnames(data))], collapse = "\n"),";"),
      "MISSING = .;",
      paste0("CLASSES = g(",length(unique(data$id_k)),") ",sprintf("c(%d);", k)),
      paste0("KNOWNCLASS = g(id_k ="), 
      paste(str, collapse = "\n"),
      ");",
      "WEIGHT = ws;",
      "STRATIFICATION = id_s;",
      "CLUSTER = id_j;",
      " ",
      "ANALYSIS:",
      "TYPE = COMPLEX MIXTURE;",
      "PROCESSORS = 4;",
      "STARTS = 1000 250;",
      "STITERATIONS = 20;",
      "STSEED = 288;",
      "",
      "MODEL:",
      "%OVERALL%",
      "c ON g;",
      " ",
      castxPH,
      " ",
      "OUTPUT: ",
      "TECH10",#for bivariate model fit information
      #"TECH11",
      #"SAMP",
      #"TECH14;", Time consuming tests
      #"CINTERVAL",
      "SVALUES",
      ";",
      "",
      "SAVEDATA:",
      paste0("FILE = GND_Prob_MGCntry_C", j,"cl", k,"_1CHet.dat;"),
      "SAVE = CPROBABILITIES;"
      
    ), fileConn)
    close(fileConn)
  })
}
for (j in 3:3) { #input file for each CYCLE 1:3
  data <- ds_lc0 %>%  filter(CYCLE == paste0("C",j)) %>%
    dplyr::select(all_of(sample), all_of(ScalesETH), IDSTUD, COUNTRY) %>%
    mutate_if(is.factor, ~ as.numeric(.x)) %>%
    data.frame()
  # list of countries for multigroup model
  t <- cbind(unique(data[, "id_k"]),"!", unique(data$COUNTRY), 
             g = rownames(unique(data$COUNTRY)))
  tc <- textConnection("str", "w")
  sink(tc)   # divert output to tc connection
  print(t, quote = FALSE)  # print in str string instead of console
  sink()     # set the output back to console
  close(tc)
  str <- substr(str,floor(length(str)/10)+5,nchar(str[1])) # we get rid of the row numbers that come with print
  str <- paste0(str[-1],collapse="\n")        # we build a proper unique string with your pipes and new lines
  
  data<- data %>% select(-COUNTRY)
  prepareMplusData(df = data,
                   filename = paste0("data/MplusModels/CountryMG/ETH_DtaC",j,".dat"), interactive =FALSE)
  
  # Identify restrictions for partial homogeneity model
  cas <- data.frame(NULL)
  lapply(3:5, function(k) { #input file for different number of classes
    partial = 0 
    id = 0
    mat <- data.frame(NULL)
    for(g in 1:length(unique(data$id_k))){
      
      cast<- data.frame(outer(paste0("%g#",g), paste0(".c#",1:k,"%"), paste, sep=""))
      vars<-sort(colnames(data)[grepl('^ETH', colnames(data))])
      for (c in 1:k) {
        n = length(vars)*id
        id = id + 1  
        for(v in 1:length(vars)){
          if (!is.na(partial[c])  & g != 1 & c %in% partial) {
            mat[c,1] <- id
            mat[c,v+1] <- cas[partial[c],v+2]
          } else {
            mat[c,1] <- id
            mat[c,v+1] <- paste0("[",vars[v],"$1] (",n + v,");")
          }
        }
      }
      cas <- rbind(cas,cbind(t(cast),mat))
    }
    casr <- reshape2::melt(cas, id.vars = "V1") %>% arrange(V1) 
    casr <- data.frame(casr[,c(-1,-2)])
    
    casc <- textConnection("castxPH", "w")
    sink(casc)   # divert output to tc connection
    print(casr)  # print in str string instead of console
    sink()     # set the output back to console
    close(casc)
    castxPH <- substr(castxPH,5+2,nchar(castxPH[1])) # we get rid of the row numbers that come with print
    castxPH <- paste0(castxPH[-1],collapse="\n")
    
    fileConn <- file(paste0("data/MplusModels/CountryMG/ETH_MGCnty_C",j,"cl",sprintf("%d", k),"_1CHet.inp"))
    writeLines(c(
      paste0("TITLE: C.Het MG Country LCA ETH C", j," with ", k ," classes;"),
      "DATA: ",
      paste0("FILE = ETH_DtaC",j,".dat;"),
      "",
      "VARIABLE: ",
      paste0("NAMES = ", paste(colnames(data), collapse = "\n"),";"),
      "IDVARIABLE = IDSTUD;",
      paste0("USEVARIABLES = ", paste(colnames(data)[grepl('^ETH', colnames(data))], collapse = "\n"),";"),
      paste0("CATEGORICAL = ", paste(colnames(data)[grepl('^ETH', colnames(data))], collapse = "\n"),";"),
      "MISSING = .;",
      paste0("CLASSES = g(",length(unique(data$id_k)),") ",sprintf("c(%d);", k)),
      paste0("KNOWNCLASS = g(id_k ="), 
      paste(str, collapse = "\n"),
      ");",
      "WEIGHT = ws;",
      "STRATIFICATION = id_s;",
      "CLUSTER = id_j;",
      " ",
      "ANALYSIS:",
      "TYPE = COMPLEX MIXTURE;",
      "PROCESSORS = 4;",
      "STARTS = 1000 250;",
      "STITERATIONS = 20;",
      "STSEED = 288;",
      "",
      "MODEL:",
      "%OVERALL%",
      "c ON g;",
      " ",
      castxPH,
      " ",
      "OUTPUT: ",
      "TECH10",#for bivariate model fit information
      #"TECH11",
      #"SAMP",
      #"TECH14;", Time consuming tests
      #"CINTERVAL",
      "SVALUES",
      ";",
      "",
      "SAVEDATA:",
      paste0("FILE = ETH_Prob_MGCntry_C", j,"cl", k,"_1CHet.dat;"),
      "SAVE = CPROBABILITIES;"
      
    ), fileConn)
    close(fileConn)
  })
}
for (j in 3:3) { #input file for each CYCLE 1:3
  data <- ds_lc0 %>%  filter(CYCLE == paste0("C",j)) %>%
    dplyr::select(all_of(sample), all_of(ScalesGND), all_of(ScalesETH), IDSTUD, COUNTRY) %>%
    mutate_if(is.factor, ~ as.numeric(.x)) %>%
    data.frame()
  # list of countries for multigroup model
  t <- cbind(unique(data[, "id_k"]),"!", unique(data$COUNTRY), 
             g = rownames(unique(data$COUNTRY)))
  tc <- textConnection("str", "w")
  sink(tc)   # divert output to tc connection
  print(t, quote = FALSE)  # print in str string instead of console
  sink()     # set the output back to console
  close(tc)
  str <- substr(str,floor(length(str)/10)+5,nchar(str[1])) # we get rid of the row numbers that come with print
  str <- paste0(str[-1],collapse="\n")        # we build a proper unique string with your pipes and new lines
  
  data<- data %>% select(-COUNTRY)
  # prepareMplusData(df = data,
  #                  filename = paste0("data/MplusModels/CountryMG/GNDETH_DtaC",j,".dat"), interactive =FALSE)
  
  # Identify restrictions for partial homogeneity model
  cas <- data.frame(NULL)
  lapply(7:7, function(k) { #input file for different number of classes
    partial = 0 
    id = 0
    mat <- data.frame(NULL)
    for(g in 1:length(unique(data$id_k))){
      
      cast<- data.frame(outer(paste0("%g#",g), paste0(".c#",1:k,"%"), paste, sep=""))
      vars<-sort(colnames(data)[grepl('^GND|ETH', colnames(data))])
      for (c in 1:k) {
        n = length(vars)*id
        id = id + 1  
        for(v in 1:length(vars)){
          if (!is.na(partial[c])  & g != 1 & c %in% partial) {
            mat[c,1] <- id
            mat[c,v+1] <- cas[partial[c],v+2]
          } else {
            mat[c,1] <- id
            mat[c,v+1] <- paste0("[",vars[v],"$1] (",n + v,");")
          }
        }
      }
      cas <- rbind(cas,cbind(t(cast),mat))
    }
    casr <- reshape2::melt(cas, id.vars = "V1") %>% arrange(V1) 
    casr <- data.frame(casr[,c(-1,-2)])
    options(max.print = 2000)
    casc <- textConnection("castxPH", "w")
    sink(casc)   # divert output to tc connection
    print(casr)  # print in str string instead of console
    sink()     # set the output back to console
    close(casc)
    castxPH <- substr(castxPH,5+2,nchar(castxPH[1])) # we get rid of the row numbers that come with print
    castxPH <- paste0(castxPH[-1],collapse="\n")
    
    fileConn <- file(paste0("data/MplusModels/CountryMG/GNDETH_MGCnty_C",j,"cl",sprintf("%d", k),"_1CHet.inp"))
    writeLines(c(
      paste0("TITLE: C.Het MG Country LCA GNDETH C", j," with ", k ," classes;"),
      "DATA: ",
      paste0("FILE = GNDETH_DtaC",j,".dat;"),
      "",
      "VARIABLE: ",
      paste0("NAMES = ", paste(colnames(data), collapse = "\n"),";"),
      "IDVARIABLE = IDSTUD;",
      paste0("USEVARIABLES = ", paste(colnames(data)[grepl('^ETH|GND', colnames(data))], collapse = "\n"),";"),
      paste0("CATEGORICAL = ", paste(colnames(data)[grepl('^ETH|GND', colnames(data))], collapse = "\n"),";"),
      "MISSING = .;",
      paste0("CLASSES = g(",length(unique(data$id_k)),") ",sprintf("c(%d);", k)),
      paste0("KNOWNCLASS = g(id_k ="), 
      paste(str, collapse = "\n"),
      ");",
      "WEIGHT = ws;",
      "STRATIFICATION = id_s;",
      "CLUSTER = id_j;",
      " ",
      "ANALYSIS:",
      "TYPE = COMPLEX MIXTURE;",
      "PROCESSORS = 4;",
      "STARTS = 1000 250;",
      "STITERATIONS = 20;",
      "STSEED = 288;",
      "",
      "MODEL:",
      "%OVERALL%",
      "c ON g;",
      " ",
      castxPH,
      " ",
      "OUTPUT: ",
      "TECH10",#for bivariate model fit information
      #"TECH11",
      #"SAMP",
      #"TECH14;", Time consuming tests
      #"CINTERVAL",
      "SVALUES",
      ";",
      "",
      "SAVEDATA:",
      paste0("FILE = GNDETH_Prob_MGCntry_C", j,"cl", k,"_1CHet.dat;"),
      "SAVE = CPROBABILITIES;"
      
    ), fileConn)
    close(fileConn)
  })
}


#----------------LCA with covariate COUNTRY by CYCLE--------------------------------------
for (j in 3:3) { #input file for each CYCLE 1:3
  data <- ds_lc0 %>% filter(CYCLE == paste0("C",j)) %>%
    dplyr::select(all_of(sample), all_of(ScalesGND), IDSTUD) %>%
    mutate_if(is.factor, ~ as.numeric(.x)) %>%
    data.frame()
  prepareMplusData(df = data,
                   filename = paste0("data/MplusModels/CountryCov/GND_Dta_C",j,".dat"), interactive =FALSE)
  cas <- list()
  lapply(4:6, function(k) { #input file for different number of classes
    for(c in 1:k){
      cas[c] <- paste0("%c#",c,"%")
    }
    fileConn <- file(paste0("data/MplusModels/CountryCov/GND_CntCov_C",j,"cl",sprintf("%d", k),".inp"))
    writeLines(c(
      paste0("TITLE: LCA CntCov C", j," GND with ", k ," classes;"),
      "DATA: ",
      paste0("FILE = GND_Dta_C",j,".dat;"),
      "",
      "VARIABLE: ",
      paste0("NAMES = ", paste(colnames(data), collapse = "\n"),";"),
      "IDVARIABLE = IDSTUD;",
      paste0("USEVARIABLES = ", paste(colnames(data)[grepl('^GND', colnames(data))], collapse = "\n")," id_k;"),
      paste0("CATEGORICAL = ", paste(colnames(data)[grepl('^GND', colnames(data))], collapse = "\n")," id_k;"),
      "MISSING = .;",
      paste0("CLASSES = ",sprintf("c(%d);", k)),
      "WEIGHT = ws;",
      "STRATIFICATION = id_s;",
      "CLUSTER = id_j;",
      " ",
      "ANALYSIS:",
      "TYPE = COMPLEX MIXTURE;",
      "PROCESSORS = 4;",
      "STARTS = 1000 250;",
      "STITERATIONS = 20;",
      "STSEED = 288;",
      " ",
      "MODEL:",
      "%OVERALL%",
      "c on id_k",
      ";",
      "OUTPUT: ",
      "TECH10",#for bivariate model fit information
      "TECH11",
      #"SAMP",
      "STDYX",
      "TECH14", #Time consuming tests
      #"CINTERVAL",
      "SVALUES",
      ";",
      "",
      "SAVEDATA:",
      paste0("FILE = GND_Prob_CntCov_C", j,"cl", k,".dat;"),
      "SAVE = CPROBABILITIES;"
      
    ), fileConn)
    close(fileConn)
  })
}
for (j in 3:3) { #input file for each CYCLE 1:3
  data <- ds_lc0 %>% filter(CYCLE == paste0("C",j)) %>%
    dplyr::select(all_of(sample), all_of(ScalesETH), IDSTUD) %>%
    mutate_if(is.factor, ~ as.numeric(.x)) %>%
    data.frame()
  prepareMplusData(df = data,
                   filename = paste0("data/MplusModels/CountryCov/ETH_Dta_C",j,".dat"), interactive =FALSE)
  cas <- list()
  lapply(4:6, function(k) { #input file for different number of classes
    for(c in 1:k){
      cas[c] <- paste0("%c#",c,"%")
    }
    fileConn <- file(paste0("data/MplusModels/CountryCov/ETH_CntCov_C",j,"cl",sprintf("%d", k),".inp"))
    writeLines(c(
      paste0("TITLE: LCA CntCov C", j," ETH with ", k ," classes;"),
      "DATA: ",
      paste0("FILE = ETH_Dta_C",j,".dat;"),
      "",
      "VARIABLE: ",
      paste0("NAMES = ", paste(colnames(data), collapse = "\n"),";"),
      "IDVARIABLE = IDSTUD;",
      paste0("USEVARIABLES = ", paste(colnames(data)[grepl('^ETH', colnames(data))], collapse = "\n")," id_k;"),
      paste0("CATEGORICAL = ", paste(colnames(data)[grepl('^ETH', colnames(data))], collapse = "\n")," id_k;"),
      "MISSING = .;",
      paste0("CLASSES = ",sprintf("c(%d);", k)),
      "WEIGHT = ws;",
      "STRATIFICATION = id_s;",
      "CLUSTER = id_j;",
      " ",
      "ANALYSIS:",
      "TYPE = COMPLEX MIXTURE;",
      "PROCESSORS = 4;",
      "STARTS = 1000 250;",
      "STITERATIONS = 20;",
      "STSEED = 288;",
      " ",
      "MODEL:",
      "%OVERALL%",
      "c on id_k",
      ";",
      "OUTPUT: ",
      "TECH10",#for bivariate model fit information
      "TECH11",
      #"SAMP",
      "STDYX",
      "TECH14", #Time consuming tests
      #"CINTERVAL",
      "SVALUES",
      ";",
      "",
      "SAVEDATA:",
      paste0("FILE = ETH_Prob_CntCov_C", j,"cl", k,".dat;"),
      "SAVE = CPROBABILITIES;"
      
    ), fileConn)
    close(fileConn)
  })
}
for (j in 3:3) { #input file for each CYCLE 1:3
  data <- ds_lc0 %>% filter(CYCLE == paste0("C",j)) %>%
    dplyr::select(all_of(sample), all_of(ScalesGND), all_of(ScalesETH), IDSTUD) %>%
    mutate_if(is.factor, ~ as.numeric(.x)) %>%
    data.frame()
  prepareMplusData(df = data,
                   filename = paste0("data/MplusModels/CountryCov/GNDETH_Dta_C",j,".dat"), interactive =FALSE)
  cas <- list()
  lapply(5:7, function(k) { #input file for different number of classes
    for(c in 1:k){
      cas[c] <- paste0("%c#",c,"%")
    }
    fileConn <- file(paste0("data/MplusModels/CountryCov/GNDETH_CntCov_C",j,"cl",sprintf("%d", k),".inp"))
    writeLines(c(
      paste0("TITLE: LCA CntCov C", j," GNDETH with ", k ," classes;"),
      "DATA: ",
      paste0("FILE = GNDETH_Dta_C",j,".dat;"),
      "",
      "VARIABLE: ",
      paste0("NAMES = ", paste(colnames(data), collapse = "\n"),";"),
      "IDVARIABLE = IDSTUD;",
      paste0("USEVARIABLES = ", paste(colnames(data)[grepl('^ETH|GND', colnames(data))], collapse = "\n")," id_k;"),
      paste0("CATEGORICAL = ", paste(colnames(data)[grepl('^ETH|GND', colnames(data))], collapse = "\n")," id_k;"),
      "MISSING = .;",
      paste0("CLASSES = ",sprintf("c(%d);", k)),
      "WEIGHT = ws;",
      "STRATIFICATION = id_s;",
      "CLUSTER = id_j;",
      " ",
      "ANALYSIS:",
      "TYPE = COMPLEX MIXTURE;",
      "PROCESSORS = 4;",
      "STARTS = 1000 250;",
      "STITERATIONS = 20;",
      "STSEED = 288;",
      " ",
      "MODEL:",
      "%OVERALL%",
      "c on id_k",
      ";",
      "OUTPUT: ",
      "TECH10",#for bivariate model fit information
      "TECH11",
      #"SAMP",
      "STDYX",
      "TECH14", #Time consuming tests
      #"CINTERVAL",
      "SVALUES",
      ";",
      "",
      "SAVEDATA:",
      paste0("FILE = GNDETH_Prob_CntCov_C", j,"cl", k,".dat;"),
      "SAVE = CPROBABILITIES;"
      
    ), fileConn)
    close(fileConn)
  })
}



#----------------3P.Hom Multigroup Region --------------------------------------
for (j in 3:3) { #input file for each CYCLE 1:3
  
  data <- ds_lc0 %>%  filter(CYCLE == paste0("C",j)) %>%
    dplyr::select(-CYCLE, -COUNTRY) %>%
    mutate(GROUP = factor(GROUP)) %>% 
    mutate_if(is.factor, ~ as.numeric(.x)) %>%
    data.frame()
  
  # list of regions for multigroup model
  t <- cbind(unique(data[, "GROUP"]),"!", unique(ds_lc0[ds_lc0$CYCLE == paste0("C",j), "GROUP"]), 
             g = rownames(unique(ds_lc0[ds_lc0$CYCLE == paste0("C",j), "GROUP"])))
  tc <- textConnection("str", "w")
  sink(tc)   # divert output to tc connection
  print(t)  # print in str string instead of console
  sink()     # set the output back to console
  close(tc)
  str <- substr(str,floor(length(str)/10)+3,nchar(str[1])) # we get rid of the row numbers that come with print
  str <- paste0(str[-1],collapse="\n")        # we build a proper unique string with your pipes and new lines
  
  # Identify restrictions for partial homogeneity model
  lapply(3:3, function(k) { #input file for different number of classes
    cas <- data.frame(NULL)
    
    partial = 1:k 
    id = 0
    mat <- data.frame(NULL)
    for(g in 1:length(unique(data$GROUP))){
      
      cast<- data.frame(outer(paste0("%g#",g), paste0(".c#",1:k,"%"), paste, sep=""))
      vars<-sort(colnames(data)[grepl('^GND', colnames(data))])
      for (c in 1:k) {
        n = length(vars)*id
        id = id + 1  
        for(v in 1:length(vars)){
          if (!is.na(partial[c])  & g != 1 & c %in% partial) {
            mat[c,1] <- id
            mat[c,v+1] <- cas[partial[c],v+2]
          } else {
            mat[c,1] <- id
            mat[c,v+1] <- paste0("[",vars[v],"$1] (",n + v,");")
          }
        }
      }
      cas <- rbind(cas,cbind(t(cast),mat))
    }
    casr <- reshape2::melt(cas, id.vars = "V1") %>% arrange(V1) 
    casr <- data.frame(casr[,c(-1,-2)])
    
    casc <- textConnection("castxPH", "w")
    sink(casc)   # divert output to tc connection
    print(casr)  # print in str string instead of console
    sink()     # set the output back to console
    close(casc)
    castxPH <- substr(castxPH,5+2,nchar(castxPH[1])) # we get rid of the row numbers that come with print
    castxPH <- paste0(castxPH[-1],collapse="\n")
    
    fileConn <- file(paste0("data/MplusModels/RegionMG/MGRegion_C",j,"cl",sprintf("%d", k),"_3PHom.inp"))
    writeLines(c(
      paste0("TITLE: P.Hom MG Region LCA C", j," with ", k ," classes;"),
      "DATA: ",
      paste0("FILE = DtaC",j,".dat;"),
      "",
      "VARIABLE: ",
      paste0("NAMES = ", paste(colnames(data), collapse = "\n"),";"),
      "IDVARIABLE = IDSTUD;",
      paste0("USEVARIABLES = ", paste(colnames(data)[grepl('^GND', colnames(data))], collapse = "\n"),";"),
      paste0("CATEGORICAL = ", paste(colnames(data)[grepl('^GND', colnames(data))], collapse = "\n"),";"),
      "MISSING = .;",
      paste0("CLASSES = g(",length(unique(data$GROUP)),") ",sprintf("c(%d);", k)),
      paste0("KNOWNCLASS = g(GROUP ="), 
      paste(str, collapse = "\n"),
      ");",
      "WEIGHT = ws;",
      "STRATIFICATION = id_s;",
      "CLUSTER = id_j;",
      " ",
      "ANALYSIS:",
      "TYPE = COMPLEX MIXTURE;",
      "PROCESSORS = 4;",
      "STARTS = 100 50;",
      "STITERATIONS = 5;",
      "STSEED = 288;",
      "",
      "MODEL:",
      "%OVERALL%",
      "c ON g;",
      " ",
      castxPH,
      " ",
      "OUTPUT: ",
      "TECH10",#for bivariate model fit information
      #"TECH11",
      #"SAMP",
      #"TECH14;", Time consuming tests
      #"CINTERVAL",
      "SVALUES",
      ";",
      "",
      "SAVEDATA:",
      paste0("FILE = Prob_MGRegion_C", j,"cl", k,"PHom.dat;"),
      "SAVE = CPROBABILITIES;"
      
    ), fileConn)
    close(fileConn)
  })
}

#--------------------
runModels(target = "data/MplusModels/ByCountry", recursive = TRUE, replaceOutfile = "never") #modifiedDate
runModels(target = "data/MplusModels/LCA", recursive = TRUE, replaceOutfile = "never") #modifiedDate
#runModels(target = "data/MplusModels/CountryCov", recursive = TRUE, replaceOutfile = "never") #modifiedDate
runModels(target = "data/MplusModels/CountryMG", recursive = FALSE, replaceOutfile = "never") #modifiedDate
runModels(target = "data/MplusModels/Multilevel", recursive = FALSE, replaceOutfile = "never") #modifiedDate

lcaGND <- readModels(target = "data/MplusModels/LCA", recursive = FALSE, filefilter = "GND_lca_*")
lcaETH <- readModels(target = "data/MplusModels/LCA", recursive = FALSE, filefilter = "/ETH_lca_")
lcaGNDETH <- readModels(target = "data/MplusModels/LCA", recursive = FALSE, filefilter = "GNDETH_lca_*")

ConflcaGND <- readModels(target = "data/MplusModels/LCA", recursive = FALSE, filefilter = "Conflca_*")
ConflcaETH <- readModels(target = "data/MplusModels/LCA", recursive = FALSE, filefilter = "Conflca_*")

save(lcaGND, ConflcaGND,
     lcaETH, ConflcaETH,
     lcaGNDETH, 
     file = "data/MplusModels_LCA.RData")

CntCovGND <- readModels(target = "data/MplusModels/CountryCov", recursive = FALSE, filefilter = "GND_CntCov_*")
CntCovETH <- readModels(target = "data/MplusModels/CountryCov", recursive = FALSE, filefilter = "/ETH_CntCov_*")
#CntCovGNDETH <- readModels(target = "data/MplusModels/CountryCov", recursive = FALSE, filefilter = "GNDETH_CntCov_*")

save(CntCovGND, 
     CntCovETH, 
     #CntCovGNDETH,
     file = "data/MplusModels_CntCov.RData")


MGCntry_GND <- readModels(target = "data/MplusModels/CountryMG", recursive = FALSE, filefilter = "GND_mgCnty_*")
MGCntry_ETH <- readModels(target = "data/MplusModels/CountryMG", recursive = FALSE, filefilter = "/ETH_mgCnty_*")
#MGCntry_GNDETH <- readModels(target = "data/MplusModels/CountryMG", recursive = FALSE, filefilter = "GNDETH_mgCnty_*")

save(MGCntry_GND, 
     MGCntry_ETH, 
     #MGCntry_GNDETH,
     file = "data/MplusModels_MGCntry.RData")


ByCountry_GND <- readModels(target = "data/MplusModels/ByCountry", recursive = TRUE, filefilter = "GNDlca_[A-Z]{3}_C3cl")
ByCountry_ETH <- readModels(target = "data/MplusModels/ByCountry", recursive = TRUE, filefilter = "/ETHlca_[A-Z]{3}_C3cl")
ByCountry_GNDETH <- readModels(target = "data/MplusModels/ByCountry", recursive = TRUE, filefilter = "GNDETHlca_[A-Z]{3}_C3cl")

save(ByCountry_GND,
     ByCountry_ETH,
     ByCountry_GNDETH,
     file = "data/MplusModels_ByCountry.RData")


