library(MplusAutomation)
ds_lc <- data_model %>% 
  dplyr::select(all_of(sample), c("GND1", "GND2", "GND4", "GND6"), IDSTUD, GROUP, CYCLE, COUNTRY) 

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



#------------------------------------------------------------
#-----------------------Mplus code---------------------------
#------------------------------------------------------------

#----------------All scales together by CYCLE--------------------------------------
for (j in 3:3) { #input file for each CYCLE 1:3
  data <- ds_lc0 %>%  filter(CYCLE == paste0("C",j) & GROUP == "South America") %>%
    dplyr::select(-GROUP, -CYCLE, -COUNTRY) %>%
    mutate_if(is.factor, ~ as.numeric(.x)) %>%
    data.frame()
  prepareMplusData(df = data,
                   filename = paste0("data/MplusModels/LCA/LA_Dta_C",j,".dat"), interactive =FALSE)
  cas <- list()
  lapply(1:4, function(k) { #input file for different number of classes
    for(c in 1:k){
      cas[c] <- paste0("%c#",c,"%")
    }
    fileConn <- file(paste0("data/MplusModels/LCA/LA_lca_C",j,"cl",sprintf("%d", k),".inp"))
    writeLines(c(
      paste0("TITLE: LCA C", j," LA with ", k ," classes;"),
      "DATA: ",
      paste0("FILE = LA_Dta_C",j,".dat;"),
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
      "STARTS = 100 50;",
      "STITERATIONS = 5;",
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
      paste0("FILE = LA_Prob_C", j,"cl", k,".dat;"),
      "SAVE = CPROBABILITIES;"
      
    ), fileConn)
    close(fileConn)
  })
}
for (j in 3:3) { #input file for each CYCLE 1:3
  data <- ds_lc0 %>%  filter(CYCLE == paste0("C",j) & GROUP == "Europe") %>%
    dplyr::select(-GROUP, -CYCLE, -COUNTRY) %>%
    mutate_if(is.factor, ~ as.numeric(.x)) %>%
    data.frame()
  prepareMplusData(df = data,
                   filename = paste0("data/MplusModels/LCA/EU_Dta_C",j,".dat"), interactive =FALSE)
  cas <- list()
  lapply(1:4, function(k) { #input file for different number of classes
    for(c in 1:k){
      cas[c] <- paste0("%c#",c,"%")
    }
    fileConn <- file(paste0("data/MplusModels/LCA/EU_lca_C",j,"cl",sprintf("%d", k),".inp"))
    writeLines(c(
      paste0("TITLE: LCA C", j," EU with ", k ," classes;"),
      "DATA: ",
      paste0("FILE = EU_Dta_C",j,".dat;"),
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
      "STARTS = 100 50;",
      "STITERATIONS = 5;",
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
      paste0("FILE = EU_Prob_C", j,"cl", k,".dat;"),
      "SAVE = CPROBABILITIES;"
      
    ), fileConn)
    close(fileConn)
  })
}
#----------------By country scales together by CYCLE ----------------------
for (j in 3:3) { #input file for each CYCLE 1:3
  data1 <- ds_lc0 %>%  filter(CYCLE == paste0("C",j)) %>%
    dplyr::select(-GROUP, - CYCLE) %>%
    mutate_if(is.factor, ~ as.numeric(.x)) %>%
    data.frame()
  cnt <- unique(data1[,c("COUNTRY","id_k")]) %>% arrange(as.character(COUNTRY))
  data1 <- data1  %>% dplyr::select(-COUNTRY)
  prepareMplusData(df = data1,
                   filename = paste0("data/MplusModels/ByCountry/DtaC",j,".dat"), interactive =FALSE)
  
  for(c in 1:nrow(cnt)){
    data <- data1 %>%  filter(id_k == cnt$id_k[c])
    
    lapply(1:4, function(k) { #input file for different number of classes
      fileConn <- file(paste0("data/MplusModels/ByCountry/lca_",cnt$COUNTRY[c],"_C",j,"cl",sprintf("%d", k),".inp"))
      writeLines(c(
        paste0("TITLE: ", cnt$COUNTRY[c], "LCA - C", j," with ", k ," classes;"),
        "DATA: ",
        paste0("FILE = DtaC",j,".dat;"),
        "",
        "VARIABLE: ",
        paste0("NAMES = ", paste(colnames(data), collapse = "\n"),";"),
        "IDVARIABLE = IDSTUD;",
        paste0("USEVARIABLES = ", paste(colnames(data)[grepl('^GND', colnames(data))], collapse = "\n"),";"),
        paste0("USEOBSERVATIONS ARE id_k EQ ", cnt$id_k[c], ";"),
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
        "STARTS = 100 25;",
        "STITERATIONS = 5;",
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
        paste0("FILE = Prob_", cnt$COUNTRY[c] ,"_lca_C", j,"cl", k,".dat;"),
        "SAVE = CPROBABILITIES;"
        
      ), fileConn)
      close(fileConn)
    })
  }
}
#------


#----------------C.Het Multigroup COUNTRY by CYCLE--------------------------------------
for (j in 3:3) { #input file for each CYCLE 1:3
  data <- ds_lc0 %>%  filter(CYCLE == paste0("C",j) & GROUP == "South America") %>%
    dplyr::select(-GROUP, -CYCLE, -COUNTRY) %>%
    mutate_if(is.factor, ~ as.numeric(.x)) %>%
    data.frame()

  prepareMplusData(df = data,
                   filename = paste0("data/MplusModels/CountryMG/LA_DtaC",j,".dat"), interactive =FALSE)
  
  # list of countries for multigroup model
  t <- cbind(unique(data[, "id_k"]),"!", unique(ds_lc0[ds_lc0$CYCLE == paste0("C",j) & ds_lc0$GROUP == "South America", "COUNTRY"]), 
             g = rownames(unique(ds_lc0[ds_lc0$CYCLE == paste0("C",j) & ds_lc0$GROUP == "South America", "COUNTRY"])))
  tc <- textConnection("str", "w")
  sink(tc)   # divert output to tc connection
  print(t)  # print in str string instead of console
  sink()     # set the output back to console
  close(tc)
  str <- substr(str,floor(length(str)/10)+3,nchar(str[1])) # we get rid of the row numbers that come with print
  str <- paste0(str[-1],collapse="\n")        # we build a proper unique string with your pipes and new lines
  
  # Identify restrictions for partial homogeneity model
  cas <- data.frame(NULL)
  lapply(2:4, function(k) { #input file for different number of classes
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

    fileConn <- file(paste0("data/MplusModels/CountryMG/LA_MGCnty_C",j,"cl",sprintf("%d", k),"_1CHet.inp"))
    writeLines(c(
      paste0("TITLE: C.Het MG Country LCA LA C", j," with ", k ," classes;"),
      "DATA: ",
      paste0("FILE = LA_DtaC",j,".dat;"),
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
      paste0("FILE = LA_Prob_MGCntry_C", j,"cl", k,"CHet.dat;"),
      "SAVE = CPROBABILITIES;"
      
    ), fileConn)
    close(fileConn)
  })
}
for (j in 3:3) { #input file for each CYCLE 1:3
  data <- ds_lc0 %>%  filter(CYCLE == paste0("C",j) & GROUP == "Europe") %>%
    dplyr::select(-GROUP, -CYCLE, -COUNTRY) %>%
    mutate_if(is.factor, ~ as.numeric(.x)) %>%
    data.frame()
  
  prepareMplusData(df = data,
                   filename = paste0("data/MplusModels/CountryMG/EU_DtaC",j,".dat"), interactive =FALSE)
  
  # list of countries for multigroup model
  t <- cbind(unique(data[, "id_k"]),"!", unique(ds_lc0[ds_lc0$CYCLE == paste0("C",j)  & ds_lc0$GROUP == "Europe", "COUNTRY"]), 
             g = rownames(unique(ds_lc0[ds_lc0$CYCLE == paste0("C",j) & ds_lc0$GROUP == "Europe", "COUNTRY"])))
  tc <- textConnection("str", "w")
  sink(tc)   # divert output to tc connection
  print(t)  # print in str string instead of console
  sink()     # set the output back to console
  close(tc)
  str <- substr(str,floor(length(str)/10)+3,nchar(str[1])) # we get rid of the row numbers that come with print
  str <- paste0(str[-1],collapse="\n")        # we build a proper unique string with your pipes and new lines
  
  # Identify restrictions for partial homogeneity model
  cas <- data.frame(NULL)
  lapply(2:4, function(k) { #input file for different number of classes
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
    
    fileConn <- file(paste0("data/MplusModels/CountryMG/EU_MGCnty_C",j,"cl",sprintf("%d", k),"_1CHet.inp"))
    writeLines(c(
      paste0("TITLE: C.Het MG Country LCA EU C", j," with ", k ," classes;"),
      "DATA: ",
      paste0("FILE = EU_DtaC",j,".dat;"),
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
      paste0("FILE = EU_Prob_MGCntry_C", j,"cl", k,"CHet.dat;"),
      "SAVE = CPROBABILITIES;"
      
    ), fileConn)
    close(fileConn)
  })
}

#----------------P.Hom Multigroup COUNTRY by CYCLE--------------------------------------

for (j in 3:3) { #input file for each CYCLE 1:3
  data <- ds_lc0 %>%  filter(CYCLE == paste0("C",j) & GROUP == "South America") %>%
    dplyr::select(-GROUP, -CYCLE, -COUNTRY) %>%
    mutate_if(is.factor, ~ as.numeric(.x)) %>%
    data.frame()
  
  # list of countries for multigroup model
  t <- cbind(unique(data[, "id_k"]),"!", unique(ds_lc0[ds_lc0$CYCLE == paste0("C",j)  & ds_lc0$GROUP == "South America", "COUNTRY"]), 
             g = rownames(unique(ds_lc0[ds_lc0$CYCLE == paste0("C",j) & ds_lc0$GROUP == "South America", "COUNTRY"])))
  tc <- textConnection("str", "w")
  sink(tc)   # divert output to tc connection
  print(t)  # print in str string instead of console
  sink()     # set the output back to console
  close(tc)
  str <- substr(str,floor(length(str)/10)+3,nchar(str[1])) # we get rid of the row numbers that come with print
  str <- paste0(str[-1],collapse="\n")        # we build a proper unique string with your pipes and new lines
  
  # Identify restrictions for partial homogeneity model 
  cas <- data.frame(NULL)
  lapply(2:4, function(k) { #input file for different number of classes
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
    
    fileConn <- file(paste0("data/MplusModels/CountryMG/LA_MGCnty_C",j,"cl",sprintf("%d", k),"_2PHom.inp"))
    writeLines(c(
      paste0("TITLE: P.Hom MG Country LCA LA C", j," with ", k ," classes;"),
      "DATA: ",
      paste0("FILE = LA_DtaC",j,".dat;"),
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
      paste0("FILE = LA_Prob_MGCntry_C", j,"cl", k,"PHom.dat;"),
      "SAVE = CPROBABILITIES;"
      
    ), fileConn)
    close(fileConn)
  })
}
for (j in 3:3) { #input file for each CYCLE 1:3
  data <- ds_lc0 %>%  filter(CYCLE == paste0("C",j) & GROUP == "Europe") %>%
    dplyr::select(-GROUP, -CYCLE, -COUNTRY) %>%
    mutate_if(is.factor, ~ as.numeric(.x)) %>%
    data.frame()
  
  # list of countries for multigroup model
  t <- cbind(unique(data[, "id_k"]),"!", unique(ds_lc0[ds_lc0$CYCLE == paste0("C",j) & ds_lc0$GROUP == "Europe", "COUNTRY"]), 
             g = rownames(unique(ds_lc0[ds_lc0$CYCLE == paste0("C",j) & ds_lc0$GROUP == "Europe", "COUNTRY"])))
  tc <- textConnection("str", "w")
  sink(tc)   # divert output to tc connection
  print(t)  # print in str string instead of console
  sink()     # set the output back to console
  close(tc)
  str <- substr(str,floor(length(str)/10)+3,nchar(str[1])) # we get rid of the row numbers that come with print
  str <- paste0(str[-1],collapse="\n")        # we build a proper unique string with your pipes and new lines
  
  # Identify restrictions for partial homogeneity model 
  cas <- data.frame(NULL)
  lapply(2:4, function(k) { #input file for different number of classes
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
    
    fileConn <- file(paste0("data/MplusModels/CountryMG/EU_MGCnty_C",j,"cl",sprintf("%d", k),"_2PHom.inp"))
    writeLines(c(
      paste0("TITLE: P.Hom MG Country LCA EU C", j," with ", k ," classes;"),
      "DATA: ",
      paste0("FILE = EU_DtaC",j,".dat;"),
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
      paste0("FILE = EU_Prob_MGCntry_C", j,"cl", k,"PHom.dat;"),
      "SAVE = CPROBABILITIES;"
      
    ), fileConn)
    close(fileConn)
  })
}


#----------------C.Hom Multigroup COUNTRY by CYCLE--------------------------------------

for (j in 3:3) { #input file for each CYCLE 1:3
  
  data <- ds_lc0 %>%  filter(CYCLE == paste0("C",j) & GROUP == "South America") %>%
    dplyr::select(-GROUP, -CYCLE, -COUNTRY) %>%
    mutate_if(is.factor, ~ as.numeric(.x)) %>%
    data.frame()
  
  # list of countries for multigroup model
  t <- cbind(unique(data[, "id_k"]),"!", unique(ds_lc0[ds_lc0$CYCLE == paste0("C",j) & ds_lc0$GROUP == "South America", "COUNTRY"]), 
             g = rownames(unique(ds_lc0[ds_lc0$CYCLE == paste0("C",j)  & ds_lc0$GROUP == "South America", "COUNTRY"])))
  tc <- textConnection("str", "w")
  sink(tc)   # divert output to tc connection
  print(t)  # print in str string instead of console
  sink()     # set the output back to console
  close(tc)
  str <- substr(str,floor(length(str)/10)+3,nchar(str[1])) # we get rid of the row numbers that come with print
  str <- paste0(str[-1],collapse="\n")        # we build a proper unique string with your pipes and new lines
  
  cas <- data.frame(NULL)
  lapply(2:4, function(k) { #input file for different number of classes
     
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
    
    fileConn <- file(paste0("data/MplusModels/CountryMG/LA_MGCnty_C",j,"cl",sprintf("%d", k),"_3CHom.inp"))
    writeLines(c(
      paste0("TITLE:C.Hom MG Country LCA LA C", j," with ", k ," classes;"),
      "DATA: ",
      paste0("FILE = LA_DtaC",j,".dat;"),
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
      "STARTS = 100 50;",
      "STITERATIONS = 5;",
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
      paste0("FILE = LA_Prob_MGCntry_C", j,"cl", k,"CHom.dat;"),
      "SAVE = CPROBABILITIES;"
      
    ), fileConn)
    close(fileConn)
  })
}
for (j in 3:3) { #input file for each CYCLE 1:3
  
  data <- ds_lc0 %>%  filter(CYCLE == paste0("C",j) & GROUP == "Europe") %>%
    dplyr::select(-GROUP, -CYCLE, -COUNTRY) %>%
    mutate_if(is.factor, ~ as.numeric(.x)) %>%
    data.frame()
  
  # list of countries for multigroup model
  t <- cbind(unique(data[, "id_k"]),"!", unique(ds_lc0[ds_lc0$CYCLE == paste0("C",j) & ds_lc0$GROUP == "Europe", "COUNTRY"]), 
             g = rownames(unique(ds_lc0[ds_lc0$CYCLE == paste0("C",j) & ds_lc0$GROUP == "Europe", "COUNTRY"])))
  tc <- textConnection("str", "w")
  sink(tc)   # divert output to tc connection
  print(t)  # print in str string instead of console
  sink()     # set the output back to console
  close(tc)
  str <- substr(str,floor(length(str)/10)+3,nchar(str[1])) # we get rid of the row numbers that come with print
  str <- paste0(str[-1],collapse="\n")        # we build a proper unique string with your pipes and new lines
  
  cas <- data.frame(NULL)
  lapply(2:4, function(k) { #input file for different number of classes
    
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
    
    fileConn <- file(paste0("data/MplusModels/CountryMG/EU_MGCnty_C",j,"cl",sprintf("%d", k),"_3CHom.inp"))
    writeLines(c(
      paste0("TITLE:C.Hom MG Country LCA EU C", j," with ", k ," classes;"),
      "DATA: ",
      paste0("FILE = EU_DtaC",j,".dat;"),
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
      "STARTS = 100 50;",
      "STITERATIONS = 5;",
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
      paste0("FILE = EU_Prob_MGCntry_C", j,"cl", k,"CHom.dat;"),
      "SAVE = CPROBABILITIES;"
      
    ), fileConn)
    close(fileConn)
  })
}




#----------------1C.Het Multigroup Region --------------------------------------

for (j in 3:3) { #input file for each CYCLE 1:3
  data <- ds_lc0 %>%  filter(CYCLE == paste0("C",j)) %>%
    dplyr::select(-CYCLE, -COUNTRY) %>%
    mutate(GROUP = factor(GROUP)) %>% 
    mutate_if(is.factor, ~ as.numeric(.x)) %>%
    data.frame()
  prepareMplusData(df = data,
                   filename = paste0("data/MplusModels/RegionMG/DtaC",j,".dat"), interactive =FALSE)

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
    
    partial = 0
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
    
    fileConn <- file(paste0("data/MplusModels/RegionMG/MGRegion_C",j,"cl",sprintf("%d", k),"_1CHet.inp"))
    writeLines(c(
      paste0("TITLE: C.Het MG Region LCA C", j," with ", k ," classes;"),
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
      paste0("FILE = Prob_MGRegion_C", j,"cl", k,"CHet.dat;"),
      "SAVE = CPROBABILITIES;"
      
    ), fileConn)
    close(fileConn)
  })
}

#----------------2P.Het Multigroup Region --------------------------------------

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
    
    partial = 1:(k-1) 
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
    
    fileConn <- file(paste0("data/MplusModels/RegionMG/MGRegion_C",j,"cl",sprintf("%d", k),"_2PHet.inp"))
    writeLines(c(
      paste0("TITLE: P.Het k-1 MG Region LCA C", j," with ", k ," classes;"),
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
      paste0("FILE = Prob_MGRegion_C", j,"cl", k,"PHet.dat;"),
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

#----------------4C.Hom Multigroup Region--------------------------------------

for (j in 3:3) { #input file for each CYCLE 1:3
  
  data <- ds_lc0 %>%  filter(CYCLE == paste0("C",j)) %>%
    dplyr::select(-CYCLE, -COUNTRY) %>%
    mutate(GROUP = factor(GROUP)) %>% 
    mutate_if(is.factor, ~ as.numeric(.x)) %>%
    data.frame()
  
  # list of countries for multigroup model
  t <- cbind(unique(data[, "GROUP"]),"!", unique(ds_lc0[ds_lc0$CYCLE == paste0("C",j), "GROUP"]), 
             g = rownames(unique(ds_lc0[ds_lc0$CYCLE == paste0("C",j), "GROUP"])))
  tc <- textConnection("str", "w")
  sink(tc)   # divert output to tc connection
  print(t)  # print in str string instead of console
  sink()     # set the output back to console
  close(tc)
  str <- substr(str,floor(length(str)/10)+3,nchar(str[1])) # we get rid of the row numbers that come with print
  str <- paste0(str[-1],collapse="\n")        # we build a proper unique string with your pipes and new lines
  
  cas <- data.frame(NULL)
  lapply(3:3, function(k) { #input file for different number of classes
    
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
    
    fileConn <- file(paste0("data/MplusModels/RegionMG/MGRegion_C",j,"cl",sprintf("%d", k),"_4CHom.inp"))
    writeLines(c(
      paste0("TITLE:C.Hom MG Region LCA C", j," with ", k ," classes;"),
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
      paste0("FILE = Prob_MGRegion_C", j,"cl", k,"CHom.dat;"),
      "SAVE = CPROBABILITIES;"
      
    ), fileConn)
    close(fileConn)
  })
}

#--------

#runModels(target = "data/MplusModels", recursive = TRUE, replaceOutfile = "never") #modifiedDate
#runModels(target = "data/MplusModels/CountryMG", recursive = FALSE, replaceOutfile = "never") #modifiedDate
runModels(target = "data/MplusModels/RegionMG", recursive = FALSE, replaceOutfile = "never") #modifiedDate

lca_LA <- readModels(target = "data/MplusModels/LCA", recursive = FALSE, filefilter = "LA_lca_*")
MGCntry_LA <- readModels(target = "data/MplusModels/CountryMG", recursive = FALSE, filefilter = "LA_mgCnty_*")

save(lca_LA, 
     MGCntry_LA, 
     file = "data/MplusModels_LA.RData")


lca_EU <- readModels(target = "data/MplusModels/LCA", recursive = FALSE, filefilter = "EU_lca_*")
MGCntry_EU <- readModels(target = "data/MplusModels/CountryMG", recursive = FALSE, filefilter = "EU_mgCnty_*")

save(lca_EU, 
     MGCntry_EU, 
     file = "data/MplusModels_EU.RData")

RegionMG <- readModels(target = "data/MplusModels/RegionMG", recursive = FALSE, filefilter = "mgregion_*")
save(RegionMG, 
     file = "data/MplusModels_RegionMG.RData")

ByCountry_lca1C3 <- readModels(target = "data/MplusModels/ByCountry", recursive = TRUE, filefilter = "lca_[A-Z]{3}_C3cl1")
ByCountry_lca1<- list(C3 = ByCountry_lca1C3)

ByCountry_lca2C3 <- readModels(target = "data/MplusModels/ByCountry", recursive = TRUE, filefilter = "lca_[A-Z]{3}_C3cl2")
ByCountry_lca2<- list(C3 = ByCountry_lca2C3)

ByCountry_lca3C3 <- readModels(target = "data/MplusModels/ByCountry", recursive = TRUE, filefilter = "lca_[A-Z]{3}_C3cl3")
ByCountry_lca3<- list(C3 = ByCountry_lca3C3)

ByCountry_lca4C3 <- readModels(target = "data/MplusModels/ByCountry", recursive = TRUE, filefilter = "lca_[A-Z]{3}_C3cl4")
ByCountry_lca4<- list(C3 = ByCountry_lca4C3)

save(ByCountry_lca1, 
     ByCountry_lca2, 
     ByCountry_lca3, 
     ByCountry_lca4, 
     file = "data/MplusModels_ByCountryC3.RData")






#----------------By country Multigroup Gender by CYCLE-----
# 
# for (j in 1:3) { #input file for each CYCLE 1:3
# 
#   data1 <- ds_lc0 %>%  filter(CYCLE == paste0("C",j)) %>%
#     dplyr::select(all_of(Id), all_of(sampleID), all_of(ScalesM2), -CYCLE, T_GENDER, cntgnd) %>%
#     mutate_if(is.factor, ~ as.numeric(.x)) %>%
#     rename_with(~ gsub("bT_GNDREQ", "GND", .x, fixed = TRUE)) %>%
#     data.frame()
# 
#   cnt <- unique(data1[,c("COUNTRY","id_k")]) %>% arrange(as.character(COUNTRY))
#   data1 <- data1  %>% dplyr::select(-COUNTRY)
# 
#   for(c in 1:nrow(cnt)){
# 
#     data <- data1 %>%  filter(id_k == cnt$id_k[c])
#     # list of countries for multigroup model
#     gend <- as.vector(unique(data[!is.na(data$T_GENDER), "T_GENDER"]))
#     t <- data.frame(cbind(gend,"!",attr(ISC_lvR[["T_GENDER"]], "levels")))
#     tc <- textConnection("str", "w")
#     sink(tc)   # divert output to tc connection
#     print(t)  # print in str string instead of console
#     sink()     # set the output back to console
#     close(tc)
#     str <- substr(str,floor(length(str)/10)+3,nchar(str[1])) # we get rid of the row numbers that come with print
#     str <- paste0(str[-1],collapse="\n")        # we build a proper unique string with your pipes and new lines
#     #cat(str)
# 
#     lapply(2:3, function(k) { #input file for different number of classes
# 
#       fileConn <- file(paste0("Mplus models/ByCountry/MGGndr_",cnt$COUNTRY[c],"_C",j,"cl",sprintf("%d", k),".inp"))
#       writeLines(c(
#         paste0("TITLE: ", cnt$COUNTRY[c], " - MG Gender LCA C", j," with ", k ," classes;"),
#         "DATA: ",
#         paste0("FILE = DtaC",j,".dat;"),
#         "",
#         "VARIABLE: ",
#         paste0("NAMES = ", paste(colnames(data), collapse = "\n"),";"),
#         "IDVARIABLE = IDSTUD;",
#         paste0("USEVARIABLES = ", paste(colnames(data)[grepl('^GND', colnames(data))], collapse = "\n"),";"),
#         paste0("USEOBSERVATIONS ARE id_k EQ ", cnt$id_k[c], ";"),
#         paste0("CATEGORICAL = ", paste(colnames(data)[grepl('^GND', colnames(data))], collapse = "\n"),";"),
#         "MISSING = .;",
#         paste0("CLASSES = g(",length(gend),") ",sprintf("c(%d);", k)),
#         paste0("KNOWNCLASS = g(T_GENDER ="),
#         paste(str, collapse = "\n"),
#         ");",
#         "WEIGHT = ws;",
#         "STRATIFICATION = id_s;",
#         "CLUSTER = id_j;",
#         " ",
#         "ANALYSIS:",
#         "TYPE = COMPLEX MIXTURE;",
#         "PROCESSORS = 4;",
#         "STARTS = 100 50;",
#         "STITERATIONS = 5;",
#         "STSEED = 288;",
#         "",
#         "MODEL:",
#         "%OVERALL%",
#         "c ON g;",
#         " ",
#         "OUTPUT: ",
#         "TECH10",#for bivariate model fit information
#         #"TECH11",
#         #"SAMP",
#         "TECH14;", #Time consuming tests
#         #"CINTERVAL",
#         "SVALUES",
#         ";",
#         "",
#         "PLOT:",
#         "TYPE = PLOT3;",
#         "series = GND1-GND6(*);",
#         "",
#         "SAVEDATA:",
#         paste0("FILE = Prob_",cnt$COUNTRY[c],"_MGGndr_C", j,"cl", k,".dat;"),
#         "SAVE = CPROBABILITIES;"
# 
#       ), fileConn)
#       close(fileConn)
#     })
#   }
# }
# 
# runModels(target = "Mplus models/ByCountry", replaceOutfile = "never") #modifiedDate
# 
# ByCountry_MGGenderC1 <- readModels(target = "Mplus models/ByCountry", recursive = TRUE, filefilter = "MGGndr_[A-Z]{3}_C1cl*")
# ByCountry_MGGenderC2 <- readModels(target = "Mplus models/ByCountry", recursive = TRUE, filefilter = "MGGndr_[A-Z]{3}_C2cl*")
# ByCountry_MGGenderC3 <- readModels(target = "Mplus models/ByCountry", recursive = TRUE, filefilter = "MGGndr_[A-Z]{3}_C3cl*")
# ByCountry_MGGender<- list(C1 = ByCountry_MGGenderC1, C2 = ByCountry_MGGenderC2, C3 = ByCountry_MGGenderC3)
# #rm(list = list(ByCountry_MGGenderC1, ByCountry_MGGenderC2, ByCountry_MGGenderC3))
# #-----
# 
# ByCountry_MGGndr2C1 <- readModels(target = "Mplus models/ByCountry", recursive = TRUE, filefilter = "MGGndr_[A-Z]{3}_C1cl2")
# ByCountry_MGGndr2C2 <- readModels(target = "Mplus models/ByCountry", recursive = TRUE, filefilter = "MGGndr_[A-Z]{3}_C2cl2")
# ByCountry_MGGndr2C3 <- readModels(target = "Mplus models/ByCountry", recursive = TRUE, filefilter = "MGGndr_[A-Z]{3}_C3cl2")
# ByCountry_MGGndr2<- list(C1 = ByCountry_MGGndr2C1, C2 = ByCountry_MGGndr2C2, C3 = ByCountry_MGGndr2C3)
# 
# ByCountry_MGGndr3C1 <- readModels(target = "Mplus models/ByCountry", recursive = TRUE, filefilter = "MGGndr_[A-Z]{3}_C1cl3")
# ByCountry_MGGndr3C2 <- readModels(target = "Mplus models/ByCountry", recursive = TRUE, filefilter = "MGGndr_[A-Z]{3}_C2cl3")
# ByCountry_MGGndr3C3 <- readModels(target = "Mplus models/ByCountry", recursive = TRUE, filefilter = "MGGndr_[A-Z]{3}_C3cl3")
# ByCountry_MGGndr3<- list(C1 = ByCountry_MGGndr3C1, C2 = ByCountry_MGGndr3C2, C3 = ByCountry_MGGndr3C3)
# #ByCountry_MGGndr3[[3]][21] <- NULL
# save(lca,
#      MGGndr, 
#      MGCntry,
#      MGCntryGndr,
#      ByCountry_lca2, 
#      ByCountry_lca3, 
#      ByCountry_MGGndr2,  
#      ByCountry_MGGndr3, 
#      file = "MplusModels.RData")
# 
# 
# #----------------Multigroup COUNTRY training prob by CYCLE--------------------------------------
# 
# for (j in 1:3) { #input file for each CYCLE 1:3
#   prob <- data.frame()
#   cnt <- unique(ds_lc0[ds_lc0$CYCLE == paste0("C",j), c("COUNTRY","id_k")])
#   for(i in 1:nrow(cnt)) {
#     probi<-read.table(paste0("Mplus models/ByCountry/Prob_",cnt$COUNTRY[i],"_lca_C",j,"cl3.dat"), 
#                       header = FALSE, na.strings = "*",  
#                       col.names =  c("GND1", "GND2", "GND5", "GND3", "GND4", "GND6", "CPROB1", "CPROB2", "CPROB3", "C", 
#                                      "ws", "IDSTUD", "id_s", "id_j")) 
#     probi<-cbind(id_k=cnt$id_k[i], probi)
#     prob <- rbind(prob,probi)
#   }
#   
#   
#   data <- ds_lc0 %>%  filter(CYCLE == paste0("C",j)) %>%
#     dplyr::select(all_of(Id), all_of(sampleID), all_of(ScalesM2), -COUNTRY, -CYCLE, T_GENDER, cntgnd) %>%
#     mutate_if(is.factor, ~ as.numeric(.x)) %>%
#     rename_with(~ gsub("bT_GNDREQ", "GND", .x, fixed = TRUE)) %>%
#     data.frame()
#   
#   dataPriors <- right_join(data, prob[,c("id_k","IDSTUD", "CPROB1", "CPROB2", "CPROB3")], by = c("id_k","IDSTUD"))
#   
#   prepareMplusData(df = dataPriors,
#                    filename = paste0("Mplus models/Country Multigroup/Priors/DtaPriorsC",j,".dat"), interactive =FALSE)
#   
#   # list of countries for multigroup model
#   t <- cbind(unique(data[, "id_k"]),"!",unique(ds_lc0[ds_lc0$CYCLE == paste0("C",j), "COUNTRY"]), g = rownames(unique(ds_lc0[ds_lc0$CYCLE == paste0("C",j), "COUNTRY"])))
#   tc <- textConnection("str", "w")
#   sink(tc)   # divert output to tc connection
#   print(t)  # print in str string instead of console
#   sink()     # set the output back to console
#   close(tc)
#   str <- substr(str,floor(length(str)/10)+3,nchar(str[1])) # we get rid of the row numbers that come with print
#   str <- paste0(str[-1],collapse="\n")        # we build a proper unique string with your pipes and new lines
#   
#   cas <- data.frame(NULL)
#   lapply(3:3, function(k) { #input file for different number of classes
#     for(g in 1:length(unique(data$id_k))){
#       cast<- data.frame(outer(paste0("%g#",g), paste0(".c#",1:k,"%"), paste, sep=""))
#       for (c in 1:k) {
#         cas[g,c] <- cast[c]
#       }
#     }
#     casc <- textConnection("castx", "w")
#     sink(casc)   # divert output to tc connection
#     print(cas)  # print in str string instead of console
#     sink()     # set the output back to console
#     close(casc)
#     castx <- substr(castx,floor(length(castx)/10)+3,nchar(castx[1])) # we get rid of the row numbers that come with print
#     castx <- str_replace_all(paste0(castx[-1],collapse="\n")," ","\n")
#     fileConn <- file(paste0("Mplus models/Country Multigroup/Priors/MGCnty_C",j,"cl",sprintf("%d", k),".inp"))
#     writeLines(c(
#       paste0("TITLE: Priors MG Country LCA C", j," with ", k ," classes;"),
#       "DATA: ",
#       paste0("FILE = DtaPriorsC",j,".dat;"),
#       "",
#       "VARIABLE: ",
#       paste0("NAMES = ", paste(colnames(dataPriors), collapse = "\n"),";"),
#       "IDVARIABLE = IDSTUD;",
#       paste0("USEVARIABLES = ", paste(colnames(dataPriors)[grepl('^GND|CPROB', colnames(dataPriors))], collapse = "\n"),";"),
#       paste0("CATEGORICAL = ", paste(colnames(dataPriors)[grepl('^GND', colnames(dataPriors))], collapse = "\n"),";"),
#       "MISSING = .;",
#       paste0("CLASSES = ",sprintf("c(%d);", k)),
#       "TRAINING = CPROB1 CPROB2 CPROB3 (PROBABILITIES);",
#       "WEIGHT = ws;",
#       "STRATIFICATION = id_s;",
#       "CLUSTER = id_j;",
#       " ",
#       "ANALYSIS:",
#       "TYPE = COMPLEX MIXTURE;",
#       "PROCESSORS = 4;",
#       "STARTS = 100 50;",
#       "STITERATIONS = 5;",
#       "STSEED = 288;",
#       "",
#       "OUTPUT: ",
#       "TECH10",#for bivariate model fit information
#       #"TECH11",
#       #"SAMP",
#       #"TECH14;", Time consuming tests
#       #"CINTERVAL",
#       "SVALUES",
#       ";",
#       "",
#       "SAVEDATA:",
#       paste0("FILE = Prob_MGCntry_C", j,"cl", k,"Priors.dat;"),
#       "SAVE = CPROBABILITIES;"
#       
#     ), fileConn)
#     close(fileConn)
#   })
# }
# 
