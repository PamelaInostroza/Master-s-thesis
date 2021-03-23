library(MplusAutomation)
library(gridExtra)
detach(package:plyr)

ISC_lvRlca <- ISC_lvR %>% 
  dplyr::select(all_of(Id), all_of(sampleID), all_of(ScalesM2), all_of(Man_cate), all_of(Man_cont))  %>% 
  mutate(cntgnd = factor(ifelse(!is.na(T_GENDER), paste0(COUNTRY,T_GENDER), NA)))

load("MplusModels.RData")


classes2M2 <- c("Engaged with \ngender equality",
                "Gender differentiation")
classes3M2 <- c("Engaged with \ngender equality",
                "Gender differentiation",
                "Not engaged with \ngender equality")


#--Model fit
cat('\n')
cat('\n')
cat('## Models with 2 and 3 classes  \n')
cat('\n')
cat('\n')
Graph_modelfit("lca", mg = FALSE)
cat('\n')
cat('\n')

# compareModels(lca$lca_C1cl2.out,
#               lca$lca_C1cl3.out, diffTest = TRUE)
# compareModels(lca$lca_C2cl2.out,
#               lca$lca_C2cl3.out, diffTest = TRUE)
# compareModels(lca$lca_C3cl2.out,
#               lca$lca_C3cl3.out, diffTest = TRUE)

# ------------Results with 2 classes by cycle ----
cat('\n')
cat('\n')
cat('### Results with 2-classes  \n')
cat('\n')
cat('\n')
warn_lca2 <- list(C1 = lca$lca_C1cl2.out$warnings,
                  C2 = lca$lca_C2cl2.out$warnings,
                  C3 = lca$lca_C3cl2.out$warnings)
cat('\n')
cat('\n')
cat('\n')
cat('\n')
wr <- names(warn_lca2[grep("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED",warn_lca2)])
if (length(wr) == 0) print("ALL BEST LOGLIKELIHOOD VALUES WERE REPLICATED") else
  cat("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED for the following cases:  \n", wr)
cat('\n')
cat('\n')

#----C1
lca_C1cl2 <- lca$lca_C1cl2.out$parameters$probability.scale %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
lca_C1cl2$Class <- factor(lca_C1cl2$Class, 
                          levels = c("1", "2"), 
                          labels = classes2M2)

#----C2
lca_C2cl2 <- lca$lca_C2cl2.out$parameters$probability.scale %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
lca_C2cl2$Class <- factor(lca_C2cl2$Class, 
                          levels = c("1", "2"), 
                          labels = classes2M2)

#----C3
lca_C3cl2 <- lca$lca_C3cl2.out$parameters$probability.scale %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
lca_C3cl2$Class <- factor(lca_C3cl2$Class, 
                          levels = c("1", "2"), 
                          labels = classes2M2)

lc2 <- rbind(cbind(Cycle = "1999", lca_C1cl2),
             cbind(Cycle = "2009", lca_C2cl2),
             cbind(Cycle = "2016", lca_C3cl2))

graphclass(lc2, nclass = 2, orden = c(5,2,1,3,4,6) ,title = "LCA with 2 classes", mg = FALSE)
cat('\n')
cat('\n')

sizelca2 <- full_join(full_join(lca$lca_C1cl2.out$class_counts$modelEstimated %>% dplyr::select(-count) %>% 
                      rename_with(~ c("1999", "Class")[which(c("proportion", "class") == .x)], .cols = c("proportion", "class")) %>% 
                      mutate(Class =  factor(
                               case_when(
                                 Class == 1 ~ 1, 
                                 Class == 2 ~ 2), levels = 1:2, labels = classes2M2)), 
                    lca$lca_C2cl2.out$class_counts$modelEstimated %>% dplyr::select(-count) %>% 
                      rename_with(~ c("2009", "Class")[which(c("proportion", "class") == .x)], .cols = c("proportion", "class")) %>% 
                      mutate(Class =  factor(
                        case_when(
                          Class == 1 ~ 1, 
                          Class == 2 ~ 2), levels = 1:2, labels = classes2M2)), by = "Class"),
          lca$lca_C3cl2.out$class_counts$modelEstimated %>% dplyr::select(-count) %>% 
            rename_with(~ c("2016", "Class")[which(c("proportion", "class") == .x)], .cols = c("proportion", "class")) %>% 
            mutate(Class = factor(
              case_when(
                Class == 1 ~ 1, 
                Class == 2 ~ 2), levels = 1:2, labels = classes2M2)), by = "Class")


sizelca2 <- sizelca2 %>% reshape2::melt(id.vars = c("Class"), variable.name = "Cycle") %>% 
  arrange(Cycle) %>% 
  group_by(Cycle) %>%
  mutate(countT= sum(value, na.rm = TRUE)) %>%
  group_by(Class) %>%
  mutate(per=value/countT) %>% dplyr::select(Cycle, Class, per) 

HighProb(lc2, orden = c(5,2,1,3,4,6), title = "Response categories probabilities for 2 classes by Cycle")
cat('\n')
cat('\n')

sizelca2 %>% 
  reshape2::dcast(Cycle ~ Class) %>% 
  kbl(caption = "Size two classes", booktabs = TRUE, longtable = TRUE, row.names = FALSE, digits = 3) %>%
  kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"), 
                position = "left", font_size = 11, full_width = FALSE) %>% 
  column_spec(2:3, width = "8em") %>%  
  print()
cat('\n')
cat('\n')
# ----------

# ------------Results with 3 classes by cycle ----
cat('\n')
cat('\n')
cat('### Results with 3-classes  \n')
cat('\n')
cat('\n')

warn_lca3 <- list(C1 = lca$lca_C1cl3.out$warnings,
                  C2 = lca$lca_C2cl3.out$warnings,
                  C3 = lca$lca_C3cl3.out$warnings)
cat('\n')
cat('\n')
wr <- names(warn_lca3[grep("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED",warn_lca3)])
if (length(wr) == 0) print("ALL BEST LOGLIKELIHOOD VALUES WERE REPLICATED") else
  cat("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED for the following cases:  \n", wr)
cat('\n')
cat('\n')

#----C1
lca_C1cl3 <- lca$lca_C1cl3.out$parameters$probability.scale %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
lca_C1cl3$Class <- factor(lca_C1cl3$Class, 
                          levels = c("1", "2", "3"), 
                          labels = classes3M2)

#----C2
lca_C2cl3 <- lca$lca_C2cl3.out$parameters$probability.scale %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
lca_C2cl3$Class <- factor(lca_C2cl3$Class, 
                          levels = c("3", "2", "1"), 
                          labels = classes3M2)


#----C3
lca_C3cl3 <- lca$lca_C3cl3.out$parameters$probability.scale %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) 
lca_C3cl3$Class <- factor(lca_C3cl3$Class, 
                          levels = c("3", "1", "2"), 
                          labels = classes3M2)

lc3 <- rbind(cbind(Cycle = "1999", lca_C1cl3),
             cbind(Cycle = "2009", lca_C2cl3),
             cbind(Cycle = "2016", lca_C3cl3))

graphclass(lc3, nclass = 3, orden = c(5,2,1,3,4,6) ,title = "LCA with 3 classes", mg = FALSE)
cat('\n')
cat('\n')

lci3 <- VarClass(lc3)
lci3 %>% group_by(Cycle, Class, param) %>% filter(category == 2) %>% select(Cycle, param, Class, value) %>% 
  mutate(value = cell_spec(value, color = ifelse(value > 0.8, "blue", "red"))) %>% 
  reshape2::dcast(param  + Cycle ~ Class) %>%
  kbl(caption = "Probabilities to Agree each item by Class", booktabs = TRUE, longtable = TRUE, 
      align = "llrrr", row.names = FALSE, digits = 3, escape = FALSE) %>%
  kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"), 
                position = "left", font_size = 11, full_width = FALSE) %>% 
  column_spec(1, width = "14em") %>%  
  column_spec(3:5, width = "8em") %>%  
  collapse_rows(1, valign = "top") %>% 
  print()
cat('\n')
cat('\n')

sizelca3 <- full_join(full_join(lca$lca_C1cl3.out$class_counts$modelEstimated %>% dplyr::select(-count) %>% 
                      rename_with(~ c("1999", "Class")[which(c("proportion", "class") == .x)], .cols = c("proportion", "class")) %>% 
                      mutate(Class = factor(
                               case_when(
                                 Class == 1 ~ 1, 
                                 Class == 2 ~ 2,
                                 Class == 3 ~ 3), levels = 1:3, labels = classes3M2)), 
                    lca$lca_C2cl3.out$class_counts$modelEstimated %>% dplyr::select(-count) %>% 
                      rename_with(~ c("2009", "Class")[which(c("proportion", "class") == .x)], .cols = c("proportion", "class")) %>% 
                      mutate(Class =  factor(
                               case_when(
                                 Class == 1 ~ 3, 
                                 Class == 2 ~ 2,
                                 Class == 3 ~ 1), levels = 1:3, labels = classes3M2)), by = "Class"),
          lca$lca_C3cl3.out$class_counts$modelEstimated %>% dplyr::select(-count) %>% 
            rename_with(~ c("2016", "Class")[which(c("proportion", "class") == .x)], .cols = c("proportion", "class")) %>% 
            mutate(Class =  factor(
                     case_when(
                       Class == 1 ~ 2, 
                       Class == 2 ~ 3,
                       Class == 3 ~ 1), levels = 1:3, labels = classes3M2)), by = "Class")

sizelca3 <- sizelca3 %>% reshape2::melt(id.vars = c("Class"), variable.name = "Cycle") %>% 
  arrange(Cycle) %>% 
  group_by(Cycle) %>%
  mutate(countT= sum(value, na.rm = TRUE)) %>%
  group_by(Class) %>%
  mutate(per=value/countT) %>% dplyr::select(Cycle, Class, per) 


HighProb(lc3, orden = c(5,2,1,3,4,6), title = "Response category probabilities for three classes by Cycle")
cat('\n')
cat('\n')

sizelca3 %>% 
  reshape2::dcast(Cycle ~ Class) %>% 
  kbl(caption = "Size three classes", booktabs = TRUE, longtable = TRUE, row.names = FALSE, digits = 3) %>%
  kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"), 
                position = "left", font_size = 11, full_width = FALSE) %>% 
  column_spec(2:4, width = "8em") %>%  
  print()
cat('\n')
cat('\n')
# ------

cat('\\newpage')
cat('\n')
cat('\n')
# ------------Results with 3C MG Gndr classes by cycle ----
cat('\n')
cat('\n')
cat('## Multigroup analysis for Gender  \n')
cat('\n')
cat('\n')
Graph_modelfitMG("MGGndr", title = "Gender Multigroup (2 groups), 2 and 3 classes")

cat('\n')
cat('\n')
cat('### Results with 3-classes  \n')
cat('\n')
cat('\n')

# ------ MG Gender _1CHet ---- 
cat('#### Complete Heterogeneity across gender \n')
cat('\n')
cat('\n')
warn_MGGndr <- list(C1 = MGGndr$MGGndr_C1cl3.out$warnings,
                     C2 = MGGndr$MGGndr_C2cl3.out$warnings,
                     C3 = MGGndr$MGGndr_C3cl3.out$warnings)
cat('\n')
cat('\n')
wr <- names(warn_MGGndr[grep("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED",warn_MGGndr)])
if (length(wr) == 0) print("ALL BEST LOGLIKELIHOOD VALUES WERE REPLICATED") else
  cat("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED for the following cases:  \n", wr)
cat('\n')
cat('\n')
#----C1
MGGndr_C1cl3 <- MGGndr$MGGndr_C1cl3_1CHet.out$parameters$probability.scale %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>% 
  mutate(Group = factor(sub('\\..*', '', Class), 
                        levels = c("1", "2"), labels = c("Boy", "Girl")), 
         Class1 = sub('*.\\.', '', Class),
         ClassN = factor(
           case_when(
           Class == "1.1" ~ 2,
           Class == "1.2" ~ 3,
           Class == "1.3" ~ 1,
           Class == "2.1" ~ 2,
           Class == "2.2" ~ 1,
           Class == "2.3" ~ 3),
           levels = c("1", "2", "3"), labels = classes3M2))
#----C2
MGGndr_C2cl3 <- MGGndr$MGGndr_C2cl3_1CHet.out$parameters$probability.scale %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x))%>% 
  mutate(Group = factor(sub('\\..*', '', Class), 
                        levels = c("1", "2"), labels = c("Boy", "Girl")), 
         Class1 = sub('*.\\.', '', Class),
         ClassN = factor(
           case_when(
           Class == "1.1" ~ 3,
           Class == "1.2" ~ 2,
           Class == "1.3" ~ 1,
           Class == "2.1" ~ 2,
           Class == "2.2" ~ 1,
           Class == "2.3" ~ 3),
           levels = c("1", "2", "3"), labels = classes3M2))
#----C3
MGGndr_C3cl3 <- MGGndr$MGGndr_C3cl3_1CHet.out$parameters$probability.scale %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>% 
  mutate(Group = factor(sub('\\..*', '', Class), 
                        levels = c("1", "2"), labels = c("Boy", "Girl")),  
         Class1 = sub('*.\\.', '', Class),
         ClassN = factor(case_when(
           Class == "1.1" ~ 1,
           Class == "1.2" ~ 3,
           Class == "1.3" ~ 2,
           Class == "2.1" ~ 2,
           Class == "2.2" ~ 3,
           Class == "2.3" ~ 1),
         levels = c("1", "2", "3"), labels = classes3M2))

MGGndr3 <- rbind(cbind(Cycle = "1999", MGGndr_C1cl3),
                 cbind(Cycle = "2009", MGGndr_C2cl3),
                 cbind(Cycle = "2016", MGGndr_C3cl3))
#---
graphclass(MGGndr3, nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Gender Complete Heterogeneity")
cat('\n')
cat('\n')

lcign <- VarClass(MGGndr3)
lcign %>% group_by(Cycle, ClassN, Group, param) %>% filter(category == 2) %>% 
  select(Cycle, param, ClassN, Group, value) %>% 
  mutate(value = cell_spec(value, color = ifelse(value > 0.8, "blue", "red"))) %>% 
  reshape2::dcast(param  + Cycle ~ Group + ClassN) %>%
  kbl(caption = "Probabilities to Agree each item by Class and Gender Complete Heterogeneity", booktabs = TRUE, longtable = TRUE, 
      align = "llrrrrrr", row.names = FALSE, digits = 3, col.names = c("Item", "Year", classes3M2, classes3M2), escape = FALSE) %>%
  kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"), 
                position = "left", font_size = 10, full_width = FALSE) %>% 
  column_spec(1, width = "14em") %>%  
  column_spec(2, width = "2em") %>%  
  column_spec(3:8, width = "4em") %>%  
  collapse_rows(1, valign = "top") %>%
  add_header_above(c(" "=1," "=1, "Boy" = 3, "Girl" = 3)) %>% 
  print()
cat('\n')
cat('\n')

#Extracting patterns
sizeMGGndr3 <- full_join(
  full_join(MGGndr$MGGndr_C1cl3_1CHet.out$class_counts$modelEstimated.patterns %>% 
              rename_with(~ c("1999", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>% 
              mutate(Class = paste(Group,C,sep="."),
                     Class1 = sub('*.\\.', '', Class),
                     ClassN = factor(case_when(
                       Class == "1.1" ~ 2,
                       Class == "1.2" ~ 3,
                       Class == "1.3" ~ 1,
                       Class == "2.1" ~ 2,
                       Class == "2.2" ~ 1,
                       Class == "2.3" ~ 3),
                levels = 1:3, labels = classes3M2)) %>% dplyr::select(-proportion, -C, -Class) ,
            MGGndr$MGGndr_C2cl3_1CHet.out$class_counts$modelEstimated.patterns %>% 
              rename_with(~ c("2009", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>% 
              mutate(Class = paste(Group,C,sep="."),
                     Class1 = sub('*.\\.', '', Class),
                     ClassN = factor(case_when(
                       Class == "1.1" ~ 3,
                       Class == "1.2" ~ 2,
                       Class == "1.3" ~ 1,
                       Class == "2.1" ~ 2,
                       Class == "2.2" ~ 1,
                       Class == "2.3" ~ 3),
            levels = 1:3, labels = classes3M2)) %>% dplyr::select(-proportion, -C, -Class), by = c("Group","ClassN")),
  MGGndr$MGGndr_C3cl3_1CHet.out$class_counts$modelEstimated.patterns %>%
    rename_with(~ c("2016", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>% 
    mutate(Class = paste(Group,C,sep="."),
           Class1 = sub('*.\\.', '', Class),
           ClassN = factor(case_when(
             Class == "1.1" ~ 1,
             Class == "1.2" ~ 3,
             Class == "1.3" ~ 2,
             Class == "2.1" ~ 2,
             Class == "2.2" ~ 3,
             Class == "2.3" ~ 1),
             levels = 1:3, labels = classes3M2)) %>% dplyr::select(-proportion, -C, -Class) , by = c("Group","ClassN")) %>%
  mutate(Group = factor(Group, levels = c("1", "2"), labels = c("Boy", "Girl"))) %>% select(Group, ClassN, `1999`, `2009`, `2016`)

sizeMGGndr3 <- sizeMGGndr3 %>% reshape2::melt(id.vars = c("Group","ClassN"), variable.name = "Cycle") %>% 
  arrange(Group, Cycle) %>% 
  group_by(Group, Cycle) %>%
  mutate(countT= sum(value, na.rm = TRUE)) %>%
  group_by(ClassN) %>%
  mutate(per=value/countT) %>% dplyr::select(Group, Cycle, ClassN, per) 

HighProbMG(MGGndr3, sizeMGGndr3, orden = c(5,2,1,3,4,6), n = 1:2, title = "Probabilities of Agree and size for each class in Gender Multigroup analysis\n Complete Heterogeneity by Cycle")

sizeMGGndr3 %>% 
  reshape2::dcast(Cycle ~ Group + ClassN) %>% 
  kbl(caption = "Size three classes MG Gender Complete Heterogeneity", booktabs = TRUE, longtable = TRUE, row.names = FALSE, 
      digits = 3, col.names = c("Year", classes3M2, classes3M2)) %>%
  kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"),
                position = "left", font_size = 11, full_width = FALSE) %>%     
  column_spec(2:7, width = "4em") %>% 
  add_header_above(c(" " = 1, "Boy" = 3, "Girl" = 3)) %>% 
  print()
cat('\n')
cat('\n')
# ------ MG Gender PH1 ----
cat('#### Partial Homogeneity - one class equal across gender \n')
cat('\n')
cat('\n')
warn_MGGndr1PH1 <- list(C1 = MGGndr$MGGndr_C1cl31PH1.out$warnings,
                        C2 = MGGndr$MGGndr_C2cl31PH1.out$warnings,
                        C3 = MGGndr$MGGndr_C3cl31PH1.out$warnings)
cat('\n')
cat('\n')
wr <- names(warn_MGGndr1PH1[grep("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED",warn_MGGndr1PH1)])
if (length(wr) == 0) print("ALL BEST LOGLIKELIHOOD VALUES WERE REPLICATED") else
  cat("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED for the following cases:  \n", wr)
cat('\n')
cat('\n')
#----C1
MGGndr_C1cl3 <- MGGndr$MGGndr_C1cl3PH1.out$parameters$probability.scale %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>% 
  mutate(Group = factor(sub('\\..*', '', Class), 
                        levels = c("1", "2"), labels = c("Boy", "Girl")), 
         Class1 = sub('*.\\.', '', Class),
         ClassN = factor(
           case_when(
             Class1 == "1" ~ 2,
             Class1 == "2" ~ 3,
             Class1 == "3" ~ 1),
           levels = c("1", "2", "3"), labels = classes3M2))
#----C2
MGGndr_C2cl3 <- MGGndr$MGGndr_C2cl3PH1.out$parameters$probability.scale %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x))%>% 
  mutate(Group = factor(sub('\\..*', '', Class), 
                        levels = c("1", "2"), labels = c("Boy", "Girl")), 
         Class1 = sub('*.\\.', '', Class),
         ClassN = factor(
           case_when(
             Class1 == "1" ~ 2,
             Class1 == "2" ~ 1,
             Class1 == "3" ~ 3),
           levels = c("1", "2", "3"), labels = classes3M2))
#----C3
MGGndr_C3cl3 <- MGGndr$MGGndr_C3cl3PH1.out$parameters$probability.scale %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>% 
  mutate(Group = factor(sub('\\..*', '', Class), 
                        levels = c("1", "2"), labels = c("Boy", "Girl")),  
         Class1 = sub('*.\\.', '', Class),
         ClassN = factor(case_when(
           Class1 == "1" ~ 2,
           Class1 == "2" ~ 1,
           Class1 == "3" ~ 3),
           levels = c("1", "2", "3"), labels = classes3M2))

MGGndr3PH1 <- rbind(cbind(Cycle = "1999", MGGndr_C1cl3),
                 cbind(Cycle = "2009", MGGndr_C2cl3),
                 cbind(Cycle = "2016", MGGndr_C3cl3))
#---
graphclass(MGGndr3PH1, nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Gender Partial Homogeneity - 1 class equal across gender")
cat('\n')
cat('\n')

lcign <- VarClass(MGGndr3PH1)
lcign %>% group_by(Cycle, ClassN, Group, param) %>% filter(category == 2) %>% 
  select(Cycle, param, ClassN, Group, value) %>% 
  mutate(value = cell_spec(value, color = ifelse(value > 0.8, "blue", "red"))) %>% 
  reshape2::dcast(param  + Cycle ~ Group + ClassN) %>%
  kbl(caption = "Probabilities to Agree each item by Class and Gender - Partial homogeneity 1 class", booktabs = TRUE, longtable = TRUE, 
      align = "llrrrrrr", row.names = FALSE, digits = 3, col.names = c("Item", "Year", classes3M2, classes3M2), escape = FALSE) %>%
  kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"), 
                position = "left", font_size = 10, full_width = FALSE) %>% 
  column_spec(1, width = "14em") %>%  
  column_spec(2, width = "2em") %>%  
  column_spec(3:8, width = "4em") %>%  
  collapse_rows(1, valign = "top") %>%
  add_header_above(c(" "=1," "=1, "Boy" = 3, "Girl" = 3)) %>% 
  print()
cat('\n')
cat('\n')

#Extracting patterns
sizeMGGndr3PH1 <- full_join(
  full_join(MGGndr$MGGndr_C1cl3PH1.out$class_counts$modelEstimated.patterns %>% 
              rename_with(~ c("1999", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>% 
              mutate(Class = paste(Group,C,sep="."),
                     Class1 = sub('*.\\.', '', Class),
                     ClassN = factor(case_when(
                       Class1 == "1" ~ 2,
                       Class1 == "2" ~ 3,
                       Class1 == "3" ~ 1),
                       levels = 1:3, labels = classes3M2)) %>% dplyr::select(-proportion, -C, -Class) ,
            MGGndr$MGGndr_C2cl3PH1.out$class_counts$modelEstimated.patterns %>% 
              rename_with(~ c("2009", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>% 
              mutate(Class = paste(Group,C,sep="."),
                     Class1 = sub('*.\\.', '', Class),
                     ClassN = factor(case_when(
                       Class1 == "1" ~ 2,
                       Class1 == "2" ~ 1,
                       Class1 == "3" ~ 3),
                       levels = 1:3, labels = classes3M2)) %>% dplyr::select(-proportion, -C, -Class), by = c("Group","ClassN")),
  MGGndr$MGGndr_C3cl3PH1.out$class_counts$modelEstimated.patterns %>%
    rename_with(~ c("2016", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>% 
    mutate(Class = paste(Group,C,sep="."),
           Class1 = sub('*.\\.', '', Class),
           ClassN = factor(case_when(
             Class1 == "1" ~ 2,
             Class1 == "2" ~ 1,
             Class1 == "3" ~ 3),
             levels = 1:3, labels = classes3M2)) %>% dplyr::select(-proportion, -C, -Class) , by = c("Group","ClassN")) %>%
  mutate(Group = factor(Group, levels = c("1", "2"), labels = c("Boy", "Girl"))) %>% select(Group, ClassN, `1999`, `2009`, `2016`)

sizeMGGndr3PH1 <- sizeMGGndr3PH1 %>% reshape2::melt(id.vars = c("Group","ClassN"), variable.name = "Cycle") %>% 
  arrange(Group, Cycle) %>% 
  group_by(Group, Cycle) %>%
  mutate(countT= sum(value, na.rm = TRUE)) %>%
  group_by(ClassN) %>%
  mutate(per=value/countT) %>% dplyr::select(Group, Cycle, ClassN, per) 

HighProbMG(MGGndr3PH1, sizeMGGndr3PH1, orden = c(5,2,1,3,4,6), n = 1:2, title = "Probabilities of Agree and size for each class in Gender Multigroup analysis\n Partial Homogeneity (1 class fixed) by Cycle")

sizeMGGndr3PH1 %>% 
  reshape2::dcast(Cycle ~ Group + ClassN) %>% 
  kbl(caption = "Size three classes MG Gender Partial Homogeneity - one class equal across gender", booktabs = TRUE, longtable = TRUE, row.names = FALSE, 
      digits = 3, col.names = c("Year", classes3M2, classes3M2)) %>%
  kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"),
                position = "left", font_size = 11, full_width = FALSE) %>%     
  column_spec(2:7, width = "4em") %>% 
  add_header_above(c(" " = 1, "Boy" = 3, "Girl" = 3)) %>% 
  print()
cat('\n')
cat('\n')

# ------ MG Gender PH2 ----
cat('#### Partial Homogeneity - two classes equal across gender \n')
cat('\n')
cat('\n')
warn_MGGndr1PH2 <- list(C1 = MGGndr$MGGndr_C1cl31PH2.out$warnings,
                        C2 = MGGndr$MGGndr_C2cl31PH2.out$warnings,
                        C3 = MGGndr$MGGndr_C3cl31PH2.out$warnings)
cat('\n')
cat('\n')
wr <- names(warn_MGGndr1PH2[grep("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED",warn_MGGndr1PH2)])
if (length(wr) == 0) print("ALL BEST LOGLIKELIHOOD VALUES WERE REPLICATED") else
  cat("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED for the following cases:  \n", wr)
cat('\n')
cat('\n')
#----C1
MGGndrPH2_C1cl3 <- MGGndr$MGGndr_C1cl3PH2.out$parameters$probability.scale %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>% 
  mutate(Group = factor(sub('\\..*', '', Class), 
                        levels = c("1", "2"), labels = c("Boy", "Girl")), 
         Class1 = sub('*.\\.', '', Class),
         ClassN = factor(
           case_when(
             Class1 == "1" ~ 1,
             Class1 == "2" ~ 2,
             Class1 == "3" ~ 3),
           levels = c("1", "2", "3"), labels = classes3M2))
#----C2
MGGndrPH2_C2cl3 <- MGGndr$MGGndr_C2cl3PH2.out$parameters$probability.scale %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x))%>% 
  mutate(Group = factor(sub('\\..*', '', Class), 
                        levels = c("1", "2"), labels = c("Boy", "Girl")), 
         Class1 = sub('*.\\.', '', Class),
         ClassN = factor(
           case_when(
             Class1 == "1" ~ 2,
             Class1 == "2" ~ 3,
             Class1 == "3" ~ 1),
           levels = c("1", "2", "3"), labels = classes3M2))
#----C3
MGGndrPH2_C3cl3 <- MGGndr$MGGndr_C3cl3PH2.out$parameters$probability.scale %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>% 
  mutate(Group = factor(sub('\\..*', '', Class), 
                        levels = c("1", "2"), labels = c("Boy", "Girl")),  
         Class1 = sub('*.\\.', '', Class),
         ClassN = factor(case_when(
           Class1 == "1" ~ 2,
           Class1 == "2" ~ 3,
           Class1 == "3" ~ 1),
           levels = c("1", "2", "3"), labels = classes3M2))

MGGndr3PH2 <- rbind(cbind(Cycle = "1999", MGGndrPH2_C1cl3),
                     cbind(Cycle = "2009", MGGndrPH2_C2cl3),
                     cbind(Cycle = "2016", MGGndrPH2_C3cl3))
#---
graphclass(MGGndr3PH2, nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Gender Partial Homogeneity - 2 classes equal across gender")
cat('\n')
cat('\n')

lcign <- VarClass(MGGndr3PH2)
lcign %>% group_by(Cycle, ClassN, Group, param) %>% filter(category == 2) %>% 
  select(Cycle, param, ClassN, Group, value) %>% 
  mutate(value = cell_spec(value, color = ifelse(value > 0.8, "blue", "red"))) %>% 
  reshape2::dcast(param  + Cycle ~ Group + ClassN) %>%
  kbl(caption = "Probabilities to Agree each item by Class and Gender - Partial homogeneity 2 classes", booktabs = TRUE, longtable = TRUE, 
      align = "llrrrrrr", row.names = FALSE, digits = 3, col.names = c("Item", "Year", classes3M2, classes3M2), escape = FALSE) %>%
  kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"), 
                position = "left", font_size = 10, full_width = FALSE) %>% 
  column_spec(1, width = "14em") %>%  
  column_spec(2, width = "2em") %>%  
  column_spec(3:8, width = "4em") %>%  
  collapse_rows(1, valign = "top") %>%
  add_header_above(c(" "=1," "=1, "Boy" = 3, "Girl" = 3)) %>% 
  print()
cat('\n')
cat('\n')

#Extracting patterns
sizeMGGndr3PH2 <- full_join(
  full_join(MGGndr$MGGndr_C1cl3PH2.out$class_counts$modelEstimated.patterns %>% 
              rename_with(~ c("1999", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>% 
              mutate(Class = paste(Group,C,sep="."),
                     Class1 = sub('*.\\.', '', Class),
                     ClassN = factor(case_when(
                       Class1 == "1" ~ 1,
                       Class1 == "2" ~ 2,
                       Class1 == "3" ~ 3),
                       levels = 1:3, labels = classes3M2)) %>% dplyr::select(-proportion, -C, -Class) ,
            MGGndr$MGGndr_C2cl3PH2.out$class_counts$modelEstimated.patterns %>% 
              rename_with(~ c("2009", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>% 
              mutate(Class = paste(Group,C,sep="."),
                     Class1 = sub('*.\\.', '', Class),
                     ClassN = factor(case_when(
                       Class1 == "1" ~ 2,
                       Class1 == "2" ~ 3,
                       Class1 == "3" ~ 1),
                       levels = 1:3, labels = classes3M2)) %>% dplyr::select(-proportion, -C, -Class), by = c("Group","ClassN")),
  MGGndr$MGGndr_C3cl3PH2.out$class_counts$modelEstimated.patterns %>%
    rename_with(~ c("2016", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>% 
    mutate(Class = paste(Group,C,sep="."),
           Class1 = sub('*.\\.', '', Class),
           ClassN = factor(case_when(
             Class1 == "1" ~ 2,
             Class1 == "2" ~ 3,
             Class1 == "3" ~ 1),
             levels = 1:3, labels = classes3M2)) %>% dplyr::select(-proportion, -C, -Class) , by = c("Group","ClassN")) %>%
  mutate(Group = factor(Group, levels = c("1", "2"), labels = c("Boy", "Girl"))) %>% select(Group, ClassN, `1999`, `2009`, `2016`)

sizeMGGndr3PH2 <- sizeMGGndr3PH2 %>% reshape2::melt(id.vars = c("Group","ClassN"), variable.name = "Cycle") %>% 
  arrange(Group, Cycle) %>% 
  group_by(Group, Cycle) %>%
  mutate(countT= sum(value, na.rm = TRUE)) %>%
  group_by(ClassN) %>%
  mutate(per=value/countT) %>% dplyr::select(Group, Cycle, ClassN, per) 

HighProbMG(MGGndr3PH2, sizeMGGndr3PH2, orden = c(5,2,1,3,4,6), n = 1:2, title = "Probabilities of Agree and size for each class in Gender Multigroup analysis\n Partial Homogeneity (2 classes fixed) by Cycle")

sizeMGGndr3PH2 %>% 
  reshape2::dcast(Cycle ~ Group + ClassN) %>% 
  kbl(caption = "Size three classes MG Gender Partial Homogeneity - two classes equal across gender", booktabs = TRUE, longtable = TRUE, row.names = FALSE, 
      digits = 3, col.names = c("Year", classes3M2, classes3M2)) %>%
  kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"),
                position = "left", font_size = 11, full_width = FALSE) %>%     
  column_spec(2:7, width = "4em") %>% 
  add_header_above(c(" " = 1, "Boy" = 3, "Girl" = 3)) %>% 
  print()
cat('\n')
cat('\n')
# ------ MG Gender PH3 ----
cat('#### Complete Homogeneity - three classes equal across gender \n')
cat('\n')
cat('\n')
warn_MGGndr1PH3 <- list(C1 = MGGndr$MGGndr_C1cl31PH3.out$warnings,
                        C2 = MGGndr$MGGndr_C2cl31PH3.out$warnings,
                        C3 = MGGndr$MGGndr_C3cl31PH3.out$warnings)
cat('\n')
cat('\n')
wr <- names(warn_MGGndr1PH3[grep("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED",warn_MGGndr1PH3)])
if (length(wr) == 0) print("ALL BEST LOGLIKELIHOOD VALUES WERE REPLICATED") else
  cat("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED for the following cases:  \n", wr)
cat('\n')
cat('\n')
#----C1
MGGndrPH3_C1cl3 <- MGGndr$MGGndr_C1cl3PH3.out$parameters$probability.scale %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>% 
  mutate(Class = factor(case_when(
    Class == "C#1" ~ 2,
    Class == "C#2" ~ 1,
    Class == "C#3" ~ 3),
    levels = c(1, 2, 3), labels = classes3M2),
    ClassN = Class)
#----C2
MGGndrPH3_C2cl3 <- MGGndr$MGGndr_C2cl3PH3.out$parameters$probability.scale %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x))%>% 
  mutate(Class = factor(
           case_when(
             Class == "C#1" ~ 1,
             Class == "C#2" ~ 3,
             Class == "C#3" ~ 2),
           levels = c(1, 2, 3), labels = classes3M2),
         ClassN = Class)
#----C3
MGGndrPH3_C3cl3 <- MGGndr$MGGndr_C3cl3PH3.out$parameters$probability.scale %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>% 
  mutate(Class = factor(case_when(
           Class == "C#1" ~ 1,
           Class == "C#2" ~ 3,
           Class == "C#3" ~ 2),
           levels = c(1, 2, 3), labels = classes3M2),
         ClassN = Class)

MGGndr3PH3 <- rbind(cbind(Cycle = "1999", MGGndrPH3_C1cl3),
                    cbind(Cycle = "2009", MGGndrPH3_C2cl3),
                    cbind(Cycle = "2016", MGGndrPH3_C3cl3))
#---
graphclass(MGGndr3PH3, nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Gender Complete Homogeneity across gender", mg = FALSE)
cat('\n')
cat('\n')

lcign <- VarClass(MGGndr3PH3)
lcign %>% group_by(Cycle, ClassN, param) %>% filter(category == 2) %>% 
  select(Cycle, param, Class, value) %>% 
  mutate(value = cell_spec(value, color = ifelse(value > 0.8, "blue", "red"))) %>% 
  reshape2::dcast(param  + Cycle ~ Class) %>%
  kbl(caption = "Probabilities to Agree each item by Class and Gender  - Complete homogeneity", booktabs = TRUE, longtable = TRUE, 
      align = "llrrrrrr", row.names = FALSE, digits = 3, col.names = c("Item", "Year", classes3M2), escape = FALSE) %>%
  kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"), 
                position = "left", font_size = 10, full_width = FALSE) %>% 
  column_spec(1, width = "18em") %>%  
  column_spec(2, width = "2em") %>%  
  column_spec(3:5, width = "4em") %>%  
  collapse_rows(1, valign = "top") %>%
  print()
cat('\n')
cat('\n')

#Extracting patterns
sizeMGGndr3PH3 <- full_join(
  full_join(MGGndr$MGGndr_C1cl3PH3.out$class_counts$modelEstimated.patterns %>% 
              rename_with(~ c("1999", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>% 
              mutate(Class = paste(Group,C,sep="."),
                     Class1 = sub('*.\\.', '', Class),
                     ClassN = factor(case_when(
                       Class1 == "1" ~ 2,
                       Class1 == "2" ~ 1,
                       Class1 == "3" ~ 3),
                       levels = 1:3, labels = classes3M2)) %>% dplyr::select(-proportion, -C, -Class) ,
            MGGndr$MGGndr_C2cl3PH3.out$class_counts$modelEstimated.patterns %>% 
              rename_with(~ c("2009", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>% 
              mutate(Class = paste(Group,C,sep="."),
                     Class1 = sub('*.\\.', '', Class),
                     ClassN = factor(case_when(
                       Class1 == "1" ~ 1,
                       Class1 == "2" ~ 3,
                       Class1 == "3" ~ 2),
                       levels = 1:3, labels = classes3M2)) %>% dplyr::select(-proportion, -C, -Class), by = c("Group","ClassN")),
  MGGndr$MGGndr_C3cl3PH3.out$class_counts$modelEstimated.patterns %>%
    rename_with(~ c("2016", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>% 
    mutate(Class = paste(Group,C,sep="."),
           Class1 = sub('*.\\.', '', Class),
           ClassN = factor(case_when(
             Class1 == "1" ~ 1,
             Class1 == "2" ~ 3,
             Class1 == "3" ~ 2),
             levels = 1:3, labels = classes3M2)) %>% dplyr::select(-proportion, -C, -Class) , by = c("Group","ClassN")) %>%
  mutate(Group = factor(Group, levels = c("1", "2"), labels = c("Boy", "Girl"))) %>% select(Group, ClassN, `1999`, `2009`, `2016`)

sizeMGGndr3PH3 <- sizeMGGndr3PH3 %>% reshape2::melt(id.vars = c("Group","ClassN"), variable.name = "Cycle") %>% 
  arrange(Group, Cycle) %>% 
  group_by(Group, Cycle) %>%
  mutate(countT= sum(value, na.rm = TRUE)) %>%
  group_by(ClassN) %>%
  mutate(per=value/countT) %>% dplyr::select(Group, Cycle, ClassN, per) 

MGGndr3PH3 <- rbind(cbind(Group = "Boy", MGGndr3PH3), cbind(Group = "Girl", MGGndr3PH3))
HighProbMG(MGGndr3PH3, sizeMGGndr3PH3, orden = c(5,2,1,3,4,6), n = 1:2, title = "Probabilities of Agree and size for each class in Gender Multigroup analysis Complete Homogeneity by Cycle")

sizeMGGndr3PH3 %>% 
  reshape2::dcast(Cycle ~ Group + ClassN) %>% 
  kbl(caption = "Size three classes MG Gender Complete Homogeneity across gender", booktabs = TRUE, longtable = TRUE, row.names = FALSE, 
      digits = 3, col.names = c("Year", classes3M2, classes3M2)) %>%
  kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"),
                position = "left", font_size = 11, full_width = FALSE) %>%     
  column_spec(2:7, width = "4em") %>% 
  add_header_above(c(" " = 1, "Boy" = 3, "Girl" = 3)) %>% 
  print()
cat('\n')
cat('\n')
# ----------

cat('\\newpage')

# ------------Results with MGCntry 2-classes by cycle ----
cat('\n')
cat('\n')
cat('## Multigroup analysis for Country  \n')
cat('\n')
cat('\n')
Graph_modelfitMG("MGCntry", title = "Country Multigroup")

cat('\n')
cat('\n')
cat('### Results with 2-classes  \n')
cat('\n')
cat('\n')
# ------ 2C MG Country _1CHet ----
cat('#### Complete Heterogeneity across country \n')
cat('\n')
cat('\n')

warn_MGCntry <- list(C1 = MGCntry$MGCnty_C1cl2_1CHet.out$warnings,
                     C2 = MGCntry$MGCnty_C2cl2_1CHet.out$warnings,
                     C3 = MGCntry$MGCnty_C3cl2_1CHet.out$warnings)
cat('\n')
cat('\n')
wr <- names(warn_MGCntry[grep("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED",warn_MGCntry)])
if (length(wr) == 0) print("ALL BEST LOGLIKELIHOOD VALUES WERE REPLICATED") else
  cat("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED for the following cases:  \n", wr)
cat('\n')
cat('\n')


#----C1
cnt1 <- sort(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C1", c("COUNTRY")])$COUNTRY)
MGCnty_C1cl2 <- MGCntry$MGCnty_C1cl2_1CHet.out$parameters$probability.scale %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>% 
  mutate(Group = factor(sub('\\..*', '', Class), 
                        levels = 1:length(cnt1), labels = cnt1), 
         ClassN = factor(case_when(
           #-----
           Class == "1.1" ~ 2, #BGR
           Class == "1.2" ~ 1,
           Class == "2.1" ~ 1, #CHL
           Class == "2.2" ~ 2,
           Class == "3.1" ~ 2, #COL
           Class == "3.2" ~ 1,
           Class == "4.1" ~ 2, #DNK
           Class == "4.2" ~ 1,
           Class == "5.1" ~ 1, #EST
           Class == "5.2" ~ 2,
           Class == "6.1" ~ 2, #FIN
           Class == "6.2" ~ 1,
           Class == "7.1" ~ 1, #HKG
           Class == "7.2" ~ 2,
           Class == "8.1" ~ 1, #ITA
           Class == "8.2" ~ 2,
           Class == "9.1" ~ 1, #LTU
           Class == "9.2" ~ 2,
           Class == "10.1" ~ 1, #LVA
           Class == "10.2" ~ 2,
           Class == "11.1" ~ 1, #NOR
           Class == "11.2" ~ 2,
           Class == "12.1" ~ 2, #RUS
           Class == "12.2" ~ 1,
           Class == "13.1" ~ 1, #SVN
           Class == "13.2" ~ 2,
           Class == "14.1" ~ 1, #SWE
           Class == "14.2" ~ 2
           #-----
         ), levels = c("1", "2"), labels = classes2M2))
#----C2
cnt2 <- sort(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C2", c("COUNTRY")])$COUNTRY)
MGCnty_C2cl2 <- MGCntry$MGCnty_C2cl2_1CHet.out$parameters$probability.scale %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x))%>% 
  mutate(Group = factor(sub('\\..*', '', Class), 
                        levels = 1:length(cnt2), labels = cnt2), 
         ClassN = factor(case_when(
           #-----
           Class == "1.1" ~ 1, #BFL
           Class == "1.2" ~ 2,
           Class == "2.1" ~ 2, #BGR
           Class == "2.2" ~ 1,
           Class == "3.1" ~ 1, #CHL
           Class == "3.2" ~ 2,
           Class == "4.1" ~ 1, #COL
           Class == "4.2" ~ 2,
           Class == "5.1" ~ 1, #DNK \\\SPECIAL CLASS 3
           Class == "5.2" ~ 2,
           Class == "6.1" ~ 1, #DOM
           Class == "6.2" ~ 2,
           Class == "7.1" ~ 1, #EST
           Class == "7.2" ~ 2,
           Class == "8.1" ~ 1, #FIN
           Class == "8.2" ~ 2,
           Class == "9.1" ~ 1, #HKG
           Class == "9.2" ~ 2,
           Class == "10.1" ~ 2, #ITA
           Class == "10.2" ~ 1,
           Class == "11.1" ~ 2, #KOR
           Class == "11.2" ~ 1,
           Class == "12.1" ~ 1, #LTU
           Class == "12.2" ~ 2,
           Class == "13.1" ~ 1, #LVA
           Class == "13.2" ~ 2,
           Class == "14.1" ~ 1, #MEX
           Class == "14.2" ~ 2,
           Class == "15.1" ~ 2, #MLT
           Class == "15.2" ~ 1,
           Class == "16.1" ~ 2, #NLD
           Class == "16.2" ~ 1,
           Class == "17.1" ~ 1, #NOR
           Class == "17.2" ~ 2,
           Class == "18.1" ~ 1, #RUS
           Class == "18.2" ~ 2,
           Class == "19.1" ~ 1, #SVN
           Class == "19.2" ~ 2,
           Class == "20.1" ~ 1, #SWE
           Class == "20.2" ~ 2,
           Class == "21.1" ~ 2, #TWN
           Class == "21.2" ~ 1
           #-----
         ), levels = c("1", "2"), labels = classes2M2))
#----C3
cnt3 <- sort(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C3", c("COUNTRY")])$COUNTRY)
MGCnty_C3cl2 <- MGCntry$MGCnty_C3cl2_1CHet.out$parameters$probability.scale %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>% 
  mutate(Group = factor(sub('\\..*', '', Class), 
                        levels = 1:length(cnt3), labels = cnt3), 
         ClassN = factor(case_when(
           #-----
           Class == "1.1" ~ 1, #BFL
           Class == "1.2" ~ 2,
           Class == "2.1" ~ 2, #BGR
           Class == "2.2" ~ 1,
           Class == "3.1" ~ 2, #CHL
           Class == "3.2" ~ 1,
           Class == "4.1" ~ 2, #COL
           Class == "4.2" ~ 1,
           Class == "5.1" ~ 2, #DNK
           Class == "5.2" ~ 1,
           Class == "6.1" ~ 2, #DOM
           Class == "6.2" ~ 1,
           Class == "7.1" ~ 2, #EST
           Class == "7.2" ~ 1,
           Class == "8.1" ~ 1, #FIN
           Class == "8.2" ~ 2,
           Class == "9.1" ~ 1, #HKG
           Class == "9.2" ~ 2,
           Class == "10.1" ~ 2, #ITA
           Class == "10.2" ~ 1,
           Class == "11.1" ~ 1, #KOR
           Class == "11.2" ~ 2,
           Class == "12.1" ~ 1, #LTU
           Class == "12.2" ~ 2,
           Class == "13.1" ~ 1, #LVA
           Class == "13.2" ~ 2,
           Class == "14.1" ~ 1, #MEX
           Class == "14.2" ~ 2,
           Class == "15.1" ~ 2, #MLT
           Class == "15.2" ~ 1,
           Class == "16.1" ~ 2, #NLD
           Class == "16.2" ~ 1,
           Class == "17.1" ~ 2, #NOR
           Class == "17.2" ~ 1,
           Class == "18.1" ~ 1, #RUS
           Class == "18.2" ~ 2,
           Class == "19.1" ~ 1, #SVN
           Class == "19.2" ~ 2,
           Class == "20.1" ~ 1, #SWE
           Class == "20.2" ~ 2,
           Class == "21.1" ~ 2, #TWN
           Class == "21.2" ~ 1  
           #-----
         ), levels = c("1", "2"), labels = classes2M2))

MGCnty3 <- rbind(#cbind(Cycle = "1999", MGCnty_C1cl2),
                 cbind(Cycle = "2009", MGCnty_C2cl2),
                 cbind(Cycle = "2016", MGCnty_C3cl2))
#---
graphclass(MGCnty3[MGCnty3$Group %in% cnt3[c(1,6)],], nclass = 2, orden = c(5,2,1,3,4,6) ,title = "MG Country Complete Heterogeneity")
graphclass(MGCnty3[MGCnty3$Group %in% cnt3[2:3],], nclass = 2, orden = c(5,2,1,3,4,6) ,title = "MG Country Complete Heterogeneity")
graphclass(MGCnty3[MGCnty3$Group %in% cnt3[4:5],], nclass = 2, orden = c(5,2,1,3,4,6) ,title = "MG Country Complete Heterogeneity")
graphclass(MGCnty3[MGCnty3$Group %in% cnt3[7:8],], nclass = 2, orden = c(5,2,1,3,4,6) ,title = "MG Country Complete Heterogeneity")
graphclass(MGCnty3[MGCnty3$Group %in% cnt3[9:10],], nclass = 2, orden = c(5,2,1,3,4,6) ,title = "MG Country Complete Heterogeneity")
graphclass(MGCnty3[MGCnty3$Group %in% cnt3[12:13],], nclass = 2, orden = c(5,2,1,3,4,6) ,title = "MG Country Complete Heterogeneity")
graphclass(MGCnty3[MGCnty3$Group %in% cnt3[c(11,16,21)],], nclass = 2, orden = c(5,2,1,3,4,6) ,title = "MG Country Complete Heterogeneity")
graphclass(MGCnty3[MGCnty3$Group %in% cnt3[14:15],], nclass = 2, orden = c(5,2,1,3,4,6) ,title = "MG Country Complete Heterogeneity")
graphclass(MGCnty3[MGCnty3$Group %in% cnt3[17:18],], nclass = 2, orden = c(5,2,1,3,4,6) ,title = "MG Country Complete Heterogeneity")
graphclass(MGCnty3[MGCnty3$Group %in% cnt3[19:20],], nclass = 2, orden = c(5,2,1,3,4,6) ,title = "MG Country Complete Heterogeneity")

lcicn <- VarClass(MGCnty3)
lcicn %>% group_by(Cycle, ClassN, Group, param) %>% filter(category == 2) %>% 
  select(Cycle, param, ClassN, Group, value) %>% 
  mutate(value = cell_spec(value, color = ifelse(value > 0.8, "blue", "red"))) %>% 
  reshape2::dcast(Group + param  + Cycle ~ ClassN) %>% arrange(as.character(Group)) %>% 
  kbl(caption = "Probabilities to Agree each item by Class and Country Complete Heterogeneity", booktabs = TRUE, longtable = TRUE, 
      align = "llrrr", row.names = FALSE, digits = 3, col.names = c("Country", "Item", "Year", classes2M2), escape = FALSE) %>%
  kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"), 
                position = "left", font_size = 11, full_width = FALSE) %>% 
  column_spec(2, width = "18em") %>%  
  column_spec(3, width = "2em") %>%  
  column_spec(4:5, width = "4em") %>%  
  collapse_rows(c(1,2), valign = "top") %>%
  print()

cat('\n')
cat('\n')
#Extracting patterns
sizeMGCnty3 <- full_join(
  full_join(MGCntry$MGCnty_C1cl2_1CHet.out$class_counts$modelEstimated.patterns %>% 
              rename_with(~ c("1999", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>% 
              mutate(Class = paste(Group,C,sep="."),
                     Group = factor(Group, levels = 1:length(cnt1), labels = cnt1)) %>% 
              mutate(ClassN = factor(
                case_when(
                  #-----
                  Class == "1.1" ~ 2, #BGR
                  Class == "1.2" ~ 1,
                  Class == "2.1" ~ 1, #CHL
                  Class == "2.2" ~ 2,
                  Class == "3.1" ~ 2, #COL
                  Class == "3.2" ~ 1,
                  Class == "4.1" ~ 2, #DNK
                  Class == "4.2" ~ 1,
                  Class == "5.1" ~ 1, #EST
                  Class == "5.2" ~ 2,
                  Class == "6.1" ~ 2, #FIN
                  Class == "6.2" ~ 1,
                  Class == "7.1" ~ 1, #HKG
                  Class == "7.2" ~ 2,
                  Class == "8.1" ~ 1, #ITA
                  Class == "8.2" ~ 2,
                  Class == "9.1" ~ 1, #LTU
                  Class == "9.2" ~ 2,
                  Class == "10.1" ~ 1, #LVA
                  Class == "10.2" ~ 2,
                  Class == "11.1" ~ 1, #NOR
                  Class == "11.2" ~ 2,
                  Class == "12.1" ~ 2, #RUS
                  Class == "12.2" ~ 1,
                  Class == "13.1" ~ 1, #SVN
                  Class == "13.2" ~ 2,
                  Class == "14.1" ~ 1, #SWE
                  Class == "14.2" ~ 2
                  #-----
                ), levels = 1:2, labels = classes2M2)) %>% dplyr::select(-proportion, -C, -Class) ,
            MGCntry$MGCnty_C2cl2_1CHet.out$class_counts$modelEstimated.patterns %>% 
              rename_with(~ c("2009", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>% 
              mutate(Class = paste(Group,C,sep="."),
                     Group = factor(Group, levels = 1:length(cnt2), labels = cnt2)) %>%
              mutate(ClassN = factor(case_when(
                #-----
                Class == "1.1" ~ 1, #BFL
                Class == "1.2" ~ 2,
                Class == "2.1" ~ 2, #BGR
                Class == "2.2" ~ 1,
                Class == "3.1" ~ 1, #CHL
                Class == "3.2" ~ 2,
                Class == "4.1" ~ 1, #COL
                Class == "4.2" ~ 2,
                Class == "5.1" ~ 1, #DNK \\\SPECIAL CLASS 3
                Class == "5.2" ~ 2,
                Class == "6.1" ~ 1, #DOM
                Class == "6.2" ~ 2,
                Class == "7.1" ~ 1, #EST
                Class == "7.2" ~ 2,
                Class == "8.1" ~ 1, #FIN
                Class == "8.2" ~ 2,
                Class == "9.1" ~ 1, #HKG
                Class == "9.2" ~ 2,
                Class == "10.1" ~ 2, #ITA
                Class == "10.2" ~ 1,
                Class == "11.1" ~ 2, #KOR
                Class == "11.2" ~ 1,
                Class == "12.1" ~ 1, #LTU
                Class == "12.2" ~ 2,
                Class == "13.1" ~ 1, #LVA
                Class == "13.2" ~ 2,
                Class == "14.1" ~ 1, #MEX
                Class == "14.2" ~ 2,
                Class == "15.1" ~ 2, #MLT
                Class == "15.2" ~ 1,
                Class == "16.1" ~ 2, #NLD
                Class == "16.2" ~ 1,
                Class == "17.1" ~ 1, #NOR
                Class == "17.2" ~ 2,
                Class == "18.1" ~ 1, #RUS
                Class == "18.2" ~ 2,
                Class == "19.1" ~ 1, #SVN
                Class == "19.2" ~ 2,
                Class == "20.1" ~ 1, #SWE
                Class == "20.2" ~ 2,
                Class == "21.1" ~ 2, #TWN
                Class == "21.2" ~ 1
                #-----
              ), levels = 1:2, labels = classes2M2)) %>% dplyr::select(-proportion, -C, -Class), by = c("Group","ClassN")),
  MGCntry$MGCnty_C3cl2_1CHet.out$class_counts$modelEstimated.patterns %>%
    rename_with(~ c("2016", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>% 
    mutate(Class = paste(Group,C,sep="."),
           Group = factor(Group, levels = 1:length(cnt3), labels = cnt3)) %>%
    mutate(ClassN = factor(case_when(
      #-----
      Class == "1.1" ~ 1, #BFL
      Class == "1.2" ~ 2,
      Class == "2.1" ~ 2, #BGR
      Class == "2.2" ~ 1,
      Class == "3.1" ~ 2, #CHL
      Class == "3.2" ~ 1,
      Class == "4.1" ~ 2, #COL
      Class == "4.2" ~ 1,
      Class == "5.1" ~ 2, #DNK
      Class == "5.2" ~ 1,
      Class == "6.1" ~ 2, #DOM
      Class == "6.2" ~ 1,
      Class == "7.1" ~ 2, #EST
      Class == "7.2" ~ 1,
      Class == "8.1" ~ 1, #FIN
      Class == "8.2" ~ 2,
      Class == "9.1" ~ 1, #HKG
      Class == "9.2" ~ 2,
      Class == "10.1" ~ 2, #ITA
      Class == "10.2" ~ 1,
      Class == "11.1" ~ 1, #KOR
      Class == "11.2" ~ 2,
      Class == "12.1" ~ 1, #LTU
      Class == "12.2" ~ 2,
      Class == "13.1" ~ 1, #LVA
      Class == "13.2" ~ 2,
      Class == "14.1" ~ 1, #MEX
      Class == "14.2" ~ 2,
      Class == "15.1" ~ 2, #MLT
      Class == "15.2" ~ 1,
      Class == "16.1" ~ 2, #NLD
      Class == "16.2" ~ 1,
      Class == "17.1" ~ 2, #NOR
      Class == "17.2" ~ 1,
      Class == "18.1" ~ 1, #RUS
      Class == "18.2" ~ 2,
      Class == "19.1" ~ 1, #SVN
      Class == "19.2" ~ 2,
      Class == "20.1" ~ 1, #SWE
      Class == "20.2" ~ 2,
      Class == "21.1" ~ 2, #TWN
      Class == "21.2" ~ 1  
      #-----
    ), levels = 1:2, labels = classes2M2)) %>% dplyr::select(-proportion, -C, -Class) , by = c("Group","ClassN")) %>% 
  select(Group, ClassN, `1999`, `2009`, `2016`)

sizeMGCnty3 <- sizeMGCnty3 %>% reshape2::melt(id.vars = c("Group","ClassN"), variable.name = "Cycle") %>% 
  arrange(Group, Cycle) %>% 
  group_by(Group, Cycle) %>%
  mutate(countT= sum(value, na.rm = TRUE)) %>%
  group_by(ClassN) %>%
  mutate(per=value/countT) %>% dplyr::select(Group, Cycle, ClassN, per) 

HighProbMG(MGCnty3, sizeMGCnty3, orden = c(5,2,1,3,4,6), n = c(1,6,11), title = "Probabilities of Agree and size for each class in Country Multigroup analysis\n Complete Heterogeneity by Cycle")
HighProbMG(MGCnty3, sizeMGCnty3, orden = c(5,2,1,3,4,6), n = c(14,15,16,21))
HighProbMG(MGCnty3, sizeMGCnty3, orden = c(5,2,1,3,4,6), n = c(2:4))
HighProbMG(MGCnty3, sizeMGCnty3, orden = c(5,2,1,3,4,6), n = c(5,7,8))
HighProbMG(MGCnty3, sizeMGCnty3, orden = c(5,2,1,3,4,6), n = c(9,10,12:13))
HighProbMG(MGCnty3, sizeMGCnty3, orden = c(5,2,1,3,4,6), n = 17:20) 

sizeMGCnty3  %>% 
  reshape2::dcast(Group + Cycle ~ ClassN) %>% arrange(as.character(Group)) %>% 
  kbl(caption = "Size three classes MG Country Complete Heterogeneity", booktabs = TRUE, longtable = TRUE, row.names = FALSE, digits = 3) %>%
  kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"),
                position = "left", font_size = 11, full_width = FALSE) %>%
  column_spec(1:2, width = "4em") %>%  
  column_spec(3:4, width = "8em") %>%  
  collapse_rows(1) %>% 
  print()

# ------ 2C MG Country PH1 ----
cat('\n')
cat('\n')
cat('#### Partial homogeneity - one class equal across countries  \n')
cat('\n')
cat('\n')
warn_MGCntryPH1 <- list(C1 = MGCntry$MGCnty_C1cl2PH1.out$warnings,
                        C2 = MGCntry$MGCnty_C2cl2PH1.out$warnings,
                        C3 = MGCntry$MGCnty_C3cl2PH1.out$warnings)
cat('\n')
cat('\n')
wr <- names(warn_MGCntryPH1[grep("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED",warn_MGCntryPH1)])
if (length(wr) == 0) print("ALL BEST LOGLIKELIHOOD VALUES WERE REPLICATED") else
  cat("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED for the following cases:  \n", wr)
cat('\n')
cat('\n')


#----C1
cnt1 <- sort(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C1", c("COUNTRY")])$COUNTRY)
MGCnty_C1cl2PH1 <- MGCntry$MGCnty_C1cl2PH1.out$parameters$probability.scale %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>% 
  mutate(Group = factor(sub('\\..*', '', Class), 
                        levels = 1:length(cnt1), labels = cnt1),
         Class1 = sub('.{1,2}\\.', '', Class),
         ClassN = factor(
           case_when(
             Class1 == "1" ~ 2,
             Class1 == "2" ~ 1),
           levels = c("1", "2"), labels = classes2M2))
#----C2
cnt2 <- sort(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C2", c("COUNTRY")])$COUNTRY)
MGCnty_C2cl2PH1 <- MGCntry$MGCnty_C2cl2PH1.out$parameters$probability.scale %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>% 
  mutate(Group = factor(sub('\\..*', '', Class), 
                        levels = 1:length(cnt2), labels = cnt2), 
         Class1 = sub('.{1,2}\\.', '', Class),
         ClassN = factor(
           case_when(
             Class1 == "1" ~ 2,
             Class1 == "2" ~ 1),
           levels = c("1", "2"), labels = classes2M2))
#----C3
cnt3 <- sort(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C3", c("COUNTRY")])$COUNTRY)
MGCnty_C3cl2PH1 <- MGCntry$MGCnty_C3cl2PH1.out$parameters$probability.scale %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>% 
  mutate(Group = factor(sub('\\..*', '', Class), 
                        levels = 1:length(cnt3), labels = cnt3), 
         Class1 = sub('.{1,2}\\.', '', Class),
         ClassN = factor(
           case_when(
             Class1 == "1" ~ 2,
             Class1 == "2" ~ 1),
           levels = c("1", "2"), labels = classes2M2))

MGCnty3PH1 <- rbind(cbind(Cycle = "1999", MGCnty_C1cl2PH1),
                    cbind(Cycle = "2009", MGCnty_C2cl2PH1),
                    cbind(Cycle = "2016", MGCnty_C3cl2PH1))
#---
graphclass(MGCnty3PH1[MGCnty3PH1$Group %in% cnt3[c(1,6)],], nclass = 2, orden = c(5,2,1,3,4,6) ,title = "MG Country Partial homogeneity 1 class")
graphclass(MGCnty3PH1[MGCnty3PH1$Group %in% cnt3[2:3],], nclass = 2, orden = c(5,2,1,3,4,6) ,title = "MG Country Partial homogeneity 1 class")
graphclass(MGCnty3PH1[MGCnty3PH1$Group %in% cnt3[4:5],], nclass = 2, orden = c(5,2,1,3,4,6) ,title = "MG Country Partial homogeneity 1 class")
graphclass(MGCnty3PH1[MGCnty3PH1$Group %in% cnt3[7:8],], nclass = 2, orden = c(5,2,1,3,4,6) ,title = "MG Country Partial homogeneity 1 class")
graphclass(MGCnty3PH1[MGCnty3PH1$Group %in% cnt3[9:10],], nclass = 2, orden = c(5,2,1,3,4,6) ,title = "MG Country Partial homogeneity 1 class")
graphclass(MGCnty3PH1[MGCnty3PH1$Group %in% cnt3[12:13],], nclass = 2, orden = c(5,2,1,3,4,6) ,title = "MG Country Partial homogeneity 1 class")
graphclass(MGCnty3PH1[MGCnty3PH1$Group %in% cnt3[c(11,16,21)],], nclass = 2, orden = c(5,2,1,3,4,6) ,title = "MG Country Partial homogeneity 1 class")
graphclass(MGCnty3PH1[MGCnty3PH1$Group %in% cnt3[14:15],], nclass = 2, orden = c(5,2,1,3,4,6) ,title = "MG Country Partial homogeneity 1 class")
graphclass(MGCnty3PH1[MGCnty3PH1$Group %in% cnt3[17:18],], nclass = 2, orden = c(5,2,1,3,4,6) ,title = "MG Country Partial homogeneity 1 class")
graphclass(MGCnty3PH1[MGCnty3PH1$Group %in% cnt3[19:20],], nclass = 2, orden = c(5,2,1,3,4,6) ,title = "MG Country Partial homogeneity 1 class")

lcicn <- VarClass(MGCnty3PH1)
lcicn %>% group_by(Cycle, ClassN, Group, param) %>% filter(category == 2) %>% 
  select(Cycle, param, ClassN, Group, value) %>% 
  mutate(value = cell_spec(value, color = ifelse(value > 0.8, "blue", "red"))) %>% 
  reshape2::dcast(Group + param  + Cycle ~ ClassN) %>% arrange(as.character(Group)) %>% 
  kbl(caption = "Probabilities to Agree each item by Class and Country", booktabs = TRUE, longtable = TRUE, 
      align = "lllrrr", row.names = FALSE, digits = 3, col.names = c("Country", "Item", "Year", classes2M2), escape = FALSE) %>%
  kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"), 
                position = "left", font_size = 11, full_width = FALSE) %>% 
  column_spec(2, width = "18em") %>%  
  column_spec(3, width = "2em") %>%  
  column_spec(4:5, width = "4em") %>%  
  collapse_rows(c(1,2), valign = "top") %>%
  print()

cat('\n')
cat('\n')
#Extracting patterns
sizeMGCnty3PH1 <- full_join(
  full_join(MGCntry$MGCnty_C1cl2PH1.out$class_counts$modelEstimated.pattern %>% 
              rename_with(~ c("1999", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>% 
              mutate(Class = paste(Group,C,sep="."),
                     Group = factor(Group, levels = 1:length(cnt1), labels = cnt1),
                     Class1 = sub('.{1,2}\\.', '', Class)) %>% 
              mutate(ClassN = factor(
                case_when(
                  Class1 == "1" ~ 2,
                  Class1 == "2" ~ 1
                ), levels = 1:2, labels = classes2M2)) %>% dplyr::select(-proportion, -C, -Class) ,
            MGCntry$MGCnty_C2cl2PH1.out$class_counts$modelEstimated.patterns %>% 
              rename_with(~ c("2009", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>% 
              mutate(Class = paste(Group,C,sep="."),
                     Group = factor(Group, levels = 1:length(cnt2), labels = cnt2),
                     Class1 = sub('.{1,2}\\.', '', Class)) %>%
              mutate(ClassN = factor(case_when(
                Class1 == "1" ~ 2,
                Class1 == "2" ~ 1
              ), levels = 1:2, labels = classes2M2)) %>% dplyr::select(-proportion, -C, -Class), by = c("Group","ClassN")),
  MGCntry$MGCnty_C3cl2PH1.out$class_counts$modelEstimated.patterns %>%
    rename_with(~ c("2016", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>% 
    mutate(Class = paste(Group,C,sep="."),
           Group = factor(Group, levels = 1:length(cnt3), labels = cnt3),
           Class1 = sub('.{1,2}\\.', '', Class)) %>%
    mutate(ClassN = factor(case_when(
      Class1 == "1" ~ 2,
      Class1 == "2" ~ 1
    ), levels = 1:2, labels = classes2M2)) %>% dplyr::select(-proportion, -C, -Class) , by = c("Group","ClassN")) %>% 
  select(Group, ClassN, `1999`, `2009`, `2016`)

sizeMGCnty3PH1 <- sizeMGCnty3PH1 %>% reshape2::melt(id.vars = c("Group","ClassN"), variable.name = "Cycle") %>% 
  arrange(Group, Cycle) %>% 
  group_by(Group, Cycle) %>%
  mutate(countT= sum(value, na.rm = TRUE)) %>%
  group_by(ClassN) %>%
  mutate(per=value/countT) %>% dplyr::select(Group, Cycle, ClassN, per) 

HighProbMG(MGCnty3PH1, sizeMGCnty3PH1, orden = c(5,2,1,3,4,6), n = c(1,6,11), title = "Probabilities of Agree and size for each class in Country Multigroup analysis Partial homogeneity (1 class fixed across countries) by Cycle")
HighProbMG(MGCnty3PH1, sizeMGCnty3PH1, orden = c(5,2,1,3,4,6), n = c(14,15,16,21))
HighProbMG(MGCnty3PH1, sizeMGCnty3PH1, orden = c(5,2,1,3,4,6), n = c(2:4))
HighProbMG(MGCnty3PH1, sizeMGCnty3PH1, orden = c(5,2,1,3,4,6), n = c(5,7,8))
HighProbMG(MGCnty3PH1, sizeMGCnty3PH1, orden = c(5,2,1,3,4,6), n = c(9,10,12:13))
HighProbMG(MGCnty3PH1, sizeMGCnty3PH1, orden = c(5,2,1,3,4,6), n = 17:20) 

sizeMGCnty3PH1  %>% 
  reshape2::dcast(Group + Cycle ~ ClassN) %>% arrange(as.character(Group)) %>% 
  kbl(caption = "Size three classes MG Country - Partial homogeneity (1 class equal across countries)", booktabs = TRUE, longtable = TRUE, row.names = FALSE, digits = 3) %>%
  kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"),
                position = "left", font_size = 11, full_width = FALSE) %>%
  column_spec(1:2, width = "4em") %>%  
  column_spec(3:4, width = "8em") %>%  
  collapse_rows(1) %>% 
  print()
cat('\n')
cat('\n') 
# ------ 2C MG Country PH2 ----
cat('#### Complete homogeneity - all classes equal across countries  \n')
cat('\n')
cat('\n')
warn_MGCntryPH2 <- list(C1 = MGCntry$MGCnty_C1cl2PH2.out$warnings,
                        C2 = MGCntry$MGCnty_C2cl2PH2.out$warnings,
                        C3 = MGCntry$MGCnty_C3cl2PH2.out$warnings)
cat('\n')
cat('\n')
wr <- names(warn_MGCntryPH2[grep("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED",warn_MGCntryPH2)])
if (length(wr) == 0) print("ALL BEST LOGLIKELIHOOD VALUES WERE REPLICATED") else
  cat("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED for the following cases:  \n", wr)
cat('\n')
cat('\n')


#----C1
cnt1 <- sort(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C1", c("COUNTRY")])$COUNTRY)
MGCnty_C1cl2PH2 <- MGCntry$MGCnty_C1cl2PH2.out$parameters$probability.scale %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>% 
  mutate(Class = factor(
           case_when(
             Class == "C#1" ~ 2,
             Class == "C#2" ~ 1),
           levels = c("1", "2"), labels = classes2M2),
         ClassN = Class)
#----C2
cnt2 <- sort(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C2", c("COUNTRY")])$COUNTRY)
MGCnty_C2cl2PH2 <- MGCntry$MGCnty_C2cl2PH2.out$parameters$probability.scale %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>% 
  mutate(Class = factor(
           case_when(
             Class == "C#1" ~ 2,
             Class == "C#2" ~ 1),
           levels = c("1", "2"), labels = classes2M2),
         ClassN = Class)
#----C3
cnt3 <- sort(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C3", c("COUNTRY")])$COUNTRY)
MGCnty_C3cl2PH2 <- MGCntry$MGCnty_C3cl2PH2.out$parameters$probability.scale %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>% 
  mutate(Class = factor(
           case_when(
             Class == "C#1" ~ 1,
             Class == "C#2" ~ 2),
           levels = c("1", "2"), labels = classes2M2),
         ClassN = Class)

MGCnty3PH2 <- rbind(cbind(Cycle = "1999", MGCnty_C1cl2PH2),
                    cbind(Cycle = "2009", MGCnty_C2cl2PH2),
                    cbind(Cycle = "2016", MGCnty_C3cl2PH2))
#---
graphclass(MGCnty3PH2, nclass = 2, orden = c(5,2,1,3,4,6) ,title = "MG Country Complete homogeneity", mg = FALSE)

lcicn <- VarClass(MGCnty3PH2)
lcicn %>% group_by(Cycle, ClassN, param) %>% filter(category == 2) %>% 
  select(Cycle, param, ClassN, value) %>% 
  mutate(value = cell_spec(value, color = ifelse(value > 0.8, "blue", "red"))) %>% 
  reshape2::dcast(param  + Cycle ~ ClassN) %>% 
  kbl(caption = "Probabilities to Agree each item by Class and Country", booktabs = TRUE, longtable = TRUE, 
      align = "llrr", row.names = FALSE, digits = 3, col.names = c("Item", "Year", classes2M2), escape = FALSE) %>%
  kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"), 
                position = "left", font_size = 11, full_width = FALSE) %>% 
  column_spec(1, width = "18em") %>%  
  column_spec(2, width = "2em") %>%  
  column_spec(3:4, width = "4em") %>%  
  collapse_rows(c(1), valign = "top") %>%
  print()

cat('\n')
cat('\n')
#Extracting patterns
sizeMGCnty3PH2 <- full_join(
  full_join(MGCntry$MGCnty_C1cl2PH2.out$class_counts$modelEstimated.pattern %>% 
              rename_with(~ c("1999", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>% 
              mutate(Class = paste(Group,C,sep="."),
                     Group = factor(Group, levels = 1:length(cnt1), labels = cnt1),
                     Class1 = sub('.{1,2}\\.', '', Class)) %>% 
              mutate(ClassN = factor(
                case_when(
                  Class1 == "1" ~ 2,
                  Class1 == "2" ~ 1
                ), levels = 1:2, labels = classes2M2)) %>% dplyr::select(-proportion, -C, -Class) ,
            MGCntry$MGCnty_C2cl2PH2.out$class_counts$modelEstimated.patterns %>% 
              rename_with(~ c("2009", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>% 
              mutate(Class = paste(Group,C,sep="."),
                     Group = factor(Group, levels = 1:length(cnt2), labels = cnt2),
                     Class1 = sub('.{1,2}\\.', '', Class)) %>%
              mutate(ClassN = factor(case_when(
                Class1 == "1" ~ 2,
                Class1 == "2" ~ 1
              ), levels = 1:2, labels = classes2M2)) %>% dplyr::select(-proportion, -C, -Class), by = c("Group","ClassN")),
  MGCntry$MGCnty_C3cl2PH2.out$class_counts$modelEstimated.patterns %>%
    rename_with(~ c("2016", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>% 
    mutate(Class = paste(Group,C,sep="."),
           Group = factor(Group, levels = 1:length(cnt3), labels = cnt3),
           Class1 = sub('.{1,2}\\.', '', Class)) %>%
    mutate(ClassN = factor(case_when(
      Class1 == "1" ~ 1,
      Class1 == "2" ~ 2
    ), levels = 1:2, labels = classes2M2)) %>% dplyr::select(-proportion, -C, -Class) , by = c("Group","ClassN")) %>% 
  select(Group, ClassN, `1999`, `2009`, `2016`)

sizeMGCnty3PH2 <- sizeMGCnty3PH2 %>% reshape2::melt(id.vars = c("Group","ClassN"), variable.name = "Cycle") %>% 
  arrange(Group, Cycle) %>% 
  group_by(Group, Cycle) %>%
  mutate(countT= sum(value, na.rm = TRUE)) %>%
  group_by(ClassN) %>%
  mutate(per=value/countT) %>% dplyr::select(Group, Cycle, ClassN, per) 

MGCnty3PH2rep <- rbind(cbind(Group=rep(cnt1, each = nrow(MGCnty3PH2[MGCnty3PH2$Cycle == '1999',])), MGCnty3PH2[MGCnty3PH2$Cycle == '1999',]),
                       cbind(Group=rep(cnt2, each = nrow(MGCnty3PH2[MGCnty3PH2$Cycle == '2009',])), MGCnty3PH2[MGCnty3PH2$Cycle == '2009',]),
                       cbind(Group=rep(cnt3, each = nrow(MGCnty3PH2[MGCnty3PH2$Cycle == '2016',])), MGCnty3PH2[MGCnty3PH2$Cycle == '2016',]))

HighProbMG(MGCnty3PH2rep, sizeMGCnty3PH2, orden = c(5,2,1,3,4,6), n = c(1,6,11), title = "Probabilities of Agree and size for each class in Country Multigroup analysis\n Complete homogeneity by Cycle")
HighProbMG(MGCnty3PH2rep, sizeMGCnty3PH2, orden = c(5,2,1,3,4,6), n = c(14,15,16,21))
HighProbMG(MGCnty3PH2rep, sizeMGCnty3PH2, orden = c(5,2,1,3,4,6), n = c(2:4))
HighProbMG(MGCnty3PH2rep, sizeMGCnty3PH2, orden = c(5,2,1,3,4,6), n = c(5,7,8))
HighProbMG(MGCnty3PH2rep, sizeMGCnty3PH2, orden = c(5,2,1,3,4,6), n = c(9,10,12:13))
HighProbMG(MGCnty3PH2rep, sizeMGCnty3PH2, orden = c(5,2,1,3,4,6), n = 17:20) 

sizeMGCnty3PH2  %>% 
  reshape2::dcast(Group + Cycle ~ ClassN) %>% arrange(as.character(Group)) %>% 
  kbl(caption = "Size three classes MG Country Complete homogeneity", booktabs = TRUE, longtable = TRUE, row.names = FALSE, digits = 3) %>%
  kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"),
                position = "left", font_size = 11, full_width = FALSE) %>%
  column_spec(1:2, width = "4em") %>%  
  column_spec(3:4, width = "8em") %>%  
  collapse_rows(1) %>% 
  print()
cat('\n')
cat('\n') 

#----------

cat('\n')
cat('\n')
cat('### Results with 3-classes  \n')
cat('\n')
cat('\n')
# ------ 3C MG Country _1CHet ----
cat('#### Complete Heterogeneity across country \n')
cat('\n')
cat('\n')

warn_MGCntry <- list(C1 = MGCntry$MGCnty_C1cl3_1CHet.out$warnings,
                     C2 = MGCntry$MGCnty_C2cl3_1CHet.out$warnings,
                     C3 = MGCntry$MGCnty_C3cl3_1CHet.out$warnings)
cat('\n')
cat('\n')
wr <- names(warn_MGCntry[grep("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED",warn_MGCntry)])
if (length(wr) == 0) print("ALL BEST LOGLIKELIHOOD VALUES WERE REPLICATED") else
  cat("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED for the following cases:  \n", wr)
cat('\n')
cat('\n')


#----C1
cnt1 <- sort(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C1", c("COUNTRY")])$COUNTRY)
MGCnty_C1cl3 <- MGCntry$MGCnty_C1cl3_1CHet.out$parameters$probability.scale %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>% 
  mutate(Group = factor(sub('\\..*', '', Class), 
                        levels = 1:length(cnt1), labels = cnt1), 
         ClassN = factor(case_when(
           #-----
           Class == "1.1" ~ 1, #BGR
           Class == "1.2" ~ 2,
           Class == "1.3" ~ 3,
           Class == "2.1" ~ 3, #CHL
           Class == "2.2" ~ 2,
           Class == "2.3" ~ 1,
           Class == "3.1" ~ 3, #COL
           Class == "3.2" ~ 1,
           Class == "3.3" ~ 2,
           Class == "4.1" ~ 1, #DNK
           Class == "4.2" ~ 2,
           Class == "4.3" ~ 3,
           Class == "5.1" ~ 2, #EST
           Class == "5.2" ~ 3,
           Class == "5.3" ~ 1,
           Class == "6.1" ~ 1, #FIN
           Class == "6.2" ~ 2,
           Class == "6.3" ~ 3,
           Class == "7.1" ~ 2, #HKG
           Class == "7.2" ~ 1,
           Class == "7.3" ~ 3,
           Class == "8.1" ~ 1, #ITA
           Class == "8.2" ~ 3,
           Class == "8.3" ~ 2,
           Class == "9.1" ~ 1, #LTU
           Class == "9.2" ~ 2,
           Class == "9.3" ~ 3,
           Class == "10.1" ~ 2, #LVA
           Class == "10.2" ~ 1,
           Class == "10.3" ~ 3,
           Class == "11.1" ~ 1, #NOR
           Class == "11.2" ~ 3,
           Class == "11.3" ~ 2,
           Class == "12.1" ~ 2, #RUS
           Class == "12.2" ~ 1,
           Class == "12.3" ~ 3,
           Class == "13.1" ~ 1, #SVN
           Class == "13.2" ~ 3,
           Class == "13.3" ~ 2,
           Class == "14.1" ~ 1, #SWE
           Class == "14.2" ~ 2,
           Class == "14.3" ~ 3
           #-----
         ), levels = c("1", "2", "3"), labels = classes3M2))
#----C2
cnt2 <- sort(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C2", c("COUNTRY")])$COUNTRY)
MGCnty_C2cl3 <- MGCntry$MGCnty_C2cl3_1CHet.out$parameters$probability.scale %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x))%>% 
  mutate(Group = factor(sub('\\..*', '', Class), 
                        levels = 1:length(cnt2), labels = cnt2), 
         ClassN = factor(case_when(
           #-----
           Class == "1.1" ~ 1, #BFL
           Class == "1.2" ~ 3,
           Class == "1.3" ~ 2,
           Class == "2.1" ~ 2, #BGR
           Class == "2.2" ~ 3,
           Class == "2.3" ~ 1,
           Class == "3.1" ~ 1, #CHL
           Class == "3.2" ~ 3,
           Class == "3.3" ~ 2, 
           Class == "4.1" ~ 1, #COL
           Class == "4.2" ~ 2,
           Class == "4.3" ~ 3,
           Class == "5.1" ~ 3, #DNK \\\SPECIAL CLASS 3
           Class == "5.2" ~ 2,
           Class == "5.3" ~ 1,
           Class == "6.1" ~ 1, #DOM
           Class == "6.2" ~ 3,
           Class == "6.3" ~ 2,
           Class == "7.1" ~ 3, #EST
           Class == "7.2" ~ 2,
           Class == "7.3" ~ 1,
           Class == "8.1" ~ 1, #FIN
           Class == "8.2" ~ 2,
           Class == "8.3" ~ 3,
           Class == "9.1" ~ 3, #HKG
           Class == "9.2" ~ 2,
           Class == "9.3" ~ 1,
           Class == "10.1" ~ 1, #ITA
           Class == "10.2" ~ 2,
           Class == "10.3" ~ 3,
           Class == "11.1" ~ 1, #KOR
           Class == "11.2" ~ 3,
           Class == "11.3" ~ 2,
           Class == "12.1" ~ 3, #LTU
           Class == "12.2" ~ 2,
           Class == "12.3" ~ 1,
           Class == "13.1" ~ 3, #LVA
           Class == "13.2" ~ 1,
           Class == "13.3" ~ 2,
           Class == "14.1" ~ 1, #MEX
           Class == "14.2" ~ 3,
           Class == "14.3" ~ 2,
           Class == "15.1" ~ 1, #MLT
           Class == "15.2" ~ 3,
           Class == "15.3" ~ 2,
           Class == "16.1" ~ 3, #NLD
           Class == "16.2" ~ 2,
           Class == "16.3" ~ 1,
           Class == "17.1" ~ 3, #NOR
           Class == "17.2" ~ 1,
           Class == "17.3" ~ 2,
           Class == "18.1" ~ 2, #RUS
           Class == "18.2" ~ 1,
           Class == "18.3" ~ 3,
           Class == "19.1" ~ 3, #SVN
           Class == "19.2" ~ 1,
           Class == "19.3" ~ 2,
           Class == "20.1" ~ 3, #SWE
           Class == "20.2" ~ 1,
           Class == "20.3" ~ 2,
           Class == "21.1" ~ 3, #TWN
           Class == "21.2" ~ 1,
           Class == "21.3" ~ 2
           #-----
         ), levels = c("1", "2", "3"), labels = classes3M2))
#----C3
cnt3 <- sort(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C3", c("COUNTRY")])$COUNTRY)
MGCnty_C3cl3 <- MGCntry$MGCnty_C3cl3_1CHet.out$parameters$probability.scale %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>% 
  mutate(Group = factor(sub('\\..*', '', Class), 
                        levels = 1:length(cnt3), labels = cnt3), 
         ClassN = factor(case_when(
           #-----
           Class == "1.1" ~ 2, #BFL
           Class == "1.2" ~ 3,
           Class == "1.3" ~ 1,
           Class == "2.1" ~ 2, #BGR
           Class == "2.2" ~ 3,
           Class == "2.3" ~ 1,
           Class == "3.1" ~ 3, #CHL
           Class == "3.2" ~ 1,
           Class == "3.3" ~ 2, 
           Class == "4.1" ~ 2, #COL
           Class == "4.2" ~ 3,
           Class == "4.3" ~ 1,
           Class == "5.1" ~ 2, #DNK
           Class == "5.2" ~ 3,
           Class == "5.3" ~ 1,
           Class == "6.1" ~ 2, #DOM
           Class == "6.2" ~ 3,
           Class == "6.3" ~ 1,
           Class == "7.1" ~ 2, #EST
           Class == "7.2" ~ 3,
           Class == "7.3" ~ 1,
           Class == "8.1" ~ 2, #FIN
           Class == "8.2" ~ 1,
           Class == "8.3" ~ 3,
           Class == "9.1" ~ 1, #HKG
           Class == "9.2" ~ 2,
           Class == "9.3" ~ 3,
           Class == "10.1" ~ 1, #ITA
           Class == "10.2" ~ 3,
           Class == "10.3" ~ 2,
           Class == "11.1" ~ 2, #KOR
           Class == "11.2" ~ 3,
           Class == "11.3" ~ 1,
           Class == "12.1" ~ 3, #LTU
           Class == "12.2" ~ 1,
           Class == "12.3" ~ 2,
           Class == "13.1" ~ 3, #LVA
           Class == "13.2" ~ 1,
           Class == "13.3" ~ 2,
           Class == "14.1" ~ 3, #MEX
           Class == "14.2" ~ 1,
           Class == "14.3" ~ 2,
           Class == "15.1" ~ 2, #MLT
           Class == "15.2" ~ 3,
           Class == "15.3" ~ 1,
           Class == "16.1" ~ 3, #NLD
           Class == "16.2" ~ 2,
           Class == "16.3" ~ 1,
           Class == "17.1" ~ 1, #NOR
           Class == "17.2" ~ 3,
           Class == "17.3" ~ 2,
           Class == "18.1" ~ 3, #RUS
           Class == "18.2" ~ 2,
           Class == "18.3" ~ 1,
           Class == "19.1" ~ 1, #SVN
           Class == "19.2" ~ 3,
           Class == "19.3" ~ 2,
           Class == "20.1" ~ 1, #SWE
           Class == "20.2" ~ 3,
           Class == "20.3" ~ 2,
           Class == "21.1" ~ 2, #TWN
           Class == "21.2" ~ 3,
           Class == "21.3" ~ 1         
           #-----
         ), levels = c("1", "2", "3"), labels = classes3M2))

MGCnty3 <- rbind(cbind(Cycle = "1999", MGCnty_C1cl3),
                 cbind(Cycle = "2009", MGCnty_C2cl3),
                 cbind(Cycle = "2016", MGCnty_C3cl3))
#---
graphclass(MGCnty3[MGCnty3$Group %in% cnt3[c(1,6)],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country Complete Heterogeneity")
graphclass(MGCnty3[MGCnty3$Group %in% cnt3[2:3],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country Complete Heterogeneity")
graphclass(MGCnty3[MGCnty3$Group %in% cnt3[4:5],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country Complete Heterogeneity")
graphclass(MGCnty3[MGCnty3$Group %in% cnt3[7:8],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country Complete Heterogeneity")
graphclass(MGCnty3[MGCnty3$Group %in% cnt3[9:10],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country Complete Heterogeneity")
graphclass(MGCnty3[MGCnty3$Group %in% cnt3[12:13],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country Complete Heterogeneity")
graphclass(MGCnty3[MGCnty3$Group %in% cnt3[c(11,16,21)],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country Complete Heterogeneity")
graphclass(MGCnty3[MGCnty3$Group %in% cnt3[14:15],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country Complete Heterogeneity")
graphclass(MGCnty3[MGCnty3$Group %in% cnt3[17:18],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country Complete Heterogeneity")
graphclass(MGCnty3[MGCnty3$Group %in% cnt3[19:20],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country Complete Heterogeneity")

lcicn <- VarClass(MGCnty3)
lcicn %>% group_by(Cycle, ClassN, Group, param) %>% filter(category == 2) %>% 
  select(Cycle, param, ClassN, Group, value) %>% 
  mutate(value = cell_spec(value, color = ifelse(value > 0.8, "blue", "red"))) %>% 
  reshape2::dcast(Group + param  + Cycle ~ ClassN) %>% arrange(as.character(Group)) %>% 
  kbl(caption = "Probabilities to Agree each item by Class and Country Complete Heterogeneity", booktabs = TRUE, longtable = TRUE, 
      align = "llrrr", row.names = FALSE, digits = 3, col.names = c("Country", "Item", "Year", classes3M2), escape = FALSE) %>%
  kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"), 
                position = "left", font_size = 11, full_width = FALSE) %>% 
  column_spec(2, width = "18em") %>%  
  column_spec(3, width = "2em") %>%  
  column_spec(4:6, width = "4em") %>%  
  collapse_rows(c(1,2), valign = "top") %>%
  print()

cat('\n')
cat('\n')
#Extracting patterns
sizeMGCnty3 <- full_join(
  full_join(MGCntry$MGCnty_C1cl3_1CHet.out$class_counts$modelEstimated.patterns %>% 
              rename_with(~ c("1999", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>% 
              mutate(Class = paste(Group,C,sep="."),
                     Group = factor(Group, levels = 1:length(cnt1), labels = cnt1)) %>% 
              mutate(ClassN = factor(
                case_when(
                  #-----
                  Class == "1.1" ~ 1, #BGR
                  Class == "1.2" ~ 2,
                  Class == "1.3" ~ 3,
                  Class == "2.1" ~ 3, #CHL
                  Class == "2.2" ~ 2,
                  Class == "2.3" ~ 1,
                  Class == "3.1" ~ 3, #COL
                  Class == "3.2" ~ 1,
                  Class == "3.3" ~ 2,
                  Class == "4.1" ~ 1, #DNK
                  Class == "4.2" ~ 2,
                  Class == "4.3" ~ 3,
                  Class == "5.1" ~ 2, #EST
                  Class == "5.2" ~ 3,
                  Class == "5.3" ~ 1,
                  Class == "6.1" ~ 1, #FIN
                  Class == "6.2" ~ 2,
                  Class == "6.3" ~ 3,
                  Class == "7.1" ~ 2, #HKG
                  Class == "7.2" ~ 1,
                  Class == "7.3" ~ 3,
                  Class == "8.1" ~ 1, #ITA
                  Class == "8.2" ~ 3,
                  Class == "8.3" ~ 2,
                  Class == "9.1" ~ 1, #LTU
                  Class == "9.2" ~ 2,
                  Class == "9.3" ~ 3,
                  Class == "10.1" ~ 2, #LVA
                  Class == "10.2" ~ 1,
                  Class == "10.3" ~ 3,
                  Class == "11.1" ~ 1, #NOR
                  Class == "11.2" ~ 3,
                  Class == "11.3" ~ 2,
                  Class == "12.1" ~ 2, #RUS
                  Class == "12.2" ~ 1,
                  Class == "12.3" ~ 3,
                  Class == "13.1" ~ 1, #SVN
                  Class == "13.2" ~ 3,
                  Class == "13.3" ~ 2,
                  Class == "14.1" ~ 1, #SWE
                  Class == "14.2" ~ 2,
                  Class == "14.3" ~ 3
                  #-----
                ), levels = 1:3, labels = classes3M2)) %>% dplyr::select(-proportion, -C, -Class) ,
            MGCntry$MGCnty_C2cl3_1CHet.out$class_counts$modelEstimated.patterns %>% 
              rename_with(~ c("2009", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>% 
              mutate(Class = paste(Group,C,sep="."),
                     Group = factor(Group, levels = 1:length(cnt2), labels = cnt2)) %>%
              mutate(ClassN = factor(case_when(
                #-----
                Class == "1.1" ~ 1, #BFL
                Class == "1.2" ~ 3,
                Class == "1.3" ~ 2,
                Class == "2.1" ~ 2, #BGR
                Class == "2.2" ~ 3,
                Class == "2.3" ~ 1,
                Class == "3.1" ~ 1, #CHL
                Class == "3.2" ~ 3,
                Class == "3.3" ~ 2, 
                Class == "4.1" ~ 1, #COL
                Class == "4.2" ~ 2,
                Class == "4.3" ~ 3,
                Class == "5.1" ~ 3, #DNK \\\SPECIAL CLASS 3
                Class == "5.2" ~ 2,
                Class == "5.3" ~ 1,
                Class == "6.1" ~ 1, #DOM
                Class == "6.2" ~ 3,
                Class == "6.3" ~ 2,
                Class == "7.1" ~ 3, #EST
                Class == "7.2" ~ 2,
                Class == "7.3" ~ 1,
                Class == "8.1" ~ 1, #FIN
                Class == "8.2" ~ 2,
                Class == "8.3" ~ 3,
                Class == "9.1" ~ 3, #HKG
                Class == "9.2" ~ 2,
                Class == "9.3" ~ 1,
                Class == "10.1" ~ 1, #ITA
                Class == "10.2" ~ 2,
                Class == "10.3" ~ 3,
                Class == "11.1" ~ 1, #KOR
                Class == "11.2" ~ 3,
                Class == "11.3" ~ 2,
                Class == "12.1" ~ 3, #LTU
                Class == "12.2" ~ 2,
                Class == "12.3" ~ 1,
                Class == "13.1" ~ 3, #LVA
                Class == "13.2" ~ 1,
                Class == "13.3" ~ 2,
                Class == "14.1" ~ 1, #MEX
                Class == "14.2" ~ 3,
                Class == "14.3" ~ 2,
                Class == "15.1" ~ 1, #MLT
                Class == "15.2" ~ 3,
                Class == "15.3" ~ 2,
                Class == "16.1" ~ 3, #NLD
                Class == "16.2" ~ 2,
                Class == "16.3" ~ 1,
                Class == "17.1" ~ 3, #NOR
                Class == "17.2" ~ 1,
                Class == "17.3" ~ 2,
                Class == "18.1" ~ 2, #RUS
                Class == "18.2" ~ 1,
                Class == "18.3" ~ 3,
                Class == "19.1" ~ 3, #SVN
                Class == "19.2" ~ 1,
                Class == "19.3" ~ 2,
                Class == "20.1" ~ 3, #SWE
                Class == "20.2" ~ 1,
                Class == "20.3" ~ 2,
                Class == "21.1" ~ 3, #TWN
                Class == "21.2" ~ 1,
                Class == "21.3" ~ 2
                #-----
              ), levels = 1:3, labels = classes3M2)) %>% dplyr::select(-proportion, -C, -Class), by = c("Group","ClassN")),
  MGCntry$MGCnty_C3cl3_1CHet.out$class_counts$modelEstimated.patterns %>%
    rename_with(~ c("2016", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>% 
    mutate(Class = paste(Group,C,sep="."),
           Group = factor(Group, levels = 1:length(cnt3), labels = cnt3)) %>%
    mutate(ClassN = factor(case_when(
      #-----
      Class == "1.1" ~ 2, #BFL
      Class == "1.2" ~ 3,
      Class == "1.3" ~ 1,
      Class == "2.1" ~ 2, #BGR
      Class == "2.2" ~ 3,
      Class == "2.3" ~ 1,
      Class == "3.1" ~ 3, #CHL
      Class == "3.2" ~ 1,
      Class == "3.3" ~ 2, 
      Class == "4.1" ~ 2, #COL
      Class == "4.2" ~ 3,
      Class == "4.3" ~ 1,
      Class == "5.1" ~ 2, #DNK
      Class == "5.2" ~ 3,
      Class == "5.3" ~ 1,
      Class == "6.1" ~ 2, #DOM
      Class == "6.2" ~ 3,
      Class == "6.3" ~ 1,
      Class == "7.1" ~ 2, #EST
      Class == "7.2" ~ 3,
      Class == "7.3" ~ 1,
      Class == "8.1" ~ 2, #FIN
      Class == "8.2" ~ 1,
      Class == "8.3" ~ 3,
      Class == "9.1" ~ 1, #HKG
      Class == "9.2" ~ 2,
      Class == "9.3" ~ 3,
      Class == "10.1" ~ 1, #ITA
      Class == "10.2" ~ 3,
      Class == "10.3" ~ 2,
      Class == "11.1" ~ 2, #KOR
      Class == "11.2" ~ 3,
      Class == "11.3" ~ 1,
      Class == "12.1" ~ 3, #LTU
      Class == "12.2" ~ 1,
      Class == "12.3" ~ 2,
      Class == "13.1" ~ 3, #LVA
      Class == "13.2" ~ 1,
      Class == "13.3" ~ 2,
      Class == "14.1" ~ 3, #MEX
      Class == "14.2" ~ 1,
      Class == "14.3" ~ 2,
      Class == "15.1" ~ 2, #MLT
      Class == "15.2" ~ 3,
      Class == "15.3" ~ 1,
      Class == "16.1" ~ 3, #NLD
      Class == "16.2" ~ 2,
      Class == "16.3" ~ 1,
      Class == "17.1" ~ 1, #NOR
      Class == "17.2" ~ 3,
      Class == "17.3" ~ 2,
      Class == "18.1" ~ 3, #RUS
      Class == "18.2" ~ 2,
      Class == "18.3" ~ 1,
      Class == "19.1" ~ 1, #SVN
      Class == "19.2" ~ 3,
      Class == "19.3" ~ 2,
      Class == "20.1" ~ 1, #SWE
      Class == "20.2" ~ 3,
      Class == "20.3" ~ 2,
      Class == "21.1" ~ 2, #TWN
      Class == "21.2" ~ 3,
      Class == "21.3" ~ 1
      #-----
    ), levels = 1:3, labels = classes3M2)) %>% dplyr::select(-proportion, -C, -Class) , by = c("Group","ClassN")) %>% 
  select(Group, ClassN, `1999`, `2009`, `2016`)

sizeMGCnty3 <- sizeMGCnty3 %>% reshape2::melt(id.vars = c("Group","ClassN"), variable.name = "Cycle") %>% 
    arrange(Group, Cycle) %>% 
    group_by(Group, Cycle) %>%
    mutate(countT= sum(value, na.rm = TRUE)) %>%
    group_by(ClassN) %>%
    mutate(per=value/countT) %>% dplyr::select(Group, Cycle, ClassN, per) 
  
  HighProbMG(MGCnty3, sizeMGCnty3, orden = c(5,2,1,3,4,6), n = c(1,6,11), title = "Probabilities of Agree and size for each class in Country Multigroup analysis\n Complete Heterogeneity by Cycle")
  HighProbMG(MGCnty3, sizeMGCnty3, orden = c(5,2,1,3,4,6), n = c(14,15,16,21))
  HighProbMG(MGCnty3, sizeMGCnty3, orden = c(5,2,1,3,4,6), n = c(2:4))
  HighProbMG(MGCnty3, sizeMGCnty3, orden = c(5,2,1,3,4,6), n = c(5,7,8))
  HighProbMG(MGCnty3, sizeMGCnty3, orden = c(5,2,1,3,4,6), n = c(9,10,12:13))
  HighProbMG(MGCnty3, sizeMGCnty3, orden = c(5,2,1,3,4,6), n = 17:20) 
  
  sizeMGCnty3  %>% 
  reshape2::dcast(Group + Cycle ~ ClassN) %>% arrange(as.character(Group)) %>% 
  kbl(caption = "Size three classes MG Country Complete Heterogeneity", booktabs = TRUE, longtable = TRUE, row.names = FALSE, digits = 3) %>%
  kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"),
                position = "left", font_size = 11, full_width = FALSE) %>%
    column_spec(1:2, width = "4em") %>%  
    column_spec(3:5, width = "8em") %>%  
    collapse_rows(1) %>% 
    print()
  
# ------ 3C MG Country PH1 ----
cat('\n')
cat('\n')
cat('#### Partial homogeneity - one class equal across countries  \n')
cat('\n')
cat('\n')
warn_MGCntryPH1 <- list(C1 = MGCntry$MGCnty_C1cl3PH1.out$warnings,
                        C2 = MGCntry$MGCnty_C2cl3PH1.out$warnings,
                        C3 = MGCntry$MGCnty_C3cl3PH1.out$warnings)
cat('\n')
cat('\n')
wr <- names(warn_MGCntryPH1[grep("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED",warn_MGCntryPH1)])
if (length(wr) == 0) print("ALL BEST LOGLIKELIHOOD VALUES WERE REPLICATED") else
  cat("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED for the following cases:  \n", wr)
cat('\n')
cat('\n')


#----C1
cnt1 <- sort(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C1", c("COUNTRY")])$COUNTRY)
MGCnty_C1cl3PH1 <- MGCntry$MGCnty_C1cl3PH1.out$parameters$probability.scale %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>% 
  mutate(Group = factor(sub('\\..*', '', Class), 
                        levels = 1:length(cnt1), labels = cnt1),
         Class1 = sub('.{1,2}\\.', '', Class),
         ClassN = factor(
           case_when(
             Class1 == "1" ~ 3,
             Class1 == "2" ~ 2,
             Class1 == "3" ~ 1),
           levels = c("1", "2", "3"), labels = classes3M2))
#----C2
cnt2 <- sort(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C2", c("COUNTRY")])$COUNTRY)
MGCnty_C2cl3PH1 <- MGCntry$MGCnty_C2cl3PH1.out$parameters$probability.scale %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>% 
  mutate(Group = factor(sub('\\..*', '', Class), 
                        levels = 1:length(cnt2), labels = cnt2), 
         Class1 = sub('.{1,2}\\.', '', Class),
         ClassN = factor(
           case_when(
             Class1 == "1" ~ 3,
             Class1 == "2" ~ 1,
             Class1 == "3" ~ 2),
           levels = c("1", "2", "3"), labels = classes3M2))
#----C3
cnt3 <- sort(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C3", c("COUNTRY")])$COUNTRY)
MGCnty_C3cl3PH1 <- MGCntry$MGCnty_C3cl3PH1.out$parameters$probability.scale %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>% 
  mutate(Group = factor(sub('\\..*', '', Class), 
                        levels = 1:length(cnt3), labels = cnt3), 
         Class1 = sub('.{1,2}\\.', '', Class),
         ClassN = factor(
           case_when(
             Class1 == "1" ~ 3,
             Class1 == "2" ~ 2,
             Class1 == "3" ~ 1),
           levels = c("1", "2", "3"), labels = classes3M2))

MGCnty3PH1 <- rbind(cbind(Cycle = "1999", MGCnty_C1cl3PH1),
                    cbind(Cycle = "2009", MGCnty_C2cl3PH1),
                    cbind(Cycle = "2016", MGCnty_C3cl3PH1))
#---
graphclass(MGCnty3PH1[MGCnty3PH1$Group %in% cnt3[c(1,6)],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country Partial homogeneity 1 class")
graphclass(MGCnty3PH1[MGCnty3PH1$Group %in% cnt3[2:3],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country Partial homogeneity 1 class")
graphclass(MGCnty3PH1[MGCnty3PH1$Group %in% cnt3[4:5],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country Partial homogeneity 1 class")
graphclass(MGCnty3PH1[MGCnty3PH1$Group %in% cnt3[7:8],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country Partial homogeneity 1 class")
graphclass(MGCnty3PH1[MGCnty3PH1$Group %in% cnt3[9:10],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country Partial homogeneity 1 class")
graphclass(MGCnty3PH1[MGCnty3PH1$Group %in% cnt3[12:13],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country Partial homogeneity 1 class")
graphclass(MGCnty3PH1[MGCnty3PH1$Group %in% cnt3[c(11,16,21)],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country Partial homogeneity 1 class")
graphclass(MGCnty3PH1[MGCnty3PH1$Group %in% cnt3[14:15],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country Partial homogeneity 1 class")
graphclass(MGCnty3PH1[MGCnty3PH1$Group %in% cnt3[17:18],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country Partial homogeneity 1 class")
graphclass(MGCnty3PH1[MGCnty3PH1$Group %in% cnt3[19:20],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country Partial homogeneity 1 class")

lcicn <- VarClass(MGCnty3PH1)
lcicn %>% group_by(Cycle, ClassN, Group, param) %>% filter(category == 2) %>% 
  select(Cycle, param, ClassN, Group, value) %>% 
  mutate(value = cell_spec(value, color = ifelse(value > 0.8, "blue", "red"))) %>% 
  reshape2::dcast(Group + param  + Cycle ~ ClassN) %>% arrange(as.character(Group)) %>% 
  kbl(caption = "Probabilities to Agree each item by Class and Country", booktabs = TRUE, longtable = TRUE, 
      align = "lllrrr", row.names = FALSE, digits = 3, col.names = c("Country", "Item", "Year", classes3M2), escape = FALSE) %>%
  kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"), 
                position = "left", font_size = 11, full_width = FALSE) %>% 
  column_spec(2, width = "18em") %>%  
  column_spec(3, width = "2em") %>%  
  column_spec(4:6, width = "4em") %>%  
  collapse_rows(c(1,2), valign = "top") %>%
  print()

cat('\n')
cat('\n')
#Extracting patterns
sizeMGCnty3PH1 <- full_join(
  full_join(MGCntry$MGCnty_C1cl3PH1.out$class_counts$modelEstimated.pattern %>% 
              rename_with(~ c("1999", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>% 
              mutate(Class = paste(Group,C,sep="."),
                     Group = factor(Group, levels = 1:length(cnt1), labels = cnt1),
                     Class1 = sub('.{1,2}\\.', '', Class)) %>% 
              mutate(ClassN = factor(
                case_when(
                  Class1 == "1" ~ 2,
                  Class1 == "2" ~ 3,
                  Class1 == "3" ~ 1
                ), levels = 1:3, labels = classes3M2)) %>% dplyr::select(-proportion, -C, -Class) ,
            MGCntry$MGCnty_C2cl3PH1.out$class_counts$modelEstimated.patterns %>% 
              rename_with(~ c("2009", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>% 
              mutate(Class = paste(Group,C,sep="."),
                     Group = factor(Group, levels = 1:length(cnt2), labels = cnt2),
                     Class1 = sub('.{1,2}\\.', '', Class)) %>%
              mutate(ClassN = factor(case_when(
                Class1 == "1" ~ 2,
                Class1 == "2" ~ 3,
                Class1 == "3" ~ 1
              ), levels = 1:3, labels = classes3M2)) %>% dplyr::select(-proportion, -C, -Class), by = c("Group","ClassN")),
  MGCntry$MGCnty_C3cl3PH1.out$class_counts$modelEstimated.patterns %>%
    rename_with(~ c("2016", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>% 
    mutate(Class = paste(Group,C,sep="."),
           Group = factor(Group, levels = 1:length(cnt3), labels = cnt3),
           Class1 = sub('.{1,2}\\.', '', Class)) %>%
    mutate(ClassN = factor(case_when(
      Class1 == "1" ~ 3,
      Class1 == "2" ~ 1,
      Class1 == "3" ~ 2
    ), levels = 1:3, labels = classes3M2)) %>% dplyr::select(-proportion, -C, -Class) , by = c("Group","ClassN")) %>% 
  select(Group, ClassN, `1999`, `2009`, `2016`)

sizeMGCnty3PH1 <- sizeMGCnty3PH1 %>% reshape2::melt(id.vars = c("Group","ClassN"), variable.name = "Cycle") %>% 
  arrange(Group, Cycle) %>% 
  group_by(Group, Cycle) %>%
  mutate(countT= sum(value, na.rm = TRUE)) %>%
  group_by(ClassN) %>%
  mutate(per=value/countT) %>% dplyr::select(Group, Cycle, ClassN, per) 

HighProbMG(MGCnty3PH1, sizeMGCnty3PH1, orden = c(5,2,1,3,4,6), n = c(1,6,11), title = "Probabilities of Agree and size for each class in Country Multigroup analysis Partial homogeneity (1 class fixed across countries) by Cycle")
HighProbMG(MGCnty3PH1, sizeMGCnty3PH1, orden = c(5,2,1,3,4,6), n = c(14,15,16,21))
HighProbMG(MGCnty3PH1, sizeMGCnty3PH1, orden = c(5,2,1,3,4,6), n = c(2:4))
HighProbMG(MGCnty3PH1, sizeMGCnty3PH1, orden = c(5,2,1,3,4,6), n = c(5,7,8))
HighProbMG(MGCnty3PH1, sizeMGCnty3PH1, orden = c(5,2,1,3,4,6), n = c(9,10,12:13))
HighProbMG(MGCnty3PH1, sizeMGCnty3PH1, orden = c(5,2,1,3,4,6), n = 17:20) 

sizeMGCnty3PH1  %>% 
  reshape2::dcast(Group + Cycle ~ ClassN) %>% arrange(as.character(Group)) %>% 
  kbl(caption = "Size three classes MG Country - Partial homogeneity (1 class equal across countries)", booktabs = TRUE, longtable = TRUE, row.names = FALSE, digits = 3) %>%
  kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"),
                position = "left", font_size = 11, full_width = FALSE) %>%
  column_spec(1:2, width = "4em") %>%  
  column_spec(3:5, width = "8em") %>%  
  collapse_rows(1) %>% 
  print()
cat('\n')
cat('\n') 
# ------ 3C MG Country PH2 ----
cat('#### Partial homogeneity - two classes equal across countries  \n')
cat('\n')
cat('\n')
warn_MGCntryPH2 <- list(C1 = MGCntry$MGCnty_C1cl3PH2.out$warnings,
                     C2 = MGCntry$MGCnty_C2cl3PH2.out$warnings,
                     C3 = MGCntry$MGCnty_C3cl3PH2.out$warnings)
cat('\n')
cat('\n')
wr <- names(warn_MGCntryPH2[grep("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED",warn_MGCntryPH2)])
if (length(wr) == 0) print("ALL BEST LOGLIKELIHOOD VALUES WERE REPLICATED") else
  cat("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED for the following cases:  \n", wr)
cat('\n')
cat('\n')


#----C1
cnt1 <- sort(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C1", c("COUNTRY")])$COUNTRY)
MGCnty_C1cl3PH2 <- MGCntry$MGCnty_C1cl3PH2.out$parameters$probability.scale %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>% 
  mutate(Group = factor(sub('\\..*', '', Class), 
                        levels = 1:length(cnt1), labels = cnt1),
         Class1 = sub('.{1,2}\\.', '', Class),
         ClassN = factor(
           case_when(
             Class1 == "1" ~ 2,
             Class1 == "2" ~ 3,
             Class1 == "3" ~ 1),
           levels = c("1", "2", "3"), labels = classes3M2))
#----C2
cnt2 <- sort(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C2", c("COUNTRY")])$COUNTRY)
MGCnty_C2cl3PH2 <- MGCntry$MGCnty_C2cl3PH2.out$parameters$probability.scale %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>% 
  mutate(Group = factor(sub('\\..*', '', Class), 
                        levels = 1:length(cnt2), labels = cnt2), 
         Class1 = sub('.{1,2}\\.', '', Class),
         ClassN = factor(
           case_when(
             Class1 == "1" ~ 2,
             Class1 == "2" ~ 3,
             Class1 == "3" ~ 1),
           levels = c("1", "2", "3"), labels = classes3M2))
#----C3
cnt3 <- sort(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C3", c("COUNTRY")])$COUNTRY)
MGCnty_C3cl3PH2 <- MGCntry$MGCnty_C3cl3PH2.out$parameters$probability.scale %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>% 
  mutate(Group = factor(sub('\\..*', '', Class), 
                        levels = 1:length(cnt3), labels = cnt3), 
         Class1 = sub('.{1,2}\\.', '', Class),
         ClassN = factor(
           case_when(
             Class1 == "1" ~ 3,
             Class1 == "2" ~ 1,
             Class1 == "3" ~ 2),
           levels = c("1", "2", "3"), labels = classes3M2))

MGCnty3PH2 <- rbind(cbind(Cycle = "1999", MGCnty_C1cl3PH2),
                 cbind(Cycle = "2009", MGCnty_C2cl3PH2),
                 cbind(Cycle = "2016", MGCnty_C3cl3PH2))
#---
graphclass(MGCnty3PH2[MGCnty3PH2$Group %in% cnt3[c(1,6)],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country Partial homogeneity in 2 classes")
graphclass(MGCnty3PH2[MGCnty3PH2$Group %in% cnt3[2:3],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country Partial homogeneity 2 classes")
graphclass(MGCnty3PH2[MGCnty3PH2$Group %in% cnt3[4:5],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country Partial homogeneity 2 classes")
graphclass(MGCnty3PH2[MGCnty3PH2$Group %in% cnt3[7:8],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country Partial homogeneity 2 classes")
graphclass(MGCnty3PH2[MGCnty3PH2$Group %in% cnt3[9:10],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country Partial homogeneity 2 classes")
graphclass(MGCnty3PH2[MGCnty3PH2$Group %in% cnt3[12:13],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country Partial homogeneity 2 classes")
graphclass(MGCnty3PH2[MGCnty3PH2$Group %in% cnt3[c(11,16,21)],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country Partial homogeneity 2 classes")
graphclass(MGCnty3PH2[MGCnty3PH2$Group %in% cnt3[14:15],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country Partial homogeneity 2 classes")
graphclass(MGCnty3PH2[MGCnty3PH2$Group %in% cnt3[17:18],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country Partial homogeneity 2 classes")
graphclass(MGCnty3PH2[MGCnty3PH2$Group %in% cnt3[19:20],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country Partial homogeneity 2 classes")

lcicn <- VarClass(MGCnty3PH2)
lcicn %>% group_by(Cycle, ClassN, Group, param) %>% filter(category == 2) %>% 
  select(Cycle, param, ClassN, Group, value) %>% 
  mutate(value = cell_spec(value, color = ifelse(value > 0.8, "blue", "red"))) %>% 
  reshape2::dcast(Group + param  + Cycle ~ ClassN) %>% arrange(as.character(Group)) %>% 
  kbl(caption = "Probabilities to Agree each item by Class and Country", booktabs = TRUE, longtable = TRUE, 
      align = "lllrrr", row.names = FALSE, digits = 3, col.names = c("Country", "Item", "Year", classes3M2), escape = FALSE) %>%
  kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"), 
                position = "left", font_size = 11, full_width = FALSE) %>% 
  column_spec(2, width = "18em") %>%  
  column_spec(3, width = "2em") %>%  
  column_spec(4:6, width = "4em") %>%  
  collapse_rows(c(1,2), valign = "top") %>%
  print()

cat('\n')
cat('\n')
#Extracting patterns
sizeMGCnty3PH2 <- full_join(
  full_join(MGCntry$MGCnty_C1cl3PH2.out$class_counts$modelEstimated.pattern %>% 
              rename_with(~ c("1999", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>% 
              mutate(Class = paste(Group,C,sep="."),
                     Group = factor(Group, levels = 1:length(cnt1), labels = cnt1),
                     Class1 = sub('.{1,2}\\.', '', Class)) %>% 
              mutate(ClassN = factor(
                case_when(
                  Class1 == "1" ~ 2,
                  Class1 == "2" ~ 3,
                  Class1 == "3" ~ 1
                ), levels = 1:3, labels = classes3M2)) %>% dplyr::select(-proportion, -C, -Class) ,
            MGCntry$MGCnty_C2cl3PH2.out$class_counts$modelEstimated.patterns %>% 
              rename_with(~ c("2009", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>% 
              mutate(Class = paste(Group,C,sep="."),
                     Group = factor(Group, levels = 1:length(cnt2), labels = cnt2),
                     Class1 = sub('.{1,2}\\.', '', Class)) %>%
              mutate(ClassN = factor(case_when(
                Class1 == "1" ~ 2,
                Class1 == "2" ~ 3,
                Class1 == "3" ~ 1
              ), levels = 1:3, labels = classes3M2)) %>% dplyr::select(-proportion, -C, -Class), by = c("Group","ClassN")),
  MGCntry$MGCnty_C3cl3PH2.out$class_counts$modelEstimated.patterns %>%
    rename_with(~ c("2016", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>% 
    mutate(Class = paste(Group,C,sep="."),
           Group = factor(Group, levels = 1:length(cnt3), labels = cnt3),
           Class1 = sub('.{1,2}\\.', '', Class)) %>%
    mutate(ClassN = factor(case_when(
      Class1 == "1" ~ 3,
      Class1 == "2" ~ 1,
      Class1 == "3" ~ 2
    ), levels = 1:3, labels = classes3M2)) %>% dplyr::select(-proportion, -C, -Class) , by = c("Group","ClassN")) %>% 
  select(Group, ClassN, `1999`, `2009`, `2016`)

sizeMGCnty3PH2 <- sizeMGCnty3PH2 %>% reshape2::melt(id.vars = c("Group","ClassN"), variable.name = "Cycle") %>% 
  arrange(Group, Cycle) %>% 
  group_by(Group, Cycle) %>%
  mutate(countT= sum(value, na.rm = TRUE)) %>%
  group_by(ClassN) %>%
  mutate(per=value/countT) %>% dplyr::select(Group, Cycle, ClassN, per) 

HighProbMG(MGCnty3PH2, sizeMGCnty3PH2, orden = c(5,2,1,3,4,6), n = c(1,6,11), title = "Probabilities of Agree and size for each class in Country Multigroup analysis\n Partial homogeneity 2 classes by Cycle")
HighProbMG(MGCnty3PH2, sizeMGCnty3PH2, orden = c(5,2,1,3,4,6), n = c(14,15,16,21))
HighProbMG(MGCnty3PH2, sizeMGCnty3PH2, orden = c(5,2,1,3,4,6), n = c(2:4))
HighProbMG(MGCnty3PH2, sizeMGCnty3PH2, orden = c(5,2,1,3,4,6), n = c(5,7,8))
HighProbMG(MGCnty3PH2, sizeMGCnty3PH2, orden = c(5,2,1,3,4,6), n = c(9,10,12:13))
HighProbMG(MGCnty3PH2, sizeMGCnty3PH2, orden = c(5,2,1,3,4,6), n = 17:20) 

sizeMGCnty3PH2  %>% 
  reshape2::dcast(Group + Cycle ~ ClassN) %>% arrange(as.character(Group)) %>% 
  kbl(caption = "Size three classes MG Country Partial homogeneity 2 classes", booktabs = TRUE, longtable = TRUE, row.names = FALSE, digits = 3) %>%
  kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"),
                position = "left", font_size = 11, full_width = FALSE) %>%
  column_spec(1:2, width = "4em") %>%  
  column_spec(3:5, width = "8em") %>%  
  collapse_rows(1) %>% 
  print()
cat('\n')
cat('\n') 

# ------ 3C MG Country PH3 ----
cat('#### Complete homogeneity - all classes equal across countries  \n')
cat('\n')
cat('\n')

warn_MGCntryPH3 <- list(C1 = MGCntry$MGCnty_C1cl3PH3.out$warnings,
                        C2 = MGCntry$MGCnty_C2cl3PH3.out$warnings,
                        C3 = MGCntry$MGCnty_C3cl3PH3.out$warnings)
cat('\n')
cat('\n')
wr <- names(warn_MGCntryPH3[grep("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED",warn_MGCntryPH3)])
if (length(wr) == 0) print("ALL BEST LOGLIKELIHOOD VALUES WERE REPLICATED") else
  cat("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED for the following cases:  \n", wr)
cat('\n')
cat('\n')


#----C1
cnt1 <- sort(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C1", c("COUNTRY")])$COUNTRY)
MGCnty_C1cl3PH3 <- MGCntry$MGCnty_C1cl3PH3.out$parameters$probability.scale %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>% 
  mutate(Class = factor(
           case_when(
             Class == "C#1" ~ 2,
             Class == "C#2" ~ 3,
             Class == "C#3" ~ 1),
           levels = c(1, 2, 3), labels = classes3M2),
         ClassN = Class)
#----C2
cnt2 <- sort(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C2", c("COUNTRY")])$COUNTRY)
MGCnty_C2cl3PH3 <- MGCntry$MGCnty_C2cl3PH3.out$parameters$probability.scale %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>% 
  mutate(Class = factor(
           case_when(
             Class == "C#1" ~ 1,
             Class == "C#2" ~ 2,
             Class == "C#3" ~ 3),
           levels = c(1, 2, 3), labels = classes3M2),
         ClassN = Class)
#----C3
cnt3 <- sort(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C3", c("COUNTRY")])$COUNTRY)
MGCnty_C3cl3PH3 <- MGCntry$MGCnty_C3cl3PH3.out$parameters$probability.scale %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>% 
  mutate(Class = factor(
           case_when(
             Class == "C#1" ~ 2,
             Class == "C#2" ~ 3,
             Class == "C#3" ~ 1),
           levels = c(1, 2, 3), labels = classes3M2),
         ClassN = Class)

MGCnty3PH3 <- rbind(cbind(Cycle = "1999", MGCnty_C1cl3PH3),
                    cbind(Cycle = "2009", MGCnty_C2cl3PH3),
                    cbind(Cycle = "2016", MGCnty_C3cl3PH3))
#---
graphclass(MGCnty3PH3, nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country Complete homogeneity", mg = FALSE)

lcicn <- VarClass(MGCnty3PH3)
lcicn %>% group_by(Cycle, ClassN, param) %>% filter(category == 2) %>% 
  select(Cycle, param, ClassN, value) %>% 
  mutate(value = cell_spec(value, color = ifelse(value > 0.8, "blue", "red"))) %>% 
  reshape2::dcast(param  + Cycle ~ ClassN) %>% 
  kbl(caption = "Probabilities to Agree each item by Class and Country Complete Homogeneity", booktabs = TRUE, longtable = TRUE, 
      align = "lllrrr", row.names = FALSE, digits = 3, col.names = c("Item", "Year", classes3M2), escape = FALSE) %>%
  kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"), 
                position = "left", font_size = 11, full_width = FALSE) %>% 
  column_spec(1, width = "18em") %>%  
  column_spec(2, width = "2em") %>%  
  column_spec(3:5, width = "4em") %>%  
  collapse_rows(1, valign = "top") %>%
  print()

cat('\n')
cat('\n')
#Extracting patterns
sizeMGCnty3PH3 <- full_join(
  full_join(MGCntry$MGCnty_C1cl3PH3.out$class_counts$modelEstimated.patterns %>% 
              rename_with(~ c("1999", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>% 
              mutate(Class = paste(Group,C,sep="."),
                     Group = factor(Group, levels = 1:length(cnt1), labels = cnt1),
                     Class1 = sub('.{1,2}\\.', '', Class)) %>% 
              mutate(ClassN = factor(
                case_when(
                  Class1 == "1" ~ 2,
                  Class1 == "2" ~ 3,
                  Class1 == "3" ~ 1
                ), levels = 1:3, labels = classes3M2)) %>% dplyr::select(-proportion, -C, -Class) ,
            MGCntry$MGCnty_C2cl3PH3.out$class_counts$modelEstimated.patterns %>% 
              rename_with(~ c("2009", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>% 
              mutate(Class = paste(Group,C,sep="."),
                     Group = factor(Group, levels = 1:length(cnt2), labels = cnt2),
                     Class1 = sub('.{1,2}\\.', '', Class)) %>%
              mutate(ClassN = factor(case_when(
                Class1 == "1" ~ 1,
                Class1 == "2" ~ 2,
                Class1 == "3" ~ 3
              ), levels = 1:3, labels = classes3M2)) %>% dplyr::select(-proportion, -C, -Class), by = c("Group","ClassN")),
  MGCntry$MGCnty_C3cl3PH3.out$class_counts$modelEstimated.patterns %>%
    rename_with(~ c("2016", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>% 
    mutate(Class = paste(Group,C,sep="."),
           Group = factor(Group, levels = 1:length(cnt3), labels = cnt3),
           Class1 = sub('.{1,2}\\.', '', Class)) %>%
    mutate(ClassN = factor(case_when(
      Class1 == "1" ~ 2,
      Class1 == "2" ~ 3,
      Class1 == "3" ~ 1
    ), levels = 1:3, labels = classes3M2)) %>% dplyr::select(-proportion, -C, -Class) , by = c("Group","ClassN")) %>% 
  select(Group, ClassN, `1999`, `2009`, `2016`)

sizeMGCnty3PH3 <- sizeMGCnty3PH3 %>% reshape2::melt(id.vars = c("Group","ClassN"), variable.name = "Cycle") %>% 
  arrange(Group, Cycle) %>% 
  group_by(Group, Cycle) %>%
  mutate(countT= sum(value, na.rm = TRUE)) %>%
  group_by(ClassN) %>%
  mutate(per=value/countT) %>% dplyr::select(Group, Cycle, ClassN, per) 

MGCnty3PH3rep <- rbind(cbind(Group=rep(cnt1, each = nrow(MGCnty3PH3[MGCnty3PH3$Cycle == '1999',])), MGCnty3PH3[MGCnty3PH3$Cycle == '1999',]),
                       cbind(Group=rep(cnt2, each = nrow(MGCnty3PH3[MGCnty3PH3$Cycle == '2009',])), MGCnty3PH3[MGCnty3PH3$Cycle == '2009',]),
                       cbind(Group=rep(cnt3, each = nrow(MGCnty3PH3[MGCnty3PH3$Cycle == '2016',])), MGCnty3PH3[MGCnty3PH3$Cycle == '2016',]))

HighProbMG(MGCnty3PH3rep, sizeMGCnty3PH3, orden = c(5,2,1,3,4,6), n = c(1,6,11), 
           title = "Probabilities of Agree and size for each class in Country Multigroup analysis\n Complete Homogeneity by Cycle")
HighProbMG(MGCnty3PH3rep, sizeMGCnty3PH3, orden = c(5,2,1,3,4,6), n = c(14,15,16,21))
HighProbMG(MGCnty3PH3rep, sizeMGCnty3PH3, orden = c(5,2,1,3,4,6), n = c(2:4))
HighProbMG(MGCnty3PH3rep, sizeMGCnty3PH3, orden = c(5,2,1,3,4,6), n = c(5,7,8))
HighProbMG(MGCnty3PH3rep, sizeMGCnty3PH3, orden = c(5,2,1,3,4,6), n = c(9,10,12:13))
HighProbMG(MGCnty3PH3rep, sizeMGCnty3PH3, orden = c(5,2,1,3,4,6), n = 17:20) 

sizeMGCnty3PH3  %>% 
  reshape2::dcast(Group + Cycle ~ ClassN) %>% arrange(as.character(Group)) %>% 
  kbl(caption = "Size three classes MG Country Complete Homogeneity", booktabs = TRUE, longtable = TRUE, row.names = FALSE, digits = 3) %>%
  kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"),
                position = "left", font_size = 11, full_width = FALSE) %>%
  column_spec(1:2, width = "4em") %>%  
  column_spec(3:5, width = "8em") %>%  
  collapse_rows(1) %>% 
  print()
cat('\n')
cat('\n') 


#----------

cat('\\newpage')
# ----------Results with MG CountryGender by cycle ----
cat('\n')
cat('\n')
cat('## Multigroup analysis for Country*Gender  \n')
cat('\n')
cat('\n')
Graph_modelfitMG("MGCntryGndr")

cat('\n')
cat('\n')
cat('### Results with 3-classes  \n')
cat('\n')
cat('\n')
# ------ 3C MG Country Gender _1CHet ----
cat('#### Complete Heterogeneity across Country and Gender \n')
cat('\n')
cat('\n')
warn_MGCntryGndr <- list(C1 = MGCntryGndr$MGcntygndr_C1cl3_1CHet.out$warnings,
                         C2 = MGCntryGndr$MGcntygndr_C2cl3_1CHet.out$warnings,
                         C3 = MGCntryGndr$MGcntygndr_C3cl3_1CHet.out$warnings)

cat('\n')
cat('\n')
wr <- names(warn_MGCntryGndr[grep("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED",warn_MGCntryGndr)])
if (length(wr) == 0) print("ALL BEST LOGLIKELIHOOD VALUES WERE REPLICATED") else
  cat("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED for the following cases:  \n", wr)
cat('\n')
cat('\n')

#----C1
cntgn1 <- sort(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C1", c("cntgnd")])$cntgnd)
cnt1 <- sort(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C1", c("COUNTRY")])$COUNTRY)
MGCntyGndr_C1cl3 <- MGCntryGndr$MGcntygndr_C1cl3_1CHet.out$parameters$probability.scale %>%
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>%
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>%
  mutate(Group = factor(sub('\\..*', '', Class),
                        levels = 1:length(cntgn1), labels = cntgn1),
         Group1 = factor(sub("Boy|Girl","",Group)),
         Group2 = factor(substr(Group, 4, nchar(as.character(Group)))),
         ClassN = factor(case_when(
           #-----
           Class == "1.1" ~ 2, #BGRBoy
           Class == "1.2" ~ 3,
           Class == "1.3" ~ 1,
           Class == "2.1" ~ 1, #BGRGirl
           Class == "2.2" ~ 2,
           Class == "2.3" ~ 3,
           Class == "3.1" ~ 1, #CHLBoy
           Class == "3.2" ~ 2,
           Class == "3.3" ~ 3,
           Class == "4.1" ~ 2, #CHLGirl
           Class == "4.2" ~ 3,
           Class == "4.3" ~ 1,
           Class == "5.1" ~ 2, #COLBoy
           Class == "5.2" ~ 1,
           Class == "5.3" ~ 3,
           Class == "6.1" ~ 1, #COLGirl
           Class == "6.2" ~ 3,
           Class == "6.3" ~ 2,
           Class == "7.1" ~ 1, #DKNBoy
           Class == "7.2" ~ 3,
           Class == "7.3" ~ 2,
           Class == "8.1" ~ 3, #DKNGirl
           Class == "8.2" ~ 2,
           Class == "8.3" ~ 1,
           Class == "9.1" ~ 1, #ESTBoy
           Class == "9.2" ~ 2,
           Class == "9.3" ~ 3,
           Class == "10.1" ~ 2, #ESTGirl
           Class == "10.2" ~ 3,
           Class == "10.3" ~ 1,
           Class == "11.1" ~ 3, #FINBoy
           Class == "11.2" ~ 1,
           Class == "11.3" ~ 2,
           Class == "12.1" ~ 2, #FINGirl
           Class == "12.2" ~ 3,
           Class == "12.3" ~ 1,
           Class == "13.1" ~ 2, #HKGBoy
           Class == "13.2" ~ 3,
           Class == "13.3" ~ 1,
           Class == "14.1" ~ 3, #HKGGirl
           Class == "14.2" ~ 2,
           Class == "14.3" ~ 1,
           Class == "15.1" ~ 1, #ITABoy
           Class == "15.2" ~ 3,
           Class == "15.3" ~ 2,
           Class == "16.1" ~ 1, #ITAGirl
           Class == "16.2" ~ 2,
           Class == "16.3" ~ 3,
           Class == "17.1" ~ 3, #LTUBoy
           Class == "17.2" ~ 1,
           Class == "17.3" ~ 2,
           Class == "18.1" ~ 1, #LTUGirl
           Class == "18.2" ~ 2,
           Class == "18.3" ~ 3,
           Class == "19.1" ~ 2, #LVABoy
           Class == "19.2" ~ 3,
           Class == "19.3" ~ 1,
           Class == "20.1" ~ 1, #LVAGirl
           Class == "20.2" ~ 2,
           Class == "20.3" ~ 3,
           Class == "21.1" ~ 3, #NORBoy
           Class == "21.2" ~ 1,
           Class == "21.3" ~ 2,
           Class == "22.1" ~ 2, #NORGirl
           Class == "22.2" ~ 1,
           Class == "22.3" ~ 3,
           Class == "23.1" ~ 3, #RUSBoy
           Class == "23.2" ~ 1,
           Class == "23.3" ~ 2,
           Class == "24.1" ~ 2, #RUSGirl
           Class == "24.2" ~ 3,
           Class == "24.3" ~ 1,
           Class == "25.1" ~ 1, #SVNBoy
           Class == "25.2" ~ 3,
           Class == "25.3" ~ 2,
           Class == "26.1" ~ 2, #SVNGirl
           Class == "26.2" ~ 1,
           Class == "26.3" ~ 3,
           Class == "27.1" ~ 2, #SWEBoy
           Class == "27.2" ~ 1,
           Class == "27.3" ~ 3,
           Class == "28.1" ~ 3, #SWEGirl
           Class == "28.2" ~ 1,
           Class == "28.3" ~ 2)
           # ----
           , levels = c("1", "2", "3"), labels = classes3M2))
#----C2
cntgn2 <- sort(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C2", c("cntgnd")])$cntgnd)
MGCntyGndr_C2cl3 <- MGCntryGndr$MGcntygndr_C2cl3_1CHet.out$parameters$probability.scale %>%
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>%
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>%
  mutate(Group = factor(sub('\\..*', '', Class),
                        levels = 1:length(cntgn2), labels = cntgn2),
         Group1 = factor(sub("Boy|Girl","",Group)),
         Group2 = factor(substr(Group, 4, nchar(as.character(Group)))),
         ClassN = factor(case_when(
           #-----
           Class == "1.1" ~ 3, #BFLBoy
           Class == "1.2" ~ 1,
           Class == "1.3" ~ 2,
           Class == "2.1" ~ 2, #BFLGirl
           Class == "2.2" ~ 1,
           Class == "2.3" ~ 3,
           Class == "3.1" ~ 2, #BRGBoy
           Class == "3.2" ~ 3,
           Class == "3.3" ~ 1,
           Class == "4.1" ~ 2, #BRGGirl
           Class == "4.2" ~ 3,
           Class == "4.3" ~ 1,
           Class == "5.1" ~ 1, #CHLBoy
           Class == "5.2" ~ 2,
           Class == "5.3" ~ 3,
           Class == "6.1" ~ 2, #CHLGirl
           Class == "6.2" ~ 1,
           Class == "6.3" ~ 3,
           Class == "7.1" ~ 1, #COLBoy
           Class == "7.2" ~ 3,
           Class == "7.3" ~ 2,
           Class == "8.1" ~ 2, #COLGirl
           Class == "8.2" ~ 3,
           Class == "8.3" ~ 1,
           Class == "9.1" ~ 3, #DNKBoy
           Class == "9.2" ~ 2,
           Class == "9.3" ~ 1,
           Class == "10.1" ~ 2, #DNKGirl
           Class == "10.2" ~ 1,
           Class == "10.3" ~ 3,
           Class == "11.1" ~ 3, #DOMBoy
           Class == "11.2" ~ 2,
           Class == "11.3" ~ 1,
           Class == "12.1" ~ 2, #DOMGirl
           Class == "12.2" ~ 1,
           Class == "12.3" ~ 3,
           Class == "13.1" ~ 2, #ESTBoy
           Class == "13.2" ~ 3,
           Class == "13.3" ~ 1,
           Class == "14.1" ~ 3, #ESTGirl
           Class == "14.2" ~ 2,
           Class == "14.3" ~ 1,
           Class == "15.1" ~ 3, #FINBoy
           Class == "15.2" ~ 1,
           Class == "15.3" ~ 2,
           Class == "16.1" ~ 1, #FINGirl
           Class == "16.2" ~ 3,
           Class == "16.3" ~ 2,
           Class == "17.1" ~ 1, #HKGBoy
           Class == "17.2" ~ 3,
           Class == "17.3" ~ 2,
           Class == "18.1" ~ 2, #HKGGirl
           Class == "18.2" ~ 1,
           Class == "18.3" ~ 3,
           Class == "19.1" ~ 2, #ITABoy
           Class == "19.2" ~ 1,
           Class == "19.3" ~ 3,
           Class == "20.1" ~ 2, #ITAGirl
           Class == "20.2" ~ 1,
           Class == "20.3" ~ 3,
           Class == "21.1" ~ 1, #KORBoy
           Class == "21.2" ~ 2,
           Class == "21.3" ~ 3,
           Class == "22.1" ~ 2, #KORGirl
           Class == "22.2" ~ 3,
           Class == "22.3" ~ 1,
           Class == "23.1" ~ 2, #LTUBoy
           Class == "23.2" ~ 1,
           Class == "23.3" ~ 3,
           Class == "24.1" ~ 2, #LTUGirl
           Class == "24.2" ~ 1,
           Class == "24.3" ~ 3,
           Class == "25.1" ~ 3, #LVABoy
           Class == "25.2" ~ 2,
           Class == "25.3" ~ 1,
           Class == "26.1" ~ 1, #LVAGirl
           Class == "26.2" ~ 2,
           Class == "26.3" ~ 3,
           Class == "27.1" ~ 3, #MEXBoy
           Class == "27.2" ~ 2,
           Class == "27.3" ~ 1,
           Class == "28.1" ~ 1, #MEXGirl
           Class == "28.2" ~ 2,
           Class == "28.3" ~ 3,
           Class == "29.1" ~ 3, #MLTBoy
           Class == "29.2" ~ 1,
           Class == "29.3" ~ 2,
           Class == "30.1" ~ 1, #MLTGirl
           Class == "30.2" ~ 3,
           Class == "30.3" ~ 2,
           Class == "31.1" ~ 2, #NLDBoy
           Class == "31.2" ~ 1,
           Class == "31.3" ~ 3,
           Class == "32.1" ~ 1, #NLDGirl
           Class == "32.2" ~ 3,
           Class == "32.3" ~ 2,
           Class == "33.1" ~ 1, #NORBoy
           Class == "33.2" ~ 2,
           Class == "33.3" ~ 3,
           Class == "34.1" ~ 3, #NORGirl
           Class == "34.2" ~ 1,
           Class == "34.3" ~ 2,
           Class == "35.1" ~ 3, #RUSBoy
           Class == "35.2" ~ 1,
           Class == "35.3" ~ 2,
           Class == "36.1" ~ 3, #RUSGirl
           Class == "36.2" ~ 2,
           Class == "36.3" ~ 1,
           Class == "37.1" ~ 3, #SVNBoy
           Class == "37.2" ~ 2,
           Class == "37.3" ~ 1,
           Class == "38.1" ~ 1, #SVNGirl
           Class == "38.2" ~ 2,
           Class == "38.3" ~ 3,
           Class == "39.1" ~ 1, #SWEBoy
           Class == "39.2" ~ 3,
           Class == "39.3" ~ 2,
           Class == "40.1" ~ 2, #SWEGirl
           Class == "40.2" ~ 3,
           Class == "40.3" ~ 1,
           Class == "41.1" ~ 2, #TWNBoy
           Class == "41.2" ~ 3,
           Class == "41.3" ~ 1,
           Class == "42.1" ~ 2, #TWNGirl
           Class == "42.2" ~ 1,
           Class == "42.3" ~ 3
           #----
           ), levels = c("1", "2", "3"), labels = classes3M2))
#----C3
cntgn3 <- sort(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C3", c("cntgnd")])$cntgnd)
MGCntyGndr_C3cl3 <- MGCntryGndr$MGcntygndr_C3cl3_1CHet.out$parameters$probability.scale %>%
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>%
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>%
  mutate(Group = factor(sub('\\..*', '', Class),
                        levels = 1:length(cntgn3), labels = cntgn3),
         Group1 = factor(sub("Boy|Girl","",Group)),
         Group2 = factor(substr(Group, 4, nchar(as.character(Group)))),
         ClassN = factor(case_when(
           #-----
           Class == "1.1" ~ 1, #BFLBoy
           Class == "1.2" ~ 3,
           Class == "1.3" ~ 2,
           Class == "2.1" ~ 3, #BFLGirl
           Class == "2.2" ~ 2,
           Class == "2.3" ~ 1,
           Class == "3.1" ~ 3, #BRGBoy
           Class == "3.2" ~ 2,
           Class == "3.3" ~ 1,
           Class == "4.1" ~ 1, #BRGGirl
           Class == "4.2" ~ 2,
           Class == "4.3" ~ 3,
           Class == "5.1" ~ 2, #CHLBoy
           Class == "5.2" ~ 1,
           Class == "5.3" ~ 3,
           Class == "6.1" ~ 3, #CHLGirl
           Class == "6.2" ~ 1,
           Class == "6.3" ~ 2,
           Class == "7.1" ~ 3, #COLBoy
           Class == "7.2" ~ 2,
           Class == "7.3" ~ 1,
           Class == "8.1" ~ 2, #COLGirl
           Class == "8.2" ~ 3,
           Class == "8.3" ~ 1,
           Class == "9.1" ~ 1, #DNKBoy
           Class == "9.2" ~ 2,
           Class == "9.3" ~ 3,
           Class == "10.1" ~ 3, #DNKGirl
           Class == "10.2" ~ 2,
           Class == "10.3" ~ 1,
           Class == "11.1" ~ 2, #DOMBoy
           Class == "11.2" ~ 3,
           Class == "11.3" ~ 1,
           Class == "12.1" ~ 1, #DOMGirl
           Class == "12.2" ~ 3,
           Class == "12.3" ~ 2,
           Class == "13.1" ~ 1, #ESTBoy
           Class == "13.2" ~ 3,
           Class == "13.3" ~ 2,
           Class == "14.1" ~ 2, #ESTGirl
           Class == "14.2" ~ 1,
           Class == "14.3" ~ 3,
           Class == "15.1" ~ 2, #FINBoy
           Class == "15.2" ~ 3,
           Class == "15.3" ~ 1,
           Class == "16.1" ~ 3, #FINGirl
           Class == "16.2" ~ 1,
           Class == "16.3" ~ 2,
           Class == "17.1" ~ 1, #HKGBoy
           Class == "17.2" ~ 2,
           Class == "17.3" ~ 3,
           Class == "18.1" ~ 2, #HKGGirl
           Class == "18.2" ~ 3,
           Class == "18.3" ~ 1,
           Class == "19.1" ~ 1, #ITABoy
           Class == "19.2" ~ 3,
           Class == "19.3" ~ 2,
           Class == "20.1" ~ 1, #ITAGirl
           Class == "20.2" ~ 2,
           Class == "20.3" ~ 3,
           Class == "21.1" ~ 2, #KORBoy
           Class == "21.2" ~ 3,
           Class == "21.3" ~ 1,
           Class == "22.1" ~ 3, #KORGirl
           Class == "22.2" ~ 1,
           Class == "22.3" ~ 2,
           Class == "23.1" ~ 3, #LTUBoy
           Class == "23.2" ~ 2,
           Class == "23.3" ~ 1,
           Class == "24.1" ~ 1, #LTUGirl
           Class == "24.2" ~ 2,
           Class == "24.3" ~ 3,
           Class == "25.1" ~ 2, #LVABoy
           Class == "25.2" ~ 1,
           Class == "25.3" ~ 3,
           Class == "26.1" ~ 1, #LVAGirl
           Class == "26.2" ~ 3,
           Class == "26.3" ~ 2,
           Class == "27.1" ~ 1, #MEXBoy
           Class == "27.2" ~ 3,
           Class == "27.3" ~ 2,
           Class == "28.1" ~ 1, #MEXGirl
           Class == "28.2" ~ 3,
           Class == "28.3" ~ 2,
           Class == "29.1" ~ 2, #MLTBoy
           Class == "29.2" ~ 3,
           Class == "29.3" ~ 1,
           Class == "30.1" ~ 3, #MLTGirl
           Class == "30.2" ~ 1,
           Class == "30.3" ~ 2,
           Class == "31.1" ~ 1, #NLDBoy
           Class == "31.2" ~ 2,
           Class == "31.3" ~ 3,
           Class == "32.1" ~ 2, #NLDGirl
           Class == "32.2" ~ 3,
           Class == "32.3" ~ 1,
           Class == "33.1" ~ 1, #NORBoy
           Class == "33.2" ~ 2,
           Class == "33.3" ~ 3,
           Class == "34.1" ~ 1, #NORGirl
           Class == "34.2" ~ 3,
           Class == "34.3" ~ 2,
           Class == "35.1" ~ 2, #RUSBoy
           Class == "35.2" ~ 1,
           Class == "35.3" ~ 3,
           Class == "36.1" ~ 2, #RUSGirl
           Class == "36.2" ~ 1,
           Class == "36.3" ~ 3,
           Class == "37.1" ~ 1, #SVNBoy
           Class == "37.2" ~ 2,
           Class == "37.3" ~ 3,
           Class == "38.1" ~ 2, #SVNGirl
           Class == "38.2" ~ 1,
           Class == "38.3" ~ 3,
           Class == "39.1" ~ 3, #SWEBoy
           Class == "39.2" ~ 2,
           Class == "39.3" ~ 1,
           Class == "40.1" ~ 3, #SWEGirl
           Class == "40.2" ~ 1,
           Class == "40.3" ~ 2,
           Class == "41.1" ~ 1, #TWNBoy
           Class == "41.2" ~ 3,
           Class == "41.3" ~ 2,
           Class == "42.1" ~ 2, #TWNGirl
           Class == "42.2" ~ 3,
           Class == "42.3" ~ 1
           #----
           ), levels = c("1", "2", "3"), labels = classes3M2))

MGCntyGndr3 <- rbind(cbind(Cycle = "1999", MGCntyGndr_C1cl3),
                     cbind(Cycle = "2009", MGCntyGndr_C2cl3),
                     cbind(Cycle = "2016", MGCntyGndr_C3cl3)
                     )
#---
graphclass(MGCntyGndr3[MGCntyGndr3$Group %in% cntgn3[1:2],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Complete Heterogeneity")
graphclass(MGCntyGndr3[MGCntyGndr3$Group %in% cntgn3[3:4],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Complete Heterogeneity")
graphclass(MGCntyGndr3[MGCntyGndr3$Group %in% cntgn3[5:6],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Complete Heterogeneity")
graphclass(MGCntyGndr3[MGCntyGndr3$Group %in% cntgn3[7:8],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Complete Heterogeneity")
graphclass(MGCntyGndr3[MGCntyGndr3$Group %in% cntgn3[9:10],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Complete Heterogeneity")
graphclass(MGCntyGndr3[MGCntyGndr3$Group %in% cntgn3[11:12],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Complete Heterogeneity")
graphclass(MGCntyGndr3[MGCntyGndr3$Group %in% cntgn3[13:14],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Complete Heterogeneity")
graphclass(MGCntyGndr3[MGCntyGndr3$Group %in% cntgn3[15:16],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Complete Heterogeneity")
graphclass(MGCntyGndr3[MGCntyGndr3$Group %in% cntgn3[17:18],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Complete Heterogeneity")
graphclass(MGCntyGndr3[MGCntyGndr3$Group %in% cntgn3[19:20],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Complete Heterogeneity")
graphclass(MGCntyGndr3[MGCntyGndr3$Group %in% cntgn3[21:22],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Complete Heterogeneity")
graphclass(MGCntyGndr3[MGCntyGndr3$Group %in% cntgn3[23:24],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Complete Heterogeneity")
graphclass(MGCntyGndr3[MGCntyGndr3$Group %in% cntgn3[25:26],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Complete Heterogeneity")
graphclass(MGCntyGndr3[MGCntyGndr3$Group %in% cntgn3[27:28],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Complete Heterogeneity")
graphclass(MGCntyGndr3[MGCntyGndr3$Group %in% cntgn3[29:30],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Complete Heterogeneity")
graphclass(MGCntyGndr3[MGCntyGndr3$Group %in% cntgn3[31:32],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Complete Heterogeneity")
graphclass(MGCntyGndr3[MGCntyGndr3$Group %in% cntgn3[33:34],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Complete Heterogeneity")
graphclass(MGCntyGndr3[MGCntyGndr3$Group %in% cntgn3[35:36],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Complete Heterogeneity")
graphclass(MGCntyGndr3[MGCntyGndr3$Group %in% cntgn3[37:38],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Complete Heterogeneity")
graphclass(MGCntyGndr3[MGCntyGndr3$Group %in% cntgn3[39:40],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Complete Heterogeneity")
graphclass(MGCntyGndr3[MGCntyGndr3$Group %in% cntgn3[41:42],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Complete Heterogeneity")

lcicntgn <- VarClass(MGCntyGndr3)
for(j in sort(as.character(unique(lcicntgn$Group1)))) {
  lcicntgn %>% group_by(Cycle, ClassN, Group, param) %>% filter(category == 2 & Group1 == j) %>%
    select(Cycle, param, ClassN, Group, value) %>%
    mutate(value = cell_spec(value, color = ifelse(value > 0.5, "blue", "red"))) %>%
    reshape2::dcast(param  + Cycle ~ Group + ClassN ) %>%
    kbl(caption = paste("Probabilities to Agree each item by Class and Gender Complete Heterogeneity - ", j), booktabs = TRUE, longtable = TRUE,
        align = "llrrrrrr", row.names = FALSE, digits = 3, col.names = c("Item", "Year", classes3M2, classes3M2), escape = FALSE) %>%
    kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"),
                  position = "left", font_size = 10, full_width = FALSE) %>%
    column_spec(1, width = "14em") %>%
    column_spec(2, width = "2em") %>%
    column_spec(3:8, width = "4em") %>%
    collapse_rows(1, valign = "top") %>%
    add_header_above(c(" "=1," "=1, "Boy" = 3, "Girl" = 3)) %>%
    print()
  cat('\n')
  cat('\n')
}

#Extracting patterns
sizeMGcntygndr3 <- full_join(
  full_join(MGCntryGndr$MGcntygndr_C1cl3_1CHet.out$class_counts$modelEstimated.patterns %>%
              rename_with(~ c("1999", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>%
              mutate(Class = paste(Group,C,sep="."),
                     Group = factor(Group, levels = 1:length(cntgn1), labels = cntgn1),
                     Group1 = factor(sub("Boy|Girl","",Group)),
                     Group2 = factor(substr(Group, 4, nchar(as.character(Group))))) %>%
              mutate(ClassN = factor(
                case_when(
                  #----
                          Class == "1.1" ~ 2, #BGRBoy
                          Class == "1.2" ~ 3,
                          Class == "1.3" ~ 1,
                          Class == "2.1" ~ 1, #BGRGirl
                          Class == "2.2" ~ 2,
                          Class == "2.3" ~ 3,
                          Class == "3.1" ~ 1, #CHLBoy
                          Class == "3.2" ~ 2,
                          Class == "3.3" ~ 3,
                          Class == "4.1" ~ 2, #CHLGirl
                          Class == "4.2" ~ 3,
                          Class == "4.3" ~ 1,
                          Class == "5.1" ~ 2, #COLBoy
                          Class == "5.2" ~ 1,
                          Class == "5.3" ~ 3,
                          Class == "6.1" ~ 1, #COLGirl
                          Class == "6.2" ~ 3,
                          Class == "6.3" ~ 2,
                          Class == "7.1" ~ 1, #DKNBoy
                          Class == "7.2" ~ 3,
                          Class == "7.3" ~ 2,
                          Class == "8.1" ~ 3, #DKNGirl
                          Class == "8.2" ~ 2,
                          Class == "8.3" ~ 1,
                          Class == "9.1" ~ 1, #ESTBoy
                          Class == "9.2" ~ 2,
                          Class == "9.3" ~ 3,
                          Class == "10.1" ~ 2, #ESTGirl
                          Class == "10.2" ~ 3,
                          Class == "10.3" ~ 1,
                          Class == "11.1" ~ 3, #FINBoy
                          Class == "11.2" ~ 1,
                          Class == "11.3" ~ 2,
                          Class == "12.1" ~ 2, #FINGirl
                          Class == "12.2" ~ 3,
                          Class == "12.3" ~ 1,
                          Class == "13.1" ~ 2, #HKGBoy
                          Class == "13.2" ~ 3,
                          Class == "13.3" ~ 1,
                          Class == "14.1" ~ 3, #HKGGirl
                          Class == "14.2" ~ 2,
                          Class == "14.3" ~ 1,
                          Class == "15.1" ~ 1, #ITABoy
                          Class == "15.2" ~ 3,
                          Class == "15.3" ~ 2,
                          Class == "16.1" ~ 1, #ITAGirl
                          Class == "16.2" ~ 2,
                          Class == "16.3" ~ 3,
                          Class == "17.1" ~ 3, #LTUBoy
                          Class == "17.2" ~ 1,
                          Class == "17.3" ~ 2,
                          Class == "18.1" ~ 1, #LTUGirl
                          Class == "18.2" ~ 2,
                          Class == "18.3" ~ 3,
                          Class == "19.1" ~ 2, #LVABoy
                          Class == "19.2" ~ 3,
                          Class == "19.3" ~ 1,
                          Class == "20.1" ~ 1, #LVAGirl
                          Class == "20.2" ~ 2,
                          Class == "20.3" ~ 3,
                          Class == "21.1" ~ 3, #NORBoy
                          Class == "21.2" ~ 1,
                          Class == "21.3" ~ 2,
                          Class == "22.1" ~ 2, #NORGirl
                          Class == "22.2" ~ 1,
                          Class == "22.3" ~ 3,
                          Class == "23.1" ~ 3, #RUSBoy
                          Class == "23.2" ~ 1,
                          Class == "23.3" ~ 2,
                          Class == "24.1" ~ 2, #RUSGirl
                          Class == "24.2" ~ 3,
                          Class == "24.3" ~ 1,
                          Class == "25.1" ~ 1, #SVNBoy
                          Class == "25.2" ~ 3,
                          Class == "25.3" ~ 2,
                          Class == "26.1" ~ 2, #SVNGirl
                          Class == "26.2" ~ 1,
                          Class == "26.3" ~ 3,
                          Class == "27.1" ~ 2, #SWEBoy
                          Class == "27.2" ~ 1,
                          Class == "27.3" ~ 3,
                          Class == "28.1" ~ 3, #SWEGirl
                          Class == "28.2" ~ 1,
                          Class == "28.3" ~ 2
                #----
                ), levels = 1:3, labels = classes3M2)) %>% dplyr::select(-proportion, -C, -Class) ,
            MGCntryGndr$MGcntygndr_C2cl3_1CHet.out$class_counts$modelEstimated.patterns %>%
              rename_with(~ c("2009", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>%
              mutate(Class = paste(Group,C,sep="."),
                     Group = factor(Group, levels = 1:length(cntgn2), labels = cntgn2),
                     Group1 = factor(sub("Boy|Girl","",Group)),
                     Group2 = factor(substr(Group, 4, nchar(as.character(Group))))) %>%
              mutate(ClassN = factor(case_when(
                #-----
                Class == "1.1" ~ 3, #BFLBoy
                Class == "1.2" ~ 1,
                Class == "1.3" ~ 2,
                Class == "2.1" ~ 2, #BFLGirl
                Class == "2.2" ~ 1,
                Class == "2.3" ~ 3,
                Class == "3.1" ~ 2, #BRGBoy
                Class == "3.2" ~ 3,
                Class == "3.3" ~ 1,
                Class == "4.1" ~ 2, #BRGGirl
                Class == "4.2" ~ 3,
                Class == "4.3" ~ 1,
                Class == "5.1" ~ 1, #CHLBoy
                Class == "5.2" ~ 2,
                Class == "5.3" ~ 3,
                Class == "6.1" ~ 2, #CHLGirl
                Class == "6.2" ~ 1,
                Class == "6.3" ~ 3,
                Class == "7.1" ~ 1, #COLBoy
                Class == "7.2" ~ 3,
                Class == "7.3" ~ 2,
                Class == "8.1" ~ 2, #COLGirl
                Class == "8.2" ~ 3,
                Class == "8.3" ~ 1,
                Class == "9.1" ~ 3, #DNKBoy
                Class == "9.2" ~ 2,
                Class == "9.3" ~ 1,
                Class == "10.1" ~ 2, #DNKGirl
                Class == "10.2" ~ 1,
                Class == "10.3" ~ 3,
                Class == "11.1" ~ 3, #DOMBoy
                Class == "11.2" ~ 2,
                Class == "11.3" ~ 1,
                Class == "12.1" ~ 2, #DOMGirl
                Class == "12.2" ~ 1,
                Class == "12.3" ~ 3,
                Class == "13.1" ~ 2, #ESTBoy
                Class == "13.2" ~ 3,
                Class == "13.3" ~ 1,
                Class == "14.1" ~ 3, #ESTGirl
                Class == "14.2" ~ 2,
                Class == "14.3" ~ 1,
                Class == "15.1" ~ 3, #FINBoy
                Class == "15.2" ~ 1,
                Class == "15.3" ~ 2,
                Class == "16.1" ~ 1, #FINGirl
                Class == "16.2" ~ 3,
                Class == "16.3" ~ 2,
                Class == "17.1" ~ 1, #HKGBoy
                Class == "17.2" ~ 3,
                Class == "17.3" ~ 2,
                Class == "18.1" ~ 2, #HKGGirl
                Class == "18.2" ~ 1,
                Class == "18.3" ~ 3,
                Class == "19.1" ~ 2, #ITABoy
                Class == "19.2" ~ 1,
                Class == "19.3" ~ 3,
                Class == "20.1" ~ 2, #ITAGirl
                Class == "20.2" ~ 1,
                Class == "20.3" ~ 3,
                Class == "21.1" ~ 1, #KORBoy
                Class == "21.2" ~ 2,
                Class == "21.3" ~ 3,
                Class == "22.1" ~ 2, #KORGirl
                Class == "22.2" ~ 3,
                Class == "22.3" ~ 1,
                Class == "23.1" ~ 2, #LTUBoy
                Class == "23.2" ~ 1,
                Class == "23.3" ~ 3,
                Class == "24.1" ~ 2, #LTUGirl
                Class == "24.2" ~ 1,
                Class == "24.3" ~ 3,
                Class == "25.1" ~ 3, #LVABoy
                Class == "25.2" ~ 2,
                Class == "25.3" ~ 1,
                Class == "26.1" ~ 1, #LVAGirl
                Class == "26.2" ~ 2,
                Class == "26.3" ~ 3,
                Class == "27.1" ~ 3, #MEXBoy
                Class == "27.2" ~ 2,
                Class == "27.3" ~ 1,
                Class == "28.1" ~ 1, #MEXGirl
                Class == "28.2" ~ 2,
                Class == "28.3" ~ 3,
                Class == "29.1" ~ 3, #MLTBoy
                Class == "29.2" ~ 1,
                Class == "29.3" ~ 2,
                Class == "30.1" ~ 1, #MLTGirl
                Class == "30.2" ~ 3,
                Class == "30.3" ~ 2,
                Class == "31.1" ~ 2, #NLDBoy
                Class == "31.2" ~ 1,
                Class == "31.3" ~ 3,
                Class == "32.1" ~ 1, #NLDGirl
                Class == "32.2" ~ 3,
                Class == "32.3" ~ 2,
                Class == "33.1" ~ 1, #NORBoy
                Class == "33.2" ~ 2,
                Class == "33.3" ~ 3,
                Class == "34.1" ~ 3, #NORGirl
                Class == "34.2" ~ 1,
                Class == "34.3" ~ 2,
                Class == "35.1" ~ 3, #RUSBoy
                Class == "35.2" ~ 1,
                Class == "35.3" ~ 2,
                Class == "36.1" ~ 3, #RUSGirl
                Class == "36.2" ~ 2,
                Class == "36.3" ~ 1,
                Class == "37.1" ~ 3, #SVNBoy
                Class == "37.2" ~ 2,
                Class == "37.3" ~ 1,
                Class == "38.1" ~ 1, #SVNGirl
                Class == "38.2" ~ 2,
                Class == "38.3" ~ 3,
                Class == "39.1" ~ 1, #SWEBoy
                Class == "39.2" ~ 3,
                Class == "39.3" ~ 2,
                Class == "40.1" ~ 2, #SWEGirl
                Class == "40.2" ~ 3,
                Class == "40.3" ~ 1,
                Class == "41.1" ~ 2, #TWNBoy
                Class == "41.2" ~ 3,
                Class == "41.3" ~ 1,
                Class == "42.1" ~ 2, #TWNGirl
                Class == "42.2" ~ 1,
                Class == "42.3" ~ 3
                #----
                ), levels = 1:3, labels = classes3M2)) %>% dplyr::select(-proportion, -C, -Class), by = c("Group","Group1","Group2","ClassN")),
  MGCntryGndr$MGcntygndr_C3cl3_1CHet.out$class_counts$modelEstimated.patterns %>%
    rename_with(~ c("2016", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>%
    mutate(Class = paste(Group,C,sep="."),
           Group = factor(Group, levels = 1:length(cntgn3), labels = cntgn3),
           Group1 = factor(sub("Boy|Girl","",Group)),
           Group2 = factor(substr(Group, 4, nchar(as.character(Group))))) %>%
    mutate(ClassN = factor(case_when(
      #-----
      Class == "1.1" ~ 1, #BFLBoy
      Class == "1.2" ~ 3,
      Class == "1.3" ~ 2,
      Class == "2.1" ~ 3, #BFLGirl
      Class == "2.2" ~ 2,
      Class == "2.3" ~ 1,
      Class == "3.1" ~ 3, #BRGBoy
      Class == "3.2" ~ 2,
      Class == "3.3" ~ 1,
      Class == "4.1" ~ 1, #BRGGirl
      Class == "4.2" ~ 2,
      Class == "4.3" ~ 3,
      Class == "5.1" ~ 2, #CHLBoy
      Class == "5.2" ~ 1,
      Class == "5.3" ~ 3,
      Class == "6.1" ~ 3, #CHLGirl
      Class == "6.2" ~ 1,
      Class == "6.3" ~ 2,
      Class == "7.1" ~ 3, #COLBoy
      Class == "7.2" ~ 2,
      Class == "7.3" ~ 1,
      Class == "8.1" ~ 2, #COLGirl
      Class == "8.2" ~ 3,
      Class == "8.3" ~ 1,
      Class == "9.1" ~ 1, #DNKBoy
      Class == "9.2" ~ 2,
      Class == "9.3" ~ 3,
      Class == "10.1" ~ 3, #DNKGirl
      Class == "10.2" ~ 2,
      Class == "10.3" ~ 1,
      Class == "11.1" ~ 2, #DOMBoy
      Class == "11.2" ~ 3,
      Class == "11.3" ~ 1,
      Class == "12.1" ~ 1, #DOMGirl
      Class == "12.2" ~ 3,
      Class == "12.3" ~ 2,
      Class == "13.1" ~ 1, #ESTBoy
      Class == "13.2" ~ 3,
      Class == "13.3" ~ 2,
      Class == "14.1" ~ 2, #ESTGirl
      Class == "14.2" ~ 1,
      Class == "14.3" ~ 3,
      Class == "15.1" ~ 2, #FINBoy
      Class == "15.2" ~ 3,
      Class == "15.3" ~ 1,
      Class == "16.1" ~ 3, #FINGirl
      Class == "16.2" ~ 1,
      Class == "16.3" ~ 2,
      Class == "17.1" ~ 1, #HKGBoy
      Class == "17.2" ~ 2,
      Class == "17.3" ~ 3,
      Class == "18.1" ~ 2, #HKGGirl
      Class == "18.2" ~ 3,
      Class == "18.3" ~ 1,
      Class == "19.1" ~ 1, #ITABoy
      Class == "19.2" ~ 3,
      Class == "19.3" ~ 2,
      Class == "20.1" ~ 1, #ITAGirl
      Class == "20.2" ~ 2,
      Class == "20.3" ~ 3,
      Class == "21.1" ~ 2, #KORBoy
      Class == "21.2" ~ 3,
      Class == "21.3" ~ 1,
      Class == "22.1" ~ 3, #KORGirl
      Class == "22.2" ~ 1,
      Class == "22.3" ~ 2,
      Class == "23.1" ~ 3, #LTUBoy
      Class == "23.2" ~ 2,
      Class == "23.3" ~ 1,
      Class == "24.1" ~ 1, #LTUGirl
      Class == "24.2" ~ 2,
      Class == "24.3" ~ 3,
      Class == "25.1" ~ 2, #LVABoy
      Class == "25.2" ~ 1,
      Class == "25.3" ~ 3,
      Class == "26.1" ~ 1, #LVAGirl
      Class == "26.2" ~ 3,
      Class == "26.3" ~ 2,
      Class == "27.1" ~ 1, #MEXBoy
      Class == "27.2" ~ 3,
      Class == "27.3" ~ 2,
      Class == "28.1" ~ 1, #MEXGirl
      Class == "28.2" ~ 3,
      Class == "28.3" ~ 2,
      Class == "29.1" ~ 2, #MLTBoy
      Class == "29.2" ~ 3,
      Class == "29.3" ~ 1,
      Class == "30.1" ~ 3, #MLTGirl
      Class == "30.2" ~ 1,
      Class == "30.3" ~ 2,
      Class == "31.1" ~ 1, #NLDBoy
      Class == "31.2" ~ 2,
      Class == "31.3" ~ 3,
      Class == "32.1" ~ 2, #NLDGirl
      Class == "32.2" ~ 3,
      Class == "32.3" ~ 1,
      Class == "33.1" ~ 1, #NORBoy
      Class == "33.2" ~ 2,
      Class == "33.3" ~ 3,
      Class == "34.1" ~ 1, #NORGirl
      Class == "34.2" ~ 3,
      Class == "34.3" ~ 2,
      Class == "35.1" ~ 2, #RUSBoy
      Class == "35.2" ~ 1,
      Class == "35.3" ~ 3,
      Class == "36.1" ~ 2, #RUSGirl
      Class == "36.2" ~ 1,
      Class == "36.3" ~ 3,
      Class == "37.1" ~ 1, #SVNBoy
      Class == "37.2" ~ 2,
      Class == "37.3" ~ 3,
      Class == "38.1" ~ 2, #SVNGirl
      Class == "38.2" ~ 1,
      Class == "38.3" ~ 3,
      Class == "39.1" ~ 3, #SWEBoy
      Class == "39.2" ~ 2,
      Class == "39.3" ~ 1,
      Class == "40.1" ~ 3, #SWEGirl
      Class == "40.2" ~ 1,
      Class == "40.3" ~ 2,
      Class == "41.1" ~ 1, #TWNBoy
      Class == "41.2" ~ 3,
      Class == "41.3" ~ 2,
      Class == "42.1" ~ 2, #TWNGirl
      Class == "42.2" ~ 3,
      Class == "42.3" ~ 1
      #----
      ), levels = 1:3, labels = classes3M2)) %>% dplyr::select(-proportion, -C, -Class) , by = c("Group","Group1","Group2","ClassN"))


sizeMGcntygndr3 <- sizeMGcntygndr3  %>%
  select(Group, Group1, Group2, ClassN, `1999`, `2009`, `2016`) %>%
  reshape2::melt(id.vars = c("Group", "Group1", "Group2","ClassN"), variable.name = "Cycle") %>%
  arrange(Group1, Group2, Cycle) %>%
  group_by(Group1, Group2, Cycle) %>% #add Group2 if proportion by gender and not by country
  mutate(countT= sum(value, na.rm = TRUE)) %>%
  group_by(ClassN) %>%
  mutate(per=value/countT) %>% dplyr::select(Group, Group1, Group2, Cycle, ClassN, per)


HighProbMG(MGCntyGndr3, sizeMGcntygndr3, orden = c(5,2,1,3,4,6), n = 1:2, title = "Probabilities of Agree for each class in Country*Gender \nMultigroup analysis Complete Heterogeneity by Cycle")
HighProbMG(MGCntyGndr3, sizeMGcntygndr3, orden = c(5,2,1,3,4,6), n = c(11:12,41:42))
HighProbMG(MGCntyGndr3, sizeMGcntygndr3, orden = c(5,2,1,3,4,6), n = 3:6)
HighProbMG(MGCntyGndr3, sizeMGcntygndr3, orden = c(5,2,1,3,4,6), n = 7:10)
HighProbMG(MGCntyGndr3, sizeMGcntygndr3, orden = c(5,2,1,3,4,6), n = 13:16)
HighProbMG(MGCntyGndr3, sizeMGcntygndr3, orden = c(5,2,1,3,4,6), n = 17:20)
HighProbMG(MGCntyGndr3, sizeMGcntygndr3, orden = c(5,2,1,3,4,6), n = c(21:22,27:28))
HighProbMG(MGCntyGndr3, sizeMGcntygndr3, orden = c(5,2,1,3,4,6), n = 23:26)
HighProbMG(MGCntyGndr3, sizeMGcntygndr3, orden = c(5,2,1,3,4,6), n = 29:32)
HighProbMG(MGCntyGndr3, sizeMGcntygndr3, orden = c(5,2,1,3,4,6), n = 33:36)
HighProbMG(MGCntyGndr3, sizeMGcntygndr3, orden = c(5,2,1,3,4,6), n = 37:40)

sizeMGcntygndr3 %>%
  reshape2::dcast(Group1 + Cycle ~ Group2 + ClassN) %>% arrange(as.character(Group1)) %>%
  kbl(caption = "Size three classes MG Country Gender Complete heterogeneity", booktabs = TRUE, longtable = TRUE, row.names = FALSE, digits = 3,
      col.names = c("Country", "Year", classes3M2, classes3M2)) %>%
  kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"),
                position = "left", font_size = 11, full_width = FALSE) %>%
  column_spec(1:2, width = "3em") %>%
  column_spec(3:8, width = "4em") %>%
  collapse_rows(1) %>%
  add_header_above(c(" " = 1, " " = 1, "Boy" = 3, "Girl" = 3)) %>%
  print()
cat('\n')
cat('\n')


# ------ 3C MG Country Gender PH1 ----
cat('#### Partial Homogeneity - one class equal across Country and Gender \n')
cat('\n')
cat('\n')
warn_MGCntryGndrPH1 <- list(C1 = MGCntryGndr$MGCntyGndr_C1cl3PH1.out$warnings,
                         C2 = MGCntryGndr$MGCntyGndr_C2cl3PH1.out$warnings,
                         C3 = MGCntryGndr$MGCntyGndr_C3cl3PH1.out$warnings)

cat('\n')
cat('\n')
wr <- names(warn_MGCntryGndrPH1[grep("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED",warn_MGCntryGndrPH1)])
if (length(wr) == 0) print("ALL BEST LOGLIKELIHOOD VALUES WERE REPLICATED") else
  cat("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED for the following cases:  \n", wr)
cat('\n')
cat('\n')

#----C1
cntgn1 <- sort(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C1", c("cntgnd")])$cntgnd)
cnt1 <- sort(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C1", c("COUNTRY")])$COUNTRY)
MGCntyGndr_C1cl3PH1 <- MGCntryGndr$MGCntyGndr_C1cl3PH1.out$parameters$probability.scale %>%
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>%
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>%
  mutate(Group = factor(sub('\\..*', '', Class),
                        levels = 1:length(cntgn1), labels = cntgn1),
         Group1 = factor(sub("Boy|Girl","",Group)),
         Group2 = factor(substr(Group, 4, nchar(as.character(Group)))),
         Class1 = sub('.{1,2}\\.', '', Class),
         ClassN = factor(
           case_when(
             Class1 == "1" ~ 3,
             Class1 == "2" ~ 1,
             Class1 == "3" ~ 2),
           levels = c("1", "2", "3"), labels = classes3M2))
#----C2
cntgn2 <- sort(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C2", c("cntgnd")])$cntgnd)
MGCntyGndr_C2cl3PH1 <- MGCntryGndr$MGCntyGndr_C2cl3PH1.out$parameters$probability.scale %>%
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>%
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>%
  mutate(Group = factor(sub('\\..*', '', Class),
                        levels = 1:length(cntgn2), labels = cntgn2),
         Group1 = factor(sub("Boy|Girl","",Group)),
         Group2 = factor(substr(Group, 4, nchar(as.character(Group)))),
         
         Class1 = sub('.{1,2}\\.', '', Class),
         ClassN = factor(
           case_when(
             Class1 == "1" ~ 3,
             Class1 == "2" ~ 1,
             Class1 == "3" ~ 2),
           levels = c("1", "2", "3"), labels = classes3M2))
#----C3
cntgn3 <- sort(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C3", c("cntgnd")])$cntgnd)
MGCntyGndr_C3cl3PH1 <- MGCntryGndr$MGCntyGndr_C3cl3PH1.out$parameters$probability.scale %>%
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>%
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>%
  mutate(Group = factor(sub('\\..*', '', Class),
                        levels = 1:length(cntgn3), labels = cntgn3),
         Group1 = factor(sub("Boy|Girl","",Group)),
         Group2 = factor(substr(Group, 4, nchar(as.character(Group)))),
         
         Class1 = sub('.{1,2}\\.', '', Class),
         ClassN = factor(
           case_when(
             Class1 == "1" ~ 3,
             Class1 == "2" ~ 1,
             Class1 == "3" ~ 2),
           levels = c("1", "2", "3"), labels = classes3M2))

MGCntyGndr3PH1 <- rbind(cbind(Cycle = "1999", MGCntyGndr_C1cl3PH1),
                     cbind(Cycle = "2009", MGCntyGndr_C2cl3PH1),
                     cbind(Cycle = "2016", MGCntyGndr_C3cl3PH1)
)
#---
graphclass(MGCntyGndr3PH1[MGCntyGndr3PH1$Group %in% cntgn3[1:2],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Partial homogeneity 1 class")
graphclass(MGCntyGndr3PH1[MGCntyGndr3PH1$Group %in% cntgn3[3:4],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Partial homogeneity 1 class")
graphclass(MGCntyGndr3PH1[MGCntyGndr3PH1$Group %in% cntgn3[5:6],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Partial homogeneity 1 class")
graphclass(MGCntyGndr3PH1[MGCntyGndr3PH1$Group %in% cntgn3[7:8],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Partial homogeneity 1 class")
graphclass(MGCntyGndr3PH1[MGCntyGndr3PH1$Group %in% cntgn3[9:10],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Partial homogeneity 1 class")
graphclass(MGCntyGndr3PH1[MGCntyGndr3PH1$Group %in% cntgn3[11:12],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Partial homogeneity 1 class")
graphclass(MGCntyGndr3PH1[MGCntyGndr3PH1$Group %in% cntgn3[13:14],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Partial homogeneity 1 class")
graphclass(MGCntyGndr3PH1[MGCntyGndr3PH1$Group %in% cntgn3[15:16],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Partial homogeneity 1 class")
graphclass(MGCntyGndr3PH1[MGCntyGndr3PH1$Group %in% cntgn3[17:18],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Partial homogeneity 1 class")
graphclass(MGCntyGndr3PH1[MGCntyGndr3PH1$Group %in% cntgn3[19:20],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Partial homogeneity 1 class")
graphclass(MGCntyGndr3PH1[MGCntyGndr3PH1$Group %in% cntgn3[21:22],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Partial homogeneity 1 class")
graphclass(MGCntyGndr3PH1[MGCntyGndr3PH1$Group %in% cntgn3[23:24],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Partial homogeneity 1 class")
graphclass(MGCntyGndr3PH1[MGCntyGndr3PH1$Group %in% cntgn3[25:26],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Partial homogeneity 1 class")
graphclass(MGCntyGndr3PH1[MGCntyGndr3PH1$Group %in% cntgn3[27:28],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Partial homogeneity 1 class")
graphclass(MGCntyGndr3PH1[MGCntyGndr3PH1$Group %in% cntgn3[29:30],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Partial homogeneity 1 class")
graphclass(MGCntyGndr3PH1[MGCntyGndr3PH1$Group %in% cntgn3[31:32],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Partial homogeneity 1 class")
graphclass(MGCntyGndr3PH1[MGCntyGndr3PH1$Group %in% cntgn3[33:34],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Partial homogeneity 1 class")
graphclass(MGCntyGndr3PH1[MGCntyGndr3PH1$Group %in% cntgn3[35:36],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Partial homogeneity 1 class")
graphclass(MGCntyGndr3PH1[MGCntyGndr3PH1$Group %in% cntgn3[37:38],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Partial homogeneity 1 class")
graphclass(MGCntyGndr3PH1[MGCntyGndr3PH1$Group %in% cntgn3[39:40],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Partial homogeneity 1 class")
graphclass(MGCntyGndr3PH1[MGCntyGndr3PH1$Group %in% cntgn3[41:42],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Partial homogeneity 1 class")

lcicntgn <- VarClass(MGCntyGndr3PH1)
for(j in sort(as.character(unique(lcicntgn$Group1)))) {
  lcicntgn %>% group_by(Cycle, ClassN, Group, param) %>% filter(category == 2 & Group1 == j) %>%
    select(Cycle, param, ClassN, Group, value) %>%
    mutate(value = cell_spec(value, color = ifelse(value > 0.5, "blue", "red"))) %>%
    reshape2::dcast(param  + Cycle ~ Group + ClassN ) %>%
    kbl(caption = paste("Probabilities to Agree each item by Class and Gender Partial Homogeneity 1 class - ", j), booktabs = TRUE, longtable = TRUE,
        align = "llrrrrrr", row.names = FALSE, digits = 3, col.names = c("Item", "Year", classes3M2, classes3M2), escape = FALSE) %>%
    kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"),
                  position = "left", font_size = 10, full_width = FALSE) %>%
    column_spec(1, width = "14em") %>%
    column_spec(2, width = "2em") %>%
    column_spec(3:8, width = "4em") %>%
    collapse_rows(1, valign = "top") %>%
    add_header_above(c(" "=1," "=1, "Boy" = 3, "Girl" = 3)) %>%
    print()
  cat('\n')
  cat('\n')
}

#Extracting patterns
sizeMGcntygndr3PH1 <- full_join(
  full_join(MGCntryGndr$MGCntyGndr_C1cl3PH1.out$class_counts$modelEstimated.patterns %>%
              rename_with(~ c("1999", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>%
              mutate(Class = paste(Group,C,sep="."),
                     Group = factor(Group, levels = 1:length(cntgn1), labels = cntgn1),
                     Group1 = factor(sub("Boy|Girl","",Group)),
                     Group2 = factor(substr(Group, 4, nchar(as.character(Group)))),
                     Class1 = sub('.{1,2}\\.', '', Class)) %>% 
              mutate(ClassN = factor(
                case_when(
                  Class1 == "1" ~ 3,
                  Class1 == "2" ~ 1,
                  Class1 == "3" ~ 2
                ), levels = 1:3, labels = classes3M2)) %>% dplyr::select(-proportion, -C, -Class) ,
            MGCntryGndr$MGCntyGndr_C2cl3PH1.out$class_counts$modelEstimated.patterns %>%
              rename_with(~ c("2009", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>%
              mutate(Class = paste(Group,C,sep="."),
                     Group = factor(Group, levels = 1:length(cntgn2), labels = cntgn2),
                     Group1 = factor(sub("Boy|Girl","",Group)),
                     Group2 = factor(substr(Group, 4, nchar(as.character(Group)))),
                     Class1 = sub('.{1,2}\\.', '', Class)) %>% 
              mutate(ClassN = factor(case_when(
                Class1 == "1" ~ 3,
                Class1 == "2" ~ 1,
                Class1 == "3" ~ 2
              ), levels = 1:3, labels = classes3M2)) %>% dplyr::select(-proportion, -C, -Class), by = c("Group","Group1","Group2","ClassN")),
  MGCntryGndr$MGCntyGndr_C3cl3PH1.out$class_counts$modelEstimated.patterns %>%
    rename_with(~ c("2016", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>%
    mutate(Class = paste(Group,C,sep="."),
           Group = factor(Group, levels = 1:length(cntgn3), labels = cntgn3),
           Group1 = factor(sub("Boy|Girl","",Group)),
           Group2 = factor(substr(Group, 4, nchar(as.character(Group)))),
           Class1 = sub('.{1,2}\\.', '', Class)) %>% 
    mutate(ClassN = factor(case_when(
      Class1 == "1" ~ 3,
      Class1 == "2" ~ 1,
      Class1 == "3" ~ 2
    ), levels = 1:3, labels = classes3M2)) %>% dplyr::select(-proportion, -C, -Class) , by = c("Group","Group1","Group2","ClassN"))


sizeMGcntygndr3PH1 <- sizeMGcntygndr3PH1  %>%
  select(Group, Group1, Group2, ClassN, `1999`, `2009`, `2016`) %>%
  reshape2::melt(id.vars = c("Group", "Group1", "Group2","ClassN"), variable.name = "Cycle") %>%
  arrange(Group1, Group2, Cycle) %>%
  group_by(Group1, Group2, Cycle) %>% #add Group2 if proportion by gender and not by country
  mutate(countT= sum(value, na.rm = TRUE)) %>%
  group_by(ClassN) %>%
  mutate(per=value/countT) %>% dplyr::select(Group, Group1, Group2, Cycle, ClassN, per)


HighProbMG(MGCntyGndr3PH1, sizeMGcntygndr3PH1, orden = c(5,2,1,3,4,6), n = 1:2, title = "Probabilities of Agree for each class in Country*Gender \nMultigroup analysis Partial Homogeneity 1 class by Cycle")
HighProbMG(MGCntyGndr3PH1, sizeMGcntygndr3PH1, orden = c(5,2,1,3,4,6), n = c(11:12,41:42))
HighProbMG(MGCntyGndr3PH1, sizeMGcntygndr3PH1, orden = c(5,2,1,3,4,6), n = 3:6)
HighProbMG(MGCntyGndr3PH1, sizeMGcntygndr3PH1, orden = c(5,2,1,3,4,6), n = 7:10)
HighProbMG(MGCntyGndr3PH1, sizeMGcntygndr3PH1, orden = c(5,2,1,3,4,6), n = 13:16)
HighProbMG(MGCntyGndr3PH1, sizeMGcntygndr3PH1, orden = c(5,2,1,3,4,6), n = 17:20)
HighProbMG(MGCntyGndr3PH1, sizeMGcntygndr3PH1, orden = c(5,2,1,3,4,6), n = c(21:22,27:28))
HighProbMG(MGCntyGndr3PH1, sizeMGcntygndr3PH1, orden = c(5,2,1,3,4,6), n = 23:26)
HighProbMG(MGCntyGndr3PH1, sizeMGcntygndr3PH1, orden = c(5,2,1,3,4,6), n = 29:32)
HighProbMG(MGCntyGndr3PH1, sizeMGcntygndr3PH1, orden = c(5,2,1,3,4,6), n = 33:36)
HighProbMG(MGCntyGndr3PH1, sizeMGcntygndr3PH1, orden = c(5,2,1,3,4,6), n = 37:40)

sizeMGcntygndr3PH1 %>%
  reshape2::dcast(Group1 + Cycle ~ Group2 + ClassN) %>% arrange(as.character(Group1)) %>%
  kbl(caption = "Size three classes MG Country Partial homogeneity 1 class", booktabs = TRUE, longtable = TRUE, row.names = FALSE, digits = 3,
      col.names = c("Country", "Year", classes3M2, classes3M2)) %>%
  kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"),
                position = "left", font_size = 11, full_width = FALSE) %>%
  column_spec(1:2, width = "3em") %>%
  column_spec(3:8, width = "4em") %>%
  collapse_rows(1) %>%
  add_header_above(c(" " = 1, " " = 1, "Boy" = 3, "Girl" = 3)) %>%
  print()
cat('\n')
cat('\n')

# ------ 3C MG Country Gender PH2 ----
cat('#### Partial Homogeneity - two classes equal across Country and Gender \n')
cat('\n')
cat('\n')
warn_MGCntryGndrPH2 <- list(C1 = MGCntryGndr$MGCntyGndr_C1cl3PH2.out$warnings,
                            C2 = MGCntryGndr$MGCntyGndr_C2cl3PH2.out$warnings,
                            C3 = MGCntryGndr$MGCntyGndr_C3cl3PH2.out$warnings)

cat('\n')
cat('\n')
wr <- names(warn_MGCntryGndrPH2[grep("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED",warn_MGCntryGndrPH2)])
if (length(wr) == 0) print("ALL BEST LOGLIKELIHOOD VALUES WERE REPLICATED") else
  cat("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED for the following cases:  \n", wr)
cat('\n')
cat('\n')

#----C1
cntgn1 <- sort(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C1", c("cntgnd")])$cntgnd)
cnt1 <- sort(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C1", c("COUNTRY")])$COUNTRY)
MGCntyGndr_C1cl3PH2 <- MGCntryGndr$MGCntyGndr_C1cl3PH2.out$parameters$probability.scale %>%
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>%
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>%
  mutate(Group = factor(sub('\\..*', '', Class),
                        levels = 1:length(cntgn1), labels = cntgn1),
         Group1 = factor(sub("Boy|Girl","",Group)),
         Group2 = factor(substr(Group, 4, nchar(as.character(Group)))),
         Class1 = sub('.{1,2}\\.', '', Class),
         ClassN = factor(
           case_when(
             Class1 == "1" ~ 2,
             Class1 == "2" ~ 3,
             Class1 == "3" ~ 1),
           levels = c("1", "2", "3"), labels = classes3M2))
#----C2
cntgn2 <- sort(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C2", c("cntgnd")])$cntgnd)
MGCntyGndr_C2cl3PH2 <- MGCntryGndr$MGCntyGndr_C2cl3PH2.out$parameters$probability.scale %>%
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>%
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>%
  mutate(Group = factor(sub('\\..*', '', Class),
                        levels = 1:length(cntgn2), labels = cntgn2),
         Group1 = factor(sub("Boy|Girl","",Group)),
         Group2 = factor(substr(Group, 4, nchar(as.character(Group)))),
         
         Class1 = sub('.{1,2}\\.', '', Class),
         ClassN = factor(
           case_when(
             Class1 == "1" ~ 3,
             Class1 == "2" ~ 2,
             Class1 == "3" ~ 1),
           levels = c("1", "2", "3"), labels = classes3M2))
#----C3
cntgn3 <- sort(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C3", c("cntgnd")])$cntgnd)
MGCntyGndr_C3cl3PH2 <- MGCntryGndr$MGCntyGndr_C3cl3PH2.out$parameters$probability.scale %>%
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>%
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>%
  mutate(Group = factor(sub('\\..*', '', Class),
                        levels = 1:length(cntgn3), labels = cntgn3),
         Group1 = factor(sub("Boy|Girl","",Group)),
         Group2 = factor(substr(Group, 4, nchar(as.character(Group)))),
         
         Class1 = sub('.{1,2}\\.', '', Class),
         ClassN = factor(
           case_when(
             Class1 == "1" ~ 1,
             Class1 == "2" ~ 2,
             Class1 == "3" ~ 3),
           levels = c("1", "2", "3"), labels = classes3M2))

MGCntyGndr3PH2 <- rbind(cbind(Cycle = "1999", MGCntyGndr_C1cl3PH2),
                        cbind(Cycle = "2009", MGCntyGndr_C2cl3PH2),
                        cbind(Cycle = "2016", MGCntyGndr_C3cl3PH2))

graphclass(MGCntyGndr3PH2[MGCntyGndr3PH2$Group %in% cntgn3[1:2],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Partial homogeneity 2 classes")
graphclass(MGCntyGndr3PH2[MGCntyGndr3PH2$Group %in% cntgn3[3:4],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Partial homogeneity 2 classes")
graphclass(MGCntyGndr3PH2[MGCntyGndr3PH2$Group %in% cntgn3[5:6],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Partial homogeneity 2 classes")
graphclass(MGCntyGndr3PH2[MGCntyGndr3PH2$Group %in% cntgn3[7:8],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Partial homogeneity 2 classes")
graphclass(MGCntyGndr3PH2[MGCntyGndr3PH2$Group %in% cntgn3[9:10],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Partial homogeneity 2 classes")
graphclass(MGCntyGndr3PH2[MGCntyGndr3PH2$Group %in% cntgn3[11:12],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Partial homogeneity 2 classes")
graphclass(MGCntyGndr3PH2[MGCntyGndr3PH2$Group %in% cntgn3[13:14],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Partial homogeneity 2 classes")
graphclass(MGCntyGndr3PH2[MGCntyGndr3PH2$Group %in% cntgn3[15:16],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Partial homogeneity 2 classes")
graphclass(MGCntyGndr3PH2[MGCntyGndr3PH2$Group %in% cntgn3[17:18],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Partial homogeneity 2 classes")
graphclass(MGCntyGndr3PH2[MGCntyGndr3PH2$Group %in% cntgn3[19:20],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Partial homogeneity 2 classes")
graphclass(MGCntyGndr3PH2[MGCntyGndr3PH2$Group %in% cntgn3[21:22],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Partial homogeneity 2 classes")
graphclass(MGCntyGndr3PH2[MGCntyGndr3PH2$Group %in% cntgn3[23:24],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Partial homogeneity 2 classes")
graphclass(MGCntyGndr3PH2[MGCntyGndr3PH2$Group %in% cntgn3[25:26],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Partial homogeneity 2 classes")
graphclass(MGCntyGndr3PH2[MGCntyGndr3PH2$Group %in% cntgn3[27:28],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Partial homogeneity 2 classes")
graphclass(MGCntyGndr3PH2[MGCntyGndr3PH2$Group %in% cntgn3[29:30],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Partial homogeneity 2 classes")
graphclass(MGCntyGndr3PH2[MGCntyGndr3PH2$Group %in% cntgn3[31:32],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Partial homogeneity 2 classes")
graphclass(MGCntyGndr3PH2[MGCntyGndr3PH2$Group %in% cntgn3[33:34],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Partial homogeneity 2 classes")
graphclass(MGCntyGndr3PH2[MGCntyGndr3PH2$Group %in% cntgn3[35:36],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Partial homogeneity 2 classes")
graphclass(MGCntyGndr3PH2[MGCntyGndr3PH2$Group %in% cntgn3[37:38],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Partial homogeneity 2 classes")
graphclass(MGCntyGndr3PH2[MGCntyGndr3PH2$Group %in% cntgn3[39:40],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Partial homogeneity 2 classes")
graphclass(MGCntyGndr3PH2[MGCntyGndr3PH2$Group %in% cntgn3[41:42],], nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Partial homogeneity 2 classes")

lcicntgn <- VarClass(MGCntyGndr3PH2)
for(j in sort(as.character(unique(lcicntgn$Group1)))) {
  lcicntgn %>% group_by(Cycle, ClassN, Group, param) %>% filter(category == 2 & Group1 == j) %>%
    select(Cycle, param, ClassN, Group, value) %>%
    mutate(value = cell_spec(value, color = ifelse(value > 0.5, "blue", "red"))) %>%
    reshape2::dcast(param  + Cycle ~ Group + ClassN ) %>%
    kbl(caption = paste("Probabilities to Agree each item by Class and Gender Partial Homogeneity 2 classes - ", j), booktabs = TRUE, longtable = TRUE,
        align = "llrrrrrr", row.names = FALSE, digits = 3, col.names = c("Item", "Year", classes3M2, classes3M2), escape = FALSE) %>%
    kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"),
                  position = "left", font_size = 10, full_width = FALSE) %>%
    column_spec(1, width = "14em") %>%
    column_spec(2, width = "2em") %>%
    column_spec(3:8, width = "4em") %>%
    collapse_rows(1, valign = "top") %>%
    add_header_above(c(" "=1," "=1, "Boy" = 3, "Girl" = 3)) %>%
    print()
  cat('\n')
  cat('\n')
}

#Extracting patterns
sizeMGcntygndr3PH2 <- full_join(
  full_join(MGCntryGndr$MGCntyGndr_C1cl3PH2.out$class_counts$modelEstimated.patterns %>%
              rename_with(~ c("1999", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>%
              mutate(Class = paste(Group,C,sep="."),
                     Group = factor(Group, levels = 1:length(cntgn1), labels = cntgn1),
                     Group1 = factor(sub("Boy|Girl","",Group)),
                     Group2 = factor(substr(Group, 4, nchar(as.character(Group)))),
                     Class1 = sub('.{1,2}\\.', '', Class)) %>% 
              mutate(ClassN = factor(
                case_when(
                  Class1 == "1" ~ 2,
                  Class1 == "2" ~ 3,
                  Class1 == "3" ~ 1
                ), levels = 1:3, labels = classes3M2)) %>% dplyr::select(-proportion, -C, -Class) ,
            MGCntryGndr$MGCntyGndr_C2cl3PH2.out$class_counts$modelEstimated.patterns %>%
              rename_with(~ c("2009", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>%
              mutate(Class = paste(Group,C,sep="."),
                     Group = factor(Group, levels = 1:length(cntgn2), labels = cntgn2),
                     Group1 = factor(sub("Boy|Girl","",Group)),
                     Group2 = factor(substr(Group, 4, nchar(as.character(Group)))),
                     Class1 = sub('.{1,2}\\.', '', Class)) %>% 
              mutate(ClassN = factor(case_when(
                Class1 == "1" ~ 3,
                Class1 == "2" ~ 2,
                Class1 == "3" ~ 1
              ), levels = 1:3, labels = classes3M2)) %>% dplyr::select(-proportion, -C, -Class), by = c("Group","Group1","Group2","ClassN")),
  MGCntryGndr$MGCntyGndr_C3cl3PH2.out$class_counts$modelEstimated.patterns %>%
    rename_with(~ c("2016", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>%
    mutate(Class = paste(Group,C,sep="."),
           Group = factor(Group, levels = 1:length(cntgn3), labels = cntgn3),
           Group1 = factor(sub("Boy|Girl","",Group)),
           Group2 = factor(substr(Group, 4, nchar(as.character(Group)))),
           Class1 = sub('.{1,2}\\.', '', Class)) %>% 
    mutate(ClassN = factor(case_when(
      Class1 == "1" ~ 1,
      Class1 == "2" ~ 2,
      Class1 == "3" ~ 3
    ), levels = 1:3, labels = classes3M2)) %>% dplyr::select(-proportion, -C, -Class) , by = c("Group","Group1","Group2","ClassN"))


sizeMGcntygndr3PH2 <- sizeMGcntygndr3PH2  %>%
  select(Group, Group1, Group2, ClassN, `1999`, `2009`, `2016`) %>%
  reshape2::melt(id.vars = c("Group", "Group1", "Group2","ClassN"), variable.name = "Cycle") %>%
  arrange(Group1, Group2, Cycle) %>%
  group_by(Group1, Group2, Cycle) %>% #add Group2 if proportion by gender and not by country
  mutate(countT= sum(value, na.rm = TRUE)) %>%
  group_by(ClassN) %>%
  mutate(per=value/countT) %>% dplyr::select(Group, Group1, Group2, Cycle, ClassN, per)

HighProbMG(MGCntyGndr3PH2, sizeMGcntygndr3PH2, orden = c(5,2,1,3,4,6), n = 1:2, title = "Probabilities of Agree for each class in Country*Gender \nMultigroup analysis Partial Homogeneity 2 classes by Cycle")
HighProbMG(MGCntyGndr3PH2, sizeMGcntygndr3PH2, orden = c(5,2,1,3,4,6), n = c(11:12,41:42))
HighProbMG(MGCntyGndr3PH2, sizeMGcntygndr3PH2, orden = c(5,2,1,3,4,6), n = 3:6)
HighProbMG(MGCntyGndr3PH2, sizeMGcntygndr3PH2, orden = c(5,2,1,3,4,6), n = 7:10)
HighProbMG(MGCntyGndr3PH2, sizeMGcntygndr3PH2, orden = c(5,2,1,3,4,6), n = 13:16)
HighProbMG(MGCntyGndr3PH2, sizeMGcntygndr3PH2, orden = c(5,2,1,3,4,6), n = 17:20)
HighProbMG(MGCntyGndr3PH2, sizeMGcntygndr3PH2, orden = c(5,2,1,3,4,6), n = c(21:22,27:28))
HighProbMG(MGCntyGndr3PH2, sizeMGcntygndr3PH2, orden = c(5,2,1,3,4,6), n = 23:26)
HighProbMG(MGCntyGndr3PH2, sizeMGcntygndr3PH2, orden = c(5,2,1,3,4,6), n = 29:32)
HighProbMG(MGCntyGndr3PH2, sizeMGcntygndr3PH2, orden = c(5,2,1,3,4,6), n = 33:36)
HighProbMG(MGCntyGndr3PH2, sizeMGcntygndr3PH2, orden = c(5,2,1,3,4,6), n = 37:40)

sizeMGcntygndr3PH2 %>%
  reshape2::dcast(Group1 + Cycle ~ Group2 + ClassN) %>% arrange(as.character(Group1)) %>%
  kbl(caption = "Size three classes MG Country Gender Partial homogeneity 2 classes", booktabs = TRUE, longtable = TRUE, row.names = FALSE, digits = 3,
      col.names = c("Country", "Year", classes3M2, classes3M2)) %>%
  kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"),
                position = "left", font_size = 11, full_width = FALSE) %>%
  column_spec(1:2, width = "3em") %>%
  column_spec(3:8, width = "4em") %>%
  collapse_rows(1) %>%
  add_header_above(c(" " = 1, " " = 1, "Boy" = 3, "Girl" = 3)) %>%
  print()
cat('\n')
cat('\n')


# ------ 3C MG Country Gender PH3 ----
cat('#### Complete Homogeneity - all classes equal across Country and Gender \n')
cat('\n')
cat('\n')
warn_MGCntryGndrPH3 <- list(C1 = MGCntryGndr$MGCntyGndr_C1cl3PH3.out$warnings,
                            C2 = MGCntryGndr$MGCntyGndr_C2cl3PH3.out$warnings,
                            C3 = MGCntryGndr$MGCntyGndr_C3cl3PH3.out$warnings)

cat('\n')
cat('\n')
wr <- names(warn_MGCntryGndrPH3[grep("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED",warn_MGCntryGndrPH3)])
if (length(wr) == 0) print("ALL BEST LOGLIKELIHOOD VALUES WERE REPLICATED") else
  cat("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED for the following cases:  \n", wr)
cat('\n')
cat('\n')

#----C1
cntgn1 <- sort(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C1", c("cntgnd")])$cntgnd)
cnt1 <- sort(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C1", c("COUNTRY")])$COUNTRY)
MGCntyGndr_C1cl3PH3 <- MGCntryGndr$MGCntyGndr_C1cl3PH3.out$parameters$probability.scale %>%
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>%
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>%
  mutate(Class = factor(
           case_when(
             Class == "C#1" ~ 3,
             Class == "C#2" ~ 1,
             Class == "C#3" ~ 2),
           levels = c("1", "2", "3"), labels = classes3M2),
         ClassN = Class)
#----C2
cntgn2 <- sort(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C2", c("cntgnd")])$cntgnd)
MGCntyGndr_C2cl3PH3 <- MGCntryGndr$MGCntyGndr_C2cl3PH3.out$parameters$probability.scale %>%
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>%
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>%
  mutate(Class = factor(
           case_when(
             Class == "C#1" ~ 1,
             Class == "C#2" ~ 2,
             Class == "C#3" ~ 3),
           levels = c("1", "2", "3"), labels = classes3M2),
         ClassN = Class)
#----C3
cntgn3 <- sort(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C3", c("cntgnd")])$cntgnd)
MGCntyGndr_C3cl3PH3 <- MGCntryGndr$MGCntyGndr_C3cl3PH3.out$parameters$probability.scale %>%
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>%
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>%
  mutate(Class = factor(
           case_when(
             Class == "C#1" ~ 2,
             Class == "C#2" ~ 1,
             Class == "C#3" ~ 3),
           levels = c("1", "2", "3"), labels = classes3M2),
         ClassN = Class)

MGCntyGndr3PH3 <- rbind(cbind(Cycle = "1999", MGCntyGndr_C1cl3PH3),
                        cbind(Cycle = "2009", MGCntyGndr_C2cl3PH3),
                        cbind(Cycle = "2016", MGCntyGndr_C3cl3PH3))

graphclass(MGCntyGndr3PH3, nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Country*Gender Complete homogeneity", mg =FALSE)

VarClass(MGCntyGndr3PH3) %>% group_by(Cycle, ClassN, param) %>% filter(category == 2) %>%
    select(Cycle, param, ClassN, value) %>%
    mutate(value = cell_spec(value, color = ifelse(value > 0.5, "blue", "red"))) %>%
    reshape2::dcast(param  + Cycle ~ ClassN ) %>%
    kbl(caption = paste("Probabilities to Agree each item by Class and Gender Complete homogeneity", j), booktabs = TRUE, longtable = TRUE,
        align = "llrrrrr", row.names = FALSE, digits = 3, col.names = c("Item", "Year", classes3M2), escape = FALSE) %>%
    kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"),
                  position = "left", font_size = 10, full_width = FALSE) %>%
    column_spec(1, width = "18em") %>%
    column_spec(2, width = "2em") %>%
    column_spec(3:5, width = "4em") %>%
    collapse_rows(1, valign = "top") %>%
    print()
  cat('\n')
  cat('\n')

#Extracting patterns
sizeMGcntygndr3PH3 <- full_join(
  full_join(MGCntryGndr$MGCntyGndr_C1cl3PH3.out$class_counts$modelEstimated.patterns %>%
              rename_with(~ c("1999", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>%
              mutate(Class = paste(Group,C,sep="."),
                     Group = factor(Group, levels = 1:length(cntgn1), labels = cntgn1),
                     Group1 = factor(sub("Boy|Girl","",Group)),
                     Group2 = factor(substr(Group, 4, nchar(as.character(Group)))),
                     Class1 = sub('.{1,2}\\.', '', Class)) %>% 
              mutate(ClassN = factor(
                case_when(
                  Class1 == "1" ~ 3,
                  Class1 == "2" ~ 1,
                  Class1 == "3" ~ 2
                ), levels = 1:3, labels = classes3M2)) %>% dplyr::select(-proportion, -C, -Class) ,
            MGCntryGndr$MGCntyGndr_C2cl3PH3.out$class_counts$modelEstimated.patterns %>%
              rename_with(~ c("2009", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>%
              mutate(Class = paste(Group,C,sep="."),
                     Group = factor(Group, levels = 1:length(cntgn2), labels = cntgn2),
                     Group1 = factor(sub("Boy|Girl","",Group)),
                     Group2 = factor(substr(Group, 4, nchar(as.character(Group)))),
                     Class1 = sub('.{1,2}\\.', '', Class)) %>% 
              mutate(ClassN = factor(case_when(
                Class1 == "1" ~ 1,
                Class1 == "2" ~ 2,
                Class1 == "3" ~ 3
              ), levels = 1:3, labels = classes3M2)) %>% dplyr::select(-proportion, -C, -Class), by = c("Group","Group1","Group2","ClassN")),
  MGCntryGndr$MGCntyGndr_C3cl3PH3.out$class_counts$modelEstimated.patterns %>%
    rename_with(~ c("2016", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>%
    mutate(Class = paste(Group,C,sep="."),
           Group = factor(Group, levels = 1:length(cntgn3), labels = cntgn3),
           Group1 = factor(sub("Boy|Girl","",Group)),
           Group2 = factor(substr(Group, 4, nchar(as.character(Group)))),
           Class1 = sub('.{1,2}\\.', '', Class)) %>% 
    mutate(ClassN = factor(case_when(
      Class1 == "1" ~ 2,
      Class1 == "2" ~ 1,
      Class1 == "3" ~ 3
    ), levels = 1:3, labels = classes3M2)) %>% dplyr::select(-proportion, -C, -Class) , by = c("Group","Group1","Group2","ClassN"))


sizeMGcntygndr3PH3 <- sizeMGcntygndr3PH3  %>%
  select(Group, Group1, Group2, ClassN, `1999`, `2009`, `2016`) %>%
  reshape2::melt(id.vars = c("Group", "Group1", "Group2","ClassN"), variable.name = "Cycle") %>%
  arrange(Group1, Group2, Cycle) %>%
  group_by(Group1, Group2, Cycle) %>% #add Group2 if proportion by gender and not by country
  mutate(countT= sum(value, na.rm = TRUE)) %>%
  group_by(ClassN) %>%
  mutate(per=value/countT) %>% dplyr::select(Group, Group1, Group2, Cycle, ClassN, per)

MGCntyGndr3PH3rep <- rbind(cbind(Group1=rep(cnt1, each = nrow(MGCntyGndr3PH3[MGCntyGndr3PH3$Cycle == '1999',])*2), 
                                 Group2=rep(c("Boy","Girl"), each = nrow(MGCntyGndr3PH3[MGCntyGndr3PH3$Cycle == '1999',])),
                                 MGCntyGndr3PH3[MGCntyGndr3PH3$Cycle == '1999',]),
                       cbind(Group1=rep(cnt2, each = nrow(MGCntyGndr3PH3[MGCntyGndr3PH3$Cycle == '2009',])*2), 
                             Group2=rep(c("Boy","Girl"), each = nrow(MGCntyGndr3PH3[MGCntyGndr3PH3$Cycle == '2009',])),
                             MGCntyGndr3PH3[MGCntyGndr3PH3$Cycle == '2009',]),
                       cbind(Group1=rep(cnt3, each = nrow(MGCntyGndr3PH3[MGCntyGndr3PH3$Cycle == '2016',])*2), 
                             Group2=rep(c("Boy","Girl"), each = nrow(MGCntyGndr3PH3[MGCntyGndr3PH3$Cycle == '2016',])),
                             MGCntyGndr3PH3[MGCntyGndr3PH3$Cycle == '2016',]))
MGCntyGndr3PH3rep$Group = paste0(MGCntyGndr3PH3rep$Group1,MGCntyGndr3PH3rep$Group2)
HighProbMG(MGCntyGndr3PH3rep, sizeMGcntygndr3PH3, orden = c(5,2,1,3,4,6), n = 1:2, title = "Probabilities of Agree for each class in Country*Gender \nMultigroup analysis Complete homogeneity by Cycle")
HighProbMG(MGCntyGndr3PH3rep, sizeMGcntygndr3PH3, orden = c(5,2,1,3,4,6), n = c(11:12,41:42))
HighProbMG(MGCntyGndr3PH3rep, sizeMGcntygndr3PH3, orden = c(5,2,1,3,4,6), n = 3:6)
HighProbMG(MGCntyGndr3PH3rep, sizeMGcntygndr3PH3, orden = c(5,2,1,3,4,6), n = 7:10)
HighProbMG(MGCntyGndr3PH3rep, sizeMGcntygndr3PH3, orden = c(5,2,1,3,4,6), n = 13:16)
HighProbMG(MGCntyGndr3PH3rep, sizeMGcntygndr3PH3, orden = c(5,2,1,3,4,6), n = 17:20)
HighProbMG(MGCntyGndr3PH3rep, sizeMGcntygndr3PH3, orden = c(5,2,1,3,4,6), n = c(21:22,27:28))
HighProbMG(MGCntyGndr3PH3rep, sizeMGcntygndr3PH3, orden = c(5,2,1,3,4,6), n = 23:26)
HighProbMG(MGCntyGndr3PH3rep, sizeMGcntygndr3PH3, orden = c(5,2,1,3,4,6), n = 29:32)
HighProbMG(MGCntyGndr3PH3rep, sizeMGcntygndr3PH3, orden = c(5,2,1,3,4,6), n = 33:36)
HighProbMG(MGCntyGndr3PH3rep, sizeMGcntygndr3PH3, orden = c(5,2,1,3,4,6), n = 37:40)

sizeMGcntygndr3PH3 %>%
  reshape2::dcast(Group1 + Cycle ~ Group2 + ClassN) %>% arrange(as.character(Group1)) %>%
  kbl(caption = "Size three classes MG Country Gender Complete homogeneity", booktabs = TRUE, longtable = TRUE, row.names = FALSE, digits = 3,
      col.names = c("Country", "Year", classes3M2, classes3M2)) %>%
  kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"),
                position = "left", font_size = 11, full_width = FALSE) %>%
  column_spec(1:2, width = "3em") %>%
  column_spec(3:8, width = "4em") %>%
  collapse_rows(1) %>%
  add_header_above(c(" " = 1, " " = 1, "Boy" = 3, "Girl" = 3)) %>%
  print()
cat('\n')
cat('\n')
cat('\\newpage')


cat('\n')
cat('\n')
cat('## Country analysis by cycle  \n')
cat('\n')
cat('\n')
#ByCountry_lca <- list(ByCountry_lca2,ByCountry_lca3)
#Graph_modelfitByGroup("ByCountry_lca")


# ----------Results 2C by Cntry and Cycle ----

cat('\n')
cat('\n')
cat('### Results with 2-classes  \n')
cat('\n')
cat('\n')

#----C1
ByCNTlca2C1 <- ByCountry_lca2[[1]] 
CNT <- sort(as.character(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C1","COUNTRY"])$COUNTRY))
ByCNTlca_C1cl2 <- NULL
warnByCNTlca_C1cl2 <- NULL

for (j in 1:length(ByCNTlca2C1)) {
  warnByCNTlca_C1cl2[CNT[j]] <- eval(parse(text=paste0("list(",CNT[j], " = ByCNTlca2C1$data.Mplus.models.ByCountry.lca_",CNT[j],"_C1cl2.out$warnings)")))
  
  lca <- cbind(CNT = CNT[j], Cycle = "1999",eval(parse(text=paste0("ByCNTlca2C1$data.Mplus.models.ByCountry.lca_",CNT[j],"_C1cl2.out$parameters$probability.scale"))))
  ByCNTlca_C1cl2 <- rbind(ByCNTlca_C1cl2, lca)
}

ByCNTlca_C1cl2 <- ByCNTlca_C1cl2 %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>% 
  mutate(Group = CNT, ClassN = factor(case_when(
    #-----
    CNT == "BGR" & Class == "1" ~ 2,
    CNT == "BGR" & Class == "2" ~ 1,
    CNT == "CHL" & Class == "1" ~ 1,
    CNT == "CHL" & Class == "2" ~ 2,
    CNT == "COL" & Class == "1" ~ 1,
    CNT == "COL" & Class == "2" ~ 2,
    CNT == "DNK" & Class == "1" ~ 1,
    CNT == "DNK" & Class == "2" ~ 2,
    CNT == "EST" & Class == "1" ~ 1,
    CNT == "EST" & Class == "2" ~ 2,
    CNT == "FIN" & Class == "1" ~ 1,
    CNT == "FIN" & Class == "2" ~ 2,
    CNT == "HKG" & Class == "1" ~ 1,
    CNT == "HKG" & Class == "2" ~ 2,
    CNT == "ITA" & Class == "1" ~ 1,
    CNT == "ITA" & Class == "2" ~ 2,
    CNT == "LTU" & Class == "1" ~ 1,
    CNT == "LTU" & Class == "2" ~ 2,
    CNT == "LVA" & Class == "1" ~ 1,
    CNT == "LVA" & Class == "2" ~ 2,
    CNT == "NOR" & Class == "1" ~ 1,
    CNT == "NOR" & Class == "2" ~ 2,
    CNT == "RUS" & Class == "1" ~ 1,
    CNT == "RUS" & Class == "2" ~ 2,
    CNT == "SVN" & Class == "1" ~ 1,
    CNT == "SVN" & Class == "2" ~ 2,
    CNT == "SWE" & Class == "1" ~ 2,
    CNT == "SWE" & Class == "2" ~ 1
    #----
  ), levels = c("1", "2"), labels = classes2M2))
#----C2
ByCNTlca2C2 <- ByCountry_lca2[[2]] 
CNT <- sort(as.character(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C2","COUNTRY"])$COUNTRY))
ByCNTlca_C2cl2 <- NULL
warnByCNTlca_C2cl2 <- NULL

for (j in 1:length(ByCNTlca2C2)) {
  warnByCNTlca_C2cl2[CNT[j]] <- eval(parse(text=paste0("list(",CNT[j], " = ByCNTlca2C2$data.Mplus.models.ByCountry.lca_",CNT[j],"_C2cl2.out$warnings)")))
  
  lca <- cbind(CNT = CNT[j], Cycle = "2009",eval(parse(text=paste0("ByCNTlca2C2$data.Mplus.models.ByCountry.lca_",CNT[j],"_C2cl2.out$parameters$probability.scale"))))
  ByCNTlca_C2cl2 <- rbind(ByCNTlca_C2cl2, lca)
}

ByCNTlca_C2cl2 <- ByCNTlca_C2cl2 %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x))%>% 
  mutate(Group = CNT, ClassN = factor(case_when(
    #-----
    CNT == "BFL" & Class == "1" ~ 1,
    CNT == "BFL" & Class == "2" ~ 2,
    CNT == "BGR" & Class == "1" ~ 1,
    CNT == "BGR" & Class == "2" ~ 2,
    CNT == "CHL" & Class == "1" ~ 1,
    CNT == "CHL" & Class == "2" ~ 2,
    CNT == "COL" & Class == "1" ~ 1,
    CNT == "COL" & Class == "2" ~ 2,
    CNT == "DNK" & Class == "1" ~ 2,
    CNT == "DNK" & Class == "2" ~ 1,
    CNT == "DOM" & Class == "1" ~ 1,
    CNT == "DOM" & Class == "2" ~ 2,
    CNT == "EST" & Class == "1" ~ 1,
    CNT == "EST" & Class == "2" ~ 2,
    CNT == "FIN" & Class == "1" ~ 2,
    CNT == "FIN" & Class == "2" ~ 1,
    CNT == "HKG" & Class == "1" ~ 1,
    CNT == "HKG" & Class == "2" ~ 2,
    CNT == "ITA" & Class == "1" ~ 1,
    CNT == "ITA" & Class == "2" ~ 2,
    CNT == "KOR" & Class == "1" ~ 1,
    CNT == "KOR" & Class == "2" ~ 2,
    CNT == "LTU" & Class == "1" ~ 1,
    CNT == "LTU" & Class == "2" ~ 2,
    CNT == "LVA" & Class == "1" ~ 1,
    CNT == "LVA" & Class == "2" ~ 2,
    CNT == "MEX" & Class == "1" ~ 1,
    CNT == "MEX" & Class == "2" ~ 2,
    CNT == "MLT" & Class == "1" ~ 1,
    CNT == "MLT" & Class == "2" ~ 2,
    CNT == "NLD" & Class == "1" ~ 2,
    CNT == "NLD" & Class == "2" ~ 1,
    CNT == "NOR" & Class == "1" ~ 1,
    CNT == "NOR" & Class == "2" ~ 2,
    CNT == "RUS" & Class == "1" ~ 1,
    CNT == "RUS" & Class == "2" ~ 2,
    CNT == "SVN" & Class == "1" ~ 2,
    CNT == "SVN" & Class == "2" ~ 1,
    CNT == "SWE" & Class == "1" ~ 1,
    CNT == "SWE" & Class == "2" ~ 2,
    CNT == "TWN" & Class == "1" ~ 2,
    CNT == "TWN" & Class == "2" ~ 1
    #----
  ), levels = c("1", "2"), labels = classes2M2))
#----C3
ByCNTlca2C3 <- ByCountry_lca2[[3]] 
CNT <- sort(as.character(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C3","COUNTRY"])$COUNTRY))
ByCNTlca_C3cl2 <- NULL
warnByCNTlca_C3cl2 <- NULL

for (j in 1:length(ByCNTlca2C3)) {
  warnByCNTlca_C3cl2[CNT[j]] <- eval(parse(text=paste0("list(",CNT[j], " = ByCNTlca2C3$data.Mplus.models.ByCountry.lca_",CNT[j],"_C3cl2.out$warnings)")))
  
  lca <- cbind(CNT = CNT[j], Cycle = "2016",eval(parse(text=paste0("ByCNTlca2C3$data.Mplus.models.ByCountry.lca_",CNT[j],"_C3cl2.out$parameters$probability.scale"))))
  ByCNTlca_C3cl2 <- rbind(ByCNTlca_C3cl2, lca)
}


ByCNTlca_C3cl2 <- ByCNTlca_C3cl2 %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>% 
  mutate(Group = CNT, ClassN = factor(case_when(
    #-----
    CNT == "BFL" & Class == "1" ~ 2,
    CNT == "BFL" & Class == "2" ~ 1,
    CNT == "BGR" & Class == "1" ~ 1,
    CNT == "BGR" & Class == "2" ~ 2,
    CNT == "CHL" & Class == "1" ~ 1,
    CNT == "CHL" & Class == "2" ~ 2,
    CNT == "COL" & Class == "1" ~ 1,
    CNT == "COL" & Class == "2" ~ 2,
    CNT == "DNK" & Class == "1" ~ 1,
    CNT == "DNK" & Class == "2" ~ 2,
    CNT == "DOM" & Class == "1" ~ 1,
    CNT == "DOM" & Class == "2" ~ 2,
    CNT == "EST" & Class == "1" ~ 1,
    CNT == "EST" & Class == "2" ~ 2,
    CNT == "FIN" & Class == "1" ~ 1,
    CNT == "FIN" & Class == "2" ~ 2,
    CNT == "HKG" & Class == "1" ~ 1,
    CNT == "HKG" & Class == "2" ~ 2,
    CNT == "ITA" & Class == "1" ~ 2,
    CNT == "ITA" & Class == "2" ~ 1,
    CNT == "KOR" & Class == "1" ~ 1,
    CNT == "KOR" & Class == "2" ~ 2,
    CNT == "LTU" & Class == "1" ~ 2,
    CNT == "LTU" & Class == "2" ~ 1,
    CNT == "LVA" & Class == "1" ~ 1,
    CNT == "LVA" & Class == "2" ~ 2,
    CNT == "MEX" & Class == "1" ~ 2,
    CNT == "MEX" & Class == "2" ~ 1,
    CNT == "MLT" & Class == "1" ~ 2,
    CNT == "MLT" & Class == "2" ~ 1,
    CNT == "NLD" & Class == "1" ~ 1,
    CNT == "NLD" & Class == "2" ~ 2,
    CNT == "NOR" & Class == "1" ~ 2,
    CNT == "NOR" & Class == "2" ~ 1,
    CNT == "RUS" & Class == "1" ~ 1,
    CNT == "RUS" & Class == "2" ~ 2,
    CNT == "SVN" & Class == "1" ~ 1,
    CNT == "SVN" & Class == "2" ~ 2,
    CNT == "SWE" & Class == "1" ~ 1,
    CNT == "SWE" & Class == "2" ~ 2,
    CNT == "TWN" & Class == "1" ~ 1,
    CNT == "TWN" & Class == "2" ~ 2
    #----
  ), levels = c("1", "2"), labels = classes2M2))

ByCNTlca2 <- rbind(ByCNTlca_C1cl2,
                   ByCNTlca_C2cl2,
                   ByCNTlca_C3cl2)


warningsByCNTlca2 <- list(C1 = warnByCNTlca_C1cl2,
                         C2 = warnByCNTlca_C2cl2,
                         C3 = warnByCNTlca_C3cl2)

cat('\n')
cat('\n')
wr <- names(warningsByCNTlca2[grep("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED",warningsByCNTlca2)])
if (length(wr) == 0) print("ALL BEST LOGLIKELIHOOD VALUES WERE REPLICATED") else
  cat("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED for the following cases:  \n", wr)
cat('\n')
cat('\n')
#---
ByGroupgraphclass(ByCNTlca2, nclass = 3, orden = c(5,2,1,3,4,6) ,title = "LCA with 2 classes")
cat('\n')
cat('\n')

ByCNTlcign1 <- VarClass(ByCNTlca2)
ByCNTlcign <- ByCNTlcign1 %>% group_by(CNT, Cycle, ClassN,  param) %>% filter(category == 2) %>% 
  select(CNT, Cycle, param, ClassN, value) %>% 
  mutate(value = cell_spec(value, color = ifelse(value > 0.8, "blue", "red"))) %>% 
  reshape2::dcast(CNT + param + Cycle ~ ClassN) 

ByCNTlcign %>%
  kbl(caption = "Probabilities to Agree each item by Class and Country", booktabs = TRUE, longtable = TRUE, 
      align = "lllrrrrrr", row.names = FALSE, digits = 3, col.names = c("Country","Item", "Year", classes2M2), escape = FALSE) %>%
  kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"), 
                position = "left", font_size = 10, full_width = FALSE) %>% 
  column_spec(2, width = "16em") %>%  
  column_spec(c(1,3), width = "4em") %>%  
  column_spec(4:6, width = "4em") %>%  
  collapse_rows(c(1,2), valign = "top") %>%
  print()
cat('\n')
cat('\n')

#Extracting patterns
CNT1 <- sort(as.character(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C1","COUNTRY"])$COUNTRY))
SizeByCNTlca_C1cl2 <- NULL
for (j in 1:length(ByCNTlca2C1)) {
  lca <- cbind(CNT = CNT1[j], Cycle = "1999",eval(parse(text=paste0("ByCNTlca2C1$data.Mplus.models.ByCountry.lca_",CNT1[j],"_C1cl2.out$class_counts$modelEstimated"))))
  SizeByCNTlca_C1cl2 <- rbind(SizeByCNTlca_C1cl2, lca)
}
CNT2 <- sort(as.character(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C2","COUNTRY"])$COUNTRY))
SizeByCNTlca_C2cl2 <- NULL
for (j in 1:length(ByCNTlca2C2)) {
  lca <- cbind(CNT = CNT2[j], Cycle = "2009",eval(parse(text=paste0("ByCNTlca2C2$data.Mplus.models.ByCountry.lca_",CNT2[j],"_C2cl2.out$class_counts$modelEstimated"))))
  SizeByCNTlca_C2cl2 <- rbind(SizeByCNTlca_C2cl2, lca)
}
CNT3 <- sort(as.character(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C3","COUNTRY"])$COUNTRY))
SizeByCNTlca_C3cl2 <- NULL
for (j in 1:length(ByCNTlca2C3)) {
  lca <- cbind(CNT = CNT3[j], Cycle = "2019",eval(parse(text=paste0("ByCNTlca2C3$data.Mplus.models.ByCountry.lca_",CNT3[j],"_C3cl2.out$class_counts$modelEstimated"))))
  SizeByCNTlca_C3cl2 <- rbind(SizeByCNTlca_C3cl2, lca)
}

sizelca2 <- full_join(
  full_join(SizeByCNTlca_C1cl2 %>% 
              rename_with(~ c("1999")[which(c("count") == .x)], .cols = c("count")) %>% 
              mutate(ClassN = factor(
                case_when(
                  #-----
                  CNT == "BGR" & class == "1" ~ 2,
                  CNT == "BGR" & class == "2" ~ 1,
                  CNT == "CHL" & class == "1" ~ 1,
                  CNT == "CHL" & class == "2" ~ 2,
                  CNT == "COL" & class == "1" ~ 1,
                  CNT == "COL" & class == "2" ~ 2,
                  CNT == "DNK" & class == "1" ~ 1,
                  CNT == "DNK" & class == "2" ~ 2,
                  CNT == "EST" & class == "1" ~ 1,
                  CNT == "EST" & class == "2" ~ 2,
                  CNT == "FIN" & class == "1" ~ 1,
                  CNT == "FIN" & class == "2" ~ 2,
                  CNT == "HKG" & class == "1" ~ 1,
                  CNT == "HKG" & class == "2" ~ 2,
                  CNT == "ITA" & class == "1" ~ 1,
                  CNT == "ITA" & class == "2" ~ 2,
                  CNT == "LTU" & class == "1" ~ 1,
                  CNT == "LTU" & class == "2" ~ 2,
                  CNT == "LVA" & class == "1" ~ 1,
                  CNT == "LVA" & class == "2" ~ 2,
                  CNT == "NOR" & class == "1" ~ 1,
                  CNT == "NOR" & class == "2" ~ 2,
                  CNT == "RUS" & class == "1" ~ 1,
                  CNT == "RUS" & class == "2" ~ 2,
                  CNT == "SVN" & class == "1" ~ 1,
                  CNT == "SVN" & class == "2" ~ 2,
                  CNT == "SWE" & class == "1" ~ 2,
                  CNT == "SWE" & class == "2" ~ 1
                  #----
                ), levels = 1:2, labels = classes2M2)) %>% dplyr::select(-proportion, -class, -Cycle) ,
            SizeByCNTlca_C2cl2 %>% 
              rename_with(~ c("2009")[which(c("count") == .x)], .cols = c("count")) %>% 
              mutate(ClassN = factor(case_when(
                #-----
                CNT == "BFL" & class == "1" ~ 1,
                CNT == "BFL" & class == "2" ~ 2,
                CNT == "BGR" & class == "1" ~ 1,
                CNT == "BGR" & class == "2" ~ 2,
                CNT == "CHL" & class == "1" ~ 1,
                CNT == "CHL" & class == "2" ~ 2,
                CNT == "COL" & class == "1" ~ 1,
                CNT == "COL" & class == "2" ~ 2,
                CNT == "DNK" & class == "1" ~ 2,
                CNT == "DNK" & class == "2" ~ 1,
                CNT == "DOM" & class == "1" ~ 1,
                CNT == "DOM" & class == "2" ~ 2,
                CNT == "EST" & class == "1" ~ 1,
                CNT == "EST" & class == "2" ~ 2,
                CNT == "FIN" & class == "1" ~ 2,
                CNT == "FIN" & class == "2" ~ 1,
                CNT == "HKG" & class == "1" ~ 1,
                CNT == "HKG" & class == "2" ~ 2,
                CNT == "ITA" & class == "1" ~ 1,
                CNT == "ITA" & class == "2" ~ 2,
                CNT == "KOR" & class == "1" ~ 1,
                CNT == "KOR" & class == "2" ~ 2,
                CNT == "LTU" & class == "1" ~ 1,
                CNT == "LTU" & class == "2" ~ 2,
                CNT == "LVA" & class == "1" ~ 1,
                CNT == "LVA" & class == "2" ~ 2,
                CNT == "MEX" & class == "1" ~ 1,
                CNT == "MEX" & class == "2" ~ 2,
                CNT == "MLT" & class == "1" ~ 1,
                CNT == "MLT" & class == "2" ~ 2,
                CNT == "NLD" & class == "1" ~ 2,
                CNT == "NLD" & class == "2" ~ 1,
                CNT == "NOR" & class == "1" ~ 1,
                CNT == "NOR" & class == "2" ~ 2,
                CNT == "RUS" & class == "1" ~ 1,
                CNT == "RUS" & class == "2" ~ 2,
                CNT == "SVN" & class == "1" ~ 2,
                CNT == "SVN" & class == "2" ~ 1,
                CNT == "SWE" & class == "1" ~ 1,
                CNT == "SWE" & class == "2" ~ 2,
                CNT == "TWN" & class == "1" ~ 2,
                CNT == "TWN" & class == "2" ~ 1
                #----
              ), levels = 1:2, labels = classes2M2)) %>% dplyr::select(-proportion, -class, -Cycle), by = c("CNT", "ClassN")),
  SizeByCNTlca_C3cl2 %>%
    rename_with(~ c("2016")[which(c("count") == .x)], .cols = c("count")) %>% 
    mutate(ClassN = factor(case_when(
      #-----
      CNT == "BFL" & class == "1" ~ 2,
      CNT == "BFL" & class == "2" ~ 1,
      CNT == "BGR" & class == "1" ~ 1,
      CNT == "BGR" & class == "2" ~ 2,
      CNT == "CHL" & class == "1" ~ 1,
      CNT == "CHL" & class == "2" ~ 2,
      CNT == "COL" & class == "1" ~ 1,
      CNT == "COL" & class == "2" ~ 2,
      CNT == "DNK" & class == "1" ~ 1,
      CNT == "DNK" & class == "2" ~ 2,
      CNT == "DOM" & class == "1" ~ 1,
      CNT == "DOM" & class == "2" ~ 2,
      CNT == "EST" & class == "1" ~ 1,
      CNT == "EST" & class == "2" ~ 2,
      CNT == "FIN" & class == "1" ~ 1,
      CNT == "FIN" & class == "2" ~ 2,
      CNT == "HKG" & class == "1" ~ 1,
      CNT == "HKG" & class == "2" ~ 2,
      CNT == "ITA" & class == "1" ~ 2,
      CNT == "ITA" & class == "2" ~ 1,
      CNT == "KOR" & class == "1" ~ 1,
      CNT == "KOR" & class == "2" ~ 2,
      CNT == "LTU" & class == "1" ~ 2,
      CNT == "LTU" & class == "2" ~ 1,
      CNT == "LVA" & class == "1" ~ 1,
      CNT == "LVA" & class == "2" ~ 2,
      CNT == "MEX" & class == "1" ~ 2,
      CNT == "MEX" & class == "2" ~ 1,
      CNT == "MLT" & class == "1" ~ 2,
      CNT == "MLT" & class == "2" ~ 1,
      CNT == "NLD" & class == "1" ~ 1,
      CNT == "NLD" & class == "2" ~ 2,
      CNT == "NOR" & class == "1" ~ 2,
      CNT == "NOR" & class == "2" ~ 1,
      CNT == "RUS" & class == "1" ~ 1,
      CNT == "RUS" & class == "2" ~ 2,
      CNT == "SVN" & class == "1" ~ 1,
      CNT == "SVN" & class == "2" ~ 2,
      CNT == "SWE" & class == "1" ~ 1,
      CNT == "SWE" & class == "2" ~ 2,
      CNT == "TWN" & class == "1" ~ 1,
      CNT == "TWN" & class == "2" ~ 2
      #----
    ), levels = 1:2, labels = classes2M2)) %>% dplyr::select(-proportion, -class, -Cycle) , by = c("CNT", "ClassN")) %>% 
  select(CNT, ClassN, `1999`, `2009`, `2016`)

sizelca2 <- sizelca2 %>% reshape2::melt(id.vars = c("CNT", "ClassN"), variable.name = "Cycle") %>% 
  arrange(CNT, Cycle) %>% 
  group_by(CNT, Cycle) %>%
  mutate(countT= sum(value, na.rm = TRUE)) %>%
  group_by(ClassN) %>%
  mutate(per=value/countT, Group = CNT) %>% dplyr::select(CNT, Cycle, Group, ClassN, per) 

ByGroupHighProbMG(ByCNTlca2, sizelca2, orden = c(5,2,1,3,4,6), n = 1:2, title = "Probabilities of Agree and size for each class \nin country by Cycle")
cat('\n')
cat('\n')

sizelca2 %>% 
  reshape2::dcast(CNT + Cycle ~ ClassN) %>% 
  kbl(caption = "Size two classes Country", booktabs = TRUE, longtable = TRUE, row.names = FALSE, 
      digits = 3, col.names = c("Country", "Year", classes2M2)) %>%
  kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"),
                position = "left", font_size = 11, full_width = FALSE) %>%     
  column_spec(3:4, width = "8em") %>% 
  collapse_rows(1, valign = "top") %>% 
  print()
cat('\n')
cat('\n')

# ----------Results 3C by Cntry and Cycle ----

cat('\n')
cat('\n')
cat('### Results with 3-classes  \n')
cat('\n')
cat('\n')

#----C1
ByCNTlca3C1 <- ByCountry_lca3[[1]] 
CNT <- sort(as.character(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C1","COUNTRY"])$COUNTRY))
ByCNTlca_C1cl3 <- NULL
warnByCNTlca_C1cl3 <- NULL

for (j in 1:length(ByCNTlca3C1)) {
  warnByCNTlca_C1cl3[CNT[j]] <- eval(parse(text=paste0("list(",CNT[j], " = ByCNTlca3C1$data.Mplus.models.ByCountry.lca_",CNT[j],"_C1cl3.out$warnings)")))
  
  lca <- cbind(CNT = CNT[j], Cycle = "1999",eval(parse(text=paste0("ByCNTlca3C1$data.Mplus.models.ByCountry.lca_",CNT[j],"_C1cl3.out$parameters$probability.scale"))))
  ByCNTlca_C1cl3 <- rbind(ByCNTlca_C1cl3, lca)
}

ByCNTlca_C1cl3 <- ByCNTlca_C1cl3 %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>% 
  mutate(Group = CNT, ClassN = factor(case_when(
           #-----
           CNT == "BGR" & Class == "1" ~ 1,
           CNT == "BGR" & Class == "2" ~ 3,
           CNT == "BGR" & Class == "3" ~ 2,
           CNT == "CHL" & Class == "1" ~ 3,
           CNT == "CHL" & Class == "2" ~ 2,
           CNT == "CHL" & Class == "3" ~ 1,
           CNT == "COL" & Class == "1" ~ 2,
           CNT == "COL" & Class == "2" ~ 3,
           CNT == "COL" & Class == "3" ~ 1,
           CNT == "DNK" & Class == "1" ~ 3,
           CNT == "DNK" & Class == "2" ~ 2,
           CNT == "DNK" & Class == "3" ~ 1,
           CNT == "EST" & Class == "1" ~ 2,
           CNT == "EST" & Class == "2" ~ 1,
           CNT == "EST" & Class == "3" ~ 3,
           CNT == "FIN" & Class == "1" ~ 1,
           CNT == "FIN" & Class == "2" ~ 2,
           CNT == "FIN" & Class == "3" ~ 3,
           CNT == "HKG" & Class == "1" ~ 2,
           CNT == "HKG" & Class == "2" ~ 3,
           CNT == "HKG" & Class == "3" ~ 1,
           CNT == "ITA" & Class == "1" ~ 2,
           CNT == "ITA" & Class == "2" ~ 3,
           CNT == "ITA" & Class == "3" ~ 1,
           CNT == "LTU" & Class == "1" ~ 2,
           CNT == "LTU" & Class == "2" ~ 1,
           CNT == "LTU" & Class == "3" ~ 3,
           CNT == "LVA" & Class == "1" ~ 2,
           CNT == "LVA" & Class == "2" ~ 3,
           CNT == "LVA" & Class == "3" ~ 1,
           CNT == "NOR" & Class == "1" ~ 3,
           CNT == "NOR" & Class == "2" ~ 2,
           CNT == "NOR" & Class == "3" ~ 1,
           CNT == "RUS" & Class == "1" ~ 2,
           CNT == "RUS" & Class == "2" ~ 1,
           CNT == "RUS" & Class == "3" ~ 3,
           CNT == "SVN" & Class == "1" ~ 1,
           CNT == "SVN" & Class == "2" ~ 2,
           CNT == "SVN" & Class == "3" ~ 3,
           CNT == "SWE" & Class == "1" ~ 3,
           CNT == "SWE" & Class == "2" ~ 2,
           CNT == "SWE" & Class == "3" ~ 1
           #----
         ), levels = c("1", "2", "3"), labels = classes3M2))
#----C2
ByCNTlca3C2 <- ByCountry_lca3[[2]] 
CNT <- sort(as.character(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C2","COUNTRY"])$COUNTRY))
ByCNTlca_C2cl3 <- NULL
warnByCNTlca_C2cl3 <- NULL

for (j in 1:length(ByCNTlca3C2)) {
  warnByCNTlca_C2cl3[CNT[j]] <- eval(parse(text=paste0("list(",CNT[j], " = ByCNTlca3C2$data.Mplus.models.ByCountry.lca_",CNT[j],"_C2cl3.out$warnings)")))
  
  lca <- cbind(CNT = CNT[j], Cycle = "2009",eval(parse(text=paste0("ByCNTlca3C2$data.Mplus.models.ByCountry.lca_",CNT[j],"_C2cl3.out$parameters$probability.scale"))))
  ByCNTlca_C2cl3 <- rbind(ByCNTlca_C2cl3, lca)
}

ByCNTlca_C2cl3 <- ByCNTlca_C2cl3 %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x))%>% 
  mutate(Group = CNT, ClassN = factor(case_when(
           #-----
           CNT == "BFL" & Class == "1" ~ 1,
           CNT == "BFL" & Class == "2" ~ 3,
           CNT == "BFL" & Class == "3" ~ 2,
           CNT == "BGR" & Class == "1" ~ 2,
           CNT == "BGR" & Class == "2" ~ 3,
           CNT == "BGR" & Class == "3" ~ 1,
           CNT == "CHL" & Class == "1" ~ 1,
           CNT == "CHL" & Class == "2" ~ 3,
           CNT == "CHL" & Class == "3" ~ 2,
           CNT == "COL" & Class == "1" ~ 3,
           CNT == "COL" & Class == "2" ~ 1,
           CNT == "COL" & Class == "3" ~ 2,
           CNT == "DNK" & Class == "1" ~ 1,
           CNT == "DNK" & Class == "2" ~ 3,
           CNT == "DNK" & Class == "3" ~ 2,
           CNT == "DOM" & Class == "1" ~ 2,
           CNT == "DOM" & Class == "2" ~ 1,
           CNT == "DOM" & Class == "3" ~ 3,
           CNT == "EST" & Class == "1" ~ 2,
           CNT == "EST" & Class == "2" ~ 1,
           CNT == "EST" & Class == "3" ~ 3,
           CNT == "FIN" & Class == "1" ~ 2,
           CNT == "FIN" & Class == "2" ~ 3,
           CNT == "FIN" & Class == "3" ~ 1,
           CNT == "HKG" & Class == "1" ~ 1,
           CNT == "HKG" & Class == "2" ~ 3,
           CNT == "HKG" & Class == "3" ~ 2,
           CNT == "ITA" & Class == "1" ~ 3,
           CNT == "ITA" & Class == "2" ~ 2,
           CNT == "ITA" & Class == "3" ~ 1,
           CNT == "KOR" & Class == "1" ~ 1,
           CNT == "KOR" & Class == "2" ~ 3,
           CNT == "KOR" & Class == "3" ~ 2,
           CNT == "LTU" & Class == "1" ~ 2,
           CNT == "LTU" & Class == "2" ~ 3,
           CNT == "LTU" & Class == "3" ~ 1,
           CNT == "LVA" & Class == "1" ~ 3,
           CNT == "LVA" & Class == "2" ~ 2,
           CNT == "LVA" & Class == "3" ~ 1,
           CNT == "MEX" & Class == "1" ~ 1,
           CNT == "MEX" & Class == "2" ~ 3,
           CNT == "MEX" & Class == "3" ~ 2,
           CNT == "MLT" & Class == "1" ~ 2,
           CNT == "MLT" & Class == "2" ~ 1,
           CNT == "MLT" & Class == "3" ~ 3,
           CNT == "NLD" & Class == "1" ~ 2,
           CNT == "NLD" & Class == "2" ~ 3,
           CNT == "NLD" & Class == "3" ~ 1,
           CNT == "NOR" & Class == "1" ~ 2,
           CNT == "NOR" & Class == "2" ~ 3,
           CNT == "NOR" & Class == "3" ~ 1,
           CNT == "RUS" & Class == "1" ~ 1,
           CNT == "RUS" & Class == "2" ~ 3,
           CNT == "RUS" & Class == "3" ~ 2,
           CNT == "SVN" & Class == "1" ~ 2,
           CNT == "SVN" & Class == "2" ~ 1,
           CNT == "SVN" & Class == "3" ~ 3,
           CNT == "SWE" & Class == "1" ~ 2,
           CNT == "SWE" & Class == "2" ~ 3,
           CNT == "SWE" & Class == "3" ~ 1,
           CNT == "TWN" & Class == "1" ~ 1,
           CNT == "TWN" & Class == "2" ~ 3,
           CNT == "TWN" & Class == "3" ~ 2
           #----
         ), levels = c("1", "2", "3"), labels = classes3M2))
#----C3
ByCNTlca3C3 <- ByCountry_lca3[[3]] 
CNT <- sort(as.character(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C3","COUNTRY"])$COUNTRY))
ByCNTlca_C3cl3 <- NULL
warnByCNTlca_C3cl3 <- NULL

for (j in 1:length(ByCNTlca3C3)) {
  warnByCNTlca_C3cl3[CNT[j]] <- eval(parse(text=paste0("list(",CNT[j], " = ByCNTlca3C3$data.Mplus.models.ByCountry.lca_",CNT[j],"_C3cl3.out$warnings)")))
  
  lca <- cbind(CNT = CNT[j], Cycle = "2016",eval(parse(text=paste0("ByCNTlca3C3$data.Mplus.models.ByCountry.lca_",CNT[j],"_C3cl3.out$parameters$probability.scale"))))
  ByCNTlca_C3cl3 <- rbind(ByCNTlca_C3cl3, lca)
}


ByCNTlca_C3cl3 <- ByCNTlca_C3cl3 %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>% 
  mutate(Group = CNT, ClassN = factor(case_when(
           #-----
           CNT == "BFL" & Class == "1" ~ 2,
           CNT == "BFL" & Class == "2" ~ 3,
           CNT == "BFL" & Class == "3" ~ 1,
           CNT == "BGR" & Class == "1" ~ 1,
           CNT == "BGR" & Class == "2" ~ 3,
           CNT == "BGR" & Class == "3" ~ 2,
           CNT == "CHL" & Class == "1" ~ 2,
           CNT == "CHL" & Class == "2" ~ 1,
           CNT == "CHL" & Class == "3" ~ 3,
           CNT == "COL" & Class == "1" ~ 1,
           CNT == "COL" & Class == "2" ~ 2,
           CNT == "COL" & Class == "3" ~ 3,
           CNT == "DNK" & Class == "1" ~ 1,
           CNT == "DNK" & Class == "2" ~ 2,
           CNT == "DNK" & Class == "3" ~ 3,
           CNT == "DOM" & Class == "1" ~ 1,
           CNT == "DOM" & Class == "2" ~ 3,
           CNT == "DOM" & Class == "3" ~ 2,
           CNT == "EST" & Class == "1" ~ 2,
           CNT == "EST" & Class == "2" ~ 3,
           CNT == "EST" & Class == "3" ~ 1,
           CNT == "FIN" & Class == "1" ~ 1,
           CNT == "FIN" & Class == "2" ~ 3,
           CNT == "FIN" & Class == "3" ~ 2,
           CNT == "HKG" & Class == "1" ~ 2,
           CNT == "HKG" & Class == "2" ~ 3,
           CNT == "HKG" & Class == "3" ~ 1,
           CNT == "ITA" & Class == "1" ~ 2,
           CNT == "ITA" & Class == "2" ~ 1,
           CNT == "ITA" & Class == "3" ~ 3,
           CNT == "KOR" & Class == "1" ~ 1,
           CNT == "KOR" & Class == "2" ~ 3,
           CNT == "KOR" & Class == "3" ~ 2,
           CNT == "LTU" & Class == "1" ~ 1,
           CNT == "LTU" & Class == "2" ~ 3,
           CNT == "LTU" & Class == "3" ~ 2,
           CNT == "LVA" & Class == "1" ~ 1,
           CNT == "LVA" & Class == "2" ~ 3,
           CNT == "LVA" & Class == "3" ~ 2,
           CNT == "MEX" & Class == "1" ~ 3,
           CNT == "MEX" & Class == "2" ~ 2,
           CNT == "MEX" & Class == "3" ~ 1,
           CNT == "MLT" & Class == "1" ~ 1,
           CNT == "MLT" & Class == "2" ~ 2,
           CNT == "MLT" & Class == "3" ~ 3,
           CNT == "NLD" & Class == "1" ~ 2,
           CNT == "NLD" & Class == "2" ~ 3,
           CNT == "NLD" & Class == "3" ~ 1,
           CNT == "NOR" & Class == "1" ~ 3,
           CNT == "NOR" & Class == "2" ~ 2,
           CNT == "NOR" & Class == "3" ~ 1,
           CNT == "RUS" & Class == "1" ~ 2,
           CNT == "RUS" & Class == "2" ~ 1,
           CNT == "RUS" & Class == "3" ~ 3,
           CNT == "SVN" & Class == "1" ~ 2,
           CNT == "SVN" & Class == "2" ~ 3,
           CNT == "SVN" & Class == "3" ~ 1,
           CNT == "SWE" & Class == "1" ~ 1,
           CNT == "SWE" & Class == "2" ~ 3,
           CNT == "SWE" & Class == "3" ~ 2,
           CNT == "TWN" & Class == "1" ~ 1,
           CNT == "TWN" & Class == "2" ~ 3,
           CNT == "TWN" & Class == "3" ~ 2
           #----
         ), levels = c("1", "2", "3"), labels = classes3M2))

ByCNTlca3 <- rbind(ByCNTlca_C1cl3,
                   ByCNTlca_C2cl3,
                   ByCNTlca_C3cl3)


warningsByCNTlca <- list(C1 = warnByCNTlca_C1cl3,
                 C2 = warnByCNTlca_C2cl3,
                 C3 = warnByCNTlca_C3cl3)

cat('\n')
cat('\n')
wr <- names(warningsByCNTlca[grep("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED",warningsByCNTlca)])
if (length(wr) == 0) print("ALL BEST LOGLIKELIHOOD VALUES WERE REPLICATED") else
  cat("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED for the following cases:  \n", wr)
cat('\n')
cat('\n')
#---
ByGroupgraphclass(ByCNTlca3, nclass = 3, orden = c(5,2,1,3,4,6) ,title = "LCA with 3 classes")
cat('\n')
cat('\n')

#To create multigroup .inp file
# ByCNTlca3 %>% filter((value > 0.99 | value < 0.01) & Cycle == 1999) %>% group_by(CNT, Cycle, ClassN,  param) %>% filter(category == 2) %>% 
#   select(CNT, Cycle, param, ClassN, value) %>% 
#   reshape2::dcast(CNT + param + Cycle ~ ClassN) 


ByCNTlcign1 <- VarClass(ByCNTlca3)
ByCNTlcign <- ByCNTlcign1 %>% group_by(CNT, Cycle, ClassN,  param) %>% filter(category == 2) %>% 
  select(CNT, Cycle, param, ClassN, value) %>% 
  mutate(value = cell_spec(value, color = ifelse(value > 0.8, "blue", "red"))) %>% 
  reshape2::dcast(CNT + param + Cycle ~ ClassN) 

ByCNTlcign %>%
  kbl(caption = "Probabilities to Agree each item by Class and Country", booktabs = TRUE, longtable = TRUE, 
      align = "lllrrrrrr", row.names = FALSE, digits = 3, col.names = c("Country","Item", "Year", classes3M2), escape = FALSE) %>%
  kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"), 
                position = "left", font_size = 10, full_width = FALSE) %>% 
  column_spec(2, width = "16em") %>%  
  column_spec(c(1,3), width = "4em") %>%  
  column_spec(4:6, width = "4em") %>%  
  collapse_rows(c(1,2), valign = "top") %>%
  print()
cat('\n')
cat('\n')

#Extracting patterns
CNT1 <- sort(as.character(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C1","COUNTRY"])$COUNTRY))
SizeByCNTlca_C1cl3 <- NULL
for (j in 1:length(ByCNTlca3C1)) {
  lca <- cbind(CNT = CNT1[j], Cycle = "1999",eval(parse(text=paste0("ByCNTlca3C1$data.Mplus.models.ByCountry.lca_",CNT1[j],"_C1cl3.out$class_counts$modelEstimated"))))
  SizeByCNTlca_C1cl3 <- rbind(SizeByCNTlca_C1cl3, lca)
}
CNT2 <- sort(as.character(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C2","COUNTRY"])$COUNTRY))
SizeByCNTlca_C2cl3 <- NULL
for (j in 1:length(ByCNTlca3C2)) {
  lca <- cbind(CNT = CNT2[j], Cycle = "2009",eval(parse(text=paste0("ByCNTlca3C2$data.Mplus.models.ByCountry.lca_",CNT2[j],"_C2cl3.out$class_counts$modelEstimated"))))
  SizeByCNTlca_C2cl3 <- rbind(SizeByCNTlca_C2cl3, lca)
}
CNT3 <- sort(as.character(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C3","COUNTRY"])$COUNTRY))
SizeByCNTlca_C3cl3 <- NULL
for (j in 1:length(ByCNTlca3C3)) {
  lca <- cbind(CNT = CNT3[j], Cycle = "2019",eval(parse(text=paste0("ByCNTlca3C3$data.Mplus.models.ByCountry.lca_",CNT3[j],"_C3cl3.out$class_counts$modelEstimated"))))
  SizeByCNTlca_C3cl3 <- rbind(SizeByCNTlca_C3cl3, lca)
}

sizelca3 <- full_join(
  full_join(SizeByCNTlca_C1cl3 %>% 
              rename_with(~ c("1999")[which(c("count") == .x)], .cols = c("count")) %>% 
              mutate(ClassN = factor(
                case_when(
                  #-----
                  CNT == "BGR" & class == "1" ~ 1,
                  CNT == "BGR" & class == "2" ~ 3,
                  CNT == "BGR" & class == "3" ~ 2,
                  CNT == "CHL" & class == "1" ~ 3,
                  CNT == "CHL" & class == "2" ~ 2,
                  CNT == "CHL" & class == "3" ~ 1,
                  CNT == "COL" & class == "1" ~ 2,
                  CNT == "COL" & class == "2" ~ 3,
                  CNT == "COL" & class == "3" ~ 1,
                  CNT == "DNK" & class == "1" ~ 3,
                  CNT == "DNK" & class == "2" ~ 2,
                  CNT == "DNK" & class == "3" ~ 1,
                  CNT == "EST" & class == "1" ~ 2,
                  CNT == "EST" & class == "2" ~ 1,
                  CNT == "EST" & class == "3" ~ 3,
                  CNT == "FIN" & class == "1" ~ 1,
                  CNT == "FIN" & class == "2" ~ 2,
                  CNT == "FIN" & class == "3" ~ 3,
                  CNT == "HKG" & class == "1" ~ 2,
                  CNT == "HKG" & class == "2" ~ 3,
                  CNT == "HKG" & class == "3" ~ 1,
                  CNT == "ITA" & class == "1" ~ 2,
                  CNT == "ITA" & class == "2" ~ 3,
                  CNT == "ITA" & class == "3" ~ 1,
                  CNT == "LTU" & class == "1" ~ 2,
                  CNT == "LTU" & class == "2" ~ 1,
                  CNT == "LTU" & class == "3" ~ 3,
                  CNT == "LVA" & class == "1" ~ 2,
                  CNT == "LVA" & class == "2" ~ 3,
                  CNT == "LVA" & class == "3" ~ 1,
                  CNT == "NOR" & class == "1" ~ 3,
                  CNT == "NOR" & class == "2" ~ 2,
                  CNT == "NOR" & class == "3" ~ 1,
                  CNT == "RUS" & class == "1" ~ 2,
                  CNT == "RUS" & class == "2" ~ 1,
                  CNT == "RUS" & class == "3" ~ 3,
                  CNT == "SVN" & class == "1" ~ 1,
                  CNT == "SVN" & class == "2" ~ 2,
                  CNT == "SVN" & class == "3" ~ 3,
                  CNT == "SWE" & class == "1" ~ 3,
                  CNT == "SWE" & class == "2" ~ 2,
                  CNT == "SWE" & class == "3" ~ 1
                  #----
                ), levels = 1:3, labels = classes3M2)) %>% dplyr::select(-proportion, -class, -Cycle) ,
            SizeByCNTlca_C2cl3 %>% 
              rename_with(~ c("2009")[which(c("count") == .x)], .cols = c("count")) %>% 
              mutate(ClassN = factor(case_when(
                #-----
                CNT == "BFL" & class == "1" ~ 1,
                CNT == "BFL" & class == "2" ~ 3,
                CNT == "BFL" & class == "3" ~ 2,
                CNT == "BGR" & class == "1" ~ 2,
                CNT == "BGR" & class == "2" ~ 3,
                CNT == "BGR" & class == "3" ~ 1,
                CNT == "CHL" & class == "1" ~ 1,
                CNT == "CHL" & class == "2" ~ 3,
                CNT == "CHL" & class == "3" ~ 2,
                CNT == "COL" & class == "1" ~ 3,
                CNT == "COL" & class == "2" ~ 1,
                CNT == "COL" & class == "3" ~ 2,
                CNT == "DNK" & class == "1" ~ 1,
                CNT == "DNK" & class == "2" ~ 3,
                CNT == "DNK" & class == "3" ~ 2,
                CNT == "DOM" & class == "1" ~ 2,
                CNT == "DOM" & class == "2" ~ 1,
                CNT == "DOM" & class == "3" ~ 3,
                CNT == "EST" & class == "1" ~ 2,
                CNT == "EST" & class == "2" ~ 1,
                CNT == "EST" & class == "3" ~ 3,
                CNT == "FIN" & class == "1" ~ 2,
                CNT == "FIN" & class == "2" ~ 3,
                CNT == "FIN" & class == "3" ~ 1,
                CNT == "HKG" & class == "1" ~ 1,
                CNT == "HKG" & class == "2" ~ 3,
                CNT == "HKG" & class == "3" ~ 2,
                CNT == "ITA" & class == "1" ~ 3,
                CNT == "ITA" & class == "2" ~ 2,
                CNT == "ITA" & class == "3" ~ 1,
                CNT == "KOR" & class == "1" ~ 1,
                CNT == "KOR" & class == "2" ~ 3,
                CNT == "KOR" & class == "3" ~ 2,
                CNT == "LTU" & class == "1" ~ 2,
                CNT == "LTU" & class == "2" ~ 3,
                CNT == "LTU" & class == "3" ~ 1,
                CNT == "LVA" & class == "1" ~ 3,
                CNT == "LVA" & class == "2" ~ 2,
                CNT == "LVA" & class == "3" ~ 1,
                CNT == "MEX" & class == "1" ~ 1,
                CNT == "MEX" & class == "2" ~ 3,
                CNT == "MEX" & class == "3" ~ 2,
                CNT == "MLT" & class == "1" ~ 2,
                CNT == "MLT" & class == "2" ~ 1,
                CNT == "MLT" & class == "3" ~ 3,
                CNT == "NLD" & class == "1" ~ 2,
                CNT == "NLD" & class == "2" ~ 3,
                CNT == "NLD" & class == "3" ~ 1,
                CNT == "NOR" & class == "1" ~ 2,
                CNT == "NOR" & class == "2" ~ 3,
                CNT == "NOR" & class == "3" ~ 1,
                CNT == "RUS" & class == "1" ~ 1,
                CNT == "RUS" & class == "2" ~ 3,
                CNT == "RUS" & class == "3" ~ 2,
                CNT == "SVN" & class == "1" ~ 2,
                CNT == "SVN" & class == "2" ~ 1,
                CNT == "SVN" & class == "3" ~ 3,
                CNT == "SWE" & class == "1" ~ 2,
                CNT == "SWE" & class == "2" ~ 3,
                CNT == "SWE" & class == "3" ~ 1,
                CNT == "TWN" & class == "1" ~ 1,
                CNT == "TWN" & class == "2" ~ 3,
                CNT == "TWN" & class == "3" ~ 2
                #----
              ), levels = 1:3, labels = classes3M2)) %>% dplyr::select(-proportion, -class, -Cycle), by = c("CNT", "ClassN")),
  SizeByCNTlca_C3cl3 %>%
    rename_with(~ c("2016")[which(c("count") == .x)], .cols = c("count")) %>% 
    mutate(ClassN = factor(case_when(
      #-----
      CNT == "BFL" & class == "1" ~ 2,
      CNT == "BFL" & class == "2" ~ 3,
      CNT == "BFL" & class == "3" ~ 1,
      CNT == "BGR" & class == "1" ~ 1,
      CNT == "BGR" & class == "2" ~ 3,
      CNT == "BGR" & class == "3" ~ 2,
      CNT == "CHL" & class == "1" ~ 2,
      CNT == "CHL" & class == "2" ~ 1,
      CNT == "CHL" & class == "3" ~ 3,
      CNT == "COL" & class == "1" ~ 1,
      CNT == "COL" & class == "2" ~ 2,
      CNT == "COL" & class == "3" ~ 3,
      CNT == "DNK" & class == "1" ~ 1,
      CNT == "DNK" & class == "2" ~ 2,
      CNT == "DNK" & class == "3" ~ 3,
      CNT == "DOM" & class == "1" ~ 1,
      CNT == "DOM" & class == "2" ~ 3,
      CNT == "DOM" & class == "3" ~ 2,
      CNT == "EST" & class == "1" ~ 2,
      CNT == "EST" & class == "2" ~ 3,
      CNT == "EST" & class == "3" ~ 1,
      CNT == "FIN" & class == "1" ~ 1,
      CNT == "FIN" & class == "2" ~ 3,
      CNT == "FIN" & class == "3" ~ 2,
      CNT == "HKG" & class == "1" ~ 2,
      CNT == "HKG" & class == "2" ~ 3,
      CNT == "HKG" & class == "3" ~ 1,
      CNT == "ITA" & class == "1" ~ 2,
      CNT == "ITA" & class == "2" ~ 1,
      CNT == "ITA" & class == "3" ~ 3,
      CNT == "KOR" & class == "1" ~ 1,
      CNT == "KOR" & class == "2" ~ 3,
      CNT == "KOR" & class == "3" ~ 2,
      CNT == "LTU" & class == "1" ~ 1,
      CNT == "LTU" & class == "2" ~ 3,
      CNT == "LTU" & class == "3" ~ 2,
      CNT == "LVA" & class == "1" ~ 1,
      CNT == "LVA" & class == "2" ~ 3,
      CNT == "LVA" & class == "3" ~ 2,
      CNT == "MEX" & class == "1" ~ 3,
      CNT == "MEX" & class == "2" ~ 2,
      CNT == "MEX" & class == "3" ~ 1,
      CNT == "MLT" & class == "1" ~ 1,
      CNT == "MLT" & class == "2" ~ 2,
      CNT == "MLT" & class == "3" ~ 3,
      CNT == "NLD" & class == "1" ~ 2,
      CNT == "NLD" & class == "2" ~ 3,
      CNT == "NLD" & class == "3" ~ 1,
      CNT == "NOR" & class == "1" ~ 1,
      CNT == "NOR" & class == "2" ~ 3,
      CNT == "NOR" & class == "3" ~ 2,
      CNT == "RUS" & class == "1" ~ 2,
      CNT == "RUS" & class == "2" ~ 1,
      CNT == "RUS" & class == "3" ~ 3,
      CNT == "SVN" & class == "1" ~ 2,
      CNT == "SVN" & class == "2" ~ 3,
      CNT == "SVN" & class == "3" ~ 1,
      CNT == "SWE" & class == "1" ~ 1,
      CNT == "SWE" & class == "2" ~ 3,
      CNT == "SWE" & class == "3" ~ 2,
      CNT == "TWN" & class == "1" ~ 1,
      CNT == "TWN" & class == "2" ~ 3,
      CNT == "TWN" & class == "3" ~ 2
      #----
    ), levels = 1:3, labels = classes3M2)) %>% dplyr::select(-proportion, -class, -Cycle) , by = c("CNT", "ClassN")) %>% 
  select(CNT, ClassN, `1999`, `2009`, `2016`)

sizelca3 <- sizelca3 %>% reshape2::melt(id.vars = c("CNT", "ClassN"), variable.name = "Cycle") %>% 
  arrange(CNT, Cycle) %>% 
  group_by(CNT, Cycle) %>%
  mutate(countT= sum(value, na.rm = TRUE)) %>%
  group_by(ClassN) %>%
  mutate(per=value/countT, Group = CNT) %>% dplyr::select(CNT, Cycle, Group, ClassN, per) 

ByGroupHighProbMG(ByCNTlca3, sizelca3, orden = c(5,2,1,3,4,6), n = 1:2, title = "Probabilities of Agree and size for each class \nin country by Cycle")
cat('\n')
cat('\n')

sizelca3 %>% 
  reshape2::dcast(CNT + Cycle ~ ClassN) %>% 
  kbl(caption = "Size three classes Country", booktabs = TRUE, longtable = TRUE, row.names = FALSE, 
      digits = 3, col.names = c("Country", "Year", classes3M2)) %>%
  kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"),
                position = "left", font_size = 11, full_width = FALSE) %>%     
  column_spec(3:5, width = "8em") %>% 
  collapse_rows(1, valign = "top") %>% 
  print()
cat('\n')
cat('\n')

cat('\\newpage')
cat('\n')
cat('\n')
cat('### Country Multigroup analysis for Gender by cycle  \n')
cat('\n')
cat('\n')
#Graph_modelfitByGroup("ByCountry_MGGndr")

# ----------Results with 2C MG Gender by Cntry and Cycle ----

cat('\n')
cat('\n')
cat('#### Results with 2-classes  \n')
cat('\n')
cat('\n')
#----C1
ByCNTMGGndC1 <- ByCountry_MGGndr2[[1]] 
CNT <- sort(as.character(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C1","COUNTRY"])$COUNTRY))
ByCNTMGGndr_C1cl2 <- NULL
warnByCNTMGGndr_C1cl2 <- NULL

for (j in 1:length(ByCNTMGGndC1)) {
  warnByCNTMGGndr_C1cl2[CNT[j]] <- eval(parse(text=paste0("list(",CNT[j], " = ByCNTMGGndC1$data.Mplus.models.ByCountry.MGGndr_",CNT[j],"_C1cl2.out$warnings)")))
  
  BycntMGGndr <- cbind(CNT = CNT[j], Cycle = "1999",eval(parse(text=paste0("ByCNTMGGndC1$data.Mplus.models.ByCountry.MGGndr_",CNT[j],"_C1cl2.out$parameters$probability.scale"))))
  ByCNTMGGndr_C1cl2 <- rbind(ByCNTMGGndr_C1cl2, BycntMGGndr)
}

ByCNTMGGndr_C1cl2 <- ByCNTMGGndr_C1cl2 %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>% 
  mutate(Group = factor(sub('\\..*', '', Class), 
                        levels = c("1", "2"), labels = c("Boy", "Girl")), 
         ClassN = factor(case_when(
           #-----
           CNT == "BGR" & Class == "1.1" ~ 1,
           CNT == "BGR" & Class == "1.2" ~ 2,
           CNT == "BGR" & Class == "2.1" ~ 1,
           CNT == "BGR" & Class == "2.2" ~ 2, 
           CNT == "CHL" & Class == "1.1" ~ 1,
           CNT == "CHL" & Class == "1.2" ~ 2,
           CNT == "CHL" & Class == "2.1" ~ 2,
           CNT == "CHL" & Class == "2.2" ~ 1,
           CNT == "COL" & Class == "1.1" ~ 1,
           CNT == "COL" & Class == "1.2" ~ 2,
           CNT == "COL" & Class == "2.1" ~ 2,
           CNT == "COL" & Class == "2.2" ~ 1, 
           CNT == "DNK" & Class == "1.1" ~ 2,
           CNT == "DNK" & Class == "1.2" ~ 1,
           CNT == "DNK" & Class == "2.1" ~ 1,
           CNT == "DNK" & Class == "2.2" ~ 2,
           CNT == "EST" & Class == "1.1" ~ 2,
           CNT == "EST" & Class == "1.2" ~ 1,
           CNT == "EST" & Class == "2.1" ~ 2,
           CNT == "EST" & Class == "2.2" ~ 1, 
           CNT == "FIN" & Class == "1.1" ~ 1,
           CNT == "FIN" & Class == "1.2" ~ 2,
           CNT == "FIN" & Class == "2.1" ~ 1,
           CNT == "FIN" & Class == "2.2" ~ 2,
           CNT == "HKG" & Class == "1.1" ~ 2,
           CNT == "HKG" & Class == "1.2" ~ 1,
           CNT == "HKG" & Class == "2.1" ~ 1,
           CNT == "HKG" & Class == "2.2" ~ 2, 
           CNT == "ITA" & Class == "1.1" ~ 1,
           CNT == "ITA" & Class == "1.2" ~ 2,
           CNT == "ITA" & Class == "2.1" ~ 2,
           CNT == "ITA" & Class == "2.2" ~ 1,
           CNT == "LTU" & Class == "1.1" ~ 1,
           CNT == "LTU" & Class == "1.2" ~ 2,
           CNT == "LTU" & Class == "2.1" ~ 2,
           CNT == "LTU" & Class == "2.2" ~ 1, 
           CNT == "LVA" & Class == "1.1" ~ 1,
           CNT == "LVA" & Class == "1.2" ~ 2,
           CNT == "LVA" & Class == "2.1" ~ 2,
           CNT == "LVA" & Class == "2.2" ~ 1,
           CNT == "NOR" & Class == "1.1" ~ 2,
           CNT == "NOR" & Class == "1.2" ~ 1,
           CNT == "NOR" & Class == "2.1" ~ 2,
           CNT == "NOR" & Class == "2.2" ~ 1, 
           CNT == "RUS" & Class == "1.1" ~ 2,
           CNT == "RUS" & Class == "1.2" ~ 1,
           CNT == "RUS" & Class == "2.1" ~ 2,
           CNT == "RUS" & Class == "2.2" ~ 1,
           CNT == "SVN" & Class == "1.1" ~ 1,
           CNT == "SVN" & Class == "1.2" ~ 2,
           CNT == "SVN" & Class == "2.1" ~ 2,
           CNT == "SVN" & Class == "2.2" ~ 1, 
           CNT == "SWE" & Class == "1.1" ~ 1,
           CNT == "SWE" & Class == "1.2" ~ 2,
           CNT == "SWE" & Class == "2.1" ~ 1,
           CNT == "SWE" & Class == "2.2" ~ 2
           #----
         ), levels = c("1", "2"), labels = classes2M2))
#----C2
ByCNTMGGndC2 <- ByCountry_MGGndr2[[2]] 
CNT <- sort(as.character(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C2","COUNTRY"])$COUNTRY))
ByCNTMGGndr_C2cl2 <- NULL
warnByCNTMGGndr_C2cl2 <- NULL

for (j in 1:length(ByCNTMGGndC2)) {
  warnByCNTMGGndr_C2cl2[CNT[j]] <- eval(parse(text=paste0("list(",CNT[j], " = ByCNTMGGndC2$data.Mplus.models.ByCountry.MGGndr_",CNT[j],"_C2cl2.out$warnings)")))
  
  BycntMGGndr <- cbind(CNT = CNT[j], Cycle = "2009",eval(parse(text=paste0("ByCNTMGGndC2$data.Mplus.models.ByCountry.MGGndr_",CNT[j],"_C2cl2.out$parameters$probability.scale"))))
  ByCNTMGGndr_C2cl2 <- rbind(ByCNTMGGndr_C2cl2, BycntMGGndr)
}

ByCNTMGGndr_C2cl2 <- ByCNTMGGndr_C2cl2 %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x))%>% 
  mutate(Group = factor(sub('\\..*', '', Class), 
                        levels = c("1", "2"), labels = c("Boy", "Girl")), 
         ClassN = factor(case_when(
           #-----
           CNT == "BFL" & Class == "1.1" ~ 1,
           CNT == "BFL" & Class == "1.2" ~ 2,
           CNT == "BFL" & Class == "2.1" ~ 2,
           CNT == "BFL" & Class == "2.2" ~ 1, 
           CNT == "BGR" & Class == "1.1" ~ 1,
           CNT == "BGR" & Class == "1.2" ~ 2,
           CNT == "BGR" & Class == "2.1" ~ 2,
           CNT == "BGR" & Class == "2.2" ~ 1, 
           CNT == "CHL" & Class == "1.1" ~ 2,
           CNT == "CHL" & Class == "1.2" ~ 1,
           CNT == "CHL" & Class == "2.1" ~ 2,
           CNT == "CHL" & Class == "2.2" ~ 1,
           CNT == "COL" & Class == "1.1" ~ 1,
           CNT == "COL" & Class == "1.2" ~ 2,
           CNT == "COL" & Class == "2.1" ~ 1,
           CNT == "COL" & Class == "2.2" ~ 2, 
           CNT == "DNK" & Class == "1.1" ~ 2,
           CNT == "DNK" & Class == "1.2" ~ 1,
           CNT == "DNK" & Class == "2.1" ~ 1,
           CNT == "DNK" & Class == "2.2" ~ 2, 
           CNT == "DOM" & Class == "1.1" ~ 2,
           CNT == "DOM" & Class == "1.2" ~ 1,
           CNT == "DOM" & Class == "2.1" ~ 2,
           CNT == "DOM" & Class == "2.2" ~ 1,
           CNT == "EST" & Class == "1.1" ~ 1,
           CNT == "EST" & Class == "1.2" ~ 2,
           CNT == "EST" & Class == "2.1" ~ 1,
           CNT == "EST" & Class == "2.2" ~ 2, 
           CNT == "FIN" & Class == "1.1" ~ 2,
           CNT == "FIN" & Class == "1.2" ~ 1,
           CNT == "FIN" & Class == "2.1" ~ 1,
           CNT == "FIN" & Class == "2.2" ~ 2,
           CNT == "HKG" & Class == "1.1" ~ 1,
           CNT == "HKG" & Class == "1.2" ~ 2,
           CNT == "HKG" & Class == "2.1" ~ 1,
           CNT == "HKG" & Class == "2.2" ~ 2, 
           CNT == "ITA" & Class == "1.1" ~ 1,
           CNT == "ITA" & Class == "1.2" ~ 2,
           CNT == "ITA" & Class == "2.1" ~ 2,
           CNT == "ITA" & Class == "2.2" ~ 1, 
           CNT == "KOR" & Class == "1.1" ~ 1,
           CNT == "KOR" & Class == "1.2" ~ 2,
           CNT == "KOR" & Class == "2.1" ~ 2,
           CNT == "KOR" & Class == "2.2" ~ 1,
           CNT == "LTU" & Class == "1.1" ~ 2,
           CNT == "LTU" & Class == "1.2" ~ 1,
           CNT == "LTU" & Class == "2.1" ~ 2,
           CNT == "LTU" & Class == "2.2" ~ 1, 
           CNT == "LVA" & Class == "1.1" ~ 1,
           CNT == "LVA" & Class == "1.2" ~ 2,
           CNT == "LVA" & Class == "2.1" ~ 2,
           CNT == "LVA" & Class == "2.2" ~ 1,
           CNT == "MEX" & Class == "1.1" ~ 1,
           CNT == "MEX" & Class == "1.2" ~ 2,
           CNT == "MEX" & Class == "2.1" ~ 2,
           CNT == "MEX" & Class == "2.2" ~ 1,
           CNT == "MLT" & Class == "1.1" ~ 1,
           CNT == "MLT" & Class == "1.2" ~ 2,
           CNT == "MLT" & Class == "2.1" ~ 2,
           CNT == "MLT" & Class == "2.2" ~ 1, 
           CNT == "NLD" & Class == "1.1" ~ 2,
           CNT == "NLD" & Class == "1.2" ~ 1,
           CNT == "NLD" & Class == "2.1" ~ 1,
           CNT == "NLD" & Class == "2.2" ~ 2, 
           CNT == "NOR" & Class == "1.1" ~ 1,
           CNT == "NOR" & Class == "1.2" ~ 2,
           CNT == "NOR" & Class == "2.1" ~ 1,
           CNT == "NOR" & Class == "2.2" ~ 2, 
           CNT == "RUS" & Class == "1.1" ~ 2,
           CNT == "RUS" & Class == "1.2" ~ 1,
           CNT == "RUS" & Class == "2.1" ~ 1,
           CNT == "RUS" & Class == "2.2" ~ 2,
           CNT == "SVN" & Class == "1.1" ~ 1,
           CNT == "SVN" & Class == "1.2" ~ 2,
           CNT == "SVN" & Class == "2.1" ~ 1,
           CNT == "SVN" & Class == "2.2" ~ 2, 
           CNT == "SWE" & Class == "1.1" ~ 1,
           CNT == "SWE" & Class == "1.2" ~ 2,
           CNT == "SWE" & Class == "2.1" ~ 2,
           CNT == "SWE" & Class == "2.2" ~ 1, 
           CNT == "TWN" & Class == "1.1" ~ 1,
           CNT == "TWN" & Class == "1.2" ~ 2,
           CNT == "TWN" & Class == "2.1" ~ 1,
           CNT == "TWN" & Class == "2.2" ~ 2
           #----
         ), levels = c("1", "2"), labels = classes2M2))
#----C3
ByCNTMGGndC3 <- ByCountry_MGGndr2[[3]] 
CNT <- sort(as.character(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C3","COUNTRY"])$COUNTRY))
ByCNTMGGndr_C3cl2 <- NULL
warnByCNTMGGndr_C3cl2 <- NULL

for (j in 1:length(ByCNTMGGndC3)) {
  warnByCNTMGGndr_C3cl2[CNT[j]] <- eval(parse(text=paste0("list(",CNT[j], " = ByCNTMGGndC3$data.Mplus.models.ByCountry.MGGndr_",CNT[j],"_C3cl2.out$warnings)")))
  
  BycntMGGndr <- cbind(CNT = CNT[j], Cycle = "2016",eval(parse(text=paste0("ByCNTMGGndC3$data.Mplus.models.ByCountry.MGGndr_",CNT[j],"_C3cl2.out$parameters$probability.scale"))))
  ByCNTMGGndr_C3cl2 <- rbind(ByCNTMGGndr_C3cl2, BycntMGGndr)
}

ByCNTMGGndr_C3cl2 <- ByCNTMGGndr_C3cl2 %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>% 
  mutate(Group = factor(sub('\\..*', '', Class), 
                        levels = c("1", "2"), labels = c("Boy", "Girl")), 
         ClassN = factor(case_when(
           #-----
           CNT == "BFL" & Class == "1.1" ~ 1,
           CNT == "BFL" & Class == "1.2" ~ 2,
           CNT == "BFL" & Class == "2.1" ~ 2,
           CNT == "BFL" & Class == "2.2" ~ 1, 
           CNT == "BGR" & Class == "1.1" ~ 1,
           CNT == "BGR" & Class == "1.2" ~ 2,
           CNT == "BGR" & Class == "2.1" ~ 2,
           CNT == "BGR" & Class == "2.2" ~ 1, 
           CNT == "CHL" & Class == "1.1" ~ 2,
           CNT == "CHL" & Class == "1.2" ~ 1,
           CNT == "CHL" & Class == "2.1" ~ 1,
           CNT == "CHL" & Class == "2.2" ~ 2,
           CNT == "COL" & Class == "1.1" ~ 2,
           CNT == "COL" & Class == "1.2" ~ 1,
           CNT == "COL" & Class == "2.1" ~ 2,
           CNT == "COL" & Class == "2.2" ~ 1, 
           CNT == "DNK" & Class == "1.1" ~ 1,
           CNT == "DNK" & Class == "1.2" ~ 2,
           CNT == "DNK" & Class == "2.1" ~ 2,
           CNT == "DNK" & Class == "2.2" ~ 1, 
           CNT == "DOM" & Class == "1.1" ~ 1,
           CNT == "DOM" & Class == "1.2" ~ 2,
           CNT == "DOM" & Class == "2.1" ~ 1,
           CNT == "DOM" & Class == "2.2" ~ 2,
           CNT == "EST" & Class == "1.1" ~ 1,
           CNT == "EST" & Class == "1.2" ~ 2,
           CNT == "EST" & Class == "2.1" ~ 2,
           CNT == "EST" & Class == "2.2" ~ 1, 
           CNT == "FIN" & Class == "1.1" ~ 2,
           CNT == "FIN" & Class == "1.2" ~ 1,
           CNT == "FIN" & Class == "2.1" ~ 2,
           CNT == "FIN" & Class == "2.2" ~ 1,
           CNT == "HKG" & Class == "1.1" ~ 1,
           CNT == "HKG" & Class == "1.2" ~ 2,
           CNT == "HKG" & Class == "2.1" ~ 1,
           CNT == "HKG" & Class == "2.2" ~ 2, 
           CNT == "ITA" & Class == "1.1" ~ 2,
           CNT == "ITA" & Class == "1.2" ~ 1,
           CNT == "ITA" & Class == "2.1" ~ 2,
           CNT == "ITA" & Class == "2.2" ~ 1, 
           CNT == "KOR" & Class == "1.1" ~ 1,
           CNT == "KOR" & Class == "1.2" ~ 2,
           CNT == "KOR" & Class == "2.1" ~ 2,
           CNT == "KOR" & Class == "2.2" ~ 1,
           CNT == "LTU" & Class == "1.1" ~ 1,
           CNT == "LTU" & Class == "1.2" ~ 2,
           CNT == "LTU" & Class == "2.1" ~ 2,
           CNT == "LTU" & Class == "2.2" ~ 1, 
           CNT == "LVA" & Class == "1.1" ~ 1,
           CNT == "LVA" & Class == "1.2" ~ 2,
           CNT == "LVA" & Class == "2.1" ~ 2,
           CNT == "LVA" & Class == "2.2" ~ 1,
           CNT == "MEX" & Class == "1.1" ~ 2,
           CNT == "MEX" & Class == "1.2" ~ 1,
           CNT == "MEX" & Class == "2.1" ~ 2,
           CNT == "MEX" & Class == "2.2" ~ 1,
           CNT == "MLT" & Class == "1.1" ~ 2,
           CNT == "MLT" & Class == "1.2" ~ 1,
           CNT == "MLT" & Class == "2.1" ~ 2,
           CNT == "MLT" & Class == "2.2" ~ 1, 
           CNT == "NLD" & Class == "1.1" ~ 2,
           CNT == "NLD" & Class == "1.2" ~ 1,
           CNT == "NLD" & Class == "2.1" ~ 2,
           CNT == "NLD" & Class == "2.2" ~ 1, 
           CNT == "NOR" & Class == "1.1" ~ 1,
           CNT == "NOR" & Class == "1.2" ~ 2,
           CNT == "NOR" & Class == "2.1" ~ 1,
           CNT == "NOR" & Class == "2.2" ~ 2, 
           CNT == "RUS" & Class == "1.1" ~ 1,
           CNT == "RUS" & Class == "1.2" ~ 2,
           CNT == "RUS" & Class == "2.1" ~ 1,
           CNT == "RUS" & Class == "2.2" ~ 2,
           CNT == "SVN" & Class == "1.1" ~ 2,
           CNT == "SVN" & Class == "1.2" ~ 1,
           CNT == "SVN" & Class == "2.1" ~ 2,
           CNT == "SVN" & Class == "2.2" ~ 1, 
           CNT == "SWE" & Class == "1.1" ~ 1,
           CNT == "SWE" & Class == "1.2" ~ 2,
           CNT == "SWE" & Class == "2.1" ~ 1,
           CNT == "SWE" & Class == "2.2" ~ 2, 
           CNT == "TWN" & Class == "1.1" ~ 1,
           CNT == "TWN" & Class == "1.2" ~ 2,
           CNT == "TWN" & Class == "2.1" ~ 2,
           CNT == "TWN" & Class == "2.2" ~ 1
           #----
         ), levels = c("1", "2"), labels = classes2M2))

ByCNTMGGndr3 <- rbind(ByCNTMGGndr_C1cl2,
                      ByCNTMGGndr_C2cl2,
                      ByCNTMGGndr_C3cl2)

warningsByCNTMGGndr <- list(C1 = warnByCNTMGGndr_C1cl2,
                            C2 = warnByCNTMGGndr_C2cl2,
                            C3 = warnByCNTMGGndr_C3cl2)

cat('\n')
cat('\n')
wr1 <- names(warningsByCNTMGGndr$C1[grep("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED",warningsByCNTMGGndr$C1)])
if (length(wr) == 0) print("ALL BEST LOGLIKELIHOOD VALUES WERE REPLICATED") else
  cat("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED for the following cases:  \n C1:  \n", wr1)
cat('\n')
cat('\n')
wr2 <- names(warningsByCNTMGGndr$C2[grep("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED",warningsByCNTMGGndr$C2)])
if (length(wr) == 0) print("ALL BEST LOGLIKELIHOOD VALUES WERE REPLICATED") else
  cat("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED for the following cases:  \n C2:  \n", wr2)
cat('\n')
cat('\n')

wr3 <- names(warningsByCNTMGGndr$C3[grep("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED",warningsByCNTMGGndr$C3)])
if (length(wr) == 0) print("ALL BEST LOGLIKELIHOOD VALUES WERE REPLICATED") else
  cat("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED for the following cases:  \n C3:  \n", wr3)
cat('\n')
cat('\n')

#---
ByGroupgraphclass(ByCNTMGGndr3, nclass = 2, orden = c(5,2,1,3,4,6) ,title = "MG Gender with 2 classes")
cat('\n')
cat('\n')

ByCNTlcign <- VarClass(ByCNTMGGndr3)
ByCNTlcign %>% group_by(CNT, Cycle, ClassN, Group, param) %>% filter(category == 2) %>% 
  select(CNT, Cycle, param, ClassN, Group, value) %>% 
  mutate(value = cell_spec(value, color = ifelse(value > 0.8, "blue", "red"))) %>% 
  reshape2::dcast(CNT + param + Cycle ~ Group + ClassN) %>%
  kbl(caption = "Probabilities to Agree each item by Class and Gender", booktabs = TRUE, longtable = TRUE, 
      align = "lllrrrrrr", row.names = FALSE, digits = 3, col.names = c("Country","Item", "Year", classes2M2, classes2M2), escape = FALSE) %>%
  kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"), 
                position = "left", font_size = 10, full_width = FALSE) %>% 
  column_spec(2, width = "12em") %>%  
  column_spec(c(1,3), width = "2em") %>%  
  column_spec(4:7, width = "4em") %>%  
  collapse_rows(c(1,2), valign = "top") %>%
  add_header_above(c(" "=1, " "=1," "=1, "Boy" = 2, "Girl" = 2)) %>% 
  print()
cat('\n')
cat('\n')

#Extracting patterns
CNT1 <- sort(as.character(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C1","COUNTRY"])$COUNTRY))
SizeByCNTMGGndr_C1cl2 <- NULL
for (j in 1:length(ByCNTMGGndC1)) {
  BycntMGGndr <- cbind(CNT = CNT1[j], Cycle = "1999",eval(parse(text=paste0("ByCNTMGGndC1$data.Mplus.models.ByCountry.MGGndr_",CNT1[j],"_C1cl2.out$class_counts$modelEstimated.patterns"))))
  SizeByCNTMGGndr_C1cl2 <- rbind(SizeByCNTMGGndr_C1cl2, BycntMGGndr)
}
CNT2 <- sort(as.character(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C2","COUNTRY"])$COUNTRY))
SizeByCNTMGGndr_C2cl2 <- NULL
for (j in 1:length(ByCNTMGGndC2)) {
  BycntMGGndr <- cbind(CNT = CNT2[j], Cycle = "2009",eval(parse(text=paste0("ByCNTMGGndC2$data.Mplus.models.ByCountry.MGGndr_",CNT2[j],"_C2cl2.out$class_counts$modelEstimated.patterns"))))
  SizeByCNTMGGndr_C2cl2 <- rbind(SizeByCNTMGGndr_C2cl2, BycntMGGndr)
}
CNT3 <- sort(as.character(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C3","COUNTRY"])$COUNTRY))
SizeByCNTMGGndr_C3cl2 <- NULL
for (j in 1:length(ByCNTMGGndC3)) {
  BycntMGGndr <- cbind(CNT = CNT3[j], Cycle = "2019",eval(parse(text=paste0("ByCNTMGGndC3$data.Mplus.models.ByCountry.MGGndr_",CNT3[j],"_C3cl2.out$class_counts$modelEstimated.patterns"))))
  SizeByCNTMGGndr_C3cl2 <- rbind(SizeByCNTMGGndr_C3cl2, BycntMGGndr)
}

sizeMGGndr3 <- full_join(
  full_join(SizeByCNTMGGndr_C1cl2 %>% 
              rename_with(~ c("1999", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>% 
              mutate(Class = paste(Group,C,sep=".")) %>% 
              mutate(ClassN = factor(
                case_when(
                  #-----
                  CNT == "BGR" & Class == "1.1" ~ 1,
                  CNT == "BGR" & Class == "1.2" ~ 2,
                  CNT == "BGR" & Class == "2.1" ~ 1,
                  CNT == "BGR" & Class == "2.2" ~ 2, 
                  CNT == "CHL" & Class == "1.1" ~ 1,
                  CNT == "CHL" & Class == "1.2" ~ 2,
                  CNT == "CHL" & Class == "2.1" ~ 2,
                  CNT == "CHL" & Class == "2.2" ~ 1,
                  CNT == "COL" & Class == "1.1" ~ 1,
                  CNT == "COL" & Class == "1.2" ~ 2,
                  CNT == "COL" & Class == "2.1" ~ 2,
                  CNT == "COL" & Class == "2.2" ~ 1, 
                  CNT == "DNK" & Class == "1.1" ~ 2,
                  CNT == "DNK" & Class == "1.2" ~ 1,
                  CNT == "DNK" & Class == "2.1" ~ 1,
                  CNT == "DNK" & Class == "2.2" ~ 2,
                  CNT == "EST" & Class == "1.1" ~ 2,
                  CNT == "EST" & Class == "1.2" ~ 1,
                  CNT == "EST" & Class == "2.1" ~ 2,
                  CNT == "EST" & Class == "2.2" ~ 1, 
                  CNT == "FIN" & Class == "1.1" ~ 1,
                  CNT == "FIN" & Class == "1.2" ~ 2,
                  CNT == "FIN" & Class == "2.1" ~ 1,
                  CNT == "FIN" & Class == "2.2" ~ 2,
                  CNT == "HKG" & Class == "1.1" ~ 2,
                  CNT == "HKG" & Class == "1.2" ~ 1,
                  CNT == "HKG" & Class == "2.1" ~ 1,
                  CNT == "HKG" & Class == "2.2" ~ 2, 
                  CNT == "ITA" & Class == "1.1" ~ 1,
                  CNT == "ITA" & Class == "1.2" ~ 2,
                  CNT == "ITA" & Class == "2.1" ~ 2,
                  CNT == "ITA" & Class == "2.2" ~ 1,
                  CNT == "LTU" & Class == "1.1" ~ 1,
                  CNT == "LTU" & Class == "1.2" ~ 2,
                  CNT == "LTU" & Class == "2.1" ~ 2,
                  CNT == "LTU" & Class == "2.2" ~ 1, 
                  CNT == "LVA" & Class == "1.1" ~ 1,
                  CNT == "LVA" & Class == "1.2" ~ 2,
                  CNT == "LVA" & Class == "2.1" ~ 2,
                  CNT == "LVA" & Class == "2.2" ~ 1,
                  CNT == "NOR" & Class == "1.1" ~ 2,
                  CNT == "NOR" & Class == "1.2" ~ 1,
                  CNT == "NOR" & Class == "2.1" ~ 2,
                  CNT == "NOR" & Class == "2.2" ~ 1, 
                  CNT == "RUS" & Class == "1.1" ~ 2,
                  CNT == "RUS" & Class == "1.2" ~ 1,
                  CNT == "RUS" & Class == "2.1" ~ 2,
                  CNT == "RUS" & Class == "2.2" ~ 1,
                  CNT == "SVN" & Class == "1.1" ~ 1,
                  CNT == "SVN" & Class == "1.2" ~ 2,
                  CNT == "SVN" & Class == "2.1" ~ 2,
                  CNT == "SVN" & Class == "2.2" ~ 1, 
                  CNT == "SWE" & Class == "1.1" ~ 1,
                  CNT == "SWE" & Class == "1.2" ~ 2,
                  CNT == "SWE" & Class == "2.1" ~ 1,
                  CNT == "SWE" & Class == "2.2" ~ 2
                  #----
                ), levels = 1:2, labels = classes2M2)) %>% dplyr::select(-proportion, -C, -Class, -Cycle) ,
            SizeByCNTMGGndr_C2cl2 %>% 
              rename_with(~ c("2009", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>% 
              mutate(Class = paste(Group,C,sep=".")) %>%
              mutate(ClassN = factor(case_when(
                #-----
                CNT == "BFL" & Class == "1.1" ~ 1,
                CNT == "BFL" & Class == "1.2" ~ 2,
                CNT == "BFL" & Class == "2.1" ~ 2,
                CNT == "BFL" & Class == "2.2" ~ 1, 
                CNT == "BGR" & Class == "1.1" ~ 1,
                CNT == "BGR" & Class == "1.2" ~ 2,
                CNT == "BGR" & Class == "2.1" ~ 2,
                CNT == "BGR" & Class == "2.2" ~ 1, 
                CNT == "CHL" & Class == "1.1" ~ 2,
                CNT == "CHL" & Class == "1.2" ~ 1,
                CNT == "CHL" & Class == "2.1" ~ 2,
                CNT == "CHL" & Class == "2.2" ~ 1,
                CNT == "COL" & Class == "1.1" ~ 1,
                CNT == "COL" & Class == "1.2" ~ 2,
                CNT == "COL" & Class == "2.1" ~ 1,
                CNT == "COL" & Class == "2.2" ~ 2, 
                CNT == "DNK" & Class == "1.1" ~ 2,
                CNT == "DNK" & Class == "1.2" ~ 1,
                CNT == "DNK" & Class == "2.1" ~ 1,
                CNT == "DNK" & Class == "2.2" ~ 2, 
                CNT == "DOM" & Class == "1.1" ~ 2,
                CNT == "DOM" & Class == "1.2" ~ 1,
                CNT == "DOM" & Class == "2.1" ~ 2,
                CNT == "DOM" & Class == "2.2" ~ 1,
                CNT == "EST" & Class == "1.1" ~ 1,
                CNT == "EST" & Class == "1.2" ~ 2,
                CNT == "EST" & Class == "2.1" ~ 1,
                CNT == "EST" & Class == "2.2" ~ 2, 
                CNT == "FIN" & Class == "1.1" ~ 2,
                CNT == "FIN" & Class == "1.2" ~ 1,
                CNT == "FIN" & Class == "2.1" ~ 1,
                CNT == "FIN" & Class == "2.2" ~ 2,
                CNT == "HKG" & Class == "1.1" ~ 1,
                CNT == "HKG" & Class == "1.2" ~ 2,
                CNT == "HKG" & Class == "2.1" ~ 1,
                CNT == "HKG" & Class == "2.2" ~ 2, 
                CNT == "ITA" & Class == "1.1" ~ 1,
                CNT == "ITA" & Class == "1.2" ~ 2,
                CNT == "ITA" & Class == "2.1" ~ 2,
                CNT == "ITA" & Class == "2.2" ~ 1, 
                CNT == "KOR" & Class == "1.1" ~ 1,
                CNT == "KOR" & Class == "1.2" ~ 2,
                CNT == "KOR" & Class == "2.1" ~ 2,
                CNT == "KOR" & Class == "2.2" ~ 1,
                CNT == "LTU" & Class == "1.1" ~ 2,
                CNT == "LTU" & Class == "1.2" ~ 1,
                CNT == "LTU" & Class == "2.1" ~ 2,
                CNT == "LTU" & Class == "2.2" ~ 1, 
                CNT == "LVA" & Class == "1.1" ~ 1,
                CNT == "LVA" & Class == "1.2" ~ 2,
                CNT == "LVA" & Class == "2.1" ~ 2,
                CNT == "LVA" & Class == "2.2" ~ 1,
                CNT == "MEX" & Class == "1.1" ~ 1,
                CNT == "MEX" & Class == "1.2" ~ 2,
                CNT == "MEX" & Class == "2.1" ~ 2,
                CNT == "MEX" & Class == "2.2" ~ 1,
                CNT == "MLT" & Class == "1.1" ~ 1,
                CNT == "MLT" & Class == "1.2" ~ 2,
                CNT == "MLT" & Class == "2.1" ~ 2,
                CNT == "MLT" & Class == "2.2" ~ 1, 
                CNT == "NLD" & Class == "1.1" ~ 2,
                CNT == "NLD" & Class == "1.2" ~ 1,
                CNT == "NLD" & Class == "2.1" ~ 1,
                CNT == "NLD" & Class == "2.2" ~ 2, 
                CNT == "NOR" & Class == "1.1" ~ 1,
                CNT == "NOR" & Class == "1.2" ~ 2,
                CNT == "NOR" & Class == "2.1" ~ 1,
                CNT == "NOR" & Class == "2.2" ~ 2, 
                CNT == "RUS" & Class == "1.1" ~ 2,
                CNT == "RUS" & Class == "1.2" ~ 1,
                CNT == "RUS" & Class == "2.1" ~ 1,
                CNT == "RUS" & Class == "2.2" ~ 2,
                CNT == "SVN" & Class == "1.1" ~ 1,
                CNT == "SVN" & Class == "1.2" ~ 2,
                CNT == "SVN" & Class == "2.1" ~ 1,
                CNT == "SVN" & Class == "2.2" ~ 2, 
                CNT == "SWE" & Class == "1.1" ~ 1,
                CNT == "SWE" & Class == "1.2" ~ 2,
                CNT == "SWE" & Class == "2.1" ~ 2,
                CNT == "SWE" & Class == "2.2" ~ 1, 
                CNT == "TWN" & Class == "1.1" ~ 1,
                CNT == "TWN" & Class == "1.2" ~ 2,
                CNT == "TWN" & Class == "2.1" ~ 1,
                CNT == "TWN" & Class == "2.2" ~ 2
                #----
              ), levels = 1:2, labels = classes2M2)) %>% dplyr::select(-proportion, -C, -Class, -Cycle), by = c("CNT","Group","ClassN")),
  SizeByCNTMGGndr_C3cl2 %>%
    rename_with(~ c("2016", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>% 
    mutate(Class = paste(Group,C,sep=".")) %>%
    mutate(ClassN = factor(case_when(
      #-----
      CNT == "BFL" & Class == "1.1" ~ 1,
      CNT == "BFL" & Class == "1.2" ~ 2,
      CNT == "BFL" & Class == "2.1" ~ 2,
      CNT == "BFL" & Class == "2.2" ~ 1, 
      CNT == "BGR" & Class == "1.1" ~ 1,
      CNT == "BGR" & Class == "1.2" ~ 2,
      CNT == "BGR" & Class == "2.1" ~ 2,
      CNT == "BGR" & Class == "2.2" ~ 1, 
      CNT == "CHL" & Class == "1.1" ~ 2,
      CNT == "CHL" & Class == "1.2" ~ 1,
      CNT == "CHL" & Class == "2.1" ~ 1,
      CNT == "CHL" & Class == "2.2" ~ 2,
      CNT == "COL" & Class == "1.1" ~ 2,
      CNT == "COL" & Class == "1.2" ~ 1,
      CNT == "COL" & Class == "2.1" ~ 2,
      CNT == "COL" & Class == "2.2" ~ 1, 
      CNT == "DNK" & Class == "1.1" ~ 1,
      CNT == "DNK" & Class == "1.2" ~ 2,
      CNT == "DNK" & Class == "2.1" ~ 2,
      CNT == "DNK" & Class == "2.2" ~ 1, 
      CNT == "DOM" & Class == "1.1" ~ 1,
      CNT == "DOM" & Class == "1.2" ~ 2,
      CNT == "DOM" & Class == "2.1" ~ 1,
      CNT == "DOM" & Class == "2.2" ~ 2,
      CNT == "EST" & Class == "1.1" ~ 1,
      CNT == "EST" & Class == "1.2" ~ 2,
      CNT == "EST" & Class == "2.1" ~ 2,
      CNT == "EST" & Class == "2.2" ~ 1, 
      CNT == "FIN" & Class == "1.1" ~ 2,
      CNT == "FIN" & Class == "1.2" ~ 1,
      CNT == "FIN" & Class == "2.1" ~ 2,
      CNT == "FIN" & Class == "2.2" ~ 1,
      CNT == "HKG" & Class == "1.1" ~ 1,
      CNT == "HKG" & Class == "1.2" ~ 2,
      CNT == "HKG" & Class == "2.1" ~ 1,
      CNT == "HKG" & Class == "2.2" ~ 2, 
      CNT == "ITA" & Class == "1.1" ~ 2,
      CNT == "ITA" & Class == "1.2" ~ 1,
      CNT == "ITA" & Class == "2.1" ~ 2,
      CNT == "ITA" & Class == "2.2" ~ 1, 
      CNT == "KOR" & Class == "1.1" ~ 1,
      CNT == "KOR" & Class == "1.2" ~ 2,
      CNT == "KOR" & Class == "2.1" ~ 2,
      CNT == "KOR" & Class == "2.2" ~ 1,
      CNT == "LTU" & Class == "1.1" ~ 1,
      CNT == "LTU" & Class == "1.2" ~ 2,
      CNT == "LTU" & Class == "2.1" ~ 2,
      CNT == "LTU" & Class == "2.2" ~ 1, 
      CNT == "LVA" & Class == "1.1" ~ 1,
      CNT == "LVA" & Class == "1.2" ~ 2,
      CNT == "LVA" & Class == "2.1" ~ 2,
      CNT == "LVA" & Class == "2.2" ~ 1,
      CNT == "MEX" & Class == "1.1" ~ 2,
      CNT == "MEX" & Class == "1.2" ~ 1,
      CNT == "MEX" & Class == "2.1" ~ 2,
      CNT == "MEX" & Class == "2.2" ~ 1,
      CNT == "MLT" & Class == "1.1" ~ 2,
      CNT == "MLT" & Class == "1.2" ~ 1,
      CNT == "MLT" & Class == "2.1" ~ 2,
      CNT == "MLT" & Class == "2.2" ~ 1, 
      CNT == "NLD" & Class == "1.1" ~ 2,
      CNT == "NLD" & Class == "1.2" ~ 1,
      CNT == "NLD" & Class == "2.1" ~ 2,
      CNT == "NLD" & Class == "2.2" ~ 1, 
      CNT == "NOR" & Class == "1.1" ~ 1,
      CNT == "NOR" & Class == "1.2" ~ 2,
      CNT == "NOR" & Class == "2.1" ~ 1,
      CNT == "NOR" & Class == "2.2" ~ 2, 
      CNT == "RUS" & Class == "1.1" ~ 1,
      CNT == "RUS" & Class == "1.2" ~ 2,
      CNT == "RUS" & Class == "2.1" ~ 1,
      CNT == "RUS" & Class == "2.2" ~ 2,
      CNT == "SVN" & Class == "1.1" ~ 2,
      CNT == "SVN" & Class == "1.2" ~ 1,
      CNT == "SVN" & Class == "2.1" ~ 2,
      CNT == "SVN" & Class == "2.2" ~ 1, 
      CNT == "SWE" & Class == "1.1" ~ 1,
      CNT == "SWE" & Class == "1.2" ~ 2,
      CNT == "SWE" & Class == "2.1" ~ 1,
      CNT == "SWE" & Class == "2.2" ~ 2, 
      CNT == "TWN" & Class == "1.1" ~ 1,
      CNT == "TWN" & Class == "1.2" ~ 2,
      CNT == "TWN" & Class == "2.1" ~ 2,
      CNT == "TWN" & Class == "2.2" ~ 1
      #----
    ), levels = 1:2, labels = classes2M2)) %>% dplyr::select(-proportion, -C, -Class, -Cycle) , by = c("CNT","Group","ClassN")) %>%
  mutate(Group = factor(Group, levels = c("1", "2"), labels = c("Boy", "Girl"))) %>% select(CNT, Group, ClassN, `1999`, `2009`, `2016`)

sizeMGGndr3 <- sizeMGGndr3 %>% reshape2::melt(id.vars = c("CNT","Group","ClassN"), variable.name = "Cycle") %>% 
  arrange(CNT, Group, Cycle) %>% 
  group_by(CNT, Group, Cycle) %>%
  mutate(countT= sum(value, na.rm = TRUE)) %>%
  group_by(ClassN) %>%
  mutate(per=value/countT) %>% dplyr::select(CNT, Group, Cycle, ClassN, per) 

ByGroupHighProbMG(ByCNTMGGndr3, sizeMGGndr3, orden = c(5,2,1,3,4,6), n = 1:2, title = "Probabilities of Agree and size for each class in\n Gender Multigroup analysis by Cycle")


sizeMGGndr3 %>% 
  reshape2::dcast(CNT + Cycle ~ Group + ClassN) %>% 
  kbl(caption = "Size three classes MG Gender", booktabs = TRUE, longtable = TRUE, row.names = FALSE, 
      digits = 3, col.names = c("Country","Year", classes2M2, classes2M2)) %>%
  kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"),
                position = "left", font_size = 11, full_width = FALSE) %>%     
  column_spec(1:2, width = "2em") %>% 
  column_spec(3:6, width = "4em") %>% 
  add_header_above(c(" " = 1, " " = 1, "Boy" = 2, "Girl" = 2)) %>%  
  collapse_rows(1, valign = "top") %>% 
  print()
cat('\n')
cat('\n')

# ----------Results with 3C MG Gender by Cntry and Cycle ----

cat('\n')
cat('\n')
cat('#### Results with 3-classes  \n')
cat('\n')
cat('\n')
#----C1
ByCNTMGGndC1 <- ByCountry_MGGndr3[[1]] 
CNT <- sort(as.character(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C1","COUNTRY"])$COUNTRY))
ByCNTMGGndr_C1cl3 <- NULL
warnByCNTMGGndr_C1cl3 <- NULL

for (j in 1:length(ByCNTMGGndC1)) {
  warnByCNTMGGndr_C1cl3[CNT[j]] <- eval(parse(text=paste0("list(",CNT[j], " = ByCNTMGGndC1$data.Mplus.models.ByCountry.MGGndr_",CNT[j],"_C1cl3.out$warnings)")))
  
  BycntMGGndr <- cbind(CNT = CNT[j], Cycle = "1999",eval(parse(text=paste0("ByCNTMGGndC1$data.Mplus.models.ByCountry.MGGndr_",CNT[j],"_C1cl3.out$parameters$probability.scale"))))
  ByCNTMGGndr_C1cl3 <- rbind(ByCNTMGGndr_C1cl3, BycntMGGndr)
}

ByCNTMGGndr_C1cl3 <- ByCNTMGGndr_C1cl3 %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>% 
  mutate(Group = factor(sub('\\..*', '', Class), 
                        levels = c("1", "2"), labels = c("Boy", "Girl")), 
         ClassN = factor(case_when(
           #-----
           CNT == "BGR" & Class == "1.1" ~ 1,
           CNT == "BGR" & Class == "1.2" ~ 2,
           CNT == "BGR" & Class == "1.3" ~ 3,
           CNT == "BGR" & Class == "2.1" ~ 1,
           CNT == "BGR" & Class == "2.2" ~ 3,
           CNT == "BGR" & Class == "2.3" ~ 2, 
           CNT == "CHL" & Class == "1.1" ~ 1,
           CNT == "CHL" & Class == "1.2" ~ 3,
           CNT == "CHL" & Class == "1.3" ~ 2,
           CNT == "CHL" & Class == "2.1" ~ 1,
           CNT == "CHL" & Class == "2.2" ~ 2,
           CNT == "CHL" & Class == "2.3" ~ 3,
           CNT == "COL" & Class == "1.1" ~ 1,
           CNT == "COL" & Class == "1.2" ~ 3,
           CNT == "COL" & Class == "1.3" ~ 2,
           CNT == "COL" & Class == "2.1" ~ 2,
           CNT == "COL" & Class == "2.2" ~ 3,
           CNT == "COL" & Class == "2.3" ~ 1, 
           CNT == "DNK" & Class == "1.1" ~ 2,
           CNT == "DNK" & Class == "1.2" ~ 3,
           CNT == "DNK" & Class == "1.3" ~ 1,
           CNT == "DNK" & Class == "2.1" ~ 1,
           CNT == "DNK" & Class == "2.2" ~ 3,
           CNT == "DNK" & Class == "2.3" ~ 2,
           CNT == "EST" & Class == "1.1" ~ 2,
           CNT == "EST" & Class == "1.2" ~ 3,
           CNT == "EST" & Class == "1.3" ~ 1,
           CNT == "EST" & Class == "2.1" ~ 2,
           CNT == "EST" & Class == "2.2" ~ 1,
           CNT == "EST" & Class == "2.3" ~ 3, 
           CNT == "FIN" & Class == "1.1" ~ 2,
           CNT == "FIN" & Class == "1.2" ~ 3,
           CNT == "FIN" & Class == "1.3" ~ 1,
           CNT == "FIN" & Class == "2.1" ~ 1,
           CNT == "FIN" & Class == "2.2" ~ 3,
           CNT == "FIN" & Class == "2.3" ~ 2,
           CNT == "HKG" & Class == "1.1" ~ 1,
           CNT == "HKG" & Class == "1.2" ~ 2,
           CNT == "HKG" & Class == "1.3" ~ 3,
           CNT == "HKG" & Class == "2.1" ~ 1,
           CNT == "HKG" & Class == "2.2" ~ 2,
           CNT == "HKG" & Class == "2.3" ~ 3, 
           CNT == "ITA" & Class == "1.1" ~ 2,
           CNT == "ITA" & Class == "1.2" ~ 3,
           CNT == "ITA" & Class == "1.3" ~ 1,
           CNT == "ITA" & Class == "2.1" ~ 1,
           CNT == "ITA" & Class == "2.2" ~ 3,
           CNT == "ITA" & Class == "2.3" ~ 2,
           CNT == "LTU" & Class == "1.1" ~ 1,
           CNT == "LTU" & Class == "1.2" ~ 2,
           CNT == "LTU" & Class == "1.3" ~ 3,
           CNT == "LTU" & Class == "2.1" ~ 2,
           CNT == "LTU" & Class == "2.2" ~ 1,
           CNT == "LTU" & Class == "2.3" ~ 3, 
           CNT == "LVA" & Class == "1.1" ~ 2,
           CNT == "LVA" & Class == "1.2" ~ 3,
           CNT == "LVA" & Class == "1.3" ~ 1,
           CNT == "LVA" & Class == "2.1" ~ 2,
           CNT == "LVA" & Class == "2.2" ~ 1,
           CNT == "LVA" & Class == "2.3" ~ 3,
           CNT == "NOR" & Class == "1.1" ~ 1,
           CNT == "NOR" & Class == "1.2" ~ 3,
           CNT == "NOR" & Class == "1.3" ~ 2,
           CNT == "NOR" & Class == "2.1" ~ 1,
           CNT == "NOR" & Class == "2.2" ~ 3,
           CNT == "NOR" & Class == "2.3" ~ 2, 
           CNT == "RUS" & Class == "1.1" ~ 1,
           CNT == "RUS" & Class == "1.2" ~ 2,
           CNT == "RUS" & Class == "1.3" ~ 3,
           CNT == "RUS" & Class == "2.1" ~ 1,
           CNT == "RUS" & Class == "2.2" ~ 2,
           CNT == "RUS" & Class == "2.3" ~ 3,
           CNT == "SVN" & Class == "1.1" ~ 2,
           CNT == "SVN" & Class == "1.2" ~ 3,
           CNT == "SVN" & Class == "1.3" ~ 1,
           CNT == "SVN" & Class == "2.1" ~ 1,
           CNT == "SVN" & Class == "2.2" ~ 3,
           CNT == "SVN" & Class == "2.3" ~ 2, 
           CNT == "SWE" & Class == "1.1" ~ 1,
           CNT == "SWE" & Class == "1.2" ~ 3,
           CNT == "SWE" & Class == "1.3" ~ 2,
           CNT == "SWE" & Class == "2.1" ~ 1,
           CNT == "SWE" & Class == "2.2" ~ 2,
           CNT == "SWE" & Class == "2.3" ~ 3,
           #----
           ), levels = c("1", "2", "3"), labels = classes3M2))
#----C2
ByCNTMGGndC2 <- ByCountry_MGGndr3[[2]] 
CNT <- sort(as.character(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C2","COUNTRY"])$COUNTRY))
ByCNTMGGndr_C2cl3 <- NULL
warnByCNTMGGndr_C2cl3 <- NULL

for (j in 1:length(ByCNTMGGndC2)) {
  warnByCNTMGGndr_C2cl3[CNT[j]] <- eval(parse(text=paste0("list(",CNT[j], " = ByCNTMGGndC2$data.Mplus.models.ByCountry.MGGndr_",CNT[j],"_C2cl3.out$warnings)")))
  
  BycntMGGndr <- cbind(CNT = CNT[j], Cycle = "2009",eval(parse(text=paste0("ByCNTMGGndC2$data.Mplus.models.ByCountry.MGGndr_",CNT[j],"_C2cl3.out$parameters$probability.scale"))))
  ByCNTMGGndr_C2cl3 <- rbind(ByCNTMGGndr_C2cl3, BycntMGGndr)
}

ByCNTMGGndr_C2cl3 <- ByCNTMGGndr_C2cl3 %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x))%>% 
  mutate(Group = factor(sub('\\..*', '', Class), 
                        levels = c("1", "2"), labels = c("Boy", "Girl")), 
         ClassN = factor(case_when(
           #-----
           CNT == "BFL" & Class == "1.1" ~ 3,
           CNT == "BFL" & Class == "1.2" ~ 1,
           CNT == "BFL" & Class == "1.3" ~ 2,
           CNT == "BFL" & Class == "2.1" ~ 1,
           CNT == "BFL" & Class == "2.2" ~ 3,
           CNT == "BFL" & Class == "2.3" ~ 2, 
           CNT == "BGR" & Class == "1.1" ~ 1,
           CNT == "BGR" & Class == "1.2" ~ 3,
           CNT == "BGR" & Class == "1.3" ~ 2,
           CNT == "BGR" & Class == "2.1" ~ 1,
           CNT == "BGR" & Class == "2.2" ~ 2,
           CNT == "BGR" & Class == "2.3" ~ 3, 
           CNT == "CHL" & Class == "1.1" ~ 2,
           CNT == "CHL" & Class == "1.2" ~ 3,
           CNT == "CHL" & Class == "1.3" ~ 1,
           CNT == "CHL" & Class == "2.1" ~ 2,
           CNT == "CHL" & Class == "2.2" ~ 3,
           CNT == "CHL" & Class == "2.3" ~ 1,
           CNT == "COL" & Class == "1.1" ~ 3,
           CNT == "COL" & Class == "1.2" ~ 2,
           CNT == "COL" & Class == "1.3" ~ 1,
           CNT == "COL" & Class == "2.1" ~ 3,
           CNT == "COL" & Class == "2.2" ~ 1,
           CNT == "COL" & Class == "2.3" ~ 2, 
           CNT == "DNK" & Class == "1.1" ~ 2,
           CNT == "DNK" & Class == "1.2" ~ 3,
           CNT == "DNK" & Class == "1.3" ~ 1,
           CNT == "DNK" & Class == "2.1" ~ 3,
           CNT == "DNK" & Class == "2.2" ~ 2,
           CNT == "DNK" & Class == "2.3" ~ 1, 
           CNT == "DOM" & Class == "1.1" ~ 1,
           CNT == "DOM" & Class == "1.2" ~ 3,
           CNT == "DOM" & Class == "1.3" ~ 2,
           CNT == "DOM" & Class == "2.1" ~ 3,
           CNT == "DOM" & Class == "2.2" ~ 1,
           CNT == "DOM" & Class == "2.3" ~ 2,
           CNT == "EST" & Class == "1.1" ~ 2,
           CNT == "EST" & Class == "1.2" ~ 3,
           CNT == "EST" & Class == "1.3" ~ 1,
           CNT == "EST" & Class == "2.1" ~ 1,
           CNT == "EST" & Class == "2.2" ~ 3,
           CNT == "EST" & Class == "2.3" ~ 2, 
           CNT == "FIN" & Class == "1.1" ~ 1,
           CNT == "FIN" & Class == "1.2" ~ 3,
           CNT == "FIN" & Class == "1.3" ~ 2,
           CNT == "FIN" & Class == "2.1" ~ 1,
           CNT == "FIN" & Class == "2.2" ~ 2,
           CNT == "FIN" & Class == "2.3" ~ 3,
           CNT == "HKG" & Class == "1.1" ~ 2,
           CNT == "HKG" & Class == "1.2" ~ 3,
           CNT == "HKG" & Class == "1.3" ~ 1,
           CNT == "HKG" & Class == "2.1" ~ 1,
           CNT == "HKG" & Class == "2.2" ~ 3,
           CNT == "HKG" & Class == "2.3" ~ 2, 
           CNT == "ITA" & Class == "1.1" ~ 2,
           CNT == "ITA" & Class == "1.2" ~ 3,
           CNT == "ITA" & Class == "1.3" ~ 1,
           CNT == "ITA" & Class == "2.1" ~ 2,
           CNT == "ITA" & Class == "2.2" ~ 1,
           CNT == "ITA" & Class == "2.3" ~ 3, 
           CNT == "KOR" & Class == "1.1" ~ 2,
           CNT == "KOR" & Class == "1.2" ~ 3,
           CNT == "KOR" & Class == "1.3" ~ 1,
           CNT == "KOR" & Class == "2.1" ~ 2,
           CNT == "KOR" & Class == "2.2" ~ 3,
           CNT == "KOR" & Class == "2.3" ~ 1,
           CNT == "LTU" & Class == "1.1" ~ 1,
           CNT == "LTU" & Class == "1.2" ~ 3,
           CNT == "LTU" & Class == "1.3" ~ 2,
           CNT == "LTU" & Class == "2.1" ~ 2,
           CNT == "LTU" & Class == "2.2" ~ 3,
           CNT == "LTU" & Class == "2.3" ~ 1, 
           CNT == "LVA" & Class == "1.1" ~ 3,
           CNT == "LVA" & Class == "1.2" ~ 2,
           CNT == "LVA" & Class == "1.3" ~ 1,
           CNT == "LVA" & Class == "2.1" ~ 3,
           CNT == "LVA" & Class == "2.2" ~ 1,
           CNT == "LVA" & Class == "2.3" ~ 2,
           CNT == "MEX" & Class == "1.1" ~ 3,
           CNT == "MEX" & Class == "1.2" ~ 2,
           CNT == "MEX" & Class == "1.3" ~ 1,
           CNT == "MEX" & Class == "2.1" ~ 1,
           CNT == "MEX" & Class == "2.2" ~ 3,
           CNT == "MEX" & Class == "2.3" ~ 2,
           CNT == "MLT" & Class == "1.1" ~ 1,
           CNT == "MLT" & Class == "1.2" ~ 3,
           CNT == "MLT" & Class == "1.3" ~ 2,
           CNT == "MLT" & Class == "2.1" ~ 1,
           CNT == "MLT" & Class == "2.2" ~ 2,
           CNT == "MLT" & Class == "2.3" ~ 3, 
           CNT == "NLD" & Class == "1.1" ~ 1,
           CNT == "NLD" & Class == "1.2" ~ 3,
           CNT == "NLD" & Class == "1.3" ~ 2,
           CNT == "NLD" & Class == "2.1" ~ 1,
           CNT == "NLD" & Class == "2.2" ~ 2,
           CNT == "NLD" & Class == "2.3" ~ 3, 
           CNT == "NOR" & Class == "1.1" ~ 2,
           CNT == "NOR" & Class == "1.2" ~ 1,
           CNT == "NOR" & Class == "1.3" ~ 3,
           CNT == "NOR" & Class == "2.1" ~ 2,
           CNT == "NOR" & Class == "2.2" ~ 3,
           CNT == "NOR" & Class == "2.3" ~ 1, 
           CNT == "RUS" & Class == "1.1" ~ 1,
           CNT == "RUS" & Class == "1.2" ~ 3,
           CNT == "RUS" & Class == "1.3" ~ 2,
           CNT == "RUS" & Class == "2.1" ~ 1,
           CNT == "RUS" & Class == "2.2" ~ 2,
           CNT == "RUS" & Class == "2.3" ~ 3,
           CNT == "SVN" & Class == "1.1" ~ 1,
           CNT == "SVN" & Class == "1.2" ~ 3,
           CNT == "SVN" & Class == "1.3" ~ 2,
           CNT == "SVN" & Class == "2.1" ~ 1,
           CNT == "SVN" & Class == "2.2" ~ 2,
           CNT == "SVN" & Class == "2.3" ~ 3, 
           CNT == "SWE" & Class == "1.1" ~ 2,
           CNT == "SWE" & Class == "1.2" ~ 1,
           CNT == "SWE" & Class == "1.3" ~ 3,
           CNT == "SWE" & Class == "2.1" ~ 3,
           CNT == "SWE" & Class == "2.2" ~ 2,
           CNT == "SWE" & Class == "2.3" ~ 1, 
           CNT == "TWN" & Class == "1.1" ~ 3,
           CNT == "TWN" & Class == "1.2" ~ 2,
           CNT == "TWN" & Class == "1.3" ~ 1,
           CNT == "TWN" & Class == "2.1" ~ 3,
           CNT == "TWN" & Class == "2.2" ~ 1,
           CNT == "TWN" & Class == "2.3" ~ 2
           #----
           ), levels = c("1", "2", "3"), labels = classes3M2))
#----C3
ByCNTMGGndC3 <- ByCountry_MGGndr3[[3]] 
CNT <- sort(as.character(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C3","COUNTRY"])$COUNTRY))
ByCNTMGGndr_C3cl3 <- NULL
warnByCNTMGGndr_C3cl3 <- NULL

for (j in 1:length(ByCNTMGGndC3)) {
  warnByCNTMGGndr_C3cl3[CNT[j]] <- eval(parse(text=paste0("list(",CNT[j], " = ByCNTMGGndC3$data.Mplus.models.ByCountry.MGGndr_",CNT[j],"_C3cl3.out$warnings)")))
  
  BycntMGGndr <- cbind(CNT = CNT[j], Cycle = "2016",eval(parse(text=paste0("ByCNTMGGndC3$data.Mplus.models.ByCountry.MGGndr_",CNT[j],"_C3cl3.out$parameters$probability.scale"))))
  ByCNTMGGndr_C3cl3 <- rbind(ByCNTMGGndr_C3cl3, BycntMGGndr)
}

ByCNTMGGndr_C3cl3 <- ByCNTMGGndr_C3cl3 %>% 
  rename_with(~ c("Class", "value")[which(c("LatentClass", "est") == .x)], .cols = c("LatentClass", "est")) %>% 
  mutate_at( c("param", "category", "Class"), ~ as.factor(.x)) %>% 
  mutate(Group = factor(sub('\\..*', '', Class), 
                        levels = c("1", "2"), labels = c("Boy", "Girl")), 
         ClassN = factor(case_when(
           #-----
           CNT == "BFL" & Class == "1.1" ~ 1,
           CNT == "BFL" & Class == "1.2" ~ 3,
           CNT == "BFL" & Class == "1.3" ~ 2,
           CNT == "BFL" & Class == "2.1" ~ 1,
           CNT == "BFL" & Class == "2.2" ~ 2,
           CNT == "BFL" & Class == "2.3" ~ 3, 
           CNT == "BGR" & Class == "1.1" ~ 1,
           CNT == "BGR" & Class == "1.2" ~ 3,
           CNT == "BGR" & Class == "1.3" ~ 2,
           CNT == "BGR" & Class == "2.1" ~ 1,
           CNT == "BGR" & Class == "2.2" ~ 2,
           CNT == "BGR" & Class == "2.3" ~ 3, 
           CNT == "CHL" & Class == "1.1" ~ 2,
           CNT == "CHL" & Class == "1.2" ~ 1,
           CNT == "CHL" & Class == "1.3" ~ 3,
           CNT == "CHL" & Class == "2.1" ~ 3,
           CNT == "CHL" & Class == "2.2" ~ 2,
           CNT == "CHL" & Class == "2.3" ~ 1,
           CNT == "COL" & Class == "1.1" ~ 1,
           CNT == "COL" & Class == "1.2" ~ 3,
           CNT == "COL" & Class == "1.3" ~ 2,
           CNT == "COL" & Class == "2.1" ~ 3,
           CNT == "COL" & Class == "2.2" ~ 1,
           CNT == "COL" & Class == "2.3" ~ 2, 
           CNT == "DNK" & Class == "1.1" ~ 2,
           CNT == "DNK" & Class == "1.2" ~ 3,
           CNT == "DNK" & Class == "1.3" ~ 1,
           CNT == "DNK" & Class == "2.1" ~ 2,
           CNT == "DNK" & Class == "2.2" ~ 1,
           CNT == "DNK" & Class == "2.3" ~ 3, 
           CNT == "DOM" & Class == "1.1" ~ 1,
           CNT == "DOM" & Class == "1.2" ~ 3,
           CNT == "DOM" & Class == "1.3" ~ 2,
           CNT == "DOM" & Class == "2.1" ~ 3,
           CNT == "DOM" & Class == "2.2" ~ 2,
           CNT == "DOM" & Class == "2.3" ~ 1,
           CNT == "EST" & Class == "1.1" ~ 2,
           CNT == "EST" & Class == "1.2" ~ 3,
           CNT == "EST" & Class == "1.3" ~ 1,
           CNT == "EST" & Class == "2.1" ~ 2,
           CNT == "EST" & Class == "2.2" ~ 3,
           CNT == "EST" & Class == "2.3" ~ 1, 
           CNT == "FIN" & Class == "1.1" ~ 1,
           CNT == "FIN" & Class == "1.2" ~ 3,
           CNT == "FIN" & Class == "1.3" ~ 2,
           CNT == "FIN" & Class == "2.1" ~ 1,
           CNT == "FIN" & Class == "2.2" ~ 2,
           CNT == "FIN" & Class == "2.3" ~ 3,
           CNT == "HKG" & Class == "1.1" ~ 2,
           CNT == "HKG" & Class == "1.2" ~ 1,
           CNT == "HKG" & Class == "1.3" ~ 3,
           CNT == "HKG" & Class == "2.1" ~ 3,
           CNT == "HKG" & Class == "2.2" ~ 2,
           CNT == "HKG" & Class == "2.3" ~ 1, 
           CNT == "ITA" & Class == "1.1" ~ 2,
           CNT == "ITA" & Class == "1.2" ~ 1,
           CNT == "ITA" & Class == "1.3" ~ 3,
           CNT == "ITA" & Class == "2.1" ~ 3,
           CNT == "ITA" & Class == "2.2" ~ 2,
           CNT == "ITA" & Class == "2.3" ~ 1, 
           CNT == "KOR" & Class == "1.1" ~ 1,
           CNT == "KOR" & Class == "1.2" ~ 2,
           CNT == "KOR" & Class == "1.3" ~ 3,
           CNT == "KOR" & Class == "2.1" ~ 1,
           CNT == "KOR" & Class == "2.2" ~ 3,
           CNT == "KOR" & Class == "2.3" ~ 2,
           CNT == "LTU" & Class == "1.1" ~ 2,
           CNT == "LTU" & Class == "1.2" ~ 3,
           CNT == "LTU" & Class == "1.3" ~ 1,
           CNT == "LTU" & Class == "2.1" ~ 2,
           CNT == "LTU" & Class == "2.2" ~ 3,
           CNT == "LTU" & Class == "2.3" ~ 1, 
           CNT == "LVA" & Class == "1.1" ~ 1,
           CNT == "LVA" & Class == "1.2" ~ 3,
           CNT == "LVA" & Class == "1.3" ~ 2,
           CNT == "LVA" & Class == "2.1" ~ 2,
           CNT == "LVA" & Class == "2.2" ~ 3,
           CNT == "LVA" & Class == "2.3" ~ 1,
           CNT == "MEX" & Class == "1.1" ~ 1,
           CNT == "MEX" & Class == "1.2" ~ 2,
           CNT == "MEX" & Class == "1.3" ~ 3,
           CNT == "MEX" & Class == "2.1" ~ 3,
           CNT == "MEX" & Class == "2.2" ~ 1,
           CNT == "MEX" & Class == "2.3" ~ 2,
           CNT == "MLT" & Class == "1.1" ~ 2,
           CNT == "MLT" & Class == "1.2" ~ 3,
           CNT == "MLT" & Class == "1.3" ~ 1,
           CNT == "MLT" & Class == "2.1" ~ 3,
           CNT == "MLT" & Class == "2.2" ~ 1,
           CNT == "MLT" & Class == "2.3" ~ 2, 
           CNT == "NLD" & Class == "1.1" ~ 1,
           CNT == "NLD" & Class == "1.2" ~ 3,
           CNT == "NLD" & Class == "1.3" ~ 2,
           CNT == "NLD" & Class == "2.1" ~ 2,
           CNT == "NLD" & Class == "2.2" ~ 3,
           CNT == "NLD" & Class == "2.3" ~ 1, 
           CNT == "NOR" & Class == "1.1" ~ 2,
           CNT == "NOR" & Class == "1.2" ~ 1,
           CNT == "NOR" & Class == "1.3" ~ 3,
           CNT == "NOR" & Class == "2.1" ~ 1,
           CNT == "NOR" & Class == "2.2" ~ 2,
           CNT == "NOR" & Class == "2.3" ~ 3, 
           CNT == "RUS" & Class == "1.1" ~ 1,
           CNT == "RUS" & Class == "1.2" ~ 3,
           CNT == "RUS" & Class == "1.3" ~ 2,
           CNT == "RUS" & Class == "2.1" ~ 1,
           CNT == "RUS" & Class == "2.2" ~ 3,
           CNT == "RUS" & Class == "2.3" ~ 2,
           CNT == "SVN" & Class == "1.1" ~ 1,
           CNT == "SVN" & Class == "1.2" ~ 2,
           CNT == "SVN" & Class == "1.3" ~ 3,
           CNT == "SVN" & Class == "2.1" ~ 1,
           CNT == "SVN" & Class == "2.2" ~ 2,
           CNT == "SVN" & Class == "2.3" ~ 3, 
           CNT == "SWE" & Class == "1.1" ~ 1,
           CNT == "SWE" & Class == "1.2" ~ 3,
           CNT == "SWE" & Class == "1.3" ~ 2,
           CNT == "SWE" & Class == "2.1" ~ 2,
           CNT == "SWE" & Class == "2.2" ~ 3,
           CNT == "SWE" & Class == "2.3" ~ 1, 
           CNT == "TWN" & Class == "1.1" ~ 1,
           CNT == "TWN" & Class == "1.2" ~ 3,
           CNT == "TWN" & Class == "1.3" ~ 2,
           CNT == "TWN" & Class == "2.1" ~ 1,
           CNT == "TWN" & Class == "2.2" ~ 3,
           CNT == "TWN" & Class == "2.3" ~ 2
           #----
           ), levels = c("1", "2", "3"), labels = classes3M2))

ByCNTMGGndr3 <- rbind(ByCNTMGGndr_C1cl3,
                     ByCNTMGGndr_C2cl3,
                     ByCNTMGGndr_C3cl3)

warningsByCNTMGGndr <- list(C1 = warnByCNTMGGndr_C1cl3,
                            C2 = warnByCNTMGGndr_C2cl3,
                            C3 = warnByCNTMGGndr_C3cl3)

cat('\n')
cat('\n')
wr1 <- names(warningsByCNTMGGndr$C1[grep("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED",warningsByCNTMGGndr$C1)])
if (length(wr) == 0) print("ALL BEST LOGLIKELIHOOD VALUES WERE REPLICATED") else
  cat("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED for the following cases:  \n C1:  \n", wr1)
cat('\n')
cat('\n')
wr2 <- names(warningsByCNTMGGndr$C2[grep("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED",warningsByCNTMGGndr$C2)])
if (length(wr) == 0) print("ALL BEST LOGLIKELIHOOD VALUES WERE REPLICATED") else
  cat("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED for the following cases:  \n C2:  \n", wr2)
cat('\n')
cat('\n')

wr3 <- names(warningsByCNTMGGndr$C3[grep("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED",warningsByCNTMGGndr$C3)])
if (length(wr) == 0) print("ALL BEST LOGLIKELIHOOD VALUES WERE REPLICATED") else
  cat("THE BEST LOGLIKELIHOOD VALUE WAS NOT REPLICATED for the following cases:  \n C3:  \n", wr3)
cat('\n')
cat('\n')

#---
ByGroupgraphclass(ByCNTMGGndr3, nclass = 3, orden = c(5,2,1,3,4,6) ,title = "MG Gender with 3 classes")
cat('\n')
cat('\n')

ByCNTlcign <- VarClass(ByCNTMGGndr3)
ByCNTlcign %>% group_by(CNT, Cycle, ClassN, Group, param) %>% filter(category == 2) %>% 
  select(CNT, Cycle, param, ClassN, Group, value) %>% 
  mutate(value = cell_spec(value, color = ifelse(value > 0.8, "blue", "red"))) %>% 
  reshape2::dcast(CNT + param + Cycle ~ Group + ClassN) %>%
  kbl(caption = "Probabilities to Agree each item by Class and Gender", booktabs = TRUE, longtable = TRUE, 
      align = "lllrrrrrr", row.names = FALSE, digits = 3, col.names = c("Country","Item", "Year", classes3M2, classes3M2), escape = FALSE) %>%
  kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"), 
                position = "left", font_size = 10, full_width = FALSE) %>% 
  column_spec(2, width = "12em") %>%  
  column_spec(c(1,3), width = "2em") %>%  
  column_spec(4:9, width = "4em") %>%  
  collapse_rows(c(1,2), valign = "top") %>%
  add_header_above(c(" "=1, " "=1," "=1, "Boy" = 3, "Girl" = 3)) %>% 
  print()
cat('\n')
cat('\n')

#Extracting patterns
CNT1 <- sort(as.character(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C1","COUNTRY"])$COUNTRY))
SizeByCNTMGGndr_C1cl3 <- NULL
for (j in 1:length(ByCNTMGGndC1)) {
  BycntMGGndr <- cbind(CNT = CNT1[j], Cycle = "1999",eval(parse(text=paste0("ByCNTMGGndC1$data.Mplus.models.ByCountry.MGGndr_",CNT1[j],"_C1cl3.out$class_counts$modelEstimated.patterns"))))
  SizeByCNTMGGndr_C1cl3 <- rbind(SizeByCNTMGGndr_C1cl3, BycntMGGndr)
}
CNT2 <- sort(as.character(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C2","COUNTRY"])$COUNTRY))
SizeByCNTMGGndr_C2cl3 <- NULL
for (j in 1:length(ByCNTMGGndC2)) {
  BycntMGGndr <- cbind(CNT = CNT2[j], Cycle = "2009",eval(parse(text=paste0("ByCNTMGGndC2$data.Mplus.models.ByCountry.MGGndr_",CNT2[j],"_C2cl3.out$class_counts$modelEstimated.patterns"))))
  SizeByCNTMGGndr_C2cl3 <- rbind(SizeByCNTMGGndr_C2cl3, BycntMGGndr)
}
CNT3 <- sort(as.character(unique(ISC_lvRlca[ISC_lvRlca$cycle == "C3","COUNTRY"])$COUNTRY))
SizeByCNTMGGndr_C3cl3 <- NULL
for (j in 1:length(ByCNTMGGndC3)) {
  BycntMGGndr <- cbind(CNT = CNT3[j], Cycle = "2019",eval(parse(text=paste0("ByCNTMGGndC3$data.Mplus.models.ByCountry.MGGndr_",CNT3[j],"_C3cl3.out$class_counts$modelEstimated.patterns"))))
  SizeByCNTMGGndr_C3cl3 <- rbind(SizeByCNTMGGndr_C3cl3, BycntMGGndr)
}

sizeMGGndr3 <- full_join(
  full_join(SizeByCNTMGGndr_C1cl3 %>% 
              rename_with(~ c("1999", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>% 
              mutate(Class = paste(Group,C,sep=".")) %>% 
              mutate(ClassN = factor(
                case_when(
                  #-----
                  CNT == "BGR" & Class == "1.1" ~ 1,
                  CNT == "BGR" & Class == "1.2" ~ 2,
                  CNT == "BGR" & Class == "1.3" ~ 3,
                  CNT == "BGR" & Class == "2.1" ~ 1,
                  CNT == "BGR" & Class == "2.2" ~ 3,
                  CNT == "BGR" & Class == "2.3" ~ 2, 
                  CNT == "CHL" & Class == "1.1" ~ 1,
                  CNT == "CHL" & Class == "1.2" ~ 3,
                  CNT == "CHL" & Class == "1.3" ~ 2,
                  CNT == "CHL" & Class == "2.1" ~ 1,
                  CNT == "CHL" & Class == "2.2" ~ 2,
                  CNT == "CHL" & Class == "2.3" ~ 3,
                  CNT == "COL" & Class == "1.1" ~ 1,
                  CNT == "COL" & Class == "1.2" ~ 3,
                  CNT == "COL" & Class == "1.3" ~ 2,
                  CNT == "COL" & Class == "2.1" ~ 2,
                  CNT == "COL" & Class == "2.2" ~ 3,
                  CNT == "COL" & Class == "2.3" ~ 1, 
                  CNT == "DNK" & Class == "1.1" ~ 2,
                  CNT == "DNK" & Class == "1.2" ~ 3,
                  CNT == "DNK" & Class == "1.3" ~ 1,
                  CNT == "DNK" & Class == "2.1" ~ 1,
                  CNT == "DNK" & Class == "2.2" ~ 3,
                  CNT == "DNK" & Class == "2.3" ~ 2,
                  CNT == "EST" & Class == "1.1" ~ 2,
                  CNT == "EST" & Class == "1.2" ~ 3,
                  CNT == "EST" & Class == "1.3" ~ 1,
                  CNT == "EST" & Class == "2.1" ~ 2,
                  CNT == "EST" & Class == "2.2" ~ 1,
                  CNT == "EST" & Class == "2.3" ~ 3, 
                  CNT == "FIN" & Class == "1.1" ~ 2,
                  CNT == "FIN" & Class == "1.2" ~ 3,
                  CNT == "FIN" & Class == "1.3" ~ 1,
                  CNT == "FIN" & Class == "2.1" ~ 1,
                  CNT == "FIN" & Class == "2.2" ~ 3,
                  CNT == "FIN" & Class == "2.3" ~ 2,
                  CNT == "HKG" & Class == "1.1" ~ 1,
                  CNT == "HKG" & Class == "1.2" ~ 2,
                  CNT == "HKG" & Class == "1.3" ~ 3,
                  CNT == "HKG" & Class == "2.1" ~ 1,
                  CNT == "HKG" & Class == "2.2" ~ 2,
                  CNT == "HKG" & Class == "2.3" ~ 3, 
                  CNT == "ITA" & Class == "1.1" ~ 2,
                  CNT == "ITA" & Class == "1.2" ~ 3,
                  CNT == "ITA" & Class == "1.3" ~ 1,
                  CNT == "ITA" & Class == "2.1" ~ 1,
                  CNT == "ITA" & Class == "2.2" ~ 3,
                  CNT == "ITA" & Class == "2.3" ~ 2,
                  CNT == "LTU" & Class == "1.1" ~ 1,
                  CNT == "LTU" & Class == "1.2" ~ 2,
                  CNT == "LTU" & Class == "1.3" ~ 3,
                  CNT == "LTU" & Class == "2.1" ~ 2,
                  CNT == "LTU" & Class == "2.2" ~ 1,
                  CNT == "LTU" & Class == "2.3" ~ 3, 
                  CNT == "LVA" & Class == "1.1" ~ 2,
                  CNT == "LVA" & Class == "1.2" ~ 3,
                  CNT == "LVA" & Class == "1.3" ~ 1,
                  CNT == "LVA" & Class == "2.1" ~ 2,
                  CNT == "LVA" & Class == "2.2" ~ 1,
                  CNT == "LVA" & Class == "2.3" ~ 3,
                  CNT == "NOR" & Class == "1.1" ~ 1,
                  CNT == "NOR" & Class == "1.2" ~ 3,
                  CNT == "NOR" & Class == "1.3" ~ 2,
                  CNT == "NOR" & Class == "2.1" ~ 1,
                  CNT == "NOR" & Class == "2.2" ~ 3,
                  CNT == "NOR" & Class == "2.3" ~ 2, 
                  CNT == "RUS" & Class == "1.1" ~ 1,
                  CNT == "RUS" & Class == "1.2" ~ 2,
                  CNT == "RUS" & Class == "1.3" ~ 3,
                  CNT == "RUS" & Class == "2.1" ~ 1,
                  CNT == "RUS" & Class == "2.2" ~ 2,
                  CNT == "RUS" & Class == "2.3" ~ 3,
                  CNT == "SVN" & Class == "1.1" ~ 2,
                  CNT == "SVN" & Class == "1.2" ~ 3,
                  CNT == "SVN" & Class == "1.3" ~ 1,
                  CNT == "SVN" & Class == "2.1" ~ 1,
                  CNT == "SVN" & Class == "2.2" ~ 3,
                  CNT == "SVN" & Class == "2.3" ~ 2, 
                  CNT == "SWE" & Class == "1.1" ~ 1,
                  CNT == "SWE" & Class == "1.2" ~ 3,
                  CNT == "SWE" & Class == "1.3" ~ 2,
                  CNT == "SWE" & Class == "2.1" ~ 1,
                  CNT == "SWE" & Class == "2.2" ~ 2,
                  CNT == "SWE" & Class == "2.3" ~ 3,
                  #----
                  ), levels = 1:3, labels = classes3M2)) %>% dplyr::select(-proportion, -C, -Class, -Cycle) ,
            SizeByCNTMGGndr_C2cl3 %>% 
              rename_with(~ c("2009", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>% 
              mutate(Class = paste(Group,C,sep=".")) %>%
              mutate(ClassN = factor(case_when(
                #-----
                CNT == "BFL" & Class == "1.1" ~ 3,
                CNT == "BFL" & Class == "1.2" ~ 1,
                CNT == "BFL" & Class == "1.3" ~ 2,
                CNT == "BFL" & Class == "2.1" ~ 1,
                CNT == "BFL" & Class == "2.2" ~ 3,
                CNT == "BFL" & Class == "2.3" ~ 2, 
                CNT == "BGR" & Class == "1.1" ~ 1,
                CNT == "BGR" & Class == "1.2" ~ 3,
                CNT == "BGR" & Class == "1.3" ~ 2,
                CNT == "BGR" & Class == "2.1" ~ 1,
                CNT == "BGR" & Class == "2.2" ~ 2,
                CNT == "BGR" & Class == "2.3" ~ 3, 
                CNT == "CHL" & Class == "1.1" ~ 2,
                CNT == "CHL" & Class == "1.2" ~ 3,
                CNT == "CHL" & Class == "1.3" ~ 1,
                CNT == "CHL" & Class == "2.1" ~ 2,
                CNT == "CHL" & Class == "2.2" ~ 3,
                CNT == "CHL" & Class == "2.3" ~ 1,
                CNT == "COL" & Class == "1.1" ~ 3,
                CNT == "COL" & Class == "1.2" ~ 2,
                CNT == "COL" & Class == "1.3" ~ 1,
                CNT == "COL" & Class == "2.1" ~ 3,
                CNT == "COL" & Class == "2.2" ~ 1,
                CNT == "COL" & Class == "2.3" ~ 2, 
                CNT == "DNK" & Class == "1.1" ~ 2,
                CNT == "DNK" & Class == "1.2" ~ 3,
                CNT == "DNK" & Class == "1.3" ~ 1,
                CNT == "DNK" & Class == "2.1" ~ 3,
                CNT == "DNK" & Class == "2.2" ~ 2,
                CNT == "DNK" & Class == "2.3" ~ 1, 
                CNT == "DOM" & Class == "1.1" ~ 1,
                CNT == "DOM" & Class == "1.2" ~ 3,
                CNT == "DOM" & Class == "1.3" ~ 2,
                CNT == "DOM" & Class == "2.1" ~ 3,
                CNT == "DOM" & Class == "2.2" ~ 1,
                CNT == "DOM" & Class == "2.3" ~ 2,
                CNT == "EST" & Class == "1.1" ~ 2,
                CNT == "EST" & Class == "1.2" ~ 3,
                CNT == "EST" & Class == "1.3" ~ 1,
                CNT == "EST" & Class == "2.1" ~ 1,
                CNT == "EST" & Class == "2.2" ~ 3,
                CNT == "EST" & Class == "2.3" ~ 2, 
                CNT == "FIN" & Class == "1.1" ~ 1,
                CNT == "FIN" & Class == "1.2" ~ 3,
                CNT == "FIN" & Class == "1.3" ~ 2,
                CNT == "FIN" & Class == "2.1" ~ 1,
                CNT == "FIN" & Class == "2.2" ~ 2,
                CNT == "FIN" & Class == "2.3" ~ 3,
                CNT == "HKG" & Class == "1.1" ~ 2,
                CNT == "HKG" & Class == "1.2" ~ 3,
                CNT == "HKG" & Class == "1.3" ~ 1,
                CNT == "HKG" & Class == "2.1" ~ 1,
                CNT == "HKG" & Class == "2.2" ~ 3,
                CNT == "HKG" & Class == "2.3" ~ 2, 
                CNT == "ITA" & Class == "1.1" ~ 2,
                CNT == "ITA" & Class == "1.2" ~ 3,
                CNT == "ITA" & Class == "1.3" ~ 1,
                CNT == "ITA" & Class == "2.1" ~ 2,
                CNT == "ITA" & Class == "2.2" ~ 1,
                CNT == "ITA" & Class == "2.3" ~ 3, 
                CNT == "KOR" & Class == "1.1" ~ 2,
                CNT == "KOR" & Class == "1.2" ~ 3,
                CNT == "KOR" & Class == "1.3" ~ 1,
                CNT == "KOR" & Class == "2.1" ~ 2,
                CNT == "KOR" & Class == "2.2" ~ 3,
                CNT == "KOR" & Class == "2.3" ~ 1,
                CNT == "LTU" & Class == "1.1" ~ 1,
                CNT == "LTU" & Class == "1.2" ~ 3,
                CNT == "LTU" & Class == "1.3" ~ 2,
                CNT == "LTU" & Class == "2.1" ~ 2,
                CNT == "LTU" & Class == "2.2" ~ 3,
                CNT == "LTU" & Class == "2.3" ~ 1, 
                CNT == "LVA" & Class == "1.1" ~ 3,
                CNT == "LVA" & Class == "1.2" ~ 2,
                CNT == "LVA" & Class == "1.3" ~ 1,
                CNT == "LVA" & Class == "2.1" ~ 3,
                CNT == "LVA" & Class == "2.2" ~ 1,
                CNT == "LVA" & Class == "2.3" ~ 2,
                CNT == "MEX" & Class == "1.1" ~ 3,
                CNT == "MEX" & Class == "1.2" ~ 2,
                CNT == "MEX" & Class == "1.3" ~ 1,
                CNT == "MEX" & Class == "2.1" ~ 1,
                CNT == "MEX" & Class == "2.2" ~ 3,
                CNT == "MEX" & Class == "2.3" ~ 2,
                CNT == "MLT" & Class == "1.1" ~ 1,
                CNT == "MLT" & Class == "1.2" ~ 3,
                CNT == "MLT" & Class == "1.3" ~ 2,
                CNT == "MLT" & Class == "2.1" ~ 1,
                CNT == "MLT" & Class == "2.2" ~ 2,
                CNT == "MLT" & Class == "2.3" ~ 3, 
                CNT == "NLD" & Class == "1.1" ~ 1,
                CNT == "NLD" & Class == "1.2" ~ 3,
                CNT == "NLD" & Class == "1.3" ~ 2,
                CNT == "NLD" & Class == "2.1" ~ 1,
                CNT == "NLD" & Class == "2.2" ~ 2,
                CNT == "NLD" & Class == "2.3" ~ 3, 
                CNT == "NOR" & Class == "1.1" ~ 2,
                CNT == "NOR" & Class == "1.2" ~ 1,
                CNT == "NOR" & Class == "1.3" ~ 3,
                CNT == "NOR" & Class == "2.1" ~ 2,
                CNT == "NOR" & Class == "2.2" ~ 3,
                CNT == "NOR" & Class == "2.3" ~ 1, 
                CNT == "RUS" & Class == "1.1" ~ 1,
                CNT == "RUS" & Class == "1.2" ~ 3,
                CNT == "RUS" & Class == "1.3" ~ 2,
                CNT == "RUS" & Class == "2.1" ~ 1,
                CNT == "RUS" & Class == "2.2" ~ 2,
                CNT == "RUS" & Class == "2.3" ~ 3,
                CNT == "SVN" & Class == "1.1" ~ 1,
                CNT == "SVN" & Class == "1.2" ~ 3,
                CNT == "SVN" & Class == "1.3" ~ 2,
                CNT == "SVN" & Class == "2.1" ~ 1,
                CNT == "SVN" & Class == "2.2" ~ 2,
                CNT == "SVN" & Class == "2.3" ~ 3, 
                CNT == "SWE" & Class == "1.1" ~ 2,
                CNT == "SWE" & Class == "1.2" ~ 1,
                CNT == "SWE" & Class == "1.3" ~ 3,
                CNT == "SWE" & Class == "2.1" ~ 3,
                CNT == "SWE" & Class == "2.2" ~ 2,
                CNT == "SWE" & Class == "2.3" ~ 1, 
                CNT == "TWN" & Class == "1.1" ~ 3,
                CNT == "TWN" & Class == "1.2" ~ 2,
                CNT == "TWN" & Class == "1.3" ~ 1,
                CNT == "TWN" & Class == "2.1" ~ 3,
                CNT == "TWN" & Class == "2.2" ~ 1,
                CNT == "TWN" & Class == "2.3" ~ 2
                #----
                ), levels = 1:3, labels = classes3M2)) %>% dplyr::select(-proportion, -C, -Class, -Cycle), by = c("CNT","Group","ClassN")),
  SizeByCNTMGGndr_C3cl3 %>%
    rename_with(~ c("2016", "Group")[which(c("count", "G") == .x)], .cols = c("count", "G")) %>% 
    mutate(Class = paste(Group,C,sep=".")) %>%
    mutate(ClassN = factor(case_when(
      #-----
      CNT == "BFL" & Class == "1.1" ~ 1,
      CNT == "BFL" & Class == "1.2" ~ 3,
      CNT == "BFL" & Class == "1.3" ~ 2,
      CNT == "BFL" & Class == "2.1" ~ 1,
      CNT == "BFL" & Class == "2.2" ~ 2,
      CNT == "BFL" & Class == "2.3" ~ 3, 
      CNT == "BGR" & Class == "1.1" ~ 1,
      CNT == "BGR" & Class == "1.2" ~ 3,
      CNT == "BGR" & Class == "1.3" ~ 2,
      CNT == "BGR" & Class == "2.1" ~ 1,
      CNT == "BGR" & Class == "2.2" ~ 2,
      CNT == "BGR" & Class == "2.3" ~ 3, 
      CNT == "CHL" & Class == "1.1" ~ 2,
      CNT == "CHL" & Class == "1.2" ~ 1,
      CNT == "CHL" & Class == "1.3" ~ 3,
      CNT == "CHL" & Class == "2.1" ~ 3,
      CNT == "CHL" & Class == "2.2" ~ 2,
      CNT == "CHL" & Class == "2.3" ~ 1,
      CNT == "COL" & Class == "1.1" ~ 1,
      CNT == "COL" & Class == "1.2" ~ 3,
      CNT == "COL" & Class == "1.3" ~ 2,
      CNT == "COL" & Class == "2.1" ~ 3,
      CNT == "COL" & Class == "2.2" ~ 1,
      CNT == "COL" & Class == "2.3" ~ 2, 
      CNT == "DNK" & Class == "1.1" ~ 2,
      CNT == "DNK" & Class == "1.2" ~ 3,
      CNT == "DNK" & Class == "1.3" ~ 1,
      CNT == "DNK" & Class == "2.1" ~ 2,
      CNT == "DNK" & Class == "2.2" ~ 1,
      CNT == "DNK" & Class == "2.3" ~ 3, 
      CNT == "DOM" & Class == "1.1" ~ 1,
      CNT == "DOM" & Class == "1.2" ~ 3,
      CNT == "DOM" & Class == "1.3" ~ 2,
      CNT == "DOM" & Class == "2.1" ~ 3,
      CNT == "DOM" & Class == "2.2" ~ 2,
      CNT == "DOM" & Class == "2.3" ~ 1,
      CNT == "EST" & Class == "1.1" ~ 2,
      CNT == "EST" & Class == "1.2" ~ 3,
      CNT == "EST" & Class == "1.3" ~ 1,
      CNT == "EST" & Class == "2.1" ~ 2,
      CNT == "EST" & Class == "2.2" ~ 3,
      CNT == "EST" & Class == "2.3" ~ 1, 
      CNT == "FIN" & Class == "1.1" ~ 1,
      CNT == "FIN" & Class == "1.2" ~ 3,
      CNT == "FIN" & Class == "1.3" ~ 2,
      CNT == "FIN" & Class == "2.1" ~ 1,
      CNT == "FIN" & Class == "2.2" ~ 2,
      CNT == "FIN" & Class == "2.3" ~ 3,
      CNT == "HKG" & Class == "1.1" ~ 2,
      CNT == "HKG" & Class == "1.2" ~ 1,
      CNT == "HKG" & Class == "1.3" ~ 3,
      CNT == "HKG" & Class == "2.1" ~ 3,
      CNT == "HKG" & Class == "2.2" ~ 2,
      CNT == "HKG" & Class == "2.3" ~ 1, 
      CNT == "ITA" & Class == "1.1" ~ 2,
      CNT == "ITA" & Class == "1.2" ~ 1,
      CNT == "ITA" & Class == "1.3" ~ 3,
      CNT == "ITA" & Class == "2.1" ~ 3,
      CNT == "ITA" & Class == "2.2" ~ 2,
      CNT == "ITA" & Class == "2.3" ~ 1, 
      CNT == "KOR" & Class == "1.1" ~ 1,
      CNT == "KOR" & Class == "1.2" ~ 2,
      CNT == "KOR" & Class == "1.3" ~ 3,
      CNT == "KOR" & Class == "2.1" ~ 1,
      CNT == "KOR" & Class == "2.2" ~ 3,
      CNT == "KOR" & Class == "2.3" ~ 2,
      CNT == "LTU" & Class == "1.1" ~ 2,
      CNT == "LTU" & Class == "1.2" ~ 3,
      CNT == "LTU" & Class == "1.3" ~ 1,
      CNT == "LTU" & Class == "2.1" ~ 2,
      CNT == "LTU" & Class == "2.2" ~ 3,
      CNT == "LTU" & Class == "2.3" ~ 1, 
      CNT == "LVA" & Class == "1.1" ~ 1,
      CNT == "LVA" & Class == "1.2" ~ 3,
      CNT == "LVA" & Class == "1.3" ~ 2,
      CNT == "LVA" & Class == "2.1" ~ 2,
      CNT == "LVA" & Class == "2.2" ~ 3,
      CNT == "LVA" & Class == "2.3" ~ 1,
      CNT == "MEX" & Class == "1.1" ~ 1,
      CNT == "MEX" & Class == "1.2" ~ 2,
      CNT == "MEX" & Class == "1.3" ~ 3,
      CNT == "MEX" & Class == "2.1" ~ 3,
      CNT == "MEX" & Class == "2.2" ~ 1,
      CNT == "MEX" & Class == "2.3" ~ 2,
      CNT == "MLT" & Class == "1.1" ~ 2,
      CNT == "MLT" & Class == "1.2" ~ 3,
      CNT == "MLT" & Class == "1.3" ~ 1,
      CNT == "MLT" & Class == "2.1" ~ 3,
      CNT == "MLT" & Class == "2.2" ~ 1,
      CNT == "MLT" & Class == "2.3" ~ 2, 
      CNT == "NLD" & Class == "1.1" ~ 1,
      CNT == "NLD" & Class == "1.2" ~ 3,
      CNT == "NLD" & Class == "1.3" ~ 2,
      CNT == "NLD" & Class == "2.1" ~ 2,
      CNT == "NLD" & Class == "2.2" ~ 3,
      CNT == "NLD" & Class == "2.3" ~ 1, 
      CNT == "NOR" & Class == "1.1" ~ 2,
      CNT == "NOR" & Class == "1.2" ~ 1,
      CNT == "NOR" & Class == "1.3" ~ 3,
      CNT == "NOR" & Class == "2.1" ~ 1,
      CNT == "NOR" & Class == "2.2" ~ 2,
      CNT == "NOR" & Class == "2.3" ~ 3, 
      CNT == "RUS" & Class == "1.1" ~ 1,
      CNT == "RUS" & Class == "1.2" ~ 3,
      CNT == "RUS" & Class == "1.3" ~ 2,
      CNT == "RUS" & Class == "2.1" ~ 1,
      CNT == "RUS" & Class == "2.2" ~ 3,
      CNT == "RUS" & Class == "2.3" ~ 2,
      CNT == "SVN" & Class == "1.1" ~ 1,
      CNT == "SVN" & Class == "1.2" ~ 2,
      CNT == "SVN" & Class == "1.3" ~ 3,
      CNT == "SVN" & Class == "2.1" ~ 1,
      CNT == "SVN" & Class == "2.2" ~ 2,
      CNT == "SVN" & Class == "2.3" ~ 3, 
      CNT == "SWE" & Class == "1.1" ~ 1,
      CNT == "SWE" & Class == "1.2" ~ 3,
      CNT == "SWE" & Class == "1.3" ~ 2,
      CNT == "SWE" & Class == "2.1" ~ 2,
      CNT == "SWE" & Class == "2.2" ~ 3,
      CNT == "SWE" & Class == "2.3" ~ 1, 
      CNT == "TWN" & Class == "1.1" ~ 1,
      CNT == "TWN" & Class == "1.2" ~ 3,
      CNT == "TWN" & Class == "1.3" ~ 2,
      CNT == "TWN" & Class == "2.1" ~ 1,
      CNT == "TWN" & Class == "2.2" ~ 3,
      CNT == "TWN" & Class == "2.3" ~ 2
      #----
      ), levels = 1:3, labels = classes3M2)) %>% dplyr::select(-proportion, -C, -Class, -Cycle) , by = c("CNT","Group","ClassN")) %>%
  mutate(Group = factor(Group, levels = c("1", "2"), labels = c("Boy", "Girl"))) %>% select(CNT, Group, ClassN, `1999`, `2009`, `2016`)

sizeMGGndr3 <- sizeMGGndr3 %>% reshape2::melt(id.vars = c("CNT","Group","ClassN"), variable.name = "Cycle") %>% 
  arrange(CNT, Group, Cycle) %>% 
  group_by(CNT, Group, Cycle) %>%
  mutate(countT= sum(value, na.rm = TRUE)) %>%
  group_by(ClassN) %>%
  mutate(per=value/countT) %>% dplyr::select(CNT, Group, Cycle, ClassN, per) 

ByGroupHighProbMG(ByCNTMGGndr3, sizeMGGndr3, orden = c(5,2,1,3,4,6), n = 1:2, title = "Probabilities of Agree and size for each class in Gender Multigroup analysis by Cycle")


sizeMGGndr3 %>% 
  reshape2::dcast(CNT + Cycle ~ Group + ClassN) %>% 
  kbl(caption = "Size three classes MG Gender", booktabs = TRUE, longtable = TRUE, row.names = FALSE, 
      digits = 3, col.names = c("Country","Year", classes3M2, classes3M2)) %>%
  kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"),
                position = "left", font_size = 11, full_width = FALSE) %>%     
  column_spec(1:2, width = "2em") %>% 
  column_spec(3:8, width = "4em") %>% 
  add_header_above(c(" " = 1, " " = 1, "Boy" = 3, "Girl" = 3)) %>%  
  collapse_rows(1, valign = "top") %>% 
  print()
cat('\n')
cat('\n')