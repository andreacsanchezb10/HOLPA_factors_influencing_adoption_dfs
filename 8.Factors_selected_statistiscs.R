library(dplyr)
library(readxl)
#############################################################    
########## UPLOAD DATA #####-----
#############################################################
factors_list_analysis<-read_excel("factors_list.xlsx",sheet = "factors_list_analysis")

per_structural_model<-read_excel("factors_list.xlsx",sheet = "structural_model")

per_data_clean<- read.csv("per_data_Binary.csv",sep=",")%>%
  dplyr::select(-X)

per_selectedFactors<- rbind(
  read.csv("results/direct/per/per_adoption_binary_selectedFactors.csv",sep=","),
  read.csv("results/indirect/per/per_household_shock_recover_capacity_selectedFactors.csv",sep=","),
  read.csv("results/indirect/per/per_influence_nr_selectedFactors.csv",sep=","),
  read.csv("results/indirect/per/per_training_participation_selectedFactors.csv",sep=","))%>%
  rename("column_name_new"="selected_factors")%>%
  mutate(path="Complete path")

length(per_selectedFactors$column_name_new) #47

per_selectedFactors<-per_selectedFactors%>%distinct(column_name_new, .keep_all = TRUE)
length(per_selectedFactors$column_name_new) #35
sort(per_selectedFactors$column_name_new) #35

#############################################################    
########## SELECTED FACTORS #####-----
#############################################################
##=== Select most important factors ----
per_data_analysis<- per_data_clean%>%
  dplyr::select(all_of(per_selectedFactors$column_name_new),dfs_adoption_binary)

names(per_data_analysis)
str(per_data_analysis)
dim(per_data_analysis)#[1] 200   36
summary(per_data_analysis)
describe(per_data_analysis)


library(tidyr)
library(stringr)
library(ggplot2)
library(patchwork) # For combining multiple plots
library(caret)
library(AppliedPredictiveModeling)
library(readxl)
library(ggplot2)
library(reshape2)
library(psych)

global_survey<-read.csv("h_global_survey.csv",sep=",")
per_global_choices<-read.csv("per_global_choices.csv",sep=",")


#############################################################
########## FUNCTIONS TO CALCULATE SUMMARY STATISTICS #####-----
#############################################################
summary_stats_num <- function(df,numeric_valid_columns,factors_list) {
  df %>%
    #select(-kobo_farmer_id) %>%  # Exclude 'id' column
    select(all_of(numeric_valid_columns))%>%
    describe(.)%>%
    tibble::rownames_to_column("column_name_new") %>%
    left_join(factors_list%>%select(category_1,sub_category,constructs,constructs_type,weights,factor, metric, metric_type,column_name_new,description),by="column_name_new")
}

summary_stats_factor <- function(df,factor_valid_columns,categorical_choices,factors_list) {
  df %>%
    #select(-kobo_farmer_id) %>%  # Exclude 'id' column
    select(all_of(factor_valid_columns))%>%
    mutate(across(all_of(factor_valid_columns),as.factor))%>%
    #describe(.)%>%
    #tibble::rownames_to_column("column_name_new") %>%
    pivot_longer(cols = everything(), names_to = "column_name_new2", values_to = "name_choice") %>%
    group_by(column_name_new2, name_choice) %>%
    summarise(Count = n(), .groups = "drop") %>%
    ungroup() %>%
    group_by(column_name_new2) %>%
    mutate(Total = sum(Count)) %>%
    ungroup()%>%
    mutate(Percentage = (Count / Total) * 100)%>%
    left_join(categorical_choices%>%select(column_name_new2,type_question,name_choice),by= c("column_name_new2","name_choice"))%>%
    mutate(variable_category = case_when( type_question == "select_one" ~ paste0(column_name_new2, "_", name_choice), TRUE ~ column_name_new2))%>%
    select(-type_question)%>%
    left_join(categorical_choices%>%select(-name_choice),by= c("variable_category","column_name_new2"))%>%
    mutate(column_name_new = case_when(is.na(column_name_new) ~ column_name_new2,  TRUE ~ column_name_new))%>%
    select(-variable_category,-type_question)%>%
    mutate(label_choice= case_when(
      is.na(label_choice) & name_choice=="0" ~ "No",
      is.na(label_choice) & name_choice=="1" ~ "Yes",
      column_name_new %in%c("livestock_health","livestock_source","livestock_exotic_local","livestock_antibiotics",
                            "livestock_feed","livestock_injury","livestock_vaccinations","fair_price_livestock") &is.na(name_choice)~ "Farmers without livestock production",
      column_name_new %in%c("fair_price_crops") &is.na(name_choice)~ "Farmers without crop production",
      column_name_new2=="ethnicity"~name_choice,
      TRUE ~ label_choice))%>%
    left_join(factors_list%>%select(category_1,sub_category,category_3,factor, metric, metric_type,categorical_type,column_name_new,description),by="column_name_new")%>%
    filter(!is.na(metric_type))%>%
    distinct(column_name_new2, name_choice, Count, .keep_all = TRUE)
}

#############################################################    
########## SUMMARY STATISTICS #####-----
#############################################################
##=== ADOPTION BINARY OUTCOME ====
per_dfs_adoption<- per_data_analysis%>%
  select(dfs_adoption_binary)%>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))%>%
  pivot_longer(
    cols = everything(),
    names_to = "practice",
    values_to = "adoption"
  )%>%
  group_by(practice,adoption)%>%
  summarise(n_farmers=n(),
            .groups = "drop") %>%
  mutate(percent_farmers= (n_farmers/200)*100)%>%
  mutate(adoption_label = case_when(
    adoption=="1"~"Adopters",
    TRUE~"Non-adopters")) 
per_dfs_adoption$adoption_label <- factor(per_dfs_adoption$adoption_label, levels = c("Non-adopters", "Adopters"))


ggplot(per_dfs_adoption, aes(x = percent_farmers,y= adoption_label, fill = factor(adoption))) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_manual(values = c("0" = "grey70", "1" = "forestgreen"),
                  labels = c("Not-dopters", "Adopters"),
                  name = "Adoption status") +
  scale_x_continuous(expand = c(0, 0),limits = c(0,100),
                     breaks = c(0,20,40,60,80,100,120,140,160,180,200)) +
  labs(x = "Percentage of farmers",
       y = "")+
 
  theme(
    #panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.grid.major.x  = element_line(color = "grey85",size = 0.6),
    axis.text.y =element_text(color="black",size=15, family = "sans"),
    axis.text.x=element_text(color="black",size=13, family = "sans"),
    axis.title =element_text(color="black",size=15, face = "bold",family = "sans"),
    panel.border = element_blank(),
    axis.line = element_line(color="grey50", size= 1),
    axis.ticks.y=element_blank(),
    axis.ticks.x=element_line(color="grey50", size= 1),
    legend.position = "none",
    panel.background = element_rect(fill = "white"),
    plot.margin = unit(c(t=0.5,r=0.5,b=0.5,l=0.5), "cm"))


#landscape 12.17*7.48

per_selectedFactors_direct<- rbind(
  read.csv("results/direct/per/per_adoption_binary_selectedFactors.csv",sep=","))%>%
  rename("column_name_new"="selected_factors")%>%
  mutate(path="Direct path")

per_selectedFactors_plot<- rbind(
  read.csv("results/indirect/per/per_household_shock_recover_capacity_selectedFactors.csv",sep=","),
  read.csv("results/indirect/per/per_influence_nr_selectedFactors.csv",sep=","),
  read.csv("results/indirect/per/per_training_participation_selectedFactors.csv",sep=","))%>%
  rename("column_name_new"="selected_factors")%>%
  distinct(column_name_new, .keep_all = TRUE)%>%
  mutate(path="Indirect path")%>%
  rbind(per_selectedFactors_direct,
        per_selectedFactors)%>%
  group_by(path,category_1)%>%
  summarise(freq = n())%>%
  ungroup()
per_selectedFactors_plot$path <- factor(per_selectedFactors_plot$path, levels = c("Complete path", "Indirect path", "Direct path"))
per_selectedFactors_plot$category_1 <- factor(per_selectedFactors_plot$category_1, levels = c(
  "vulnerability_context",
  "social_capital",
  "P&I_context_knowledge",
  "P&I_context_value_chain",
  "physical_capital",
  "human_capital",
  "natural_capital",
  "financial_capital",
  "farmers_behaviour",
  "farm_management_characteristics",
  "biophysical_context"
))

sort(unique(per_selectedFactors_plot$category_1))
ggplot(per_selectedFactors_plot, aes(x = freq,y= factor(path), fill = factor(category_1),
                                     color=factor(category_1))) +
  geom_bar(stat="identity")+
  scale_x_continuous(expand = c(0, 0),limits = c(0,40))+
  labs(x = "Number of predictors",
       y = "")+
  scale_fill_manual(values=c("biophysical_context"= "#f0c602",
                      "farm_management_characteristics"="#F09319",
                      "farmers_behaviour"= "#ea6044",
                      "financial_capital"="#d896ff",
                      "natural_capital"=  "#87CEEB",
                      "human_capital"="#6a57b8",
                      "physical_capital"="#496491",
                      "P&I_context_value_chain"="#92c46d",
                      "P&I_context_knowledge"="#92c46d",
                      "social_capital"= "#297d7d",
                      "vulnerability_context"= "#297d7d"))+
  scale_color_manual(values=c("biophysical_context"= "#f0c602",
                             "farm_management_characteristics"="#F09319",
                             "farmers_behaviour"= "#ea6044",
                             "financial_capital"="#d896ff",
                             "natural_capital"=  "#87CEEB",
                             "human_capital"="#6a57b8",
                             "physical_capital"="#496491",
                             "P&I_context_value_chain"="#92c46d",
                             "P&I_context_knowledge"="#92c46d",
                             "social_capital"= "#297d7d",
                             "vulnerability_context"= "#297d7d"))+
  theme(
    #panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.grid.major.x  = element_line(color = "grey85",size = 0.6),
    axis.text.y =element_text(color="black",size=15, family = "sans"),
    axis.text.x=element_text(color="black",size=13, family = "sans"),
    axis.title =element_text(color="black",size=15, face = "bold",family = "sans"),
    panel.border = element_blank(),
    axis.line = element_line(color="grey50", size= 1),
    axis.ticks.y=element_line(color="grey50", size= 1),
    axis.ticks.x=element_line(color="grey50", size= 1),
    legend.position = "none",
    
    panel.background = element_rect(fill = "white"),
    plot.margin = unit(c(t=0.5,r=0.5,b=0.5,l=0.5), "cm"))
  
#landscape 12.17*10


##=== SELECTED FACTORS ====
#--- Numerical factors -----
per_columns_numeric <- intersect(factors_list_analysis$column_name_new[factors_list_analysis$metric_type %in% c("continuous","categorical")], colnames(per_data_analysis))
print(per_columns_numeric)  # Check if it holds expected values

per_summary_numerical <- summary_stats_num(per_data_analysis,per_columns_numeric,factors_list_analysis)%>%
  mutate(mean=round(mean,1),
         sd=round(sd,1),
         min=round(min,2),
         max=round(max,2),
         statistic= paste0(mean," (",sd,") [",min,", ",max,"]"),
         name_label=NA)%>%
  select(category_1,factor,name_label,statistic)

sort(unique(per_summary_numerical$factor))

#--- Categorical and binary factors -----
per_categorical_choices<-per_global_choices%>%
  mutate(name_new=as.character(name_new),
         name_choice= if_else(is.na(name_new),name_choice,name_new))%>%
  select(column_name_new,name_choice,label_choice,type_question)%>%
  filter(!is.na(name_choice))%>%
  mutate(variable_category=if_else(type_question=="select_one",paste0(column_name_new,"_",name_choice),column_name_new))%>%
  mutate(column_name_new2=if_else(type_question=="select_multiple",paste0(column_name_new,".",name_choice),column_name_new))

### For factor and binary variables
#(select_one)
per_columns_factor_so <- intersect(factors_list_analysis$column_name_new[factors_list_analysis$metric_type %in%c("binary")], colnames(per_data_analysis))
print(per_columns_factor_so)  # Check if it holds expected values

#(select_multiple)
#columns_factor_sm <- intersect(per_categorical_choices$column_name_new2[per_categorical_choices$type_question %in%c("select_multiple")], colnames(per_data_adoptionBinary_analysis))
#print(columns_factor_sm)  # Check if it holds expected values

per_columns_factor<-c(per_columns_factor_so, columns_factor_sm)
print(per_columns_factor)  # Check if it holds expected values

per_summary_categorical <- summary_stats_factor(per_data_analysis,per_columns_factor_so,per_categorical_choices,factors_list_analysis)%>%
  mutate(name_label= paste0(name_choice,": ",label_choice),
         statistic= paste0(Count," (",Percentage,"%)"))%>%
  select(category_1,factor,name_label,statistic)%>%
  rbind(per_summary_numerical)
sort(unique(per_summary_categorical$factor))

print(per_summary_categorical)  # Check if it holds expected values
sort(unique(per_summary_categorical$factor))
write.csv(per_summary_categorical,"results/per_summary_selectedFactors.csv",row.names=FALSE)

