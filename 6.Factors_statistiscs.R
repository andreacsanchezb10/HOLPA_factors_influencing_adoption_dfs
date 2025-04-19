library(dplyr)
library(readxl)
#############################################################    
########## UPLOAD DATA #####-----
#############################################################
per_factors_list<-read_excel("factors_list.xlsx",sheet = "per_factors_list")%>%
  filter(is.na(peru_remove))

per_adoptionBinary_outcomes<-read_excel("factors_list.xlsx",sheet = "per_factors_list")%>%
  filter(category_1=="outcome")%>%
  filter(str_detect(factor, "adoption"))
per_adoptionBinary_outcomes<-per_adoptionBinary_outcomes$column_name_new
per_adoptionBinary_outcomes

per_structural_model<-read_excel("factors_list.xlsx",sheet = "structural_model")
filter(peru_binary=="peru")

per_data_clean<- read.csv("per_data_Binary.csv",sep=",")

per_adoptionBinary_selectedFactors<- read.csv("results/per_adoptionBinary_selectedFactors.csv",sep=",")%>%
  rename("column_name_new"="selected_factors")%>%
  left_join(per_factors_list%>%select(category_1,constructs,factor,constructs_type,weights,column_name_new),by="column_name_new")%>%
  select(category_1,constructs, column_name_new,factor, constructs, constructs_type,weights)%>%
  mutate(country="Peru",
         outcome= "Adoption binary")
  rbind(c(category_1="outcome",
          constructs="dfs_adoption_binary", 
          column_name_new="dfs_adoption_binary",
          constructs_type="composite", 
          weights="mode_A"))%>%
  filter(constructs!="main_crop")%>%
 
sort(unique(per_adoptionBinary_selectedFactors$constructs))


#############################################################    
########## SELECTED FACTORS #####-----
#############################################################
##=== Select most important factors ----
per_data_adoptionBinary_analysis<- per_data_clean%>%
  select(kobo_farmer_id,dfs_adoption_binary,
    all_of(per_adoptionBinary_selectedFactors$column_name_new))
  mutate(across(everything(), ~ as.numeric(as.character(.))))

names(per_data_adoptionBinary_analysis)
str(per_data_adoptionBinary_analysis)
dim(per_data_adoptionBinary_analysis)#[1] 200   22
summary(per_data_adoptionBinary_analysis)
describe(per_data_adoptionBinary_analysis)


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
    left_join(factors_list%>%select(category_1,category_2,constructs,constructs_type,weights,factor, metric, metric_type,column_name_new,description),by="column_name_new")
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
    left_join(factors_list%>%select(category_1,category_2,category_3,factor, metric, metric_type,categorical_type,column_name_new,description),by="column_name_new")%>%
    filter(!is.na(metric_type))%>%
    distinct(column_name_new2, name_choice, Count, .keep_all = TRUE)
}

#############################################################    
########## SUMMARY STATISTICS #####-----
#############################################################
##=== ADOPTION BINARY OUTCOME ====
per_data_adoptionBinary_outcome<- per_data_clean%>%
  select(all_of(per_adoptionBinary_outcomes))%>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))%>%
  pivot_longer(
    cols = everything(),
    names_to = "practice",
    values_to = "adoption"
  )%>%
  group_by(practice,adoption)%>%
  summarise(n_farmers=n(),
            .groups = "drop") %>%
  mutate(practice_clean = case_when(
    practice=="dfs_agroforestry_adoption"~"Agroforestry",
    practice=="dfs_intercropping_adoption" ~"Intercropping",
    practice=="dfs_cover_crops_adoption"~"Cover crops",
    practice=="dfs_crop_rotation_adoption" ~"Crop rotation" ,
    practice=="dfs_strip_vegetation_adoption"~"Embedded seminatural habitats",
    
    practice=="dfs_fallow_adoption" ~"Fallow",
    practice=="dfs_hedgerows_adoption"~"Hedgerows",
    practice=="dfs_homegarden_adoption"~"Homegarden",
    practice=="dfs_adoption_binary"~"Total",
    
    TRUE~"NA")) %>%
    arrange(factor(practice_clean, levels = c("Total", "Homegarden", "Hedgerows",
                                           "Fallow","Embedded seminatural habitats",
                                          "Crop rotation","Cover crops","Intercropping", "Agroforestry")))

levels_m_dp_recla<- c("Total", "Homegarden", "Hedgerows",
                      "Fallow","Embedded seminatural habitats",
                      "Crop rotation","Cover crops","Intercropping", "Agroforestry")


ggplot(per_data_adoptionBinary_outcome, aes(x = n_farmers,y= factor(practice_clean, levels_m_dp_recla), fill = factor(adoption))) +
  geom_col(position = "stack") + 
  scale_fill_manual(values = c("0" = "grey70", "1" = "forestgreen"),
                  labels = c("Not-dopters", "Adopters"),
                  name = "Adoption status") +
  scale_x_continuous(expand = c(0, 0),limits = c(0,200),
                     breaks = c(0,20,40,60,80,100,120,140,160,180,200)) +
  labs(x = "Number of farmers",
       y = "")+
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.y =element_text(color="black",size=11, family = "sans"),
    axis.text.x=element_text(color="black",size=11, family = "sans"),
    axis.title =element_text(color="black",size=13, face = "bold",family = "sans"),
    panel.border = element_blank(),
    axis.line = element_line(color="grey50", size= 1),
    axis.ticks.y=element_blank(),
    axis.ticks.x=element_line(color="grey50", size= 1),
    legend.position = "none",
    panel.background = element_rect(fill = "white"),
    plot.margin = unit(c(t=0.5,r=0.5,b=0.5,l=0.5), "cm"))
p_sytems_stages
  
  geom_bar(aes(fill = adoption))
  geom_bar()   # Fill makes proportion bars
  facet_wrap(~ Practice, ncol = 3, scales = "free_y")   # Facet by practice
  scale_y_continuous(labels = scales::percent_format()) +
  
  labs(
    x = "Adoption Status",
    y = "Percentage of Farmers",
    title = "Distribution of Adoption Status Across Practices"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

head(per_data_adoptionBinary_outcome)


##=== SELECTED FACTORS ====
#--- Numerical factors -----
columns_numeric <- intersect(per_factors_list$column_name_new[per_factors_list$metric_type == "continuous"], colnames(per_data_adoptionBinary_analysis))
print(columns_numeric)  # Check if it holds expected values

per_summary_numerical <- summary_stats_num(per_data_adoptionBinary_analysis,columns_numeric,per_factors_list)%>%
  mutate(mean=round(mean,1),
         sd=round(sd,1),
         statistic= paste0(mean," (",sd,") [",min,"-",max,"]"),
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
columns_factor_so <- intersect(per_factors_list$column_name_new[per_factors_list$metric_type %in%c("categorical","binary")], colnames(per_data_adoptionBinary_analysis))
print(columns_factor_so)  # Check if it holds expected values

#(select_multiple)
columns_factor_sm <- intersect(per_categorical_choices$column_name_new2[per_categorical_choices$type_question %in%c("select_multiple")], colnames(per_data_adoptionBinary_analysis))
print(columns_factor_sm)  # Check if it holds expected values

columns_factor<-c(columns_factor_so, columns_factor_sm)
print(columns_factor)  # Check if it holds expected values

per_summary_categorical <- summary_stats_factor(per_data_adoptionBinary_analysis,columns_factor,per_categorical_choices,per_factors_list)%>%
  mutate(name_label= paste0(name_choice,": ",label_choice),
         statistic= paste0(Count," (",Percentage,"%)"))%>%
  select(category_1,factor,name_label,statistic)%>%
  rbind(per_summary_numerical)
sort(unique(per_summary_categorical$column_name_new2))

print(per_summary_categorical)  # Check if it holds expected values
sort(unique(per_summary_categorical$factor))
write.csv(per_summary_categorical,"results/per_summary_selectedFactors.csv",row.names=FALSE)






#############################################

# Load required libraries
library(ggplot2)
library(ggh4x)  # for grouping factors into categories


# Set factor levels for nice ordering (reverse for top-to-bottom order)
per_adoptionBinary_selectedFactors$factor <- factor(per_adoptionBinary_selectedFactors$factor,
                                                             levels = (unique(per_adoptionBinary_selectedFactors$factor)))
per_adoptionBinary_selectedFactors$category_1 <- factor(data_plot$category_1, 
                                                        levels = c("BIOPHYSICAL CONTEXT",
                                                                   "FARM MANAGEMENT CHARACTERISTICS",
                                                                   "FARMERS' BEHAVIOUR",
                                                                   "FINANCIAL CAPITAL",
                                                                   "NATURAL CAPITAL",
                                                                   "ACCESS TO KNOWLEDGE",
                                                                   
                                                                   
                                                                   "SOCIAL CAPITAL", "Physical Capital", "Institutional"))
overall_strips <- strip_themed(
  background_y = elem_list_rect(fill = c("black")),
  text_y = elem_list_text(size= 0.0005, colour= c("black"), angle = 90),
  by_layer_y = FALSE
)

theme_overall <- theme(
  strip.placement.y = "outside",
  axis.title.y = element_blank(),
  axis.title.x = element_text(color="black", size=13, family = "sans", face = "bold", vjust = -1),
  axis.text.x = element_text(color="black", size=12, family = "sans"),
  plot.background = element_rect(fill = "White", color = "White"),
  panel.background = element_blank(),
  axis.line = element_line(colour = "black")
)


# Replace facet_col() with facet_wrap()
ggplot(per_adoptionBinary_selectedFactors, aes(x = outcome, y = factor)) +
  geom_point(aes(color = country, shape = outcome), size = 8) +
  facet_grid2(vars(category_1),
              scales= "free", space='free_y', switch = "y",
              strip = overall_strips)+
  theme_overall
  labs(
    y = "Ucayali, Peru")
  
  
  facet_wrap(~category_1, ncol = 1, scales = "free_y")              # <<< THIS
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.spacing = unit(2, "lines"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 12),
    axis.text.y = element_text(size = 10)
  ) +
  labs(
    y = "Selected Factors",
    color = "Country",
    shape = "Outcome",
    title = "Selected Factors for Adoption and Intensity Across Countries"
  )
