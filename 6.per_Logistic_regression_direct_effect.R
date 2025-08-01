library(dplyr)
library(readxl)
library(seminr)
library(purrr)
library(rlang)
library(psych)
library(ggplot2)

#############################################################    
########## UPLOAD DATA #####-----
#############################################################
factors_list_analysis<-read_excel("factors_list.prueba.xlsx",sheet = "factors_list_analysis")

#per_structural_model<-read_excel("factors_list.xlsx",sheet = "structural_model")

per_direct_predictors<- read.csv("results/per/direct/per_adoption_binary_selectedFactors.csv")
  select(path,category_1,constructs, column_name_new,constructs,factor, constructs_type,weights,country)%>%
  filter(country=="peru")%>%
  filter(str_detect(path, "^direct"))

sort(unique(per_direct_predictors$constructs))

#############################################################    
########## SELECTED FACTORS #####-----
#############################################################
per_data_analysis<-read.csv("per_data_Binary.csv",sep=",")
rownames(per_data_analysis) <- per_data_analysis$X
per_data_analysis<- per_data_analysis%>%
  dplyr::select(-X)%>%
  dplyr::select(dfs_adoption_binary,all_of(per_direct_predictors$selected_factors))%>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))

names(per_data_analysis)
str(per_data_analysis)
dim(per_data_analysis)#[1] 200   16
summary(per_data_analysis)
describe(per_data_analysis)


##=== STEP 3: CHECK FOR CORRELATION ACROSS FACTORS ======
# Function to calculate Spearman's correlation
create_cor_df <- function(data, factors_list_analysis) {
  cor_matrix <- cor(data %>% mutate(across(everything(), as.numeric)),
                    method = "spearman", use = "pairwise.complete.obs")
  
  cor_df <- as.data.frame(cor_matrix) %>%
    rownames_to_column("factor1") %>%  # <- fixed pipe here
    pivot_longer(-factor1, names_to = "factor2", values_to = "spearman_correlation") %>%
    left_join(factors_list_analysis %>% select(column_name_new, category_1),
              by = c("factor1" = "column_name_new")) %>%
    rename(category_1.factor1 = category_1) %>%
    left_join(factors_list_analysis %>% select(column_name_new, category_1),
              by = c("factor2" = "column_name_new")) %>%
    rename(category_1.factor2 = category_1)
  
  return(cor_df)
}



plot_correlation_betw_category <- function(cor_df, factor_info_df) {
  library(dplyr)
  library(ggplot2)
  
  # Step 1: Order categories: biophysical_context first, then alphabetically
  ordered_categories <- c("biophysical_context", sort(unique(factor_info_df$category_1[factor_info_df$category_1 != "biophysical_context"])))
  
  # Step 2: Assign order to factors based on category order and factor name
  factor_levels <- factor_info_df %>%
    #filter(category_1 != "outcome") %>%
    mutate(
      category_order = match(category_1, ordered_categories)
    ) %>%
    arrange(category_order, column_name_new) %>%
    distinct(column_name_new) %>%
    mutate(order = row_number()) %>%
    rename(factor = column_name_new)
  
  # Step 3: Add factor order info to cor_df
  cor_df <- cor_df %>%
    left_join(factor_levels %>% rename(factor1 = factor, order1 = order), by = "factor1") %>%
    left_join(factor_levels %>% rename(factor2 = factor, order2 = order), by = "factor2") %>%
    filter(order1 <= order2) %>%  # upper triangle only
    mutate(
      factor1 = factor(factor1, levels = factor_levels$factor),
      factor2 = factor(factor2, levels = factor_levels$factor)
    )
  
  # Step 4: Plot
  ggplot(cor_df, aes(x = factor1, y = factor2, fill = spearman_correlation)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(spearman_correlation, 2)), size =5) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                         limits = c(-1, 1), name = "Spearman\nCorrelation") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, size = 9),
      axis.text.y = element_text(size = 12),
      panel.grid.major = element_blank(),
      legend.text = element_text(color="black",size=14, family = "sans"),
      legend.title = element_text(color="black",size=14, family = "sans", face = "bold")
    ) +
    labs(
      title = "Peru",
      x = NULL,
      y = NULL
    )
}

per_factors_list <- as.data.frame(colnames(per_data_analysis))%>%
  rename("column_name_new"= "colnames(per_data_analysis)")%>%
  left_join(factors_list_analysis%>%select(column_name_new, category_1),by="column_name_new")
  filter(category_1!="outcome")

per_data_analysis_cor<-create_cor_df(per_data_analysis,per_factors_list)
str(per_data_analysis_cor)

plot_correlation_betw_category(per_data_analysis_cor, factors_list_analysis)
#13*11


## Apply the logistic regression model ====
#https://stats.oarc.ucla.edu/r/dae/logit-regression/
str(per_data_analysis$dfs_adoption_binary)
per_data_analysis$dfs_adoption_binary<- as.factor(per_data_analysis$dfs_adoption_binary)

per_logit_model <- glm(dfs_adoption_binary ~ ., 
                       data = per_data_analysis, 
                       family = binomial(link = "logit"))

summary(per_logit_model)

library(sjPlot)

tab_model(per_logit_model, show.ci = FALSE, file = "results/per/direct/per_logit_model_table.html")

exp(coef(per_logit_model))
per_logit_model.results<-as.data.frame(exp(cbind(OR = coef(per_logit_model), confint(per_logit_model))))





