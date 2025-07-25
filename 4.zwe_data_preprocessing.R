library(dplyr)
library(readxl)
library(reshape2)
library(summarytools)
library(corrplot)
library(caret)

#############################################################    
########## UPLOAD DATA #####-----
#############################################################
factors_list<-read_excel("factors_list.xlsx",sheet = "factors_list")
sort(unique(factors_list$category_1))

zwe_data_clean<- read.csv("zwe_data_clean.csv",sep=",")
sort(unique(zwe_data_clean$soil_erosion_perception))
zwe_summary_categorical<-read.csv("zwe_summary_categorical.csv",sep=",") #541
zwe_summary_numerical<-read.csv("zwe_summary_numerical.csv",sep=",")  #80 factors

factors_category<-zwe_summary_numerical%>%dplyr::select(column_name_new, category_1,sub_category)%>%
  rbind(zwe_summary_categorical%>%dplyr::select(column_name_new,category_1,sub_category))%>%
  distinct()

sort(unique(zwe_data_clean$province))

#### Select the factors that were listed as important for adoption according to:
# - data availability
# - systematic evidence map
# - meta-analysis
# - Dessart et al 2018
# - Context document (Zimbabwe)

zwe_variables_list<-c(unique(zwe_summary_categorical$column_name_new2),unique(zwe_summary_numerical$column_name_new))
zwe_variables_list

zwe_data_analysis<- zwe_data_clean%>%
  dplyr::select(kobo_farmer_id,all_of(zwe_variables_list))

rownames(zwe_data_analysis) <- zwe_data_analysis$kobo_farmer_id

zwe_data_analysis<- zwe_data_analysis%>%
  dplyr::select(-kobo_farmer_id)

dim(zwe_data_analysis) #201 farmers; 280 variables evaluated

a<-as.data.frame(c(colnames(zwe_data_analysis)))%>%
  rename("column_name_new"="c(colnames(zwe_data_analysis))")%>%
  left_join(factors_list%>%select(category_1,factor,column_name_new,constructs,constructs_type), by="column_name_new")%>%
  group_by(category_1) %>%
  mutate(column_name_new_count = n()) %>%
  tally()

ggplot(data=a, aes(x=n, y=category_1, fill= category_1)) +
  geom_bar(stat="identity")+
  geom_text(aes(label = n), 
            hjust = -0.2, # Adjust position of the label to the right of the bar
            size = 4) +
  theme_minimal() +
  labs(x = "Number of factors", y = "Category") +
  theme(legend.position = "none")

dim(zwe_data_analysis) #201 farmers; 20 outcomes; 260 factors; 
#[1] 201 280


#############################################################    
########## DATA TYPE CONVERSION #####-----
#############################################################

###### --- CATEGORICAL AND BINARY VARIABLES -----
#### Convert categorical and binary to factor
columns_categorical <- intersect(zwe_summary_categorical$column_name_new2, colnames(zwe_data_analysis))
print(columns_categorical)  # Check if it holds expected values

zwe_data_analysis<- zwe_data_analysis%>%
  mutate(across(all_of(columns_categorical), as.factor))
sort(unique(zwe_data_analysis$main_crops_annual ))

###### --- NUMERICAL VARIABLES -----
#### Convert continuous variables to numeric
columns_continuous <- intersect(zwe_summary_numerical$column_name_new, colnames(zwe_data_analysis))
print(columns_continuous)  # Check if it holds expected values

zwe_data_analysis<- zwe_data_analysis%>%
  mutate(across(all_of(columns_continuous), as.numeric))


#############################################################    
############# CREATING DUMMY VARIABLES -----
#############################################################
### Only for categorical nominal...
columns_categorical_nominal <- intersect(zwe_summary_categorical$column_name_new2[zwe_summary_categorical$categorical_type=="nominal" ], colnames(zwe_data_analysis))
print(columns_categorical_nominal)  # Check if it holds expected values

#filter out columns with fewer than 2 unique values.
# Ensure only columns with >1 unique level
valid_nominals <- columns_categorical_nominal[sapply(zwe_data_analysis[columns_categorical_nominal], function(col) {
  n_levels <- length(unique(col))
  return(n_levels > 1)
})]
valid_nominals

zwe_data_analysis[valid_nominals] <- lapply(zwe_data_analysis[valid_nominals], function(col) {
  if (!is.factor(col)) as.factor(col) else col
})

formula <- as.formula(paste("~", paste(valid_nominals, collapse = " + ")))

dummies <- dummyVars(formula, data = zwe_data_analysis)
dummy_matrix <- predict(dummies, newdata = zwe_data_analysis)

dummy_names <- colnames(predict(dummies, newdata = zwe_data_analysis))
print(dummy_names)

# Get the factor names and count their corresponding dummy columns
dummy_count <- sapply(columns_categorical_nominal, function(col) {
  sum(grepl(paste0("^", col, "\\."), dummy_names))
})
print(dummy_count)

binary_factors <- names(dummy_count[dummy_count == 2])
print(binary_factors)

dummies <- predict(dummies, newdata = zwe_data_analysis)%>%
  as.data.frame()%>%
  mutate(across(everything(), as.factor))  # not across_all() which is deprecated
str(dummies)
zwe_data_Binary <- zwe_data_analysis%>%
  cbind(dummies)%>%
  dplyr::select(-all_of(columns_categorical_nominal))


cols_to_remove <- sapply(binary_factors, function(col) {
  # Get the two dummy columns for the binary factor
  cols <- grep(paste0("^", col, "\\."), dummy_names, value = TRUE)
  
  if (length(cols) == 2) {
    # Count the number of "1"s in each column
    count_1_col1 <- sum(zwe_data_Binary[[cols[1]]] == 1, na.rm = TRUE)
    count_1_col2 <- sum(zwe_data_Binary[[cols[2]]] == 1, na.rm = TRUE)
    
    # Remove the column with fewer "1"s
    if (count_1_col1 > count_1_col2) {
      return(cols[2])  # Remove the second column if the first has more "1"s
    } else {
      return(cols[1])  # Remove the first column if the second has more "1"s
    }
  } else {
    return(NA)
  }
})
cols_to_remove

zwe_data_Binary <- zwe_data_Binary %>%
  dplyr::select(-all_of(cols_to_remove))

b<-as.data.frame(c(colnames(zwe_data_Binary)))%>%
  rename("column_name_new"="c(colnames(zwe_data_Binary))")%>%
  left_join(factors_list%>%
              dplyr::select(category_1,column_name_new), by="column_name_new")%>%
  mutate(category_1= case_when(
    column_name_new== "year_assessment.2023"~"biophysical_context",
    TRUE~category_1))%>%
  group_by(category_1) %>%
  mutate(column_name_new_count = n()) %>%
  tally()%>%filter(category_1!="xxx")

ggplot(data=b, aes(x=n, y=category_1, fill= category_1)) +
  geom_bar(stat="identity")+
  geom_text(aes(label = n), 
            hjust = -0.2, # Adjust position of the label to the right of the bar
            size = 4) +
  theme_minimal() +
  labs(x = "Number of factors", y = "Category") +
  theme(legend.position = "none")

dim(zwe_data_Binary) #201 farmers; 20 outcomes; 347 factors
#[1] 200 367

write.csv(zwe_data_Binary,"zwe_data_Binary.csv",row.names=TRUE)

