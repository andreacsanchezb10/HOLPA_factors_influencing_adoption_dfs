factors_list<-read_excel("factors_list.xlsx",sheet = "factors_list")
names(factors_list)

global_survey <- read_excel("factors_list.xlsx",sheet = "holpa_survey")%>%
  #select only the necessary columns
  select("type", "name","label::English ((en))")%>%
  #rename columns names
  rename("label_question" = "label::English ((en))")%>%
  rename("name_question" = "name")%>%
  #remove rows without questions
  filter(!type%in%c("begin_group","begin_repeat","end_repeat","end_group","start","end"))%>%
  #separate question type components
  mutate(type_question = ifelse(substr(type,1,10)=="select_one","select_one",
                                ifelse(substr(type,1,10)=="select_mul","select_multiple",type)))%>%
  #create column with list_name codes matching the choices worksheet
  mutate(list_name = if_else(type_question== "select_one"|type_question== "select_multiple", 
                             str_replace(.$type, paste0(".*", .$type_question), ""),NA))%>%
  mutate(list_name = str_replace_all(list_name, " ", ""))  #%>% mutate(global_r_list_name =  sub('*_', "", name_question)) %>%mutate(global_r_list_name = ifelse(grepl("_", global_r_list_name, fixed = TRUE)==TRUE,global_r_list_name,""))

global_choices <- read_excel("factors_list.xlsx",sheet = "holpa_choices")%>%
  select("list_name","name","label::English ((en))","name_new")%>%
  rename("label_choice" = "label::English ((en))")%>%
  rename("name_choice" = "name")

factors_list<-read_excel("factors_list.xlsx",sheet = "factors_list")%>%
  left_join(global_survey,by=c("column_name_old"="name_question"))%>%
  filter(!is.na(column_name_new))%>%
  mutate(type_question=if_else(is.na(type_question),metric_type,type_question))

categorical_choices_new<-read_excel("factors_list.xlsx",sheet = "factors_list")%>%
  left_join(global_survey,by=c("column_name_old"="name_question"))%>%
  filter(!is.na(column_name_new))%>%
  left_join(global_choices,by="list_name")%>%
  filter(!is.na(name_new))

colnames(per_data_clean)

#### categorical and binary data as factor 
common_cols <- intersect(colnames(per_data_clean), unique(categorical_choices_new$column_name_new))
print(common_cols)

df<-per_data_clean
# Replace values dynamically for all common columns
for (col in common_cols) {
  mapping <- categorical_choices_new %>%
    filter(column_name_new == col) %>%
    select(name_choice, name_new) %>%
    { setNames(.$name_new, .$name_choice) }  # Alternative to deframe()
  
  df[[col]] <- recode(df[[col]], !!!mapping)
}

sort(unique(df$marital_status))
sort(unique(df$gender))




# Check results
str(df$gender)
str(df$marital_status)


columns_factor <- intersect(factors_list$column_name_new[factors_list$metric_type %in%c("categorical","binary")], colnames(per_data))
print(columns_factor)  # Check if it holds expected values

per_data_clean<- per_data_clean%>%
  mutate(across(all_of(columns_factor), as.factor))
