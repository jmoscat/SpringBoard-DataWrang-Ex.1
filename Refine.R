library(dplyr)
library(tidyr)
data <- read.csv("refine.csv", header = TRUE, sep = ",")
data_fr <- tbl_df(data)
data_fr <- mutate_each(data_fr, funs(toupper))
data_fr <- data_fr %>% separate(Product.code...number, c("product_code", "product_number"), "-")

#Creating product name
data_fr <- data_fr %>% 
  filter(product_code == "P")%>% 
  mutate(product = ifelse(product_code == "P", "Smartphone", ifelse(product_code == "V", "TV", ifelse(product_code == "x", "Laptop",ifelse(product_code == "Q", "Tablet","" )))))%>% 
  select (-product_code)

#Creating product name
data_fr <- data_fr %>% unite(geo_address, address, city, country, sep = ",")

#Creating binary columns
data_fr <- data_fr %>% mutate (company_philips = ifelse(company == "PHILLIPS", 1, 0))
data_fr <- data_fr %>% mutate (company_akzo = ifelse(company == "AKZO", 1, 0))
data_fr <- data_fr %>% mutate (company_van_houten = ifelse(company == "VAN HOUTEN", 1, 0))
data_fr <- data_fr %>% mutate (company_unilever = ifelse(company == "UNILVER", 1, 0))

data_fr <- data_fr %>% mutate (product_smartphone = ifelse(product_code == "p", 1, 0))
data_fr <- data_fr %>% mutate (product_tv = ifelse(product_code == "v", 1, 0))
data_fr <- data_fr %>% mutate (product_laptop = ifelse(product_code == "x", 1, 0))
data_fr <- data_fr %>% mutate (product_tablet = ifelse(product_code == "q", 1, 0))

write.csv(data_fr, file = "refine_clean.csv")