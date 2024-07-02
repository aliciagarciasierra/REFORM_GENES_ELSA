
########################################################################
######## ROBUSTNESS TESTS ############################################## 
########################################################################


#####################################################################
######## TABLE S2. OPTIMAL BANDWIDTH  ###############################
#####################################################################


########################## EDUCATION ################################

# First, we select the optimal bandwidth using the function "rdbwselect" 

data$bandwidth<-NULL
bwsyoe <- rdbwselect(data$yoe,data$rabyear, p=1, c = 1933,
                  all=TRUE)
bwsyoe$bws
optimalbw<-bwsyoe$bws[1, 3] # we select the the mserd version bias corrected
print(optimalbw)

# Second, we filter the sample to account for those years within the optimal bandwidth
# (i.e. the cutoff point (1933) +,- the optimal bw)

optimal <- data %>%
  filter(rabyear >= 1927 & rabyear <= 1938)

# Third, we rerun the models with this new sample

m1<- feols(yoe~ treatment*pgeducation*ses +
          pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
          timetrend +
          male
        , data=optimal)
etable(m1, cluster="timetrend", digits = "r3", signif.code = signif_codes)


########################## INCOME ################################

# First, we select the optimal bandwidth using the function "rdbwselect" 

data$bandwidth<-NULL
bwsincome <- rdbwselect(data$incomelog,data$rabyear, p=1, c = 1933,
                     all=TRUE)
bwsincome$bws
optimalbw<-bwsincome$bws[1, 3] # we select the the mserd version bias corrected
print(optimalbw)

# Second, we filter the sample to account for those years within the optimal bandwidth
# (i.e. the cutoff point (1933) +,- the optimal bw)

optimal <- data %>%
  filter(rabyear >= 1927 & rabyear <= 1938)

# Third, we rerun the models with this new sample

m2<- feols(incomelog~ treatment*pgeducation*ses +
              pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
              timetrend +
              male
            , data=optimal)
etable(m2, cluster="timetrend", digits = "r3",  signif.code = signif_codes)


########################## WEALTH ################################

# First, we select the optimal bandwidth using the function "rdbwselect" 

data$bandwidth<-NULL
bwswealth <- rdbwselect(data$wealthlog,data$rabyear, p=1, c = 1933,
                        all=TRUE)
bwswealth$bws
optimalbw<-bwswealth$bws[1, 3] # we select the the mserd version bias corrected
print(optimalbw)

# Second, we filter the sample to account for those years within the optimal bandwidth
# (i.e. the cutoff point (1933) +,- the optimal bw)

optimal <- data %>%
  filter(rabyear >= 1927 & rabyear <= 1938)

# Third, we rerun the models with this new sample

m3<- feols(wealthlog~ treatment*pgeducation*ses +
              pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
              timetrend +
              male
            , data=optimal)
etable(m3, cluster="timetrend", digits = "r3",  signif.code = signif_codes)


#####################################################################
######## TABLE S3. PLACEBO TESTS CHANGING THE CUTOFF ################
#####################################################################

############################ CUTOFF 1928 ##########################

# Change treatment to 1928 and rerun the models
data$treatmentplacebo<-NA
data$treatmentplacebo[data$rabyear>=1928]<-1
data$treatmentplacebo[data$rabyear<1928]<-0
data$treatmentplacebo<-as.factor(data$treatmentplacebo)  #check it's a factor

# Model education
m1 <- feols(yoe~ treatmentplacebo*pgeducation*ses +
              pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
              timetrend +
              timetrend*pc1 + timetrend*pc4 + timetrend*pc7 +
              timetrend*pc2 + timetrend*pc5 + timetrend*pc8 +
              timetrend*pc3 + timetrend*pc6 + timetrend*pc9 + timetrend*pc10 +
              timetrend*pgeducation +
              timetrend*treatmentplacebo +
              timetrend*ses +
              male,
            data=data)
etable(m1, cluster="timetrend", digits = "r3",  signif.code = signif_codes)

# Model income 
m2 <- feols(incomelog~ treatmentplacebo*pgeducation*ses +
              pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
              timetrend +
              timetrend*pc1 + timetrend*pc4 + timetrend*pc7 +
              timetrend*pc2 + timetrend*pc5 + timetrend*pc8 +
              timetrend*pc3 + timetrend*pc6 + timetrend*pc9 + timetrend*pc10 +
              timetrend*pgeducation +
              timetrend*treatmentplacebo +
              timetrend*ses +
              male,
            data=data)
etable(m2, cluster="timetrend", digits = "r3",  signif.code = signif_codes)


# Model wealth
m3 <- feols(wealthlog~ treatmentplacebo*pgeducation*ses +
              pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
              timetrend +
              timetrend*pc1 + timetrend*pc4 + timetrend*pc7 +
              timetrend*pc2 + timetrend*pc5 + timetrend*pc8 +
              timetrend*pc3 + timetrend*pc6 + timetrend*pc9 + timetrend*pc10 +
              timetrend*pgeducation +
              timetrend*treatmentplacebo +
              timetrend*ses +
              male,
            data=data)
etable(m3, cluster="timetrend", digits = "r3",  signif.code = signif_codes)



############################ CUTOFF 1930 ##########################

# Change treatment to 1930 and rerun the models
data$treatmentplacebo<-NA
data$treatmentplacebo[data$rabyear>=1930]<-1
data$treatmentplacebo[data$rabyear<1930]<-0
data$treatmentplacebo<-as.factor(data$treatmentplacebo)

# Model education
m4 <- feols(yoe~ treatmentplacebo*pgeducation*ses +
              pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
              timetrend +
              timetrend*pc1 + timetrend*pc4 + timetrend*pc7 +
              timetrend*pc2 + timetrend*pc5 + timetrend*pc8 +
              timetrend*pc3 + timetrend*pc6 + timetrend*pc9 + timetrend*pc10 +
              timetrend*pgeducation +
              timetrend*treatmentplacebo +
              timetrend*ses +
              male,
            data=data)
etable(m4, cluster="timetrend", digits = "r3",  signif.code = signif_codes)

# Model income 
m5 <- feols(incomelog~ treatmentplacebo*pgeducation*ses +
              pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
              timetrend +
              timetrend*pc1 + timetrend*pc4 + timetrend*pc7 +
              timetrend*pc2 + timetrend*pc5 + timetrend*pc8 +
              timetrend*pc3 + timetrend*pc6 + timetrend*pc9 + timetrend*pc10 +
              timetrend*pgeducation +
              timetrend*treatmentplacebo +
              timetrend*ses +
              male,
            data=data)
etable(m5, cluster="timetrend", digits = "r3",  signif.code = signif_codes)


# Model wealth
m6 <- feols(wealthlog~ treatmentplacebo*pgeducation*ses +
              pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
              timetrend +
              timetrend*pc1 + timetrend*pc4 + timetrend*pc7 +
              timetrend*pc2 + timetrend*pc5 + timetrend*pc8 +
              timetrend*pc3 + timetrend*pc6 + timetrend*pc9 + timetrend*pc10 +
              timetrend*pgeducation +
              timetrend*treatmentplacebo +
              timetrend*ses +
              male,
            data=data)
etable(m6, cluster="timetrend", digits = "r3",  signif.code = signif_codes)

############################ CUTOFF 1936 ##########################

# Change treatment to 1936 and rerun the models
data$treatmentplacebo<-NA
data$treatmentplacebo[data$rabyear>=1936]<-1
data$treatmentplacebo[data$rabyear<1936]<-0
data$treatmentplacebo<-as.factor(data$treatmentplacebo)

# Model education
m7 <- feols(yoe~ treatmentplacebo*pgeducation*ses +
              pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
              timetrend +
              timetrend*pc1 + timetrend*pc4 + timetrend*pc7 +
              timetrend*pc2 + timetrend*pc5 + timetrend*pc8 +
              timetrend*pc3 + timetrend*pc6 + timetrend*pc9 + timetrend*pc10 +
              timetrend*pgeducation +
              timetrend*treatmentplacebo +
              timetrend*ses +
              male,
            data=data)
etable(m7, cluster="timetrend", digits = "r3",  signif.code = signif_codes)

# Model income 
m8 <- feols(incomelog~ treatmentplacebo*pgeducation*ses +
              pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
              timetrend +
              timetrend*pc1 + timetrend*pc4 + timetrend*pc7 +
              timetrend*pc2 + timetrend*pc5 + timetrend*pc8 +
              timetrend*pc3 + timetrend*pc6 + timetrend*pc9 + timetrend*pc10 +
              timetrend*pgeducation +
              timetrend*treatmentplacebo +
              timetrend*ses +
              male,
            data=data)
etable(m8, cluster="timetrend", digits = "r3",  signif.code = signif_codes)


# Model wealth
m9 <- feols(wealthlog~ treatmentplacebo*pgeducation*ses +
              pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
              timetrend +
              timetrend*pc1 + timetrend*pc4 + timetrend*pc7 +
              timetrend*pc2 + timetrend*pc5 + timetrend*pc8 +
              timetrend*pc3 + timetrend*pc6 + timetrend*pc9 + timetrend*pc10 +
              timetrend*pgeducation +
              timetrend*treatmentplacebo +
              timetrend*ses +
              male,
            data=data)
etable(m9, cluster="timetrend", digits = "r3",  signif.code = signif_codes)


############################ CUTOFF 1938 ##########################

# Change treatment to 1938 and rerun the models
data$treatmentplacebo<-NA
data$treatmentplacebo[data$rabyear>=1938]<-1
data$treatmentplacebo[data$rabyear<1938]<-0
data$treatmentplacebo<-as.factor(data$treatmentplacebo)

# Model education
m10 <- feols(yoe~ treatmentplacebo*pgeducation*ses +
              pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
              timetrend +
              timetrend*pc1 + timetrend*pc4 + timetrend*pc7 +
              timetrend*pc2 + timetrend*pc5 + timetrend*pc8 +
              timetrend*pc3 + timetrend*pc6 + timetrend*pc9 + timetrend*pc10 +
              timetrend*pgeducation +
              timetrend*treatmentplacebo +
              timetrend*ses +
              male,
            data=data)
etable(m10, cluster="timetrend", digits = "r3",  signif.code = signif_codes)

# Model income 
m11 <- feols(incomelog~ treatmentplacebo*pgeducation*ses +
              pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
              timetrend +
              timetrend*pc1 + timetrend*pc4 + timetrend*pc7 +
              timetrend*pc2 + timetrend*pc5 + timetrend*pc8 +
              timetrend*pc3 + timetrend*pc6 + timetrend*pc9 + timetrend*pc10 +
              timetrend*pgeducation +
              timetrend*treatmentplacebo +
              timetrend*ses +
              male,
            data=data)
etable(m11, cluster="timetrend", digits = "r3",  signif.code = signif_codes)


# Model wealth
m12 <- feols(wealthlog~ treatmentplacebo*pgeducation*ses +
              pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
              timetrend +
              timetrend*pc1 + timetrend*pc4 + timetrend*pc7 +
              timetrend*pc2 + timetrend*pc5 + timetrend*pc8 +
              timetrend*pc3 + timetrend*pc6 + timetrend*pc9 + timetrend*pc10 +
              timetrend*pgeducation +
              timetrend*treatmentplacebo +
              timetrend*ses +
              male,
            data=data)
etable(m12, cluster="timetrend", digits = "r3",  signif.code = signif_codes)

######################################################################
######## TABLE S4. GENDER DIFFERENCES  ############################### 
######################################################################

######################## EDUCATION ############################## 

# Filter dataset to separeate both genders
data_female <- data %>% filter(male == 0)
data_male <- data %>% filter(male == 1)

# Fit the models
m1_female <- feols(yoe ~ treatment * pgeducation * ses +
                     pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 +
                     timetrend +
                     timetrend * pc1 + timetrend * pc4 + timetrend * pc7 +
                     timetrend * pc2 + timetrend * pc5 + timetrend * pc8 +
                     timetrend * pc3 + timetrend * pc6 + timetrend * pc9 + timetrend * pc10 +
                     timetrend * pgeducation +
                     timetrend * treatment +
                     timetrend * ses ,
                   data = data_female)


m1_male <- feols(yoe ~ treatment * pgeducation * ses +
                     pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 +
                     timetrend +
                     timetrend * pc1 + timetrend * pc4 + timetrend * pc7 +
                     timetrend * pc2 + timetrend * pc5 + timetrend * pc8 +
                     timetrend * pc3 + timetrend * pc6 + timetrend * pc9 + timetrend * pc10 +
                     timetrend * pgeducation +
                     timetrend * treatment +
                     timetrend * ses ,
                   data = data_male)

# Display the results
etable(m1_female, cluster = "timetrend", digits = "r3",signif.code = signif_codes)
etable(m1_male, cluster = "timetrend", digits = "r3", signif.code = signif_codes)

######################## INCOME ############################## 


# Fit the models
m2_female <- feols(incomelog ~ treatment * pgeducation * ses +
                     pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 +
                     timetrend +
                     timetrend * pc1 + timetrend * pc4 + timetrend * pc7 +
                     timetrend * pc2 + timetrend * pc5 + timetrend * pc8 +
                     timetrend * pc3 + timetrend * pc6 + timetrend * pc9 + timetrend * pc10 +
                     timetrend * pgeducation +
                     timetrend * treatment +
                     timetrend * ses ,
                   data = data_female)


m2_male <- feols(incomelog ~ treatment * pgeducation * ses +
                   pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 +
                   timetrend +
                   timetrend * pc1 + timetrend * pc4 + timetrend * pc7 +
                   timetrend * pc2 + timetrend * pc5 + timetrend * pc8 +
                   timetrend * pc3 + timetrend * pc6 + timetrend * pc9 + timetrend * pc10 +
                   timetrend * pgeducation +
                   timetrend * treatment +
                   timetrend * ses ,
                 data = data_male)

# Display the results
etable(m2_female, cluster = "timetrend", digits = "r3",signif.code = signif_codes)
etable(m2_male, cluster = "timetrend", digits = "r3",signif.code = signif_codes) 

############################## WEALTH ####################################

# Fit the models
m3_female <- feols(wealthlog ~ treatment * pgeducation * ses +
                     pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 +
                     timetrend +
                     timetrend * pc1 + timetrend * pc4 + timetrend * pc7 +
                     timetrend * pc2 + timetrend * pc5 + timetrend * pc8 +
                     timetrend * pc3 + timetrend * pc6 + timetrend * pc9 + timetrend * pc10 +
                     timetrend * pgeducation +
                     timetrend * treatment +
                     timetrend * ses ,
                   data = data_female)


m3_male <- feols(wealthlog ~ treatment * pgeducation * ses +
                   pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 +
                   timetrend +
                   timetrend * pc1 + timetrend * pc4 + timetrend * pc7 +
                   timetrend * pc2 + timetrend * pc5 + timetrend * pc8 +
                   timetrend * pc3 + timetrend * pc6 + timetrend * pc9 + timetrend * pc10 +
                   timetrend * pgeducation +
                   timetrend * treatment +
                   timetrend * ses ,
                 data = data_male)

# Display the results
etable(m3_female, cluster = "timetrend", digits = "r3",signif.code = signif_codes)
etable(m3_male, cluster = "timetrend", digits = "r3",signif.code = signif_codes) 


#####################################################################
######## FIGURE S5: THREE WAY INTERACTION FOR MALES ##################
#####################################################################

a<-plot_model(m3_male, type = "pred", terms = c("pgeducation[-3:3]","ses[-1.1426, -0.2473, 0.7228]", "treatment[0,1]"))

a<- a+ theme_pilot(axis_title_size = 11,
                   axis_text_size = 11,
                   legend_text_size = 12,
                   legend_title_size = 12,
                   legend_position= "right") +
  labs(x= "PGI",
       y = "Predicted income") +
  ggtitle(" ") +
  theme(legend.position = "bottom",
        legend.box = "horizontal") +
  scale_color_manual(name = " ", values = line_colors, labels = c("Low SES", "Medium SES", "High SES")) +
  scale_fill_manual(name = " ", values = ribbon_colors, labels = c("Low SES", "Medium SES", "High SES")) 

a$data$facet<- ifelse(a$data$facet== 0, "Non-exposed to the reform",
                      ifelse(a$data$facet== 1, "Exposed to the reform",NA))

# Relevel the facet variable to ensure correct order and labeling
a$data$facet <- factor(a$data$facet, levels = c("Non-exposed to the reform", "Exposed to the reform"))

a


################################################################################
############################# BINARY EDUCATION LEVEL  ##########################
################################################################################

# Select variables including tertiary binary variable 
# (note that original data needst to be originated from file 01_DATA_CLEANING)

myvars<-c("idauniq", # id
          "rabyear", # year of birth
          "timetrend", #time trend
          "treatment", #treatment
          "yoe",  "incomelog", "wealthlog",  #outcomes 
          "tertiary",
          "pgeducation", "pgeducationgroups", # PGI
          "pc1", "pc2", "pc3", "pc4", "pc5", "pc6", "pc7", "pc8", "pc9", "pc10", # principal components
          "male", "ses", # ascribed charactersitics
          "raedyrs_e") #raw years of education for RDD graphs


# Extract variables of interest
data<-data_complete[myvars]

# Filter to have complete observations
data <- data[complete.cases(data),] # final N should be 1922

# Model

m1<- feglm(tertiary~ treatment*pgeducation*ses +
             pc1 + pc2 +pc3 + pc4+ pc5 + pc6 + pc7 + pc8 + pc9 + pc10  +
             timetrend +
             timetrend*pc1 + timetrend*pc4 + timetrend*pc7 +
             timetrend*pc2 + timetrend*pc5 + timetrend*pc8 +
             timetrend*pc3 + timetrend*pc6 + timetrend*pc9 + timetrend*pc10 +
             timetrend*pgeducation +
             timetrend*treatment +
             timetrend*ses +
             male
           , data=data)
etable(m1, cluster="timetrend", digits = "r3",signif.code = signif_codes) 



#########################################################################
######## FIGURE S6: AVERAGE PGI BY COHORT, SELECTION INTO DYING  ########
#########################################################################

# Calculate mean and 95% confidence intervals by cohort
cohort_data <- data %>%
  group_by(rabyear) %>%
  summarise(mean_pgeducation = mean(pgeducation),
            ci_lower = tidy(t.test(pgeducation))$conf.low,
            ci_upper = tidy(t.test(pgeducation))$conf.high)

# View the resulting cohort_data data frame
cohort_data

# Plot
ggplot(data = cohort_data, aes(x = rabyear, y = mean_pgeducation)) +
  geom_line(color = "royalblue1") +  # Line plot
  geom_point(color = "royalblue1", size = 3) +  # Points for each cohort
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.1, color = "royalblue1", alpha=0.5) +  
  labs(title = " ",
       x = "Year of birth",
       y = "Average PGI") +
  scale_x_continuous(breaks = seq(min(data$rabyear), max(data$rabyear), by = 2)) + 
  theme_pilot(axis_title_size = 11,
              axis_text_size = 11,
              legend_text_size = 12,
              legend_title_size = 12,
              legend_position= "bottom") # Colors for the three groups
