

###############################################
######## BALANCE TESTS #####################
###############################################

# We run several models to check whether the treatment
# has an impact on several pre-treatment variables, so that we can see whether
# some groups were more likely to be treated


# First we select a sample with all the variables of interest for these balance tests 
# (mostly belonging to the life history module)
myvars<-c("idauniq", # id
          "rabyear", # year of birth
          "timetrend", #time trend
          "treatment", #treatment
          "rsmcold","male", "rsill", "rsargue", "rsfinan", "rhcia", "radiv", "rarel1" ) 

# Extract variables of interest from the original dataset
balance<-merged[myvars]

# Filter to have complete observations
balance<- balance[complete.cases(balance),] # final N should be 2132


# 1. Good mother-child relationship
table(balance$rsmcold)
balance$close<-balance$rsmcold
balance$close[balance$close<0]<-NA
balance$close[balance$close==1 | balance$close==2]<-0
balance$close[balance$close==3 | balance$close==4]<-1
table(balance$close)

m1 <- lm(close~ treatment + timetrend, balance)
summary(m1)

# 2. Gender
table(balance$male)

m2 <- lm(male~ treatment+ timetrend, balance)
summary(m2)

# 3. Childhood health
table(balance$rsill)
balance$ill<-balance$rsill
balance$ill[balance$ill<=-2]<-NA
balance$ill[balance$ill==2]<-0
table(balance$ill)

m3 <- lm(ill~ treatment+ timetrend, balance)
summary(m3)

# 4. Conflictive home environment
table(balance$rsargue)
balance$homenv<-balance$rsargue
balance$homenv[balance$homenv<=-2]<-NA
balance$homenv[balance$homenv==2]<-0
table(balance$homenv)

m4 <- lm(homenv~ treatment+ timetrend, balance)
summary(m4)

# 5. Financial hardship as kid
table(balance$rsfinan)
balance$hardship<-balance$rsfinan
balance$hardship[balance$hardship<=-2]<-NA
balance$hardship[balance$hardship==2]<-0
table(balance$hardship)

m5 <- lm(hardship~ treatment+ timetrend, balance)
summary(m5)

# 6. Self-reported health
table(balance$rhcia)
balance$health<-balance$rhcia
balance$health[balance$health<=-1]<-NA
balance$health[balance$health>=6]<-NA
table(balance$health)

m6 <- lm(health~ treatment+ timetrend, balance)
summary(m6)

# 7. Parents divorced
table(balance$radiv)
balance$parentsdivorce<-balance$radiv
balance$parentsdivorce[balance$parentsdivorce==1 | balance$parentsdivorce>=3]<-1
balance$parentsdivorce[balance$parentsdivorce==2]<-0
balance$parentsdivorce[balance$parentsdivorce<=-1]<-NA
table(balance$parentsdivorce)

m7 <- lm(parentsdivorce~ treatment+ timetrend, balance)
summary(m7)

# 8. Lived with mother

table(balance$rarel1)
balance$livedwmother<-balance$rarel1
balance$livedwmother[balance$livedwmother<=-1]<-NA
table(balance$livedwmother)

m8 <- lm(livedwmother~ treatment+ timetrend, balance)
summary(m8)

################################################################################
################ Figure 2. Balance checks ######################################
################################################################################

# We combine all the previous models in a graph.

# Function to extract treatment coefficient and its standard error
extract_treatment_coefficient <- function(model) {
  coef_names <- names(coef(model))
  treatment_coef_name <- grep("^treatment1$", coef_names)  # Find coefficient for "treatment1"
  if (length(treatment_coef_name) > 0) {
    coefficient <- coef(model)[treatment_coef_name]
    se <- sqrt(vcov(model)[treatment_coef_name, treatment_coef_name])
    return(data.frame(Estimate = coefficient, SE = se))
  } else {
    return(NULL)
  }
}

# Extract treatment coefficient and SE for each model
coefficients <- lapply(list(m1, m2, m3, m4, m5, m6, m7, m8), extract_treatment_coefficient)
balance <- do.call(rbind, coefficients)
balance$model <- paste0("Model ", seq_along(coefficients))

# We create a variable model containing all the relevant models
balance$model <- c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6", "Model 7", "Model 8")

# We define the desired names
balance$new_names <- c("Good mother-child relationship", "Gender", "Suffered an accident", "Conflictive home environment", 
                    "Financial hardship", "Self-reported health", "Parents divorced", "Lived with mother")

# We replace the names in 'models'
balance$model <- balance$new_names

# And finally, we create the plot using ggplot2
figure <- ggplot(balance, aes(x = model, y = Estimate, ymin = Estimate - 1.96 * SE, ymax = Estimate + 1.96 * SE)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(position = position_dodge(width = 0.5), width = 0.2) +
  labs(x = "Model", y = "Coefficient Estimate") +
  theme_minimal() +
  ggtitle("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = 0, 
             colour = "gray", lty = 2) + 
  theme_pilot(axis_title_size = 11,
              axis_text_size = 11,
              legend_text_size = 12,
              legend_title_size = 12,
              legend_position= "right") +
  coord_flip()+
  guides(color = guide_legend(reverse=TRUE)) +
  labs(x= "Variables",
       y = "Coefficients")

figure
