
######################################################################################################################
##########  Table 1. Association between the outcomes of interest and the educational reform.  #################
######################################################################################################################

m1 <- feols(yoe~treatment + i(timetrend), data=data)
etable(m1, cluster="timetrend")
m2 <- feols(incomelog~treatment + i(timetrend), data=data)
etable(m2, cluster="timetrend")
m3 <- feols(wealthlog~treatment + i(timetrend), data=data)
etable(m3, cluster="timetrend")


#############################################################################################################################
###### Figure 3. Years of education by year of birth and terciles of the polygenic index of education (PGI). #############
#############################################################################################################################

# Relabelgroups of education
data$pgeducationgroups <- factor(data$pgeducationgroups,
                                levels = c("1", "2", "3"),
                                labels = c("Low", "Medium", "High"))

#Plot
plot1 <- ggplot(data, aes(x = rabyear, y = yoe, color = pgeducationgroups)) +
  geom_smooth(method = "lm", se = TRUE, alpha=0.15) + # Add a linear regression line
  labs(x = "Year of birth", y = "Years of education", title = " ", color= "Propensity towards education (PGI)") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(data$rabyear), max(data$rabyear), by = 2)) + 
  theme_pilot(axis_title_size = 11,
              axis_text_size = 11,
              legend_text_size = 12,
              legend_title_size = 12,
              legend_position= "bottom") +
  scale_color_discrete(labels = c("Low ability", "Medium ability", "High ability")) +
  scale_color_manual(values = c("coral1", "goldenrod1", "mediumpurple3")) # Colors for the three groups
plot1

##########################################################################
######## Figure S3. Income by year of birth and PGI. #####################
######################################################################### 

plot3 <- ggplot(data, aes(x = rabyear, y = incomelog, color = pgeducationgroups)) +
  geom_smooth(method = "lm", se = TRUE, alpha=0.15) + # Add a linear regression line
  labs(x = "Year of birth", y = "Income", title = " ", color= "Propensity towards education (PGI)") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(data$rabyear), max(data$rabyear), by = 2)) + 
  theme_pilot(axis_title_size = 11,
              axis_text_size = 11,
              legend_text_size = 12,
              legend_title_size = 12,
              legend_position= "bottom") +
  scale_color_discrete(labels = c("Low ability", "Medium ability", "High ability")) +
  scale_color_manual(values = c("coral1", "goldenrod1", "mediumpurple3")) # Colors for the three groups
plot3


######################################################################### 
############ Figure S4. Wealth by year of birth and PGI. ################ 
######################################################################### 

plot4 <- ggplot(data, aes(x = rabyear, y = wealthlog, color = pgeducationgroups)) +
  geom_smooth(method = "lm", se = TRUE, alpha=0.15) + # Add a linear regression line
  labs(x = "Year of birth", y = "Wealth", title = " ", color= "Propensity towards education (PGI)") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(data$rabyear), max(data$rabyear), by = 2)) + 
  theme_pilot(axis_title_size = 11,
              axis_text_size = 11,
              legend_text_size = 12,
              legend_title_size = 12,
              legend_position= "bottom") +
  scale_color_discrete(labels = c("Low ability", "Medium ability", "High ability")) +
  scale_color_manual(values = c("coral1", "goldenrod1", "mediumpurple3")) # Colors for the three groups
plot4

