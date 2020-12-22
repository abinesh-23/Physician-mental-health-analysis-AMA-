################################################## codes for creating exploratory data analysis plots ##########################################

############################## Prepared by Abinesh Senthil Kumar for individual problem  under Dr. Mukherjee (Fall 2020) ############################

################################################################################################################################################

# run the data_wrangling code first before running this script

# Plots

# PTSD omitting NA values

ggplot(na.omit(renamed),  aes(x=na.omit(factor(renamed$ptsd_scaled, levels = c('Probable PTSD', 'No'))) )) +  
  geom_bar(aes(y = (..count..)), width = 0.6) + 
  geom_text(aes(y = (..count..),label = ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",
            hjust = -0.2, size = 9.8) +
  scale_y_continuous( expand = expansion(mult = c(0, .1))) + 
  scale_x_discrete(labels = c("Probable\nPTSD", 
                              "No")) +
  labs(title = "PTSD", y = "", x = "") + theme_classic()  +
  theme(plot.title = element_text(size = 26,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=29, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# PTSD for male and female

forptsd_m_f <- data.frame(renamed$ptsd_scaled, renamed$sex_5.2) %>% filter(renamed.sex_5.2 == "Male" |
                                                                         renamed.sex_5.2 == "Female") %>% na.omit() 

ggplot(na.omit(forptsd_m_f),  aes(fill = na.omit(forptsd_m_f$renamed.sex_5.2 %>% factor()) , x=na.omit(factor(forptsd_m_f$renamed.ptsd_scaled)) )) +
  geom_bar(aes(y = (..count..)), width = 0.6, position = "dodge") +
  geom_text(aes(y = (..count..),label = ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count", position = position_dodge(0.65),
            hjust = -0.2, size = 9.8) +
  scale_y_continuous( expand = expansion(mult = c(0, .1))) +
  labs(title = "PTSD for Male and Female", y = "", x = "") + theme_classic()  +
  theme(plot.title = element_text(size = 26,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=29, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.position = c(0.68, 0.3),
        legend.text = element_text(size = 18)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

################

# underlying health conditions omitting NA values

fig1 <- ggplot(na.omit(renamed), aes(x=na.omit(as.factor(renamed$underlyinh_health_condition_2.2)))) +
  geom_bar(aes(y = (..count..)), width = 0.6)+
  geom_text(aes(y = (..count..),label = ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..), accuracy = 0.01))),
            stat="count",hjust = 0.3,vjust = -1.6, size = 9.8) +
  labs(title = "Underlying health condition(s)", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,730)) +  
  theme_classic() +
  scale_x_discrete(labels = c("I don't\nknow /\nNot sure",
                              'No',
                              'Yes')) +
  theme(plot.title = element_text(size = 26,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=29, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# living changes concern barplot omitting NA values
ggplot(na.omit(renamed), aes(x=na.omit(as.factor(renamed$living_changes_cooncern_covid_2.5)))) +
  geom_bar(aes(y = (..count..)), width = 0.6)+
  geom_text(aes(y = (..count..),label = ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..), accuracy = 0.01))),
            stat="count", hjust = -0.1,vjust = -0.1, size = 10.8) +
  labs(title = "Living changes concerning of transmitting COVID-19 family / others", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,700)) +  
  theme_classic() +
  theme(plot.title = element_text(size = 26,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

fig2 <- ggplot(na.omit(renamed), aes(x=na.omit(as.factor(renamed$living_changes_cooncern_covid_2.5)))) +
  geom_bar(aes(y = (..count..)), width = 0.6)+
  geom_text(aes(y = (..count..),label = ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..), accuracy = 0.01))),
            stat="count", hjust = 0.5,vjust = -2.2, size = 9.8) +
  labs(title = "Concerns of transmitting COVID-19 family / others", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,700)) +  
  theme_classic() +
  theme(plot.title = element_text(size = 26,hjust = 1, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# working extra hours barplot omitting NA values


fig3 <- ggplot(na.omit(renamed), aes(x=na.omit(as.factor(renamed$work_extra_hours_2.7)))) +
  geom_bar(aes(y = (..count..)), width = 0.6)+
  geom_text(aes(y = (..count..),label = ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..), accuracy = 0.01))),
            stat="count", hjust = 0.5,vjust = -4.9, size = 9.8) +
  labs(title = "Extra work hours to care for COVID-19 patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic() +
  theme(plot.title = element_text(size = 26,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size = 30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size = 30)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# working outside normal responsibilities bar plot omitting NA values

fig4 <- ggplot(na.omit(renamed), aes(x=na.omit(as.factor(renamed$work_outside_normal_resp_2.8)))) +
  geom_bar(aes(y = (..count..)), width = 0.6)+
  geom_text(aes(y = (..count..),label = ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..), accuracy = 0.01))),
            stat="count", hjust = 0.5,vjust = -4.9, size = 9.8) +
  labs(title = "Working outside normal scope of clinical responsibilities ", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic() +
  theme(plot.title = element_text(size = 26,hjust = 0.8, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30)
  ) + coord_flip() + scale_fill_grey(start = 0, end = .9)


grid.arrange(fig4, fig3, ncol = 2, nrow = 1)

# arranging first four figures in a single graph

grid.arrange(fig1, fig2, fig4, fig3, ncol = 2, nrow = 2, widths = c(1.18,1))

##########################

# change in job difficulty due to COVID barplot omitting NA values

ggplot(na.omit(renamed), aes(x=na.omit(factor(renamed$job_difficult_dueto_COVID_2.9, levels = c('Extremely more','A lot more',
                                                                                                'Moderately more', 'A little bit more',
                                                                                                'None at all'))))) +
  geom_bar(aes(y = (..count..)), width = 0.6)+
  geom_text(aes(y = (..count..),label = ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..), accuracy = 0.01))),
            stat="count", hjust = 0.5,vjust = -2.5, size = 8.8) +
  labs(title = "Difficulty level of job due to COVID-19", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) + 
  scale_x_discrete(labels = c("Extremely\nmore",
                              'A lot\nmore',
                              'Moderately\nmore',
                              'A little bit\nmore',
                              'None')) +
  theme_classic() +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30)
  ) + coord_flip() + scale_fill_grey(start = 0, end = .9)


fig5 <- ggplot(na.omit(renamed), aes(x=na.omit(factor(renamed$job_difficult_dueto_COVID_2.9, levels = c('Extremely more','A lot more',
                                                                                                        'Moderately more', 'A little bit more',
                                                                                                        'None at all'
))))) +
  geom_bar(aes(y = (..count..)), width = 0.6)+
  geom_text(aes(y = (..count..),label = ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..), accuracy = 0.01))),
            stat="count", hjust = 0.5,vjust = -2.5, size = 8.8) +
  labs(title = "Difficulty level of job due to COVID-19", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) + 
  scale_x_discrete(labels = c("Extremely\nmore",
                              'A lot\nmore',
                              'Moderately\nmore',
                              'A little bit\nmore',
                              'None')) +
  theme_classic() +
  theme(plot.title = element_text(size = 30,hjust = 1.3, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30)
  ) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# creating a new dataframe by combining three columns (turnover intent questions) to create side by side barplots

forsidebyside <- data.frame(renamed$turnoverintent_switch_teams_2.15.1, renamed$turnoverintent_leave_current_2.15.2,
                            renamed$turnoverintent_leave_healthcare_2.15.3, 1:nrow(renamed))

colnames(forsidebyside) <- c('Switch_units/teams', 'Leave_your_current_employer', 'Leave_the_field_of_healthcare_entirely', 'ID')

melted <- melt(forsidebyside, id.vars = 'ID') %>% na.omit()

# side by side barplot of combined columns omitting NA values

fig6 <- ggplot((melted), aes(x= factor(value, levels = c( 'Definitely', 'Likely', 'Moderate', 'Slight', 'None')),
                             fill = factor(variable, levels = c('Leave_the_field_of_healthcare_entirely',
                                                                'Leave_your_current_employer',
                                                                'Switch_units/teams')))) +
  geom_bar(aes(y = (..count..)), position = 'dodge', width = 0.85)+ 
  geom_text(aes(y = (..count..),label = (scales::percent((..count..)/tapply(..count.., ..fill.. , sum)[..fill..], accuracy = 0.01))),
            stat="count", position=position_dodge(1), vjust= 0.3, hjust = -0.1, size = 8.8) +
  labs(title = "Turnover intent", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,750)) +  
  theme_classic() +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),
        legend.position = c(0.68, 0.3),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 18))  + 
  scale_fill_grey(start = .9, end = .1) + coord_flip() 

# arranging two plots in one graph (turnover_intent, difficluty of job)

grid.arrange(fig5,fig6, ncol = 2, nrow = 1, widths= c(1,1))

# burnout barplot omitting NA values

fig7 <- ggplot(na.omit(renamed), aes(x=na.omit(factor(renamed$level_of_burnout_3.3_scaled, levels = c("Severe burnout",
                                                                                                      'Moderately severe burnout',
                                                                                                      'Moderate burnout',
                                                                                                      'Mild burnout',
                                                                                                      'No burnout'))))) +
  geom_bar(aes(y = (..count..)), width = 0.6)+
  geom_text(aes(y = (..count..),label = ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..), accuracy = 0.01))),
            stat="count", hjust = 0.5,vjust = -2.2, size = 9.8)  +
  labs(title = "Burnout", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic() +
  scale_x_discrete(labels = c("Severe\nburnout",
                              'Moderately\nsevere\nburnout',
                              'Moderate\nburnout',
                              'Mild\nburnout',
                              'No\nburnout')) +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30)
  ) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# final depression validated scores barplot omitting NA values

fig8 <- ggplot(na.omit(renamed),  aes(x=na.omit(factor(renamed$depression_final_scaled, levels = c('Severe depression', 'Moderately Severe depression', 'Moderate depression',
                                                                                                   'Mild depression', 'None-Minimal depression'))))) +  
  geom_bar(aes(y = (..count..)), width = 0.6)+
  geom_text(aes(y = (..count..),label = ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))),
            stat="count", hjust = 0.25,vjust = -2.2, size = 9.8) +
  labs(title = "Depression", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,650)) +  
  theme_classic() + 
  scale_x_discrete(labels = c("Severe\ndepression", 
                              "Moderately\nSevere\ndepression",
                              'Moderate\ndepression',
                              'Mild\ndepression',
                              'None-\nMinimal\ndepression')) +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30)) + coord_flip() + scale_fill_grey(start = 0, end = .9)



# combining two plots in single graph (Burnout, depression)

grid.arrange(fig7, fig8, ncol = 2, nrow = 1, widths = c(1,1))

# primary work setting barplot omitting NA values

fig9 <- ggplot(na.omit(renamed), aes(x=na.omit(factor(renamed$Primary_work_setting_5.11, levels = c("Others (please specify):",
                                                                                                    'Outpatient center',
                                                                                                    'Two-physician practice',
                                                                                                    'Solo practice',
                                                                                                    'Hospital',
                                                                                                    'Group practice',
                                                                                                    'Academic medical center'))))) +
  geom_bar(aes(y = (..count..)), width = 0.6)+
  geom_text(aes(y = (..count..),label = ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..), accuracy = 0.01))),
            stat="count",hjust = 0.7,vjust = -1.6, size = 9.8)+
  labs(title = "Primary work setting", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic() +
  scale_x_discrete(labels = c("Others",
                              'Outpatient\ncenter',
                              'Two-physician\npractice',
                              'Solo practice',
                              'Hospital',
                              'Group practice',
                              'Academic\nmedical center')) +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30)
  ) + coord_flip() + scale_fill_grey(start = 0, end = .9)


####
# combining work setting within hospital columns to create side by side bar plots (combined because it was in on hot encoded format) 

forsidebysidev1 <- data.frame(renamed$Work_setting_emergency_dept_5.11.2.1, renamed$work_setting_general_floor_5.11.2.2,
                              renamed$work_setting_step_down_unit_5.11.2.3, renamed$work_setting_ICU_5.11.2.4,
                              renamed$work_setting_operating_room_5.11.2.5, renamed$work_setting_other_5.11.2.6,
                              1:nrow(renamed))

colnames(forsidebysidev1) <- c('Emergency_department', 'General_floor', 'Step_down_unit', 'Intensive_care_unit', 'Operating_room',
                               'Other', 'ID')

meltedv1 <- melt(forsidebysidev1, id.vars = 'ID') %>% na.omit()

# side by side bar plots with combined columns omitting NA values

fig10 <- ggplot((meltedv1), aes(x= factor(value, levels = c( 'Other (please mention):', 'Operating Room', 'Intensive Care Unit (any type)',
                                                             'Step Down Unit', 'General Floor', 'Emergency Department')))) +
  geom_bar(aes(y = (..count..)), position = 'dodge', width = 0.6)+
  geom_text(aes(y = (..count..),label = (scales::percent((..count..)/sum(..count..), accuracy = 0.01))),
            stat="count", position=position_dodge(1), hjust = 0.7,vjust = -1.9, size = 9.8) +
  labs(title = "Work setting within the hospital", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic() +
  scale_x_discrete(labels = c("Others",
                              'Operating Room',
                              'Intensive Care\nUnit (any type)',
                              'Step Down Unit',
                              'General Floor',
                              'Emergency\nDepartment')) +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),
        legend.position = c(0.6, 0.3),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 15))  + 
  scale_fill_grey(start = .9, end = .1) + coord_flip() 

# combining two plots in single graph (Primary work setting, work setting within hospital)

grid.arrange(fig9, fig10, ncol = 2, nrow = 1)

# sufficient resources 

ggplot(na.omit(renamed), aes(x=na.omit(factor(renamed$Sufficient_resources_2.10, levels = c("Always",
                                                                                            "Most of the time",
                                                                                            "About half the time",
                                                                                            "Sometimes",
                                                                                            "Never",
                                                                                            "Not applicable (N/A)"))))) +
  geom_bar(aes(y = (..count..)), width = 0.6)+
  geom_text(aes(y = (..count..),label = ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..), accuracy = 0.01))),
            stat="count",hjust = 0.7,vjust = -1.9, size = 9.8)+
  labs(title = "Sufficient resources availability", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic() +
  scale_x_discrete(labels = c("Always",
                              "Most of the time",
                              "About\nhalf the time",
                              "Sometimes",
                              "Never",
                              "Not\napplicable (N/A)")) +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30)
  ) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# part of decision making process

ggplot(na.omit(renamed), aes(x=na.omit(factor(renamed$Part_of_decision_making_2.11, levels = c("Always",
                                                                                            "Most of the time",
                                                                                            "About half the time",
                                                                                            "Sometimes",
                                                                                            "Never",
                                                                                            "Not applicable (N/A)"))))) +
  geom_bar(aes(y = (..count..)), width = 0.6)+
  geom_text(aes(y = (..count..),label = ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..), accuracy = 0.01))),
            stat="count",hjust = 0.3,vjust = -1.9, size = 9.8)+
  labs(title = "Part of decision making process", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic() +
  scale_x_discrete(labels = c("Always",
                              "Most of the time",
                              "About\nhalf the time",
                              "Sometimes",
                              "Never",
                              "Not\napplicable (N/A)")) +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30)
  ) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# felt mistreated

ggplot(na.omit(renamed), aes(x=na.omit(factor(renamed$discrim_mistreated_2.14, levels = c("Yes", "No"))))) +
  geom_bar(aes(y = (..count..)), width = 0.6)+
  geom_text(aes(y = (..count..),label = ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..), accuracy = 0.01))),
            stat="count",hjust = 0.4,vjust = -4.9, size = 9.8)+
  labs(title = "Felt mistreated/stigmatized", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic() +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30)
  ) + coord_flip() + scale_fill_grey(start = 0, end = .9)

####

################################# Resilience
# final resilience scores histogram showing mean

ggplot(na.omit(renamed), aes(x=na.omit(renamed$resilience_final_score))) +
    geom_histogram(binwidth=1, col = 'black', fill = 'grey') + geom_density(aes(y = 1.1*..count..),alpha=.2, fill="antiquewhite3") +
  geom_vline(aes(xintercept=mean(na.omit(renamed$resilience_final_score))),color="black", linetype="dashed", size=1) + 
  annotate(x=mean(na.omit(renamed$resilience_final_score)) + 2.9, y=+Inf,
           label = sprintf("Mean = %0.2f",mean(na.omit(renamed$resilience_final_score))),vjust=2,geom="text", size = 12, fontface = 'bold') + 
  labs(title = "Resilience scores histogram", y = "", x = '' )+ theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30)) + scale_fill_grey(start = 0, end = .9)  

# for combined plots
overallres <- ggplot(na.omit(renamed), aes(x=na.omit(renamed$resilience_final_score))) +
  geom_histogram(binwidth=1, col = 'black', fill = 'grey') + geom_density(aes(y = 1.1*..count..),alpha=.2, fill="antiquewhite3") +
  geom_vline(aes(xintercept=mean(na.omit(renamed$resilience_final_score))),color="black", linetype="dashed", size=1) + 
  annotate(x=mean(na.omit(renamed$resilience_final_score)) + 4.9, y=+Inf,
           label = sprintf("Mean = %0.2f",mean(na.omit(renamed$resilience_final_score))),vjust=2,geom="text", size = 10) + 
  labs(title = "Overall resilience scores", y = "", x = '' )+ theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  theme(plot.title = element_text(size = 25,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=20, hjust = 0.5),
        axis.text.x = element_text(size=20)) + scale_fill_grey(start = 0, end = .9)  


# resilience flexibility histogram
resflex <- ggplot(na.omit(renamed), aes(x=na.omit(renamed$resilience_hardiness_flexibility))) +
  geom_histogram(binwidth=1, col = 'black', fill = 'grey') + geom_density(aes(y = 1.1*..count..),alpha=.2, fill="antiquewhite3") +
  geom_vline(aes(xintercept=mean(na.omit(renamed$resilience_hardiness_flexibility))),color="black", linetype="dashed", size=1) + 
  annotate(x=mean(na.omit(renamed$resilience_hardiness_flexibility)) + 1, y=+Inf,
           label = sprintf("Mean = %0.2f",mean(na.omit(renamed$resilience_hardiness_flexibility))),vjust=2,geom="text", size = 10) + 
  labs(title = "Flexibility", y = "", x = '' )+ theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  theme(plot.title = element_text(size = 28,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=20, hjust = 0.5),
        axis.text.x = element_text(size=20)) + scale_fill_grey(start = 0, end = .9)  


# resilience sse histogram
ressse <- ggplot(na.omit(renamed), aes(x=na.omit(renamed$resilience_hardiness_SSE))) +
  geom_histogram(binwidth=1, col = 'black', fill = 'grey') + geom_density(aes(y = 1.1*..count..),alpha=.2, fill="antiquewhite3") +
  geom_vline(aes(xintercept=mean(na.omit(renamed$resilience_hardiness_SSE))),color="black", linetype="dashed", size=1) + 
  annotate(x=mean(na.omit(renamed$resilience_hardiness_SSE)) + 2.1, y=+Inf,
           label = sprintf("Mean = %0.2f",mean(na.omit(renamed$resilience_hardiness_SSE))),vjust=2,geom="text", size = 10) + 
  labs(title = "Sense of self-efficacy", y = "", x = '' )+ theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  theme(plot.title = element_text(size = 28,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=20, hjust = 0.5),
        axis.text.x = element_text(size=20)) + scale_fill_grey(start = 0, end = .9)  

# resilience ability to regulate emotion
resregem <- ggplot(na.omit(renamed), aes(x=na.omit(renamed$resilience_hardiness_regulate_em))) +
  geom_histogram(binwidth=1, col = 'black', fill = 'grey') + geom_density(aes(y = 1.1*..count..),alpha=.2, fill="antiquewhite3") +
  geom_vline(aes(xintercept=mean(na.omit(renamed$resilience_hardiness_regulate_em))),color="black", linetype="dashed", size=1) + 
  annotate(x=mean(na.omit(renamed$resilience_hardiness_regulate_em)) + 0.8, y=+Inf,
           label = sprintf("Mean = %0.2f",mean(na.omit(renamed$resilience_hardiness_regulate_em))),vjust=2,geom="text", size = 10) + 
  labs(title = "Ability to regulate emotion", y = "", x = '' )+ theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  theme(plot.title = element_text(size = 28,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=20, hjust = 0.5),
        axis.text.x = element_text(size=20)) + scale_fill_grey(start = 0, end = .9)  

# resilience optimism
resoptimism <- ggplot(na.omit(renamed), aes(x=na.omit(renamed$resilience_hardiness_optimism))) +
  geom_histogram(binwidth=1, col = 'black', fill = 'grey') + geom_density(aes(y = 1*..count..),alpha=.2, fill="antiquewhite3") +
  geom_vline(aes(xintercept=mean(na.omit(renamed$resilience_hardiness_optimism))),color="black", linetype="dashed", size=1) + 
  annotate(x=mean(na.omit(renamed$resilience_hardiness_optimism)) + 1.7, y=+Inf,
           label = sprintf("Mean = %0.2f",mean(na.omit(renamed$resilience_hardiness_optimism))),vjust=2,geom="text", size = 10) + 
  labs(title = "Optimism", y = "", x = '' )+ theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  theme(plot.title = element_text(size = 28,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=20, hjust = 0.5),
        axis.text.x = element_text(size=20)) + scale_fill_grey(start = 0, end = .9)  

# resilience congnitive focus
rescogfocus <- ggplot(na.omit(renamed), aes(x=na.omit(renamed$resilience_hardiness_cognitivefocus))) +
  geom_histogram(binwidth=1, col = 'black', fill = 'grey') + geom_density(aes(y = 0.5*..count..),alpha=.2, fill="antiquewhite3") +
  geom_vline(aes(xintercept=mean(na.omit(renamed$resilience_hardiness_cognitivefocus))),color="black", linetype="dashed", size=1) + 
  annotate(x=mean(na.omit(renamed$resilience_hardiness_cognitivefocus)) + 0.59, y=+Inf,
           label = sprintf("Mean = %0.2f",mean(na.omit(renamed$resilience_hardiness_cognitivefocus))),vjust=2,geom="text", size = 10) + 
  labs(title = "Cognitive focus", y = "", x = '' )+ theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  theme(plot.title = element_text(size = 28,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=20, hjust = 0.5),
        axis.text.x = element_text(size=20)) + scale_fill_grey(start = 0, end = .9)  


grid.arrange(resflex, ressse, resregem, resoptimism, rescogfocus, overallres,
             top = textGrob("Resilience - Hardiness", gp=gpar(fontsize=35, fontface = "bold")))
###############################################
# Age histogram

ggplot(na.omit(renamed), aes(x=na.omit(renamed$Age))) +
  geom_histogram(binwidth=1, col = 'black', fill = 'grey') + geom_density(aes(y = 1.1*..count..),alpha=.2, fill="antiquewhite3") +
  geom_vline(aes(xintercept=mean(na.omit(renamed$Age))),color="black", linetype="dashed", size=1) + 
  annotate(x=mean(na.omit(renamed$Age)) + 5, y=+Inf,
           label = sprintf("Mean = %0.2f",mean(na.omit(renamed$Age))),vjust=2,geom="text", size = 12, fontface = 'bold') + 
  labs(title = "Age of responders distribution", y = "", x = '' )+ theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  theme(plot.title = element_text(size = 35,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30)) + scale_fill_grey(start = 0, end = .9)  

# Years practicing histogram

ggplot(na.omit(renamed), aes(x=na.omit(renamed$Years_practicing_5.8))) +
  geom_histogram(binwidth=1, col = 'black', fill = 'grey') + geom_density(aes(y = 1.1*..count..),alpha=.2, fill="antiquewhite3") +
  geom_vline(aes(xintercept=mean(na.omit(renamed$Years_practicing_5.8))),color="black", linetype="dashed", size=1) + 
  annotate(x=mean(na.omit(renamed$Years_practicing_5.8)) + 5.5, y=+Inf,
           label = sprintf("Mean = %0.2f",mean(na.omit(renamed$Years_practicing_5.8))),vjust=2,geom="text", size = 12, fontface = 'bold') + 
  labs(title = "Years of Experience", y = "", x = '' )+ theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  theme(plot.title = element_text(size = 35,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30)) + scale_fill_grey(start = 0, end = .9)  


###################

# Demographics table

# sorting factors based on their percentage of values (decreasing) 

renamed <- within(renamed,sex_5.2 <- factor(sex_5.2, levels=names(sort(table(sex_5.2),decreasing=T))))
renamed <- within(renamed,hispanic_or_latino_5.3 <- factor(hispanic_or_latino_5.3, levels=names(sort(table(hispanic_or_latino_5.3),decreasing=T))))
renamed <- within(renamed,Immigrant_5.5 <- factor(Immigrant_5.5, levels=names(sort(table(Immigrant_5.5),decreasing=T))))
renamed <- within(renamed,Relationship_5.6 <- factor(Relationship_5.6, levels=names(sort(table(Relationship_5.6),decreasing=T))))
renamed <- within(renamed,Primary_work_setting_5.11 <- factor(Primary_work_setting_5.11, levels=names(sort(table(Primary_work_setting_5.11),decreasing=T))))
renamed <- within(renamed,Working_status_5.12 <- factor(Working_status_5.12, levels=names(sort(table(Working_status_5.12),decreasing=T))))

# assigning labels for the variables in table

label(renamed$Age) <- "Age"
units(renamed$Age) <- "years"
label(renamed$sex_5.2) <- "Sex"
label(renamed$hispanic_or_latino_5.3) <- "Ethnicity - Hispanic/Latino"
label(renamed$Immigrant_5.5) <- "Immigrant"
label(renamed$Relationship_5.6) <- "Relationship status"
label(renamed$Years_practicing_5.8) <- "Years of Experience"
label(renamed$Primary_work_setting_5.11) <- "Primary work setting"
label(renamed$Working_status_5.12) <- "Working status"

table1::table1(~ Age + sex_5.2 + hispanic_or_latino_5.3 + Immigrant_5.5 + Relationship_5.6 +
                 Years_practicing_5.8 + Primary_work_setting_5.11 + Working_status_5.12, data = (renamed))

# race distribution 

forracedist <- within(forracedist,race.value <- factor(race.value, levels=names(sort(table(race.value),decreasing=T))))

label(forracedist$race.value) <- "Race"
table1(~ forracedist$race.value)

# speciality distribution

forspecialitydist <- within(forspecialitydist,speciality.value <- factor(speciality.value, levels=names(sort(table(speciality.value),decreasing=T))))

label(forspecialitydist$speciality.value) <- "Medical Speciality"
table1::table1(~ forspecialitydist$speciality.value)

# hospital type distribution

forhospitaltypedist <- within(forhospitaltypedist,hospitaltype.value <- factor(hospitaltype.value, levels=names(sort(table(hospitaltype.value),decreasing=T))))

label(forhospitaltypedist$hospitaltype.value) <- "Hospital type"
table1(~ forhospitaltypedist$hospitaltype.value)

# work setting within hospital distribution

forhospwithindist <- within(forhospwithindist,worksettingwithinhospital.value <- factor(worksettingwithinhospital.value,
                                                                                        levels=names(sort(table(worksettingwithinhospital.value),decreasing=T))))

label(forhospwithindist$worksettingwithinhospital.value) <- "Work setting within hospital"
table1(~ forhospwithindist$worksettingwithinhospital.value)

############ Percent of population (POP)

# pop black

popblack <- ggplot(na.omit(renamed), aes(x=na.omit(renamed$POP_Black_5.13.1))) +
  geom_histogram(binwidth=1, col = 'black', fill = 'grey') + geom_density(aes(y = 1.1*..count..),alpha=.2, fill="antiquewhite3") +
  geom_vline(aes(xintercept=mean(na.omit(renamed$POP_Black_5.13.1))),color="black", linetype="dashed", size=1) + 
  annotate(x=mean(na.omit(renamed$POP_Black_5.13.1)) + 15.5, y=+Inf,
           label = sprintf("Mean = %0.2f",mean(na.omit(renamed$POP_Black_5.13.1))),vjust=2,geom="text", size = 10) + 
  labs(title = "Black", y = "", x = '' )+ theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  theme(plot.title = element_text(size = 28,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=20, hjust = 0.5),
        axis.text.x = element_text(size=20)) + scale_fill_grey(start = 0, end = .9)  

# pop non-hispanic/white

popnhwhite <- ggplot(na.omit(renamed), aes(x=na.omit(renamed$POP_Non_Hisp_white_5.13.2))) +
  geom_histogram(binwidth=1, col = 'black', fill = 'grey') + geom_density(aes(y = 1.1*..count..),alpha=.2, fill="antiquewhite3") +
  geom_vline(aes(xintercept=mean(na.omit(renamed$POP_Non_Hisp_white_5.13.2))),color="black", linetype="dashed", size=1) + 
  annotate(x=mean(na.omit(renamed$POP_Non_Hisp_white_5.13.2)) + 15.5, y=+Inf,
           label = sprintf("Mean = %0.2f",mean(na.omit(renamed$POP_Non_Hisp_white_5.13.2))),vjust=2,geom="text", size = 10) + 
  labs(title = "non-hispanic/white", y = "", x = '' )+ theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  theme(plot.title = element_text(size = 28,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=20, hjust = 0.5),
        axis.text.x = element_text(size=20)) + scale_fill_grey(start = 0, end = .9)  

# pop hispanic/latino

pophisplat <- ggplot(na.omit(renamed), aes(x=na.omit(renamed$POP_Hispanic_latino_5.13.3))) +
  geom_histogram(binwidth=1, col = 'black', fill = 'grey') + geom_density(aes(y = 1.1*..count..),alpha=.2, fill="antiquewhite3") +
  geom_vline(aes(xintercept=mean(na.omit(renamed$POP_Hispanic_latino_5.13.3))),color="black", linetype="dashed", size=1) + 
  annotate(x=mean(na.omit(renamed$POP_Hispanic_latino_5.13.3)) + 15.5, y=+Inf,
           label = sprintf("Mean = %0.2f",mean(na.omit(renamed$POP_Hispanic_latino_5.13.3))),vjust=2,geom="text", size = 10,) + 
  labs(title = "Hispanic/latino", y = "", x = '' )+ theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  theme(plot.title = element_text(size = 28,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=20, hjust = 0.5),
        axis.text.x = element_text(size=20)) + scale_fill_grey(start = 0, end = .9)  

# pop Asian

popasian <- ggplot(na.omit(renamed), aes(x=na.omit(renamed$POP_Asian_5.13.4))) +
  geom_histogram(binwidth=1, col = 'black', fill = 'grey') + geom_density(aes(y = 1.1*..count..),alpha=.2, fill="antiquewhite3") +
  geom_vline(aes(xintercept=mean(na.omit(renamed$POP_Asian_5.13.4))),color="black", linetype="dashed", size=1) + 
  annotate(x=mean(na.omit(renamed$POP_Asian_5.13.4)) + 15.5, y=+Inf,
           label = sprintf("Mean = %0.2f",mean(na.omit(renamed$POP_Asian_5.13.4))),vjust=2,geom="text", size = 10) + 
  labs(title = "Asian", y = "", x = '' )+ theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  theme(plot.title = element_text(size = 28,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=20, hjust = 0.5),
        axis.text.x = element_text(size=20)) + scale_fill_grey(start = 0, end = .9)  

# pop american indian/alaska native

popamerindalasnat <- ggplot(na.omit(renamed), aes(x=na.omit(renamed$POP_American_indian_5.13.5))) +
  geom_histogram(binwidth=1, col = 'black', fill = 'grey') + geom_density(aes(y = 1.1*..count..),alpha=.2, fill="antiquewhite3") +
  geom_vline(aes(xintercept=mean(na.omit(renamed$POP_American_indian_5.13.5))),color="black", linetype="dashed", size=1) + 
  annotate(x=mean(na.omit(renamed$POP_American_indian_5.13.5)) + 15.5, y=+Inf,
           label = sprintf("Mean = %0.2f",mean(na.omit(renamed$POP_American_indian_5.13.5))),vjust=2,geom="text", size = 10) + 
  labs(title = "American Indian/Alaska native", y = "", x = '' )+ theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  theme(plot.title = element_text(size = 28,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=20, hjust = 0.5),
        axis.text.x = element_text(size=20)) + scale_fill_grey(start = 0, end = .9)  

# pop native hawaiian or pacific islander

popnativehaw <- ggplot(na.omit(renamed), aes(x=na.omit(renamed$POP_Native_hawaiian_5.13.6))) +
  geom_histogram(binwidth=1, col = 'black', fill = 'grey') + geom_density(aes(y = 1.1*..count..),alpha=.2, fill="antiquewhite3") +
  geom_vline(aes(xintercept=mean(na.omit(renamed$POP_Native_hawaiian_5.13.6))),color="black", linetype="dashed", size=1) + 
  annotate(x=mean(na.omit(renamed$POP_Native_hawaiian_5.13.6)) + 15.5, y=+Inf,
           label = sprintf("Mean = %0.2f",mean(na.omit(renamed$POP_Native_hawaiian_5.13.6))),vjust=2,geom="text", size = 10) + 
  labs(title = "Native Hawaiian or Pacific Islander", y = "", x = '' )+ theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,60)) +
  theme(plot.title = element_text(size = 28,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=20, hjust = 0.5),
        axis.text.x = element_text(size=20)) + scale_fill_grey(start = 0, end = .9)  

# combining pop plots into one

grid.arrange(popblack, popnhwhite, pophisplat, popasian, popamerindalasnat, popnativehaw,
             top = textGrob("Percent of Population", gp=gpar(fontsize=35, fontface = "bold")))


# perceived organizational support 8 question survey final scores

ggplot(na.omit(orgsupport8q), aes(x=na.omit(orgsupport8q$finalscoresorg))) +
  geom_histogram(binwidth=1, col = 'black', fill = 'grey') + geom_density(aes(y = 1.1*..count..),alpha=.2, fill="antiquewhite3") +
  geom_vline(aes(xintercept=mean(na.omit(orgsupport8q$finalscoresorg))),color="black", linetype="dashed", size=1) + 
  annotate(x=mean(na.omit(orgsupport8q$finalscoresorg)) + 5, y=+Inf,
           label = sprintf("Mean = %0.2f",mean(na.omit(orgsupport8q$finalscoresorg))),vjust=2,geom="text", size = 12, fontface = 'bold') + 
  labs(title = "Perceived organizational support final scores ", y = "", x = '' )+ theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  theme(plot.title = element_text(size = 35,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30)) + scale_fill_grey(start = 0, end = .9)  

# employer actions

# q1

ggplot(na.omit(renamed), aes(x=na.omit(factor(renamed$org_supp_even_wrkld_4.1.1)))) +
  geom_bar(aes(y = (..count..)), width = 0.6)+
  geom_text(aes(y = (..count..),label = ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..), accuracy = 0.01))),
            stat="count",hjust = 0.4,vjust = -2.9, size = 9.8)+
  labs(title = "Employer - Evenly distributed workload", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  scale_x_discrete(labels = c("No",
                              'Not\nApplicable (N/A)',
                              'Not sure',
                              'Yes')) +
  theme_classic() +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30)
  ) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# q2


ggplot(na.omit(renamed), aes(x=na.omit(factor(renamed$org_supp_psych_hotline_4.1.2)))) +
  geom_bar(aes(y = (..count..)), width = 0.6)+
  geom_text(aes(y = (..count..),label = ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..), accuracy = 0.01))),
            stat="count",hjust = 0.4,vjust = -2.9, size = 9.8)+
  labs(title = "Employer - Arrange for psychological hotline", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)))  +   
  scale_x_discrete(labels = c("No",
                              'Not\nApplicable (N/A)',
                              'Not sure',
                              'Yes')) +
  
  
  theme_classic() +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30)
  ) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# q3

ggplot(na.omit(renamed), aes(x=na.omit(factor(renamed$org_supp_training_4.1.3)))) +
  geom_bar(aes(y = (..count..)), width = 0.6)+
  geom_text(aes(y = (..count..),label = ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..), accuracy = 0.01))),
            stat="count",hjust = 0.4,vjust = -2.9, size = 9.8)+
  labs(title = "Employer - Offer training programs", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  
  scale_x_discrete(labels = c("No",
                              'Not\nApplicable (N/A)',
                              'Not sure',
                              'Yes')) +
  
  theme_classic() +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30)
  ) + coord_flip() + scale_fill_grey(start = 0, end = .9)


################################################################################################################
############################################################################################################################################
###### Plots requested by prasangsha for presentation ########################
############################################################################################################################################
################################################################################################################

# burnout barplot with difficulty lot more and extremenly more omitting NA values

filteredjdlmem <- filter(renamed, renamed$job_difficult_dueto_COVID_2.9 == "A lot more" |
                           renamed$job_difficult_dueto_COVID_2.9 == "Extremely more") 
  

filteredjdburnout <- ggplot(na.omit(filteredjdlmem), aes(x=na.omit(factor(filteredjdlmem$level_of_burnout_3.3_scaled, levels = c("Severe burnout",
                                                                                                      'Moderately severe burnout',
                                                                                                      'Moderate burnout',
                                                                                                      'Mild burnout',
                                                                                                      'No burnout'))))) +
  geom_bar(aes(y = (..count..)), width = 0.6)+
  geom_text(aes(y = (..count..),label = ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..), accuracy = 0.01))),
            stat="count", hjust = 0.5,vjust = -2.2, size = 9.8)  +
  labs(title = "Burnout", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic() +
  scale_x_discrete(labels = c("Severe\nburnout",
                              'Moderately\nsevere\nburnout',
                              'Moderate\nburnout',
                              'Mild\nburnout',
                              'No\nburnout')) +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30)
  ) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# final depression validated scores barplot with difficulty lot more and extremely more omitting NA values

filteredjddepression <- ggplot(na.omit(filteredjdlmem),  aes(x=na.omit(factor(filteredjdlmem$depression_final_scaled, levels = c('Severe depression', 'Moderately Severe depression', 'Moderate depression',
                                                                                                   'Mild depression', 'None-Minimal depression'))))) +  
  geom_bar(aes(y = (..count..)), width = 0.6)+
  geom_text(aes(y = (..count..),label = ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))),
            stat="count", hjust = 0.25,vjust = -2.2, size = 9.8) +
  labs(title = "Depression", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,185)) +  
  theme_classic() + 
  scale_x_discrete(labels = c("Severe\ndepression", 
                              "Moderately\nSevere\ndepression",
                              'Moderate\ndepression',
                              'Mild\ndepression',
                              'None-\nMinimal\ndepression')) +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30)) + coord_flip() + scale_fill_grey(start = 0, end = .9)



# combining two plots in single graph (Burnout, depression)

grid.arrange(filteredjdburnout, filteredjddepression, ncol = 2, nrow = 1, widths = c(1,1))

##############################

filteredworkingoutnm <- filter(renamed, work_outside_normal_resp_2.8 == "Yes") 

# burnout filtered with working outside yes

filteredworkingoutnmburnout <- ggplot(na.omit(filteredworkingoutnm), aes(x=na.omit(factor(filteredworkingoutnm$level_of_burnout_3.3_scaled, levels = c("Severe burnout",
                                                                                                                                 'Moderately severe burnout',
                                                                                                                                 'Moderate burnout',
                                                                                                                                 'Mild burnout',
                                                                                                                                 'No burnout'))))) +
  geom_bar(aes(y = (..count..)), width = 0.6)+
  geom_text(aes(y = (..count..),label = ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..), accuracy = 0.01))),
            stat="count", hjust = 0.5,vjust = -2.2, size = 9.8)  +
  labs(title = "Burnout", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic() +
  scale_x_discrete(labels = c("Severe\nburnout",
                              'Moderately\nsevere\nburnout',
                              'Moderate\nburnout',
                              'Mild\nburnout',
                              'No\nburnout')) +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30)
  ) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# final depression validated scores barplot with working outside normal yes omitting NA values

filteredworkingoutnmdepression <- ggplot(na.omit(filteredworkingoutnm),  aes(x=na.omit(factor(filteredworkingoutnm$depression_final_scaled, levels = c('Severe depression', 'Moderately Severe depression', 'Moderate depression',
                                                                                                                                 'Mild depression', 'None-Minimal depression'))))) +  
  geom_bar(aes(y = (..count..)), width = 0.6)+
  geom_text(aes(y = (..count..),label = ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))),
            stat="count", hjust = 0.25,vjust = -2.2, size = 9.8) +
  labs(title = "Depression", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,140)) +  
  theme_classic() + 
  scale_x_discrete(labels = c("Severe\ndepression", 
                              "Moderately\nSevere\ndepression",
                              'Moderate\ndepression',
                              'Mild\ndepression',
                              'None-\nMinimal\ndepression')) +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30)) + coord_flip() + scale_fill_grey(start = 0, end = .9)



# combining two plots in single graph (Burnout, depression)

grid.arrange(filteredworkingoutnmburnout, filteredworkingoutnmdepression, ncol = 2, nrow = 1, widths = c(1,1))

##########################


filteredlivingchyes <- filter(renamed, renamed$living_changes_cooncern_covid_2.5 == "Yes") 

# burnout filtered with living changes yes

filteredlivingchyesburnout <- ggplot(na.omit(filteredlivingchyes), aes(x=na.omit(factor(filteredlivingchyes$level_of_burnout_3.3_scaled, levels = c("Severe burnout",
                                                                                                                                                       'Moderately severe burnout',
                                                                                                                                                       'Moderate burnout',
                                                                                                                                                       'Mild burnout',
                                                                                                                                                       'No burnout'))))) +
  geom_bar(aes(y = (..count..)), width = 0.6)+
  geom_text(aes(y = (..count..),label = ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..), accuracy = 0.01))),
            stat="count", hjust = 0.5,vjust = -2.2, size = 9.8)  +
  labs(title = "Burnout", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic() +
  scale_x_discrete(labels = c("Severe\nburnout",
                              'Moderately\nsevere\nburnout',
                              'Moderate\nburnout',
                              'Mild\nburnout',
                              'No\nburnout')) +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30)
  ) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# final depression validated scores barplot with living changes yes omitting NA values

filteredlivingvhyesdepression <- ggplot(na.omit(filteredlivingchyes),  aes(x=na.omit(factor(filteredlivingchyes$depression_final_scaled, levels = c('Severe depression', 'Moderately Severe depression', 'Moderate depression',
                                                                                                                                                       'Mild depression', 'None-Minimal depression'))))) +  
  geom_bar(aes(y = (..count..)), width = 0.6)+
  geom_text(aes(y = (..count..),label = ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))),
            stat="count", hjust = 0.25,vjust = -2.2, size = 9.8) +
  labs(title = "Depression", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,220)) +  
  theme_classic() + 
  scale_x_discrete(labels = c("Severe\ndepression", 
                              "Moderately\nSevere\ndepression",
                              'Moderate\ndepression',
                              'Mild\ndepression',
                              'None-\nMinimal\ndepression')) +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30)) + coord_flip() + scale_fill_grey(start = 0, end = .9)



# combining two plots in single graph (Burnout, depression)

grid.arrange(filteredlivingchyesburnout, filteredlivingvhyesdepression, ncol = 2, nrow = 1, widths = c(1,1))

#########################

filteredsuffres <- filter(renamed, Sufficient_resources_2.10 == "Never" | Sufficient_resources_2.10 == "Sometimes" |
                                Sufficient_resources_2.10 == 'About half the time') 

# burnout filtered with sufficient resource avail

filteredsuffchburnout <- ggplot(na.omit(filteredsuffres), aes(x=na.omit(factor(filteredsuffres$level_of_burnout_3.3_scaled, levels = c("Severe burnout",
                                                                                                                                                    'Moderately severe burnout',
                                                                                                                                                    'Moderate burnout',
                                                                                                                                                    'Mild burnout',
                                                                                                                                                    'No burnout'))))) +
  geom_bar(aes(y = (..count..)), width = 0.6)+
  geom_text(aes(y = (..count..),label = ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..), accuracy = 0.01))),
            stat="count", hjust = 0.5,vjust = -2.2, size = 9.8)  +
  labs(title = "Burnout", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic() +
  scale_x_discrete(labels = c("Severe\nburnout",
                              'Moderately\nsevere\nburnout',
                              'Moderate\nburnout',
                              'Mild\nburnout',
                              'No\nburnout')) +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30)
  ) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# final depression validated scores barplot with suff res avail omitting NA values

filteredsuffresdepression <- ggplot(na.omit(filteredsuffres),  aes(x=na.omit(factor(filteredsuffres$depression_final_scaled, levels = c('Severe depression', 'Moderately Severe depression', 'Moderate depression',
                                                                                                                                          'Mild depression', 'None-Minimal depression'))))) +  
  geom_bar(aes(y = (..count..)), width = 0.6)+
  geom_text(aes(y = (..count..),label = ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))),
            stat="count", hjust = 0.25,vjust = -2.2, size = 9.8) +
  labs(title = "Depression", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,140)) +  
  theme_classic() + 
  scale_x_discrete(labels = c("Severe\ndepression", 
                              "Moderately\nSevere\ndepression",
                              'Moderate\ndepression',
                              'Mild\ndepression',
                              'None-\nMinimal\ndepression')) +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30)) + coord_flip() + scale_fill_grey(start = 0, end = .9)



# combining two plots in single graph (Burnout, depression)

grid.arrange(filteredsuffchburnout, filteredsuffresdepression, ncol = 2, nrow = 1, widths = c(1,1))

###########################

# filtering only male and female 

filtermfdepburnmf <- data.frame(renamed$level_of_burnout_3.3_scaled, renamed$sex_5.2, renamed$depression_final_scaled) %>% na.omit()
colnames(filtermfdepburnmf) <- c('level_of_burnout_3.3_scaled', 'sex_5.2', 'depression_final_scaled')
filtermfdepburnmf <- filter(filtermfdepburnmf, sex_5.2 == "Male" | sex_5.2 == "Female")


# burnout barplot omitting NA values for male and female

burnoutmf <- ggplot(na.omit(filtermfdepburnmf), aes(fill = filtermfdepburnmf$sex_5.2 ,x=na.omit(factor(filtermfdepburnmf$level_of_burnout_3.3_scaled, levels = c("Severe burnout",
                                                                                                                        'Moderately severe burnout',
                                                                                                                        'Moderate burnout',
                                                                                                                        'Mild burnout',
                                                                                                                        'No burnout'))))) +
                    geom_bar(aes(y = (..count..)), width = 0.6, position = "dodge")+
                    geom_text(aes(y = (..count..),label = ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..), accuracy = 0.01))),
                              stat="count", position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8)  +
                    labs(title = "Burnout for Male and Female", y = "", x = '' ) + 
                    scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
                    theme_classic() +
                    scale_x_discrete(labels = c("Severe\nburnout",
                                                'Moderately\nsevere\nburnout',
                                                'Moderate\nburnout',
                                                'Mild\nburnout',
                                                'No\nburnout')) +
                    theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
                          axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
                          axis.text.x = element_text(size=30),legend.position = c(0.58, 0.25),
                          legend.title = element_text(colour = 'white', size = 0.01),
                          legend.text = element_text(size = 28)
                    ) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# final depression validated scores barplot omitting NA values for male and female only

depressionmf <- ggplot(na.omit(filtermfdepburnmf),  aes(fill = na.omit(filtermfdepburnmf$sex_5.2) ,x=na.omit(factor(filtermfdepburnmf$depression_final_scaled, levels = c('Severe depression', 'Moderately Severe depression', 'Moderate depression',
                                                                                                                       'Mild depression', 'None-Minimal depression'))))) +  
                      geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
                      geom_text(aes(y = (..count..),label = ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))),
                                stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
                      labs(title = "Depression for Male and Female", y = "", x = '' ) + 
                      scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,280)) +  
                      theme_classic() + 
                      scale_x_discrete(labels = c("Severe\ndepression", 
                                                  "Moderately\nSevere\ndepression",
                                                  'Moderate\ndepression',
                                                  'Mild\ndepression',
                                                  'None-\nMinimal\ndepression')) +
                      theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
                            axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
                            axis.text.x = element_text(size=30),legend.position = c(0.58, 0.25),
                            legend.title = element_text(colour = 'white', size = 0.01),
                            legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)
                    
                    
# 2nd filter request from prasangsha

# filtering for people who didnt say never, sometimes and about half the time and finding depression distribution


filteredsuffresdidnt <- filter(renamed, Sufficient_resources_2.10 != "Never" & Sufficient_resources_2.10 != "Sometimes" &
                                  Sufficient_resources_2.10 != 'About half the time') 

ggplot(na.omit(filteredsuffresdidnt),  aes(x=na.omit(factor(filteredsuffresdidnt$depression_final_scaled, levels = c('Severe depression', 'Moderately Severe depression', 'Moderate depression',
                                                                                                           'Mild depression', 'None-Minimal depression'))))) +  
  geom_bar(aes(y = (..count..)), width = 0.6)+
  geom_text(aes(y = (..count..),label = ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))),
            stat="count", hjust = 0.25,vjust = -2.2, size = 9.8) +
  labs(title = "Depression", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,440)) +  
  theme_classic() + 
  scale_x_discrete(labels = c("Severe\ndepression", 
                              "Moderately\nSevere\ndepression",
                              'Moderate\ndepression',
                              'Mild\ndepression',
                              'None-\nMinimal\ndepression')) +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# filtering for people who didnt say lot more and extremely more for job difficulty questions and finding depression distribution

filtereddiffjobdidnt <- filter(renamed, job_difficult_dueto_COVID_2.9 != "A lot more" & job_difficult_dueto_COVID_2.9 != "Extremely more")

ggplot(na.omit(filtereddiffjobdidnt),  aes(x=na.omit(factor(filtereddiffjobdidnt$depression_final_scaled, levels = c('Severe depression', 'Moderately Severe depression', 'Moderate depression',
                                                                                                                     'Mild depression', 'None-Minimal depression'))))) +  
  geom_bar(aes(y = (..count..)), width = 0.6)+
  geom_text(aes(y = (..count..),label = ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))),
            stat="count", hjust = 0.25,vjust = -2.2, size = 9.8) +
  labs(title = "Depression", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,400)) +  
  theme_classic() + 
  scale_x_discrete(labels = c("Severe\ndepression", 
                              "Moderately\nSevere\ndepression",
                              'Moderate\ndepression',
                              'Mild\ndepression',
                              'None-\nMinimal\ndepression')) +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

##########################################################################################################################################
######################################## End of block prasangsha presentation plots ########################################
##########################################################################################################################################
##########

# plotting states from zip codes on US map

table(renamed$state_name_from_zip) %>% sort()
table(renamed$State_of_practice_5.9) %>% sort()

# states given by responders

states <- table(renamed$State_of_practice_5.9) %>% data.frame()
colnames(states) <- c("state", "Freq")

plot_usmap(regions = "states", data = states, values = "Freq", color = "red", labels = T) + 
  scale_fill_continuous( low = "white", high = "blue", name = "Number of responders", label = scales :: comma) +
  theme(legend.position = 'top')

# states got from zip codes given by responsers

states <- table(renamed$state_name_from_zip) %>% data.frame()
colnames(states) <- c("state", "Freq")

plot_usmap(regions = "states", data = states, values = "Freq", color = "red", labels = T) + 
  scale_fill_continuous( low = "white", high = "blue", name = "Number of responders") +
  theme(legend.position = 'top')


table(renamed$state_name_from_zip)
table(renamed$State_of_practice_5.9)

###########

# only male and female distribution

onlymf <- (renamed %>% filter(sex_5.2 == "Male" | sex_5.2 == "Female"))$sex_5.2 %>% data.frame() %>% na.omit()
colnames(onlymf) <- "sex"

ggplot(na.omit(onlymf), aes(x=na.omit(factor(sex)))) +
  geom_bar(aes(y = (..count..)), width = 0.6)+
  geom_text(aes(y = (..count..),label = ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..), accuracy = 0.01))),
            stat="count",hjust = 0.4,vjust = -4.9, size = 9.8)+
  labs(title = "Gender", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic() +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30)
  ) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# percent of population radar chart

colors_border=c( rgb(0.1,0.1,0.1,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.5,0.5,0.5,0.5), rgb(0.9,0.9,0.9,0.9) , rgb(0.7,0.5,0.1,0.4) )

radarchart(pop, axistype = 1, pcol = colors_border, pfcol = colors_in, plwd=4 , plty=1,
           cglcol="grey", cglty=1, axislabcol="black", caxislabels=seq(0,100,25), cglwd=0.9,vlcex=1.5)

# barplot of frequency of directly worked with covid patients

ggplot(na.omit(renamed), aes(x=na.omit(factor(renamed$Direcly_worked_COVID_patients_2.1)))) +
  geom_bar(aes(y = (..count..)), width = 0.6)+
  geom_text(aes(y = (..count..),label = ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..), accuracy = 0.01))),
            stat="count",hjust = 0.4,vjust = -4.9, size = 9.8)+
  labs(title = "Worked directly with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic() +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30)
  ) + coord_flip() + scale_fill_grey(start = 0, end = .9)

table(renamed$Direcly_worked_COVID_patients_2.1)

############# burnout, depression and PTSD for working woth covid patients


filtermfdepburnworkcovid <- data.frame(renamed$level_of_burnout_3.3_scaled, renamed$Direcly_worked_COVID_patients_2.1, 
                                       renamed$depression_final_scaled, renamed$ptsd_scaled) %>% na.omit()
colnames(filtermfdepburnworkcovid) <- c('level_of_burnout_3.3_scaled', 'worked_with_COVID_pat', 'depression_final_scaled', 'ptsd_scaled')

# burnout plot

ggplot(na.omit(filtermfdepburnworkcovid), aes(fill = factor(filtermfdepburnworkcovid$worked_with_COVID_pat) ,
                                              x=na.omit(factor(filtermfdepburnworkcovid$level_of_burnout_3.3_scaled,
                                                               levels = c("Severe burnout",
                                                                           'Moderately severe burnout',
                                                                           'Moderate burnout',
                                                                           'Mild burnout',
                                                                           'No burnout'))))) +
  geom_bar(aes(y = (..count..)), width = 0.6, position = "dodge")+
  geom_text(aes(y = (..count..),label = ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..), accuracy = 0.01))),
            stat="count", position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8)  +
  labs(title = "Burnout for physicians working directly with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic() +
  scale_x_discrete(labels = c("Severe\nburnout",
                              'Moderately\nsevere\nburnout',
                              'Moderate\nburnout',
                              'Mild\nburnout',
                              'No\nburnout')) +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.58, 0.25),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)
  ) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# final burnout restructured

# v1

ggplot(na.omit(filtermfdepburnworkcovid), aes(fill = factor(filtermfdepburnworkcovid$worked_with_COVID_pat) ,
                                              x=na.omit(factor(filtermfdepburnworkcovid$level_of_burnout_3.3_scaled,
                                                               levels = c("Severe burnout",
                                                                          'Moderately severe burnout',
                                                                          'Moderate burnout',
                                                                          'Mild burnout',
                                                                          'No burnout'))))) +
  geom_bar(aes(y = (..count..)), width = 0.6, position = "dodge")+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..])),
            stat="count", position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8)  +
  labs(title = "Burnout for physicians working directly with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic() +
  scale_x_discrete(labels = c("Severe\nburnout",
                              'Moderately\nsevere\nburnout',
                              'Moderate\nburnout',
                              'Mild\nburnout',
                              'No\nburnout')) +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.58, 0.25),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)
  ) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# v2

ggplot(na.omit(filtermfdepburnworkcovid), aes(x = factor(filtermfdepburnworkcovid$worked_with_COVID_pat) ,
                                              fill = na.omit(factor(filtermfdepburnworkcovid$level_of_burnout_3.3_scaled,
                                                               levels = c("Severe burnout",
                                                                          'Moderately severe burnout',
                                                                          'Moderate burnout',
                                                                          'Mild burnout',
                                                                          'No burnout'))))) +
  geom_bar(aes(y = (..count..)), width = 0.6, position = "dodge")+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..])),
            stat="count", position = position_dodge(0.65), hjust = -0.1, vjust = 0.4, size = 9.8)  +
  labs(title = "Burnout for physicians working directly with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic()  +
  scale_x_discrete(labels = c("Directly\nworked", "Not directly\nworked")) + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)
  ) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# final depression validated scores barplot omitting NA values for working with COVID patients

ggplot(na.omit(filtermfdepburnworkcovid),  aes(fill = na.omit(factor(filtermfdepburnworkcovid$worked_with_COVID_pat)) ,
                                               x=na.omit(factor(filtermfdepburnworkcovid$depression_final_scaled,
                                                                levels = c('Severe depression', 'Moderately Severe depression', 'Moderate depression',
                                                                          'Mild depression', 'None-Minimal depression'))))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label = ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "Depression for Physicians directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,580)) +  
  theme_classic() + 
  scale_x_discrete(labels = c("Severe\ndepression", 
                              "Moderately\nSevere\ndepression",
                              'Moderate\ndepression',
                              'Mild\ndepression',
                              'None-\nMinimal\ndepression')) +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.58, 0.25),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# depression restructured 

# v1

ggplot(na.omit(filtermfdepburnworkcovid),  aes(fill = na.omit(factor(filtermfdepburnworkcovid$worked_with_COVID_pat)) ,
                                               x=na.omit(factor(filtermfdepburnworkcovid$depression_final_scaled,
                                                                levels = c('Severe depression', 'Moderately Severe depression', 'Moderate depression',
                                                                           'Mild depression', 'None-Minimal depression'))))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..])),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "Depression for Physicians directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,580)) +  
  theme_classic() + 
  scale_x_discrete(labels = c("Severe\ndepression", 
                              "Moderately\nSevere\ndepression",
                              'Moderate\ndepression',
                              'Mild\ndepression',
                              'None-\nMinimal\ndepression')) +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.58, 0.25),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# v2

ggplot(na.omit(filtermfdepburnworkcovid),  aes(x = na.omit(factor(filtermfdepburnworkcovid$worked_with_COVID_pat)) ,
                                               fill =na.omit(factor(filtermfdepburnworkcovid$depression_final_scaled,
                                                                levels = c('Severe depression', 'Moderately Severe depression', 'Moderate depression',
                                                                           'Mild depression', 'None-Minimal depression'))))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..])),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "Depression for Physicians directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,400)) +  
  theme_classic() + 
  scale_x_discrete(labels = c("Directly\nworked", "Not directly\nworked")) +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.65),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)




# PTSD for physicians working directly with covid patients

ggplot(na.omit(filtermfdepburnworkcovid),  aes(fill = na.omit(factor(filtermfdepburnworkcovid$worked_with_COVID_pat)) ,
                                               x=na.omit(factor(filtermfdepburnworkcovid$ptsd_scaled)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label = ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD for Physicians directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,780)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.75),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


######
# PTSD restructured to form 100% within groups 

ggplot(na.omit(filtermfdepburnworkcovid),  aes(fill =  na.omit(factor(filtermfdepburnworkcovid$ptsd_scaled)),
                                               x=na.omit(factor(filtermfdepburnworkcovid$worked_with_COVID_pat)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..])),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD for Physicians directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,780)) +  
  theme_classic() + 
  scale_x_discrete(labels = c("Directly\nworked", "Not directly\nworked")) +
   theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# sufficient resources and working with covid patients 

resourcecoidpatworking <- data.frame(renamed$Sufficient_resources_2.10, renamed$Direcly_worked_COVID_patients_2.1) %>% na.omit()

ggplot(na.omit(resourcecoidpatworking),  aes(fill = na.omit(factor(resourcecoidpatworking$renamed.Direcly_worked_COVID_patients_2.1, )) ,
                                               x=na.omit(factor(resourcecoidpatworking$renamed.Sufficient_resources_2.10, levels = c("Always",
                                                                                                                                     "Most of the time",
                                                                                                                                     "About half the time",
                                                                                                                                     "Sometimes",
                                                                                                                                     "Never",
                                                                                                                                     "Not applicable (N/A)"))))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label = ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "Sufficient resources availability for working with covid patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,780)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.58, 0.25),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# v1

ggplot(na.omit(resourcecoidpatworking),  aes(fill = na.omit(factor(resourcecoidpatworking$renamed.Direcly_worked_COVID_patients_2.1, )) ,
                                             x=na.omit(factor(resourcecoidpatworking$renamed.Sufficient_resources_2.10, levels = c("Always",
                                                                                                                                   "Most of the time",
                                                                                                                                   "About half the time",
                                                                                                                                   "Sometimes",
                                                                                                                                   "Never",
                                                                                                                                   "Not applicable (N/A)"))))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..])),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "Sufficient resources availability for working with covid patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,780)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.58, 0.25),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# v2

ggplot(na.omit(resourcecoidpatworking),  aes(x = na.omit(factor(resourcecoidpatworking$renamed.Direcly_worked_COVID_patients_2.1, )) ,
                                             fill =na.omit(factor(resourcecoidpatworking$renamed.Sufficient_resources_2.10, levels = c("Always",
                                                                                                                                   "Most of the time",
                                                                                                                                   "About half the time",
                                                                                                                                   "Sometimes",
                                                                                                                                   "Never",
                                                                                                                                   "Not applicable (N/A)"))))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..])),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "Sufficient resources availability for working with covid patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,780)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.58, 0.25),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# ptsd and resilience hardiness radarchart

colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.1), rgb(0.8,0.2,0.5,0.1) , rgb(0.7,0.5,0.1,0.4) )

radarchart(normalized_resilience_hardiness, axistype = 1, pcol=colors_border , pfcol=colors_in , plwd=3 , plty=1,
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(1,5,1), cglwd=0.9,
           #custom labels
           vlcex=1.5, title=paste("Average hardiness with respect to PTSD"), cex.main = 3 )


legend(x=0.7, y=1, legend = c("No", "Probable PTSD"), bty = "n", pch=20 ,
       col=colors_in , text.col = "black", cex=1.2, pt.cex=7)

# sex and resilience hardiness radarchart 

colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.1), rgb(0.8,0.2,0.5,0.1) , rgb(0.7,0.5,0.1,0.4) )

radarchart(normalized_resilience_hardiness_wrt_sex, axistype = 1, pcol=colors_border , pfcol=colors_in , plwd=3 , plty=1,
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(1,5,1), cglwd=0.9,
           #custom labels
           vlcex=1.5, title=paste("Average hardiness with respect to sex"), cex.main = 3 )


legend(x=0.7, y=1, legend = c("Female", "Male"), bty = "n", pch=20 ,
       col=colors_in , text.col = "black", cex=1.2, pt.cex=7)

# burnout and resilience hardiness radarchart

colors_border=c( "#F31E1E", "#574141" , "#FFFF00", "#3399FF", "#FFCCE5", "#33FF33" )

radarchart(normalized_resilience_hardiness_burnout, axistype = 1, pcol=colors_border , plwd=3 , plty=1,
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(1,5,1), cglwd=0.9,
           vlcex=1.5, title=paste("Average hardiness with respect to burnout"), cex.main = 3 )


legend(x=1.7, y=1, legend = c("Mild burnout", "Moderate burnout", "Moderately severe burnout",
                              "No burnout", "Severe burnout"), bty = "n", pch=20 ,
       col=colors_border , text.col = "black", cex=1.2, pt.cex=7)


# depression and resilience hardiness radarchart

colors_border=c( "#F31E1E", "#574141" , "#FFFF00", "#3399FF", "#FFCCE5", "#33FF33" )

radarchart(normalized_resilience_hardiness_depression, axistype = 1, pcol=colors_border , plwd=3 , plty=1,
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(1,5,1), cglwd=0.9,
           vlcex=1.5, title=paste("Average hardiness with respect to depression"), cex.main = 3 )

legend(x=1.7, y=1, legend = c("None-Minimal depression", "Mild depression", "Moderate depression", "Moderately Severe depression",
                              "Severe depression"), bty = "n", pch=20 ,
       col=colors_border , text.col = "black", cex=1.2, pt.cex=7)

# chi-sq test for depression, burnout, PTSD and directly worked or not

forchisq <- data.frame(renamed$depression_final_scaled, renamed$level_of_burnout_3.3_scaled, renamed$ptsd_scaled, renamed$Direcly_worked_COVID_patients_2.1)

colnames(forchisq) <- c("Depression", "Burnout", "PTSD", "Directly worked with COVID patients")


mapply(function(x, y) chisq.test(x, y)$p.value, forchisq[, -4], MoreArgs=list(forchisq[,4]))

#living arrangement changes with respect to working with covid patients

livingchangesworkedornot <- data.frame(renamed$living_changes_cooncern_covid_2.5, renamed$Direcly_worked_COVID_patients_2.1) %>% na.omit()



ggplot(na.omit(livingchangesworkedornot),  aes(x = na.omit(factor(livingchangesworkedornot$renamed.Direcly_worked_COVID_patients_2.1 )) ,
                                             fill =na.omit(factor(livingchangesworkedornot$renamed.living_changes_cooncern_covid_2.5)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..])),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "Living changes with respect to directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic() + 
  scale_x_discrete(labels = c("Directly\nworked", "Not directly\nworked")) + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.85),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# working outside normal scope of responsibilities with respect to directly working with COVID patients

workingoutsideworkedornot <- data.frame(renamed$work_outside_normal_resp_2.8, renamed$Direcly_worked_COVID_patients_2.1) %>% na.omit()
  

ggplot(na.omit(workingoutsideworkedornot),  aes(x = na.omit(factor(workingoutsideworkedornot$renamed.Direcly_worked_COVID_patients_2.1 )) ,
                                               fill =na.omit(factor(workingoutsideworkedornot$renamed.work_outside_normal_resp_2.8)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..])),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "Working outside scope with respect to directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic() + 
  scale_x_discrete(labels = c("Directly\nworked", "Not directly\nworked")) +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.85),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# extra work hours to care for COVID patients

extrahoursworkedornot <- data.frame(renamed$work_extra_hours_2.7, renamed$Direcly_worked_COVID_patients_2.1) %>% na.omit()

ggplot(na.omit(extrahoursworkedornot),  aes(x = na.omit(factor(extrahoursworkedornot$renamed.Direcly_worked_COVID_patients_2.1 )) ,
                                                fill =na.omit(factor(extrahoursworkedornot$renamed.work_extra_hours_2.7)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..])),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "Working extra hours with respect to directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic() + 
  scale_x_discrete(labels = c("Directly\nworked", "Not directly\nworked")) + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.85),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# dificulty level of job with respect to working with COVID patients

difficultyworkedornot <- data.frame(renamed$job_difficult_dueto_COVID_2.9, renamed$Direcly_worked_COVID_patients_2.1) %>% na.omit()

# v1   

ggplot(na.omit(difficultyworkedornot),  aes(fill = na.omit(factor(difficultyworkedornot$renamed.Direcly_worked_COVID_patients_2.1 )) ,
                                            x =na.omit(factor(difficultyworkedornot$renamed.job_difficult_dueto_COVID_2.9, levels = c("None at all", "A little bit more",
                                                                                                                                      "Moderately more", "A lot more", "Extremely more" ))))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..])),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "Job difficulty with respect to directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)),  limits = c(0,300)) +  
  theme_classic() +  
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.15),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# v2

ggplot(na.omit(difficultyworkedornot),  aes(x = na.omit(factor(difficultyworkedornot$renamed.Direcly_worked_COVID_patients_2.1 )) ,
                                            fill =na.omit(factor(difficultyworkedornot$renamed.job_difficult_dueto_COVID_2.9,
                                                                 levels = c("None at all", "A little bit more",
                                                                            "Moderately more", "A lot more", "Extremely more" ))))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..])),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "Job difficulty with respect to directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic() + 
  scale_x_discrete(labels = c("Directly\nworked", "Not directly\nworked")) +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.65),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# part of decision making process with respect to directly working with covid patients

partdecisionworkedornot <- data.frame(renamed$Part_of_decision_making_2.11, renamed$Direcly_worked_COVID_patients_2.1) %>% na.omit()

# v1


ggplot(na.omit(partdecisionworkedornot),  aes(fill = na.omit(factor(partdecisionworkedornot$renamed.Direcly_worked_COVID_patients_2.1 )) ,
                                              x =na.omit(factor(partdecisionworkedornot$renamed.Part_of_decision_making_2.1,levels = c("Not applicable (N/A)",
                                                                                                                                       "Never",
                                                                                                                                       "Sometimes",
                                                                                                                                       "About half the time",
                                                                                                                                       "Most of the time",
                                                                                                                                       "Always"))))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..])),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "Part of decision making process with respect to directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic() +  
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.75),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)




# v2

ggplot(na.omit(partdecisionworkedornot),  aes(x = na.omit(factor(partdecisionworkedornot$renamed.Direcly_worked_COVID_patients_2.1 )) ,
                                            fill =na.omit(factor(partdecisionworkedornot$renamed.Part_of_decision_making_2.11,
                                                                 levels = c("Not applicable (N/A)",
                                                                            "Never",
                                                                            "Sometimes",
                                                                            "About half the time",
                                                                            "Most of the time",
                                                                            "Always"))))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..])),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "Part of decision making process with respect to directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic() + 
  scale_x_discrete(labels = c("Directly\nworked", "Not directly\nworked")) +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# turnover intent - leave current employer with respect to directly worked with COVID patients

turnoverleavecurrentworkedornot <- data.frame(renamed$turnoverintent_leave_current_2.15.2, renamed$Direcly_worked_COVID_patients_2.1) %>% na.omit()

# v1

ggplot(na.omit(turnoverleavecurrentworkedornot),  aes(fill = na.omit(factor(turnoverleavecurrentworkedornot$renamed.Direcly_worked_COVID_patients_2.1 )) ,
                                                      x =na.omit(factor(turnoverleavecurrentworkedornot$renamed.turnoverintent_leave_current_2.15.2,
                                                                           levels = c("None", "Slight", "Moderate",
                                                                                      "Likely", "Definitely"))))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..])),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "Leave current employer with respect to directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,430)) +  
  theme_classic()  + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)



# v2

ggplot(na.omit(turnoverleavecurrentworkedornot),  aes(x = na.omit(factor(turnoverleavecurrentworkedornot$renamed.Direcly_worked_COVID_patients_2.1 )) ,
                                              fill =na.omit(factor(turnoverleavecurrentworkedornot$renamed.turnoverintent_leave_current_2.15.2,
                                                                   levels = c("None", "Slight", "Moderate",
                                                                              "Likely", "Definitely"))))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..])),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "Leave current employer with respect to directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,430)) +  
  theme_classic() + 
  scale_x_discrete(labels = c("Directly\nworked", "Not directly\nworked")) +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# turnover intent - leave healthcare with respect to directly worked with COVID patients

turnoverleavehealthworkedornot <- data.frame(renamed$turnoverintent_leave_healthcare_2.15.3, renamed$Direcly_worked_COVID_patients_2.1) %>% na.omit()

# v1

ggplot(na.omit(turnoverleavehealthworkedornot),  aes(fill = na.omit(factor(turnoverleavehealthworkedornot$renamed.Direcly_worked_COVID_patients_2.1 )) ,
                                                      x =na.omit(factor(turnoverleavehealthworkedornot$renamed.turnoverintent_leave_healthcare_2.15.3,
                                                                        levels = c("None", "Slight", "Moderate",
                                                                                   "Likely", "Definitely"))))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "Leave healthcare with respect to directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic()  + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# v2

ggplot(na.omit(turnoverleavehealthworkedornot),  aes(x = na.omit(factor(turnoverleavehealthworkedornot$renamed.Direcly_worked_COVID_patients_2.1 )) ,
                                                     fill =na.omit(factor(turnoverleavehealthworkedornot$renamed.turnoverintent_leave_healthcare_2.15.3,
                                                                       levels = c("None", "Slight", "Moderate",
                                                                                  "Likely", "Definitely"))))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "Leave healthcare with respect to directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic()  + 
  scale_x_discrete(labels = c("Directly\nworked", "Not directly\nworked")) +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# turnover intent - switch team with respect to directly worked with COVID patients

turnoverswitchteamsworkedornot <- data.frame(renamed$turnoverintent_switch_teams_2.15.1, renamed$Direcly_worked_COVID_patients_2.1) %>% na.omit()


# v1

ggplot(na.omit(turnoverswitchteamsworkedornot),  aes(fill = na.omit(factor(turnoverswitchteamsworkedornot$renamed.Direcly_worked_COVID_patients_2.1)) ,
                                                     x =na.omit(factor(turnoverswitchteamsworkedornot$renamed.turnoverintent_switch_teams_2.15.1,
                                                                       levels = c("None", "Slight", "Moderate",
                                                                                  "Likely", "Definitely"))))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "Switch teams with respect to directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic()  + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# v2


ggplot(na.omit(turnoverswitchteamsworkedornot),  aes(x = na.omit(factor(turnoverswitchteamsworkedornot$renamed.Direcly_worked_COVID_patients_2.1 )) ,
                                                     fill =na.omit(factor(turnoverswitchteamsworkedornot$renamed.turnoverintent_switch_teams_2.15.1,
                                                                          levels = c("None", "Slight", "Moderate",
                                                                                     "Likely", "Definitely"))))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "Switch teams with respect to directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic()  + 
  scale_x_discrete(labels = c("Directly\nworked", "Not directly\nworked")) +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# primary work setting with respect to directly working with COVID patients,

primaryworkworkedornot <- data.frame(renamed$Primary_work_setting_5.11, renamed$Direcly_worked_COVID_patients_2.1) %>% na.omit()

primaryworkworkedornot$renamed.Primary_work_setting_5.11 <- revalue(primaryworkworkedornot$renamed.Primary_work_setting_5.11, c("Others (please specify):" = "Others"))

primaryworkworkedornot$renamed.Primary_work_setting_5.11 <- factor(primaryworkworkedornot$renamed.Primary_work_setting_5.11,
                                                                   levels = c("Others",
                                                                              "Outpatient center",
                                                                              "Two-physician practice",
                                                                              "Solo practice",
                                                                              "Hospital",
                                                                              "Group practice",
                                                                              "Academic medical center"))


# v1

ggplot(na.omit(primaryworkworkedornot),  aes(fill = na.omit(factor(primaryworkworkedornot$renamed.Direcly_worked_COVID_patients_2.1)) ,
                                                     x = na.omit(factor(primaryworkworkedornot$renamed.Primary_work_setting_5.11, )))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "Primary work setting with respect to directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,210)) +  
  theme_classic()  + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.30),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# v2

ggplot(na.omit(primaryworkworkedornot),  aes(x = na.omit(factor(primaryworkworkedornot$renamed.Direcly_worked_COVID_patients_2.1 )) ,
                                                     fill =na.omit(factor(primaryworkworkedornot$renamed.Primary_work_setting_5.11)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "Primary work setting with respect to directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic()  + 
  scale_x_discrete(labels = c("Directly\nworked", "Not directly\nworked")) +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


############# PTSD individual symptoms

# question 1 (unwanted memories)

ptsdq1vsworked <- data.frame(ptsdindiv$renamed.PTSD_unwanted_memories_3.2.1, renamed$Direcly_worked_COVID_patients_2.1) %>% na.omit()

# v1

ggplot(na.omit(ptsdq1vsworked),  aes(fill = na.omit(factor(ptsdq1vsworked$renamed.Direcly_worked_COVID_patients_2.1)) ,
                                             x = na.omit(factor(ptsdq1vsworked$ptsdindiv.renamed.PTSD_unwanted_memories_3.2.1)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD - unwanted memories and directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic()  + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# table to verify the whether proportions in ggplot turned out to be expected or not

table(renamed$PTSD_unwanted_memories_3.2.1, renamed$Direcly_worked_COVID_patients_2.1) %>% prop.table(margin = 2)

# v2

ggplot(na.omit(ptsdq1vsworked),  aes(x = na.omit(factor(ptsdq1vsworked$renamed.Direcly_worked_COVID_patients_2.1)) ,
                                     fill = na.omit(factor(ptsdq1vsworked$ptsdindiv.renamed.PTSD_unwanted_memories_3.2.1)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD - unwanted memories and directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic()  + 
  scale_x_discrete(labels = c("Directly\nworked", "Not directly\nworked")) + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# question 2 (repeated disturbing dreams)

ptsdq2vsworked <- data.frame(ptsdindiv$renamed.PTSD_disturbing_dreams_3.2.2, ptsdindiv$renamed.Direcly_worked_COVID_patients_2.1) %>% na.omit()

# v1


ggplot(na.omit(ptsdq2vsworked),  aes(fill = na.omit(factor(ptsdq2vsworked$ptsdindiv.renamed.Direcly_worked_COVID_patients_2.1)) ,
                                     x = na.omit(factor(ptsdq2vsworked$ptsdindiv.renamed.PTSD_disturbing_dreams_3.2.2)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD - disturbing dreams and directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic()  + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# v2

ggplot(na.omit(ptsdq2vsworked),  aes(x = na.omit(factor(ptsdq2vsworked$ptsdindiv.renamed.Direcly_worked_COVID_patients_2.1)) ,
                                     fill = na.omit(factor(ptsdq2vsworked$ptsdindiv.renamed.PTSD_disturbing_dreams_3.2.2)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD - disturbing dreams and directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic()  + 
  scale_x_discrete(labels = c("Directly\nworked", "Not directly\nworked")) + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# question 3 (suddenly acting disturbing dream happening again)

ptsdq3vsworked <- data.frame(ptsdindiv$renamed.PTSD_actually_happening_again_3.2.3, ptsdindiv$renamed.Direcly_worked_COVID_patients_2.1) %>% na.omit()

# v1

ggplot(na.omit(ptsdq3vsworked),  aes(fill = na.omit(factor(ptsdq3vsworked$ptsdindiv.renamed.Direcly_worked_COVID_patients_2.1)) ,
                                     x = na.omit(factor(ptsdq3vsworked$ptsdindiv.renamed.PTSD_actually_happening_again_3.2.3)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD - experience happening again and directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic()  + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# v2

ggplot(na.omit(ptsdq3vsworked),  aes(x = na.omit(factor(ptsdq3vsworked$ptsdindiv.renamed.Direcly_worked_COVID_patients_2.1)) ,
                                     fill = na.omit(factor(ptsdq3vsworked$ptsdindiv.renamed.PTSD_actually_happening_again_3.2.3)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD - experience happening again and directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic()  + 
  scale_x_discrete(labels = c("Directly\nworked", "Not directly\nworked")) + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)
        
# question 4 (Feeling very upset when something reminded of stressfull experience)

ptsdq4vsworked <- data.frame(ptsdindiv$renamed.PTSD_upset_when_reminded_3.2.4, ptsdindiv$renamed.Direcly_worked_COVID_patients_2.1) %>% na.omit()


# v1

ggplot(na.omit(ptsdq4vsworked),  aes(fill = na.omit(factor(ptsdq4vsworked$ptsdindiv.renamed.Direcly_worked_COVID_patients_2.1)) ,
                                     x = na.omit(factor(ptsdq4vsworked$ptsdindiv.renamed.PTSD_upset_when_reminded_3.2.4)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD - feeling upset and directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic()  + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# v2

ggplot(na.omit(ptsdq4vsworked),  aes(x = na.omit(factor(ptsdq4vsworked$ptsdindiv.renamed.Direcly_worked_COVID_patients_2.1)) ,
                                     fill = na.omit(factor(ptsdq4vsworked$ptsdindiv.renamed.PTSD_upset_when_reminded_3.2.4)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD - feeling upset and directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic()  + 
  scale_x_discrete(labels = c("Directly\nworked", "Not directly\nworked")) + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# question 5 (having strong physical reaction)

ptsdq5vsworked <- data.frame(ptsdindiv$renamed.PTSD_physical_reaction_reminded_3.2.5, ptsdindiv$renamed.Direcly_worked_COVID_patients_2.1) %>% na.omit()


# v1

ggplot(na.omit(ptsdq5vsworked),  aes(fill = na.omit(factor(ptsdq5vsworked$ptsdindiv.renamed.Direcly_worked_COVID_patients_2.1)) ,
                                     x = na.omit(factor(ptsdq5vsworked$ptsdindiv.renamed.PTSD_physical_reaction_reminded_3.2.5)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD - physical reaction and directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic()  + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)



# v2

ggplot(na.omit(ptsdq5vsworked),  aes(x = na.omit(factor(ptsdq5vsworked$ptsdindiv.renamed.Direcly_worked_COVID_patients_2.1)) ,
                                     fill = na.omit(factor(ptsdq5vsworked$ptsdindiv.renamed.PTSD_physical_reaction_reminded_3.2.5)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD - physical reaction and directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic()  + 
  scale_x_discrete(labels = c("Directly\nworked", "Not directly\nworked")) + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)



# question 6 (avoiding memories)

ptsdq6vsworked <- data.frame(ptsdindiv$renamed.PTSD_avoiding_memories_3.2.6, ptsdindiv$renamed.Direcly_worked_COVID_patients_2.1) %>% na.omit()

# v1

ggplot(na.omit(ptsdq6vsworked),  aes(fill = na.omit(factor(ptsdq6vsworked$ptsdindiv.renamed.Direcly_worked_COVID_patients_2.1)) ,
                                     x = na.omit(factor(ptsdq6vsworked$ptsdindiv.renamed.PTSD_avoiding_memories_3.2.6)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD - avoiding memories and directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic()  + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# v2


ggplot(na.omit(ptsdq6vsworked),  aes(x = na.omit(factor(ptsdq6vsworked$ptsdindiv.renamed.Direcly_worked_COVID_patients_2.1)) ,
                                     fill = na.omit(factor(ptsdq6vsworked$ptsdindiv.renamed.PTSD_avoiding_memories_3.2.6)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD - avoiding memories and directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic()  + 
  scale_x_discrete(labels = c("Directly\nworked", "Not directly\nworked")) +  
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)



# question 7 (avoiding external reminders)

ptsdq7vsworked <- data.frame(ptsdindiv$renamed.PTSD_avoiding_externam_reminded_3.2.7, ptsdindiv$renamed.Direcly_worked_COVID_patients_2.1) %>% na.omit()

# v1

ggplot(na.omit(ptsdq7vsworked),  aes(fill = na.omit(factor(ptsdq7vsworked$ptsdindiv.renamed.Direcly_worked_COVID_patients_2.1)) ,
                                     x = na.omit(factor(ptsdq7vsworked$ptsdindiv.renamed.PTSD_avoiding_externam_reminded_3.2.7)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD - avoiding external reminders and directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic()  + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)



# v2

ggplot(na.omit(ptsdq7vsworked),  aes(x = na.omit(factor(ptsdq7vsworked$ptsdindiv.renamed.Direcly_worked_COVID_patients_2.1)) ,
                                     fill = na.omit(factor(ptsdq7vsworked$ptsdindiv.renamed.PTSD_avoiding_externam_reminded_3.2.7)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD - avoiding external reminders and directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic()  + 
  scale_x_discrete(labels = c("Directly\nworked", "Not directly\nworked")) +  
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# question 8 (trouble remembering stressful experience)

ptsdq8vsworked <- data.frame(ptsdindiv$renamed.PTSD_trouble_remembering_3.2.8, ptsdindiv$renamed.Direcly_worked_COVID_patients_2.1) %>% na.omit()


# v1

ggplot(na.omit(ptsdq8vsworked),  aes(fill = na.omit(factor(ptsdq8vsworked$ptsdindiv.renamed.Direcly_worked_COVID_patients_2.1)) ,
                                     x = na.omit(factor(ptsdq8vsworked$ptsdindiv.renamed.PTSD_trouble_remembering_3.2.8)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD - trouble remembering and directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic()  + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# v2

ggplot(na.omit(ptsdq8vsworked),  aes(x = na.omit(factor(ptsdq8vsworked$ptsdindiv.renamed.Direcly_worked_COVID_patients_2.1)) ,
                                     fill = na.omit(factor(ptsdq8vsworked$ptsdindiv.renamed.PTSD_trouble_remembering_3.2.8)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD - trouble remembering and directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic()  + 
  scale_x_discrete(labels = c("Directly\nworked", "Not directly\nworked")) +  
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# question 9 (having strong negative beliefs about yourself)

ptsdq9vsworked <- data.frame(ptsdindiv$renamed.PTSD_negative_belief_self_3.2.9, ptsdindiv$renamed.Direcly_worked_COVID_patients_2.1) %>% na.omit()

# v1

ggplot(na.omit(ptsdq9vsworked),  aes(fill = na.omit(factor(ptsdq9vsworked$ptsdindiv.renamed.Direcly_worked_COVID_patients_2.1)) ,
                                     x = na.omit(factor(ptsdq9vsworked$ptsdindiv.renamed.PTSD_negative_belief_self_3.2.9)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD - strong negative beliefs and directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic()  + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# v2

ggplot(na.omit(ptsdq9vsworked),  aes(x = na.omit(factor(ptsdq9vsworked$ptsdindiv.renamed.Direcly_worked_COVID_patients_2.1)) ,
                                     fill = na.omit(factor(ptsdq9vsworked$ptsdindiv.renamed.PTSD_negative_belief_self_3.2.9)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD - strong negative beliefs and directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic()  + 
  scale_x_discrete(labels = c("Directly\nworked", "Not directly\nworked")) +  
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# question 10 (blaming yourself or someone)

ptsdq10vsworked <- data.frame(ptsdindiv$renamed.PTSD_blaming_self_3.2.10, ptsdindiv$renamed.Direcly_worked_COVID_patients_2.1) %>% na.omit()


# v1

ggplot(na.omit(ptsdq10vsworked),  aes(fill = na.omit(factor(ptsdq10vsworked$ptsdindiv.renamed.Direcly_worked_COVID_patients_2.1)) ,
                                     x = na.omit(factor(ptsdq10vsworked$ptsdindiv.renamed.PTSD_blaming_self_3.2.10)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD - blaming self or others and directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic()  + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# v2

ggplot(na.omit(ptsdq10vsworked),  aes(x = na.omit(factor(ptsdq10vsworked$ptsdindiv.renamed.Direcly_worked_COVID_patients_2.1)) ,
                                     fill = na.omit(factor(ptsdq10vsworked$ptsdindiv.renamed.PTSD_blaming_self_3.2.10)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD - blaming self or others and directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic()  + 
  scale_x_discrete(labels = c("Directly\nworked", "Not directly\nworked")) +  
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# question 11 (having strong negative feeling)

ptsdq11vsworked <- data.frame(ptsdindiv$renamed.PTSD_negative_feelings_3.2.11, ptsdindiv$renamed.Direcly_worked_COVID_patients_2.1) %>% na.omit()


# v1

ggplot(na.omit(ptsdq11vsworked),  aes(fill = na.omit(factor(ptsdq11vsworked$ptsdindiv.renamed.Direcly_worked_COVID_patients_2.1)) ,
                                      x = na.omit(factor(ptsdq11vsworked$ptsdindiv.renamed.PTSD_negative_feelings_3.2.11)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD - strong negative feeling and directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic()  + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# v2

ggplot(na.omit(ptsdq11vsworked),  aes(x = na.omit(factor(ptsdq11vsworked$ptsdindiv.renamed.Direcly_worked_COVID_patients_2.1)) ,
                                      fill = na.omit(factor(ptsdq11vsworked$ptsdindiv.renamed.PTSD_negative_feelings_3.2.11)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD - strong negative feeling and directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic()  + 
  scale_x_discrete(labels = c("Directly\nworked", "Not directly\nworked")) +  
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)



# question 12 (Loss of interest in activities)

ptsdq12vsworked <- data.frame(ptsdindiv$renamed.PTSD_loss_of_interest_3.2.12, ptsdindiv$renamed.Direcly_worked_COVID_patients_2.1) %>% na.omit()

# v1

ggplot(na.omit(ptsdq12vsworked),  aes(fill = na.omit(factor(ptsdq12vsworked$ptsdindiv.renamed.Direcly_worked_COVID_patients_2.1)) ,
                                      x = na.omit(factor(ptsdq12vsworked$ptsdindiv.renamed.PTSD_loss_of_interest_3.2.12)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD - loss of interest and directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic()  + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# v2

ggplot(na.omit(ptsdq12vsworked),  aes(x = na.omit(factor(ptsdq12vsworked$ptsdindiv.renamed.Direcly_worked_COVID_patients_2.1)) ,
                                      fill = na.omit(factor(ptsdq12vsworked$ptsdindiv.renamed.PTSD_loss_of_interest_3.2.12)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD - loss of interest and directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic()  + 
  scale_x_discrete(labels = c("Directly\nworked", "Not directly\nworked")) +  
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# question 13 (feeling distant or cutoff)

ptsdq13vsworked <- data.frame(ptsdindiv$renamed.PTSD_distant_from_people_3.2.13, ptsdindiv$renamed.Direcly_worked_COVID_patients_2.1) %>% na.omit()

# v1

ggplot(na.omit(ptsdq13vsworked),  aes(fill = na.omit(factor(ptsdq13vsworked$ptsdindiv.renamed.Direcly_worked_COVID_patients_2.1)) ,
                                      x = na.omit(factor(ptsdq13vsworked$ptsdindiv.renamed.PTSD_distant_from_people_3.2.13)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD - feeling distant and directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic()  + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# v2

ggplot(na.omit(ptsdq13vsworked),  aes(x = na.omit(factor(ptsdq13vsworked$ptsdindiv.renamed.Direcly_worked_COVID_patients_2.1)) ,
                                      fill = na.omit(factor(ptsdq13vsworked$ptsdindiv.renamed.PTSD_distant_from_people_3.2.13)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD - feeling distant and directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic()  + 
  scale_x_discrete(labels = c("Directly\nworked", "Not directly\nworked")) +  
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# question 14 (trouble experienceing positive feeling)

ptsdq14vsworked <- data.frame(ptsdindiv$renamed.PTSD_trouble_exp_positive_feelings_3.2.14, ptsdindiv$renamed.Direcly_worked_COVID_patients_2.1) %>% na.omit()

# v1

ggplot(na.omit(ptsdq14vsworked),  aes(fill = na.omit(factor(ptsdq14vsworked$ptsdindiv.renamed.Direcly_worked_COVID_patients_2.1)) ,
                                      x = na.omit(factor(ptsdq14vsworked$ptsdindiv.renamed.PTSD_trouble_exp_positive_feelings_3.2.14)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD - trouble experiencing positive feeling and directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic()  + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# v2

ggplot(na.omit(ptsdq14vsworked),  aes(x = na.omit(factor(ptsdq14vsworked$ptsdindiv.renamed.Direcly_worked_COVID_patients_2.1)) ,
                                      fill = na.omit(factor(ptsdq14vsworked$ptsdindiv.renamed.PTSD_trouble_exp_positive_feelings_3.2.14)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD - trouble experiencing positive feeling and directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic()  + 
  scale_x_discrete(labels = c("Directly\nworked", "Not directly\nworked")) +  
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# question 15 (irritable behaviour)

ptsdq15vsworked <- data.frame(ptsdindiv$renamed.PTSD_irritable_behavior_3.2.15, ptsdindiv$renamed.Direcly_worked_COVID_patients_2.1) %>% na.omit()

# v1

ggplot(na.omit(ptsdq15vsworked),  aes(fill = na.omit(factor(ptsdq15vsworked$ptsdindiv.renamed.Direcly_worked_COVID_patients_2.1)) ,
                                      x = na.omit(factor(ptsdq15vsworked$ptsdindiv.renamed.PTSD_irritable_behavior_3.2.15)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD - irritable behavior and directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic()  + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# v2

ggplot(na.omit(ptsdq15vsworked),  aes(x = na.omit(factor(ptsdq15vsworked$ptsdindiv.renamed.Direcly_worked_COVID_patients_2.1)) ,
                                      fill = na.omit(factor(ptsdq15vsworked$ptsdindiv.renamed.PTSD_irritable_behavior_3.2.15)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD - irritable behavior and directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic()  + 
  scale_x_discrete(labels = c("Directly\nworked", "Not directly\nworked")) +  
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# question 16 (taking too many risks)

ptsdq16vsworked <- data.frame(ptsdindiv$renamed.PTSD_taking_too_many_risk_3.2.16, ptsdindiv$renamed.Direcly_worked_COVID_patients_2.1) %>% na.omit()

# v1

ggplot(na.omit(ptsdq16vsworked),  aes(fill = na.omit(factor(ptsdq16vsworked$ptsdindiv.renamed.Direcly_worked_COVID_patients_2.1)) ,
                                      x = na.omit(factor(ptsdq16vsworked$ptsdindiv.renamed.PTSD_taking_too_many_risk_3.2.16)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD - taking too many risk and directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic()  + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# v2

ggplot(na.omit(ptsdq16vsworked),  aes(x = na.omit(factor(ptsdq16vsworked$ptsdindiv.renamed.Direcly_worked_COVID_patients_2.1)) ,
                                      fill = na.omit(factor(ptsdq16vsworked$ptsdindiv.renamed.PTSD_taking_too_many_risk_3.2.16)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD - taking too many risk and directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic()  + 
  scale_x_discrete(labels = c("Directly\nworked", "Not directly\nworked")) +  
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# question 17 (being super alert)

ptsdq17vsworked <- data.frame(ptsdindiv$renamed.PTSD_super_alert_3.2.17, ptsdindiv$renamed.Direcly_worked_COVID_patients_2.1) %>% na.omit()


# v1

ggplot(na.omit(ptsdq17vsworked),  aes(fill = na.omit(factor(ptsdq17vsworked$ptsdindiv.renamed.Direcly_worked_COVID_patients_2.1)) ,
                                      x = na.omit(factor(ptsdq17vsworked$ptsdindiv.renamed.PTSD_super_alert_3.2.17)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD - being super alert and directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic()  + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# v2

ggplot(na.omit(ptsdq17vsworked),  aes(x = na.omit(factor(ptsdq17vsworked$ptsdindiv.renamed.Direcly_worked_COVID_patients_2.1)) ,
                                      fill = na.omit(factor(ptsdq17vsworked$ptsdindiv.renamed.PTSD_super_alert_3.2.17)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD - being super alert and directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic()  + 
  scale_x_discrete(labels = c("Directly\nworked", "Not directly\nworked")) +  
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# question 18 (feeling jumpy or easily starled)

ptsdq18vsworked <- data.frame(ptsdindiv$renamed.PTSD_easily_startled_3.2.18, ptsdindiv$renamed.Direcly_worked_COVID_patients_2.1) %>% na.omit()

# v1

ggplot(na.omit(ptsdq18vsworked),  aes(fill = na.omit(factor(ptsdq18vsworked$ptsdindiv.renamed.Direcly_worked_COVID_patients_2.1)) ,
                                      x = na.omit(factor(ptsdq18vsworked$ptsdindiv.renamed.PTSD_easily_startled_3.2.18)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD - feeling jumpy and directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic()  + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# v2

ggplot(na.omit(ptsdq18vsworked),  aes(x = na.omit(factor(ptsdq18vsworked$ptsdindiv.renamed.Direcly_worked_COVID_patients_2.1)) ,
                                      fill = na.omit(factor(ptsdq18vsworked$ptsdindiv.renamed.PTSD_easily_startled_3.2.18)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD - feeling jumpy and directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic()  + 
  scale_x_discrete(labels = c("Directly\nworked", "Not directly\nworked")) +  
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# question 19 (having difficulty concentrating)

ptsdq19vsworked <- data.frame(ptsdindiv$renamed.PTSD_difficulty_concentrating_3.2.19, ptsdindiv$renamed.Direcly_worked_COVID_patients_2.1) %>% na.omit()

# v1

ggplot(na.omit(ptsdq19vsworked),  aes(fill = na.omit(factor(ptsdq19vsworked$ptsdindiv.renamed.Direcly_worked_COVID_patients_2.1)) ,
                                      x = na.omit(factor(ptsdq19vsworked$ptsdindiv.renamed.PTSD_difficulty_concentrating_3.2.19)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD - having difficulty concentrating and directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic()  + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# v2

ggplot(na.omit(ptsdq19vsworked),  aes(x = na.omit(factor(ptsdq19vsworked$ptsdindiv.renamed.Direcly_worked_COVID_patients_2.1)) ,
                                      fill = na.omit(factor(ptsdq19vsworked$ptsdindiv.renamed.PTSD_difficulty_concentrating_3.2.19)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD - having difficulty concentrating and directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic()  + 
  scale_x_discrete(labels = c("Directly\nworked", "Not directly\nworked")) +  
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# question 20 (trouble falling or staying asleep)

ptsdq20vsworked <- data.frame(ptsdindiv$renamed.PTSD_trouble_asleep_3.2.20, ptsdindiv$renamed.Direcly_worked_COVID_patients_2.1) %>% na.omit()

# v1

ggplot(na.omit(ptsdq20vsworked),  aes(fill = na.omit(factor(ptsdq20vsworked$ptsdindiv.renamed.Direcly_worked_COVID_patients_2.1)) ,
                                      x = na.omit(factor(ptsdq20vsworked$ptsdindiv.renamed.PTSD_trouble_asleep_3.2.20)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD - trouble falling asleep and directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic()  + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# v2

ggplot(na.omit(ptsdq20vsworked),  aes(x = na.omit(factor(ptsdq20vsworked$ptsdindiv.renamed.Direcly_worked_COVID_patients_2.1)) ,
                                      fill = na.omit(factor(ptsdq20vsworked$ptsdindiv.renamed.PTSD_trouble_asleep_3.2.20)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD - trouble falling asleep and directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +  
  theme_classic()  + 
  scale_x_discrete(labels = c("Directly\nworked", "Not directly\nworked")) +  
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.80),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# summary statistic table for ptsd criterion

label(ptsdcriterion$criterionB) <- "Criterion B"
label(ptsdcriterion$criterionC) <- "Criterion C"
label(ptsdcriterion$criterionD) <- "Criterion D"
label(ptsdcriterion$criterionE) <- "Criterion E"

table1(~ ptsdcriterion$criterionB + ptsdcriterion$criterionC + ptsdcriterion$criterionD + ptsdcriterion$criterionE)


########
# density plot for criterion b scores and directly worked or not

ptsdcriterionb <- data.frame(ptsdcriterion$criterionB, ptsdcriterion$directlyworked_or_not) %>% na.omit()

meanb <- aggregate(ptsdcriterion.criterionB ~ ptsdcriterion.directlyworked_or_not, ptsdcriterionb, mean)

annot <- data.frame(
  x1 = c(3.25, 0.75),
  y1 = c(0.3, 0.3)
)

meanb$x1 <- annot$x1
meanb$y1 <- annot$y1

meanb$ptsdcriterion.criterionB <- meanb$ptsdcriterion.criterionB %>% round(2)

ggplot(data=ptsdcriterionb, aes(x=ptsdcriterion.criterionB, group=ptsdcriterion.directlyworked_or_not, fill=ptsdcriterion.directlyworked_or_not)) +
  geom_density(adjust=1.5, alpha=.4) +
  labs(title = "PTSD - criterion B", x = '', y = 'Density') + 
  geom_vline(data = meanb, aes(xintercept = ptsdcriterion.criterionB, color = ptsdcriterion.directlyworked_or_not), linetype = "dashed", size = 1) +
  geom_text(data = meanb, aes(x = x1, y = y1, label = ptsdcriterion.criterionB, fontface = 2),
            hjust=0, size=8.5, face = 'bold') +
  theme_ipsum() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),axis.title.y = element_text(size=30, hjust = 0.5, vjust = 1.7),
        legend.position = c(0.78, 0.70),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) 


# density plot for criterion c scores and directly worked or not

ptsdcriterionc <- data.frame(ptsdcriterion$criterionC, ptsdcriterion$directlyworked_or_not) %>% na.omit()

meanc <- aggregate(ptsdcriterion.criterionC ~ ptsdcriterion.directlyworked_or_not, ptsdcriterionc, mean)

annot <- data.frame(
  x1 = c(1.3, 0.3),
  y1 = c(0.75, 0.75)
)

meanc$x1 <- annot$x1
meanc$y1 <- annot$y1

meanc$ptsdcriterion.criterionC <- meanc$ptsdcriterion.criterionC %>% round(2)


ggplot(data=ptsdcriterionc, aes(x=ptsdcriterion.criterionC, group=ptsdcriterion.directlyworked_or_not, fill=ptsdcriterion.directlyworked_or_not)) +
  geom_density(adjust=1.5, alpha=.4) +
  labs(title = "PTSD - criterion C", x = '', y = 'Density') + 
  geom_vline(data = meanc, aes(xintercept = ptsdcriterion.criterionC, color = ptsdcriterion.directlyworked_or_not), linetype = "dashed", size = 1) +
  geom_text(data = meanc, aes(x = x1, y = y1, label = ptsdcriterion.criterionC, fontface = 2),
            hjust=0, size=8.5, face = 'bold') +
  theme_ipsum() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),axis.title.y = element_text(size=30, hjust = 0.5, vjust = 1.7),
        legend.position = c(0.78, 0.70),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) 



# density plot for criterion d scores and directly worked or not

ptsdcriteriond <- data.frame(ptsdcriterion$criterionD, ptsdcriterion$directlyworked_or_not) %>% na.omit()

meand <- aggregate(ptsdcriterion.criterionD ~ ptsdcriterion.directlyworked_or_not, ptsdcriteriond, mean)

annot <- data.frame(
  x1 = c(5.2, 2.4),
  y1 = c(0.125, 0.125)
)

meand$x1 <- annot$x1
meand$y1 <- annot$y1

meand$ptsdcriterion.criterionD <- meand$ptsdcriterion.criterionD %>% round(2)


ggplot(data=ptsdcriteriond, aes(x=ptsdcriterion.criterionD, group=ptsdcriterion.directlyworked_or_not, fill=ptsdcriterion.directlyworked_or_not)) +
  geom_density(adjust=1.5, alpha=.4) +
  labs(title = "PTSD - criterion D", x = '', y = 'Density') + 
  geom_vline(data = meand, aes(xintercept = ptsdcriterion.criterionD, color = ptsdcriterion.directlyworked_or_not), linetype = "dashed", size = 1) +
  geom_text(data = meand, aes(x = x1, y = y1, label = ptsdcriterion.criterionD, fontface = 2),
            hjust=0, size=8.5, face = 'bold') +
  theme_ipsum() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),axis.title.y = element_text(size=30, hjust = 0.5, vjust = 1.7),
        legend.position = c(0.78, 0.70),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) 


# density plot for criterion e scores and directly worked or not

ptsdcriterione <- data.frame(ptsdcriterion$criterionE, ptsdcriterion$directlyworked_or_not) %>% na.omit()

meane <- aggregate(ptsdcriterion.criterionE ~ ptsdcriterion.directlyworked_or_not, ptsdcriterione, mean)

annot <- data.frame(
  x1 = c(5, 2.4),
  y1 = c(0.125, 0.125)
)

meane$x1 <- annot$x1
meane$y1 <- annot$y1

meane$ptsdcriterion.criterionE <- meane$ptsdcriterion.criterionE %>% round(2)

ggplot(data=ptsdcriterione, aes(x=ptsdcriterion.criterionE, group=ptsdcriterion.directlyworked_or_not, fill=ptsdcriterion.directlyworked_or_not)) +
  geom_density(adjust=1.5, alpha=.4) +
  labs(title = "PTSD - criterion E", x = '', y = 'Density') + 
  geom_vline(data = meane, aes(xintercept = ptsdcriterion.criterionE, color = ptsdcriterion.directlyworked_or_not), linetype = "dashed", size = 1) +
  geom_text(data = meane, aes(x = x1, y = y1, label = ptsdcriterion.criterionE, fontface = 2),
            hjust=0, size=8.5, face = 'bold') +
  theme_ipsum() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),axis.title.y = element_text(size=30, hjust = 0.5, vjust = 1.7),
        legend.position = c(0.78, 0.70),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) 


# chi sq test for individual ptsd symptoms

# data frame for chi sq individual symptoms 

chisqptsdindiv <- ptsdindiv

colnames(chisqptsdindiv) <- c("unwanted memories q1", "disturbing dreams q2", "experience happening again q3", "feeling very upset when reminded q4",
                              "having strong physical reaction when reminded q5", "avoiding memories q6", "avoiding external reminders q7",
                              "trouble remembering important parts q8", "having strong negative baliefs about self q9", "blaming self or someone else for what happened q10",
                              "strong negative feeling such as fear, horror, etc. q11", "loss of interest in activities q12", "feeling distant or cutoff from other people q13",
                              "trouble experienceing positive feelings q14", "irritable behavior q15", "taking too many risks q16", "being super alert q17", "feeling jumpy or easily starled q18",
                              "difficulty concentrating q19", "trouble falling or staying asleep q20", "directly worked with covid patients")

for(i in 1:ncol(chisqptsdindiv)){
  
  chisqptsdindiv[,i] <- as.factor(chisqptsdindiv[,i]) 
  
}


options(scipen = 999)
mapply(function(x, y) chisq.test(x, y)$p.value, chisqptsdindiv[, -ncol(chisqptsdindiv)], MoreArgs=list(chisqptsdindiv[, ncol(chisqptsdindiv)]))


# t test for different ptsd criterions

# checking sample size and variance betwwen groups before performing t-test

tapply(ptsdcriterionb$ptsdcriterion.criterionB, ptsdcriterionb$ptsdcriterion.directlyworked_or_not, var)
tapply(ptsdcriterionc$ptsdcriterion.criterionC, ptsdcriterionc$ptsdcriterion.directlyworked_or_not, var)
tapply(ptsdcriteriond$ptsdcriterion.criterionD, ptsdcriteriond$ptsdcriterion.directlyworked_or_not, var)
tapply(ptsdcriterione$ptsdcriterion.criterionE, ptsdcriterione$ptsdcriterion.directlyworked_or_not, var)

ptsdcriterionb %>% group_by(ptsdcriterion.directlyworked_or_not) %>% tally()
ptsdcriterionc %>% group_by(ptsdcriterion.directlyworked_or_not) %>% tally()
ptsdcriteriond %>% group_by(ptsdcriterion.directlyworked_or_not) %>% tally()
ptsdcriterione %>% group_by(ptsdcriterion.directlyworked_or_not) %>% tally()
# statistical tests between variables of interest and (ptsd final scores and criterion)

# criterion B
t.test(ptsdcriterionb$ptsdcriterion.criterionB ~ ptsdcriterionb$ptsdcriterion.directlyworked_or_not)
# criterion C
t.test(ptsdcriterionc$ptsdcriterion.criterionC ~ ptsdcriterionc$ptsdcriterion.directlyworked_or_not)
# criterion D
t.test(ptsdcriteriond$ptsdcriterion.criterionD ~ ptsdcriteriond$ptsdcriterion.directlyworked_or_not)
# criterion E
t.test(ptsdcriterione$ptsdcriterion.criterionE ~ ptsdcriterione$ptsdcriterion.directlyworked_or_not)

# directly worked with covid patients or not with PTSD final score
t.test(renamed$ptsdfinalscore ~ renamed$Direcly_worked_COVID_patients_2.1)

# time worked and ptsd final score correlation
cor(monthsworked_filtered$renamed.Months_worked_with_COVID_pat_2.1.1, monthsworked_filtered$renamed.ptsdfinalscore)

# time worked and different criterion

# criterion B
cor(monthsworked_filtered$renamed.Months_worked_with_COVID_pat_2.1.1, monthsworked_filtered$criterionB)
# criterion C
cor(monthsworked_filtered$renamed.Months_worked_with_COVID_pat_2.1.1, monthsworked_filtered$criterionC)
# criterion D
cor(monthsworked_filtered$renamed.Months_worked_with_COVID_pat_2.1.1, monthsworked_filtered$criterionD)
# criterion E
cor(monthsworked_filtered$renamed.Months_worked_with_COVID_pat_2.1.1, monthsworked_filtered$criterionE)


# underlying health condition vs ptsd final score, ptsd criterion B, C, D, E

meanfinalptsd <- aggregate(renamed.ptsdfinalscore ~ renamed.underlyinh_health_condition_2.2, underlyinghc, mean)

annot <- data.frame(
  x1 = c(6.25, 12.99, 14.95),
  y1 = c(0.051, 0.041, 0.051)
)

meanfinalptsd$x1 <- annot$x1
meanfinalptsd$y1 <- annot$y1

meanfinalptsd$renamed.ptsdfinalscore <- meanfinalptsd$renamed.ptsdfinalscore %>% round(2)


ggplot(data=underlyinghc, aes(x=renamed.ptsdfinalscore, group=renamed.underlyinh_health_condition_2.2, fill=renamed.underlyinh_health_condition_2.2)) +
  geom_density(adjust=1.5, alpha=.4) +
  labs(title = "PTSD (Final score) - Underlying health condition", x = '', y = 'Density') + 
  geom_vline(data = meanfinalptsd, aes(xintercept = renamed.ptsdfinalscore, color = renamed.underlyinh_health_condition_2.2), linetype = "dashed", size = 1) +
  geom_text(data = meanfinalptsd, aes(x = x1, y = y1, label = renamed.ptsdfinalscore, fontface = 2),
            hjust=0, size=8.5, face = 'bold') +
  theme_ipsum() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),axis.title.y = element_text(size=30, hjust = 0.5, vjust = 1.7),
        legend.position = c(0.78, 0.70),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) 


# PTSD criterion B

indivgroupdensityplot(underlyinghc, "criterionB", "renamed.underlyinh_health_condition_2.2",
                      c(1.8,1.5,3), c(0.17, 0.21, 0.21), "PTSD (Criterion B) - Underlying health condition"  )

# PTSD criterion C
indivgroupdensityplot(underlyinghc, "criterionC", "renamed.underlyinh_health_condition_2.2",
                      c(0.89,0.6,1.3), c(0.37, 0.51, 0.51), "PTSD (Criterion C) - Underlying health condition"  )


# PTSD criterion D

indivgroupdensityplot(underlyinghc, "criterionD", "renamed.underlyinh_health_condition_2.2",
                      c(4.7,3,5.3), c(0.107, 0.131, 0.131), "PTSD (Criterion D) - Underlying health condition"  )


# PTSD criterion E

indivgroupdensityplot(underlyinghc, "criterionE", "renamed.underlyinh_health_condition_2.2",
                      c(4.5,2.7,5.1), c(0.107, 0.131, 0.131), "PTSD (Criterion E) - Underlying health condition"  )


# ptsd final score

aov((log(0.00000001+underlyinghc$renamed.ptsdfinalscore)) ~ underlyinghc$renamed.underlyinh_health_condition_2.2) %>% plot()

aov(underlyinghc$renamed.ptsdfinalscore ~ underlyinghc$renamed.underlyinh_health_condition_2.2) %>% TukeyHSD() 

# ptsd criterion 

# criterion B

aov(underlyinghc$criterionB ~ underlyinghc$renamed.underlyinh_health_condition_2.2) %>% summary()

aov(underlyinghc$criterionB ~ underlyinghc$renamed.underlyinh_health_condition_2.2) %>% TukeyHSD()

# criterion C

aov(underlyinghc$criterionD ~ underlyinghc$renamed.underlyinh_health_condition_2.2) %>% summary()

aov(underlyinghc$criterionD ~ underlyinghc$renamed.underlyinh_health_condition_2.2) %>% TukeyHSD() 

# criterion D

aov(underlyinghc$criterionD ~ underlyinghc$renamed.underlyinh_health_condition_2.2) %>% summary()

aov(underlyinghc$criterionD ~ underlyinghc$renamed.underlyinh_health_condition_2.2) %>% TukeyHSD() 

# criterion E

aov(underlyinghc$criterionE ~ underlyinghc$renamed.underlyinh_health_condition_2.2) %>% summary()

aov(underlyinghc$criterionE ~ underlyinghc$renamed.underlyinh_health_condition_2.2) %>% TukeyHSD() 


# covid fears with respect to ptsd final score and different criterions

# final score

indivgroupdensityplot(covidfears, "renamed.ptsdfinalscore", "renamed.Demo_fear_cont_COVID_2.4.1",
                      c(20.5,2,18.5,12,1), c(0.085,0.077,0.06,0.07,0.085), "PTSD (Final score) - COVID fear"  )

# criterion B
indivgroupdensityplot(covidfears, "criterionB", "renamed.Demo_fear_cont_COVID_2.4.1",
                      c(20.5,2,18.5,12,1), c(0.085,0.077,0.06,0.07,0.085), "PTSD (criterion B) - COVID fear"  )


# criterion C
indivgroupdensityplot(covidfears, "criterionC", "renamed.Demo_fear_cont_COVID_2.4.1",
                      c(20.5,2,18.5,12,1), c(0.085,0.077,0.06,0.07,0.085), "PTSD (criterion C) - COVID fear"  )


# criterion D
indivgroupdensityplot(covidfears, "criterionD", "renamed.Demo_fear_cont_COVID_2.4.1",
                      c(20.5,2,18.5,12,1), c(0.085,0.077,0.06,0.07,0.085), "PTSD (criterion D) - COVID fear"  )

# criterion E
indivgroupdensityplot(covidfears, "criterionE", "renamed.Demo_fear_cont_COVID_2.4.1",
                      c(20.5,2,18.5,12,1), c(0.085,0.077,0.06,0.07,0.085), "PTSD (criterion E) - COVID fear"  )



############################################################## table required by prof. Mukherjee for ptsd #########################################################

# creating table for demographics

renamed <- within(renamed,sex_5.2 <- factor(sex_5.2, levels=names(sort(table(sex_5.2),decreasing=T))))
renamed <- within(renamed,Immigrant_5.5 <- factor(Immigrant_5.5, levels=names(sort(table(Immigrant_5.5),decreasing=T))))
renamed <- within(renamed,Country_5.5.1 <- factor(Country_5.5.1, levels=names(sort(table(Country_5.5.1),decreasing=T))))
renamed <- within(renamed,Relationship_5.6 <- factor(Relationship_5.6, levels=names(sort(table(Relationship_5.6),decreasing=T))))
renamed <- within(renamed,State_of_practice_5.9 <- factor(State_of_practice_5.9, levels=names(sort(table(State_of_practice_5.9),decreasing=T))))
renamed <- within(renamed,Primary_work_setting_5.11 <- factor(Primary_work_setting_5.11, levels=names(sort(table(Primary_work_setting_5.11),decreasing=T))))
renamed <- within(renamed,Working_status_5.12 <- factor(Working_status_5.12, levels=names(sort(table(Working_status_5.12),decreasing=T))))
renamed <- within(renamed,Furloughed_5.12.2 <- factor(Furloughed_5.12.2, levels=names(sort(table(Furloughed_5.12.2),decreasing=T))))
renamed <- within(renamed,Laid_off_5.12.3 <- factor(Laid_off_5.12.3, levels=names(sort(table(Laid_off_5.12.3),decreasing=T))))
renamed <- within(renamed,Leave_due_to_COVID_5.12.14 <- factor(Leave_due_to_COVID_5.12.14, levels=names(sort(table(Leave_due_to_COVID_5.12.14),decreasing=T))))

renamed <- within(renamed, Primary_work_setting_5.11 <- factor(Primary_work_setting_5.11, levels = c("Group practice","Hospital","Academic medical center",
                                                    "Solo practice","Outpatient center","Two-physician practice" ,"Others")))

label(renamed$Age) <- "Age"
units(renamed$Age) <- "years"
label(renamed$sex_5.2) <- "Sex"
label(renamed$Immigrant_5.5) <- "Immigrant"
label(renamed$Country_5.5.1) <- "Country of origin"
label(renamed$Relationship_5.6) <- "Relationship status" 
label(renamed$Years_practicing_5.8) <- "Years in practice"
units(renamed$Years_practicing_5.8) <- "years"
label(renamed$State_of_practice_5.9) <- "State of practice"
label(renamed$Primary_work_setting_5.11) <- "Work setting"
label(renamed$Working_status_5.12) <- "Working status"
label(renamed$Furloughed_5.12.2) <- "Furlough status"
label(renamed$POP_Black_5.13.1) <- "Patient population - Black or African American"
units(renamed$POP_Black_5.13.1) <- "Percent"
label(renamed$POP_Non_Hisp_white_5.13.2) <- "Patient population - Non-Hispanic White"
units(renamed$POP_Non_Hisp_white_5.13.2) <- "Percent"
label(renamed$POP_Hispanic_latino_5.13.3) <- "Patient population - Hispanic or Latino"
units(renamed$POP_Hispanic_latino_5.13.3) <- "Percent"
label(renamed$POP_Asian_5.13.4) <- "Patient population - Asian"
units(renamed$POP_Asian_5.13.4) <- "Percent"
label(renamed$POP_American_indian_5.13.5) <- "Patient population - American Indian or Alaska Native"
units(renamed$POP_American_indian_5.13.5) <- "Percent"
label(renamed$POP_Native_hawaiian_5.13.6) <- "Patient population - Native Hawaiian or Pacific Islander"
units(renamed$POP_Native_hawaiian_5.13.6) <- "Percent"

table1::table1(~ Age + sex_5.2 + Immigrant_5.5 + Relationship_5.6 +
                 Years_practicing_5.8  + Primary_work_setting_5.11 + Working_status_5.12 +
                 Furloughed_5.12.2 + POP_Black_5.13.1 + 
                 POP_Non_Hisp_white_5.13.2 + POP_Hispanic_latino_5.13.3 + POP_Asian_5.13.4 + 
                 POP_American_indian_5.13.5 + POP_Native_hawaiian_5.13.6 + 
                 + State_of_practice_5.9 + Country_5.5.1| Direcly_worked_COVID_patients_2.1, data = renamed)


# table for onehot encoded race

racewithid <- within(racewithid,value <- factor(value, levels=names(sort(table(value),decreasing=T))))
racewithid <- within(racewithid,value <- factor(value, levels=c("White", "Asian", "Black or African American",
                                                  "American Indian or Alaska Native", "Native Hawaiian or Pacific Islander", "Others")))

label(racewithid$value) <- "Race"

table1::table1(~ value | Direcly_worked_COVID_patients_2.1, data = racewithid)

# table for onehot encoded speciality

specialitywithid$value <- specialitywithid$value %>% as.factor() 
specialitywithid <- within(specialitywithid,value <- factor(value, levels= value %>% levels() %>% sort()))
specialitywithid <- within(specialitywithid,value <- factor(value, levels= c("Adult Critical Care","Anesthesiology","Cardiology",
                                                         "Dermatology","Electrophysiology","Emergency Medicine",                         
                                                         "Endocrinology","Family Medicine","Gastroenterologist",                         
                                                         "General Surgery","Geriatrics","Immunology",                                 
                                                         "Infectious diseases","Internal Medicine","Interventional Radiology",                   
                                                         "Medical Genetics","Neonatal Critical Care","Neurology",                                  
                                                         "Neurosurgery","Obstetrics and Gynecology","Oncology",                                   
                                                         "Ophthalmology","Orthopedic surgery","Orthopedics",                                
                                                         "Otolaryngology","Pathology","Pediatric Critical Care",                    
                                                         "Pediatric Emergency Medicine","Pediatrics","Physical Medicine and Rehabilitation",       
                                                         "Podiatry","Preventive medicine / occupational medicine","Psychiatry",                                 
                                                         "Pulmonology","Radiation oncology","Radiology",                                  
                                                         "Rheumatology","Urology","Vascular Surgery", "Others")))


label(specialitywithid$value) <- "Physician speciality"

table1::table1(~ value | Direcly_worked_COVID_patients_2.1, data = specialitywithid)

# table for onehot encoded hospital type

hospitaltypewithid <- within(hospitaltypewithid,value <- factor(value, levels=names(sort(table(value),decreasing=T))))

hospitaltypewithid <- within(hospitaltypewithid,value <- factor(value, levels= c( "Medium Hospital (100-500 beds)","Community (Non-federal Acute Care) Hospital",                 
                                                            "Urban Hospital","Suburban Hospital",                                           
                                                            "Teaching / Research Hospital","Large hospitals (>500 beds)",                                 
                                                            "Critical Access Hospital","Rural Hospital",                                              
                                                            "Small Hospital (<100 beds)","Veterans or Federal Hospital",                                
                                                            "Specialty (e.g., Cancer Hospital, Children's Hospital, etc.)", "Non-federal Psychiatric Care",                                
                                                            "Rehabilitation Hospital", "Others")))


label(hospitaltypewithid$value) <- "Hospital type"

table1::table1(~ value | Direcly_worked_COVID_patients_2.1, data = hospitaltypewithid)

# table for work setting within hospital 

worksettingwithinhospitalwithid <- within(worksettingwithinhospitalwithid,value <- factor(value, levels=names(sort(table(value),decreasing=T))))

worksettingwithinhospitalwithid <- within(worksettingwithinhospitalwithid,value <- factor(value, levels=c("Emergency Department","General Floor","Intensive Care Unit (any type)",
                                                                       "Operating Room","Step Down Unit", "Others")))


label(worksettingwithinhospitalwithid$value) <- "Work setting of physicians within the hospitals"

table1::table1(~ value | Direcly_worked_COVID_patients_2.1, data = worksettingwithinhospitalwithid)

###############################################################################################################################################
###############################################################################################################################################
############################################################## ptsd analysis ##################################################################
###############################################################################################################################################
###############################################################################################################################################

##############################################################################################################################################
########## filtering in rename itself for directly worked with COVID patients ################################################################
# Do not run this piece of code if you don't want to produce the corresponding plots as it will replace the original data frame #########################
# Perform all data wrangling before running this code as this will replace the main dataframe with filtered one ##############################
##############################################################################################################################################

renamed <- renamed %>% filter(Direcly_worked_COVID_patients_2.1 == "Directly worked")

# ptsd distribution

ggplot(na.omit(renamed),  aes(x=na.omit(factor(renamed$ptsd_scaled, levels = c( 'No',  "Pre-subclinical",  "sub-clinical", 'Probable'))) )) +  
  geom_bar(aes(y = (..count..)), width = 0.6) + 
  geom_text(aes(y = (..count..),label = ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",
            hjust = -0.2, size = 9.8) +
  scale_y_continuous( expand = expansion(mult = c(0, .1)), limits = c(0,430)) +
  labs(title = "PTSD", y = "", x = "") + theme_classic()  +
  theme(plot.title = element_text(size = 26,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=29, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# ptsd worked with covid and not worked with covid

ptsd_wkwith_covid <- data.frame(renamed$Direcly_worked_COVID_patients_2.1, renamed$ptsd_scaled) %>% na.omit()

# V1

ggplot(na.omit(ptsd_wkwith_covid),  aes(x =  na.omit(factor(ptsd_wkwith_covid$renamed.ptsd_scaled)),
                                               fill=na.omit(factor(ptsd_wkwith_covid$renamed.Direcly_worked_COVID_patients_2.1)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..])),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD for Physicians directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,450)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# V2

ggplot(na.omit(ptsd_wkwith_covid),  aes(fill =  na.omit(factor(ptsd_wkwith_covid$renamed.ptsd_scaled)),
                                               x=na.omit(factor(ptsd_wkwith_covid$renamed.Direcly_worked_COVID_patients_2.1)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..])),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD for Physicians directly working with COVID patients", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,450)) +  
  theme_classic() + 
  scale_x_discrete(labels = c("Directly\nworked", "Not directly\nworked")) +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# PTSD and Age violin plot

ptsd_age <- data.frame(renamed$Age, renamed$ptsd_scaled) %>% na.omit()

DataMean <- ptsd_age %>% 
  group_by(renamed.ptsd_scaled) %>%
  dplyr::summarize(mean = mean(renamed.Age))

ggplot(ptsd_age, aes(x=renamed.ptsd_scaled, y=renamed.Age)) + 
  geom_violin(trim=F, fill='#A4A4A4') +
  geom_boxplot(width=0.1, fill="white") + stat_summary(fun=mean, geom="crossbar", width=0.2, size=0.3, color="red") +
  geom_text(data = DataMean, aes(x = c(1.1,2.1,3.1,4.1), y = c(85,80,80,80), label = round(mean,2), fontface = 2),
            hjust=0, size=8.5) +
  labs(title="PTSD and Age (mean)",x="", y='') +
theme_classic()  +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30, face = 'bold'),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) 


# PTSD and Sex

ptsd_sex <- data.frame(renamed$sex_5.2, renamed$ptsd_scaled) %>% na.omit()

ptsd_sex <- filter(ptsd_sex, renamed.sex_5.2 == "Male" | renamed.sex_5.2 == "Female")

# V1

ggplot(na.omit(ptsd_sex),  aes(x =  na.omit(factor(ptsd_sex$renamed.ptsd_scaled)),
                                        fill=na.omit(factor(ptsd_sex$renamed.sex_5.2)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..])),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD and sex", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,220)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# V2

ggplot(na.omit(ptsd_sex),  aes(fill =  na.omit(factor(ptsd_sex$renamed.ptsd_scaled)),
                               x = na.omit(factor(ptsd_sex$renamed.sex_5.2)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..])),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD and sex", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,220)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.88, 0.85),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# PTSD and months worked with COVID

ptsd_months_covid <- data.frame(renamed$Months_worked_with_COVID_pat_2.1.1, renamed$ptsd_scaled) %>% na.omit()


DataMean <- ptsd_months_covid %>% 
  group_by(renamed.ptsd_scaled) %>%
  dplyr::summarize(mean = mean(renamed.Months_worked_with_COVID_pat_2.1.1))

ggplot(ptsd_months_covid, aes(x=renamed.ptsd_scaled, y=renamed.Months_worked_with_COVID_pat_2.1.1)) + 
  geom_violin(trim=F, fill='#A4A4A4') +
  geom_boxplot(width=0.1, fill="white") + stat_summary(fun=mean, geom="crossbar", width=0.2, size=0.3, color="red") +
  geom_text(data = DataMean, aes(x = c(1.1,2.1,3.1,4.1), y = mean+5, label = round(mean,2), fontface = 2),
            hjust=0, size=8.5) +
  labs(title="PTSD and Months worked with COVID patients (mean)",x="", y='') +
  theme_classic()  +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30, face = 'bold'),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28))


# PTSD and part of decision making process

ptsd_part_of_decision <- data.frame(renamed$ptsd_scaled, renamed$Part_of_decision_making_2.11) %>% na.omit()

# V1

ggplot(na.omit(ptsd_part_of_decision),  aes(x =  na.omit(factor(renamed.ptsd_scaled)),
                               fill=na.omit(factor(renamed.Part_of_decision_making_2.11, levels = c("Always",
                                                                                                    "Most of the time",
                                                                                                    "About half the time",
                                                                                                    "Sometimes",
                                                                                                    "Never",
                                                                                                    "Not applicable (N/A)"))))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..])),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 6.8) +
  labs(title = "PTSD and Part of decision making", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,180)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# V2

ggplot(na.omit(ptsd_part_of_decision),  aes(fill =  na.omit(factor(renamed.ptsd_scaled)),
                                            x = na.omit(factor(renamed.Part_of_decision_making_2.11, levels = c("Not applicable (N/A)",
                                                                                                                "Never",
                                                                                                                "Sometimes",
                                                                                                                "About half the time",
                                                                                                                "Most of the time",
                                                                                                                "Always"
                                                                                                                 ))))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..])),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 6.8) +
  labs(title = "PTSD and part of decision making", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,180)) +
  scale_x_discrete(labels = c("Not\napplicable (N/A)",
                              "Never",
                              "Sometimes",
                              "About\nhalf the time",
                              "Most of the time",
                              "Always")) + 
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.58, 0.35),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# PTSD and changed living arrangements

ptsd_living_changes <- data.frame(renamed$living_changes_cooncern_covid_2.5, renamed$ptsd_scaled) %>% na.omit()

# V1

ggplot(na.omit(ptsd_living_changes),  aes(x =  na.omit(factor(renamed.ptsd_scaled)),
                                            fill=na.omit(factor(renamed.living_changes_cooncern_covid_2.5)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..])),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD and living changes", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,250)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# V2

ggplot(na.omit(ptsd_living_changes),  aes(fill =  na.omit(factor(renamed.ptsd_scaled)),
                                          x = na.omit(factor(renamed.living_changes_cooncern_covid_2.5)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..])),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 9.8) +
  labs(title = "PTSD and living changes", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,250)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.85),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# PTSD and Felt stigmatized

ptsd_stigmatized <- data.frame(renamed$Demo_felt_stigmatized_2.4.3, renamed$ptsd_scaled) %>% na.omit()

ptsd_stigmatized$renamed.Demo_felt_stigmatized_2.4.3 <- factor(ptsd_stigmatized$renamed.Demo_felt_stigmatized_2.4.3, c("None at all", "A little", "A moderate amount", "A lot", "A great deal")) 

# V1

ggplot(na.omit(ptsd_stigmatized),  aes(x =  na.omit(factor(renamed.ptsd_scaled)),
                                          fill=na.omit(factor(ptsd_stigmatized$renamed.Demo_felt_stigmatized_2.4.3)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 7.8) +
  labs(title = "PTSD and Felt stigmatized", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,220)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# V2

ggplot(na.omit(ptsd_stigmatized),  aes(fill =  na.omit(factor(renamed.ptsd_scaled)),
                                       x = na.omit(factor(ptsd_stigmatized$renamed.Demo_felt_stigmatized_2.4.3)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 7.8) +
  labs(title = "PTSD and Felt stigmatized", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,220)) +
  scale_x_discrete(labels = c("None\nat all",
                              "little",
                              "moderate\namount",
                              "A lot",
                              "A great\ndeal",
                              "Not\napplicable (N/A)")) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# PTSD and relationship status

ptsd_relationship <- data.frame(renamed$ptsd_scaled, renamed$Relationship_5.6) %>% na.omit() 

# V1

ggplot(na.omit(ptsd_relationship),  aes(x =  na.omit(factor(renamed.ptsd_scaled)),
                                       fill=na.omit(factor(renamed.Relationship_5.6)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 7.8) +
  labs(title = "PTSD and relationship status", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,330)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# V2

ggplot(na.omit(ptsd_relationship),  aes(fill =  na.omit(factor(renamed.ptsd_scaled)),
                                        x = na.omit(factor(renamed.Relationship_5.6)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 7.8) +
  labs(title = "PTSD and relationship status", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,330)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# PTSD and emotional support

ptsd_emotional_special_person <- data.frame(renamed$supp_special_person_4.3.1, renamed$ptsd_scaled) %>% na.omit()

ptsd_emotional_special_person$renamed.supp_special_person_4.3.1 <- factor(ptsd_emotional_special_person$renamed.supp_special_person_4.3.1, levels = c( "Very strongly disagree", "Strongly disagree", "Mildly disagree",
                                                                                    "Neutral", "Mildly agree", "Strongly agree", "Very strongly agree"))


# V1

ggplot(na.omit(ptsd_emotional_special_person),  aes(x =  na.omit(factor(renamed.ptsd_scaled)),
                                        fill=na.omit(factor(renamed.supp_special_person_4.3.1)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and emotional support (special person)", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,260)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# V2

ggplot(na.omit(ptsd_emotional_special_person),  aes(fill =  na.omit(factor(renamed.ptsd_scaled)),
                                                    x = na.omit(factor(renamed.supp_special_person_4.3.1)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and emotional support (special person)", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,260)) +
  scale_x_discrete(labels = c("Very strongly\ndisagree",
                              "Strongly\ndisagree",
                              "Mildly\ndisagree",
                              "Neutral",
                              "Mildly\nagree",
                              "strongly\nagree",
                              "Very strongly\nagree"))+  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


ptsd_emotional_ct_on_frnds <- data.frame(renamed$supp_count_on_friends_4.3.2, renamed$ptsd_scaled) %>% na.omit()


ptsd_emotional_ct_on_frnds$renamed.supp_count_on_friends_4.3.2 <- factor(ptsd_emotional_ct_on_frnds$renamed.supp_count_on_friends_4.3.2, levels = c( "Very strongly disagree", "Strongly disagree", "Mildly disagree",
                                                                                                                                                       "Neutral", "Mildly agree", "Strongly agree", "Very strongly agree"))

# V1

ggplot(na.omit(ptsd_emotional_ct_on_frnds),  aes(x =  na.omit(factor(renamed.ptsd_scaled)),
                                                    fill=na.omit(factor(renamed.supp_count_on_friends_4.3.2)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and emotional support (count on friends)", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,160)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# V2

ggplot(na.omit(ptsd_emotional_ct_on_frnds),  aes(fill =  na.omit(factor(renamed.ptsd_scaled)),
                                                    x = na.omit(factor(renamed.supp_count_on_friends_4.3.2)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and emotional support (count on friends)", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,160)) +
  scale_x_discrete(labels = c("Very strongly\ndisagree",
                              "Strongly\ndisagree",
                              "Mildly\ndisagree",
                              "Neutral",
                              "Mildly\nagree",
                              "strongly\nagree",
                              "Very strongly\nagree"))+  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


ptsd_emotional_supp_frm_family <- data.frame(renamed$supp_emotional_help_4.3.3, renamed$ptsd_scaled) %>% na.omit()


ptsd_emotional_supp_frm_family$renamed.supp_emotional_help_4.3.3 <- factor(ptsd_emotional_supp_frm_family$renamed.supp_emotional_help_4.3.3, levels = c( "Very strongly disagree", "Strongly disagree", "Mildly disagree",
                                                                                                                                                     "Neutral", "Mildly agree", "Strongly agree", "Very strongly agree"))


# V1

ggplot(na.omit(ptsd_emotional_supp_frm_family),  aes(x =  na.omit(factor(renamed.ptsd_scaled)),
                                                 fill=na.omit(factor(renamed.supp_emotional_help_4.3.3)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and emotional support (family)", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,200)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# V2

ggplot(na.omit(ptsd_emotional_supp_frm_family),  aes(fill =  na.omit(factor(renamed.ptsd_scaled)),
                                                 x = na.omit(factor(renamed.supp_emotional_help_4.3.3)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and emotional support (family)", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,200)) +
  scale_x_discrete(labels = c("Very strongly\ndisagree",
                              "Strongly\ndisagree",
                              "Mildly\ndisagree",
                              "Neutral",
                              "Mildly\nagree",
                              "strongly\nagree",
                              "Very strongly\nagree"))+  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# PTSd and years practicing

ptsd_years_practicing <- data.frame(renamed$Years_practicing_5.8, renamed$ptsd_scaled) %>% na.omit()

DataMean <- ptsd_years_practicing %>% 
  group_by(renamed.ptsd_scaled) %>%
  dplyr::summarize(mean = mean(renamed.Years_practicing_5.8))

ggplot(ptsd_years_practicing, aes(x=renamed.ptsd_scaled, y = renamed.Years_practicing_5.8)) + 
  geom_violin(trim=F, fill='#A4A4A4') +
  geom_boxplot(width=0.1, fill="white") + stat_summary(fun=mean, geom="crossbar", width=0.2, size=0.3, color="red") +
  geom_text(data = DataMean, aes(x = c(1.1,2.1,3.1,4.1), y = mean+35, label = round(mean,2), fontface = 2),
            hjust=0, size=8.5) +
  labs(title="PTSD and years practicing (mean)",x="", y='') +
  theme_classic()  +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30, face = 'bold'),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28))


# PTSD and working extra hours

ptsd_work_extra <- data.frame(renamed$work_extra_hours_2.7, renamed$ptsd_scaled) %>% na.omit()


# V1

ggplot(na.omit(ptsd_work_extra),  aes(x =  na.omit(factor(renamed.ptsd_scaled)),
                                                     fill=na.omit(factor(renamed.work_extra_hours_2.7)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and work extra hours", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,300)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)



# V2

ggplot(na.omit(ptsd_work_extra),  aes(fill =  na.omit(factor(renamed.ptsd_scaled)),
                                      x = na.omit(factor(renamed.work_extra_hours_2.7)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and work extra hours", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,300)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)



# PTSD and average working hours during peak COVID week

ptsd_ave_week_peak <- data.frame(renamed$Hours_worked_peak_month_2.7.1, renamed$ptsd_scaled) %>% na.omit()

DataMean <- ptsd_ave_week_peak %>% 
  group_by(renamed.ptsd_scaled) %>%
  dplyr::summarize(mean = mean(renamed.Hours_worked_peak_month_2.7.1))

ggplot(ptsd_ave_week_peak, aes(x=renamed.ptsd_scaled, y = renamed.Hours_worked_peak_month_2.7.1)) + 
  geom_violin(trim=F, fill='#A4A4A4') +
  geom_boxplot(width=0.1, fill="white") + stat_summary(fun=mean, geom="crossbar", width=0.2, size=0.3, color="red") +
  geom_text(data = DataMean, aes(x = c(1.1,2.1,3.1,4.1), y = mean+50, label = round(mean,2), fontface = 2),
            hjust=0, size=8.5) +
  labs(title="PTSD and average working hours during peak COVID19 (mean)",x="", y='') +
  theme_classic()  +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30, face = 'bold'),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28))



# PTSD and working outside normal responsibilities

ptsd_novel_resp <- data.frame(renamed$work_outside_normal_resp_2.8, renamed$ptsd_scaled) %>% na.omit()

# V1

ggplot(na.omit(ptsd_novel_resp),  aes(x =  na.omit(factor(renamed.ptsd_scaled)),
                                      fill=na.omit(factor(renamed.work_outside_normal_resp_2.8)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and Working outside normal scope of responsibilities", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,310)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# V2

ggplot(na.omit(ptsd_novel_resp),  aes(fill =  na.omit(factor(renamed.ptsd_scaled)),
                                      x = na.omit(factor(renamed.work_outside_normal_resp_2.8)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and Working outside normal scope of responsibilities", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,310)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# PTSD and hours worked outside normal responsibilities

ptsd_hours_outside_norm <- data.frame(renamed$Hours_doing_outside_resp_2.8.1, renamed$ptsd_scaled) %>% na.omit()

DataMean <- ptsd_hours_outside_norm %>% 
  group_by(renamed.ptsd_scaled) %>%
  dplyr::summarize(mean = mean(renamed.Hours_doing_outside_resp_2.8.1))

ggplot(ptsd_hours_outside_norm, aes(x=renamed.ptsd_scaled, y = renamed.Hours_doing_outside_resp_2.8.1)) + 
  geom_violin(trim=F, fill='#A4A4A4') +
  geom_boxplot(width=0.1, fill="white") + stat_summary(fun=mean, geom="crossbar", width=0.2, size=0.3, color="red") +
  geom_text(data = DataMean, aes(x = c(1.1,2.1,3.1,4.1), y = mean+80, label = round(mean,2), fontface = 2),
            hjust=0, size=8.5) +
  labs(title="PTSD and average hours worked outside normal responsibilities (mean)",x="", y='') +
  theme_classic()  +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30, face = 'bold'),legend.position = c(0.68, 0.55))


# PTSD and increased job difficulty 

ptsd_job_diff <- data.frame(renamed$job_difficult_dueto_COVID_2.9, renamed$ptsd_scaled)

ptsd_job_diff$renamed.job_difficult_dueto_COVID_2.9 <- factor(ptsd_job_diff$renamed.job_difficult_dueto_COVID_2.9, levels = c("None at all", "A little bit more", "Moderately more", "A lot more", "Extremely more"))

# V1

ggplot(na.omit(ptsd_job_diff),  aes(x =  na.omit(factor(renamed.ptsd_scaled)),
                                      fill=na.omit(factor(renamed.job_difficult_dueto_COVID_2.9)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and increased job difficulty", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,155)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# V2

ggplot(na.omit(ptsd_job_diff),  aes(fill =  na.omit(factor(renamed.ptsd_scaled)),
                                    x = na.omit(factor(renamed.job_difficult_dueto_COVID_2.9)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and increased job difficulty", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,155)) +  
  scale_x_discrete(labels = c("None at\nall",
                              "A little bit\nmore",
                              "Moderately\nmore",
                              "A lot\nmore",
                              "Extremely\nmore")) +
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.85),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# PTSD and sufficient resources

ptsd_suff_resources <- data.frame(renamed$Sufficient_resources_2.10, renamed$ptsd_scaled) %>% na.omit()

ptsd_suff_resources$renamed.Sufficient_resources_2.10 <- factor(ptsd_suff_resources$renamed.Sufficient_resources_2.10, levels = c("Not applicable (N/A)", "Never", "Sometimes",
                                                                                                                                  "About half the time", "Most of the time", "Always"))

# V1

ggplot(na.omit(ptsd_suff_resources),  aes(x =  na.omit(factor(renamed.ptsd_scaled)),
                                    fill=na.omit(factor(renamed.Sufficient_resources_2.10)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and sufficient resources availability", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,220)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# V2

ggplot(na.omit(ptsd_suff_resources),  aes(fill =  na.omit(factor(renamed.ptsd_scaled)),
                                          x = na.omit(factor(renamed.Sufficient_resources_2.10)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and sufficient resources availability", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,220)) +  
  scale_x_discrete(labels = c("Not applicable",
                              "Never",
                              "Sometimes",
                              "About half\nthe time",
                              "Most of the\ntime",
                              "Always")) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# PTSD and organizational support - 1

ptsd_org_supp_1 <- data.frame(renamed$org_supp_even_wrkld_4.1.1, renamed$ptsd_scaled) %>% na.omit()

ptsd_org_supp_1$renamed.org_supp_even_wrkld_4.1.1 <- factor(ptsd_org_supp_1$renamed.org_supp_even_wrkld_4.1.1, levels = c("Not applicable (N/A)", "Not sure", "No", "Yes")) 

# V1

ggplot(na.omit(ptsd_org_supp_1),  aes(x =  na.omit(factor(renamed.ptsd_scaled)),
                                          fill=na.omit(factor(renamed.org_supp_even_wrkld_4.1.1)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and organizational support - evenly distribute workload", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,200)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# V2

ggplot(na.omit(ptsd_org_supp_1),  aes(fill =  na.omit(factor(renamed.ptsd_scaled)),
                                      x = na.omit(factor(renamed.org_supp_even_wrkld_4.1.1)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and organizational support - evenly distribute workload", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,200)) +  
  scale_x_discrete(labels = c("Not applicable",
                              "Not sure",
                              "No",
                              "Yes")) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# PTSD and organizational support - 2

ptsd_org_supp_2 <- data.frame(renamed$org_supp_psych_hotline_4.1.2, renamed$ptsd_scaled) %>% na.omit()

ptsd_org_supp_2$renamed.org_supp_psych_hotline_4.1.2 <- factor(ptsd_org_supp_2$renamed.org_supp_psych_hotline_4.1.2, levels = c("Not applicable (N/A)", "Not sure", "No", "Yes")) 

# V1

ggplot(na.omit(ptsd_org_supp_2),  aes(x =  na.omit(factor(renamed.ptsd_scaled)),
                                      fill=na.omit(factor(renamed.org_supp_psych_hotline_4.1.2)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and organizational support - psychological support hotline", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,205)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# V2

ggplot(na.omit(ptsd_org_supp_2),  aes(fill =  na.omit(factor(renamed.ptsd_scaled)),
                                      x = na.omit(factor(renamed.org_supp_psych_hotline_4.1.2)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and organizational support - psychological support hotline", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,205)) +  
  scale_x_discrete(labels = c("Not applicable",
                              "Not sure",
                              "No",
                              "Yes")) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# PTSD and organizational support - 3

ptsd_org_supp_3 <- data.frame(renamed$org_supp_training_4.1.3, renamed$ptsd_scaled) %>% na.omit()

ptsd_org_supp_3$renamed.org_supp_training_4.1.3 <- factor(ptsd_org_supp_3$renamed.org_supp_training_4.1.3, levels = c("Not applicable (N/A)", "Not sure", "No", "Yes")) 

# V1

ggplot(na.omit(ptsd_org_supp_3),  aes(x =  na.omit(factor(renamed.ptsd_scaled)),
                                      fill=na.omit(factor(ptsd_org_supp_3$renamed.org_supp_training_4.1.3)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and organizational support - offer online training", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,160)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# V2

ggplot(na.omit(ptsd_org_supp_3),  aes(fill =  na.omit(factor(renamed.ptsd_scaled)),
                                      x = na.omit(factor(renamed.org_supp_training_4.1.3)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and organizational support - offer online training", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,160)) +  
  scale_x_discrete(labels = c("Not applicable",
                              "Not sure",
                              "No",
                              "Yes")) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.25),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# PTSD and depression (categories)

ptsd_depression <- data.frame(renamed$depression_final_scaled, renamed$ptsd_scaled) %>% na.omit()

# V1

ggplot(na.omit(ptsd_depression),  aes(x =  na.omit(factor(renamed.ptsd_scaled)),
                                      fill=na.omit(factor(renamed.depression_final_scaled)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and Depression categories", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,300)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# V2

ggplot(na.omit(ptsd_depression),  aes(fill =  na.omit(factor(renamed.ptsd_scaled)),
                                      x = na.omit(factor(renamed.depression_final_scaled)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and Depression categories", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,300)) + 
  scale_x_discrete(labels = c('None-\nMinimal\ndepression',
                              'Mild\ndepression',
                              'Moderate\ndepression',
                              "Moderately\nSevere\ndepression",
                              "Severe\ndepression"
                              ))+  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# PTSD and depression (scores)

ptsd_depression_scores <- data.frame(renamed$depression_final_score, renamed$ptsd_scaled) %>% na.omit()


DataMean <- ptsd_depression_scores %>% 
  group_by(renamed.ptsd_scaled) %>%
  dplyr::summarize(mean = mean(renamed.depression_final_score))

ggplot(ptsd_depression_scores, aes(x=renamed.ptsd_scaled, y = renamed.depression_final_score)) + 
  geom_violin(trim=F, fill='#A4A4A4') +
  geom_boxplot(width=0.1, fill="white") + stat_summary(fun=mean, geom="crossbar", width=0.2, size=0.3, color="red") +
  geom_text(data = DataMean, aes(x = c(1.1,2.1,3.1,4.1), y = mean+15, label = round(mean,2), fontface = 2),
            hjust=0, size=8.5) +
  labs(title="PTSD and depression scores (mean)",x="", y='') +
  theme_classic()  +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30, face = 'bold'),legend.position = c(0.68, 0.55))


# PTSD and burnout

ptsd_burnout <- data.frame(renamed$level_of_burnout_3.3_scaled, renamed$ptsd_scaled) %>% na.omit()

ptsd_burnout$renamed.level_of_burnout_3.3_scaled <- factor(ptsd_burnout$renamed.level_of_burnout_3.3_scaled, levels = c("Severe burnout",
                                                                                                                        'Moderately severe burnout',
                                                                                                                        'Moderate burnout',
                                                                                                                        'Mild burnout',
                                                                                                                        'No burnout'))


# V1

ggplot(na.omit(ptsd_burnout),  aes(x =  na.omit(factor(renamed.ptsd_scaled)),
                                      fill=na.omit(factor(renamed.level_of_burnout_3.3_scaled)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and level of burnout", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,350)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# V2

ggplot(na.omit(ptsd_burnout),  aes(fill =  na.omit(factor(renamed.ptsd_scaled)),
                                   x = na.omit(factor(renamed.level_of_burnout_3.3_scaled)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and level of burnout", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,350)) +
  scale_x_discrete(labels = c("Severe\nburnout",
                              'Moderately\nsevere\nburnout',
                              'Moderate\nburnout',
                              'Mild\nburnout',
                              'No\nburnout')) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.35),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# PTSD and Resilience - final score

ptsd_resilience_fin_score <- data.frame(renamed$resilience_final_score, renamed$ptsd_scaled) %>% na.omit()


DataMean <- ptsd_resilience_fin_score %>% 
  group_by(renamed.ptsd_scaled) %>%
  dplyr::summarize(mean = mean(renamed.resilience_final_score))

ggplot(ptsd_resilience_fin_score, aes(x=renamed.ptsd_scaled, y = renamed.resilience_final_score)) + 
  geom_violin(trim=F, fill='#A4A4A4') +
  geom_boxplot(width=0.1, fill="white") + stat_summary(fun=mean, geom="crossbar", width=0.2, size=0.3, color="red") +
  geom_text(data = DataMean, aes(x = c(1.1,2.1,3.1,4.1), y = mean+13, label = round(mean,2), fontface = 2),
            hjust=0, size=8.5) +
  labs(title="PTSD and resilience scores (mean)",x="", y='') +
  theme_classic()  +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30, face = 'bold'),legend.position = c(0.68, 0.55))


# PTSD and Resilience - cognitive focus

ptsd_resilience_cog_focus <- data.frame(renamed$resilience_hardiness_cognitivefocus, renamed$ptsd_scaled) %>% na.omit()

DataMean <- ptsd_resilience_cog_focus %>% 
  group_by(renamed.ptsd_scaled) %>%
  dplyr::summarize(mean = mean(renamed.resilience_hardiness_cognitivefocus))

ggplot(ptsd_resilience_cog_focus, aes(x=renamed.ptsd_scaled, y = renamed.resilience_hardiness_cognitivefocus)) + 
  geom_violin(trim=F, fill='#A4A4A4') +
  geom_boxplot(width=0.1, fill="white") + stat_summary(fun=mean, geom="crossbar", width=0.2, size=0.3, color="red") +
  geom_text(data = DataMean, aes(x = c(1.1,2.1,3.1,4.1), y = mean+1.3, label = round(mean,2), fontface = 2),
            hjust=0, size=8.5) +
  labs(title="PTSD and resilience - cognitive focus (mean)",x="", y='') +
  theme_classic()  +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30, face = 'bold'),legend.position = c(0.68, 0.55))

# PTSD and resilience - flexibility

ptsd_resilience_flex <- data.frame(renamed$resilience_hardiness_flexibility, renamed$ptsd_scaled) %>% na.omit()

DataMean <- ptsd_resilience_flex %>% 
  group_by(renamed.ptsd_scaled) %>%
  dplyr::summarize(mean = mean(renamed.resilience_hardiness_flexibility))

ggplot(ptsd_resilience_flex, aes(x=renamed.ptsd_scaled, y = renamed.resilience_hardiness_flexibility)) + 
  geom_violin(trim=F, fill='#A4A4A4') +
  geom_boxplot(width=0.1, fill="white") + stat_summary(fun=mean, geom="crossbar", width=0.2, size=0.3, color="red") +
  geom_text(data = DataMean, aes(x = c(1.1,2.1,3.1,4.1), y = mean+2.4, label = round(mean,2), fontface = 2),
            hjust=0, size=8.5) +
  labs(title="PTSD and resilience - flexibility (mean)",x="", y='') +
  theme_classic()  +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30, face = 'bold'),legend.position = c(0.68, 0.55))


# PTSD and resilience - optimism

ptsd_resilience_optimism <- data.frame(renamed$resilience_hardiness_optimism, renamed$ptsd_scaled) %>% na.omit()

DataMean <- ptsd_resilience_optimism %>% 
  group_by(renamed.ptsd_scaled) %>%
  dplyr::summarize(mean = mean(renamed.resilience_hardiness_optimism))

ggplot(ptsd_resilience_optimism, aes(x=renamed.ptsd_scaled, y = renamed.resilience_hardiness_optimism)) + 
  geom_violin(trim=F, fill='#A4A4A4') +
  geom_boxplot(width=0.1, fill="white") + stat_summary(fun=mean, geom="crossbar", width=0.2, size=0.3, color="red") +
  geom_text(data = DataMean, aes(x = c(1.1,2.1,3.1,4.1), y = mean+4.4, label = round(mean,2), fontface = 2),
            hjust=0, size=8.5) +
  labs(title="PTSD and resilience - optimism (mean)",x="", y='') +
  theme_classic()  +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30, face = 'bold'),legend.position = c(0.68, 0.55))


# PTSD and resilience - SSE

ptsd_resilience_SSE <- data.frame(renamed$resilience_hardiness_SSE, renamed$ptsd_scaled) %>% na.omit()

DataMean <- ptsd_resilience_SSE %>% 
  group_by(renamed.ptsd_scaled) %>%
  dplyr::summarize(mean = mean(renamed.resilience_hardiness_SSE))

ggplot(ptsd_resilience_SSE, aes(x=renamed.ptsd_scaled, y = renamed.resilience_hardiness_SSE)) + 
  geom_violin(trim=F, fill='#A4A4A4') +
  geom_boxplot(width=0.1, fill="white") + stat_summary(fun=mean, geom="crossbar", width=0.2, size=0.3, color="red") +
  geom_text(data = DataMean, aes(x = c(1.1,2.1,3.1,4.1), y = mean+4.4, label = round(mean,2), fontface = 2),
            hjust=0, size=8.5) +
  labs(title="PTSD and resilience - sense of self-efficacy (mean)",x="", y='') +
  theme_classic()  +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30, face = 'bold'),legend.position = c(0.68, 0.55))

# PTSD and resilience - regulate emotion

ptsd_resilience_regulate_em <- data.frame(renamed$resilience_hardiness_regulate_em, renamed$ptsd_scaled) %>% na.omit()

DataMean <- ptsd_resilience_regulate_em %>% 
  group_by(renamed.ptsd_scaled) %>%
  dplyr::summarize(mean = mean(renamed.resilience_hardiness_regulate_em))

ggplot(ptsd_resilience_regulate_em, aes(x=renamed.ptsd_scaled, y = renamed.resilience_hardiness_regulate_em)) + 
  geom_violin(trim=F, fill='#A4A4A4') +
  geom_boxplot(width=0.1, fill="white") + stat_summary(fun=mean, geom="crossbar", width=0.2, size=0.3, color="red") +
  geom_text(data = DataMean, aes(x = c(1.1,2.1,3.1,4.1), y = mean+1.63, label = round(mean,2), fontface = 2),
            hjust=0, size=8.5) +
  labs(title="PTSD and resilience - regulate emotion (mean)",x="", y='') +
  theme_classic()  +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30, face = 'bold'),legend.position = c(0.68, 0.55))

# PTSD and Race

racequestions_for_table <- data.frame(renamed$race_americanindian_5.4.1, renamed$race_asian_5.4.2, renamed$race_black_5.4.3, renamed$race_native_haw_5.4.4, 
                                      renamed$race_white_5.4.5, renamed$race_others_5.4.6, renamed$ID) 

ptsd_race <-  revert_onehot_with_id(racequestions_for_table)

ptsd_race <- inner_join(racewithid, renamed[, c("ID","ptsd_scaled") ], by = c('ID' = 'ID')) %>% na.omit()

ptsd_race$value <- factor(ptsd_race$value, levels = c("Others", "White", "Native Hawaiian or Pacific Islander","Black or African American",  "Asian", "American Indian or Alaska Native")) 

# V1

ggplot(na.omit(ptsd_race),  aes(x =  na.omit(factor(ptsd_scaled)),
                                   fill=na.omit(factor(value)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and race", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,300)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# V2

ggplot(na.omit(ptsd_race),  aes(fill =  na.omit(factor(ptsd_scaled)),
                                x = na.omit(factor(value)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and race", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,300)) +
  scale_x_discrete(labels = c("Others",
                              "White",
                              "Native Hawaiian\nor\nPacific Islander",
                              "Black or\nAfrican American",
                              "Asian",
                              "American Indian or\nAlaska Native")) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# PTSD and immigrant

ptsd_immigrant <- data.frame(renamed$Immigrant_5.5, renamed$ptsd_scaled) %>% na.omit()


# V1

ggplot(na.omit(ptsd_immigrant),  aes(x =  na.omit(factor(renamed.ptsd_scaled)),
                                fill=na.omit(factor(renamed.Immigrant_5.5)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and immigrant", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,310)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# V2

ggplot(na.omit(ptsd_immigrant),  aes(fill =  na.omit(factor(renamed.ptsd_scaled)),
                                     x = na.omit(factor(renamed.Immigrant_5.5)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and immigrant", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,310)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# ptsd and country

ptsd_country <- data.frame(renamed$Country_5.5.1, renamed$ptsd_scaled) %>% na.omit()

ptsd_country <- within(ptsd_country,renamed.Country_5.5.1 <- factor(renamed.Country_5.5.1, levels=names(sort(table(renamed.Country_5.5.1),decreasing=T))))

label(ptsd_country$renamed.Country_5.5.1) <- "Country"

table1::table1(~ptsd_country$renamed.Country_5.5.1 | ptsd_country$renamed.ptsd_scaled)

# ptsd and underlying health conditions

ptsd_underlying_health <- data.frame(renamed$underlyinh_health_condition_2.2, renamed$ptsd_scaled) %>% na.omit()

# V1

ggplot(na.omit(ptsd_underlying_health),  aes(x =  na.omit(factor(renamed.ptsd_scaled)),
                                     fill=na.omit(factor(renamed.underlyinh_health_condition_2.2)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and underlying health condition", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,305)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# V2

ggplot(na.omit(ptsd_underlying_health),  aes(fill =  na.omit(factor(renamed.ptsd_scaled)),
                                             x = na.omit(factor(renamed.underlyinh_health_condition_2.2)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and underlying health condition", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,305)) +
  scale_x_discrete(labels = c("I don't\nknow /\nNot sure",
                              'No',
                              'Yes')) +
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.75),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# ptsd and pregnant

ptsd_pregnant <- data.frame(renamed$self_or_partner_preganant_2.3.1, renamed$Self_or_partner_with_newborn_2.3.2,
                            renamed$self_or_part_no_preg_no_newborn_2.3.3, renamed$ID)

ptsd_pregnant <- revert_onehot_with_id(ptsd_pregnant)

ptsd_pregnant <- inner_join(ptsd_pregnant, renamed[, c("ID","ptsd_scaled") ], by = c('ID' = 'ID')) %>% na.omit()

# V1

ggplot(na.omit(ptsd_pregnant),  aes(x =  na.omit(factor(ptsd_scaled)),
                                             fill=na.omit(factor(value)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and self/partner with newborn/pregnant", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,405)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# V2

ggplot(na.omit(ptsd_pregnant),  aes(fill =  na.omit(factor(ptsd_scaled)),
                                    x = na.omit(factor(value)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and self/partner with newborn/pregnant", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,405)) +
  scale_x_discrete(labels = c("No",
                              'Yes,\npregnant',
                              'Yes,\nwith a new born')) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# PTSD and covid fears

ptsd_covid_fears <- data.frame(renamed$Demo_fear_cont_COVID_2.4.1, renamed$ptsd_scaled) %>% na.omit()

# V1

ggplot(na.omit(ptsd_covid_fears),  aes(x =  na.omit(factor(renamed.ptsd_scaled)),
                                    fill=na.omit(factor(renamed.Demo_fear_cont_COVID_2.4.1)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and fear contracting COVID", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,150)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# V2

ggplot(na.omit(ptsd_covid_fears),  aes(fill =  na.omit(factor(renamed.ptsd_scaled)),
                                       x=na.omit(factor(renamed.Demo_fear_cont_COVID_2.4.1)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and fear contracting COVID", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,150)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.84, 0.85),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# PTSD and transmit to others concern

ptsd_transmit_concern <- data.frame(renamed$Demo_conc_might_transmit_2.4.2, renamed$ptsd_scaled) %>% na.omit()

# V1

ggplot(na.omit(ptsd_transmit_concern),  aes(x =  na.omit(factor(renamed.ptsd_scaled)),
                                       fill=na.omit(factor(renamed.Demo_conc_might_transmit_2.4.2)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and COVID transmit concern", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,150)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# V2

ggplot(na.omit(ptsd_transmit_concern),  aes(fill =  na.omit(factor(renamed.ptsd_scaled)),
                                            x = na.omit(factor(renamed.Demo_conc_might_transmit_2.4.2)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and COVID transmit concern", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,150)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.84, 0.85),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# PTSD and average work hours per week before covid

ptsd_hours_bfore_covid <- data.frame(renamed$Hours_worked_before_covid_2.6, renamed$ptsd_scaled) %>% na.omit()

DataMean <- ptsd_hours_bfore_covid %>% 
  group_by(renamed.ptsd_scaled) %>%
  dplyr::summarize(mean = mean(renamed.Hours_worked_before_covid_2.6))

ggplot(ptsd_hours_bfore_covid, aes(x=renamed.ptsd_scaled, y = renamed.Hours_worked_before_covid_2.6)) + 
  geom_violin(trim=F, fill='#A4A4A4') +
  geom_boxplot(width=0.1, fill="white") + stat_summary(fun=mean, geom="crossbar", width=0.2, size=0.3, color="red") +
  geom_text(data = DataMean, aes(x = c(1.1,2.1,3.1,4.1), y = mean+50, label = round(mean,2), fontface = 2),
            hjust=0, size=8.5) +
  labs(title="PTSD and hours worked before COVID (mean)",x="", y='') +
  theme_classic()  +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30, face = 'bold'),legend.position = c(0.68, 0.55))


# PTSD and race mistreated

ptsd_race_mistreated <- data.frame(renamed$discrim_mistreated_2.14, renamed$ptsd_scaled) %>% na.omit()

# V1

ggplot(na.omit(ptsd_race_mistreated),  aes(x =  na.omit(factor(renamed.ptsd_scaled)),
                                            fill=na.omit(factor(renamed.discrim_mistreated_2.14)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and mistreated because of race", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,405)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# V2

ggplot(na.omit(ptsd_race_mistreated),  aes(fill =  na.omit(factor(renamed.ptsd_scaled)),
                                           x = na.omit(factor(renamed.discrim_mistreated_2.14)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and mistreated because of race", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,405)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# PTSD and race - treated with less courtesy

ptsd_race_1 <- data.frame(renamed$discrim_less_courtesy_2.14.1.1, renamed$ptsd_scaled) %>% na.omit()

ptsd_race_1$renamed.discrim_less_courtesy_2.14.1.1 <- factor(ptsd_race_1$renamed.discrim_less_courtesy_2.14.1.1, levels = c("Never", "Once during the entire period",
                                                                      "A few times during the entire period", "A few times a month",
                                                                      "Atleast once a week","Almost everyday" )) 

# V1

ggplot(na.omit(ptsd_race_1),  aes(x =  na.omit(factor(renamed.ptsd_scaled)),
                                           fill=na.omit(factor(renamed.discrim_less_courtesy_2.14.1.1)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and treated with less courtesy because of race", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)),breaks = c(0,5,8), limits = c(0,8)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.75),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# V2

ggplot(na.omit(ptsd_race_1),  aes(fill =  na.omit(factor(renamed.ptsd_scaled)),
                                  x = na.omit(factor(renamed.discrim_less_courtesy_2.14.1.1)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and treated with less courtesy because of race", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)),breaks = c(0,5,8), limits = c(0,8)) +
  scale_x_discrete(labels = c("Never", "Once during the\nentire period",
                              "A few times during\nthe entire period", "A few times\na month",
                              "Atleast once\na week","Almost\neveryday")) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# ptsd and race - treated with less respect

ptsd_race_2 <- data.frame(renamed$discrim_less_respect_2.14.1.2, renamed$ptsd_scaled) %>% na.omit()


ptsd_race_2$renamed.discrim_less_respect_2.14.1.2 <- factor(ptsd_race_2$renamed.discrim_less_respect_2.14.1.2, levels = c("Never", "Once during the entire period",
                                                                                                                            "A few times during the entire period", "A few times a month",
                                                                                                                            "Atleast once a week","Almost everyday" )) 

# V1

ggplot(na.omit(ptsd_race_2),  aes(x =  na.omit(factor(renamed.ptsd_scaled)),
                                  fill=na.omit(factor(renamed.discrim_less_respect_2.14.1.2)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and treated with less respect because of race", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)),breaks = c(0,5,8), limits = c(0,8)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# V2

ggplot(na.omit(ptsd_race_2),  aes(fill =  na.omit(factor(renamed.ptsd_scaled)),
                                  x = na.omit(factor(renamed.discrim_less_respect_2.14.1.2)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and treated with less respect because of race", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)),breaks = c(0,5,8), limits = c(0,8)) +
  scale_x_discrete(labels = c("Never", "Once during the\nentire period",
                              "A few times during\nthe entire period", "A few times\na month",
                              "Atleast once\na week","Almost\neveryday")) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.75),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# ptsd and race - acted as if you are not smart

ptsd_race_3 <- data.frame(renamed$discrim_not_smart_2.14.1.3, renamed$ptsd_scaled) %>% na.omit()


ptsd_race_3$renamed.discrim_not_smart_2.14.1.3 <- factor(ptsd_race_3$renamed.discrim_not_smart_2.14.1.3, levels = c("Never", "Once during the entire period",
                                                                                                                          "A few times during the entire period", "A few times a month",
                                                                                                                          "Atleast once a week","Almost everyday" )) 
# V1


ggplot(na.omit(ptsd_race_3),  aes(x =  na.omit(factor(renamed.ptsd_scaled)),
                                  fill=na.omit(factor(renamed.discrim_not_smart_2.14.1.3)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and acted as if not smart because of race", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)),breaks = c(0,5,7), limits = c(0,7)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# V2

ggplot(na.omit(ptsd_race_3),  aes(fill =  na.omit(factor(renamed.ptsd_scaled)),
                                  x = na.omit(factor(renamed.discrim_not_smart_2.14.1.3)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and acted as if not smart because of race", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)),breaks = c(0,5,7), limits = c(0,7)) +
  scale_x_discrete(labels = c("Never", "Once during the\nentire period",
                              "A few times during\nthe entire period", "A few times\na month",
                              "Atleast once\na week","Almost\neveryday")) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.75),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# PTSD and race - acted as if they were afraid of you

ptsd_race_4 <- data.frame(renamed$discrim_afraid_2.14.1.4, renamed$ptsd_scaled) %>% na.omit()


ptsd_race_4$renamed.discrim_afraid_2.14.1.4 <- factor(ptsd_race_4$renamed.discrim_afraid_2.14.1.4, levels = c("Never", "Once during the entire period",
                                                                                                                 "A few times during the entire period", "A few times a month",
                                                                                                                 "Atleast once a week","Almost everyday" )) 
# V1

ggplot(na.omit(ptsd_race_4),  aes(x =  na.omit(factor(renamed.ptsd_scaled)),
                                  fill=na.omit(factor(renamed.discrim_afraid_2.14.1.4)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and acted as if afraid because of race", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)),breaks = c(0,5,7), limits = c(0,7)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)


# V2

ggplot(na.omit(ptsd_race_4),  aes(fill =  na.omit(factor(renamed.ptsd_scaled)),
                                  x = na.omit(factor(renamed.discrim_afraid_2.14.1.4)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and acted as if afraid because of race", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)),breaks = c(0,5,7), limits = c(0,7)) +
  scale_x_discrete(labels = c("Never", "Once during the\nentire period",
                              "A few times during\nthe entire period", "A few times\na month",
                              "Atleast once\na week","Almost\neveryday")) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# PTSD and race - acted as if you're dishonest

ptsd_race_5 <- data.frame(renamed$discrim_dishonest_2.14.1.5, renamed$ptsd_scaled) %>% na.omit()


ptsd_race_5$renamed.discrim_dishonest_2.14.1.5 <- factor(ptsd_race_5$renamed.discrim_dishonest_2.14.1.5, levels = c("Never", "Once during the entire period",
                                                                                                              "A few times during the entire period", "A few times a month",
                                                                                                              "Atleast once a week","Almost everyday" )) 
# V1

ggplot(na.omit(ptsd_race_5),  aes(x =  na.omit(factor(renamed.ptsd_scaled)),
                                  fill=na.omit(factor(renamed.discrim_dishonest_2.14.1.5)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and acted as if not honest because of race", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)),breaks = c(0,5,7), limits = c(0,7)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.75),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# V2

ggplot(na.omit(ptsd_race_5),  aes(fill =  na.omit(factor(renamed.ptsd_scaled)),
                                  x = na.omit(factor(renamed.discrim_dishonest_2.14.1.5)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and acted as if not honest because of race", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)),breaks = c(0,5,7), limits = c(0,7)) +
  scale_x_discrete(labels = c("Never", "Once during the\nentire period",
                              "A few times during\nthe entire period", "A few times\na month",
                              "Atleast once\na week","Almost\neveryday")) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# PTSD and race - acted as if they're better than you

ptsd_race_6 <- data.frame(renamed$discrim_better_2.14.1.6, renamed$ptsd_scaled) %>% na.omit()


ptsd_race_6$renamed.discrim_better_2.14.1.6 <- factor(ptsd_race_6$renamed.discrim_better_2.14.1.6, levels = c("Never", "Once during the entire period",
                                                                                                                    "A few times during the entire period", "A few times a month",
                                                                                                                    "Atleast once a week","Almost everyday" )) 
# V1

ggplot(na.omit(ptsd_race_6),  aes(x =  na.omit(factor(renamed.ptsd_scaled)),
                                  fill=na.omit(factor(renamed.discrim_better_2.14.1.6)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and acted as if they're better because of race", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)),breaks = c(0,5,7), limits = c(0,7)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.75),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# V2

ggplot(na.omit(ptsd_race_6),  aes(fill =  na.omit(factor(renamed.ptsd_scaled)),
                                  x = na.omit(factor(renamed.discrim_better_2.14.1.6)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and acted as if they're better because of race", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)),breaks = c(0,5,7), limits = c(0,7)) +
  scale_x_discrete(labels = c("Never", "Once during the\nentire period",
                              "A few times during\nthe entire period", "A few times\na month",
                              "Atleast once\na week","Almost\neveryday")) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.75),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# PTSD and race - called names

ptsd_race_7 <- data.frame(renamed$discrim_call_names_2.14.1.7, renamed$ptsd_scaled) %>% na.omit()

ptsd_race_7$renamed.discrim_call_names_2.14.1.7 <- factor(ptsd_race_7$renamed.discrim_call_names_2.14.1.7, levels = c("Never", "Once during the entire period",
                                                                                                              "A few times during the entire period", "A few times a month",
                                                                                                              "Atleast once a week","Almost everyday" )) 
# V1

ggplot(na.omit(ptsd_race_7),  aes(x =  na.omit(factor(renamed.ptsd_scaled)),
                                  fill=na.omit(factor(renamed.discrim_call_names_2.14.1.7)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and called names because of race", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)),breaks = c(0,5,9), limits = c(0,9)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# V2

ggplot(na.omit(ptsd_race_7),  aes(fill =  na.omit(factor(renamed.ptsd_scaled)),
                                  x=na.omit(factor(renamed.discrim_call_names_2.14.1.7)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and called names because of race", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)),breaks = c(0,5,9), limits = c(0,9)) +
  scale_x_discrete(labels = c("Never", "Once during the\nentire period",
                              "A few times during\nthe entire period", "A few times\na month",
                              "Atleast once\na week","Almost\neveryday")) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.55),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# PTSD and race - threatened or harassed

ptsd_race_8 <- data.frame(renamed$discrim_threat_harassed_2.14.1.8, renamed$ptsd_scaled) %>% na.omit()


ptsd_race_8$renamed.discrim_threat_harassed_2.14.1.8 <- factor(ptsd_race_8$renamed.discrim_threat_harassed_2.14.1.8, levels = c("Never", "Once during the entire period",
                                                                                                                      "A few times during the entire period", "A few times a month",
                                                                                                                      "Atleast once a week","Almost everyday" )) 
# V1

ggplot(na.omit(ptsd_race_8),  aes(x =  na.omit(factor(renamed.ptsd_scaled)),
                                  fill=na.omit(factor(renamed.discrim_threat_harassed_2.14.1.8)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and threatened/harassed because of race", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)),breaks = c(0,5,10), limits = c(0,10)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.35),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# V2

ggplot(na.omit(ptsd_race_8),  aes(fill =  na.omit(factor(renamed.ptsd_scaled)),
                                  x = na.omit(factor(renamed.discrim_threat_harassed_2.14.1.8)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and threatened/harassed because of race", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)),breaks = c(0,5,10), limits = c(0,10)) +
  scale_x_discrete(labels = c("Never", "Once during the\nentire period",
                              "A few times during\nthe entire period", "A few times\na month",
                              "Atleast once\na week","Almost\neveryday")) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.78, 0.75),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# PTSD and speciality

ptsd_speciality <- specialityquestions_for_table %>% revert_onehot_with_id() 

ptsd_speciality <- inner_join(ptsd_speciality, renamed[, c("ID","ptsd_scaled") ], by = c('ID' = 'ID')) %>% na.omit()

ptsd_speciality <- within(ptsd_speciality,value <- factor(value, levels= c("Adult Critical Care","Anesthesiology","Cardiology",
                                                                             "Dermatology","Electrophysiology","Emergency Medicine",                         
                                                                             "Endocrinology","Family Medicine","Gastroenterologist",                         
                                                                             "General Surgery","Geriatrics","Immunology",                                 
                                                                             "Infectious diseases","Internal Medicine","Interventional Radiology",                   
                                                                             "Medical Genetics","Neonatal Critical Care","Neurology",                                  
                                                                             "Neurosurgery","Obstetrics and Gynecology","Oncology",                                   
                                                                             "Ophthalmology","Orthopedic surgery","Orthopedics",                                
                                                                             "Otolaryngology","Pathology","Pediatric Critical Care",                    
                                                                             "Pediatric Emergency Medicine","Pediatrics","Physical Medicine and Rehabilitation",       
                                                                             "Podiatry","Preventive medicine / occupational medicine","Psychiatry",                                 
                                                                             "Pulmonology","Radiation oncology","Radiology",                                  
                                                                             "Rheumatology","Urology","Vascular Surgery", "Others")))


label(ptsd_speciality$value) <- "Speciality"

table1::table1(~ptsd_speciality$value | ptsd_speciality$ptsd_scaled)

# PTSD and state

ptsd_state <- data.frame(renamed$State_of_practice_5.9, renamed$ptsd_scaled) %>% na.omit()
ptsd_state <- within(ptsd_state,renamed.State_of_practice_5.9 <- factor(renamed.State_of_practice_5.9, levels = factor(renamed.State_of_practice_5.9) %>% levels() %>% sort()))


label(ptsd_state$renamed.State_of_practice_5.9) <- "State of practice"

table1::table1(~ptsd_state$renamed.State_of_practice_5.9 | ptsd_state$renamed.ptsd_scaled)

# PTSD and primary work setting

ptsd_primary_work_setting <- data.frame(renamed$Primary_work_setting_5.11, renamed$ptsd_scaled) %>% na.omit()
 
ptsd_primary_work_setting$renamed.Primary_work_setting_5.11 <- factor(ptsd_primary_work_setting$renamed.Primary_work_setting_5.11, levels = c('Academic medical center',
                                                                                                                                              'Group practice',
                                                                                                                                              'Hospital',
                                                                                                                                              'Solo practice',
                                                                                                                                              'Two-physician practice',
                                                                                                                                              'Outpatient center',
                                                                                                                                              "Others"))

label(ptsd_primary_work_setting$renamed.Primary_work_setting_5.11) <- "Primary Work setting"

table1::table1(~ptsd_primary_work_setting$renamed.Primary_work_setting_5.11 | ptsd_primary_work_setting$renamed.ptsd_scaled)


# PTSD and hospital type

ptsd_hospital_type <- hospitaltype_for_table %>% revert_onehot_with_id()

ptsd_hospital_type <- inner_join(ptsd_hospital_type, renamed[, c("ID","ptsd_scaled") ], by = c('ID' = 'ID')) %>% na.omit()

ptsd_hospital_type <- within(ptsd_hospital_type,value <- factor(value, levels= c( "Medium Hospital (100-500 beds)","Community (Non-federal Acute Care) Hospital",                 
                                                                                  "Urban Hospital","Suburban Hospital",                                           
                                                                                  "Teaching / Research Hospital","Large hospitals (>500 beds)",                                 
                                                                                  "Critical Access Hospital","Rural Hospital",                                              
                                                                                  "Small Hospital (<100 beds)","Veterans or Federal Hospital",                                
                                                                                  "Specialty (e.g., Cancer Hospital, Children's Hospital, etc.)", "Non-federal Psychiatric Care",                                
                                                                                  "Rehabilitation Hospital", "Others")))

label(ptsd_hospital_type$value) <- "Hospital type"

table1::table1(~ptsd_hospital_type$value | ptsd_hospital_type$ptsd_scaled)


# PTSD and work setting within hospital

ptsd_worksetting <- worksettingwithinhospital_for_table %>% revert_onehot_with_id()

ptsd_worksetting <- inner_join(ptsd_worksetting, renamed[, c("ID","ptsd_scaled") ], by = c('ID' = 'ID')) %>% na.omit()


ptsd_worksetting <- within(ptsd_worksetting,value <- factor(value, levels=c("Emergency Department","General Floor","Intensive Care Unit (any type)",
                                                                            "Operating Room","Step Down Unit", "Others")))


label(ptsd_worksetting$value) <- "Work setting within hospital"

table1::table1(~ptsd_worksetting$value | ptsd_worksetting$ptsd_scaled)

# PTSD and working status

ptsd_working_status <- data.frame(renamed$Working_status_5.12, renamed$ptsd_scaled) %>% na.omit()

# V1

ggplot(na.omit(ptsd_working_status),  aes(x =  na.omit(factor(renamed.ptsd_scaled)),
                                  fill=na.omit(factor(renamed.Working_status_5.12)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..fill.. ,sum)[..fill..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and working status", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,350)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.75),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)

# V2

ggplot(na.omit(ptsd_working_status),  aes(fill =  na.omit(factor(renamed.ptsd_scaled)),
                                          x = na.omit(factor(renamed.Working_status_5.12)))) +  
  geom_bar(aes(y = (..count..)), width = 0.6, position = 'dodge')+
  geom_text(aes(y = (..count..),label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.01)),
            stat="count",position = position_dodge(0.65), hjust = -0.1,vjust = 0.4, size = 5.8) +
  labs(title = "PTSD and working status", y = "", x = '' ) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0,350)) +  
  theme_classic() + 
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30),legend.position = c(0.68, 0.75),
        legend.title = element_text(colour = 'white', size = 0.01),
        legend.text = element_text(size = 28)) + coord_flip() + scale_fill_grey(start = 0, end = .9)



# PTSD and POP - Black

ptsd_pop_black <- data.frame(renamed$POP_Black_5.13.1, renamed$ptsd_scaled) %>% na.omit()

DataMean <- ptsd_pop_black %>% 
  group_by(renamed.ptsd_scaled) %>%
  dplyr::summarize(mean = mean(renamed.POP_Black_5.13.1))

ggplot(ptsd_pop_black, aes(x=renamed.ptsd_scaled, y = renamed.POP_Black_5.13.1)) + 
  geom_violin(trim=F, fill='#A4A4A4') +
  geom_boxplot(width=0.1, fill="white") + stat_summary(fun=mean, geom="crossbar", width=0.2, size=0.3, color="red") +
  geom_text(data = DataMean, aes(x = c(1.1,2.1,3.1,4.1), y = mean+44.4, label = round(mean,2), fontface = 2),
            hjust=0, size=8.5) +
  labs(title="PTSD and percent of population - Black (mean)",x="", y='') +
  theme_classic()  +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30, face = 'bold'),legend.position = c(0.68, 0.55))

# PTSD and POP - Non - hispanic white

ptsd_pop_white <- data.frame(renamed$POP_Non_Hisp_white_5.13.2, renamed$ptsd_scaled) %>% na.omit()

DataMean <- ptsd_pop_white %>% 
  group_by(renamed.ptsd_scaled) %>%
  dplyr::summarize(mean = mean(renamed.POP_Non_Hisp_white_5.13.2))

ggplot(ptsd_pop_white, aes(x=renamed.ptsd_scaled, y = renamed.POP_Non_Hisp_white_5.13.2)) + 
  geom_violin(trim=F, fill='#A4A4A4') +
  geom_boxplot(width=0.1, fill="white") + stat_summary(fun=mean, geom="crossbar", width=0.2, size=0.3, color="red") +
  geom_text(data = DataMean, aes(x = c(1.1,2.1,3.1,4.1), y = mean+64.4, label = round(mean,2), fontface = 2),
            hjust=0, size=8.5) +
  labs(title="PTSD and percent of population - Non-hispanic white (mean)",x="", y='') +
  theme_classic()  +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30, face = 'bold'),legend.position = c(0.68, 0.55))

# PTSD and POP - Hispanic/latino

ptsd_pop_hispanic <- data.frame(renamed$POP_Hispanic_latino_5.13.3, renamed$ptsd_scaled) %>% na.omit()

DataMean <- ptsd_pop_hispanic %>% 
  group_by(renamed.ptsd_scaled) %>%
  dplyr::summarize(mean = mean(renamed.POP_Hispanic_latino_5.13.3))

ggplot(ptsd_pop_hispanic, aes(x=renamed.ptsd_scaled, y = renamed.POP_Hispanic_latino_5.13.3)) + 
  geom_violin(trim=F, fill='#A4A4A4') +
  geom_boxplot(width=0.1, fill="white") + stat_summary(fun=mean, geom="crossbar", width=0.2, size=0.3, color="red") +
  geom_text(data = DataMean, aes(x = c(1.1,2.1,3.1,4.1), y = mean+64.4, label = round(mean,2), fontface = 2),
            hjust=0, size=8.5) +
  labs(title="PTSD and percent of population - Hispanic/latino (mean)",x="", y='') +
  theme_classic()  +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30, face = 'bold'),legend.position = c(0.68, 0.55))

# PTSD and POP - Asian

ptsd_pop_asian <- data.frame(renamed$POP_Asian_5.13.4, renamed$ptsd_scaled) %>% na.omit()

DataMean <- ptsd_pop_asian %>% 
  group_by(renamed.ptsd_scaled) %>%
  dplyr::summarize(mean = mean(renamed.POP_Asian_5.13.4))

ggplot(ptsd_pop_asian, aes(x=renamed.ptsd_scaled, y = renamed.POP_Asian_5.13.4)) + 
  geom_violin(trim=F, fill='#A4A4A4') +
  geom_boxplot(width=0.1, fill="white") + stat_summary(fun=mean, geom="crossbar", width=0.2, size=0.3, color="red") +
  geom_text(data = DataMean, aes(x = c(1.1,2.1,3.1,4.1), y = mean+44.4, label = round(mean,2), fontface = 2),
            hjust=0, size=8.5) +
  labs(title="PTSD and percent of population - Asian (mean)",x="", y='') +
  theme_classic()  +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30, face = 'bold'),legend.position = c(0.68, 0.55))


# PTSD and POP - American indian

ptsd_pop_american_indian <- data.frame(renamed$POP_American_indian_5.13.5, renamed$ptsd_scaled) %>% na.omit()

DataMean <- ptsd_pop_american_indian %>% 
  group_by(renamed.ptsd_scaled) %>%
  dplyr::summarize(mean = mean(renamed.POP_American_indian_5.13.5))

ggplot(ptsd_pop_american_indian, aes(x=renamed.ptsd_scaled, y = renamed.POP_American_indian_5.13.5)) + 
  geom_violin(trim=F, fill='#A4A4A4') +
  geom_boxplot(width=0.1, fill="white") + stat_summary(fun=mean, geom="crossbar", width=0.2, size=0.3, color="red") +
  geom_text(data = DataMean, aes(x = c(1.1,2.1,3.1,4.1), y = mean+24.4, label = round(mean,2), fontface = 2),
            hjust=0, size=8.5) +
  labs(title="PTSD and percent of population - American indian (mean)",x="", y='') +
  theme_classic()  +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30, face = 'bold'),legend.position = c(0.68, 0.55))

# PTSD and POP - Native hawaiian

ptsd_pop_hawaiian <- data.frame(renamed$POP_Native_hawaiian_5.13.6, renamed$ptsd_scaled) %>% na.omit()

DataMean <- ptsd_pop_hawaiian %>% 
  group_by(renamed.ptsd_scaled) %>%
  dplyr::summarize(mean = mean(renamed.POP_Native_hawaiian_5.13.6))

ggplot(ptsd_pop_hawaiian, aes(x=renamed.ptsd_scaled, y = renamed.POP_Native_hawaiian_5.13.6)) + 
  geom_violin(trim=F, fill='#A4A4A4') +
  geom_boxplot(width=0.1, fill="white") + stat_summary(fun=mean, geom="crossbar", width=0.2, size=0.3, color="red") +
  geom_text(data = DataMean, aes(x = c(1.1,2.1,3.1,4.1), y = mean+24.4, label = round(mean,2), fontface = 2),
            hjust=0, size=8.5) +
  labs(title="PTSD and percent of population - Native Hawaiian or Pacific Islander  (mean)",x="", y='') +
  theme_classic()  +
  theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
        axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
        axis.text.x = element_text(size=30, face = 'bold'),legend.position = c(0.68, 0.55)) 

####################################################################################################################################################
####################################################################################################################################################
####################################################################################################################################################
####################################################################################################################################################
####################################################################################################################################################
