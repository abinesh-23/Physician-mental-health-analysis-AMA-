############################################################### codes for data wrangling ###########################################################

############################## Prepared by Abinesh Senthil Kumar for individual problem  under Dr. Mukherjee (Fall 2020) ############################

################################################################################################################################################

# go to line 190 to load data

# run this script fully before running plots.R codes

rm(list = ls()) # clearing workspace

# installing required packages and loading them

requiredPackages = c('ggplot2', 'scales', 'tidyverse', 'gridExtra', 'reshape2',
                     'grid', 'table1', 'usmap', 'fmsb', 'readr', 'plyr', 'dplyr', 'tidyverse',
                     'writexl', 'hrbrthemes', 'Hmisc', 'lubridate', 'BBmisc', 'tidyr', 'zipcodeR')

for(p in requiredPackages){
  if(!require(p,character.only = T)) install.packages(p)
  library(p,character.only = T)
}

# manual functions created for the analysis

### functions

# density plot for comparing multiple groups showing mean

# this function taked the dataset name in the dataset argument, points represent the continuous variable,
# groups represent the categorical variable, annotx and annoty represents the annotation position for the mean,
# titled respresents the title of the plot, legend pos is an optional argument that takes the value of the position for legend

indivgroupdensityplot <- function(dataset, points, groups, annotx, annoty, titled, legend_pos = c(0.85, 0.70)) {
  
  dataset$grp <- dataset[, which(names(dataset) == groups)]
  dataset$pts <- dataset[, which(names(dataset) == points)]
  
  # getting mean to use them as annotations 
  meanbptsd <- aggregate(pts ~ grp, dataset, mean)
  
  annot <- data.frame(
    x1 = annotx,
    y1 = annoty
  )
  
  meanbptsd$x1 <- annot$x1
  meanbptsd$y1 <- annot$y1
  
  # rounding the mean to two decimal places
  meanbptsd$pts <- meanbptsd$pts %>% round(2)
  
  # density plot
  ggplot(data=dataset, aes(x= pts, group = grp, fill= grp)) +
    geom_density(adjust=1.5, alpha=.4) +
    labs(title = titled, x = '', y = 'Density') + 
    geom_vline(data = meanbptsd, aes(xintercept = pts, color = grp), linetype = "dashed", size = 1, show.legend = F) +
    geom_text(data = meanbptsd, aes(x = x1, y = y1, label = pts, fontface = 2),
              hjust=0, size=8.5, face = 'bold') +
    theme_ipsum() + 
    theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
          axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
          axis.text.x = element_text(size=30),axis.title.y = element_text(size=30, hjust = 0.5, vjust = 1.7),
          legend.position = legend_pos,
          legend.title = element_text(colour = 'white', size = 0.01),
          legend.text = element_text(size = 28)) 
  
}


# density plot for comparing multiple groups showing median

# this function taked the dataset name in the dataset argument, points represent the continuous variable,
# groups represent the categorical variable, annotx and annoty represents the annotation position for the mean,
#titled respresents the title of the plot, legend pos is an optional argument that takes thevalue of the position for legend

indivgroupdensityplot_median <- function(dataset, points, groups, annotx, annoty, titled, legend_pos = c(0.85, 0.70)) {
  
  dataset$grp <- dataset[, which(names(dataset) == groups)]
  dataset$pts <- dataset[, which(names(dataset) == points)]
  
  # getting median to use them as annotations 
  meanbptsd <- aggregate(pts ~ grp, dataset, median)
  
  annot <- data.frame(
    x1 = annotx,
    y1 = annoty
  )
  
  meanbptsd$x1 <- annot$x1
  meanbptsd$y1 <- annot$y1
  
  # rounding the median to two decimal places
  meanbptsd$pts <- meanbptsd$pts %>% round(2)
  
  # density plot
  ggplot(data=dataset, aes(x= pts, group = grp, fill= grp)) +
    geom_density(adjust=1.5, alpha=.4) +
    labs(title = titled, x = '', y = 'Density') + 
    geom_vline(data = meanbptsd, aes(xintercept = pts, color = grp), linetype = "dashed", size = 1 , show.legend = F) +
    geom_text(data = meanbptsd, aes(x = x1, y = y1, label = pts, fontface = 2),
              hjust=0, size=8.5, face = 'bold') +
    theme_ipsum() + 
    theme(plot.title = element_text(size = 30,hjust = 0.5, face = 'bold'),
          axis.text.y = element_text(size=30, hjust = 0.5, face = 'bold'),
          axis.text.x = element_text(size=30),axis.title.y = element_text(size=30, hjust = 0.5, vjust = 1.7),
          legend.position = legend_pos,
          legend.title = element_text(colour = 'white', size = 0.01),
          legend.text = element_text(size = 28)) 
  
  
}
 

# reverting one hot variables

#This function takes input as a dataset with only the one hot encoded columns present. 

revert_onehot <- function(dset) {
  
  #adding a column, number of observations in incremental order to the dataset
  dset1 <- cbind(dset, 1:nrow(dset))
  
  #changing column names
  colnames(dset1) <- c(colnames(dset), 'ID')
  
  # finding the number of missing values
  sum_of_missing <- function(x){sum(is.na(x))}
  
  dset1missing <- which(apply(dset1[,-ncol(dset1)],1,sum_of_missing) == (ncol(dset1)-1)) %>% length()
  
  # using melt function from reshape2 to stack all the one hot encoded columns under each others 
  dset1 <- melt(dset1, id.vars = 'ID') 
  
  fordset <- data.frame(dset1$value) %>% na.omit()
  
  # manually adding missing values wrt number of persons who didn't answer anything to all the questions
  fordset[nrow(fordset)+1:dset1missing,] <- NA
  
  return(fordset)
  
}


# reverting one hot variables with id

#This function takes input as a dataset with only the one hot encoded columns present and the id variable in the end. 

revert_onehot_with_id <- function(dset) {
  
  dset1 <- dset
  
  #changing column names
  colnames(dset1) <- c(colnames(dset[,-ncol(dset1)]), 'ID')
  
  # finding the number of missing values
  sum_of_missing <- function(x){sum(is.na(x))}
  
  dset1missing_index <- which(apply(dset1[,-ncol(dset1)],1,sum_of_missing) == (ncol(dset1)-1)) %>% data.frame()
  
  colnames(dset1missing_index) <- "ID"
  
  # exception handling to capture if there are no records that didn't answer anything to all the number of questions
  tryCatch({
    dset1missing_index$value <- NA
  }, warning = function(w) {
    
  }, error = function(e) {
    
  }, finally = {
    
  })
  
  # using melt function from reshape2 to stack all the one hot encoded columns under each others 
  dset1 <- melt(dset1, id.vars = 'ID') 
  
  fordset <- dset1[ !is.na(dset1$value),]
  
  fordset <- fordset[,-c(2)]
  
  # manually adding missing values wrt number of persons who didn't answer anything to all the questions
  fordset1 <- rbind(fordset, dset1missing_index)
  
  return(fordset1)
  
}

# loading data

# data used in this script is the october 1 survey data in the wordings version (responses are in full answers and not in numbers)

file_with_path <- "C:/Users/Flynn/Desktop/ip/final_data/oct_1_wordings.csv" 

data <- read_csv(file_with_path, skip = 1) %>% data.frame()
data1 <- read_csv(file_with_path, skip = 2) %>% data.frame()
colnames(data1) <- colnames(data)
data <- data1

##################################################################################################################

# Renaming columns

renamed <- data %>% dplyr::rename('PTSD_unwanted_memories_3.2.1' = "In.this.subsection..there.is.a.list.of.reactions.that.people.may.have.in.response.to.a.very.stressful.experience..Keeping.in.mind.your.worst.most.stressful.event.s..related.to.COVID.19..how.much.were.you.bothered.by.the.following.in.the.PAST.MONTH....1..Repeated..disturbing..and.unwanted.memories.of.the.stressful.experience.",
                                  'PTSD_disturbing_dreams_3.2.2' = "In.this.subsection..there.is.a.list.of.reactions.that.people.may.have.in.response.to.a.very.stressful.experience..Keeping.in.mind.your.worst.most.stressful.event.s..related.to.COVID.19..how.much.were.you.bothered.by.the.following.in.the.PAST.MONTH....2..Repeated..disturbing.dreams.of.the.stressful.experience.",
                                  'PTSD_actually_happening_again_3.2.3' = "In.this.subsection..there.is.a.list.of.reactions.that.people.may.have.in.response.to.a.very.stressful.experience..Keeping.in.mind.your.worst.most.stressful.event.s..related.to.COVID.19..how.much.were.you.bothered.by.the.following.in.the.PAST.MONTH....3..Suddenly.feeling.or.acting.as.if.the.stressful.experience.were.actually.happening.again..as.if.you.were.actually.back.there.reliving.it..",
                                  'PTSD_upset_when_reminded_3.2.4' =  "In.this.subsection..there.is.a.list.of.reactions.that.people.may.have.in.response.to.a.very.stressful.experience..Keeping.in.mind.your.worst.most.stressful.event.s..related.to.COVID.19..how.much.were.you.bothered.by.the.following.in.the.PAST.MONTH....4..Feeling.very.upset.when.something.reminded.you.of.the.stressful.experience.",
                                  'PTSD_physical_reaction_reminded_3.2.5' = "In.this.subsection..there.is.a.list.of.reactions.that.people.may.have.in.response.to.a.very.stressful.experience..Keeping.in.mind.your.worst.most.stressful.event.s..related.to.COVID.19..how.much.were.you.bothered.by.the.following.in.the.PAST.MONTH....5..Having.strong.physical.reactions.when.something.reminded.you.of.the.stressful.experience..for.example..heart.pounding..trouble.breathing..sweating..",
                                  'PTSD_avoiding_memories_3.2.6' = "In.this.subsection..there.is.a.list.of.reactions.that.people.may.have.in.response.to.a.very.stressful.experience..Keeping.in.mind.your.worst.most.stressful.event.s..related.to.COVID.19..how.much.were.you.bothered.by.the.following.in.the.PAST.MONTH....6..Avoiding.memories..thoughts..or.feelings.related.to.the.stressful.experience.",
                                  'PTSD_avoiding_externam_reminded_3.2.7' = "In.this.subsection..there.is.a.list.of.reactions.that.people.may.have.in.response.to.a.very.stressful.experience..Keeping.in.mind.your.worst.most.stressful.event.s..related.to.COVID.19..how.much.were.you.bothered.by.the.following.in.the.PAST.MONTH....7..Avoiding.external.reminders.of.the.stressful.experience..for.example..people..places..conversations..activities..objects..or.situations..",
                                  'PTSD_trouble_remembering_3.2.8' = "In.this.subsection..there.is.a.list.of.reactions.that.people.may.have.in.response.to.a.very.stressful.experience..Keeping.in.mind.your.worst.most.stressful.event.s..related.to.COVID.19..how.much.were.you.bothered.by.the.following.in.the.PAST.MONTH....8..Trouble.remembering.important.parts.of.the.stressful.experience.",
                                  'PTSD_negative_belief_self_3.2.9' = "In.this.subsection..there.is.a.list.of.reactions.that.people.may.have.in.response.to.a.very.stressful.experience..Keeping.in.mind.your.worst.most.stressful.event.s..related.to.COVID.19..how.much.were.you.bothered.by.the.following.in.the.PAST.MONTH....9..Having.strong.negative.beliefs.about.yourself..other.people..or.the.world..for.example..having.thoughts.such.as..I.am.bad..there.is.something.seriously.wrong.with.me..no.one.can.be.trusted..the.world.is.completely.dangerous..",
                                  'PTSD_blaming_self_3.2.10' = "In.this.subsection..there.is.a.list.of.reactions.that.people.may.have.in.response.to.a.very.stressful.experience..Keeping.in.mind.your.worst.most.stressful.event.s..related.to.COVID.19..how.much.were.you.bothered.by.the.following.in.the.PAST.MONTH....10..Blaming.yourself.or.someone.else.for.the.stressful.experience.or.what.happened.after.it.",
                                  'PTSD_negative_feelings_3.2.11' = "In.this.subsection..there.is.a.list.of.reactions.that.people.may.have.in.response.to.a.very.stressful.experience..Keeping.in.mind.your.worst.most.stressful.event.s..related.to.COVID.19..how.much.were.you.bothered.by.the.following.in.the.PAST.MONTH....11..Having.strong.negative.feelings.such.as.fear..horror..anger..guilt..or.shame.",
                                  'PTSD_loss_of_interest_3.2.12' = "In.this.subsection..there.is.a.list.of.reactions.that.people.may.have.in.response.to.a.very.stressful.experience..Keeping.in.mind.your.worst.most.stressful.event.s..related.to.COVID.19..how.much.were.you.bothered.by.the.following.in.the.PAST.MONTH....12..Loss.of.interest.in.activities.that.you.used.to.enjoy.",
                                  'PTSD_distant_from_people_3.2.13' = "In.this.subsection..there.is.a.list.of.reactions.that.people.may.have.in.response.to.a.very.stressful.experience..Keeping.in.mind.your.worst.most.stressful.event.s..related.to.COVID.19..how.much.were.you.bothered.by.the.following.in.the.PAST.MONTH....13..Feeling.distant.or.cut.off.from.other.people.",
                                  'PTSD_trouble_exp_positive_feelings_3.2.14' = "In.this.subsection..there.is.a.list.of.reactions.that.people.may.have.in.response.to.a.very.stressful.experience..Keeping.in.mind.your.worst.most.stressful.event.s..related.to.COVID.19..how.much.were.you.bothered.by.the.following.in.the.PAST.MONTH....14..Trouble.experiencing.positive.feelings..for.example..being.unable.to.feel.happiness.or.have.loving.feelings.for.people.close.to.you..",
                                  'PTSD_irritable_behavior_3.2.15' = "In.this.subsection..there.is.a.list.of.reactions.that.people.may.have.in.response.to.a.very.stressful.experience..Keeping.in.mind.your.worst.most.stressful.event.s..related.to.COVID.19..how.much.were.you.bothered.by.the.following.in.the.PAST.MONTH....15..Irritable.behavior..angry.outbursts..or.acting.aggressively.",
                                  'PTSD_taking_too_many_risk_3.2.16' = "In.this.subsection..there.is.a.list.of.reactions.that.people.may.have.in.response.to.a.very.stressful.experience..Keeping.in.mind.your.worst.most.stressful.event.s..related.to.COVID.19..how.much.were.you.bothered.by.the.following.in.the.PAST.MONTH....16..Taking.too.many.risks.or.doing.things.that.could.cause.you.harm.",
                                  'PTSD_super_alert_3.2.17' = "In.this.subsection..there.is.a.list.of.reactions.that.people.may.have.in.response.to.a.very.stressful.experience..Keeping.in.mind.your.worst.most.stressful.event.s..related.to.COVID.19..how.much.were.you.bothered.by.the.following.in.the.PAST.MONTH....17..Being..super.alert..or.watchful.or.on.guard.",
                                  'PTSD_easily_startled_3.2.18' = "In.this.subsection..there.is.a.list.of.reactions.that.people.may.have.in.response.to.a.very.stressful.experience..Keeping.in.mind.your.worst.most.stressful.event.s..related.to.COVID.19..how.much.were.you.bothered.by.the.following.in.the.PAST.MONTH....18..Feeling.jumpy.or.easily.startled.",
                                  'PTSD_difficulty_concentrating_3.2.19' = "In.this.subsection..there.is.a.list.of.reactions.that.people.may.have.in.response.to.a.very.stressful.experience..Keeping.in.mind.your.worst.most.stressful.event.s..related.to.COVID.19..how.much.were.you.bothered.by.the.following.in.the.PAST.MONTH....19..Having.difficulty.concentrating.",
                                  'PTSD_trouble_asleep_3.2.20' = "In.this.subsection..there.is.a.list.of.reactions.that.people.may.have.in.response.to.a.very.stressful.experience..Keeping.in.mind.your.worst.most.stressful.event.s..related.to.COVID.19..how.much.were.you.bothered.by.the.following.in.the.PAST.MONTH....20..Trouble.falling.or.staying.asleep.",
                                  'Depression_little_interest_3.1.1' = "Over.the.last.2.weeks..how.often.have.you.been.bothered.by.the.following.problems....1..Little.interest.or.pleasure.in.doing.things",
                                  'Depression_feeling_down_3.1.2' = "Over.the.last.2.weeks..how.often.have.you.been.bothered.by.the.following.problems....2..Feeling.down..depressed.or.hopeless",
                                  'Depression_trouble_sleeping_3.1.3' = "Over.the.last.2.weeks..how.often.have.you.been.bothered.by.the.following.problems....3..Trouble.falling.asleep..staying.asleep..or.sleeping.too.much",
                                  'Depression_feeling_tired_3.1.4' = "Over.the.last.2.weeks..how.often.have.you.been.bothered.by.the.following.problems....4..Feeling.tired.or.having.little.energy" ,
                                  'Depression_poor_appetite_3.1.5' = "Over.the.last.2.weeks..how.often.have.you.been.bothered.by.the.following.problems....5..Poor.appetite.or.overeating",
                                  'Depression_feelin_bad_about_self_3.1.6' = "Over.the.last.2.weeks..how.often.have.you.been.bothered.by.the.following.problems....6..Feeling.bad.about.yourself...or.that.you.re.a.failure.or.have.let.yourself.or.your.family.down",
                                  'Depression_trouble_concentrating_3.1.7' = "Over.the.last.2.weeks..how.often.have.you.been.bothered.by.the.following.problems....7..Trouble.concentrating.on.things..such.as.reading.the.newspaper.or.watching.television",
                                  'Depression_fidgety_or_restless_3.1.8' = "Over.the.last.2.weeks..how.often.have.you.been.bothered.by.the.following.problems....8..Moving.or.speaking.so.slowly.that.other.people.could.have.noticed..Or..the.opposite...being.so.fidgety.or.restless.that.you.have.been.moving.around.a.lot.more.than.usual",
                                  'Depression_thoughts_about_hurting_self_3.1.9' = "Over.the.last.2.weeks..how.often.have.you.been.bothered.by.the.following.problems....9..Thoughts.that.you.would.be.better.off.dead.or.of.hurting.yourself.in.some.way",
                                  'underlyinh_health_condition_2.2' = "Do.you.have.any.underlying.health.condition.s..that.concerned.you.while.working.with.patients.during.the.COVID.19.pandemic.",
                                  'living_changes_cooncern_covid_2.5' = "Have.you.ever.made.any.changes.to.your.living.arrangements..even.if.temporarily..during.the.pandemic..due.to.concerns.of.transmitting.COVID.19.to.your.family...others.",
                                  'work_extra_hours_2.7' = "During.the.month.when.your.practice.received.the.largest.volume.of.COVID.19.patients..did.you.work.extra.hours.to.care.for.COVID.19.patients.",
                                  'work_outside_normal_resp_2.8' = "During.the.month.when.your.practice.received.the.largest.volume.of.COVID.19.patients..did.you.do.any.work.related.to.COVID.19.patients.that.was.outside.of.the.normal.scope.of.your.clinical.responsibilities.",
                                  'job_difficult_dueto_COVID_2.9' = "During.the.month.when.your.practice.received.the.largest.volume.of.COVID.19.patients..how.much.more.difficult.did.your.job.become.due.to.COVID.19.",
                                  'turnoverintent_switch_teams_2.15.1' = "What.is.the.likelihood.that.you.will.do.the.following.in.the.next.two.years....1..Switch.units.teams",
                                  'turnoverintent_leave_current_2.15.2' = "What.is.the.likelihood.that.you.will.do.the.following.in.the.next.two.years....2..Leave.your.current.employer",
                                  'turnoverintent_leave_healthcare_2.15.3' = "What.is.the.likelihood.that.you.will.do.the.following.in.the.next.two.years....3..Leave.the.field.of.healthcare.entirely",
                                  'level_of_burnout_3.3' = "Overall..based.on.your.definition.of.burnout..how.would.you.rate.your.level.of.burnout.",
                                  'Primary_work_setting_5.11' = "What.is.your.primary.work.setting....Selected.Choice",
                                  'Primary_work_setting_other_txt_5.11.7' = "What.is.your.primary.work.setting....Others..please.specify......Text",
                                  'Work_setting_emergency_dept_5.11.2.1'  = "What.is.your.work.setting.within.the.hospital...Select.all.that.apply....Selected.Choice...Emergency.Department",
                                  'work_setting_general_floor_5.11.2.2' = "What.is.your.work.setting.within.the.hospital...Select.all.that.apply....Selected.Choice...General.Floor",
                                  'work_setting_step_down_unit_5.11.2.3' = "What.is.your.work.setting.within.the.hospital...Select.all.that.apply....Selected.Choice...Step.Down.Unit",
                                  'work_setting_ICU_5.11.2.4' = "What.is.your.work.setting.within.the.hospital...Select.all.that.apply....Selected.Choice...Intensive.Care.Unit..any.type.",
                                  'work_setting_operating_room_5.11.2.5' =  "What.is.your.work.setting.within.the.hospital...Select.all.that.apply....Selected.Choice...Operating.Room",
                                  'work_setting_other_5.11.2.6' = "What.is.your.work.setting.within.the.hospital...Select.all.that.apply....Selected.Choice...Other..please.mention..",
                                  'work_setting_other_5.11.2.6_txt' = "What.is.your.work.setting.within.the.hospital...Select.all.that.apply....Other..please.mention......Text",
                                  'Imply_to_consent' = "Please.review.the.Consent.Information.Sheet.for.this.study..By.selecting..Yes...you.imply.consent.to.participate.in.the.survey.",
                                  'Stage_in_career' = "At.what.stage.are.you.in.your.career...check.one.",
                                  'Resilience_adapt_1.1' = "Please.indicate.how.much.you.agree.with.the.following.statements.as.they.apply.to.you..If.a.particular.situation.has.not.occurred.recently..answer.according.to.how.you.think.you.would.have.felt....1..Able.to.adapt.when.changes.occur.",
                                  'Resilience_can_deal_1.2' = "Please.indicate.how.much.you.agree.with.the.following.statements.as.they.apply.to.you..If.a.particular.situation.has.not.occurred.recently..answer.according.to.how.you.think.you.would.have.felt....2..Can.deal.with.whatever.comes",
                                  'Resilience_humorous_side_1.3' = "Please.indicate.how.much.you.agree.with.the.following.statements.as.they.apply.to.you..If.a.particular.situation.has.not.occurred.recently..answer.according.to.how.you.think.you.would.have.felt....3..Tries.to.see.humorous.side.of.problems",
                                  'Resilience_stress_strengthen_1.4' = "Please.indicate.how.much.you.agree.with.the.following.statements.as.they.apply.to.you..If.a.particular.situation.has.not.occurred.recently..answer.according.to.how.you.think.you.would.have.felt....4..Coping.with.stress.can.strengthen.me",
                                  'Resilience_bounce_after_ill_1.5' = "Please.indicate.how.much.you.agree.with.the.following.statements.as.they.apply.to.you..If.a.particular.situation.has.not.occurred.recently..answer.according.to.how.you.think.you.would.have.felt....5..Tend.to.bounce.back.after.illness..injury..or.other.hardships.",
                                  'Resilience_goals_desp_obst_1.6' = "Please.indicate.how.much.you.agree.with.the.following.statements.as.they.apply.to.you..If.a.particular.situation.has.not.occurred.recently..answer.according.to.how.you.think.you.would.have.felt....6..Can.achieve.goals.despite.obstacles",
                                  'Resilience_focus_under_pres_1.7' = "Please.indicate.how.much.you.agree.with.the.following.statements.as.they.apply.to.you..If.a.particular.situation.has.not.occurred.recently..answer.according.to.how.you.think.you.would.have.felt....7..Can.stay.focused.under.pressure",
                                  'Resilience_not_disc_by_fail_1.8' = "Please.indicate.how.much.you.agree.with.the.following.statements.as.they.apply.to.you..If.a.particular.situation.has.not.occurred.recently..answer.according.to.how.you.think.you.would.have.felt....8..Not.easily.discouraged.by.failure",
                                  'Resilience_think_self_strong_1.9' = "Please.indicate.how.much.you.agree.with.the.following.statements.as.they.apply.to.you..If.a.particular.situation.has.not.occurred.recently..answer.according.to.how.you.think.you.would.have.felt....9..Thinks.of.self.as.strong.person",
                                  'Resilience_handle_unpleasant_feel_1.10' = "Please.indicate.how.much.you.agree.with.the.following.statements.as.they.apply.to.you..If.a.particular.situation.has.not.occurred.recently..answer.according.to.how.you.think.you.would.have.felt....10..Can.handle.unpleasant.feelings",
                                  'Direcly_worked_COVID_patients_2.1' = "Have.you.directly.worked.with.COVID.19.patients.",
                                  'Months_worked_with_COVID_pat_2.1.1' = "Altogether..how.many.months.have.you.worked.with.COVID.19.patients.since.the.start.of.pandemic....Months",
                                  'self_or_partner_preganant_2.3.1' = "Were..are..you.or.your.partner.pregnant.or.had..have..a.new.born.while.working.with.patients.during.the.COVID.19.pandemic...Select.all.that.apply....Yes..pregnant",
                                  'Self_or_partner_with_newborn_2.3.2' = "Were..are..you.or.your.partner.pregnant.or.had..have..a.new.born.while.working.with.patients.during.the.COVID.19.pandemic...Select.all.that.apply....Yes..with.a.new.born",
                                  'self_or_part_no_preg_no_newborn_2.3.3' = "Were..are..you.or.your.partner.pregnant.or.had..have..a.new.born.while.working.with.patients.during.the.COVID.19.pandemic...Select.all.that.apply....No",
                                  'Demo_fear_cont_COVID_2.4.1' = "To.what.extent.have.you.experienced.the.following..if.any..during.the.COVID.19.pandemic....Feared.contracting.COVID.19.due.to.working.on.the.frontlines",
                                  'Demo_conc_might_transmit_2.4.2' = "To.what.extent.have.you.experienced.the.following..if.any..during.the.COVID.19.pandemic....Concerned.that.you.might.transmit.COVID.19.to.family..friends..or.relatives",
                                  'Demo_felt_stigmatized_2.4.3' = "To.what.extent.have.you.experienced.the.following..if.any..during.the.COVID.19.pandemic....Felt.stigmatized.by.others.because.you.worked.with.COVID.19.patients",
                                  'Hours_worked_before_covid_2.6' = "Before.COVID.19..how.many.hours.did.you.work.in.an.average.week.",
                                  'Hours_worked_peak_month_2.7.1' = "How.many.hours.did.you.work.per.week.on.average.during.the.month.when.your.practice.received.the.largest.volume.of.COVID.19.patients.",
                                  'Hours_doing_outside_resp_2.8.1' = "On.average..how.many.hours.did.you.work.doing.things.outside.of.the.normal.scope.of.your.clinical.practice.during.the.month.when.your.practice.received.the.largest.volume.of.COVID.19.patients.",
                                  'Sufficient_resources_2.10' = "How.often.did.you.have.sufficient.resources..e.g...PPE..ventilators..testing.kits..etc...to.take.care.of.all.your.COVID.19.patients.during.the.month.when.your.practice.received.the.largest.volume.of.COVID.19.patients.",
                                  'Part_of_decision_making_2.11' = "How.often.were.you.part.of.the.decision.making.process.about.allocating.ICU.bed.and.or.ventilators.during.the.month.when.your.practice.received.the.largest.volume.of.COVID.19.patients.",
                                  'Exposure_suspected_2.12' = "How.many.suspected.or.known.COVID.19.patients.did.you.intubate.or.be.present.for.intubation.during.the.month.when.your.practice.received.the.largest.volume.of.COVID.19.patients...use.your.best.guess.",
                                  'Exposure_aerosol_2.13' = "How.many.aerosol.generating.procedures..e.g...Nebulizer..HFNC..NIPPV..etc....with.suspected.or.known.COVID.19.patients.were.you.exposed.to.during.the.month.when.your.practice.received.the.largest.volume.of.COVID.19.patients...use.your.best.guess.",
                                  'discrim_mistreated_2.14' = "Have.you.felt.mistreated.or.stigmatized.by.your.patients.and.or.their.family.members.because.of.your.race.or.ethnicity.since.the.COVID.19.outbreak.",
                                  'discrim_less_courtesy_2.14.1.1' = "Since.the.COVID.19.outbreak..how.often.have.you.felt.the.following.BECAUSE.OF.YOUR.RACE.or.ETHNICITY....1..Treated.with.less.courtesy",
                                  'discrim_less_respect_2.14.1.2' = "Since.the.COVID.19.outbreak..how.often.have.you.felt.the.following.BECAUSE.OF.YOUR.RACE.or.ETHNICITY....2..Treated.with.less.respect",
                                  'discrim_not_smart_2.14.1.3' = "Since.the.COVID.19.outbreak..how.often.have.you.felt.the.following.BECAUSE.OF.YOUR.RACE.or.ETHNICITY....3..Acted.as.if.you.are.not.smart",
                                  'discrim_afraid_2.14.1.4' = "Since.the.COVID.19.outbreak..how.often.have.you.felt.the.following.BECAUSE.OF.YOUR.RACE.or.ETHNICITY....4..Acted.as.if.they.were.afraid.of.you",
                                  'discrim_dishonest_2.14.1.5' = "Since.the.COVID.19.outbreak..how.often.have.you.felt.the.following.BECAUSE.OF.YOUR.RACE.or.ETHNICITY....5..Acted.as.if.you.are.dishonest",
                                  'discrim_better_2.14.1.6' = "Since.the.COVID.19.outbreak..how.often.have.you.felt.the.following.BECAUSE.OF.YOUR.RACE.or.ETHNICITY....6..Acted.as.if.they.are.better.than.you",
                                  'discrim_call_names_2.14.1.7' = "Since.the.COVID.19.outbreak..how.often.have.you.felt.the.following.BECAUSE.OF.YOUR.RACE.or.ETHNICITY....7..Called.names",
                                  'discrim_threat_harassed_2.14.1.8' = "Since.the.COVID.19.outbreak..how.often.have.you.felt.the.following.BECAUSE.OF.YOUR.RACE.or.ETHNICITY....8..Threatened.or.harassed",
                                  'discrim_describe_feel_stig_txt_2.14.2' = "If.you.are.willing..describe.exactly.what.happened.that.made.you.feel.like.you.were.being.stigmatized.or.mistreated.by.the.patients.",
                                  'discrim_additional_exp_2.14.3' = "Have.you.had.additional.experiences.like.this.that.you.want.to.share.",
                                  'discrim_describe_feel_stig_txt_2.14.4' = "Describe.exactly.what.happened.that.made.you.feel.like.you.were.being.stigmatized.or.mistreated.by.the.patients.",
                                  'discrim_additional_exp_2.14.5' = "Have.you.had.additional.experiences.like.this.that.you.want.to.share._1",
                                  'discrim_describe_feel_stig_txt_2.14.6' = "Describe.exactly.what.happened.that.made.you.feel.like.you.were.being.stigmatized.or.mistreated.by.the.patients._1",
                                  'org_supp_even_wrkld_4.1.1' = "During.the.COVID.19.pandemic..did.your.employer.take.the.following.actions....1..Adequately.implement.shifting.of.tasks.to.evenly.distribute.your.workload",
                                  'org_supp_psych_hotline_4.1.2' = "During.the.COVID.19.pandemic..did.your.employer.take.the.following.actions....2..Arrange.for.a.psychological.support.hotline.for.the.employees..wellbeing.at.your.organization",
                                  'org_supp_training_4.1.3' = "During.the.COVID.19.pandemic..did.your.employer.take.the.following.actions....3..Offer.online.training.programs.to.their.employees.on.psychosocial.care.principles",
                                  'org_supp_values_contrib_4.2.1' = "In.answering.the.following.questions..think.about.your.experience.and.indicate.how.your.employer...organization.have.supported.you.since.the.outbreak.of.the.COVID.19.pandemic..Use.your.best.judgement.to.indicate.your.opinion....1..The.organization.values.my.contribution.to.its.well.being.",
                                  'org_supp_care_wellbeing_4.2.2' = "In.answering.the.following.questions..think.about.your.experience.and.indicate.how.your.employer...organization.have.supported.you.since.the.outbreak.of.the.COVID.19.pandemic..Use.your.best.judgement.to.indicate.your.opinion....2..The.organization.really.cares.about.my.well.being.",
                                  'org_supp_wrk_satisfaction_4.2.3' = "In.answering.the.following.questions..think.about.your.experience.and.indicate.how.your.employer...organization.have.supported.you.since.the.outbreak.of.the.COVID.19.pandemic..Use.your.best.judgement.to.indicate.your.opinion....3..The.organization.cares.about.my.general.satisfaction.at.work.",
                                  'org_supp_accomplishment_4.2.4' = "In.answering.the.following.questions..think.about.your.experience.and.indicate.how.your.employer...organization.have.supported.you.since.the.outbreak.of.the.COVID.19.pandemic..Use.your.best.judgement.to.indicate.your.opinion....4..The.organization.takes.pride.in.my.accomplishments.at.work.",
                                  'org_supp_fail_app_effort_4.2.5' = "In.answering.the.following.questions..think.about.your.experience.and.indicate.how.your.employer...organization.have.supported.you.since.the.outbreak.of.the.COVID.19.pandemic..Use.your.best.judgement.to.indicate.your.opinion....5..The.organization.fails.to.appreciate.any.extra.effort.from.me.",
                                  'org_supp_ignore_complaint_4.2.6' = "In.answering.the.following.questions..think.about.your.experience.and.indicate.how.your.employer...organization.have.supported.you.since.the.outbreak.of.the.COVID.19.pandemic..Use.your.best.judgement.to.indicate.your.opinion....6..The.organization.would.ignore.any.complaint.from.me.",
                                  'org_supp_fail_to_notice_4.2.7' = "In.answering.the.following.questions..think.about.your.experience.and.indicate.how.your.employer...organization.have.supported.you.since.the.outbreak.of.the.COVID.19.pandemic..Use.your.best.judgement.to.indicate.your.opinion....7..Even.if.l.did.the.best.job.possible..the.organization.would.fail.to.notice.",
                                  'org_supp_little_concern_4.2.8' = "In.answering.the.following.questions..think.about.your.experience.and.indicate.how.your.employer...organization.have.supported.you.since.the.outbreak.of.the.COVID.19.pandemic..Use.your.best.judgement.to.indicate.your.opinion....8..The.organization.shows.very.little.concern.for.me.",
                                  'supp_special_person_4.3.1' = "In.answering.the.following.questions..think.about.your.current.relationships.with.friends..family.members..and.special.someone..Please.indicate.to.what.extent.each.statement.describes.your.current.relationships.with.other.people..Use.the.following.scale.to.indicate.your.opinion....1..There.is.a.special.person.with.whom.I.can.share.my.joys.and.sorrows.",
                                  'supp_count_on_friends_4.3.2' = "In.answering.the.following.questions..think.about.your.current.relationships.with.friends..family.members..and.special.someone..Please.indicate.to.what.extent.each.statement.describes.your.current.relationships.with.other.people..Use.the.following.scale.to.indicate.your.opinion....2..I.can.count.on.my.friends.when.things.go.wrong.",
                                  'supp_emotional_help_4.3.3' = "In.answering.the.following.questions..think.about.your.current.relationships.with.friends..family.members..and.special.someone..Please.indicate.to.what.extent.each.statement.describes.your.current.relationships.with.other.people..Use.the.following.scale.to.indicate.your.opinion....3..I.get.the.emotional.help.and.support.I.need.from.my.family.",
                                  'cope_stress_activities_4.4.1' = "In.this.subsection..each.item.says.something.about.a.particular.way.of.coping..Please.answer.to.what.extent.you.ve.been.doing.what.the.item.says.to.cope.with.the.COVID.19.PANDEMIC.how.much.or.how.frequently..Don.t.answer.on.the.basis.of.whether.it.seems.to.be.working.or.not.just.whether.or.not.you.re.doing.it..Use.these.response.choices...Make.your.answers.as.true.FOR.YOU.as.you.can....1..I.ve.been.turning.to.work.or.other.activities.to.take.my.mind.off.things.",
                                  'cope_stress_conc_doing_4.4.2' = "In.this.subsection..each.item.says.something.about.a.particular.way.of.coping..Please.answer.to.what.extent.you.ve.been.doing.what.the.item.says.to.cope.with.the.COVID.19.PANDEMIC.how.much.or.how.frequently..Don.t.answer.on.the.basis.of.whether.it.seems.to.be.working.or.not.just.whether.or.not.you.re.doing.it..Use.these.response.choices...Make.your.answers.as.true.FOR.YOU.as.you.can....2..I.ve.been.concentrating.my.efforts.on.doing.something.about.the.situation.I.m.in.",
                                  'cope_stress_isint_real_4.4.3' = "In.this.subsection..each.item.says.something.about.a.particular.way.of.coping..Please.answer.to.what.extent.you.ve.been.doing.what.the.item.says.to.cope.with.the.COVID.19.PANDEMIC.how.much.or.how.frequently..Don.t.answer.on.the.basis.of.whether.it.seems.to.be.working.or.not.just.whether.or.not.you.re.doing.it..Use.these.response.choices...Make.your.answers.as.true.FOR.YOU.as.you.can....3..I.ve.been.saying.to.myself..this.isn.t.real..",
                                  'cope_stress_drugs_alc_4.4.4' = "In.this.subsection..each.item.says.something.about.a.particular.way.of.coping..Please.answer.to.what.extent.you.ve.been.doing.what.the.item.says.to.cope.with.the.COVID.19.PANDEMIC.how.much.or.how.frequently..Don.t.answer.on.the.basis.of.whether.it.seems.to.be.working.or.not.just.whether.or.not.you.re.doing.it..Use.these.response.choices...Make.your.answers.as.true.FOR.YOU.as.you.can....4..I.ve.been.using.alcohol.or.other.drugs.to.make.myself.feel.better.",
                                  'cope_stress_emo_supp_4.4.5' = "In.this.subsection..each.item.says.something.about.a.particular.way.of.coping..Please.answer.to.what.extent.you.ve.been.doing.what.the.item.says.to.cope.with.the.COVID.19.PANDEMIC.how.much.or.how.frequently..Don.t.answer.on.the.basis.of.whether.it.seems.to.be.working.or.not.just.whether.or.not.you.re.doing.it..Use.these.response.choices...Make.your.answers.as.true.FOR.YOU.as.you.can....5..I.ve.been.getting.emotional.support.from.others.",
                                  'cope_stress_giving_up_4.4.6' = "In.this.subsection..each.item.says.something.about.a.particular.way.of.coping..Please.answer.to.what.extent.you.ve.been.doing.what.the.item.says.to.cope.with.the.COVID.19.PANDEMIC.how.much.or.how.frequently..Don.t.answer.on.the.basis.of.whether.it.seems.to.be.working.or.not.just.whether.or.not.you.re.doing.it..Use.these.response.choices...Make.your.answers.as.true.FOR.YOU.as.you.can....6..I.ve.been.giving.up.trying.to.deal.with.it.",
                                  'cope_stress_taking_action_4.4.7' = "In.this.subsection..each.item.says.something.about.a.particular.way.of.coping..Please.answer.to.what.extent.you.ve.been.doing.what.the.item.says.to.cope.with.the.COVID.19.PANDEMIC.how.much.or.how.frequently..Don.t.answer.on.the.basis.of.whether.it.seems.to.be.working.or.not.just.whether.or.not.you.re.doing.it..Use.these.response.choices...Make.your.answers.as.true.FOR.YOU.as.you.can....7..I.ve.been.taking.action.to.try.to.make.the.situation.better.",
                                  'cope_stress_refuse_beleive_4.4.8' = "In.this.subsection..each.item.says.something.about.a.particular.way.of.coping..Please.answer.to.what.extent.you.ve.been.doing.what.the.item.says.to.cope.with.the.COVID.19.PANDEMIC.how.much.or.how.frequently..Don.t.answer.on.the.basis.of.whether.it.seems.to.be.working.or.not.just.whether.or.not.you.re.doing.it..Use.these.response.choices...Make.your.answers.as.true.FOR.YOU.as.you.can....8..I.ve.been.refusing.to.believe.that.it.has.happened.",
                                  'cope_stress_saying_things_4.4.9' = "In.this.subsection..each.item.says.something.about.a.particular.way.of.coping..Please.answer.to.what.extent.you.ve.been.doing.what.the.item.says.to.cope.with.the.COVID.19.PANDEMIC.how.much.or.how.frequently..Don.t.answer.on.the.basis.of.whether.it.seems.to.be.working.or.not.just.whether.or.not.you.re.doing.it..Use.these.response.choices...Make.your.answers.as.true.FOR.YOU.as.you.can....9..I.ve.been.saying.things.to.let.my.unpleasant.feelings.escape.",
                                  'cope_stress_help_from_4.4.10' = "In.this.subsection..each.item.says.something.about.a.particular.way.of.coping..Please.answer.to.what.extent.you.ve.been.doing.what.the.item.says.to.cope.with.the.COVID.19.PANDEMIC.how.much.or.how.frequently..Don.t.answer.on.the.basis.of.whether.it.seems.to.be.working.or.not.just.whether.or.not.you.re.doing.it..Use.these.response.choices...Make.your.answers.as.true.FOR.YOU.as.you.can....10..I.ve.been.getting.help.and.advice.from.other.people.",
                                  'cope_stress_alc_drugs_4.4.11' = "In.this.subsection..each.item.says.something.about.a.particular.way.of.coping..Please.answer.to.what.extent.you.ve.been.doing.what.the.item.says.to.cope.with.the.COVID.19.PANDEMIC.how.much.or.how.frequently..Don.t.answer.on.the.basis.of.whether.it.seems.to.be.working.or.not.just.whether.or.not.you.re.doing.it..Use.these.response.choices...Make.your.answers.as.true.FOR.YOU.as.you.can....11..I.ve.been.using.alcohol.or.other.drugs.to.help.me.get.through.it.",
                                  'cope_stress_see_positive_4.4.12' = "In.this.subsection..each.item.says.something.about.a.particular.way.of.coping..Please.answer.to.what.extent.you.ve.been.doing.what.the.item.says.to.cope.with.the.COVID.19.PANDEMIC.how.much.or.how.frequently..Don.t.answer.on.the.basis.of.whether.it.seems.to.be.working.or.not.just.whether.or.not.you.re.doing.it..Use.these.response.choices...Make.your.answers.as.true.FOR.YOU.as.you.can....12..I.ve.been.trying.to.see.it.in.a.different.light..to.make.it.seem.more.positive.",
                                  'cope_stress_crit_self_4.4.13' = "In.this.subsection..each.item.says.something.about.a.particular.way.of.coping..Please.answer.to.what.extent.you.ve.been.doing.what.the.item.says.to.cope.with.the.COVID.19.PANDEMIC.how.much.or.how.frequently..Don.t.answer.on.the.basis.of.whether.it.seems.to.be.working.or.not.just.whether.or.not.you.re.doing.it..Use.these.response.choices...Make.your.answers.as.true.FOR.YOU.as.you.can....13..I.ve.been.criticizing.myself.",
                                  'cope_stress_stratergy_4.4.14' = "In.this.subsection..each.item.says.something.about.a.particular.way.of.coping..Please.answer.to.what.extent.you.ve.been.doing.what.the.item.says.to.cope.with.the.COVID.19.PANDEMIC.how.much.or.how.frequently..Don.t.answer.on.the.basis.of.whether.it.seems.to.be.working.or.not.just.whether.or.not.you.re.doing.it..Use.these.response.choices...Make.your.answers.as.true.FOR.YOU.as.you.can....14..I.ve.been.trying.to.come.up.with.a.strategy.about.what.to.do.",
                                  'cope_stress_comfort_4.4.15' = "In.this.subsection..each.item.says.something.about.a.particular.way.of.coping..Please.answer.to.what.extent.you.ve.been.doing.what.the.item.says.to.cope.with.the.COVID.19.PANDEMIC.how.much.or.how.frequently..Don.t.answer.on.the.basis.of.whether.it.seems.to.be.working.or.not.just.whether.or.not.you.re.doing.it..Use.these.response.choices...Make.your.answers.as.true.FOR.YOU.as.you.can....15..I.ve.been.getting.comfort.and.understanding.from.someone.",
                                  'cope_stress_give_up_cope_4.4.16' = "In.this.subsection..each.item.says.something.about.a.particular.way.of.coping..Please.answer.to.what.extent.you.ve.been.doing.what.the.item.says.to.cope.with.the.COVID.19.PANDEMIC.how.much.or.how.frequently..Don.t.answer.on.the.basis.of.whether.it.seems.to.be.working.or.not.just.whether.or.not.you.re.doing.it..Use.these.response.choices...Make.your.answers.as.true.FOR.YOU.as.you.can....16..I.ve.been.giving.up.the.attempt.to.cope.",
                                  'cope_stress_looking_som_good_4.4.17' = "In.this.subsection..each.item.says.something.about.a.particular.way.of.coping..Please.answer.to.what.extent.you.ve.been.doing.what.the.item.says.to.cope.with.the.COVID.19.PANDEMIC.how.much.or.how.frequently..Don.t.answer.on.the.basis.of.whether.it.seems.to.be.working.or.not.just.whether.or.not.you.re.doing.it..Use.these.response.choices...Make.your.answers.as.true.FOR.YOU.as.you.can....17..I.ve.been.looking.for.something.good.in.what.is.happening.",
                                  'cope_stress_joke_4.4.18' = "In.this.subsection..each.item.says.something.about.a.particular.way.of.coping..Please.answer.to.what.extent.you.ve.been.doing.what.the.item.says.to.cope.with.the.COVID.19.PANDEMIC.how.much.or.how.frequently..Don.t.answer.on.the.basis.of.whether.it.seems.to.be.working.or.not.just.whether.or.not.you.re.doing.it..Use.these.response.choices...Make.your.answers.as.true.FOR.YOU.as.you.can....18..I.ve.been.making.jokes.about.it.",
                                  'cope_stress_tv_daydreaming_4.4.19' = "In.this.subsection..each.item.says.something.about.a.particular.way.of.coping..Please.answer.to.what.extent.you.ve.been.doing.what.the.item.says.to.cope.with.the.COVID.19.PANDEMIC.how.much.or.how.frequently..Don.t.answer.on.the.basis.of.whether.it.seems.to.be.working.or.not.just.whether.or.not.you.re.doing.it..Use.these.response.choices...Make.your.answers.as.true.FOR.YOU.as.you.can....19..I.ve.been.doing.something.to.think.about.it.less..such.as.going.to.movies..watching.TV..reading..daydreaming..sleeping..or.shopping.",
                                  'cope_stress_accept_happ_4.4.20' = "In.this.subsection..each.item.says.something.about.a.particular.way.of.coping..Please.answer.to.what.extent.you.ve.been.doing.what.the.item.says.to.cope.with.the.COVID.19.PANDEMIC.how.much.or.how.frequently..Don.t.answer.on.the.basis.of.whether.it.seems.to.be.working.or.not.just.whether.or.not.you.re.doing.it..Use.these.response.choices...Make.your.answers.as.true.FOR.YOU.as.you.can....20..I.ve.been.accepting.the.reality.of.the.fact.that.it.has.happened.",
                                  'cope_stress_exp_neg_4.4.21' = "In.this.subsection..each.item.says.something.about.a.particular.way.of.coping..Please.answer.to.what.extent.you.ve.been.doing.what.the.item.says.to.cope.with.the.COVID.19.PANDEMIC.how.much.or.how.frequently..Don.t.answer.on.the.basis.of.whether.it.seems.to.be.working.or.not.just.whether.or.not.you.re.doing.it..Use.these.response.choices...Make.your.answers.as.true.FOR.YOU.as.you.can....21..I.ve.been.expressing.my.negative.feelings.",
                                  'cope_stress_religion_4.4.22' = "In.this.subsection..each.item.says.something.about.a.particular.way.of.coping..Please.answer.to.what.extent.you.ve.been.doing.what.the.item.says.to.cope.with.the.COVID.19.PANDEMIC.how.much.or.how.frequently..Don.t.answer.on.the.basis.of.whether.it.seems.to.be.working.or.not.just.whether.or.not.you.re.doing.it..Use.these.response.choices...Make.your.answers.as.true.FOR.YOU.as.you.can....22..I.ve.been.trying.to.find.comfort.in.my.religion.or.spiritual.beliefs.",
                                  'cope_stress_trying_help_4.4.23' = "In.this.subsection..each.item.says.something.about.a.particular.way.of.coping..Please.answer.to.what.extent.you.ve.been.doing.what.the.item.says.to.cope.with.the.COVID.19.PANDEMIC.how.much.or.how.frequently..Don.t.answer.on.the.basis.of.whether.it.seems.to.be.working.or.not.just.whether.or.not.you.re.doing.it..Use.these.response.choices...Make.your.answers.as.true.FOR.YOU.as.you.can....23..I.ve.been.trying.to.get.advice.or.help.from.other.people.about.what.to.do.",
                                  'cope_stress_live_with_it_4.4.24' = "In.this.subsection..each.item.says.something.about.a.particular.way.of.coping..Please.answer.to.what.extent.you.ve.been.doing.what.the.item.says.to.cope.with.the.COVID.19.PANDEMIC.how.much.or.how.frequently..Don.t.answer.on.the.basis.of.whether.it.seems.to.be.working.or.not.just.whether.or.not.you.re.doing.it..Use.these.response.choices...Make.your.answers.as.true.FOR.YOU.as.you.can....24..I.ve.been.learning.to.live.with.it.",
                                  'cope_stress_think_steps_4.4.25' = "In.this.subsection..each.item.says.something.about.a.particular.way.of.coping..Please.answer.to.what.extent.you.ve.been.doing.what.the.item.says.to.cope.with.the.COVID.19.PANDEMIC.how.much.or.how.frequently..Don.t.answer.on.the.basis.of.whether.it.seems.to.be.working.or.not.just.whether.or.not.you.re.doing.it..Use.these.response.choices...Make.your.answers.as.true.FOR.YOU.as.you.can....25..I.ve.been.thinking.hard.about.what.steps.to.take.",
                                  'cope_stress_blaming_self_4.4.26' = "In.this.subsection..each.item.says.something.about.a.particular.way.of.coping..Please.answer.to.what.extent.you.ve.been.doing.what.the.item.says.to.cope.with.the.COVID.19.PANDEMIC.how.much.or.how.frequently..Don.t.answer.on.the.basis.of.whether.it.seems.to.be.working.or.not.just.whether.or.not.you.re.doing.it..Use.these.response.choices...Make.your.answers.as.true.FOR.YOU.as.you.can....26..I.ve.been.blaming.myself.for.things.that.happened.",
                                  'cope_stress_praying_4.4.27' = "In.this.subsection..each.item.says.something.about.a.particular.way.of.coping..Please.answer.to.what.extent.you.ve.been.doing.what.the.item.says.to.cope.with.the.COVID.19.PANDEMIC.how.much.or.how.frequently..Don.t.answer.on.the.basis.of.whether.it.seems.to.be.working.or.not.just.whether.or.not.you.re.doing.it..Use.these.response.choices...Make.your.answers.as.true.FOR.YOU.as.you.can....27..I.ve.been.praying.or.meditating.",
                                  'cope_stress_fun_of_situ_4.4.28' = "In.this.subsection..each.item.says.something.about.a.particular.way.of.coping..Please.answer.to.what.extent.you.ve.been.doing.what.the.item.says.to.cope.with.the.COVID.19.PANDEMIC.how.much.or.how.frequently..Don.t.answer.on.the.basis.of.whether.it.seems.to.be.working.or.not.just.whether.or.not.you.re.doing.it..Use.these.response.choices...Make.your.answers.as.true.FOR.YOU.as.you.can....28..I.ve.been.making.fun.of.the.situation.",
                                  'satisfying_event_4.5.1' = "Describe.the.most.satisfying.event.or.moment.that.you.have.experienced.",
                                  'dissatisfying_event_4.5.2' = "Describe.the.most.negative.dissatisfying.event.or.moment.that.you.have.experienced.",
                                  'year_of_birth_5.1' = "What.is.your.year.of.birth.",
                                  'sex_5.2' = "What.is.your.sex...gender.",
                                  'hispanic_or_latino_5.3' = "Do.you.identify.as.Hispanic.or.Latino.",
                                  'race_americanindian_5.4.1' = "Choose.one.or.more.races.that.you.consider.yourself.to.be....Selected.Choice...American.Indian.or.Alaska.Native",
                                  'race_asian_5.4.2' = "Choose.one.or.more.races.that.you.consider.yourself.to.be....Selected.Choice...Asian",
                                  'race_black_5.4.3' = "Choose.one.or.more.races.that.you.consider.yourself.to.be....Selected.Choice...Black.or.African.American",
                                  'race_native_haw_5.4.4' = "Choose.one.or.more.races.that.you.consider.yourself.to.be....Selected.Choice...Native.Hawaiian.or.Pacific.Islander",
                                  'race_white_5.4.5' = "Choose.one.or.more.races.that.you.consider.yourself.to.be....Selected.Choice...White",
                                  'race_others_5.4.6' = "Choose.one.or.more.races.that.you.consider.yourself.to.be....Selected.Choice...Others..please.specify..",
                                  'race_others_txt_5.4.6' = "Choose.one.or.more.races.that.you.consider.yourself.to.be....Others..please.specify.....Text",
                                  'race_asian_east_5.4.1.1' = "If.Asian..please.select.the...Selected.Choice...East.Asia..e.g...China..North.Korea..South.Korea..Taiwan..Japan..etc..",
                                  'race_asian_south_5.4.1.2' = "If.Asian..please.select.the...Selected.Choice...South.Asia..e.g...India..Bangladesh..Pakistan..Afghanistan..Sri.Lanka..etc..",
                                  'race_asian_southeast_5.4.1.3' = "If.Asian..please.select.the...Selected.Choice...Southeast.Asia..e.g...Burma..Cambodia..Thailand..Vietnam..Malaysia..etc..",
                                  'race_asian_west_5.4.1.4' = "If.Asian..please.select.the...Selected.Choice...West.Asia..e.g...Iran..Iraq..Egypt..Kuwait..Saudi.Arabia..etc..",
                                  'race_asian_north_5.4.1.5' = "If.Asian..please.select.the...Selected.Choice...North.Asia..e.g...Russia.",
                                  'race_asian_other_5.4.1.6' = "If.Asian..please.select.the...Selected.Choice...Other..please.specify..",
                                  'race_asian_other_txt_5.4.1.6' = "If.Asian..please.select.the...Other..please.specify.....Text",
                                  'Immigrant_5.5' = "Are.you.a.U.S..immigrant.",
                                  'Country_5.5.1' = "List.of.Countries",
                                  'year_moved_to_US_5.5.2' = "In.what.year.did.you.first.move.to.the.U.S..",
                                  'Relationship_5.6' = "Current.relationship.status.",
                                  'Speciality_Adult_CC_5.7.1' = "As.a.physician..what.is.your.medical.specialty...Please.check.all.that.apply....Adult.Critical.Care",
                                  'Speciality_Anesthesiology_5.7.2' = "As.a.physician..what.is.your.medical.specialty...Please.check.all.that.apply....Anesthesiology",
                                  'Speciality_Dermatology_5.7.3' = "As.a.physician..what.is.your.medical.specialty...Please.check.all.that.apply....Dermatology",
                                  'Speciality_Cardiology_5.7.4' = "As.a.physician..what.is.your.medical.specialty...Please.check.all.that.apply....Cardiology",
                                  'Speciality_Electrophysiology_5.7.5' = "As.a.physician..what.is.your.medical.specialty...Please.check.all.that.apply....Electrophysiology",
                                  'Speciality_Emergency_med_5.7.6' = "As.a.physician..what.is.your.medical.specialty...Please.check.all.that.apply....Emergency.Medicine",
                                  'Speciality_Endocrinology_5.7.7' = "As.a.physician..what.is.your.medical.specialty...Please.check.all.that.apply....Endocrinology",
                                  'Speciality_Family_med_5.7.8' = "As.a.physician..what.is.your.medical.specialty...Please.check.all.that.apply....Family.Medicine",
                                  'Speciality_Gasetroenterologist_5.7.9' = "As.a.physician..what.is.your.medical.specialty...Please.check.all.that.apply....Gastroenterologist",
                                  'Speciality_General_surgery_5.7.10' = "As.a.physician..what.is.your.medical.specialty...Please.check.all.that.apply....General.Surgery",
                                  'Speciality_Geriatrics_5.7.11' = "As.a.physician..what.is.your.medical.specialty...Please.check.all.that.apply....Geriatrics",
                                  'Speciality_Immunology_5.7.12' = "As.a.physician..what.is.your.medical.specialty...Please.check.all.that.apply....Immunology",
                                  'Speciality_Infectious_disease_5.7.13' = "As.a.physician..what.is.your.medical.specialty...Please.check.all.that.apply....Infectious.diseases",
                                  'Speciality_Internal_med_5.7.14' = "As.a.physician..what.is.your.medical.specialty...Please.check.all.that.apply....Internal.Medicine",
                                  'Speciality_Interventional_radiology_5.7.15' = "As.a.physician..what.is.your.medical.specialty...Please.check.all.that.apply....Interventional.Radiology",
                                  'Speciality_Medical_genetica_5.7.16' = "As.a.physician..what.is.your.medical.specialty...Please.check.all.that.apply....Medical.Genetics",
                                  'Speciality_Neonatal_CC_5.7.17' = "As.a.physician..what.is.your.medical.specialty...Please.check.all.that.apply....Neonatal.Critical.Care",
                                  'Speciality_Neurology_5.7.18' = "As.a.physician..what.is.your.medical.specialty...Please.check.all.that.apply....Neurology",
                                  'Speciality_Neurosurgery_5.7.19' = "As.a.physician..what.is.your.medical.specialty...Please.check.all.that.apply....Neurosurgery",
                                  'Speciality_obsterics_n_Gynecology_5.7.20' = "As.a.physician..what.is.your.medical.specialty...Please.check.all.that.apply....Obstetrics.and.Gynecology",
                                  'Speciality_Oncology_5.7.21' = "As.a.physician..what.is.your.medical.specialty...Please.check.all.that.apply....Oncology",
                                  'Speciality_Opthalmology_5.7.22' = "As.a.physician..what.is.your.medical.specialty...Please.check.all.that.apply....Ophthalmology",
                                  'Speciality_Orthopedics_5.7.23' = "As.a.physician..what.is.your.medical.specialty...Please.check.all.that.apply....Orthopedics",
                                  'Speciality_Orthopedic_surg_5.7.24' = "As.a.physician..what.is.your.medical.specialty...Please.check.all.that.apply....Orthopedic.surgery",
                                  'Speciality_Otolaryngology_5.7.25' = "As.a.physician..what.is.your.medical.specialty...Please.check.all.that.apply....Otolaryngology",
                                  'Speciality_Pathology_5.7.26' = "As.a.physician..what.is.your.medical.specialty...Please.check.all.that.apply....Pathology",
                                  'Speciality_Pediatrics_5.7.27' = "As.a.physician..what.is.your.medical.specialty...Please.check.all.that.apply....Pediatrics",
                                  'Speciality_Pediatric_CC_5.7.28' = "As.a.physician..what.is.your.medical.specialty...Please.check.all.that.apply....Pediatric.Critical.Care",
                                  'Speciality_Pediatric_emer_med_5.7.29' = "As.a.physician..what.is.your.medical.specialty...Please.check.all.that.apply....Pediatric.Emergency.Medicine",
                                  'Speciality_Physical_med_5.7.30' = "As.a.physician..what.is.your.medical.specialty...Please.check.all.that.apply....Physical.Medicine.and.Rehabilitation",
                                  'Speciality_Podiatry_5.7.31' = "As.a.physician..what.is.your.medical.specialty...Please.check.all.that.apply....Podiatry",
                                  'Speciality_Preventive_med_5.7.32' = "As.a.physician..what.is.your.medical.specialty...Please.check.all.that.apply....Preventive.medicine...occupational.medicine",
                                  'Speciality_Psychiatry_5.7.33' = "As.a.physician..what.is.your.medical.specialty...Please.check.all.that.apply....Psychiatry",
                                  'Speciality_Pulmonology_5.7.34' = "As.a.physician..what.is.your.medical.specialty...Please.check.all.that.apply....Pulmonology",
                                  'Speciality_Radiation_oncology_5.7.35' = "As.a.physician..what.is.your.medical.specialty...Please.check.all.that.apply....Radiation.oncology",
                                  'Speciality_Radiology_5.7.36' = "As.a.physician..what.is.your.medical.specialty...Please.check.all.that.apply....Radiology",
                                  'Speciality_Rheumatology_5.7.37' = "As.a.physician..what.is.your.medical.specialty...Please.check.all.that.apply....Rheumatology",
                                  'Speciality_Urology_5.7.38' = "As.a.physician..what.is.your.medical.specialty...Please.check.all.that.apply....Urology",
                                  'Speciality_Vascular_surg_5.7.39' = "As.a.physician..what.is.your.medical.specialty...Please.check.all.that.apply....Vascular.Surgery",
                                  'Speciality_Other_5.7.40' = "As.a.physician..what.is.your.medical.specialty...Please.check.all.that.apply....Other..Please.specify.",
                                  'Years_practicing_5.8' = "How.many.years..including.residency..have.you.been.practicing.medicine.",
                                  'State_of_practice_5.9' = "X50.States..D.C..and.Puerto.Rico",
                                  'Zip_code_of_practice_5.10' = "What.is.the.ZIP.code.of.your.place.of.practice.",
                                  'Work_Hosp_urban_5.11.1.1' = "What.is.the.type.of.the.hospital.s..where.you.work...Select.all.that.apply....Selected.Choice...Urban.Hospital",
                                  'Work_Hosp_Suburban_5.11.1.2' = "What.is.the.type.of.the.hospital.s..where.you.work...Select.all.that.apply....Selected.Choice...Suburban.Hospital",
                                  'Work_Hosp_Rural_5.11.1.3' = "What.is.the.type.of.the.hospital.s..where.you.work...Select.all.that.apply....Selected.Choice...Rural.Hospital",
                                  'Work_Hosp_Speciality_5.11.1.4' = "What.is.the.type.of.the.hospital.s..where.you.work...Select.all.that.apply....Selected.Choice...Specialty..e.g...Cancer.Hospital..Children.s.Hospital..etc..",
                                  'Work_Hosp_Veterans_5.11.1.5' = "What.is.the.type.of.the.hospital.s..where.you.work...Select.all.that.apply....Selected.Choice...Veterans.or.Federal.Hospital",
                                  'Work_Hosp_Community_5.11.1.6' = "What.is.the.type.of.the.hospital.s..where.you.work...Select.all.that.apply....Selected.Choice...Community..Non.federal.Acute.Care..Hospital",
                                  'Work_Hosp_Teaching_research_5.11.1.7' = "What.is.the.type.of.the.hospital.s..where.you.work...Select.all.that.apply....Selected.Choice...Teaching...Research.Hospital",
                                  'Work_Hosp_Non_Fed_psy_5.11.1.8' = "What.is.the.type.of.the.hospital.s..where.you.work...Select.all.that.apply....Selected.Choice...Non.federal.Psychiatric.Care",
                                  'Work_Hosp_Non_Fed_Long_term_5.11.1.9' = "What.is.the.type.of.the.hospital.s..where.you.work...Select.all.that.apply....Selected.Choice...Non.federal.Long.Term.Care",
                                  'Work_Hosp_Rehabilitation_5.11.1.10' = "What.is.the.type.of.the.hospital.s..where.you.work...Select.all.that.apply....Selected.Choice...Rehabilitation.Hospital",
                                  'Work_Hosp_Critical_acc_5.11.1.11' = "What.is.the.type.of.the.hospital.s..where.you.work...Select.all.that.apply....Selected.Choice...Critical.Access.Hospital",
                                  'Work_Hosp_Small_5.11.1.12' = "What.is.the.type.of.the.hospital.s..where.you.work...Select.all.that.apply....Selected.Choice...Small.Hospital...100.beds.",
                                  'Work_Hosp_Medium_5.11.1.13' = "What.is.the.type.of.the.hospital.s..where.you.work...Select.all.that.apply....Selected.Choice...Medium.Hospital..100.500.beds.",
                                  'Work_Hosp_Large_5.11.1.14' = "What.is.the.type.of.the.hospital.s..where.you.work...Select.all.that.apply....Selected.Choice...Large.hospitals...500.beds.",
                                  'Work_Hosp_others_5.11.1.15' = "What.is.the.type.of.the.hospital.s..where.you.work...Select.all.that.apply....Selected.Choice...Others..Please.specify..",
                                  'Work_Hosp_others_txt_5.11.1.15' = "What.is.the.type.of.the.hospital.s..where.you.work...Select.all.that.apply....Others..Please.specify.....Text",
                                  'Working_status_5.12' = "What.is.your.current.working.status.",
                                  'Lose_FT_job_5.12.1' = "Did.you.lose.your.full.time.working.status.due.to.COVID.19.pandemic.",
                                  'Furloughed_5.12.2' = "Were.you.furloughed.due.to.the.COVID.19.pandemic.",
                                  'Laid_off_5.12.3' = "Were.you.laid.off.due.to.the.COVID.19.pandemic.",
                                  'Leave_due_to_COVID_5.12.14' = "Are.you.on.leave.due.to.the.COVID.19.pandemic.",
                                  'POP_Black_5.13.1' = "Please.provide.your.best.estimate.of.the.percent.of.patients.from.each.racial.group.that.comprise.your.current.primary.work.setting.s.patient.population....Black.or.African.American",
                                  'POP_Non_Hisp_white_5.13.2' = "Please.provide.your.best.estimate.of.the.percent.of.patients.from.each.racial.group.that.comprise.your.current.primary.work.setting.s.patient.population....Non.Hispanic.White",
                                  'POP_Hispanic_latino_5.13.3' = "Please.provide.your.best.estimate.of.the.percent.of.patients.from.each.racial.group.that.comprise.your.current.primary.work.setting.s.patient.population....Hispanic.or.Latino",
                                  'POP_Asian_5.13.4' = "Please.provide.your.best.estimate.of.the.percent.of.patients.from.each.racial.group.that.comprise.your.current.primary.work.setting.s.patient.population....Asian",
                                  'POP_American_indian_5.13.5' = "Please.provide.your.best.estimate.of.the.percent.of.patients.from.each.racial.group.that.comprise.your.current.primary.work.setting.s.patient.population....American.Indian.or.Alaska.Native",
                                  'POP_Native_hawaiian_5.13.6' = "Please.provide.your.best.estimate.of.the.percent.of.patients.from.each.racial.group.that.comprise.your.current.primary.work.setting.s.patient.population....Native.Hawaiian.or.Pacific.Islander"
                                  
                                  )


########### removing NA values and retired persons

subsetted <- subset(renamed, renamed$Imply_to_consent == 'Yes')
subsetted <- subset(subsetted, subsetted$Stage_in_career != 'Retired')
subsetted <- subsetted[-which(is.na(subsetted$Resilience_adapt_1.1)),]

renamed <- subsetted

# adding id to identify each record
renamed$ID <- 1:nrow(renamed)

# revaluing columns
renamed$Direcly_worked_COVID_patients_2.1 <- revalue(renamed$Direcly_worked_COVID_patients_2.1,
                                                     c("Yes" = "Directly worked",
                                                       "No" = "Not directly worked")) %>% as.factor()


for (i in colnames(renamed)) {
  
  ind <- which(grepl("please specify", renamed[,i] , ignore.case = T))
  renamed[ind,i] <- "Others"
  ind1 <- which(grepl("please mention", renamed[,i] , ignore.case = T))
  renamed[ind1,i] <- "Others"
  
}


####################################### ptsd final score and distribution

# changing the values to scores based on the meta-data for PTSD questions

renamed$PTSD_unwanted_memories_3.2.1 <- revalue(renamed$PTSD_unwanted_memories_3.2.1, c("Not at all"= 0, "A little bit"= 1, "Moderately" = 2, "Quite a bit" = 3, "Extremely" = 4 )) %>% as.numeric()

renamed$PTSD_disturbing_dreams_3.2.2 <- revalue(renamed$PTSD_disturbing_dreams_3.2.2, c("Not at all"= 0, "A little bit"= 1, "Moderately" = 2, "Quite a bit" = 3, "Extremely" = 4 )) %>% as.numeric()

renamed$PTSD_actually_happening_again_3.2.3 <- revalue(renamed$PTSD_actually_happening_again_3.2.3, c("Not at all"= 0, "A little bit"= 1, "Moderately" = 2, "Quite a bit" = 3, "Extremely" = 4 )) %>% as.numeric()
 
renamed$PTSD_upset_when_reminded_3.2.4 <- revalue(renamed$PTSD_upset_when_reminded_3.2.4, c("Not at all"= 0, "A little bit"= 1, "Moderately" = 2, "Quite a bit" = 3, "Extremely" = 4 )) %>% as.numeric()

renamed$PTSD_physical_reaction_reminded_3.2.5 <- revalue(renamed$PTSD_physical_reaction_reminded_3.2.5, c("Not at all"= 0, "A little bit"= 1, "Moderately" = 2, "Quite a bit" = 3, "Extremely" = 4 )) %>% as.numeric()

renamed$PTSD_avoiding_memories_3.2.6 <- revalue(renamed$PTSD_avoiding_memories_3.2.6, c("Not at all"= 0, "A little bit"= 1, "Moderately" = 2, "Quite a bit" = 3, "Extremely" = 4 )) %>% as.numeric()

renamed$PTSD_avoiding_externam_reminded_3.2.7 <- revalue(renamed$PTSD_avoiding_externam_reminded_3.2.7, c("Not at all"= 0, "A little bit"= 1, "Moderately" = 2, "Quite a bit" = 3, "Extremely" = 4 )) %>% as.numeric()

renamed$PTSD_trouble_remembering_3.2.8 <- revalue(renamed$PTSD_trouble_remembering_3.2.8, c("Not at all"= 0, "A little bit"= 1, "Moderately" = 2, "Quite a bit" = 3, "Extremely" = 4 )) %>% as.numeric()

renamed$PTSD_negative_belief_self_3.2.9 <- revalue(renamed$PTSD_negative_belief_self_3.2.9, c("Not at all"= 0, "A little bit"= 1, "Moderately" = 2, "Quite a bit" = 3, "Extremely" = 4 )) %>% as.numeric()

renamed$PTSD_blaming_self_3.2.10 <- revalue(renamed$PTSD_blaming_self_3.2.10, c("Not at all"= 0, "A little bit"= 1, "Moderately" = 2, "Quite a bit" = 3, "Extremely" = 4 )) %>% as.numeric()

renamed$PTSD_negative_feelings_3.2.11 <- revalue(renamed$PTSD_negative_feelings_3.2.11, c("Not at all"= 0, "A little bit"= 1, "Moderately" = 2, "Quite a bit" = 3, "Extremely" = 4 )) %>% as.numeric()

renamed$PTSD_loss_of_interest_3.2.12 <- revalue(renamed$PTSD_loss_of_interest_3.2.12, c("Not at all"= 0, "A little bit"= 1, "Moderately" = 2, "Quite a bit" = 3, "Extremely" = 4 )) %>% as.numeric()

renamed$PTSD_distant_from_people_3.2.13 <- revalue(renamed$PTSD_distant_from_people_3.2.13, c("Not at all"= 0, "A little bit"= 1, "Moderately" = 2, "Quite a bit" = 3, "Extremely" = 4 )) %>% as.numeric()

renamed$PTSD_trouble_exp_positive_feelings_3.2.14 <- revalue(renamed$PTSD_trouble_exp_positive_feelings_3.2.14, c("Not at all"= 0, "A little bit"= 1, "Moderately" = 2, "Quite a bit" = 3, "Extremely" = 4 )) %>% as.numeric()

renamed$PTSD_irritable_behavior_3.2.15 <- revalue(renamed$PTSD_irritable_behavior_3.2.15, c("Not at all"= 0, "A little bit"= 1, "Moderately" = 2, "Quite a bit" = 3, "Extremely" = 4 )) %>% as.numeric()

renamed$PTSD_taking_too_many_risk_3.2.16 <- revalue(renamed$PTSD_taking_too_many_risk_3.2.16, c("Not at all"= 0, "A little bit"= 1, "Moderately" = 2, "Quite a bit" = 3, "Extremely" = 4 )) %>% as.numeric()

renamed$PTSD_super_alert_3.2.17 <- revalue(renamed$PTSD_super_alert_3.2.17, c("Not at all"= 0, "A little bit"= 1, "Moderately" = 2, "Quite a bit" = 3, "Extremely" = 4 )) %>% as.numeric()

renamed$PTSD_easily_startled_3.2.18 <- revalue(renamed$PTSD_easily_startled_3.2.18, c("Not at all"= 0, "A little bit"= 1, "Moderately" = 2, "Quite a bit" = 3, "Extremely" = 4 )) %>% as.numeric()

renamed$PTSD_difficulty_concentrating_3.2.19 <- revalue(renamed$PTSD_difficulty_concentrating_3.2.19, c("Not at all"= 0, "A little bit"= 1, "Moderately" = 2, "Quite a bit" = 3, "Extremely" = 4 )) %>% as.numeric()

renamed$PTSD_trouble_asleep_3.2.20 <- revalue(renamed$PTSD_trouble_asleep_3.2.20, c("Not at all"= 0, "A little bit"= 1, "Moderately" = 2, "Quite a bit" = 3, "Extremely" = 4 )) %>% as.numeric()


# calculating the sum of all 20 questions for PTSD and excluding NA values

renamed$ptsdfinalscore <-  rowSums((renamed[,c('PTSD_unwanted_memories_3.2.1' , 'PTSD_disturbing_dreams_3.2.2', 'PTSD_actually_happening_again_3.2.3',
                             'PTSD_upset_when_reminded_3.2.4', 'PTSD_physical_reaction_reminded_3.2.5', 'PTSD_avoiding_memories_3.2.6',
                             'PTSD_avoiding_externam_reminded_3.2.7', 'PTSD_trouble_remembering_3.2.8','PTSD_negative_belief_self_3.2.9',  'PTSD_blaming_self_3.2.10',
                             'PTSD_negative_feelings_3.2.11', 'PTSD_loss_of_interest_3.2.12', 'PTSD_distant_from_people_3.2.13',
                             'PTSD_trouble_exp_positive_feelings_3.2.14', 'PTSD_irritable_behavior_3.2.15', 'PTSD_taking_too_many_risk_3.2.16',
                             'PTSD_super_alert_3.2.17', 'PTSD_easily_startled_3.2.18', 'PTSD_difficulty_concentrating_3.2.19', 'PTSD_trouble_asleep_3.2.20')]), na.rm = F) 


# creating a new variable to get probable PTSD observations based on the cutoff 

renamed$ptsd_scaled <- ifelse(renamed$ptsdfinalscore < 31, 'No', 'Probable PTSD') %>% as.factor() 

################################### depression final score and distribution

# changing the values to scores based on the meta-data for depression questions

renamed$Depression_little_interest_3.1.1 <- revalue(renamed$Depression_little_interest_3.1.1, c("Not at all"= 0, "Several days"= 1, "More than half the days" = 2, "Nearly every day" = 3)) %>% as.numeric()

renamed$Depression_feeling_down_3.1.2 <- revalue(renamed$Depression_feeling_down_3.1.2, c("Not at all"= 0, "Several days"= 1, "More than half the days" = 2, "Nearly every day" = 3)) %>% as.numeric()

renamed$Depression_trouble_sleeping_3.1.3 <- revalue(renamed$Depression_trouble_sleeping_3.1.3, c("Not at all"= 0, "Several days"= 1, "More than half the days" = 2, "Nearly every day" = 3)) %>% as.numeric()
 
renamed$Depression_feeling_tired_3.1.4 <- revalue(renamed$Depression_feeling_tired_3.1.4, c("Not at all"= 0, "Several days"= 1, "More than half the days" = 2, "Nearly every day" = 3)) %>% as.numeric()

renamed$Depression_poor_appetite_3.1.5 <- revalue(renamed$Depression_poor_appetite_3.1.5, c("Not at all"= 0, "Several days"= 1, "More than half the days" = 2, "Nearly every day" = 3)) %>% as.numeric()

renamed$Depression_feelin_bad_about_self_3.1.6 <- revalue(renamed$Depression_feelin_bad_about_self_3.1.6, c("Not at all"= 0, "Several days"= 1, "More than half the days" = 2, "Nearly every day" = 3)) %>% as.numeric()
 
renamed$Depression_trouble_concentrating_3.1.7 <- revalue(renamed$Depression_trouble_concentrating_3.1.7, c("Not at all"= 0, "Several days"= 1, "More than half the days" = 2, "Nearly every day" = 3)) %>% as.numeric()

renamed$Depression_fidgety_or_restless_3.1.8 <- revalue(renamed$Depression_fidgety_or_restless_3.1.8, c("Not at all"= 0, "Several days"= 1, "More than half the days" = 2, "Nearly every day" = 3)) %>% as.numeric()

renamed$Depression_thoughts_about_hurting_self_3.1.9 <- revalue(renamed$Depression_thoughts_about_hurting_self_3.1.9, c("Not at all"= 0, "Several days"= 1, "More than half the days" = 2, "Nearly every day" = 3)) %>% as.numeric()
 
# calculating depression final scores omitting NA values

renamed$depression_final_score <- rowSums((renamed[,c('Depression_little_interest_3.1.1', 'Depression_feeling_down_3.1.2','Depression_trouble_sleeping_3.1.3', 
                    'Depression_feeling_tired_3.1.4',  'Depression_poor_appetite_3.1.5', 'Depression_feelin_bad_about_self_3.1.6',
                    'Depression_trouble_concentrating_3.1.7', 'Depression_fidgety_or_restless_3.1.8', 'Depression_thoughts_about_hurting_self_3.1.9')]), na.rm = F) 



# binning the scores to create scaled depression values based on the validated scores

renamed$depression_final_scaled <- cut2(renamed$depression_final_score, c(5,10,15,20))

renamed$depression_final_scaled <- revalue(renamed$depression_final_scaled, c("[ 0, 5)" = 'None-Minimal depression',
                                                                              "[ 5,10)" = 'Mild depression',
                                                                              "[10,15)" = 'Moderate depression',
                                                                              "[15,20)" = 'Moderately Severe depression',
                                                                              "[20,27]" = 'Severe depression')) %>% as.ordered()


# creating new variable to change values to validated scores for burnout

renamed$level_of_burnout_3.3_scaled <-  revalue(renamed$level_of_burnout_3.3, c("I feel completely burned out and often wonder if I can go on. I am at the point where I may need some changes or may need to seek some sort of help." = 'Severe burnout',
                                                                                "The symptoms of burnout that I'm experiencing won't go away. I think about frustration at work a lot." = 'Moderately severe burnout',
                                                                                "I am definitely burning out and have one or more symptoms of burnout, such as physical and emotional exhaustion." = 'Moderate burnout',
                                                                                "Occasionally I am under stress, and I don't always have as much energy as I once did, but I don't feel burned out." = 'Mild burnout',
                                                                                "I enjoy my work. I have no symptoms of burnout." = 'No burnout')) %>% as.factor()




##########################

# calculating resilience 

# changing resilience to scores from wordings

renamed$Resilience_adapt_1.1 <- revalue(renamed$Resilience_adapt_1.1, c("Not true at all" = 0,
                                                                         "Rarely true" = 1,
                                                                         "Sometimes true" = 2,
                                                                         "Often true" = 3,
                                                                         "True nearly all the time" = 4)) %>% as.numeric()


renamed$Resilience_can_deal_1.2 <- revalue(renamed$Resilience_can_deal_1.2, c("Not true at all" = 0,
                                                                        "Rarely true" = 1,
                                                                        "Sometimes true" = 2,
                                                                        "Often true" = 3,
                                                                        "True nearly all the time" = 4)) %>% as.numeric()


renamed$Resilience_humorous_side_1.3 <- revalue(renamed$Resilience_humorous_side_1.3, c("Not true at all" = 0,
                                                                        "Rarely true" = 1,
                                                                        "Sometimes true" = 2,
                                                                        "Often true" = 3,
                                                                        "True nearly all the time" = 4)) %>% as.numeric()


renamed$Resilience_stress_strengthen_1.4 <- revalue(renamed$Resilience_stress_strengthen_1.4, c("Not true at all" = 0,
                                                                        "Rarely true" = 1,
                                                                        "Sometimes true" = 2,
                                                                        "Often true" = 3,
                                                                        "True nearly all the time" = 4)) %>% as.numeric()


renamed$Resilience_bounce_after_ill_1.5 <- revalue(renamed$Resilience_bounce_after_ill_1.5, c("Not true at all" = 0,
                                                                        "Rarely true" = 1,
                                                                        "Sometimes true" = 2,
                                                                        "Often true" = 3,
                                                                        "True nearly all the time" = 4)) %>% as.numeric()


renamed$Resilience_goals_desp_obst_1.6 <- revalue(renamed$Resilience_goals_desp_obst_1.6, c("Not true at all" = 0,
                                                                        "Rarely true" = 1,
                                                                        "Sometimes true" = 2,
                                                                        "Often true" = 3,
                                                                        "True nearly all the time" = 4)) %>% as.numeric()


renamed$Resilience_focus_under_pres_1.7 <- revalue(renamed$Resilience_focus_under_pres_1.7, c("Not true at all" = 0,
                                                                        "Rarely true" = 1,
                                                                        "Sometimes true" = 2,
                                                                        "Often true" = 3,
                                                                        "True nearly all the time" = 4)) %>% as.numeric()


renamed$Resilience_not_disc_by_fail_1.8 <- revalue(renamed$Resilience_not_disc_by_fail_1.8, c("Not true at all" = 0,
                                                                        "Rarely true" = 1,
                                                                        "Sometimes true" = 2,
                                                                        "Often true" = 3,
                                                                        "True nearly all the time" = 4)) %>% as.numeric()


renamed$Resilience_think_self_strong_1.9 <- revalue(renamed$Resilience_think_self_strong_1.9, c("Not true at all" = 0,
                                                                        "Rarely true" = 1,
                                                                        "Sometimes true" = 2,
                                                                        "Often true" = 3,
                                                                        "True nearly all the time" = 4)) %>% as.numeric()


renamed$Resilience_handle_unpleasant_feel_1.10 <- revalue(renamed$Resilience_handle_unpleasant_feel_1.10, c("Not true at all" = 0,
                                                                        "Rarely true" = 1,
                                                                        "Sometimes true" = 2,
                                                                        "Often true" = 3,
                                                                        "True nearly all the time" = 4)) %>% as.numeric()



renamed$resilience_final_score <- rowSums((renamed[,c('Resilience_adapt_1.1', 'Resilience_can_deal_1.2', 'Resilience_humorous_side_1.3', 'Resilience_stress_strengthen_1.4',
                                                      'Resilience_bounce_after_ill_1.5', 'Resilience_goals_desp_obst_1.6', 'Resilience_focus_under_pres_1.7',
                                                      'Resilience_not_disc_by_fail_1.8', 'Resilience_think_self_strong_1.9', 'Resilience_handle_unpleasant_feel_1.10')]), na.rm = F)



# calculating individual resilience clusters

renamed$resilience_hardiness_flexibility <- rowSums(renamed[,c('Resilience_adapt_1.1', 'Resilience_bounce_after_ill_1.5')], na.rm = F)

renamed$resilience_hardiness_SSE <- rowSums(renamed[,c('Resilience_can_deal_1.2', 'Resilience_stress_strengthen_1.4',
                                                       'Resilience_think_self_strong_1.9')], na.rm = F)

renamed$resilience_hardiness_regulate_em <- renamed$Resilience_handle_unpleasant_feel_1.10

renamed$resilience_hardiness_optimism <- rowSums(renamed[,c('Resilience_humorous_side_1.3',
                                                            'Resilience_goals_desp_obst_1.6', 'Resilience_not_disc_by_fail_1.8')], na.rm = F)

renamed$resilience_hardiness_cognitivefocus <- renamed$Resilience_focus_under_pres_1.7

###########

# calculating age from DOB

# changing the non standard dob format to get years

renamed[which(nchar(renamed$year_of_birth_5.1) == 10),]$year_of_birth_5.1 <- parse_date_time(renamed[which(nchar(renamed$year_of_birth_5.1) == 10),]$year_of_birth_5.1,
                                                                                             orders = c('mdy', 'dmy')) %>% year()


renamed[which(renamed$year_of_birth_5.1 == "10972"),]$year_of_birth_5.1 <- "1972"
renamed[which(renamed$year_of_birth_5.1 == "4-2-54"),]$year_of_birth_5.1 <- "1954"
renamed[which(renamed$year_of_birth_5.1 == "19772"),]$year_of_birth_5.1 <- "1972"
renamed[which(renamed$year_of_birth_5.1 == "2968"),]$year_of_birth_5.1 <- "1968"
renamed[which(renamed$year_of_birth_5.1 == "1867"),]$year_of_birth_5.1 <- "1967"

renamed[which(nchar(renamed$year_of_birth_5.1) > 4),]$year_of_birth_5.1 <- NA
renamed[which(renamed$year_of_birth_5.1 > year(Sys.Date())),]$year_of_birth_5.1 <- NA
renamed[which(renamed$year_of_birth_5.1 == "1847"),]$year_of_birth_5.1 <- NA


renamed$Age <- year(as.Date(Sys.Date())) - as.numeric((renamed$year_of_birth_5.1)) 

############ reverting one hot encoded race variables

race <- data.frame(renamed$race_americanindian_5.4.1, renamed$race_asian_5.4.2, renamed$race_black_5.4.3, renamed$race_native_haw_5.4.4,
                   renamed$race_white_5.4.5, renamed$race_others_5.4.6, renamed$ID)

colnames(race) <- c('American Indian or Alaska native', 'Asian', 'Black or African american',
                    'Native Hawaiian or Pacific islander', 'White', 'Others', 'ID')
# 
# sum_of_missing <- function(x){sum(is.na(x))}
# 
# racemissing <- which(apply(race[,-ncol(race)],1,sum_of_missing) == (ncol(race)-1)) %>% length()
# 
# race <- reshape2::melt(race, id.vars = 'ID')
# 
# forracedist <- data.frame(race$value) %>% na.omit()
# 
# forracedist[nrow(forracedist)+1:racemissing,] <- NA

# reversing one hot encoded race variable using manual function

forracedist <- revert_onehot_with_id(race)$value %>% data.frame()
colnames(forracedist) <- "race.value"


################ reverting one hot encoded speciality variables

speciality <- data.frame(renamed$Speciality_Adult_CC_5.7.1, renamed$Speciality_Anesthesiology_5.7.2, renamed$Speciality_Dermatology_5.7.3, renamed$Speciality_Cardiology_5.7.4,
                         renamed$Speciality_Electrophysiology_5.7.5, renamed$Speciality_Emergency_med_5.7.6, renamed$Speciality_Endocrinology_5.7.7,
                         renamed$Speciality_Family_med_5.7.8, renamed$Speciality_Gasetroenterologist_5.7.9, renamed$Speciality_General_surgery_5.7.10, 
                         renamed$Speciality_Geriatrics_5.7.11, renamed$Speciality_Immunology_5.7.12, renamed$Speciality_Infectious_disease_5.7.13, renamed$Speciality_Internal_med_5.7.14,
                         renamed$Speciality_Interventional_radiology_5.7.15, renamed$Speciality_Medical_genetica_5.7.16, renamed$Speciality_Neonatal_CC_5.7.17, renamed$Speciality_Neurology_5.7.18,
                         renamed$Speciality_Neurosurgery_5.7.19, renamed$Speciality_obsterics_n_Gynecology_5.7.20, renamed$Speciality_Oncology_5.7.21,
                         renamed$Speciality_Opthalmology_5.7.22, renamed$Speciality_Orthopedics_5.7.23, renamed$Speciality_Orthopedic_surg_5.7.24,
                         renamed$Speciality_Otolaryngology_5.7.25, renamed$Speciality_Pathology_5.7.26, renamed$Speciality_Pediatrics_5.7.27,
                         renamed$Speciality_Pediatric_CC_5.7.28, renamed$Speciality_Pediatric_emer_med_5.7.29, renamed$Speciality_Physical_med_5.7.30,
                         renamed$Speciality_Podiatry_5.7.31, renamed$Speciality_Preventive_med_5.7.32, renamed$Speciality_Psychiatry_5.7.33, renamed$Speciality_Pulmonology_5.7.34,
                         renamed$Speciality_Radiation_oncology_5.7.35, renamed$Speciality_Radiology_5.7.36, renamed$Speciality_Rheumatology_5.7.37,
                         renamed$Speciality_Urology_5.7.38, renamed$Speciality_Vascular_surg_5.7.39, renamed$Speciality_Other_5.7.40, renamed$ID)


colnames(speciality) <- c('Adult Critical Care', 'Anesthesiology', 'Dermatology', 'Cardiology', 'Electrophysiology', 'Emergency Medicine', 'Endocrinology',
                          'Family Medicine', 'Gastroenterologist', 'General Surgery', 'Geriatrics', 'Immunology', 'Infectious Diseases', 'Internal Medicine',
                          'Interventional Radiology', 'Medical Genetics', 'Neonatal Critical Care', 'Neurology', 'Neurosurgery', 'Obstetrics and Gynecology',
                          'Oncology', 'Ophthalmology', 'Orthopedics', 'Orthopedic surgery', 'Otolaryngology', 'Pathology', 'Pediatrics', 'Pediatric Critical Care',
                          'Pediatric Emergency Medicine', 'Physical Medicine and Rehabilitation', 'Podiatry', 'Preventive medicine / occupational medicine',
                          'Psychiatry', 'Pulmonology', 'Radiation oncology', 'Radiology', 'Rheumatology', 'Urology', 'Vascular Surgery', 'Other', 'ID')


#specialitymissing <- which(apply(speciality[,-ncol(speciality)],1,sum_of_missing) == (ncol(speciality)-1)) %>% length()

#speciality <- melt(speciality, id.vars = 'ID')

#forspecialitydist <- data.frame(speciality$value) %>% na.omit() 

#forspecialitydist[nrow(forspecialitydist)+1:specialitymissing,] <- NA

# reversing one hot encoded speciality variable using manual function

forspecialitydist <- revert_onehot_with_id(speciality)$value %>% data.frame()
colnames(forspecialitydist) <- "speciality.value" 

####################

# Average working hours per week before covid - correcting data

renamed[grep("80\\+" , renamed$Hours_worked_before_covid_2.6),]$Hours_worked_before_covid_2.6 <- 80
renamed[grep("32 hrs" , renamed$Hours_worked_before_covid_2.6),]$Hours_worked_before_covid_2.6 <- 32
renamed[grep("60 hours" , renamed$Hours_worked_before_covid_2.6),]$Hours_worked_before_covid_2.6 <- 60
renamed[grep("Fifty" , renamed$Hours_worked_before_covid_2.6),]$Hours_worked_before_covid_2.6 <- 50
renamed[grep("40-60\\+" , renamed$Hours_worked_before_covid_2.6),]$Hours_worked_before_covid_2.6 <- 40-60
renamed[grep("Completely variable, averaged over month 40-60" , renamed$Hours_worked_before_covid_2.6),]$Hours_worked_before_covid_2.6 <- 40-60
renamed[grep("40-60 hrs a week" , renamed$Hours_worked_before_covid_2.6),]$Hours_worked_before_covid_2.6 <- 40-60
renamed[grep("40-50 hours" , renamed$Hours_worked_before_covid_2.6),]$Hours_worked_before_covid_2.6 <- 40-50
renamed[grep("84 hour work week, one week on and one week off" , renamed$Hours_worked_before_covid_2.6),]$Hours_worked_before_covid_2.6 <- 84
renamed[grep("-10" , renamed$Hours_worked_before_covid_2.6),]$Hours_worked_before_covid_2.6 <- 10
renamed[grep("40 the same I wore during and will be working after the pandemic; WTH is with this survey I already answered that I did not work w CV19 pot directly and now rthw next question ask about my work w CV19n to. This is an biased and flawed survey" , renamed$Hours_worked_before_covid_2.6),]$Hours_worked_before_covid_2.6 <- 40
renamed[grep("35 hours" , renamed$Hours_worked_before_covid_2.6),]$Hours_worked_before_covid_2.6 <- 35
renamed[grep("24/7" , renamed$Hours_worked_before_covid_2.6),]$Hours_worked_before_covid_2.6 <- NA
renamed[grep("16 clinical, 30-40 overall" , renamed$Hours_worked_before_covid_2.6),]$Hours_worked_before_covid_2.6 <- 30-40
renamed[grep("-20" , renamed$Hours_worked_before_covid_2.6),]$Hours_worked_before_covid_2.6 <- 20
renamed[grep(";0" , renamed$Hours_worked_before_covid_2.6),]$Hours_worked_before_covid_2.6 <- 0
renamed[grep("25 hr/wk" , renamed$Hours_worked_before_covid_2.6),]$Hours_worked_before_covid_2.6 <- 25
renamed[grep("50\\+" , renamed$Hours_worked_before_covid_2.6),]$Hours_worked_before_covid_2.6 <- 50
renamed[grep(">60" , renamed$Hours_worked_before_covid_2.6),]$Hours_worked_before_covid_2.6 <- 60
renamed[grep("40\\+" , renamed$Hours_worked_before_covid_2.6),]$Hours_worked_before_covid_2.6 <- 40

# function to get mean after splitting

first.word_mean <- function(my.string){
  unlist(strsplit(my.string, "-")) %>% as.numeric() %>% mean()
}

# splitting by "-" and getting the mean of splitted values

renamed$Hours_worked_before_covid_2.6 <- sapply(renamed$Hours_worked_before_covid_2.6, first.word_mean)

# Average working hours per week during peak covid - correcting data

renamed[grep("40 only telepsyquiatry" , renamed$Hours_worked_peak_month_2.7.1),]$Hours_worked_peak_month_2.7.1 <- 40
renamed[grep(">60" , renamed$Hours_worked_peak_month_2.7.1),]$Hours_worked_peak_month_2.7.1 <- 60
renamed[grep("80 +" , renamed$Hours_worked_peak_month_2.7.1),]$Hours_worked_peak_month_2.7.1 <- 80
renamed[grep("35/wk" , renamed$Hours_worked_peak_month_2.7.1),]$Hours_worked_peak_month_2.7.1 <- 35
renamed[grep("All the time!" , renamed$Hours_worked_peak_month_2.7.1),]$Hours_worked_peak_month_2.7.1 <- NA
renamed[grep("50\\+" , renamed$Hours_worked_peak_month_2.7.1),]$Hours_worked_peak_month_2.7.1 <- 50

# splitting by "-" and getting the mean of splitted values

renamed$Hours_worked_peak_month_2.7.1 <- sapply(renamed$Hours_worked_peak_month_2.7.1, first.word_mean)

# hours working outside normal responsibilities - correcting data

renamed[grep("2 hours per week" , renamed$Hours_doing_outside_resp_2.8.1),]$Hours_doing_outside_resp_2.8.1 <- 2
renamed[grep("110 hours per month" , renamed$Hours_doing_outside_resp_2.8.1),]$Hours_doing_outside_resp_2.8.1 <- 110
renamed[grep("72 hours" , renamed$Hours_doing_outside_resp_2.8.1),]$Hours_doing_outside_resp_2.8.1 <- 72
renamed[grep("reviewing decision for therapy, and de-isolation" , renamed$Hours_doing_outside_resp_2.8.1),]$Hours_doing_outside_resp_2.8.1 <- NA
renamed[grep("8 - I went from 20% clinical to 100% administrative during the peak" , renamed$Hours_doing_outside_resp_2.8.1),]$Hours_doing_outside_resp_2.8.1 <- 8
renamed[grep("50\\+" , renamed$Hours_doing_outside_resp_2.8.1),]$Hours_doing_outside_resp_2.8.1 <- 50
renamed[grep("1600" , renamed$Hours_doing_outside_resp_2.8.1),]$Hours_doing_outside_resp_2.8.1 <- NA
renamed[grep("Unsure" , renamed$Hours_doing_outside_resp_2.8.1),]$Hours_doing_outside_resp_2.8.1 <- NA
renamed[grep("30%" , renamed$Hours_doing_outside_resp_2.8.1),]$Hours_doing_outside_resp_2.8.1 <- NA
renamed[grep("Telemedicine visits" , renamed$Hours_doing_outside_resp_2.8.1),]$Hours_doing_outside_resp_2.8.1 <- NA
renamed[grep("several" , renamed$Hours_doing_outside_resp_2.8.1),]$Hours_doing_outside_resp_2.8.1 <- NA
renamed[grep("80 hours" , renamed$Hours_doing_outside_resp_2.8.1),]$Hours_doing_outside_resp_2.8.1 <- 80
renamed[grep("N/A" , renamed$Hours_doing_outside_resp_2.8.1),]$Hours_doing_outside_resp_2.8.1 <- NA
renamed[grep("No" , renamed$Hours_doing_outside_resp_2.8.1),]$Hours_doing_outside_resp_2.8.1 <- NA

# splitting by "-" and getting the mean of splitted values

renamed$Hours_doing_outside_resp_2.8.1 <- sapply(renamed$Hours_doing_outside_resp_2.8.1, first.word_mean)


# years of experience - correcting data

# function to get the first element after splitting

first.word <- function(my.string){
  unlist(strsplit(my.string, " "))[1]
}

# getting first element after splitting by space

years_practicing <- sapply(renamed$Years_practicing_5.8, first.word)

years_practicing <- gsub("[^[:alnum:][:blank:].?&/\\-]", "", years_practicing)

renamed$Years_practicing_5.8 <- years_practicing

renamed[which(renamed$Years_practicing_5.8 == "More"),]$Years_practicing_5.8 <- 15
renamed[which(renamed$Years_practicing_5.8 == "4yrs"),]$Years_practicing_5.8 <- 4
                 
renamed$Years_practicing_5.8 <- renamed$Years_practicing_5.8 %>% as.numeric()

##########################

# type of hospital

hospitaltype <- data.frame(renamed$Work_Hosp_urban_5.11.1.1, renamed$Work_Hosp_Suburban_5.11.1.2, renamed$Work_Hosp_Rural_5.11.1.3,
                           renamed$Work_Hosp_Speciality_5.11.1.4, renamed$Work_Hosp_Veterans_5.11.1.5, renamed$Work_Hosp_Community_5.11.1.6,
                           renamed$Work_Hosp_Teaching_research_5.11.1.7, renamed$Work_Hosp_Non_Fed_psy_5.11.1.8, renamed$Work_Hosp_Non_Fed_Long_term_5.11.1.9,
                           renamed$Work_Hosp_Rehabilitation_5.11.1.10, renamed$Work_Hosp_Critical_acc_5.11.1.11, renamed$Work_Hosp_Small_5.11.1.12,
                           renamed$Work_Hosp_Medium_5.11.1.13, renamed$Work_Hosp_Large_5.11.1.14, renamed$Work_Hosp_others_5.11.1.15, renamed$ID)


colnames(hospitaltype) <- c("Urban Hospital", "Suburban Hospital", "Rural Hospital", "Specialty", "Veterans or Federal Hospital",
                            "Community (Non-federal Acute Care) Hospital", "Teaching / Research Hospital", "Non-federal Psychiatric Care",
                            "Non-federal Long Term Care", "Rehabilitation Hospital", "Critical Access Hospital", "Small Hospital", 
                            "Medium Hospital", "Large hospitals", "Others", "ID" )



# sum_of_missing <- function(x){sum(is.na(x))}
# 
# hospitaltypemissing <- which(apply(hospitaltype[,-ncol(hospitaltype)],1,sum_of_missing) == (ncol(hospitaltype)-1)) %>% length()
# 
# hospitaltype <- melt(hospitaltype, id.vars = "ID")
# 
# forhospitaltypedist <- data.frame(hospitaltype$value) %>% na.omit()
# 
# forhospitaltypedist[nrow(forhospitaltypedist)+1:hospitaltypemissing,] <- NA


# reversing one hot encoded hospital type variable using manual function

forhospitaltypedist <- revert_onehot_with_id(hospitaltype)$value %>% data.frame()
colnames(forhospitaltypedist) <- "hospitaltype.value"

########## work setting within the hospital

worksettingwithinhospital <- data.frame(renamed$Work_setting_emergency_dept_5.11.2.1, renamed$work_setting_general_floor_5.11.2.2, renamed$work_setting_step_down_unit_5.11.2.3,
                                        renamed$work_setting_ICU_5.11.2.4, renamed$work_setting_operating_room_5.11.2.5, renamed$work_setting_other_5.11.2.6, 1:nrow(renamed))

colnames(worksettingwithinhospital) <- c("Emergency Department", "General Floor", "Step Down Unit", "Intensive Care Unit", "Operating Room", "Other", "ID")
# 
# worksettingwithinmissing <- which(apply(worksettingwithinhospital[,-ncol(worksettingwithinhospital)],1,sum_of_missing) == (ncol(worksettingwithinhospital)-1)) %>% length()
# 
# worksettingwithinhospital <- melt(worksettingwithinhospital, id.vars = "ID")
# 
# forhospwithindist <- data.frame(worksettingwithinhospital$value) %>% na.omit()
# 
# forhospwithindist[nrow(forhospwithindist)+1:worksettingwithinmissing,] <- NA
 

# reversing one hot encoded work setting within hospital variable using manual function

forhospwithindist <- revert_onehot_with_id(worksettingwithinhospital)$value %>% data.frame()
colnames(forhospwithindist) <- "worksettingwithinhospital.value"

###### support

orgsupport8q <- data.frame(renamed$org_supp_values_contrib_4.2.1, renamed$org_supp_care_wellbeing_4.2.2, renamed$org_supp_wrk_satisfaction_4.2.3,
                            renamed$org_supp_accomplishment_4.2.4, renamed$org_supp_fail_app_effort_4.2.5, renamed$org_supp_ignore_complaint_4.2.6,
                            renamed$org_supp_fail_to_notice_4.2.7, renamed$org_supp_little_concern_4.2.8)


for (i in names(orgsupport8q)) {
orgsupport8q[,i] <-  revalue(orgsupport8q[,i], c("Strongly disagree" = 0,
                                                   "Moderately disagree" = 1,
                                                   "Slightly disagree" = 2,
                                                   "Neither agree nor disagree" = 3,
                                                   "Slightly agree" = 4,
                                                   "Moderately agree" = 5,
                                                   "Strongly agree" = 6)) %>% as.numeric()
  
}

orgsupport8q$finalscoresorg <-  rowSums(orgsupport8q[,c("renamed.org_supp_values_contrib_4.2.1", "renamed.org_supp_care_wellbeing_4.2.2",
                                                        "renamed.org_supp_wrk_satisfaction_4.2.3", "renamed.org_supp_accomplishment_4.2.4",
                                                        "renamed.org_supp_fail_app_effort_4.2.5", "renamed.org_supp_ignore_complaint_4.2.6",
                                                        "renamed.org_supp_fail_to_notice_4.2.7", "renamed.org_supp_little_concern_4.2.8")], na.rm = F)



#data.frame(renamed$org_supp_even_wrkld_4.1.1, renamed$org_supp_psych_hotline_4.1.2, renamed$org_supp_training_4.1.3)



coping <- data.frame(renamed$cope_stress_activities_4.4.1, renamed$cope_stress_conc_doing_4.4.2, renamed$cope_stress_isint_real_4.4.3,
                     renamed$cope_stress_drugs_alc_4.4.4, renamed$cope_stress_emo_supp_4.4.5, renamed$cope_stress_giving_up_4.4.6,
                     renamed$cope_stress_taking_action_4.4.7, renamed$cope_stress_refuse_beleive_4.4.8, renamed$cope_stress_saying_things_4.4.9,
                     renamed$cope_stress_help_from_4.4.10, renamed$cope_stress_alc_drugs_4.4.11, renamed$cope_stress_see_positive_4.4.12,
                     renamed$cope_stress_crit_self_4.4.13, renamed$cope_stress_stratergy_4.4.14, renamed$cope_stress_comfort_4.4.15,
                     renamed$cope_stress_give_up_cope_4.4.16, renamed$cope_stress_looking_som_good_4.4.17, renamed$cope_stress_joke_4.4.18,
                     renamed$cope_stress_tv_daydreaming_4.4.19, renamed$cope_stress_accept_happ_4.4.20, renamed$cope_stress_exp_neg_4.4.21,
                     renamed$cope_stress_religion_4.4.22, renamed$cope_stress_trying_help_4.4.23, renamed$cope_stress_live_with_it_4.4.24,
                     renamed$cope_stress_think_steps_4.4.25, renamed$cope_stress_blaming_self_4.4.26, renamed$cope_stress_praying_4.4.27,
                     renamed$cope_stress_fun_of_situ_4.4.28)


for (i in names(coping)) {
  coping[,i] <-  revalue(coping[,i], c("I haven't been doing this at all" = 1,
                                                   "I've been doing this a little bit" = 2,
                                                   "I've been doing this in medium amount" = 3,
                                                   "I've been doing this a lot" = 4)) %>% as.numeric()
}

coping_self_distraction <- rowSums(coping[, c('renamed.cope_stress_activities_4.4.1', 'renamed.cope_stress_tv_daydreaming_4.4.19')], na.rm = F)

coping_active_coping <- rowSums(coping[, c('renamed.cope_stress_conc_doing_4.4.2', 'renamed.cope_stress_taking_action_4.4.7')], na.rm = F)

coping_denial <- rowSums(coping[, c('renamed.cope_stress_isint_real_4.4.3', 'renamed.cope_stress_refuse_beleive_4.4.8')], na.rm = F)

coping_substance_use <- rowSums(coping[, c('renamed.cope_stress_drugs_alc_4.4.4', 'renamed.cope_stress_alc_drugs_4.4.11')], na.rm = F)

coping_emotional_support <- rowSums(coping[, c('renamed.cope_stress_emo_supp_4.4.5', 'renamed.cope_stress_comfort_4.4.15')], na.rm = F)

coping_instrumental_support <- rowSums(coping[, c('renamed.cope_stress_help_from_4.4.10', 'renamed.cope_stress_trying_help_4.4.23')], na.rm = F)

coping_behavioral_disengagment <- rowSums(coping[, c('renamed.cope_stress_giving_up_4.4.6', 'renamed.cope_stress_give_up_cope_4.4.16')], na.rm = F)

coping_venting <- rowSums(coping[, c('renamed.cope_stress_saying_things_4.4.9', 'renamed.cope_stress_exp_neg_4.4.21')], na.rm = F)

coping_positive_reframing <- rowSums(coping[, c('renamed.cope_stress_see_positive_4.4.12', 'renamed.cope_stress_looking_som_good_4.4.17')], na.rm = F)

coping_planning <- rowSums(coping[, c('renamed.cope_stress_stratergy_4.4.14', 'renamed.cope_stress_think_steps_4.4.25')], na.rm = F)

coping_humor <- rowSums(coping[, c('renamed.cope_stress_joke_4.4.18', 'renamed.cope_stress_fun_of_situ_4.4.28')], na.rm = F)

coping_acceptance <- rowSums(coping[, c('renamed.cope_stress_accept_happ_4.4.20', 'renamed.cope_stress_live_with_it_4.4.24')], na.rm = F)

coping_religion <- rowSums(coping[, c('renamed.cope_stress_religion_4.4.22', 'renamed.cope_stress_praying_4.4.27')], na.rm = F)

coping_self_blame <- rowSums(coping[, c('renamed.cope_stress_crit_self_4.4.13', 'renamed.cope_stress_blaming_self_4.4.26')], na.rm = F)

########################################################################################################################################################################################
########################################################################################################################################################################################
########################################################################################################################################################################################
########################################################################################################################################################################################
########################################################################################################################################################################################
########################################################################################################################################################################################
################ getting county name, infection rate, death rate, state from zip code


forzip <- data.frame(renamed$ID, renamed$Zip_code_of_practice_5.10)

colnames(forzip) <- c("id", "zip_code")

# changing 86440 to valid zip code
forzip[grep("86440", forzip$zip_code, value = F),'zip_code'] <- "86440"

# changing locums to NA
forzip[grep("Locums", forzip$zip_code, value = F),'zip_code'] <- NA

# changing 90292 to valid zip code
forzip[grep(", , 90292", forzip$zip_code, value = F),'zip_code'] <- "90292"

# splitting records with "/" into two records
forzip <- forzip %>%
  mutate(zip_code = strsplit(as.character(zip_code), "/")) %>%
  unnest(zip_code)

# multiple to NA
forzip[grep("Multiple", forzip$zip_code, value = F),'zip_code'] <- NA

# splitting by "-" and taking only first value
forzip$zip_code  <- sapply(strsplit(forzip$zip_code, '-'), function (x) x[1])

# orange
forzip[grep("Orange", forzip$zip_code, value = F),'zip_code'] <- "92806"

# converting to numeric
forzip$zip_code <- forzip$zip_code %>% as.numeric()

# loading zip code database
zip_db <- zipcodeR::zip_code_db

# type casting
zip_db$zipcode <- as.numeric(zip_db$zipcode)

# function to get county from zip code
zip2county_1 <- function(z) {

    temp <- subset(zip_db, zip_db$zipcode == z)

    return(temp$county[1])
}

# function to get state from zip code
zip2state <- function(z) {

  temp <- subset(zip_db, zip_db$zipcode == z)

  return(temp$state[1])
}

# getting county and state from zip code
forzip$county_name_1 <- sapply(forzip$zip_code, zip2county_1) %>% tolower()
forzip$state_name_from_zip <- sapply(forzip$zip_code, zip2state)

# removing "county" from county names
forzip$county_name_1 <- gsub("county", "", forzip$county_name_1) %>% trimws()

# changing relevant new york city counties to new york city
forzip[grep("new york", forzip$county_name_1),'county_name_1'] <- "new york city"
forzip[grep("bronx", forzip$county_name_1),'county_name_1'] <- "new york city"
forzip[grep("kings", forzip$county_name_1),'county_name_1'] <- "new york city"
forzip[grep("queens", forzip$county_name_1),'county_name_1'] <- "new york city"
# removing parish
forzip$county_name_1 <- gsub("parish", "", forzip$county_name_1) %>% trimws()

# getting covid data
countiescovid <- read.csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv')

# grouping and taking today's value (inf death)
countiesgrouped <- countiescovid %>% group_by(county, state, fips) %>% arrange(date) %>% slice(c(n()))
countiesgrouped$fips <- countiesgrouped$fips %>% as.numeric()

# function to convert state to two letters
twoletterstate <- function(x) {
  return(state.abb[grep(x, state.name)][1])
  }

# converting state to two letters
countiesgrouped$state2 <- sapply(countiesgrouped$state , twoletterstate)

# changing case
countiesgrouped$county <- tolower(countiesgrouped$county)

# getting two letter for DC
countiesgrouped[grep("district of columbia", countiesgrouped$county),'state2'] <- "DC"


# function to get infected cases count
infcount <- function(x,y){
return(subset(countiesgrouped, countiesgrouped$county == x & countiesgrouped$state2 == y)$cases[1])
}

# function to get death cases count
deathcount <- function(x,y){
  return(subset(countiesgrouped, countiesgrouped$county == x & countiesgrouped$state2 == y)$deaths[1])
}

# getting infection and death count matching zip codes, counties and state
forzip$infcount <- mapply(infcount, forzip$county_name_1, forzip$state_name_from_zip)
forzip$deathcount <- mapply(deathcount, forzip$county_name_1, forzip$state_name_from_zip)

# uncomment this to add the zip code and related information to original dataser

#renamed <- left_join(renamed, forzip, by = c('id' = 'id'))

#######################################################################################################################################################################################
#######################################################################################################################################################################################
#######################################################################################################################################################################################
#######################################################################################################################################################################################
#######################################################################################################################################################################################
#######################


#for resilience spider chart wrt sex

resilience_hardiness_wrt_sex <- data.frame(renamed$resilience_hardiness_cognitivefocus, renamed$resilience_hardiness_flexibility,
                                   renamed$resilience_hardiness_optimism, renamed$resilience_hardiness_regulate_em, renamed$resilience_hardiness_SSE,
                                   renamed$sex_5.2)

colnames(resilience_hardiness_wrt_sex) <- c("Cognitive focus", "Flexibility", "Optimism", "Ability to regulate emotion", "Sense of self efficacy", "Sex")

normalized_resilience_hardiness_wrt_sex <- BBmisc::normalize(resilience_hardiness_wrt_sex[,-ncol(resilience_hardiness_wrt_sex)],
                                                             method = "range", range = c(0,5))
normalized_resilience_hardiness_wrt_sex$Sex <- resilience_hardiness_wrt_sex$Sex

normalized_resilience_hardiness_wrt_sex <- normalized_resilience_hardiness_wrt_sex %>%
                                            group_by(Sex) %>%
                                            summarise_all( funs(mean(., na.rm=TRUE))) %>% filter(Sex == "Male" | Sex == "Female")

rownames(normalized_resilience_hardiness_wrt_sex) <- normalized_resilience_hardiness_wrt_sex$Sex

normalized_resilience_hardiness_wrt_sex <- rbind(rep(5,5) , rep(0,5) , normalized_resilience_hardiness_wrt_sex[,-1])

########################################
#for resilience spider chart wrt ptsd

resilience_hardiness_ptsd <- data.frame(renamed$resilience_hardiness_cognitivefocus, renamed$resilience_hardiness_flexibility,
                                   renamed$resilience_hardiness_optimism, renamed$resilience_hardiness_regulate_em, renamed$resilience_hardiness_SSE,
                                   renamed$ptsd_scaled) %>% na.omit()


colnames(resilience_hardiness_ptsd) <- c("Cognitive focus", "Flexibility", "Optimism", "Ability to regulate emotion", "Sense of self efficacy", "ptsd")

normalized_resilience_hardiness <- BBmisc::normalize(resilience_hardiness_ptsd[,-ncol(resilience_hardiness_ptsd)],
                                                     method = "range", range = c(0,5))
normalized_resilience_hardiness$ptsd <- resilience_hardiness_ptsd$ptsd

normalized_resilience_hardiness <- normalized_resilience_hardiness %>%
  group_by(ptsd) %>%
  summarise_all( funs(mean(., na.rm=TRUE))) 

rownames(normalized_resilience_hardiness) <- normalized_resilience_hardiness$ptsd
normalized_resilience_hardiness <- rbind(rep(5,5) , rep(0,5) , normalized_resilience_hardiness[,-1])
rownames(normalized_resilience_hardiness) <- c("Max", "Min","No", "Probable PTSD")

normalized_resilience_hardiness <- normalized_resilience_hardiness %>% data.frame()


##########################
#for percent of population radar chart

pop <- data.frame(renamed$POP_Black_5.13.1, renamed$POP_Non_Hisp_white_5.13.2, renamed$POP_Hispanic_latino_5.13.3,
                  renamed$POP_Asian_5.13.4, renamed$POP_American_indian_5.13.5, renamed$POP_Native_hawaiian_5.13.6)

colnames(pop) <- c("Black", "Non-Hispanic White", "Hispanic or Latino", "Asian", "American Indian or Alaska Native", "Native Hawaiian or Pacific Islander")

pop <- pop[rowSums(is.na(pop)) != ncol(pop), ] 

pop <- pop %>% summarise_all(funs(mean(.,na.rm = T))) %>% data.frame()

pop <- rbind(rep(100,5) , rep(0,5) , pop)


# resilience based on burnout levels

resilience_hardiness_burnout <- data.frame(renamed$resilience_hardiness_cognitivefocus,renamed$resilience_hardiness_flexibility,
                                                  renamed$resilience_hardiness_optimism, renamed$resilience_hardiness_regulate_em,
                                                  renamed$resilience_hardiness_SSE,
                                                  renamed$level_of_burnout_3.3_scaled) %>% na.omit()

colnames(resilience_hardiness_burnout) <- c("Cognitive focus", "Flexibility", "Optimism", "Ability to regulate emotion",
                                                   "Sense of self efficacy", "Level of burnout")


normalized_resilience_hardiness_burnout <- BBmisc::normalize(resilience_hardiness_burnout[,-ncol(resilience_hardiness_burnout)],
                                                     method = "range", range = c(0,5))


normalized_resilience_hardiness_burnout$burnout <- resilience_hardiness_burnout$`Level of burnout`

normalized_resilience_hardiness_burnout <- normalized_resilience_hardiness_burnout %>%
  group_by(burnout) %>%
  summarise_all( funs(mean(., na.rm=TRUE)))

rownames(normalized_resilience_hardiness_burnout) <- normalized_resilience_hardiness_burnout$burnout

normalized_resilience_hardiness_burnout <- rbind(rep(5,5) , rep(0,5) , normalized_resilience_hardiness_burnout[,-1])

# resilience based on depression levels

resilience_hardiness_depression <- data.frame(renamed$resilience_hardiness_cognitivefocus,renamed$resilience_hardiness_flexibility,
                                              renamed$resilience_hardiness_optimism, renamed$resilience_hardiness_regulate_em,
                                              renamed$resilience_hardiness_SSE,
                                              renamed$depression_final_scaled) %>% na.omit()

colnames(resilience_hardiness_depression) <- c("Cognitive focus", "Flexibility", "Optimism", "Ability to regulate emotion",
                                               "Sense of self efficacy", "Depression")



normalized_resilience_hardiness_depression <- BBmisc::normalize(resilience_hardiness_depression[,-ncol(resilience_hardiness_depression)],
                                                             method = "range", range = c(0,5))

normalized_resilience_hardiness_depression$depression <- resilience_hardiness_depression$Depression


normalized_resilience_hardiness_depression <- normalized_resilience_hardiness_depression %>%
  group_by(depression) %>%
  summarise_all( funs(mean(., na.rm=TRUE)))

normalized_resilience_hardiness_depression <- rbind(rep(5,5) , rep(0,5) , normalized_resilience_hardiness_depression[,-1])

############# PTSD individual symptoms and criterion


ptsdindiv <- data.frame(renamed$PTSD_unwanted_memories_3.2.1, renamed$PTSD_disturbing_dreams_3.2.2, renamed$PTSD_actually_happening_again_3.2.3,
                        renamed$PTSD_upset_when_reminded_3.2.4, renamed$PTSD_physical_reaction_reminded_3.2.5, renamed$PTSD_avoiding_memories_3.2.6,
                        renamed$PTSD_avoiding_externam_reminded_3.2.7, renamed$PTSD_trouble_remembering_3.2.8, renamed$PTSD_negative_belief_self_3.2.9,
                        renamed$PTSD_blaming_self_3.2.10, renamed$PTSD_negative_feelings_3.2.11, renamed$PTSD_loss_of_interest_3.2.12,
                        renamed$PTSD_distant_from_people_3.2.13, renamed$PTSD_trouble_exp_positive_feelings_3.2.14, renamed$PTSD_irritable_behavior_3.2.15,
                        renamed$PTSD_taking_too_many_risk_3.2.16, renamed$PTSD_super_alert_3.2.17, renamed$PTSD_easily_startled_3.2.18,
                        renamed$PTSD_difficulty_concentrating_3.2.19, renamed$PTSD_trouble_asleep_3.2.20, renamed$Direcly_worked_COVID_patients_2.1)



criterionB <- rowSums(ptsdindiv[, c("renamed.PTSD_unwanted_memories_3.2.1", "renamed.PTSD_disturbing_dreams_3.2.2", "renamed.PTSD_actually_happening_again_3.2.3",
                      "renamed.PTSD_upset_when_reminded_3.2.4", "renamed.PTSD_physical_reaction_reminded_3.2.5")] , na.rm = F)


criterionC <- rowSums(ptsdindiv[, c("renamed.PTSD_avoiding_memories_3.2.6", "renamed.PTSD_avoiding_externam_reminded_3.2.7")],   na.rm = F)


criterionD <- rowSums(ptsdindiv[, c("renamed.PTSD_trouble_remembering_3.2.8", "renamed.PTSD_negative_belief_self_3.2.9",
                                    "renamed.PTSD_blaming_self_3.2.10", "renamed.PTSD_negative_feelings_3.2.11",
                                    "renamed.PTSD_loss_of_interest_3.2.12", "renamed.PTSD_distant_from_people_3.2.13", 
                                    "renamed.PTSD_trouble_exp_positive_feelings_3.2.14")],   na.rm = F)

criterionE <- rowSums(ptsdindiv[, c("renamed.PTSD_irritable_behavior_3.2.15", "renamed.PTSD_taking_too_many_risk_3.2.16",
                                    "renamed.PTSD_super_alert_3.2.17", "renamed.PTSD_easily_startled_3.2.18",
                                    "renamed.PTSD_difficulty_concentrating_3.2.19", "renamed.PTSD_trouble_asleep_3.2.20")], na.rm = F)


# getting all criterions and directly worked or not

ptsdcriterion <- data.frame(criterionB, criterionC, criterionD, criterionE)

ptsdcriterion$directlyworked_or_not <- renamed$Direcly_worked_COVID_patients_2.1

# amount of time worked with COVID patients

monthsworked_filtered <-  data.frame(renamed$Months_worked_with_COVID_pat_2.1.1, renamed$Direcly_worked_COVID_patients_2.1, renamed$ptsdfinalscore,
                                     criterionB, criterionC, criterionD, criterionE) %>% 
  filter(renamed.Direcly_worked_COVID_patients_2.1 == "Directly worked") %>% na.omit()


# underlying health condition

underlyinghc <- data.frame(renamed$underlyinh_health_condition_2.2, renamed$ptsdfinalscore, criterionB, criterionC, criterionD, criterionE) %>% na.omit()

# covid fears

covidfears <- data.frame(renamed$Demo_fear_cont_COVID_2.4.1, renamed$ptsdfinalscore, criterionB, criterionC, criterionD, criterionE) %>% na.omit()

# race questions for table - reverting onehot and adding directly worked with covid

racequestions_for_table <- data.frame(renamed$race_americanindian_5.4.1, renamed$race_asian_5.4.2, renamed$race_black_5.4.3, renamed$race_native_haw_5.4.4, 
                                      renamed$race_white_5.4.5, renamed$race_others_5.4.6, renamed$ID) 

racewithid <-  revert_onehot_with_id(racequestions_for_table)

racewithid <- inner_join(racewithid, renamed[, c("ID","Direcly_worked_COVID_patients_2.1") ], by = c('ID' = 'ID'))

# speciality questions for table - reverting onehot and adding directly worked with covid

specialityquestions_for_table <- data.frame(renamed$Speciality_Adult_CC_5.7.1, renamed$Speciality_Anesthesiology_5.7.2, renamed$Speciality_Dermatology_5.7.3, renamed$Speciality_Cardiology_5.7.4,
                                            renamed$Speciality_Electrophysiology_5.7.5, renamed$Speciality_Emergency_med_5.7.6, renamed$Speciality_Endocrinology_5.7.7,
                                            renamed$Speciality_Family_med_5.7.8, renamed$Speciality_Gasetroenterologist_5.7.9, renamed$Speciality_General_surgery_5.7.10, 
                                            renamed$Speciality_Geriatrics_5.7.11, renamed$Speciality_Immunology_5.7.12, renamed$Speciality_Infectious_disease_5.7.13, renamed$Speciality_Internal_med_5.7.14,
                                            renamed$Speciality_Interventional_radiology_5.7.15, renamed$Speciality_Medical_genetica_5.7.16, renamed$Speciality_Neonatal_CC_5.7.17, renamed$Speciality_Neurology_5.7.18,
                                            renamed$Speciality_Neurosurgery_5.7.19, renamed$Speciality_obsterics_n_Gynecology_5.7.20, renamed$Speciality_Oncology_5.7.21,
                                            renamed$Speciality_Opthalmology_5.7.22, renamed$Speciality_Orthopedics_5.7.23, renamed$Speciality_Orthopedic_surg_5.7.24,
                                            renamed$Speciality_Otolaryngology_5.7.25, renamed$Speciality_Pathology_5.7.26, renamed$Speciality_Pediatrics_5.7.27,
                                            renamed$Speciality_Pediatric_CC_5.7.28, renamed$Speciality_Pediatric_emer_med_5.7.29, renamed$Speciality_Physical_med_5.7.30,
                                            renamed$Speciality_Podiatry_5.7.31, renamed$Speciality_Preventive_med_5.7.32, renamed$Speciality_Psychiatry_5.7.33, renamed$Speciality_Pulmonology_5.7.34,
                                            renamed$Speciality_Radiation_oncology_5.7.35, renamed$Speciality_Radiology_5.7.36, renamed$Speciality_Rheumatology_5.7.37,
                                            renamed$Speciality_Urology_5.7.38, renamed$Speciality_Vascular_surg_5.7.39, renamed$Speciality_Other_5.7.40, renamed$ID)

specialitywithid <- revert_onehot_with_id(specialityquestions_for_table)

specialitywithid <- inner_join(specialitywithid, renamed[, c("ID","Direcly_worked_COVID_patients_2.1") ], by = c('ID' = 'ID'))

# type of hospital questions for table - reverting onehot and adding directly worked with covid


forhosptyperena <- renamed %>% filter(Primary_work_setting_5.11 == "Hospital")

hospitaltype_for_table <- data.frame(forhosptyperena$Work_Hosp_urban_5.11.1.1, forhosptyperena$Work_Hosp_Suburban_5.11.1.2, forhosptyperena$Work_Hosp_Rural_5.11.1.3,
                                     forhosptyperena$Work_Hosp_Speciality_5.11.1.4, forhosptyperena$Work_Hosp_Veterans_5.11.1.5, forhosptyperena$Work_Hosp_Community_5.11.1.6,
                                     forhosptyperena$Work_Hosp_Teaching_research_5.11.1.7, forhosptyperena$Work_Hosp_Non_Fed_psy_5.11.1.8, forhosptyperena$Work_Hosp_Non_Fed_Long_term_5.11.1.9,
                                     forhosptyperena$Work_Hosp_Rehabilitation_5.11.1.10, forhosptyperena$Work_Hosp_Critical_acc_5.11.1.11, forhosptyperena$Work_Hosp_Small_5.11.1.12,
                                     forhosptyperena$Work_Hosp_Medium_5.11.1.13, forhosptyperena$Work_Hosp_Large_5.11.1.14, forhosptyperena$Work_Hosp_others_5.11.1.15, forhosptyperena$ID)

hospitaltypewithid <- revert_onehot_with_id(hospitaltype_for_table)

hospitaltypewithid <- inner_join(hospitaltypewithid,  forhosptyperena[, c("ID","Direcly_worked_COVID_patients_2.1") ], by = c('ID' = 'ID'))


# worksettingwithinhospital_for_table  


forwrksettingwithhosp <- renamed %>% filter(Primary_work_setting_5.11 == "Hospital")

worksettingwithinhospital_for_table <- data.frame(forwrksettingwithhosp$Work_setting_emergency_dept_5.11.2.1, forwrksettingwithhosp$work_setting_general_floor_5.11.2.2, forwrksettingwithhosp$work_setting_step_down_unit_5.11.2.3,
                                                  forwrksettingwithhosp$work_setting_ICU_5.11.2.4, forwrksettingwithhosp$work_setting_operating_room_5.11.2.5, forwrksettingwithhosp$work_setting_other_5.11.2.6, forwrksettingwithhosp$ID)

worksettingwithinhospitalwithid <- revert_onehot_with_id(worksettingwithinhospital_for_table)

worksettingwithinhospitalwithid <- inner_join(worksettingwithinhospitalwithid, renamed[, c("ID","Direcly_worked_COVID_patients_2.1") ], by = c('ID' = 'ID'))


# Creating PTSD response variable

ptsdindivid_20_qns <- ptsdindiv[,-ncol(ptsdindiv)]

ptsdindivid_20_qns$fin_score <- renamed$ptsdfinalscore

ptsdindivid_20_qns$covid_exposure <- renamed$Direcly_worked_COVID_patients_2.1 

criterion_b_cond <- data.frame(ptsdindivid_20_qns$renamed.PTSD_unwanted_memories_3.2.1 >= 2, 
                               ptsdindivid_20_qns$renamed.PTSD_disturbing_dreams_3.2.2 >= 2,
                               ptsdindivid_20_qns$renamed.PTSD_actually_happening_again_3.2.3 >= 2,
                               ptsdindivid_20_qns$renamed.PTSD_upset_when_reminded_3.2.4 >= 2,
                               ptsdindivid_20_qns$renamed.PTSD_physical_reaction_reminded_3.2.5 >= 2)

criterion_c_cond <- data.frame(ptsdindivid_20_qns$renamed.PTSD_avoiding_memories_3.2.6 >= 2,
                               ptsdindivid_20_qns$renamed.PTSD_avoiding_externam_reminded_3.2.7 >= 2)


criterion_d_cond <- data.frame(ptsdindivid_20_qns$renamed.PTSD_trouble_remembering_3.2.8 >= 2,
                               ptsdindivid_20_qns$renamed.PTSD_negative_belief_self_3.2.9 >= 2,
                               ptsdindivid_20_qns$renamed.PTSD_blaming_self_3.2.10 >= 2,
                               ptsdindivid_20_qns$renamed.PTSD_negative_feelings_3.2.11 >= 2,
                               ptsdindivid_20_qns$renamed.PTSD_loss_of_interest_3.2.12 >= 2,
                               ptsdindivid_20_qns$renamed.PTSD_distant_from_people_3.2.13 >= 2,
                               ptsdindivid_20_qns$renamed.PTSD_trouble_exp_positive_feelings_3.2.14 >= 2)


criterion_e_cond <- data.frame(ptsdindivid_20_qns$renamed.PTSD_irritable_behavior_3.2.15 >= 2,
                               ptsdindivid_20_qns$renamed.PTSD_taking_too_many_risk_3.2.16 >= 2,
                               ptsdindivid_20_qns$renamed.PTSD_super_alert_3.2.17 >= 2,
                               ptsdindivid_20_qns$renamed.PTSD_easily_startled_3.2.18 >= 2,
                               ptsdindivid_20_qns$renamed.PTSD_difficulty_concentrating_3.2.19 >= 2,
                               ptsdindivid_20_qns$renamed.PTSD_trouble_asleep_3.2.20 >= 2)


ptsdindivid_20_qns$ptsd_scaled <- ifelse(((apply(criterion_b_cond, 1, sum) >= 1) + (apply(criterion_c_cond, 1, sum) >= 1) + (apply(criterion_d_cond, 1, sum) >= 2)
                                         + (apply(criterion_e_cond, 1, sum) >= 2)) >= 3 & ptsdindivid_20_qns$fin_score >= 33, "Probable",
                                 
                                 
                                  ifelse((((apply(criterion_b_cond, 1, sum) >= 1) + (apply(criterion_c_cond, 1, sum) >= 1) + (apply(criterion_d_cond, 1, sum) >= 2)
                                         + (apply(criterion_e_cond, 1, sum) >= 2)) == 2 & ptsdindivid_20_qns$fin_score >= 33) | 
                                          
                                        (((apply(criterion_b_cond, 1, sum) >= 1) + (apply(criterion_c_cond, 1, sum) >= 1) + (apply(criterion_d_cond, 1, sum) >= 2)
                                         + (apply(criterion_e_cond, 1, sum) >= 2)) >= 2 & between(ptsdindivid_20_qns$fin_score,12,32)), "sub-clinical",
                                        
                                        
                                  ifelse((((apply(criterion_b_cond, 1, sum) >= 1) + (apply(criterion_c_cond, 1, sum) >= 1) + (apply(criterion_d_cond, 1, sum) >= 2)
                                         + (apply(criterion_e_cond, 1, sum) >= 2)) == 1 & between(ptsdindivid_20_qns$fin_score,12,32)) |
                                               
                                        (((apply(criterion_b_cond, 1, sum) >= 1) + (apply(criterion_c_cond, 1, sum) >= 1) + (apply(criterion_d_cond, 1, sum) >= 2)
                                         + (apply(criterion_e_cond, 1, sum) >= 2)) == 1 & ptsdindivid_20_qns$fin_score >= 33) |
                                                 
                                        (((apply(criterion_b_cond, 1, sum) >= 1) + (apply(criterion_c_cond, 1, sum) >= 1) + (apply(criterion_d_cond, 1, sum) >= 2)
                                         + (apply(criterion_e_cond, 1, sum) >= 2)) < 1 & between(ptsdindivid_20_qns$fin_score,12,21)), "Pre-subclinical",
                                  ifelse(ptsdindivid_20_qns$fin_score < 12, "No", "dk"))))
                          


renamed$ptsd_scaled <- ptsdindivid_20_qns$ptsd_scaled

renamed$ptsd_scaled <- factor(renamed$ptsd_scaled, levels = c( 'No',  "Pre-subclinical",  "sub-clinical", 'Probable'))

####################################################################################################################################################
####################################################################################################################################################
####################################################################################################################################################
####################################################################################################################################################
####################################################################################################################################################

