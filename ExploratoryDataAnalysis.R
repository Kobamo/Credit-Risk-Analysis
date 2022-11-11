library(dplyr)
library(ggplot2)
library(ggthemes)
library(plyr)
library(InformationValue)

################### D A T A ##################################
credit_data = read.csv("credit_data.csv")

#removing disapproved credit cards
creditDataEdit = credit_data %>%
               filter(CARDHLDR == 1, AGE >= 18 & AGE <= 70) %>%
               mutate(
                DEFAULT = as.factor(DEFAULT),
                OWNRENT = as.factor(OWNRENT),
                SELFEMPL = as.factor(SELFEMPL),
                LOGINC = log(INCOME),
                MAJORDRG = as.factor(ifelse(MAJORDRG > 0, 1, 0)),
                MINORDRG = as.factor(ifelse(MINORDRG > 0, 1, 0))
               ) %>%
               as_tibble()
summary(creditDataEdit)


############## P R E L I M I N A R I E S ########################
creditDataEdit %>%
 select(DEFAULT) %>%
 table() %>%
 prop.table()


######### E X P L O  R A T O R Y     A N A L Y S I S ################
###################################################################

default_set = filter(creditDataEdit, DEFAULT == 1)
non_default_set =  filter(creditDataEdit, DEFAULT == 0)

#############3##### H O M E    O W N E R S H I P #######################

ggplot(creditDataEdit) +
 geom_bar(aes(x = OWNRENT, (..count..) / sum(..count..),
   fill = DEFAULT), position = 'dodge', width = .35, colour='black') +
 scale_x_discrete(labels = c('Rent', 'Own')) +
 scale_y_continuous(labels = scales::percent) +
 labs(
   x = 'Home Ownership',
   y = 'Percentage',
   fill = 'Default Status',
   title = 'Default status by home ownership') +
 scale_fill_discrete(labels = c('Non-default', 'Default')) +
 theme_minimal()

################## E M P L O Y M E N T   T Y P E #############################
ggplot(creditDataEdit) +
  geom_bar(aes(x=SELFEMPL, (..count..) / sum(..count..),
    fill = DEFAULT ), width = .3, colour = 'black', position = 'fill') +
    labs(
      title = 'Number of defaults by employment status',
      x = 'Employment type', 
      y = 'Proportion',
      fill = 'Defaault Status') +
  scale_x_discrete(labels = c('Regular Employment', 'Self Employment')) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(labels = c('Non-default', 'Default')) + theme_minimal()

 #################### S P E N D I N G ###################################
ggplot(creditDataEdit) + geom_boxplot(aes(x=DEFAULT, y=LOGSPEND), width = .25, outlier.shape = 1,
   colour = 'black', fill = 'pink' ) + theme_minimal()+
  labs(title = 'Default status by Spending', x = 'Default status', y ='Log Spending') + 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  scale_x_discrete(labels = c('Non-default', 'Default')) 

##################### I N C O M E #################################################
ggplot(creditDataEdit) + geom_boxplot(aes(x=DEFAULT, y=LOGINC), width = .25, outlier.shape = 1,
                                  colour = 'black', fill = 'pink' ) + theme_minimal()+
  labs(title = 'Default status by Income', x = 'Default status', y ='Log Income') + 
  theme(axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0))) +
  scale_x_discrete(labels = c('Non-default', 'Default')) 

mean(default_set$INCOME)
mean(non_default_set$INCOME)

#################### A G E #############################
ggplot(creditDataEdit) + geom_boxplot(aes(x=DEFAULT, y=AGE), width = .25, outlier.shape = 1,
                                  colour = 'black', fill = 'pink' ) + theme_minimal()+
  labs(title = 'Default status by age', x = 'Default status', y ='Age') + 
  theme(axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0))) +
  scale_x_discrete(labels = c('Non-default', 'Default')) 

mean(default_set$AGE)
mean(non_default_set$AGE)
median(default_set$AGE)
median(non_default_set$AGE)

################### R E S I D E N C E ############################
ggplot(creditDataEdit) + geom_boxplot(aes(x=DEFAULT, y=ACADMOS), width = .25, outlier.shape = NA,
                                  colour = 'black', fill = 'pink' ) + theme_minimal()+
  labs(title = 'Default status by length of stay at surrent address', x = 'Default status', y ='Months') + 
  theme(axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0))) +
  scale_x_discrete(labels = c('Non-default', 'Default')) 

mean(default_set$ACADMOS)
mean(non_default_set$ACADMOS)
median(default_set$ACADMOS)
median(non_default_set$ACADMOS) 

######################## M A J O R  D E R O G A T O R I E S ############

ggplot(creditDataEdit) + geom_bar(aes(x=MAJORDRG, fill = DEFAULT), position = 'fill',
width = .25, colour = 'black') + 
  scale_x_discrete(labels=c('Non-default', 'Default')) + scale_fill_discrete(
labels = c('Non-default', 'Default')) + scale_x_discrete(labels =
c('No major report', 'At least 1 major report')) + scale_y_continuous(
labels = scales::percent) + labs(x = '', y = 'Percentage', fill = 'Default status:', 
title = 'Default status by major derogatory reports') + theme_hc() 

######################### M I N O R  D E R O G A T O R I E S#######################
ggplot(creditDataEdit) + geom_bar(aes(x=MINORDRG, fill = DEFAULT), 
                                  position = 'fill',width = .25, colour = 'black') + 
  scale_x_discrete(labels=c('Non-default', 'Default')) + 
  scale_fill_discrete(labels = c('Non-default', 'Default')) + 
  scale_x_discrete(labels =c('No minor report', 'At least 1 minor report')) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = '', y = 'Percentage', 
       fill = 'Default status:', 
       title = 'Default status by minor derogatory reports') + 
  theme_hc()


######################### I N F O R M A T I O N     V A L U E ######################

cat.dependents = as.factor(creditDataEdit$ADEPCNT)

IVhome = IV(creditDataEdit$OWNRENT, creditDataEdit$DEFAULT)
IVemp = IV(creditDataEdit$SELFEMPL, creditDataEdit$DEFAULT)
IVmajor = IV(creditDataEdit$MAJORDRG, creditDataEdit$DEFAULT)
IVminor = IV(creditDataEdit$MINORDRG, creditDataEdit$DEFAULT)
IVdep = IV(cat.dependents, creditDataEdit$DEFAULT)

Var = c('Home Ownership', 'Number of dependents', 'Minor Derogatory reports',
        'Major Deragatory reports', 'Employment type')
IV = c(IVhome, IVdep, IVminor, IVmajor, IVemp)

IVdata = data.frame(Var, IV)


ggplot(IVdata) + geom_bar(aes(x=reorder(Var, -IV), y = IV), stat = 'identity', 
colour = 'Black', fill = 'red', width = .5) + labs(
title = "Information Value", x = '') + coord_flip() + theme_minimal() 

