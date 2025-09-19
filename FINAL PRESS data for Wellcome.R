library(tidyverse)
library(readxl)
library(writexl)
library(scales)

setwd('D:/ODW 2025/Wellcome')


##### Read in PRESS data #####

press <- read_xlsx('PRESS 2024 data - sharing w ODW.xlsx') %>%
  select(year, donorcode, donorname, agencycode, agencyname, crsid, projectnumber,
         recipientname, incomegroupname, flowcode, bi_multi, usd_commitment_defl,
         usd_disbursement_defl,projecttitle, purposecode, sectorcode,
         longdescription, db_ref, dac_regionname) %>%
  filter(year > 2017)

# filtering for relevant purpose codes:
# making core group
presscoresec <- press %>%
  filter(sectorcode %in% c('121', '122', '123', '130'))
presscorepurp <- press %>%
  filter(purposecode %in% c('72011','43071','43072','43073','16064'))
# need to manually go through 72010 so do it separately
press72010 <- press %>%
  filter(purposecode == '72010')

press72010 <- press72010 %>%
  filter(!grepl('2018-21775', projectnumber, ignore.case = TRUE)) %>%
  filter(!grepl('1467', projectnumber, ignore.case = TRUE)) %>%
  filter(!grepl('65_14470', projectnumber, ignore.case = TRUE)) %>%
  filter(!grepl('6850/A0/04/887/004', projectnumber, ignore.case = TRUE)) %>%
  filter(!grepl('6850/A0/04/887/006', projectnumber, ignore.case = TRUE)) %>%
  filter(!grepl('3180/A0/07/886/003', projectnumber, ignore.case = TRUE)) %>%
  filter(!grepl('65_11213', projectnumber, ignore.case = TRUE)) %>%
  filter(!grepl('3380/A0/08/880/003', projectnumber, ignore.case = TRUE)) %>%
  filter(!grepl('3180/A0/07/886/003', projectnumber, ignore.case = TRUE)) %>%
  filter(!grepl('SI2.830773', projectnumber, ignore.case = TRUE)) %>%
  filter(!grepl('76_50350', projectnumber, ignore.case = TRUE)) %>%
  filter(!grepl('65_11278', projectnumber, ignore.case = TRUE)) %>%
  filter(!grepl('65_14758', projectnumber, ignore.case = TRUE)) %>%
  filter(!grepl('65_11279', projectnumber, ignore.case = TRUE)) %>%
  filter(!grepl('SI2.854652', projectnumber, ignore.case = TRUE)) %>%
  filter(!grepl('Establishing data standards and dev', projecttitle, ignore.case = TRUE)) %>%
  filter(!grepl('PRM & DHS MOC[Other Contractual Services NOC]', projecttitle, ignore.case = TRUE)) %>%
  filter(!grepl('3180/A0/07/886/003', projectnumber, ignore.case = TRUE)) %>%
  filter(!grepl('3380/A0/08/880/003', projectnumber, ignore.case = TRUE)) %>%
  filter(!grepl('camp expenses in', projecttitle, ignore.case = TRUE)) %>%
  filter(!grepl('3180/A0/07/886/003', projectnumber, ignore.case = TRUE)) %>%
  filter(!grepl('4550/A0/06/005/004', projectnumber, ignore.case = TRUE)) %>%
  filter(!grepl('6850/A0/04/887/006', projectnumber, ignore.case = TRUE)) %>%
  filter(!grepl('SI2.875097', projectnumber, ignore.case = TRUE)) %>%
  filter(!grepl('SI2.830773', projectnumber, ignore.case = TRUE))

presscore <- union(presscorepurp, presscoresec)
presscore <- union(presscore, press72010)

pressdetailedsec <- press %>%
  filter(sectorcode %in% c('520','410','470'))
pressdetailedpurp <- press %>%
  filter(purposecode %in% c('12340','12250','12262','12263','12264','13040','12310',
                            '12350','12382','12191','12220','12230','12320','12330',
                            '13020','13030','12240','11250','43071','43072','43073',
                            '52020','41010','41020','41030','41040','43060','12181',
                            '12182','12196','41081','41082','13081','12261','12281',
                            '13096','16064'))
pressdetailed <- union(pressdetailedpurp, pressdetailedsec)

wellcome <- union(presscore, pressdetailed)


##### creating the filter to add in specific programs mentioned in outline: #####
# going to filter these from the original PRESS dataset to account for the variety
# of purpose codes used. some may not have been included in the filter.

## DHS, MICS, PEPFAR, CRVS (already have the CRVS data from financing paper),
## DHIS2, MICS, IHME, UNAIDS, WHO, FEWS-NET, LSMS, AGRISurvey, DATIM, GMIS,
## IPUMS, IAEG-SDGs, GAVI

#### DHS ####

dhspress <- press %>%
  filter(grepl('DHS', longdescription, ignore.case = F) |
           grepl('DHS', projecttitle, ignore.case = F) |
           grepl('demographic and health survey',longdescription, ignore.case = T) |
           grepl('demographic and health survey',projecttitle, ignore.case = T) |
           grepl('demographic & health survey',longdescription, ignore.case = T) |
           grepl('demographic & health survey',projecttitle, ignore.case = T) |
           grepl('demographic health survey', longdescription, ignore.case = T) |
           grepl('demographic health survey', projecttitle, ignore.case = T))

# need to remove the DHS ones that are not about the demographic and health survey:
dhspress <- dhspress %>%
  filter(!grepl('International Aviation Security Technical Training', projecttitle, ignore.case = T)) %>%
  filter(!grepl('U.S. Department of State', longdescription, ignore.case = TRUE)) %>%
  filter(!grepl('U.S. Department of State', projecttitle, ignore.case = TRUE)) %>%
  filter(!grepl('PRM & DHS MOC', projecttitle, ignore.case = TRUE)) %>%
  filter(!grepl('4TH OPTION', projecttitle, ignore.case = TRUE)) %>%
  filter(!grepl('009-115128-A', projectnumber)) %>%
  filter(!grepl('009-149922-A', projectnumber)) %>%
  filter(!grepl('76_68998', projectnumber))

# join to dataset: 
wellcome <- union(wellcome, dhspress)

#### PEPFAR ####
pepfarpress <- press %>%
  filter(grepl('PEPFAR', projecttitle) |
           grepl('PEPFAR', longdescription) |
           grepl("President's Emergency Plan for AIDS Relief", projecttitle, ignore.case = TRUE) |
           grepl("President's Emergency Plan for AIDS Relief", longdescription, ignore.case = TRUE))

# join to dataset:
wellcome <- union(wellcome, pepfarpress)


#### DHIS2 ####
dhis2press <- press %>%
  filter(grepl('dhis2', projecttitle, ignore.case = TRUE) |
           grepl('dhis2', longdescription, ignore.case = TRUE) |
           grepl('district health information s', projecttitle, ignore.case = TRUE) |
           grepl('district health information s', longdescription, ignore.case = TRUE))

# join to dataset:
wellcome <- union(wellcome, dhis2press)


#### UNAIDS ####
unaidspress <- press %>%
  filter(grepl('UNAIDS', projecttitle, ignore.case = T) |
           grepl('UNAIDS', longdescription, ignore.case = T)|
           grepl('UNAIDS', donorname, ignore.case = T))
# didnt do full name bc all those under the full name used UNAIDS overlap btwn pepfar and UNAIDS 

# join to dataset:
wellcome <- union(wellcome, unaidspress)


#### WHO ####
whopress <- press %>%
  filter(grepl('World Health Organi', projecttitle, ignore.case = T) |
           grepl('World Health Organi', longdescription, ignore.case = T) |
           grepl('World Health Organi', donorname, ignore.case = T) |
           grepl('WHO', projecttitle, ignore.case = F) |
           grepl('WHO', longdescription, ignore.case = F) |
           grepl('WHO', donorname, ignore.case = F))


# join to dataset
wellcome <- union(wellcome, whopress)

#### FEWS-NET ####
# no entries in press for either "FEWS-NET" and "Famine Early Warning Systems Network"


#### LSMS ####
lsmspress <- press %>%
  filter(grepl('LSMS', longdescription, ignore.case = F)|
           grepl('LSMS', projecttitle, ignore.case = F) |
           grepl('living standards measurement s', longdescription, ignore.case = T) |
           grepl('living standards measurement s', projecttitle, ignore.case = T))


# join to dataset:
wellcome <- union(wellcome, lsmspress)

#### AGRISurvey ####
# in PRESSS as AGRIS or "Agricultural integrated survey"
agrispress <- press %>%
  filter(grepl('AGRIS', projecttitle, ignore.case = F) |
           grepl('AGRIS', longdescription, ignore.case = F) |
           grepl('Agricultural integrated survey', projecttitle) |
           grepl('Agricultural integrated survey', longdescription))

# join to dataset:
wellcome <- union(wellcome, agrispress)

#### DATIM ####
# nothing either for DATIm or for "data for accountability, transparency, and impact monitoring

#### GMIS ####
# nothing either for GMIS or "grant management information system"

#### IPUMS ####
ipumspress <- press %>%
  filter(grepl('IPUMS', projecttitle, ignore.case = F) |
           grepl('IPUMS', longdescription, ignore.case = F)) # didnt use the full name since they overlap already


# join to dataset:
wellcome <- union(wellcome, ipumspress)

#### IAEG-SDGS ####
iaegpress <- press %>%
  filter(grepl('IAEG-SDG', projecttitle, ignore.case = F)|
           grepl('IAEG-SDG', longdescription, ignore.case = F)|
           grepl('Inter-agency and Expert Group on S', projecttitle, ignore.case = T)|
           grepl('Inter-agency and Expert Group on S', longdescription, ignore.case = T))

# join to dataset:
wellcome <- union(iaegpress, wellcome)

#### Gavi ####
# only 2 and both are already caught by main filter
gavipress <- press %>%
  filter(grepl('gavi', projecttitle, ignore.case = T) |
           grepl('gavi', longdescription, ignore.case = T))

# join to dataset:
wellcome <- union(wellcome, gavipress)

#### MICS ####
micspress <- press %>%
  filter(grepl('multiple indicator', projecttitle, ignore.case = TRUE) |
           grepl('multiple indicator', longdescription, ignore.case = TRUE) |
           grepl('MICS', projecttitle) |
           grepl('MICS', longdescription) |
           grepl('multi indicator', projecttitle, ignore.case = TRUE) |
           grepl('multi indicator', longdescription, ignore.case = TRUE))

# remove the unrelated one
micspress <- micspress %>%
  filter(!grepl('009-155542-A', projectnumber))

# join to dataset:
wellcome <- union(wellcome, micspress)

#### IHME ####

ihmepress <- press %>%
  filter(grepl('IHME', projecttitle) |
           grepl('IHME', longdescription) |
           grepl('institute for health metrics and evaluation', longdescription, ignore.case = TRUE) |
           grepl('institute for health metrics and evaluation', projecttitle, ignore.case = TRUE))

# only 3, and all 3 are captured by main dataset.

# join to dataset:
wellcome <- union(wellcome, ihmepress)

#### HMIS ####
hmispress <- press %>%
  filter(grepl('HMIS', projecttitle, ignore.case = F)|
           grepl('HMIS', longdescription, ignore.case = F) |
           grepl('health management information s', projecttitle, ignore.case = T) |
           grepl('health management information s', longdescription, ignore.case = T))

# join to dataset:
wellcome <- union(wellcome, hmispress)


#### PMI: ####
# no results 



##### running health key word filters for the statistical capacity building and environment code #####
#### SCB (16062)
pressSCB <- press %>%
  filter(purposecode == 16062)

## project title filter
# case insensitive:
SCBtitle1 <- pressSCB %>%
  filter(grepl("health|santé|médic|sanitair|salud|nutrition|nourish|nutrición|aliment|disease|illness|sick|malad|affection|enferm|afección|demogra|démogr|population|población|fertil|fécond|fecund|mortal|birth|naissan|nacimiento|death|décès|mort|muerte|sanitation|assainissement|saneamiento|tuberculos|immunodefici|immunodéf|hepatit|hépatit|non-communicable|no transmisibles|non transmissible|malaria|paludism|reproducti|sexual|sexuel|matern|immuni|vaccin|vacuna|human services|servicios humanos|services sociaux|infectio|infecci|wellness|bien-être|bienestar|child health|santé des enfant|infant|enfant|nourris|morbidi|behavioral|behavioural|comportemental|comportamiento|under-five|under-5|moins de cinq| moins de 5|menores de cinco|menores de 5|neonatal|néonatal|antenatal|prenatal|prénatal|childbearing|de procreér|contracep|concept|concepc|weight|peso|poids|transmis|calori|calorí|diarr|fever|fièvre|fiebre|respirat|menstru|hygiene|hygiène|body mass index|indice de masse corporelle|índice de masa corporal|stunt|croissance|retraso|wasted|wasting|émaciation|consunción|marasmo|obesi|mosquito-born|transmises par les moustique|transmitidas por mosquito|medic|médic|covid|virus|pandemi|pandém|epidem|épidém|food security|sécurité aliment|seguridad aliment|supplement|complement|suplement|service delivery|prestation de service|prestation des services|offre de service|prestación del servicio|provisión de servicio|servicios prestados|drug|droga|disability|disable|hándicap|invalidit|descapacidad|air quality|qualité de l'air|calidad del aire|aging|ageing|vieilliss|envejecimiento|civil regis|vital statistics|statistiques de l'état civil|registro civil|estadísticas vital|sida", 
               projecttitle, ignore.case = TRUE))

# those that are case sensitive (acronyms, to make sure they aren't included bc they show up in words
SCBtitle2 <- pressSCB %>%
  filter(grepl('TB|NCD|BMI|IMC|SRH|SSR|IST|CTI|STD|STI|ETS|MST|MCH|HIV|VIH|AIDS|WASH|EHA|EAH|CRVS|ESEC', projecttitle, ignore.case = FALSE))


## merge the two:
SCBtitle <- union(SCBtitle1, SCBtitle2)


## long description filter - case insensitive:
SCBdes1 <- pressSCB %>%
  filter(grepl("health|santé|médic|sanitair|salud|nutrition|nourish|nutrición|aliment|disease|illness|sick|malad|affection|enferm|afección|demogra|démogr|population|población|fertil|fécond|fecund|mortal|birth|naissan|nacimiento|death|décès|mort|muerte|sanitation|assainissement|saneamiento|tuberculos|immunodefici|immunodéf|hepatit|hépatit|non-communicable|no transmisibles|non transmissible|malaria|paludism|reproducti|sexual|sexuel|matern|immuni|vaccin|vacuna|human services|servicios humanos|services sociaux|infectio|infecci|wellness|bien-être|bienestar|child health|santé des enfant|infant|enfant|nourris|morbidi|behavioral|behavioural|comportemental|comportamiento|under-five|under-5|moins de cinq| moins de 5|menores de cinco|menores de 5|neonatal|néonatal|antenatal|prenatal|prénatal|childbearing|de procreér|contracep|concept|concepc|weight|peso|poids|transmis|calori|calorí|diarr|fever|fièvre|fiebre|respirat|menstru|hygiene|hygiène|body mass index|indice de masse corporelle|índice de masa corporal|stunt|croissance|retraso|wasted|wasting|émaciation|consunción|marasmo|obesi|mosquito-born|transmises par les moustique|transmitidas por mosquito|medic|médic|covid|virus|pandemi|pandém|epidem|épidém|food security|sécurité aliment|seguridad aliment|supplement|complement|suplement|service delivery|prestation de service|prestation des services|offre de service|prestación del servicio|provisión de servicio|servicios prestados|drug|droga|disability|disable|hándicap|invalidit|descapacidad|air quality|qualité de l'air|calidad del aire|aging|ageing|vieilliss|envejecimiento|civil regis|vital statistics|statistiques de l'état civil|registro civil|estadísticas vital|sida",
               longdescription, ignore.case = TRUE))
# those that are case sensitive (acronyms, to make sure they aren't included bc they show up in words
SCBdes2 <- pressSCB %>%
  filter(grepl('TB|NCD|BMI|IMC|SRH|SSR|IST|CTI|STD|STI|ETS|MST|MCH|HIV|VIH|AIDS|WASH|EHA|EAH|CRVS|ESEC', longdescription, ignore.case = FALSE))

## merge the two
SCBdes <- union(SCBdes1, SCBdes2)

## merge title and des together:
presshealthSCB <- union(SCBdes, SCBtitle)

## Join 16062 SCB with main dataset:
wellcome <- union(presshealthSCB, wellcome)

#### Adding environment projects ####
# using the environment sector code (410) and filtering by health related words:
pressENV <- press %>%
  filter(sectorcode == 410)

## project title
# case insensitive:
envtitle1 <- pressENV %>%
  filter(grepl("health|santé|médic|sanitair|salud|nutrition|nourish|nutrición|aliment|disease|illness|sick|malad|affection|enferm|afección|demogra|démogr|population|población|fertil|fécond|fecund|mortal|birth|naissan|nacimiento|death|décès|mort|muerte|sanitation|assainissement|saneamiento|tuberculos|immunodefici|immunodéf|hepatit|hépatit|non-communicable|no transmisibles|non transmissible|malaria|paludism|reproducti|sexual|sexuel|matern|immuni|vaccin|vacuna|human services|servicios humanos|services sociaux|infectio|infecci|wellness|bien-être|bienestar|child health|santé des enfant|infant|enfant|nourris|morbidi|behavioral|behavioural|comportemental|comportamiento|under-five|under-5|moins de cinq| moins de 5|menores de cinco|menores de 5|neonatal|néonatal|antenatal|prenatal|prénatal|childbearing|de procreér|contracep|concept|concepc|weight|peso|poids|transmis|calori|calorí|diarr|fever|fièvre|fiebre|respirat|menstru|hygiene|hygiène|body mass index|indice de masse corporelle|índice de masa corporal|stunt|croissance|retraso|wasted|wasting|émaciation|consunción|marasmo|obesi|mosquito-born|transmises par les moustique|transmitidas por mosquito|medic|médic|covid|virus|pandemi|pandém|epidem|épidém|food security|sécurité aliment|seguridad aliment|supplement|complement|suplement|service delivery|prestation de service|prestation des services|offre de service|prestación del servicio|provisión de servicio|servicios prestados|drug|droga|disability|disable|hándicap|invalidit|descapacidad|air quality|qualité de l'air|calidad del aire|aging|ageing|vieilliss|envejecimiento|civil regis|vital statistics|statistiques de l'état civil|registro civil|estadísticas vital|sida", 
               projecttitle, ignore.case = TRUE))

# those that are case sensitive (acronyms, to make sure they aren't included bc they show up in words
envtitle2 <- pressENV %>%
  filter(grepl('TB|NCD|BMI|IMC|SRH|SSR|IST|CTI|STD|STI|ETS|MST|MCH|HIV|VIH|AIDS|WASH|EHA|EAH|CRVS|ESEC', projecttitle, ignore.case = FALSE))

# merge together:
envtitle <- union(envtitle1, envtitle2)

## long description
# case insensitive
envdes1 <- pressENV %>%
  filter(grepl("health|santé|médic|sanitair|salud|nutrition|nourish|nutrición|aliment|disease|illness|sick|malad|affection|enferm|afección|demogra|démogr|population|población|fertil|fécond|fecund|mortal|birth|naissan|nacimiento|death|décès|mort|muerte|sanitation|assainissement|saneamiento|tuberculos|immunodefici|immunodéf|hepatit|hépatit|non-communicable|no transmisibles|non transmissible|malaria|paludism|reproducti|sexual|sexuel|matern|immuni|vaccin|vacuna|human services|servicios humanos|services sociaux|infectio|infecci|wellness|bien-être|bienestar|child health|santé des enfant|infant|enfant|nourris|morbidi|behavioral|behavioural|comportemental|comportamiento|under-five|under-5|moins de cinq| moins de 5|menores de cinco|menores de 5|neonatal|néonatal|antenatal|prenatal|prénatal|childbearing|de procreér|contracep|concept|concepc|weight|peso|poids|transmis|calori|calorí|diarr|fever|fièvre|fiebre|respirat|menstru|hygiene|hygiène|body mass index|indice de masse corporelle|índice de masa corporal|stunt|croissance|retraso|wasted|wasting|émaciation|consunción|marasmo|obesi|mosquito-born|transmises par les moustique|transmitidas por mosquito|medic|médic|covid|virus|pandemi|pandém|epidem|épidém|food security|sécurité aliment|seguridad aliment|supplement|complement|suplement|service delivery|prestation de service|prestation des services|offre de service|prestación del servicio|provisión de servicio|servicios prestados|drug|droga|disability|disable|hándicap|invalidit|descapacidad|air quality|qualité de l'air|calidad del aire|aging|ageing|vieilliss|envejecimiento|civil regis|vital statistics|statistiques de l'état civil|registro civil|estadísticas vital|sida", 
               longdescription, ignore.case = TRUE))

# those that are case sensitive (acronyms, to make sure they aren't included bc they show up in words
envdes2 <- pressENV %>%
  filter(grepl('TB|NCD|BMI|IMC|SRH|SSR|IST|CTI|STD|STI|ETS|MST|MCH|HIV|VIH|AIDS|WASH|EHA|EAH|CRVS|ESEC', longdescription, ignore.case = FALSE))

# merge together:
envdes <- union(envdes1, envdes2)

# merge all together:
pressenvall <- union(envdes, envtitle)

## join filtered environment sector code with main dataset:
wellcome <- union(wellcome, pressenvall)

## run program dummy strings
wellcome <- wellcome %>%
  mutate(
    dhs = case_when(
      grepl('DHS', projecttitle, ignore.case = T) ~ 1,
      grepl('DHS', longdescription, ignore.case = T) ~ 1,
      grepl('demographic and health survey', longdescription, ignore.case = T) ~ 1,
      grepl('demographic and health survey', projecttitle, ignore.case = T) ~ 1,
      grepl('demographic health survey', longdescription, ignore.case = T) ~ 1,
      grepl('demographic health survey', projecttitle, ignore.case = T) ~ 1,
      TRUE ~ 0),
    pepfar = case_when(
      grepl('PEPFAR', projecttitle, ignore.case = T) ~ 1,
      grepl('PEPFAR', longdescription, ignore.case = T) ~ 1,
      grepl("President's Emergency Plan for AIDS Relief", projecttitle, ignore.case = T) ~ 1,
      grepl("President's Emergency Plan for AIDS Relief", longdescription, ignore.case = T) ~ 1,
      TRUE ~ 0),
    dhis2 = case_when(
      grepl('DHIS2', projecttitle, ignore.case = T) ~ 1,
      grepl('DHIS2', longdescription, ignore.case = T) ~ 1,
      grepl('district health information s', projecttitle, ignore.case = T) ~ 1,
      grepl('district health information s', longdescription, ignore.case = T) ~ 1,
      TRUE ~ 0),
    unaids = case_when(
      grepl('unaids', projecttitle, ignore.case = T) ~ 1,
      grepl('unaids', longdescription, ignore.case = T) ~ 1,
      grepl('unaids', donorname, ignore.case = T) ~ 1,
      grepl('joint United Nations Programme on HIV/AIDS', projecttitle, ignore.case = T) ~ 1,
      grepl('joint United Nations Programme on HIV/AIDS', longdescription, ignore.case = T) ~ 1,
      grepl('joint United Nations Programme on HIV/AIDS', donorname, ignore.case = T) ~ 1,
      TRUE ~ 0),
    who = case_when(
      grepl('World Health Organi', projecttitle, ignore.case = T) ~ 1,
      grepl('World Health Organi', longdescription, ignore.case = T) ~ 1,
      grepl('World Health Organi', donorname, ignore.case = T) ~ 1,
      grepl('WHO', projecttitle, ignore.case = F) ~ 1,
      grepl('WHO', longdescription, ignore.case = F) ~ 1,
      grepl('WHO', donorname, ignore.case = F) ~ 1,
      TRUE ~ 0),
    lsms = case_when(
      grepl('LSMS', projecttitle, ignore.case = T) ~ 1,
      grepl('LSMS', longdescription, ignore.case = T) ~ 1,
      grepl('living standards measurement s', projecttitle, ignore.case = T) ~ 1,
      grepl('living standards measurement s', longdescription, ignore.case = T) ~ 1,
      TRUE ~ 0),
    agris = case_when(
      grepl('AGRIS', projecttitle, ignore.case = F) ~ 1,
      grepl('AGRIS', longdescription, ignore.case = F) ~ 1,
      grepl('Agricultural integrated survey', projecttitle, ignore.case = T) ~ 1,
      grepl('Agricultural integrated survey', longdescription, ignore.case = T) ~ 1,
      TRUE ~ 0),
    ipums = case_when(
      grepl('IPUMS', projecttitle, ignore.case = F) ~ 1,
      grepl('IPUMS', longdescription, ignore.case = F) ~ 1,
      grepl('Integrated Public Use Microdata Series', projecttitle, ignore.case = T) ~ 1,
      grepl('Integrated Public Use Microdata Series', longdescription, ignore.case = T) ~ 1,
      TRUE ~ 0),
    iaeg = case_when(
      grepl('IAEG-SDG', projecttitle, ignore.case = T) ~ 1,
      grepl('IAEG-SDG', longdescription, ignore.case = T) ~ 1,
      grepl('Inter-agency and Expert Group on S', projecttitle, ignore.case = T) ~ 1,
      grepl('Inter-agency and Expert Group on S', longdescription, ignore.case = T) ~ 1,
      TRUE ~ 0),
    gavi = case_when(
      grepl('gavi', longdescription, ignore.case = T) ~ 1,
      grepl('gavi', projecttitle, ignore.case = T) ~ 1,
      TRUE ~ 0),
    mics = case_when(
      grepl('MICS', projecttitle, ignore.case = F) ~ 1,
      grepl('MICS', longdescription, ignore.case = F) ~ 1,
      grepl('multi indicator', projecttitle, ignore.case = T) ~ 1,
      grepl('multi indicator', longdescription, ignore.case = T) ~ 1,
      grepl('multiple indicator', projecttitle, ignore.case = T) ~ 1,
      grepl('multiple indicator', longdescription, ignore.case = T) ~ 1,
      TRUE ~ 0),
    ihme = case_when(
      grepl('IHME', projecttitle, ignore.case = F) ~ 1,
      grepl('IHME', longdescription, ignore.case = F) ~ 1,
      grepl('institute for health metrics and evaluation', longdescription, ignore.case = T) ~ 1,
      grepl('institute for health metrics and evaluation', projecttitle, ignore.case = T) ~ 1,
      TRUE ~ 0),
    hmis = case_when(
      grepl('HMIS', projecttitle, ignore.case = F) ~ 1,
      grepl('HMIS', longdescription, ignore.case = F) ~ 1,
      grepl('health management information s', projecttitle, ignore.case = T) ~ 1,
      grepl('health management information s', longdescription, ignore.case = T) ~ 1,
      TRUE ~ 0)
  )

## manually remove the MICS one that wasn't actually about the survey:
wellcome <- wellcome %>%
  mutate(
    mics = case_when(
      grepl('009-155542-A', projectnumber) ~ 0,
      TRUE ~ mics))



wellcomemanualcheck <- wellcome %>%
  filter(purposecode %in% c(14020,14030,15110,15160,15170,16010,16062,31110,31182,
                            41010,41020,41030,41040,41081,41082,43082,72010,91010))
write_xlsx(wellcomemanualcheck, 'cleanwellcomemanualcheck.xlsx')


# in separate entries bc of character limit per line
wellcome <- wellcome %>%
  filter(!grepl('65_1844|SE-0-SE-6-11395A0101-MOZ-16062|200795-109|P000995002|P000956PRE|201812503916|009-113787-A|SCR.CTR.399546|BR-T1332&ATN/OC-15698-BR|200795-110|SDN-16/0004|62410 - CIV107|62610 - CMR110|1811-18-500002|4410/A0/05/880/005|P000708001|P000145001|SE-0-SE-6-11395A0101-MOZ-16062|SE-0-SE-6-12847A0101-GGG-|SE-0-SE-6-5511001101-GEO-16062|SE-0-SE-6-11395A0101-MOZ-16062|SE-0-SE-6-11997A010|SE-0-SE-6-10841A010|SE-0-SE-6-5502028901-ALB-16062|SE-0-SE-6-13851A0101-BFA-16062|SE-0-SE-6-12802A0101-SOM-16062|SE-0-SE-6-13705A010|009-159311-A|SE-0-SE-6-14800A0101-KEN-16062|009-147756-A|009-154043-A|0810/A0/05/887/002|97212|201712501015|204605-106|VNM/20/01/RBS VNM128 Output A.1|LBN/20/01/RBS LBN104 Output A.1|3593782|3672486|P000995003|SE-0-SE-6-512000480|P00654200|109745|StatsCan-AD2021-01|62410-CIV107-Output A.1|50200-CIV107-Output A.1|2021_1649|1811-21-500007|P010570001|RG-T4193&ATN/OC-19797-RG|BMF-IMF-CCCDI|P010575001|SDN/20/01/RBS-SDN109-Output A.1.|62110-CIV107-Output A.1.|50200-CMR110-Output A.1|62610-CMR110-Output A.1|00127033|00130965|2022_544|2022_1386|2910/A0/05/301/001|3750/A0/06/700/003|1410/A0/06/006/004|2070/A0/05/005/001|420R/A0/10/006/008|4380/A0/04/107/003|4500/A0/04/805/001|8971/A0/05/002/003|575R/A0/10/903/001|8970/A0/06/881/003|6820/A0/05/004/001|2660/A0/06/885/001|15/2015|17/2015|010138/01/5|50200 - CPV103|50200 -|4150/A0/05/884/001|0610/A0/08/126/001|4980/A0/05/600/002|4380/A0/05/113/001|1260/A0/04/804/001|2220/A0/09/001/002|575R/A0/09/703/010|1530/A0/05/004/001|2340/A0/05/883/001|2400/A0/06/004/001|8970/A0/06/881/003|3300/A0/05/105/002|4710/A0/05/880/003|2220/A0/09/001/002|0750/A0/04/806/001|4380/A0/04/107/001|2390/A0/05/882/004|0610/A0/08/126/001|4020/A0/06/300/005|2690/A0/05/943/434|3380/A0/07/001/003|4980/A0/05/600/001|1260/A0/05/801/001|8970/A0/06/881/003|2450/A0/05/004/002|2850/A0/05/882/003|4410/A0/05/886/002|2040/A0/06/600/601|1410/A0/06/005/002|2040/A0/06/400/402|0530/A0/07/001/002|575R/A0/10/903/003|8970/A0/06/881/001|2820/A0/06/405/001|7050/PC/08/005/001|4380/A0/05/113/002|2040/A0/05/909/008|1430/A0/06/805/003|0520/A0/06/003/003|1770/A0/04/707/002|0750/A0/04/808/004|0090/A0/05/001/003|6490/A0/05/006/002|456C/C0/09/200/202|6820/A0/06/883/002|2660/A0/06/885/001|000000 - RER097|86_184|P160977.IDAD2210.crs1|P150148.IDAD0880.crs1|2390/A0/05/881/001|6890/A0/06/500/004|3750/A0/06/700/003|2040/A0/06/600/601|2040/A0/06/400/403|0520/A0/06/003/003|0990/A0/07/006/001|2040/A0/06/400/403|5200/A0/05/881/006|2740/A0/06/882/001|4590/A0/05/885/004|1050/A0/06/883/001|6830/A0/05/004/002|0530/A0/07/001/002|7050/PC/08/005/001|6890/A0/06/700/004|4590/A0/05/886/001|2040/A0/06/600/601|2400/A0/06/004/001|6890/A0/06/600/002|1430/A0/06/805/003|3420/A0/06/004/002|6820/A0/06/883/002|50200-RAS126-Ouput A.1|50200-GLO256-Output Unknown|50200-PER159-Ouput A.1|50200-GLO259-Output Unknown|50200-LSO104-Ouput A.1|50200-LKA101-Output Unknown|50200-RAS126-Ouput A.1|50200-GLO246-Output Unknown|50200-GLO127-Output Unknown|50200-GLO187-Output Unknown|50200-TZA101-Ouput A.1|50200-MNG155-Ouput A.1|50200-JOR108-Ouput A.1|50200-MAR129-Ouput A.1|50200-LBN104-Ouput A.1|50200-NAM128-Ouput A.1|50200-NPL129-Ouput A.1|50200-NER105-Ouput A.1|50200-GLO381-Ouput A.1|50200-CPV103-Ouput A.1|50200-EGY108-Ouput A.1|50200-GLO903-Output Unknown|50200-RAF126-Ouput A.1|50200-BOL901-Ouput A.1|50200-BGD303-Ouput A.1|50200-ZWE104-Ouput A.1|50200-BWA104-Ouput A.1|50200-RLA151-Ouput A.1|50200-PHL102-Ouput A.1|50200-GLO226-Output Unknown|50200-GLO286-Output Unknown|50200-GLO753-Output Unknown|50200-COL153-Ouput A.1|50200-GLO241-Output Unknown|50200-KIR103-Ouput A.1|50200-GLO152-Output Unknown|50200-LKA176-Ouput A.1|50200-VNM128-Ouput A.1|50200-SEN105-Ouput A.1|50200-KHM201-Ouput A.1|50200-RAB102-Ouput A.1|50200-MDA129-Ouput A.1|50200-RAS126-Ouput A.1|ROA-1819 (Y11)-(SB-009852)',
                projectnumber, ignore.case = T)) %>%
  filter(!grepl('SI2.840735|SI2.794022|SI2.837330|SI2.840568|SI2.840862|P101336.IDA54840.crs1|P151155.IDA61400.crs2|P144139.IDA55370.crs1|P101336.IDA54850.crs1|P169198.IDAD5300.crs3|P065725.IDA33490.crs6|05 - Accountability for SRH|SI2.816012|SI2.794022|SI2.839216|009-153822-|1380/A0/06/884/001|2670/A0/07/806/003|1430/A0/06/805/003|0090/A0/05/002/003|2400/A0/07/004/001|0090/A0/06/603/002|2040/A0/06/600/601|2520/A0/05/883/001|2250/A0/07/890/001|2660/A0/07/883/003|3900/A0/08/885/002|6820/A0/06/883/002|2040/A0/06/600/601|0750/A0/05/885/002|7050/PC/08/005/001|1600/A0/06/884/001|2400/A0/06/004/001|1680/A0/07/105/001|2280/A0/05/884/001|420R/A0/10/006/001|3750/A0/06/700/003|2760/A0/05/004/001|P209 -|50200-CIV107-Output A.1|50200-EGY108-Output A.1|50200-GLO286-Output A.1|50200-GLO381-Output A.1|50200-IND103-Output A.1|50200-JOR108-Output A.1|50200-JOR126-Output A.1|50200-KGZ126-Output A.1|50200-KIR103-Output A.1|50200-LBN104-Output A.1|50200-LKA176-Output A.1|50200-MDA129-Output A.1|50200-MNE129-Output A.1|50200-MRT103-Output A.1|50200-PER159-Output A.1|50200-RAF126-Output A.1|50200-SPS126-Output A.1|50200-TLS101-Output A.1|50200-TZA101-Output A.1|50200-TZA102-Output A.1|50200-UGA131-Output A.1|50200-UKR132-Output A.1|50200-UZB102-Output A.1|50200-VNM128-Output A.1|SI2.794022|AID.CTR.424939|83737|62520-CMR110-Ouput A.1|62410-SEN105-Output A.1|00121117|VNM/20/01/RBS-VNM128-Output A.1.|125899|62410-BFA101-Output A.1|62410-CIV107-Output A.1|62610-CMR110-Output A.1|63210-PRY903-Output A.1|8970/A0/06/881/001|4230/A0/06/625/001|4380/A0/05/113/002|0090/A0/05/002/003|50200 - LSO104|50200 - AZE101|50200 - NER105|50200 - TON101|50200 - GLO801|50200 - GLO902|50200 - LKA176|50200 - ZMB135|6890/A0/06/500/004|0120/A0/06/002/003|7050/PC/08/005/001|1430/A0/06/805/003|2660/A0/06/885/001|6820/A0/06/883/002|381R/A0/10/705/001|3810/A0/04/803/004|3330/A0/05/884/005|575R/A0/10/903/003|2400/A0/06/004/001|0520/A0/06/003/003|575R/A0/10/903/001|2040/A0/06/400/402|5200/A0/05/881/006|2040/A0/06/600/601|2740/A0/06/882/001|2850/A0/05/882/003|3750/A0/06/700/003|0530/A0/07/001/002|2400/A0/06/004/002|1530/A0/05/004/001|4020/A0/07/885/002|2040/A0/06/600/601|2100155030621|2019001001|010719/01/5|50200-SPS126-Output A.1|50200-SPS126-Output A.1|50200-RAB102-Output A.1|AID.CTR.424939|SCR.CTR.420652|8970/A0/06/881/001|2280/A0/05/884/001|1050/A0/06/883/001|0750/A0/05/885/002|2040/A0/06/400/403|2400/A0/06/004/001|6830/A0/05/004/002|7050/PC/08/005/001|0090/A0/05/001/003|3900/A0/08/885/002|0090/A0/05/002/003|6890/A0/06/500/004|2520/A0/05/883/001|0780/A0/06/883/002|2670/A0/07/806/003|2040/A0/06/400/403|2760/A0/05/004/001|381R/A0/10/705/001|420R/A0/10/006/001|3750/A0/06/700/003|6890/A0/06/700/004|6820/A0/06/883/002|297R/A0/10/002/006|2040/A0/06/600/601|3810/A0/05/804/003|4590/A0/05/885/004|2040/A0/06/600/601|P065725.IDA33490.crs6|2100155041366|5900155010902|P065725.IDA33490.crs6|SCR.CTR.420652|AID.CTR.435066|P065725.IDA33490.crs6|GCP/GLO/970/GER|38_5019|2022 DAT 002|38_1036|38_5547|38_3449|38_9138|50200-COL153-Output 3.3|50200-ARG126-Output 3.3|50200-GLO323-Output 3.3|50200-PHL102-Output 3.3|009-137089-A|SE-0-SE-6-11665A0101-RSS-16062|115065|115862|204605-103|00115862|P000683001|204605-103|7F-09494.01.|125269|2022_287|INN933|ROA-1819 (Y11)-(SB-009852)|50200-MDG109-Output A.1|50200-BGD303-Output A.1|AID.CTR.401056|SCR.CTR.344044|17_III_099_Global_M|18_III_081_Global_M|12_333|38_5817|CPL-015-16|28235797|10230|201918211208|2019000091|SCR.CTR.389679|009-125693-A|009-127731-A|ESVD|101533|48_1073|CRI136-Output 3.3|QZA-18/0406-12|009-137503-A|GCRF_NERC_NS_NE/S014004/1|38_6943|18_III_081_Global_M_National forest moni|61_1486|KLD-NICFI-06|16_II_135_Afrika_G_Unterstützung nationa|20194178|1908-03940|QZA-18/0406-12|KLD-NICFI-60|INS-21/0015|2021050100850|2021040102828|25026|7834|G-1906-154016|009-148251-A)',
                projectnumber, ignore.case = T)) %>%
  filter(!grepl('16II135A|18III081|CSSF OTD INT 21019|GCRF-NE_InC_HQI-2018NE/S014004/1|105_711620|12_349|GCXE22M001|2022 CLP 005|204607-104|M2022/01682|24800-GUY177-Output 3.3|61_2328|SE-0-SE-6-13386A0102-XKX-16062|SC210098|TCP_643147|120720|009-110161-A|RG-T2012&ATN/OC-13080-RG|2014.2472.0|CHN-19/0042|009-148251-A|NERC_NS_SEAMarinePlasticsNonGrantExp|38_10525|SE-0-SE-6-12848A0101-GGG-16062|GUS-WM01.085.39.2019|RG-T2526&ATN/OC-14750-RG|MNG/16/01/RBS|QZA-18/0443-|SE-0-SE-6-11988A0101-RSS-15190|GUS-WM01.084.11.2019|SCR.CTR.418300|7F-00840.11.10|BMF-IMF-AMLCFT|65020-MNG155-Output A.1|50200-MNG155-Output A.1|LBN/20/01/RBS-LBN104-Output A.1.|P001952001|P006542002|P000741001|P000100001|P006542001|P006542003',
                projectnumber, ignore.case = T)) %>%
  filter(!grepl('TC AGGREGATED ACTIVITIES,|P209 – Subvention, Contribution of 50200-|AIR-BORNE LIGHT DETECTION|ELEPHANT|sumatran tiger|shorebird|large mammal indicator|gibbon|Infrastructure et services sociaux divers-Renforcement des capacités statistiques|P209 - Subvention à Expertise France|ESTABLISHMENT OF CARIBBEAN REGIONAL TECHNICAL ASSISTANCE CENTRE|Contribution of 50200-Department of Statistics (STATISTICS)|GEF-eligible Parties to the Convention on Biological|chimpanzee|orangutan|entrée dans les écoles statistiques africaines (ESA)|Population and Develpment',
                projecttitle, ignore.case = T)) %>%
  filter(!grepl('TC AGGREGATED ACTIVITIES|AIR-BORNE LIGHT DETECTION|ELEPHANT|sumatran tiger|shorebird|large mammal indicator|gibbon|incluant la conception des épreuves|Contribution of 50200-Department of Statistics (STATISTICS)|chimpanzee|orangutan',
                longdescription, ignore.case = T))


#### Assigning the categories for analysis based on codes: ####
wellcome <- wellcome %>%
  mutate(analysiscategory = case_when(
    purposecode %in% c(12110,12191,12220,12230,12320,12330,13010,13020,13030,14020,
                       14030,15110,15160,15170,13010,16064,43082,91010,12310,12350) ~ 'All other health',
    purposecode %in% c(12250,12262,12263,12264,13040,16064) ~ 'Infectious disease',
    purposecode %in% c(12340) ~ 'Mental health',
    purposecode %in% c(12240,31110,31182,43071,43072,52010) ~ 'Nutrition',
    purposecode %in% c(12181,12182,12261,12281,13081,16062,72010) ~ 'Discovery research',
    TRUE ~ 'Climate and health'
  ))

wellcome <- wellcome %>%
  filter(usd_disbursement_defl >= 0) %>%
  relocate(analysiscategory, .after = longdescription)

write_xlsx(wellcome, 'wellcomedatafinal.xlsx')
  
wellcomebyyearandcategory <- wellcome %>%
  group_by(year, analysiscategory) %>%
  summarize(total = sum(usd_disbursement_defl, na.rm = T))

wellcomebycategory <- wellcome %>%
  group_by(analysiscategory) %>%
  summarize(total = sum(usd_disbursement_defl, na.rm = T))

##### Graphs #####

#### spending by category by year ####
wellcome %>%
  group_by(year, analysiscategory) %>%
  summarize(real_total = sum(usd_disbursement_defl, na.rm = TRUE)) %>%
  arrange(year, real_total) %>%
  mutate(graph_order = row_number()) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = real_total, fill = analysiscategory, group = graph_order)) +
  geom_col() +
  scale_y_continuous(labels = label_dollar()) +
  labs(x = 'Year',
       y = 'Disbursements (USD)',
       title = 'The amount of ODA disbursements (USD) for health data and statistics by \npriority area between 2018-2022',
       color = 'Health Category') +
  scale_fill_brewer(palette = 'BuGn') +
  theme(legend.position = 'bottom',
        legend.title = element_blank())
ggsave('Graphs/graph for spending across years.png', dpi = 400)




#### Infectious disease graph ####
infectiousdisease <- wellcome %>%
  filter(analysiscategory == 'Infectious disease') %>%
  select(-dhs:-hmis)
write_xlsx(infectiousdisease, 'infectousdisease.xlsx')
## using the list of diseases from copilot when asked for the top mentioned diseases:
infectiousdisease <- infectiousdisease %>%
  mutate(
    diarrhea = case_when(
      grepl('diarr', projecttitle, ignore.case = TRUE) ~ 1, #this way it catches us, uk spelling; spanish and french
      grepl('diarr', longdescription, ignore.case = TRUE) ~ 1, 
      TRUE ~ 0
    ),
    polio = case_when(
      grepl('polio', projecttitle, ignore.case = TRUE) ~ 1,
      grepl('polio', longdescription, ignore.case = TRUE) ~ 1,
      TRUE ~ 0
    ),
    measles = case_when(
      grepl('measles', projecttitle, ignore.case = TRUE) ~ 1,
      grepl('measles', longdescription, ignore.case = TRUE) ~ 1,
      grepl('sarampión', projecttitle, ignore.case = TRUE) ~ 1,
      grepl('sarampión', longdescription, ignore.case = TRUE) ~ 1,
      grepl('rougeole', projecttitle, ignore.case = TRUE) ~ 1,
      grepl('rougeole', longdescription, ignore.case = TRUE) ~ 1,
      TRUE ~ 0
    ),
    rubella = case_when(
      grepl('rubella', projecttitle, ignore.case = TRUE) ~ 1,
      grepl('rubella', longdescription, ignore.case = TRUE) ~ 1,
      grepl('rubéol', projecttitle, ignore.case = TRUE) ~ 1, # catches french and spanish
      grepl('rubéol', longdescription, ignore.case = TRUE) ~ 1,
      TRUE ~ 0
    ),
    tb = case_when(
      grepl('tuberculos', projecttitle, ignore.case = TRUE) ~ 1, # catches english, french, spanish
      grepl('tuberculos', longdescription, ignore.case = TRUE) ~ 1,
      grepl('TB', projecttitle, ignore.case = FALSE) ~ 1,
      grepl('TB', longdescription, ignore.case = FALSE) ~ 1,
      TRUE ~ 0
    ),
    ebola = case_when(
      grepl('ebola', projecttitle, ignore.case = TRUE) ~ 1, # catches french and english
      grepl('ebola', longdescription, ignore.case = TRUE) ~ 1, 
      grepl('ébola', projecttitle, ignore.case = TRUE) ~ 1, 
      grepl('ébola', longdescription, ignore.case = TRUE) ~ 1, 
      TRUE ~ 0
    ),
    hivaids = case_when(
      grepl('HIV', projecttitle, ignore.case = FALSE) ~ 1,
      grepl('HIV', longdescription, ignore.case = FALSE) ~ 1,
      grepl('AIDS', projecttitle, ignore.case = FALSE) ~ 1,
      grepl('AIDS', longdescription, ignore.case = FALSE) ~ 1,
      grepl('VIH', projecttitle, ignore.case = FALSE) ~ 1, # french and spanish
      grepl('VIH', longdescription, ignore.case = FALSE) ~ 1,
      grepl('sida', projecttitle, ignore.case = TRUE) ~ 1, # need to manually check these; french does not seem to capitalize sida
      grepl('sida', longdescription, ignore.case = TRUE) ~ 1,
      TRUE ~ 0
    ),
    covid = case_when(
      grepl('covid', projecttitle, ignore.case = TRUE) ~ 1,
      grepl('covid', longdescription, ignore.case = TRUE) ~ 1,
      grepl('corona', projecttitle, ignore.case = TRUE) ~ 1, # need to manually check these
      grepl('corona', longdescription, ignore.case = TRUE) ~ 1,
      TRUE ~ 0
    ),
    malaria = case_when(
      grepl('malaria', projecttitle, ignore.case = TRUE) ~ 1,
      grepl('malaria', longdescription, ignore.case = TRUE) ~ 1,
      grepl('paludism', projecttitle, ignore.case = TRUE) ~ 1, # covers French and Spanish
      grepl('paludism', longdescription, ignore.case = TRUE) ~ 1,
      TRUE ~ 0
    ),
    tropicaldisease = case_when(
      grepl('tropical disease', projecttitle, ignore.case = TRUE) ~ 1,
      grepl('tropical disease', longdescription, ignore.case = TRUE) ~ 1,
      grepl('enfermedad tropical', projecttitle, ignore.case = TRUE) ~ 1,
      grepl('enfermedad tropical', longdescription, ignore.case = TRUE) ~ 1,
      grepl('enfermedades tropical', projecttitle, ignore.case = TRUE) ~ 1,
      grepl('enfermedades tropical', longdescription, ignore.case = TRUE) ~ 1,
      grepl('maladie tropical', projecttitle, ignore.case = TRUE) ~ 1,
      grepl('maladie tropical', longdescription, ignore.case = TRUE) ~ 1,
      grepl('maladies tropical', projecttitle, ignore.case = TRUE) ~ 1,
      grepl('maladies tropical', longdescription, ignore.case = TRUE) ~ 1,
      TRUE ~ 0
    ),
    pneumo = case_when(
      grepl('neumo', projecttitle, ignore.case = TRUE) ~ 1, # catches french, english, and spanish for pneumococcal
      grepl('neumo', longdescription, ignore.case = TRUE) ~ 1,
      TRUE ~ 0
    ))
### need to get the totals for each for graphing:

nonspecific <- infectiousdisease %>%
  filter(diarrhea == 0 & polio == 0 & measles == 0 & rubella == 0 & tb == 0 & ebola == 0 & hivaids == 0 & covid == 0 & malaria == 0 & tropicaldisease == 0 & pneumo == 0)

nonspecific <- nonspecific %>%
  group_by(year) %>%
  summarize(total = sum(usd_disbursement_defl, na.rm = TRUE)) %>%
  mutate(disease = 'Non-specific')
diarrhea <- infectiousdisease %>%
  filter(diarrhea == 1) %>%
  group_by(year) %>%
  summarize(total = sum(usd_disbursement_defl, na.rm = TRUE)) %>%
  mutate(disease = 'Diarrheal diseases')
polio <- infectiousdisease %>%
  filter(polio == 1) %>%
  group_by(year) %>%
  summarize(total = sum(usd_disbursement_defl, na.rm = TRUE)) %>%
  mutate(disease = 'Polio')
measles <- infectiousdisease %>%
  filter(measles == 1) %>%
  group_by(year) %>%
  summarize(total = sum(usd_disbursement_defl, na.rm = TRUE)) %>%
  mutate(disease = 'Measles')
rubella <- infectiousdisease %>%
  filter(rubella == 1) %>%
  group_by(year) %>%
  summarize(total = sum(usd_disbursement_defl, na.rm = TRUE)) %>%
  mutate(disease = 'Rubella')
tb <- infectiousdisease %>%
  filter(tb == 1) %>%
  group_by(year) %>%
  summarize(total = sum(usd_disbursement_defl, na.rm = TRUE)) %>%
  mutate(disease = 'Tuberculosis')
ebola <- infectiousdisease %>%
  filter(ebola == 1) %>%
  group_by(year) %>%
  summarize(total = sum(usd_disbursement_defl, na.rm = TRUE)) %>%
  mutate(disease = 'Ebola')
hivaids <- infectiousdisease %>%
  filter(hivaids == 1) %>%
  group_by(year) %>%
  summarize(total = sum(usd_disbursement_defl, na.rm = TRUE)) %>%
  mutate(disease = 'HIV/AIDS')
covid <- infectiousdisease %>%
  filter(covid == 1) %>%
  group_by(year) %>%
  summarize(total = sum(usd_disbursement_defl, na.rm = TRUE)) %>%
  mutate(disease = 'COVID-19')
malaria <- infectiousdisease %>%
  filter(malaria == 1) %>%
  group_by(year) %>%
  summarize(total = sum(usd_disbursement_defl, na.rm = TRUE)) %>%
  mutate(disease = 'Malaria')
tropicaldisease <- infectiousdisease %>%
  filter(tropicaldisease == 1) %>%
  group_by(year) %>%
  summarize(total = sum(usd_disbursement_defl, na.rm = TRUE)) %>%
  mutate(disease = 'Tropical Diseases')
pneumo <- infectiousdisease %>%
  filter(pneumo == 1) %>%
  group_by(year) %>%
  summarize(total = sum(usd_disbursement_defl, na.rm = TRUE)) %>%
  mutate(disease = 'Pneumococcal Disease')


yearlydisease4graph <- bind_rows(diarrhea, polio, measles, rubella, tb, covid, ebola, hivaids, malaria,
                                 tropicaldisease,pneumo,nonspecific) %>%
  mutate(
    # graphdisease = case_when(
    #   disease %in% c('Ebola', 'Rubella','Pneumococcal Disease', 'Tropical Diseases','Measles','Non-specific','Polio') ~ 'All other diseases',
    #   TRUE ~ disease),
    graphdisease2 = case_when(
      disease %in% c('Diarrheal diseases', 'Ebola', 'Rubella','Pneumococcal Disease', 'Tropical Diseases','Measles','Non-specific','Polio') ~ 'All other diseases',
      TRUE ~ disease)) %>%
  ungroup()

yearlydisease4graph %>%
  group_by(year, graphdisease2) %>%
  summarize(real_total = sum(total, na.rm = TRUE)) %>%
  arrange(year, real_total) %>%
  mutate(graph_order = row_number()) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = real_total, fill = graphdisease2, group = graph_order)) +
  geom_col() +
  scale_y_continuous(labels = label_dollar()) +
  scale_fill_brewer(palette = 'Purples') +
  labs(x = 'Year',
       y = 'Disbursements (USD)',
       title = 'The amount of ODA disbursements (USD) for infectious disease data and statistics by \ndisease between 2018-2022') +
  theme(legend.position = 'bottom',
        legend.title = element_blank())
ggsave('Graphs/1disbursementsbydisease.png', dpi = 400)


