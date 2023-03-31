# Input Quality Preprocessing 
library(tidyverse)
library(ggplot2)
library(plotrix)
library(papaja)
library(stringr)
library(morphemepiece)
library(tidytext)
library(wordbankr)
library(reshape2)
library(profileR)
library(lubridate)
library(morphemepiece)
library(udpipe)

lancaster_norms <- read.csv("data/Lancaster_sensorimotor_norms_for_39707_words.csv") %>%
  mutate(Word = tolower(Word))
CBOI_norms <- read.csv("data/CBOI_mean_sd.csv") %>%
  mutate(Word = tolower(Word))

### find + list all LENA transcripts on pn-opus
TD_matches <- c("TD_443_341", "TD_444_402", "TD_445_217", "TD_447_448", "TD_449_716",
                "TD_463_254", "TD_464_188", "TD_465_541", "TD_466_433", "TD_467_433",
                "TD_472_829", "TD_473_844", "TD_474_966", "TD_475_481", "TD_477_217")


LENA_counts <- read.csv("data/LENA/Automated/VIHI_its_details.csv") %>% 
  filter(match_type == "VI"|
           match_type == "VI_age")
write.csv(LENA_counts,"data/LENA/Automated/LENA_counts.csv")  


## Load in transcripts and automated metrics

###read in data that has been mass exported via ELAN; add informative column names

VITD_transcripts <- read.csv("data/LENA/Transcripts/VI_LENA_and_TD_matches_03-28-20232023-03-28.csv") %>% #need to re-generate transcripts
  mutate(VIHI_ID = as.factor(str_sub(VIHI_ID,1,10))) 

VITD_LENA_utterances_split <- VITD_transcripts %>%
  mutate(utterance_clean = str_remove_all(utterance, pattern = ("\\[.*?<>"))) %>% # remove punctuation from utterances
  separate(utterance_clean,
           into = paste0("Word", 1:70),
           sep = " ") #separate utterances into columns with 1 column per word

VITD_LENA_words <- VITD_LENA_utterances_split %>%
  mutate(uttnum = seq(1, nrow(VITD_LENA_utterances_split), 1)) %>% #give each utterance a unique number
  pivot_longer(cols = Word1:Word46,
               #pivot the word columns into a single Word column
               names_to = "utt_loc",
               #create another column that gives the location within utterance (ex:Word2)
               values_to = "Word") %>%
  select(VIHI_ID, group, speaker, xds, uttnum, utt_loc, Word) %>%
  filter(!is.na(Word)) %>%#filter out blank rows
  left_join(lancaster_norms) %>% # join with lancaster norms
  select(Word,
         VIHI_ID,
         group,
         Auditory.mean:Dominant.sensorimotor,
         utt_loc,
         uttnum,
         xds,
         speaker) %>% #remove unwanted columns
  filter(group!="HI" & speaker!="CHI")

utterances_only<-VITD_transcripts %>% 
  filter(speaker!="EE1", speaker!="CHI") %>% # remove CHI utts and electronic noise
  filter(!utterance==c("xxx.")) %>% #remove unintelligible utterances
  mutate(utterance = str_remove_all(utterance, pattern = "[[:punct:]]"),# remove punctuation from utterances
         utt_num = 1:nrow(.)) 


# quantity ----





# count word types in annotations
manual_word_types <- VITD_LENA_words %>% 
  group_by(VIHI_ID) %>%
  distinct(Word, .keep_all=TRUE) %>%
  summarise(types = n())
# count word tokens in annotations
manual_word_tokens <- VITD_LENA_words %>% 
  group_by(VIHI_ID) %>%
  summarise(tokens = n())

# interactiveness quality ----
## CTC----
## child-directed----
xds_props_wide <- utterances_only %>%
  group_by(group,VIHI_ID) %>% 
  summarise(total = n(),
            prop_ADS = sum(xds=="A")/total,
            prop_CDS = sum(xds=="C")/total,
            prop_BDS = sum(xds=="B")/total,
            prop_TDS = sum(xds=="T")/total,
            prop_ODS = sum(xds=="O")/total,
            prop_UDS = sum(xds=="U")/total,
            prop_PDS = sum(xds=="P")/total) 
write.csv(xds_props_wide, "data/LENA/Transcripts/Derived/xds_props_wide.csv")
xds_props <- xds_props_wide%>%
  pivot_longer(cols = prop_ADS:prop_PDS,
               names_to = "addressee",
               names_prefix = "prop_",
               values_to = "prop")
write.csv(xds_props, "data/LENA/Transcripts/Derived/xds_props.csv")
# linguistic quality ----
## TTR ----
### calculate type-token ratio in annotations
manual_word_TTR <- manual_word_types %>% left_join(manual_word_tokens) %>%
  mutate(TTR = types/tokens,
         group = as.factor(str_sub(VIHI_ID, 1,2)))  %>%
  filter(group!="HI")
write.csv(manual_word_TTR, "data/LENA/Transcripts/Derived/manual_word_TTR.csv")

## MLU ----
### get the morpheme counts for VI ----

tokenized_VITD_transcripts <- morphemepiece_tokenize(utterances_only$utterance, vocab = morphemepiece_vocab(),
                                                     lookup = morphemepiece_lookup(),
                                                     unk_token = "[UNK]",
                                                     max_chars = 100)%>% #split utterances into morphemes (based on entries in the morphemepiece "dictionary"). each listing has its own number
  plyr::ldply(rbind)%>% # split each tokenized_VI, then bind each as rows in a dataframe
  mutate_all(funs(ifelse(is.na(.), 0, 1)))

tokenized_VITD_transcripts_with_counts <- tokenized_VITD_transcripts %>%
  mutate(morphemecount = rowSums(tokenized_VITD_transcripts)) %>%
  mutate(utt_num = 1:nrow(tokenized_VITD_transcripts))

simple_morpheme_counts <- tokenized_VITD_transcripts_with_counts %>% 
  select(-group, -code) %>%
  left_join(utterances_only) %>%
  select(VIHI_ID, utt_num, speaker, xds, utterance, morphemecount) %>%
  mutate(group=as.factor(str_sub(VIHI_ID,1,2)))

MLUs <- simple_morpheme_counts %>% 
  group_by(group, VIHI_ID) %>%
  summarise(MLU= mean(morphemecount))
write.csv(MLUs,"data/LENA/Transcripts/Derived/MLUs.csv")

# conceptual quality ----
## sensory word props ----
sensory_props_wide <- VITD_LENA_words %>% 
  anti_join(stop_words, by= c("Word" = "word")) %>%
  filter(!is.na(Auditory.mean))  %>% # filter out words that don't have a perceptual rating
  mutate(Modality = case_when(Exclusivity.perceptual <= .5 ~ "Multimodal",
                              Max_strength.perceptual<3.5 ~ "Amodal",
                              Max_strength.perceptual>4  &  Exclusivity.perceptual <= .3 ~ Dominant.perceptual,
                              TRUE~Dominant.perceptual)) %>%
  group_by(group,VIHI_ID) %>% 
  summarise(total = n(),
    prop_Auditory = sum(Modality=="Auditory")/total,
    prop_Visual = sum(Modality=="Visual")/total,
    prop_Olfactory = sum(Modality=="Olfactory")/total,
    prop_Gustatory = sum(Modality=="Gustatory")/total,
    prop_Haptic = sum(Modality=="Haptic")/total,
    prop_Interoceptive = sum(Modality=="Interoceptive")/total,
    prop_Multimodal = sum(Modality=="Multimodal")/total,
    prop_Amodal = sum(Modality=="Amodal")/total) 
write.csv(sensory_props_wide,"data/LENA/Transcripts/Derived/sensory_props_wide.csv")
sensory_props <- sensory_props_wide%>%
  pivot_longer(cols = prop_Auditory:prop_Amodal,
               names_to = "Modality",
               names_prefix = "prop_",
               values_to = "prop") 

write.csv(sensory_props,"data/LENA/Transcripts/Derived/sensory_props.csv")

## tense/displacement----
udpipe_english <- udpipe_download_model(language = "english")
udmodel_english <- udpipe_load_model(file = udpipe_english$file_model)
annotated_utterances <- udpipe_annotate(udmodel_english, 
                                       x = utterances_only$utterance, 
                                       doc_id = utterances_only$VIHI_ID) %>%
  as.data.frame()

verbs_only <- annotated_utterances %>%
  filter(xpos %in% c("VB","VBD","VBP","VBN","VBG","VBZ","AUX","MD"),
         token != "=!" &
           token != "xxx") %>%
  distinct(doc_id,paragraph_id,sentence_id, .keep_all = TRUE) %>%
  mutate(temporality = case_when(grepl('Tense=Past',feats) ~ "not_present",
                                 grepl('Mood=Imp',feats)~ "not_present",
                                 xpos=="MD" ~ "not_present",
                                 grepl('gonna',sentence) | grepl('gotta',sentence) | grepl('wanna',sentence)|grepl('going to',sentence)| grepl('got to',sentence) |grepl('want to',sentence) |grepl('have to',sentence) ~ "not_present",
                                 grepl('Mood=Ind',feats) & grepl('Tense=Pres',feats) | grepl('VerbForm=Ger',feats) ~ "present",
                                 TRUE ~ "uncategorized"))
temporality_props_wide <- verbs_only %>% 
  dplyr::rename(VIHI_ID = doc_id) %>%
  group_by(VIHI_ID) %>%
  summarize(verb_utt_count = n(),
            prop_past = (sum(temporality=="not_present")/verb_utt_count),
            prop_present =  (sum(temporality=="present")/verb_utt_count),
            prop_uncategorized = (sum(temporality=="uncategorized")/verb_utt_count)) %>%
  mutate(group=as.factor(str_sub(VIHI_ID,1,2)))
write.csv(temporality_props_wide, "data/LENA/Transcripts/Derived/temporality_props_wide.csv")
temporality_props <- temporality_props_wide %>%
  pivot_longer(cols = prop_past:prop_uncategorized,
               names_to = "verb_temporality",
               names_prefix = "prop_",
               values_to = "prop")
write.csv(temporality_props, "data/LENA/Transcripts/Derived/temporality_props.csv")
## content word CBOI ----
content_words_only <- annotated_utterances %>% 
  filter(upos %in% c("ADJ","ADV","NOUN","VERB")) %>%
  left_join(CBOI_norms, by=c("lemma"="Word")) %>%
  filter(CBOI_Mean != "NA") %>% 
  dplyr::rename(VIHI_ID = doc_id) %>%
  select(VIHI_ID, sentence,token,lemma,upos,CBOI_Mean) %>%
  mutate(group=as.factor(str_sub(VIHI_ID,1,2)))
write.csv(content_words_only, "data/LENA/Transcripts/Derived/content_words_only.csv")
subj_CBOI_means <- content_words_only %>% 
  group_by(group,VIHI_ID) %>% 
  summarise(CBOI_Mean=mean(CBOI_Mean)) 
write.csv(subj_CBOI_means, "data/LENA/Transcripts/Derived/subj_CBOI_means.csv")
# CDI wrangling ----

#inverse logit function to calculate delay
inv_logit <- function(x){
  return(1 / (exp(-x) + 1))
} 
#get score for a given age using logit function
getScoreForAge = function(lm, age, lang, num_items){ #function that takes lm, child's age, and the number of possible words (from WG/WS)
  # just predict
  prop = inv_logit(predict.glm(lm, newdata = data.frame(age=age)))
  return(prop*num_items)
}
#get age for a given score using logit function
getAgeForScore = function(lm, score, num_items){
  proportion = (score + .000001) / num_items #added point .000001 to avoid getting inf delay when score is 0
  # http://www.talkstats.com/threads/inverse-prediction-from-binary-logistic-regression.52121/
  b0 = lm$coefficients[1]
  b1 = lm$coefficients[2]
  predicted_age = (log(proportion / (1-proportion)) - b0)/ b1
  return(predicted_age)
}
# English constants for growth curve
constants_eng = list() #create a list called constants
constants_eng[['WG']] = list() #add a WG section to the list
constants_eng[['WG']]$lowest_num_id = 33 #sets the lowest_num_id to 33 (the first question on WG asking 'does your child know X?')
constants_eng[['WG']]$highest_num_id = 430 #sets the highest_num_id to 430 (the last question on WG asking 'does your child know X?')
constants_eng[['WG']]$num_items = constants_eng[['WG']]$highest_num_id - constants_eng[['WG']]$lowest_num_id + 1 #substracts lowest_  and highest_num_ids and adds 1, to get highest possible score on WG
constants_eng[['WS']] = list() #add a WS section to the list
constants_eng[['WS']]$lowest_num_id = 1 #sets the lowest_num_id to 1 (the first question on WS asking 'does your child know X?')
constants_eng[['WS']]$highest_num_id = 680 #sets the highest_num_id to 680 (the last question on WS asking 'does your child know X?')
constants_eng[['WS']]$num_items = constants_eng[['WS']]$highest_num_id - constants_eng[['WS']]$lowest_num_id + 1 #substracts lowest_  and highest_num_ids and adds 1, to get highest possible score on WS
#English growth curves
calculate_delay = function(cdi_form, constants, verbose=F){
  print(paste('Processing ',cdi_form,'...', sep=''))	#Prints "Processing WG..." or WS message
  
  num_items = constants_eng[[cdi_form]][['num_items']]	
  print('Number of items:')
  print(num_items) #Prints number of items possible given CDI version
  
  eng_data <- get_instrument_data(language = "English (American)", 
                                  form = cdi_form, administration_info = TRUE) %>%
    mutate(num_item_id = as.numeric(str_sub(item_id, 6,)))
  eng_words = subset(eng_data, num_item_id < constants_eng[[cdi_form]][['highest_num_id']] & num_item_id > constants_eng[[cdi_form]][['lowest_num_id']]) #takes the subset of columns related to 'does your child know X word?'
  
  print('Computing counts...') #Prints "Computing counts" message
  counts <- eng_words %>% #creates counts df
    dplyr::filter(!is.na(.data$age)) %>% #filters entries without an age 
    dplyr::mutate(produces = !is.na(.data$value) & .data$value == "produces",
                  understands = !is.na(.data$value) &
                    (.data$value == "understands" | .data$value == "produces")) %>%
    dplyr::select(-.data$value) %>%
    tidyr::gather("measure_name", "value", .data$produces, .data$understands) %>%
    dplyr::filter(.data$measure_name == "produces") %>%
    dplyr::group_by(.data$age, .data$data_id) %>%
    dplyr::summarise(num_true = sum(.data$value),
                     num_false = n() - .data$num_true)
  print(counts)
  
  print('Fitting model...') #prints "Fitting model" message
  model <- stats::glm(cbind(num_true, num_false) ~ age, counts,
                      family = "binomial")
  
  if (verbose){ #if verbose argument is TRUE, prints a summary of the model
    print(summary(model))
  }
  
  
  new_data = data.frame(age = (seq(2*30.5,60*30.5,by=1) / 30.5), cdi_form)
  
  print('Getting scores...')
  
  new_scores = cbind(new_data, data.frame(predict(model, new_data, type='response', se.fit=T)))
  print(names(new_scores))
  new_scores$scores = new_scores$fit * num_items
  new_scores$se_high = new_scores$fit + new_scores$se.fit
  new_scores$se_low = new_scores$fit - new_scores$se.fit
  new_scores$se_high = new_scores$se_high * num_items
  new_scores$se_low = new_scores$se_low * num_items
  new_scores$predict_ages = new_scores$age
  
  print('Getting Wordbank norms...')
  print(num_items)
  wordbank_norms = read.csv(paste("./data/CDI/Wordbank/vocabulary_norms_table_",cdi_form,"_Prod",".csv", sep=""),
                            stringsAsFactors=F)	
  
  wordbank_norms_melted = melt(wordbank_norms, id.vars = c("language", "form", "measure", "age", "is_norming", "n", "downloaded"), measure.vars = "vocab")
  
  estimate_for_form <- subset(VIHI_CDI, Version == cdi_form)
  estimate_for_form$ProductionCDI_no = num_items - estimate_for_form$WordsProduced 
  estimate_for_form$expected_score_at_chron_age = sapply(estimate_for_form$age_months,
                                                         function(age){getScoreForAge(model, age, num_items=num_items)})
  estimate_for_form$expected_age_for_score = sapply(estimate_for_form$WordsProduced,
                                                    function(score){getAgeForScore(model, score, num_items)})
  
  print('Computing differences...')	
  estimate_for_form$diff_score_from_expected = -1 * (estimate_for_form$WordsProduced - estimate_for_form$expected_score_at_chron_age)
  # more negative, more baf
  estimate_for_form$diff_age_from_expected = estimate_for_form$age_months - estimate_for_form$expected_age_for_score
  estimate_for_form$Wordbank_n = wordbank_norms_melted %>% distinct(n) %>% as.numeric()
  estimate_for_form$Wordbank_norms_date = wordbank_norms_melted %>% distinct(downloaded) %>% as.character()
  
  if (verbose){
    print(estimate_for_form[,c('VIHI_ID','age_months', 
                               'WordsProduced', 'expected_score_at_chron_age', 
                               'diff_age_from_expected','diff_score_from_expected')])
  }
  
  print(head(estimate_for_form))
  rlist = list()		
  rlist[['normative_growth_curve_model']] = model
  rlist[['samples_from_growth_curve_model']] = new_scores
  rlist[['estimate_df']] = estimate_for_form
  rlist[['wordbank_norms_melted']] = wordbank_norms_melted
  rlist[['cdi_form']] = cdi_form
  assign(paste(cdi_form, "estimate_eng", sep = "_"), estimate_for_form, envir = .GlobalEnv)
  assign(paste(cdi_form, "estimate_eng_curves", sep = "_"), wordbank_norms_melted, envir = .GlobalEnv)
  assign(paste(cdi_form, "estimate_eng_gcurve", sep = "_"), new_scores, envir = .GlobalEnv)
}

# calculate_delay("WG", constants_eng, verbose=T)
# calculate_delay("WS", constants_eng, verbose=T)
# join the dataframes created with delay function, and adjust at the lower tails based on Wordbank %. (otherwise overestimates delay because of long tail)
# VIHI_CDI <- rbind(WG_estimate_eng, WS_estimate_eng) %>%
#   mutate(diff_age_from_expected = case_when(WordsProduced==0 ~ (age_months - 8),
#                                             WordsProduced==1 ~ (age_months - 9),
#                                             WordsProduced==2 ~ (age_months - 10),
#                                             WordsProduced==3 ~ (age_months - 11),
#                                             WordsProduced==4 ~ (age_months - 11.5),
#                                             WordsProduced==5 ~ (age_months - 12),
#                                             WordsProduced==6 ~ (age_months - 12.5),
#                                             WordsProduced==7 ~ (age_months - 12.5),
#                                             WordsProduced==12 ~ (age_months - 13.5),
#                                             WordsProduced==12 ~ (age_months - 14.5),
#                                             TRUE ~ diff_age_from_expected)) %>%
#   select(-c(Other_ID, Notes)) %>%
#   mutate(CDI_age_in_days=age,
#          CDI_age_months = age_months)
# write.csv(VIHI_CDI, "./data/CDI/Derived/VITD_CDI.csv")

