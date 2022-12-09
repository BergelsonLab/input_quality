# Input Quality Preprocessing 

## Load in transcripts

###read in data that has been mass exported via ELAN; add informative column names

single_column_with_names <- read_delim("/Volumes/pn-opus/VIHI/WorkingFiles/fake_VI_Oct19.txt", 
                                       delim = "\t", escape_double = FALSE, 
                                       trim_ws = TRUE,col_names = FALSE) %>%
  rename(tier=X1,
         participant=X2,
         onset_ms=X3,
         offset_ms=X4,
         duration=X5,
         utterance=X6,
         filename=X7,
         filepath=X8)

### wrangle data so that each utterance has a single row, all participant labels appear in a single column, all utterance transcriptions in a single column, xds annotations appear in a single column, all PI annotations appear in a single column)
wide_transcripts <- pivot_wider(single_column_with_names, names_from = "tier", values_from = "utterance") %>%
  unite("xds", contains("xds@"), remove = TRUE, na.rm = TRUE) %>%
  unite("PI", contains("PI"), remove = TRUE, na.rm = TRUE) %>%
  unite("utterance", c("CHI","UC1","FA1", "FA2", "FA3","FA4","FA5", "FA6", "FA7", "FA8", "FA9", "F10", "FAE", "MA1", "MA2","MA3", "MC1", "FC1", "FC2", "EE1"), remove = TRUE, na.rm = TRUE) %>%
  mutate(VIHI_ID = str_sub(filename, 1,10))

### find + list all LENA transcripts on pn-opus
LENA_files_list <- list.files("/Volumes/pn-opus/VIHI/SubjectFiles/LENA", pattern="*_lena.txt", full.names=TRUE, recursive = TRUE)

all_lena <- read_delim("data/all_lena.txt", 
                       delim = "\t", escape_double = FALSE, 
                       col_names = FALSE, trim_ws = TRUE) %>%
  dplyr::rename(
    Speaker = X1,
    OnsetTime = X2,
    OffsetTime = X3,
    Duration = X4,
    Utterance = X5,
    filename = X6,
    filepath = X7,
    
  )   %>%
  mutate(VIHI_ID = str_sub(filename, 1,10),
         group = as.factor(str_sub(filename, 1,2)))%>%
  filter(Speaker != 'CHI' & Speaker != 'code' & Speaker != 'sampling_type')

VIHI_LENA_utterances <- all_lena %>%
  mutate(utterance_clean = str_remove_all(Utterance, pattern = ("\\[.*?<>"))) %>% # remove punctuation from utterances
  separate(utterance_clean,
           into = paste0("Word", 1:46),
           sep = " ") #separate utterances into columns with 1 column per word

VIHI_LENA_words <- VIHI_LENA_utterances %>%
  mutate(uttnum = seq(1, nrow(VIHI_LENA_utterances), 1)) %>% #give each utterance a unique number
  pivot_longer(cols = Word1:Word46,
               #pivot the word columns into a single Word column
               names_to = "utt_loc",
               #create another column that gives the location within utterance (ex:Word2)
               values_to = "Word") %>%
  select(VIHI_ID, group, uttnum, utt_loc, Word) %>%
  filter(!is.na(Word)) %>% #filter out blank rows
  left_join(lancaster_norms) %>% # join with lancaster norms
  filter(!is.na(Auditory.mean)) %>% # filter out words that don't have a perceptual rating
  select(Word,
         VIHI_ID,
         group,
         Auditory.mean:Dominant.sensorimotor,
         utt_loc,
         uttnum) #remove unwanted columns

# count word types in annotations
manual_word_types <- VIHI_LENA_words %>% 
  group_by(VIHI_ID) %>%
  distinct(Word, .keep_all=TRUE) %>%
  summarise(types = n())
# count word tokens in annotations
manual_word_tokens <- VIHI_LENA_words %>% 
  group_by(VIHI_ID) %>%
  summarise(tokens = n())
# calculate type-token ratio in annotations
manual_word_TTR <- manual_word_types %>% left_join(manual_word_tokens) %>%
  mutate(TTR = types/tokens,
         group = as.factor(str_sub(VIHI_ID, 1,2)))  %>%
  filter(group!="HI")


#get the morpheme counts for VI
utterances_only_VI<-wide_transcripts%>%
  filter(participant!="EE1", participant!="CHI")

tokenized_VI <- morphemepiece_tokenize(utterances_only_VI$utterance, vocab = morphemepiece_vocab(),
                                       lookup = morphemepiece_lookup(),
                                       unk_token = "[UNK]",
                                       max_chars = 100)

df_tokenized_VI <- tokenized_VI %>%
  plyr::ldply(rbind)

df_morphemes_VI<- data.frame(ifelse(is.na(df_tokenized_VI),0,1))
df_morphemes_VI$morphemecount<-rowSums(df_morphemes_VI)
utterances_only_VI$morphemecount<-df_morphemes_VI$morphemecount

for_MLU_VI_morphemes<-utterances_only_VI%>%
  filter(!utterance==c("xxx."))%>%
  group_by(VIHI_ID)%>%
  summarise(
    totalmorphemes=sum(morphemecount))

forMLU_VI_utterances <-utterances_only_VI%>%
  filter(!utterance==c("xxx."))%>%
  count(VIHI_ID)

final_forMLU_VI <- merge(forMLU_VI_utterances, for_MLU_VI_morphemes, by="VIHI_ID", all = TRUE)
final_forMLU_VI$MLU_rough <-(final_forMLU_VI$totalmorphemes/final_forMLU_VI$n)

average_VI_input_MLU <-mean(final_forMLU_VI$MLU_rough)


MLU_variance_VI <-(var(final_forMLU_VI$MLU_rough))


MLU_SD_VI <-sqrt(var(final_forMLU_VI$MLU_rough))
average_VI_input_MLU
MLU_variance_VI 
MLU_SD_VI







# CDI wrangling

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

calculate_delay("WG", constants_eng, verbose=T)
calculate_delay("WS", constants_eng, verbose=T)
# join the dataframes created with delay function, and adjust at the lower tails based on Wordbank %. (otherwise overestimates delay because of long tail)
VIHI_CDI <- rbind(WG_estimate_eng, WS_estimate_eng) %>%
  mutate(diff_age_from_expected = case_when(WordsProduced==0 ~ (age_months - 8),
                                            WordsProduced==1 ~ (age_months - 9),
                                            WordsProduced==2 ~ (age_months - 10),
                                            WordsProduced==3 ~ (age_months - 11),
                                            WordsProduced==4 ~ (age_months - 11.5),
                                            WordsProduced==5 ~ (age_months - 12),
                                            WordsProduced==6 ~ (age_months - 12.5),
                                            WordsProduced==7 ~ (age_months - 12.5),
                                            WordsProduced==12 ~ (age_months - 13.5),
                                            WordsProduced==12 ~ (age_months - 14.5),
                                            TRUE ~ diff_age_from_expected)) %>%
  select(-c(Other_ID, Notes)) %>%
  mutate(CDI_age_in_days=age,
         CDI_age_months = age_months)
write.csv(VIHI_CDI, "./data/CDI/Derived/VIHI_CDI.csv")

