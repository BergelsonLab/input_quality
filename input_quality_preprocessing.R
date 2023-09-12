# Input Quality Preprocessing

## demographics
VI_matches_demo <- read.csv("data/Demographics/VI_matches_demo.csv")

lancaster_norms <-
  read.csv("data/Norms/Lancaster_sensorimotor_norms_for_39707_words.csv") %>%
  mutate(Word = tolower(Word))
CBOI_norms <- read.csv("data/Norms/CBOI_mean_sd.csv") %>%
  mutate(Word = tolower(Word))

### find + list all LENA transcripts on pn-opus
TD_matches <-
  c(
    "TD_436_678",
    "TD_443_341",
    "TD_444_402",
    "TD_445_217",
    "TD_447_448",
    "TD_448_304",
    "TD_449_716",
    "TD_463_254",
    "TD_464_188",
    "TD_465_541",
    # "TD_467_433",
    "TD_472_829",
    "TD_473_844",
    "TD_474_966",
    "TD_475_481",
    "TD_477_217"
  )


LENA_counts <-
  read.csv("data/LENA/Automated/VIHI_its_details.csv") %>%
  filter(match_type == "VI" |
           VIHI_ID %in% TD_matches) %>%
  mutate(
    AWC_stand = AWC / (total_time_dur / (30 * 60)),
    CTC_stand = CTC / (total_time_dur / (30 * 60))
  ) %>%
  left_join((VI_matches_demo %>% dplyr::select(VIHI_ID, pair)), by = "VIHI_ID") %>%
  distinct(its_path, .keep_all = TRUE) 
write.csv(LENA_counts, "data/LENA/Automated/LENA_counts.csv")

## Load in transcripts and automated metrics

###read in data that has been mass exported via ELAN; add informative column names

VITD_transcripts <-
  read.csv("data/LENA/Transcripts/Raw/VI_LENA_and_TD_matches_2023-09-11.csv") %>% 
  mutate(VIHI_ID = as.factor(str_sub(VIHI_ID, 1, 10))) %>%
  filter(VIHI_ID %in% TD_matches | group == "VI") %>%
  mutate(
    utterance_clean = str_replace_all(utterance, "<\\w+>", ""),
    utterance_clean = str_replace_all(utterance_clean, "=!\\w+\\s*", ""),
    utterance_clean = str_replace_all(utterance_clean, "[[:punct:]&&[^']]", "")
  ) %>%
  filter(speaker != "EE1", speaker != "CHI") %>% # remove CHI utts and electronic noise
  filter(!utterance == c("xxx.")) %>% #remove unintelligible utterances
  filter(!grepl("utt@|inq@", speaker)) %>% # remove non-utterance info
  mutate(utt_num = 1:nrow(.))

VITD_LENA_utterances_split <- VITD_transcripts %>%
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
  dplyr::select(VIHI_ID,
                group,
                speaker,
                sampling_type,
                xds,
                uttnum,
                utt_loc,
                Word) %>%
  filter(!is.na(Word)) %>% #filter out blank rows
  left_join(lancaster_norms) %>% # join with lancaster norms
  dplyr::select(
    Word,
    VIHI_ID,
    group,
    Auditory.mean:Dominant.sensorimotor,
    sampling_type,
    utt_loc,
    uttnum,
    xds,
    speaker
  ) %>% #remove unwanted columns
  filter(group != "HI" & speaker != "CHI")


# quantity ----

# count word tokens in annotations
manual_word_tokens <- VITD_LENA_words %>%
  filter(sampling_type == "random") %>%
  group_by(VIHI_ID) %>%
  summarise(tokens = n()) %>%
  left_join((VI_matches_demo %>% dplyr::select(VIHI_ID, pair)), by = "VIHI_ID") %>%
  mutate(group = as.factor(str_sub(VIHI_ID, 1, 2))) %>%
  distinct(VIHI_ID, .keep_all = TRUE) 

write.csv(manual_word_tokens,
          "data/LENA/Transcripts/Derived/manual_word_tokens.csv")

# interactiveness quality ----
## CTC----
## child-directed----
xds_props_wide <- VITD_transcripts %>%
  filter(sampling_type == "random") %>%
  group_by(group, VIHI_ID) %>%
  summarise(
    total = n(),
    prop_ADS = sum(xds == "A") / total,
    prop_CDS = sum(xds == "C") / total,
    prop_BDS = sum(xds == "B") / total,
    prop_TDS = sum(xds == "T") / total,
    prop_ODS = sum(xds == "O") / total,
    prop_UDS = sum(xds == "U") / total,
    prop_PDS = sum(xds == "P") / total
  ) %>%
  left_join((VI_matches_demo %>% dplyr::select(VIHI_ID, pair)), by = "VIHI_ID")
write.csv(xds_props_wide,
          "data/LENA/Transcripts/Derived/xds_props_wide.csv")
xds_props <- xds_props_wide %>%
  pivot_longer(
    cols = prop_ADS:prop_PDS,
    names_to = "addressee",
    names_prefix = "prop_",
    values_to = "prop"
  )
write.csv(xds_props, "data/LENA/Transcripts/Derived/xds_props.csv")

# linguistic quality ----
## TTR ----
### calculate type-token ratio in annotations
TTR_calculations <- VITD_LENA_words %>%
  group_by(VIHI_ID) %>%
  arrange(uttnum) %>%
  filter(Word != "0",
         speaker != "CHI",
         speaker != "EE1") %>%
  mutate(num_obs = n()) %>%
  mutate(bin = rep(1:ceiling(num_obs / 100), each = 100)[1:num_obs]) %>%
  group_by(VIHI_ID, bin) %>%
  summarise(
    num_words_in_bin = n(),
    num_unique_words = n_distinct(Word),
    ttr = num_unique_words / num_words_in_bin
  ) %>%
  group_by(VIHI_ID) %>%
  summarise(num_bins = n(),
            mean_ttr = mean(ttr)) %>%
  mutate(group = as.factor(str_sub(VIHI_ID, 1, 2))) %>%
  left_join((VI_matches_demo %>% dplyr::select(VIHI_ID, pair)), by = "VIHI_ID")


## MLU ----
tokenized_VITD_transcripts <-
  morphemepiece_tokenize(
    VITD_transcripts$utterance_clean,
    vocab = morphemepiece_vocab(),
    lookup = morphemepiece_lookup(),
    unk_token = "[UNK]",
    max_chars = 100
  ) %>% #split utterances into morphemes (based on entries in the morphemepiece "dictionary"). each listing has its own number
  plyr::ldply(rbind) %>% # split each tokenized_VI, then bind each as rows in a dataframe
  mutate_all(funs(ifelse(is.na(.), 0, 1)))

tokenized_VITD_transcripts_with_counts <-
  tokenized_VITD_transcripts %>%
  mutate(morphemecount = rowSums(tokenized_VITD_transcripts)) %>%
  mutate(utt_num = 1:nrow(tokenized_VITD_transcripts))

simple_morpheme_counts <-
  tokenized_VITD_transcripts_with_counts %>%
  left_join((VITD_transcripts %>% dplyr::select(-group,-code,-con))) %>%
  dplyr::select(VIHI_ID, utt_num, speaker, xds, utterance_clean, morphemecount) %>%
  mutate(group = as.factor(str_sub(VIHI_ID, 1, 2)))

MLUs <- simple_morpheme_counts %>%
  filter(!is.na(utterance_clean)) %>%
  group_by(group, VIHI_ID) %>%
  summarise(MLU = mean(morphemecount)) %>%
  left_join((VI_matches_demo %>% dplyr::select(VIHI_ID, pair)), by = "VIHI_ID")
write.csv(MLUs, "data/LENA/Transcripts/Derived/MLUs.csv")

# random_MLU_subset <- simple_morpheme_counts%>%
#     filter(!is.na(morphemecount) &!is.na(utterance)) %>%
#   sample_n(round(nrow(simple_morpheme_counts%>%filter(!is.na(morphemecount) &!is.na(utterance)))*.1))

# secret_random_MLU_subset <- random_MLU_subset %>%
#   dplyr::select(utt_num, speaker, xds,utterance) %>%
#   mutate(manual_morpheme_count = "NA")
# # # Export the random subset to a CSV file for manual coding in Excel
# write_csv(secret_random_MLU_subset, "data/LENA/Transcripts/Derived/secret_random_MLU_subset.csv")

random_MLU_subset <-
  read.csv("data/LENA/Transcripts/Derived/random_MLU_subset.csv")
manually_coded_MLU_subset <-
  read.csv("data/LENA/Transcripts/Derived/manually_coded_MLU_subset.csv")


# Calculate agreement between the manual coding and original data
MLU_subset_for_agreement <-
  left_join((
    manually_coded_MLU_subset %>% distinct(utterance, utt_num, .keep_all = TRUE)
  ),
  random_MLU_subset,
  by = c("utterance", "utt_num")
  ) %>%
  dplyr::select(utterance, morphemecount, manual_morpheme_count) %>%
  filter(!is.na(manual_morpheme_count) & !is.na(morphemecount)) %>%
  mutate(
    morpheme_diff = morphemecount - manual_morpheme_count,
    abs_morpheme_diff = abs(morphemecount - manual_morpheme_count)
  )

# Calculate Intraclass Correlation
MLU_agreement <- icc(MLU_subset_for_agreement %>%
                       dplyr::select(manual_morpheme_count, morphemecount))


# conceptual quality ----
## sensory word props ----
sensory_props_wide <- VITD_LENA_words %>%
  anti_join(stop_words, by = c("Word" = "word")) %>%
  filter(!is.na(Auditory.mean))  %>% # filter out words that don't have a perceptual rating
  mutate(
    Modality = case_when(
      Max_strength.perceptual < 3.5 ~ "Amodal",
      Exclusivity.perceptual <= .5 ~ "Multimodal",
      Max_strength.perceptual > 4  & Exclusivity.perceptual >= .3 ~ Dominant.perceptual,
      Max_strength.perceptual < 3.5 ~ "Amodal",
      TRUE ~ Dominant.perceptual
    )
  ) %>%
  group_by(group, VIHI_ID) %>%
  summarise(
    total = n(),
    prop_Auditory = sum(Modality == "Auditory") / total,
    prop_Visual = sum(Modality == "Visual") / total,
    prop_Olfactory = sum(Modality == "Olfactory") / total,
    prop_Gustatory = sum(Modality == "Gustatory") / total,
    prop_Haptic = sum(Modality == "Haptic") / total,
    prop_Interoceptive = sum(Modality == "Interoceptive") / total,
    prop_Multimodal = sum(Modality == "Multimodal") / total,
    prop_Amodal = sum(Modality == "Amodal") / total
  ) %>%
  left_join((VI_matches_demo %>% dplyr::select(VIHI_ID, pair)), by = "VIHI_ID")
write.csv(sensory_props_wide,
          "data/LENA/Transcripts/Derived/sensory_props_wide.csv")
sensory_props <- sensory_props_wide %>%
  pivot_longer(
    cols = prop_Auditory:prop_Amodal,
    names_to = "Modality",
    names_prefix = "prop_",
    values_to = "prop"
  )

write.csv(sensory_props,
          "data/LENA/Transcripts/Derived/sensory_props.csv")

## tense/displacement----
udpipe_english <- udpipe_download_model(language = "english")
udmodel_english <-
  udpipe_load_model(file = udpipe_english$file_model)
annotated_utterances <- udpipe_annotate(udmodel_english,
                                        x = VITD_transcripts$utterance_clean,
                                        doc_id = VITD_transcripts$VIHI_ID) %>%
  as.data.frame()
write.csv(annotated_utterances,
          "data/LENA/Transcripts/Derived/annotated_utterances.csv")

verbs_only <- annotated_utterances %>%
  filter(
    xpos %in% c("VB", "VBD", "VBP", "VBN", "VBG", "VBZ", "AUX", "MD"),
    token != "=!" &
      token != "xxx"
  ) %>%
  distinct(doc_id, paragraph_id, sentence_id, .keep_all = TRUE) %>%
  mutate(
    temporality = case_when(
      grepl('Tense=Past', feats) ~ "displaced",
      grepl('Mood=Imp', feats) ~ "uncategorized",
      xpos == "MD" ~ "displaced",
      grepl('gonna', sentence) |
        grepl('gotta', sentence) |
        grepl('wanna', sentence) |
        grepl('going to', sentence) |
        grepl('got to', sentence) |
        grepl('want to', sentence) |
        grepl('have to', sentence) | grepl('if ', sentence) ~ "displaced",
      grepl('Mood=Ind', feats) &
        grepl('Tense=Pres', feats) |
        grepl('VerbForm=Ger', feats) ~ "present",
      TRUE ~ "uncategorized"
    )
  )
write.csv(verbs_only, "data/LENA/Transcripts/Derived/verbs_only.csv")

# Randomly select a subset of the utterances for manual coding
# random_displacement_subset <- verbs_only %>%
#   sample_n(round((0.1 * nrow(verbs_only))))
# secret_random_displacement_subset <- random_displacement_subset %>%
#   dplyr::select(-doc_id, -upos, -xpos, -feats, -temporality) %>%
#   mutate(manual_temporality = "NA")
# Export the random subset to a CSV file for manual coding in Excel
# write_csv(secret_random_displacement_subset, "data/LENA/Transcripts/Derived/secret_random_displacement_subset.csv")
# Once manual coding is done in Excel, read the manually coded CSV file back into R
random_displacement_subset <-
  read_csv("data/LENA/Transcripts/Derived/random_displacement_subset.csv")

manually_coded_displacement_subset <-
  read_csv("data/LENA/Transcripts/Derived/manually_coded_displacement_subset.csv")
# Calculate agreement between the manual coding and original data
displacement_subset_for_agreement <-
  left_join(
    random_displacement_subset,
    manually_coded_displacement_subset,
    by = c(
      "paragraph_id",
      "sentence_id",
      "sentence",
      "token_id",
      "token",
      "lemma",
      "head_token_id",
      "dep_rel",
      "deps",
      "misc"
    )
  ) %>%
  dplyr::select(doc_id,
                paragraph_id,
                sentence_id,
                sentence,
                temporality,
                manual_temporality, 
                feats) %>%
  filter(!grepl("Mood=Imp", feats))

# Calculate Percent Agreement
displacement_agreement <-
  sum(
    displacement_subset_for_agreement$temporality == displacement_subset_for_agreement$manual_temporality,
    na.rm = TRUE
  ) / nrow(displacement_subset_for_agreement %>% filter(!is.na(manual_temporality)))
displaced_agreement <-
  sum((
    displacement_subset_for_agreement %>% filter(manual_temporality == "displaced")
  )$temporality == (
    displacement_subset_for_agreement %>% filter(manual_temporality == "displaced")
  )$manual_temporality
  ) / nrow(displacement_subset_for_agreement %>% filter(manual_temporality == "displaced"))


temporality_props_wide <- verbs_only %>%
  dplyr::rename(VIHI_ID = doc_id) %>%
  group_by(VIHI_ID) %>%
  summarize(
    verb_utt_count = n(),
    prop_displaced = (sum(temporality == "displaced") / verb_utt_count),
    prop_present =  (sum(temporality == "present") / verb_utt_count),
    prop_uncategorized = (sum(temporality == "uncategorized") /
                            verb_utt_count)
  ) %>%
  mutate(group = as.factor(str_sub(VIHI_ID, 1, 2))) %>%
  left_join((VI_matches_demo %>% dplyr::select(VIHI_ID, pair)), by = "VIHI_ID")
write.csv(
  temporality_props_wide,
  "data/LENA/Transcripts/Derived/temporality_props_wide.csv"
)
temporality_props <- temporality_props_wide %>%
  pivot_longer(
    cols = prop_displaced:prop_uncategorized,
    names_to = "verb_temporality",
    names_prefix = "prop_",
    values_to = "prop"
  )
write.csv(temporality_props,
          "data/LENA/Transcripts/Derived/temporality_props.csv")

## content word CBOI ----
content_words_only <- annotated_utterances %>%
  filter(upos %in% c("ADJ", "ADV", "NOUN", "VERB")) %>%
  left_join(CBOI_norms, by = c("lemma" = "Word")) %>%
  filter(CBOI_Mean != "NA") %>%
  dplyr::rename(VIHI_ID = doc_id) %>%
  dplyr::select(VIHI_ID, sentence, token, lemma, upos, CBOI_Mean) %>%
  mutate(group = as.factor(str_sub(VIHI_ID, 1, 2)))
write.csv(content_words_only,
          "data/LENA/Transcripts/Derived/content_words_only.csv")
subj_CBOI_means <- content_words_only %>%
  group_by(group, VIHI_ID) %>%
  summarise(CBOI_Mean = mean(CBOI_Mean)) %>%
  left_join((VI_matches_demo %>% dplyr::select(VIHI_ID, pair)), by = "VIHI_ID")
write.csv(subj_CBOI_means,
          "data/LENA/Transcripts/Derived/subj_CBOI_means.csv")

