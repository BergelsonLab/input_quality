# Input Quality Preprocessing

# praise: zhenya2erin: While this code is less clean than the code in the manuscript, I still didn't need any help figuring out what happens here. So most of my comments below are about implementation details.

# issue: zhenya2erin: Distinct issue. Using `distinct(***, .keep_all=TRUE)` is a hard-to-notice error waiting to happen because you are silently dropping rows with potentially non-redundant information. It groups the dataframe by the variables in `***` and keeps the first row in each group. Even when that is exactly what one wants (like when picking the first verb for temporality judgment), the table should be sorted within groups first so that "the first" makes sense. And if you do that, you might as well use `filter(row_number() == 1` or `slice(1)` instead of `distinct(***, .keep_all=TRUE)`. In all other cases, the duplication should be investigated and avoided.

library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(morphemepiece)
library(tidytext)
library(udpipe)

# functions
get_match_number <- function(df) { 
  df %>%
    left_join((VI_matches_demo %>% select(VIHI_ID,pair)), by = "VIHI_ID")
}

## demographics
VI_matches_demo <- read_csv("../data/Demographics/VI_matches_demo.csv")

lancaster_norms <-
  read_csv("../data/Norms/Lancaster_sensorimotor_norms_for_39707_words.csv") %>%
  mutate(Word = tolower(Word)) # make the contents of the Word column lowercase (to align with our transcription style)

### list of TD matches
TD_matches <- # one recording in VI_matches_demo is not assigned a match here because it is a participant's second recording
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
    "TD_472_829",
    "TD_473_844",
    "TD_474_966",
    "TD_475_481",
    "TD_477_217"
  )

## Load in transcripts and automated metrics

LENA_counts <-
  read_csv("../data/LENA/Automated/VIHI_its_details.csv") %>%
  filter(match_type == "VI" | VIHI_ID %in% TD_matches) %>% # only look at data from VI kids or their TD matches
  mutate(
    AWC_stand = AWC / (total_time_dur / (30 * 60)), # change AWC to be per 30 minutes (so that it aligns with the length of time our random sample annotations span)
    CTC_stand = CTC / (total_time_dur / (30 * 60)) # change CTC to be per 30 minutes (so that it aligns with the length of time our random sample annotations span)
  ) %>%
  get_match_number() %>% # add a column that indicates which pair each child is a member of
  # issue: zhenya2erin: See the "Distinct issue" at the top of the script.
  distinct(its_path, .keep_all = TRUE) # remove duplicate rows
write_csv(LENA_counts, "../data/LENA/Automated/LENA_counts.csv")


###read in data that has been mass exported via ELAN
VIHI_transcripts_messy <- read_csv("../data/LENA/Transcripts/Raw/VI_LENA_and_TD_matches_messy2023-05-22.csv")

VITD_transcripts <-
  read_csv("../data/LENA/Transcripts/Raw/VI_LENA_and_TD_matches_2023-09-11.csv") %>% 
  mutate(VIHI_ID = as.factor(str_sub(VIHI_ID, 1, 10))) %>%
  filter(VIHI_ID %in% TD_matches | group == "VI") %>% # only look at data from VI kids or their TD matches
  mutate(
    utterance_clean = str_replace_all(utterance, "&=\\w+", ""), # Remove substrings starting with &=
    utterance_clean = str_replace_all(utterance_clean, "<[^[:space:]]+?\\b(?!: clarifier)>", ""), # Remove text enclosed in <> only if not followed by [: clarifier]
    utterance_clean = str_replace_all(utterance_clean, "\\[-\\s[a-z]{3}\\]", ""), # Remove language tags like "[- spa]" or "[- ger]"
    utterance_clean = str_replace_all(utterance_clean, "=!\\w+\\s*", ""), # Remove any substrings starting with "=!" followed by one or more word characters and optional whitespace (get rid of the style markers =!shrieks)
    utterance_clean = str_replace_all(utterance_clean, "[[:punct:]&&[^']]", "") # Remove any punctuation, except for apostrophes
  ) %>%
  filter(speaker != "EE1", speaker != "CHI") %>% # remove CHI utts and electronic noise
  filter(!utterance == c("xxx.")) %>% #remove unintelligible utterances
  filter(!grepl("utt@|inq@", speaker)) %>% # remove non-utterance info
  filter(!is.na(utterance_clean)) %>%
  mutate(utt_num = 1:nrow(.)) # number the utterances
write_csv(VITD_transcripts, "../data/LENA/Transcripts/Derived/VITD_transcripts.csv")

VITD_LENA_utterances_split <- VITD_transcripts %>%
  rename(Word = utterance_clean) %>%
  separate_wider_delim(
    Word,
    delim = ' ',
    too_few = 'align_start',
    too_many = 'error',
    names_sep = '')

VITD_LENA_words <- VITD_LENA_utterances_split %>%
  mutate(uttnum = seq(1, nrow(VITD_LENA_utterances_split), 1)) %>% #give each utterance a unique number
  pivot_longer(cols = starts_with('Word'),
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
  filter(group != "HI" & speaker != "CHI" & Word != "")
write_csv(VITD_LENA_words, "../data/LENA/Transcripts/Derived/VITD_LENA_words.csv")

# quantity ----

# count word tokens in annotations
manual_word_tokens <- VITD_LENA_words %>%
  filter(sampling_type == "random") %>% # for quantity measures, we're only looking at random samples (high-volume might overrepresent)
  group_by(VIHI_ID) %>%
  summarise(tokens = n()) %>% # count the number of rows, grouped by child (VIHI_ID). each row is one word, so this should give us the token count.
  get_match_number() %>% # add pair info
  mutate(group = as.factor(str_sub(VIHI_ID, 1, 2))) %>% # add group (determined by VIHI_ID)
  # issue: zhenya2erin: See the "Distinct issue" at the top of the script.
  distinct(VIHI_ID, .keep_all = TRUE) 

write_csv(manual_word_tokens,
          "../data/LENA/Transcripts/Derived/manual_word_tokens.csv")

# interactiveness  ----
## CTC----
## child-directed----
xds_props_wide <- VITD_transcripts %>%
  filter(sampling_type == "random") %>% # for interaction measures, we're only looking at random samples (high-volume might overrepresent)
  group_by(group, VIHI_ID) %>%
  summarise( #calculate proportions by xds. 
    total = n(), 
    prop_ADS = sum(xds == "A") / total,
    prop_CDS = sum(xds == "C") / total,
    prop_BDS = sum(xds == "B") / total,
    prop_TDS = sum(xds == "T") / total,
    prop_ODS = sum(xds == "O") / total,
    prop_UDS = sum(xds == "U") / total,
    prop_PDS = sum(xds == "P") / total 
  ) %>%
get_match_number() 
write_csv(xds_props_wide, "../data/LENA/Transcripts/Derived/xds_props_wide.csv")
xds_props <- xds_props_wide %>%
  pivot_longer(
    cols = prop_ADS:prop_PDS,
    names_to = "addressee",
    names_prefix = "prop_",
    values_to = "prop"
  )
write_csv(xds_props, "../data/LENA/Transcripts/Derived/xds_props.csv")

# linguistic quality ----
## TTR ----
### calculate type-token ratio in annotations
# issue: zhenya2erin: 
TTR_calculations <- VITD_LENA_words %>%
  filter(Word != "0",
         speaker != "CHI",
         speaker != "EE1") %>%
  group_by(VIHI_ID) %>%
  arrange(uttnum) %>%
   mutate(bin = rep(1:ceiling(n() / 100), each = 100)[1:n()]) %>%
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
  get_match_number()
write_csv(TTR_calculations, "../data/LENA/Transcripts/Derived/TTR_calculations.csv")

## MLU ---- 
MLUs <-
  VITD_transcripts %>%
  mutate(morphemecount = lengths(morphemepiece_tokenize(utterance_clean))) %>%
  left_join((VITD_transcripts %>% dplyr::select(-group,-code,-con))) %>%
  select(VIHI_ID, utt_num, speaker, xds, utterance_clean, morphemecount) %>%
  mutate(group = as.factor(str_sub(VIHI_ID, 1, 2))) %>%
  filter(!is.na(utterance_clean)) %>%
  group_by(group, VIHI_ID) %>%
  summarise(MLU = mean(morphemecount)) %>%
  get_match_number()
write_csv(MLUs, "../data/LENA/Transcripts/Derived/MLUs.csv")


#### get a random subset of utterances, and write out to a csv. DO NOT UNCOMMENT CODE AND RERUN THESE STEPS (risks overwriting previous csv)
# random_MLU_subset <- simple_morpheme_counts%>%
#     filter(!is.na(morphemecount) &!is.na(utterance)) %>%
#   sample_n(round(nrow(simple_morpheme_counts%>%filter(!is.na(morphemecount) &!is.na(utterance)))*.1))
#### hide identifying information about participants
# secret_random_MLU_subset <- random_MLU_subset %>%
#   dplyr::select(utt_num, speaker, xds,utterance) %>%
#   mutate(manual_morpheme_count = "NA")
# # # Export the random subset to a CSV file for manual coding in Excel
# write_csv(secret_random_MLU_subset, "data/LENA/Transcripts/Derived/secret_random_MLU_subset.csv")

random_MLU_subset <-
  read_csv("../data/LENA/Transcripts/Derived/random_MLU_subset.csv")
manually_coded_MLU_subset <- #after manually counting morphemes (done by EC & LR), this gets read back in for validity
  read_csv("../data/LENA/Transcripts/Derived/manually_coded_MLU_subset.csv")


# Calculate agreement between the manual coding and original data
MLU_subset_for_agreement <-
  left_join((
    # issue: zhenya2erin: See the "Distinct issue" at the top of the script.
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
write_csv(MLU_subset_for_agreement,"../data/LENA/Transcripts/Derived/MLU_subset_for_agreement.csv")



# conceptual quality ----
## sensory word props ----
sensory_props_wide <- VITD_LENA_words %>%
  anti_join(tidytext::stop_words, by = c("Word" = "word")) %>% # get rid of stop words
  filter(!is.na(Auditory.mean))  %>% # filter out words that don't have a perceptual rating
  mutate(
    Modality = case_when( # create extra columns for Amodal and Multimodal. 
      Max_strength.perceptual < 3.5 ~ "Amodal",
      Exclusivity.perceptual <= .5 ~ "Multimodal",
      Max_strength.perceptual > 4  & Exclusivity.perceptual >= .3 ~ Dominant.perceptual,
      Max_strength.perceptual < 3.5 ~ "Amodal",
      TRUE ~ Dominant.perceptual
    )
  ) %>%
  group_by(group, VIHI_ID) %>%
  summarise(
    total = n(), # calculate proportions of words by modality out of total word tokens
    prop_Auditory = sum(Modality == "Auditory") / total,
    prop_Visual = sum(Modality == "Visual") / total,
    prop_Olfactory = sum(Modality == "Olfactory") / total,
    prop_Gustatory = sum(Modality == "Gustatory") / total,
    prop_Haptic = sum(Modality == "Haptic") / total,
    prop_Interoceptive = sum(Modality == "Interoceptive") / total,
    prop_Multimodal = sum(Modality == "Multimodal") / total,
    prop_Amodal = sum(Modality == "Amodal") / total
  ) %>%
  get_match_number()
write_csv(sensory_props_wide,
          "../data/LENA/Transcripts/Derived/sensory_props_wide.csv")
sensory_props <- sensory_props_wide %>%
  pivot_longer(
    cols = prop_Auditory:prop_Amodal,
    names_to = "Modality",
    names_prefix = "prop_",
    values_to = "prop"
  )

write_csv(sensory_props,
          "../data/LENA/Transcripts/Derived/sensory_props.csv")

## tense/displacement---- erin2zhenya: take an extra hard look at this? I want to be super sure that this is working how we think it's working
# zhenya2erin: I'll describe how *I* think it is working and how that might differ from what I understood from the manuscript.
#
# annotated_utterances:
# - We split each utterance into rows with one word token per row and add tags to each token (e.g., part of speech, lemma, etc.).
# - For each row/token, we retain the information about the recording it came from and the text of the utterance it came from. However, we lose the information about the id of the utterance it came from though it should be identical to paragraph_id so probably not a big deal.
# - Each token is additionally assigned a sentence_id based on how `udpipe` split the utterance into sentences.
#
# verbs_only:
# - We remove tokens that are not verbs or auxiliaries based on their xpos value implicitly excluding `xpos == 'NNS'` by not including it in the list of tags to keep. This implicitly removes all utterances that don't include such tokens at all and removes some of the sentences from further utterances.
# - We remove "=!" and "xxx" tokens not trusting features inferred by udpipe for these tokens when they are tagged as verbs. This also implicitly removes whole utterances and individual sentences.
# - From each of the remaining sentences (not utterances!), we keep only the first verb as identified by the conditions above (xpos in a special list, not "=!" or "xxx").
# - Finally, we assign "temporality" based on the features extracted by `udpipe` and the sentence including certain word combinations.
#
# While the above is done in two steps, it could be done in one because we don't seem to use `annotated_utterances` anywhere.
#
# And here is what I understood from the manuscript:
# - Each utterance is categorized as "displaced" or "present" or not categorized. This is done based on the first verb in the utterance. It is unclear how utterances without verbs are treated.
# - The number of uncategorized utterances and the total number of utterances are reported.
#
# Here is how my understanding of code differs from my understanding of the manuscript:
# - The code drops utterances without verbs and so the reported denominator is the number of utterances (really, sentences) with verbs, not the total number of utterances in the corpus and the reported numerator is the number of utterances (really, sentences) with verbs that are categorized as "displaced" or "present".
# - The code's output contains sentence-level information and not the utterance-level and not the utterance-level information as it says in the manuscript.
#
# I believe that the code, the manuscript, or both should be updated to make the two consistent with each other.
#
# I've also added some comments to the code below.

udpipe_english <- udpipe_download_model(language = "english") # download the udpipe english model
udmodel_english <-
  udpipe_load_model(file = udpipe_english$file_model)
# issue: zhenya2erin: I think it would be cleaner to use a compbination of VIHI_ID and utt_num as the doc_id. This would, for example, allow us to left-join temporalities to the utterances. Probably paragraph_id is identical to utt_num but why risk it?
annotated_utterances <- udpipe_annotate(udmodel_english, # apply the udpipe model of english to the cleaned utterances. this should give us syntactical parsing (not perfect, but as we see later down, pretty similar to human raters)
                                        x = VITD_transcripts$utterance_clean,
                                        doc_id = VITD_transcripts$VIHI_ID) %>%
  as.data.frame()
write_csv(annotated_utterances,
          "../data/LENA/Transcripts/Derived/annotated_utterances.csv")

verbs_only <- annotated_utterances %>%
  # issue: zhenya2erin: Why is the filtering done on xpos instead of upos? As far as I could tell, the only difference is that the current version doesn't count tokens with upos == 'VERB' *and* xpos == 'NNS' as verbs. Is that intentional? I would either switch to filtering based on upos or explain why the NNS tokens are excluded
  filter(
    xpos %in% c("VB", "VBD", "VBP", "VBN", "VBG", "VBZ", "AUX", "MD"), # filter to verbs only
    token != "=!" &
      token != "xxx"
  ) %>%
  # suggestion: zhenya2erin: In this case, `distinct` keeping the first row is the point and since the order has most likely been maintained, it should work as expected. For clarity, however, I would explicitly sort verbs within each utterance by `sentence_id` and `token_id` and then select the first row (`filter(row_number() == 1)` or `slice(1)`).
  # issue: zhenya2erin: I think parsing of the utterances into sentences is of no interest to us. Unless I am worng about that, I would drop `sentence_id` from the next line.
  distinct(doc_id, paragraph_id, sentence_id, .keep_all = TRUE) %>%
  mutate(
    temporality = case_when( # this is based on EC & LR's top-down judgments of how to categorize words based on tense
      grepl('Tense=Past', feats) ~ "displaced",
      grepl('Mood=Imp', feats) ~ "uncategorized",
      xpos == "MD" ~ "displaced",
      grepl('gonna', sentence) |
        grepl('gotta', sentence) |
        grepl('wanna', sentence) |
        grepl('going to', sentence) |
        grepl('got to', sentence) |
        grepl('want to', sentence) |
        # note: zhenya2erin: I don't know if aperitifs and digestifs form the leitmotif of the corpus but grepl('if ') would match the singular forms of all three of these nouns :-)
        grepl('have to', sentence) | grepl('if ', sentence) ~ "displaced",
      grepl('Mood=Ind', feats) &
        grepl('Tense=Pres', feats) |
        grepl('VerbForm=Ger', feats) ~ "present",
      TRUE ~ "uncategorized"
    )
  )
write_csv(verbs_only, "../data/LENA/Transcripts/Derived/verbs_only.csv")


# Randomly select a subset of the utterances for manual coding DO NOT UNCOMMENT CODE AND RERUN THESE STEPS (risks overwriting previous csv)
#### this follows the same procedure as above ^ 
# random_displacement_subset <- verbs_only %>%
#   sample_n(round((0.1 * nrow(verbs_only))))
# secret_random_displacement_subset <- random_displacement_subset %>%
#   dplyr::select(-doc_id, -upos, -xpos, -feats, -temporality) %>%
#   mutate(manual_temporality = "NA")
# Export the random subset to a CSV file for manual coding in Excel
# write_csv(secret_random_displacement_subset, "data/LENA/Transcripts/Derived/secret_random_displacement_subset.csv")
# Once manual coding is done in Excel, read the manually coded CSV file back into R
random_displacement_subset <-
  read_csv("../data/LENA/Transcripts/Derived/random_displacement_subset.csv")

manually_coded_displacement_subset <- # after manual utterance tagging by EC, LR, and GL, this gets read back in for validity
  read_csv("../data/LENA/Transcripts/Derived/manually_coded_displacement_subset.csv")
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
write_csv(displacement_subset_for_agreement, "../data/LENA/Transcripts/Derived/displacement_subset_for_agreement.csv")

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
    verb_utt_count = n(), # calculate proportions by verb tense out of total utterances
    prop_displaced = (sum(temporality == "displaced") / verb_utt_count),
    prop_present =  (sum(temporality == "present") / verb_utt_count),
    prop_uncategorized = (sum(temporality == "uncategorized") /
                            verb_utt_count)
  ) %>%
  mutate(group = as.factor(str_sub(VIHI_ID, 1, 2))) %>%
  get_match_number()
write_csv(
  temporality_props_wide,
  "../data/LENA/Transcripts/Derived/temporality_props_wide.csv"
)
temporality_props <- temporality_props_wide %>%
  pivot_longer(
    cols = prop_displaced:prop_uncategorized,
    names_to = "verb_temporality",
    names_prefix = "prop_",
    values_to = "prop"
  )
write_csv(temporality_props,
          "../data/LENA/Transcripts/Derived/temporality_props.csv")





# join all input variables into one big dataframe
lotta_data <- LENA_counts %>%
  left_join(MLUs, by=c("VIHI_ID", "group"))  %>%
  left_join(TTR_calculations, by=c("VIHI_ID", "group")) %>%
  left_join(xds_props_wide %>% dplyr::select(-total), by=c("VIHI_ID", "group")) %>%
  left_join(sensory_props_wide %>% dplyr::select(-total), by=c("VIHI_ID", "group"))%>%
  left_join(temporality_props_wide, by=c("VIHI_ID", "group")) %>%
  mutate(ParticipantNumber = as.factor(str_sub(VIHI_ID, 1, 6))) %>%
  mutate(LENA_age_in_days = as.numeric(str_sub(VIHI_ID, 8, length(VIHI_ID))))%>%
  # issue: zhenya2erin: See the "Distinct issue" at the top of the script.
  distinct(ParticipantNumber, .keep_all=TRUE)
write_csv(lotta_data, "../data/LENA/Transcripts/Derived/lotta_data.csv")



perc_random_silent <-
  ((
    VIHI_transcripts_messy %>% filter(
      !is.na(sampling_type) &
        sampling_type == "random" &
        is_silent == "Y"
    ) %>% nrow()
  ) / (
    VIHI_transcripts_messy %>% filter(!is.na(sampling_type) &
                                        sampling_type == "random") %>% nrow()
  )
  ) * 100
write_rds(perc_random_silent, "../data/LENA/Transcripts/Derived/perc_random_silent.rds")
