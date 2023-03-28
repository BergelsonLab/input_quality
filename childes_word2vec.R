library(tidyverse)
library(tidytext)
library(word2vec)
library(rdist)

# helper funcs
matrix2longdf <- function(mat, # i blatantly stole this from https://github.com/bedapub/ribiosUtils/blob/HEAD/R/matrix2longdf.R
                          row.names, col.names,
                          longdf.colnames=c("row","column","value")) {
  if(missing(row.names)) row.names <- rownames(mat)
  if(missing(col.names)) col.names <- colnames(mat)
  
  if(is.null(row.names)) row.names <- 1:nrow(mat)
  if(is.null(col.names)) col.names <- 1:ncol(mat)
  
  value <- as.vector(mat)
  if(length(row.names)!=nrow(mat))
    warning("row.names is inconsistent with the matrix dim")
  if(length(col.names)!=ncol(mat))
    warning("col.names is inconsistent with the matrix dim")
  
  rn <- rep(row.names, ncol(mat))
  cn <- rep(col.names, each=nrow(mat))
  res <- data.frame(row=rn,
                    column=cn,
                    value=value)
  colnames(res) <- longdf.colnames
  return(res)
}

# read in data
childes_utterances <- childesr::get_utterances(language="eng",role_exclude = c("Target_Child","Investigator"),db_version='2021.1') 
write_csv(childes_utterances,"data/childes_utterances.csv")

# clean data
# all lowercase
# remove punctuation
# remove stop words?
childes_utterances_clean <- childes_utterances %>% 
  filter(speaker_role != "Target_Child", speaker_role!="Investigator") 


# setting a seed just guarantees that we get the same result each time we run
# the process so if i train a word2vec model, forget to save it, i can produce
# the exact same embeddings by setting the same seed

set.seed(112345)

#before training, let's get the data into a simple, uniform format by selecting
#just one column and formatting in all lowercase 


# now this line will train the model. i've set the parameters to closely
# resemble the optimal model parameters found by Baroni et al. (2014) in a
# systematic investigation of possible iterations of word2vec. this means that
# each vector will have 400 dimensions, and the model will run  15 iterations.
# more iterations leads to a more precise model, but also higher numbers of each
# result in greater computational demand. hence commenting out this code! 
# a pre-trained model is below
start_time <- Sys.time()
childes_word2vec_model <- word2vec(x = childes_utterances_clean$gloss, type = "cbow", dim = 400, iter = 15)
end_time <- Sys.time()
time_to_train_model <- start_time - end_time #this command doesn't work, but it only took like 7 min to run


# let's go ahead and save that model

write.word2vec(childes_word2vec_model, "childes_word2vec_model.bin") 

# this function reads in a word2vec model that has been pre-trained, using the
# code above. this just means that the word embeddings have already been
# calculated - each word is stored as a 400-dimensional vector
childes_word2vec_model <- read.word2vec("childes_word2vec_model.bin") # only has 15000 word vocabulary. is that enough?


# for each VI,TD kid, I want the mean pairwise distance
# first, extract the vectors for each word type in VIHI
VITD_word_types <- unique(VIHI_LENA_words$Word)
VITD_word_embeddings <- predict(childes_word2vec_model, VITD_word_types, type="embedding")
VITD_word_pairs <- VIHI_LENA_words %>% 
  filter(speaker!="CHI") %>%
  group_by(VIHI_ID)%>%
  summarize(pair = combn(Word, 2, paste, collapse = "_"))

xds_word_pairs <- VIHI_LENA_words %>% 
  filter(speaker!="CHI",
         group=="TD",
         xds=="C"|xds=="A") %>%
  group_by(xds)%>%
  summarize(pair = combn(Word, 2, paste, collapse = "_"))


pairwise_distances_matrix <- pdist(VITD_word_embeddings, "angular")
pairwise_distances <- matrix2longdf(pairwise_distances_matrix) %>% 
  mutate(pair=paste(row,column,sep="_")) %>%
  left_join(VITD_word_pairs)

subject_pairwise_distances <- pairwise_distances %>% 
  group_by(VIHI_ID) %>% 
  summarize(mean_distance = mean(value,na.rm=TRUE), # calculate pairwise distance summary stats
            min_distance = min(value,na.rm=TRUE),
            max_distance = max(value,na.rm=TRUE),
            range_distance = max_distance-min_distance,
            sd_distance= sd(value,na.rm=TRUE))
write.csv(subject_pairwise_distances, "data/subject_pairwise_distances.csv")

xds_pairwise_distances <- pairwise_distances %>% 
  group_by(xds) %>% 
  summarize(mean_distance = mean(value,na.rm=TRUE), # calculate pairwise distance summary stats
            min_distance = min(value,na.rm=TRUE),
            max_distance = max(value,na.rm=TRUE),
            range_distance = max_distance-min_distance,
            sd_distance= sd(value,na.rm=TRUE))

ggplot(xds_pairwise_distances) +
  geom_linerange(aes(x=xds, color=xds,ymin=mean_distance+sd_distance,ymax=mean_distance-sd_distance)) +
  geom_point(aes(x=xds, color=xds,y=mean_distance)) +
  theme_classic()
