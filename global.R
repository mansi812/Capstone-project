## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----read----------------------------------------------------------------
##
unigram<-read.csv(file="Data/newunigram.csv")
bigram<-read.csv(file="Data/newbigram.csv")
trigram<-read.csv(file="Data/newtrigram.csv")
fourgram<-read.csv(file="Data/newfourgram.csv")


## ----clean---------------------------------------------------------------
library(stringr)
library(dplyr)
library(tm)
custom.words<-c(stopwords("en"),"said","will","s")
clean.text <- function(string){
                # lower case
                string <- tolower(string)
                string<-removeWords(string,custom.words)
                # e-mail
                string <- str_replace_all(string, "\\S+@\\S+", "") 
                # URLs
                string <- str_replace_all(string, "http[[:alnum:]]*", "")
                # hashtags
                string <- str_replace_all(string, "#[[:alnum:]]*", "")
                string <- str_replace_all(string, "# [[:alnum:]]*", "")
                # @
                string <- str_replace_all(string, "@[[:alnum:]]*", "")
                string <- str_replace_all(string, "@ [[:alnum:]]*", "")
                # twitter language
                string <- str_replace_all(string, "RT", "")
                string <- str_replace_all(string, "PM", "")
                string <- str_replace_all(string, "rt", "")
                string <- str_replace_all(string, "pm", "")
                # apostrophes 
                string <- str_replace_all(string, "'ll", " will")
                string <- str_replace_all(string, "'d", " would")
                string <- str_replace_all(string, "can't", "cannot")
                string <- str_replace_all(string, "n't", " not")
                string <- str_replace_all(string, "'re", " are")
                string <- str_replace_all(string, "'m", " am")
                string <- str_replace_all(string, "n'", " and")
                #'s-genitive
                string <- str_replace_all(string, "'s", " ")
                string <- str_replace_all(string, "s'", " ")
                # everything that is not number,letter, whitespace or '
                string <- str_replace_all(string, "[^[:alnum:]]", " ")
                # digits
                string <- str_replace_all(string, "[:digit:]", "")
                # more then one whitespace
                string <- str_replace_all(string, "\\s+", " ")
                # trim whitespace
                string <- str_trim(string, side = c("both"))
                # deal with "don t und dont"
                string <- str_replace_all(string, "don t", "do not")
                string <- str_replace_all(string, "dont", "do not")
                # deal with "u s for usa"
                string <- str_replace_all(string, "u s", "usa")
               
                
return(string)
}

## ------------------------------------------------------------------------
library(stringr)
getLastWords <- function(string, words) {
    pattern <- paste("[a-z']+( [a-z']+){", words - 1, "}$", sep="")
    return(substring(string, str_locate(string, pattern)[,1]))
}

## ------------------------------------------------------------------------
#pred.word3 is for cases when input is 3 words

pred.word3<-function(input.sentence)
{

  #chose the last three words from input string as an input for fourgram and repeat same steps for trigram and twogram
   lastword3<-getLastWords(input.sentence,3)
   lastword2<-getLastWords(lastword3,2)
   lastword1<-getLastWords(lastword2,1)

    ##Starting the predictions from highest(fourgram) table
   #Lets filter all the four grams starting with the three input words
   pred.four<-fourgram %>%
   filter(grepl(paste0("^", lastword3," "), n.gram)) %>% 
   mutate(predictedword=getLastWords(n.gram,1)) %>%
   mutate(probability=counts/trigram$counts[lastword3==trigram$n.gram]) %>%
   arrange(desc(probability)) %>%
  select(predictedword,probability) 

     ##Repeat the same for three gram table, and the back off factor=0.4 
     
pred.tri<-trigram %>%
filter(grepl(paste0("^", lastword2," "), n.gram)) %>%  
mutate(predictedword=getLastWords(n.gram,1)) %>%
 mutate(probability=counts*0.4/bigram$counts[bigram$n.gram==lastword2]) %>% 
 arrange(desc(probability)) %>%
  select(predictedword,probability)

##Repeat the same for two gram table with the back off factor =0.4

pred.two<- bigram %>%
 filter(grepl(paste0("^", lastword1," "), n.gram)) %>%  
mutate(predictedword=getLastWords(n.gram,1)) %>%
mutate(probability=counts*0.4*0.4/unigram$counts[unigram$n.gram==lastword1]) %>% 
 arrange(desc(probability)) %>%
  select(predictedword,probability)

##Repeat the same for one gram
pred.one<-unigram %>%
mutate(predictedword=n.gram) %>%
mutate(probability=counts*0.4*0.4*0.4/sum(unigram$counts)) %>%
  arrange(desc(probability)) %>%
select(predictedword,probability)

##Combine th results in a dataframe and arrange in the descending order
result<-rbind(pred.tri,pred.two,pred.one) %>%
  arrange(desc(probability)) %>%
top_n(3)

return(result)
}

## ----pred2---------------------------------------------------------------

##pred.word2 when the input is two words

pred.word2<-function(input.sentence)
{

  #chose the last two words from input string as an input for trigram and repeat same steps for bigram and unigram
   lastword2<-getLastWords(input.sentence,2)
   lastword1<-getLastWords(lastword2,1)
  

    ##Starting the predictions from trigram table
   #Lets filter all the tri grams starting with the two input words
   pred.tri<-trigram %>%
   filter(grepl(paste0("^", lastword2," "), n.gram)) %>% 
   mutate(predictedword=getLastWords(n.gram,1)) %>%
   mutate(probability=counts/bigram$counts[lastword2==bigram$n.gram]) %>%
   arrange(desc(probability)) %>%
  select(predictedword,probability) 

     ##Repeat the same for two gram table, and the back off factor=0.4 
     
pred.two<-bigram %>%
filter(grepl(paste0("^", lastword1," "), n.gram)) %>%  
mutate(predictedword=getLastWords(n.gram,1)) %>%
 mutate(probability=counts*0.4/unigram$counts[unigram$n.gram==lastword1]) %>% 
 arrange(desc(probability)) %>%
  select(predictedword,probability)



##Repeat the same for one gram
pred.one<-unigram %>%
mutate(predictedword=n.gram) %>%
mutate(probability=counts*0.4*0.4/sum(unigram$counts)) %>%
  arrange(desc(probability)) %>%
select(predictedword,probability)

##Combine th results in a dataframe and arrange in the descending order
result<-rbind(pred.tri,pred.two,pred.one) %>%
  arrange(desc(probability)) %>%
top_n(3)

return(result)
}

## ------------------------------------------------------------------------


##pred.word1 when the input is one word

pred.word1<-function(input.sentence)
{

  #chose the last two words from input string as an input for trigram and repeat same steps for bigram and unigram
   lastword1<-getLastWords(input.sentence,1)
   
  

    ##Starting the predictions from bigram table
   #Lets filter all the bigrams starting with the  input word
   pred.two<-bigram %>%
   filter(grepl(paste0("^", lastword1," "), n.gram)) %>% 
   mutate(predictedword=getLastWords(n.gram,1)) %>%
   mutate(probability=counts/unigram$counts[lastword1==unigram$n.gram]) %>%
   arrange(desc(probability)) %>%
  select(predictedword,probability) 

     


##Repeat the same for one gram with a back off factor of 0.4
pred.one<-unigram %>%
mutate(predictedword=n.gram) %>%
mutate(probability=counts*0.4/sum(unigram$counts)) %>%
  arrange(desc(probability)) %>%
select(predictedword,probability)

##Combine th results in a dataframe and arrange in the descending order
result<-rbind(pred.two,pred.one) %>%
  arrange(desc(probability)) %>%
top_n(3)

return(result)
}

## ------------------------------------------------------------------------

predictword<-function(input)
{
  cleaninput<-clean.text(input)
  length<-length(unlist(strsplit(cleaninput, " ")))
  if(length>=3)
  {output<-pred.word3(input)
  } else if(length==2)
  {output<-pred.word2(input)
  }
  else
    output<-pred.word1(input)
  
  return(output)
  
}


