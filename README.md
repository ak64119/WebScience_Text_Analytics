Graph Analytics on The Trial
================
Akshay Kochhar - 18230051
4 April 2019

**Introduction:**
-----------------

In this assignment, I have applied graph analytics to the text analysis domain. In order to present a meaningful report, we are required to full-fill following objectives:

1.  Graph measurements such as centrality diameter, radius, eccentricity.
2.  Using Clique and community analytical techniques, communities must be thoroughly analyzed. Important communities should be treated as sub-graphs and explain their relevance by utilizing graph measures.
3.  Using knowledge of IR and NLP, examined whether noun, n-gram, entity or named-entity based extraction approach may produce sparser but more interpretable graphs.
4.  Compared the graphs produced by different sections of the book - for example, compared the principle relationships extracted from the first half the book with those extracted from the second half of the book.

In order to fulfil the above required objectives, the book 'The Trial by Franz Kafka'\[1\] was downloaded from project Gutenberg and was manually divided into multiple text files by chapters. The separation of the book into chapter wise text files helped to find relationships between important characters of the main plot of crime fiction.

**Deliverables:**
-----------------

#### **Load Libraries:**

``` r
library(rJava)         #allows to run R inside Java applications.
library(NLP)           #simplify text preprocessing.
library(openNLP)       #processing of natural language text.
library(RWeka)         #tools for data pre-processing, classification, etc.
library(qdap)          #assist in quantitative discourse analysis.
library(magrittr)      #promote semantics for improving code. 
require("NLP")         #simplify text preprocessing.
library(tibble)        #convert data as tibble.
library(tidyverse)     #data manipulation & plotting
library(stringr)       #text cleaning and regular expressions
library(tidytext)      #provides additional text mining functions
library(widyr)         #un-tidying data into a wide matrix
library(dplyr)         #summarize tabular data.
library(igraph)        #network analysis tools
library(ggraph)        #graph visualizations 
library(tidygraph)     #approach to manipulate data frames 
```

### **1. Correlation Graph from Book**

In the below block, the chapter wise text files of the book are processed using Named-Entity Recognition techniques to form a correlation graph between Noun entities. The whole process is as follows:

1.  Read all the text from the text files using loop and convert them to string.
2.  Remove all the unnecessary symbols and spaces using gsub.
3.  Use NLP packages like 'Maxent\_Word\_Token\_Annotator' to tokenize words, sentences and apply POS tagging on the extracted workds.
4.  After applying the NLP techniques, the tokenize words are extracted in tibbles.
5.  The extracted words with POS tags are filtered to obtain words which refer to noun.
6.  The extracted noun entities are processed to rank the words on the basis of tf-idf score.

The description of NER technique is as follows:

Named-entity recognition (NER) is a type of information extraction technique that locates and classifies named entities mentioned in the unstructured or unannotated text into pre-defined categories such as person names, organizations, locations, etc. It includes taking an unannotated block of text and producing an annotated block of text that highlights the names of entities.

I have selected 'The Trial' by Franz Kafka which is a crime fiction. In the below block, six chapters of the book has been processed using NER technique in which noun words related to people name, location, organization and date have been extracted from the unannotated block of text.

Since it is a crime fiction, the probability of repetition of certain un-important words is higher. So, I have used tf-idf filtering technique to find the important words in the annotated text, which is as follows:

1.  In tf-idf, normalized term frequency (tf), measures how frequently a term occurs in a chapter. Since the chapter length of the book can be different, and the term would appear a lot in a longer chapter than a shorter chapter. Therefore, the term frequency is divided by the chapter length (or total number of terms in the chapter) to normalize it.

2.  The inverse document frequency(idf) measures the importance of the term by weighting down highly frequent terms which are not important (such as "is", "of", "that) while scaling up the important terms which occur rarely. It helps to highlight the important terms in the chapter.

``` r
########################## Read in Book ###########################

book_title = "TheTrial"
chapters = 1:6
chapter_stem = "TheTrial"
ext <-".txt"
folder<- "E:/New Volume/Academic/NUIG_College/WEB_NETWORK_SCIENCE/Assignment_4_5/book_data/"

###################################################################

############## Named-Entity Recognition (NER) #####################

book <- tibble()

####### Extract entities from an AnnotatedPlainTextDocument #######
entities <- function(doc, kind) {
  s <- doc$content
  a <- annotation(doc)
  if(hasArg(kind)) {
    k <- sapply(a$features, `[[`, "kind")
    s[a[k == kind]]
  } else {
    s[a[a$type == "entity"]]
  }
}

###################################################################


#read in each chapter
for(i in chapters){
  
  chapter <-paste0(folder,chapter_stem,i,ext)
  
  raw<-readChar(chapter,file.info(chapter)$size)
  
  raw <- paste(raw, collapse = " ")
  
  raw <- as.String(raw)
  
  chapter_text <- raw %>% 
    gsub("[\r\n]+", " ", .) %>%
    gsub('^"',"",.) %>%
    gsub('"$',"",.)
  
  word_toknz <- Maxent_Word_Token_Annotator()
  
  sent_toknz <- Maxent_Sent_Token_Annotator()
  
  pos_tagz <- Maxent_POS_Tag_Annotator()
  
  persn_entity <- Maxent_Entity_Annotator(
    language = "en", kind ="person", probs = FALSE,
    model ="C:/Users/aksha/Documents/R/win-library/3.5/openNLPmodels.en/models/en-ner-person.bin")
  
  lctn_entity <- Maxent_Entity_Annotator(
    language = "en", kind ="location",probs = FALSE,
    model ="C:/Users/aksha/Documents/R/win-library/3.5/openNLPmodels.en/models/en-ner-location.bin")
  
  orgntz_entity <- Maxent_Entity_Annotator(
    language = "en", kind ="organization",probs = FALSE,
  model ="C:/Users/aksha/Documents/R/win-library/3.5/openNLPmodels.en/models/en-ner-organization.bin")
  
  date_entity <- Maxent_Entity_Annotator(language = "en", kind ="date", 
  probs = FALSE,model ="C:/Users/aksha/Documents/R/win-library/3.5/openNLPmodels.en/models/en-ner-date.bin")
  
  tag_list <- list(sent_toknz, word_toknz, pos_tagz, persn_entity, lctn_entity, orgntz_entity, date_entity)
  
  annotated <- NLP::annotate(chapter_text, tag_list)
  
  text_doc <- AnnotatedPlainTextDocument(chapter_text,annotated)
  
  word_subset <- subset(annotated, type == 'word')

  tags = sapply(word_subset$features, '[[', "POS")

  doc_pos <- data_frame(word=chapter_text[word_subset], pos = tags)
  
  #individual tagging are extracted to check the efficiency of tagging
  
  persn <- entities(text_doc, kind = "person")   
  
  loction <- entities(text_doc, kind = "location")
 
  organiz <- entities(text_doc, kind = "organization")

  dte <- entities(text_doc, kind = "date")

  all_txt <- entities(text_doc)
    
  #creates a tibble with 3 cols: book_title, chapter, word
  words <- tibble(title = book_title, chapter=i, entity = doc_pos$word, tags = doc_pos$pos)
  
  book <- rbind(book, words) # add rows to the book tibble
  
}

#Extracting only noun POS tags from the complete list of POS tags
book$type <- case_when(
  book$tags == "NN" ~ "noun",
  book$tags == "NNS" ~ "noun",
  book$tags == "NNP" ~ "noun",
  book$tags == "NNPS" ~ "noun",
  TRUE ~ "other"
)

book_noun <- book %>% filter(type == "noun")

###################################################

################### tf-idf ########################

book_words_count <- book_noun %>%
        count(title, chapter, entity, sort = TRUE) %>%
        ungroup()

sum_word_count <- book_words_count %>%
        group_by(chapter) %>%
        summarise(total = sum(n))

book_words_count <- full_join(book_words_count, sum_word_count)

freq_by_rank <- book_words_count %>%
        group_by(chapter) %>%
        mutate(rank = row_number(),
               `term freq` = n / total)

book_words_count <- book_words_count %>%
        bind_tf_idf(entity, chapter, n)

###################################################
```

#### **Analysis**

The plot in fig 1.1 shows the important noun words per chapter extracted by NER technique and ranked from highly to least important words as per the tf-idf approach. These words are the ones which have high contextual meaning in the respective chapters of the book. By having a superficial look at the words in the plot below, we can make out that the novel is about court room drama revolving around a crime, and the central character could be 'Franz'.

``` r
################### tf-idf plot ########################

fig1.1 <- book_words_count %>%
          group_by(chapter) %>%
          top_n(10, wt = tf_idf) %>%
          ungroup() %>%
          mutate(entity = drlib::reorder_within(entity, tf_idf, chapter)) %>%
          mutate(chapter = factor(chapter, levels = c('1','2','3','4','5','6'))) %>%
          ggplot(aes(entity, tf_idf, fill = chapter)) +
          scale_fill_brewer(palette = 'Set1')+
          geom_bar(stat = "identity", alpha = .8, show.legend = FALSE) +
          labs(title = "Figure 1.1 | Highest tf-idf words",
             subtitle = "Coloured by Chapters",
             x = NULL, y = "tf_idf score",caption = "The Trial | Chapter 1 to 6") +
          drlib::scale_x_reordered() +
          facet_wrap(~chapter, ncol = 3, scales = "free") +
          coord_flip()

fig1.1
```

<img src="R_Assign_New_files/figure-markdown_github/tdf_idf-1.png" style="display: block; margin: auto;" />

``` r
#[2] drlib::reorder_within(entity, tf_idf, chapter)

#####################################################
```

In the below block, pair-wise correlation was performed. The general idea of correlation coefficient is to select words which had appeared at least 11 times in the text. The reduction in the filter of occurrence increases the number of nodes being selected, hence the resulting graph becomes populated with less important terms. If the filter of occurrence number is increased, then a small number of nodes are selected, and results in a less meaningful graph.

Similarly, if the correlation score is set too high, then the resulting graph was broken into many smaller communities with less overall meaning. A low correlation score resulted in a very large general meaning graph with low specific meaning.

A selection of '11' word appearance and correlation score of '0.60' was found optimum for my correlation graph.

In the correlation graph measure, multiple edges (A -&gt; B and B -&gt; A) were removed from the graph, and in order to show a neat and self-explanatory graph, communities were detected using louvain algorithm. These communities were further segregated using different colors representing each community.

``` r
################### word correlation ########################

word_cor <- book_noun %>%
   group_by(entity) %>%
   filter(n() >= 11) %>% 
   pairwise_cor(item=entity, feature=chapter) %>% 
   filter(!is.na(correlation), correlation >= 0.60) 


correlation_graph <-as_tbl_graph(word_cor,directed=FALSE) 


correlation_graph_initial <- correlation_graph %>% 
                             activate(edges) %>%
                             filter(!edge_is_multiple()) %>%
                             activate(nodes) %>%
                             mutate(commnty = group_louvain())

#[3] correlation_graph %>% activate(edges) %>% 
#filter(!edge_is_multiple()) %>% activate(nodes) %>%

############################################################
```

As per figure 1.2, we can observe that strong correlation (darker edges) exists within the communities and not between the communities.

``` r
################### correlation plot ########################

fig1.2 <- ggraph(correlation_graph_initial, layout = "kk") +
  geom_edge_density(aes(fill = correlation)) +
  geom_edge_link(aes(edge_alpha = correlation)) + 
  geom_node_point(aes(color = factor(commnty)), size = 5) +
  geom_node_text(aes(label = name), size = 4, 
                 repel = TRUE,segment.color = "#860B62") +
  scale_color_brewer(palette = "Dark2") +
  theme_void() +
  labs(title = "Figure 1.2 | Correlation Graph",
       subtitle = "Coloured By Communities", 
       color = "Communities",caption = "The Trial | Chapter 1 to 6") 

fig1.2
```

<img src="R_Assign_New_files/figure-markdown_github/corltion-1.png" style="display: block; margin: auto;" />

``` r
############################################################
```

### **2. Calculating Graph Measures**

In the below block, graph measurements such as centrality, diameter, radius and eccentricity were calculated for the correlation graph. It was challenging to show the measures like diameter and radius in the graph. For this, the graph measurements were stored in a tibble, hence, helping them to appear in the plot. This technique was obtained from communities' tutorial.

``` r
################### Graph Measure ########################

graph_values <- correlation_graph %>%
                activate(edges) %>%
                filter(!edge_is_multiple()) %>%
                activate(nodes) %>%
                mutate(
                  centrality = centrality_degree(),
                  diameter = graph_diameter(),
                  radius = graph_radius(),
                  eccntrcty = node_eccentricity()
                     )


graph_specifction <- as.tibble(graph_values)
  
############################################################
```

As per the centrality plot below, we can see that the node label transparency, size and color depend on the centrality measure of the graph. The nodes at the center of the plot are darker in color and are largest.

Therefore, nodes with labels 'advantage', 'trial', 'gentleman', 'affair', 'proceedings', 'bed' and 'others' have the highest centrality or importance, i.e., they have larger number of connections with other nodes and are connected to the gist of the overall murder mystery.

As we move our observational range to the periphery of the graph, we can see nodes with low centrality (light node color, smaller size and transparent labels.), i.e., words which have importance in a scene or chapter but are not that important in the context of the overall plot.

``` r
################### Centrality Plot ########################

fig2.1 <- ggraph(graph_values, layout = "fr") +
  geom_edge_link(aes(alpha = 0.2),edge_width=0.05) +
  geom_node_point(aes(size = centrality, color = centrality)) +
  geom_node_text(aes(label = name, alpha = centrality), size = 4,repel = TRUE, segment.color = "#860B62") + 
  scale_color_gradient(low = '#05D9F6', high = '#5011D1') +
  theme_void() +
  labs(title = "Figure 2.1 | Centrality Measure", color = "Centrality",
       caption = "The Trial | Chapter 1 to 6") 

fig2.1
```

<img src="R_Assign_New_files/figure-markdown_github/centrality_plot-1.png" style="display: block; margin: auto;" />

``` r
############################################################
```

The other graph in this series represents the eccentricity. In context of a network, it represents the distance between one node and the other node in that network. The diameter in the sub-title represents the maximum eccentricity and the radius represents the minimum eccentricity.

We can see that the node label transparency, size and color depend on the eccentricity measure of the graph. The nodes at the edges of the plot are darker in color and are the largest in size.

Therefore, nodes with labels 'Montag', 'Lanz', 'home', 'mind', 'window', 'candle' and 'bit' have the highest eccentricity, i.e., they have further away from the center network, whereas the nodes near the center have low eccentricity because they are the nearest to the center network and are at equal distance from any other node in the same network.

``` r
################### Eccentricity Plot ########################

fig2.2 <- ggraph(graph_values, layout = "fr") +
  geom_edge_link(aes(alpha = 0.2),edge_width=0.05) +
  geom_node_point(aes(size = eccntrcty, color = eccntrcty)) +
  geom_node_text(aes(label = name, alpha = eccntrcty), size = 4, repel = TRUE, segment.color = "#860B62") + 
  scale_color_gradient(low = '#F2C9E1', high = '#B2076C') +
  theme_void() +
  labs(title = "Figure 2.2 | Eccentricity Measure",
       subtitle = paste(paste("Diameter = ", sample(graph_specifction$diameter, 1)) , " || Radius = ", sample(graph_specifction$radius, 1)), color = "Eccentricity",caption = "The Trial | Chapter 1 to 6") 

fig2.2
```

<img src="R_Assign_New_files/figure-markdown_github/eccentricity_plot-1.png" style="display: block; margin: auto;" />

``` r
############################################################
```

### **3. Community Detection**

As per the block below, two community detection algorithms are applied to find regions in the graph which have strong internal connections and weak connections with other regions in the same graph. The two community detection algorithms used are 'Louvain' and 'Fast-Greedy'.

The purpose of finding communities in the graph is to extract more meaning. As per the objective of the assignment, these two communities were compared based on graph measures, and the relevance of the sub-graphs.

``` r
################### Community Detection ########################

multiple_grph <- correlation_graph %>% 
                 activate(edges) %>%
                 filter(!edge_is_multiple()) %>%
                 activate(nodes) %>%
                 mutate(
                   luvian = group_louvain(),
                   greedy = group_fast_greedy()
                     )

multiple_grph_values <- as.tibble(multiple_grph)

################################################################
```

#### **3.1 Louvain**

1.  Louvain algorithm is an agglomerative clustering approach which uses modularity maximisation to find smaller communities and joins them to form large communities.

2.  A total of 4 communities were detected with this algorithm.

``` r
################### Community Detection - Louvain ########################

fig3.1 <- ggraph(multiple_grph, layout = "kk") +
  geom_edge_link(aes(alpha = 0.2), show.legend = FALSE) + 
  geom_node_point(aes(color = as.factor(luvian)), size = 5) +
  geom_node_text(aes(label = name), size = 4, repel = TRUE, segment.color = "#860B62") + 
  scale_color_brewer(palette = "Set1") +
  theme_void() +
  labs(title = "Figure 3.1 | Louvain Community Detection",
       subtitle = paste("Communities = ", max(multiple_grph_values$luvian)), color = "Community",caption = "The Trial | Chapter 1 to 6") 

fig3.1
```

<img src="R_Assign_New_files/figure-markdown_github/louvain-1.png" style="display: block; margin: auto;" />

``` r
##########################################################################
```

As there is no function to cut larger graphs into smaller graphs in ggraph, I had to use a loop to cut the graphs and store the respective values in the list created for this purpose. In the block below, the centrality and eccentricity measures are obtained for all the four sub-communities detected by lovain algorithm.

Since it was not possible to analyse each community in the block above, I decided to cut the graph into separate sub-graphs to study the measures of each of the sub-graphs.

``` r
################### Louvain - Sub-Graphs ##########################

sample <- multiple_grph

sample <- sample %>% 
           activate(nodes) 

sample_measure <- sample %>% as.tibble()

number_comm <- max(sample_measure$luvian)

sep_community <- list()
graph_cent <- list()
graph_eccent <- list()

i <- 1

while(i <= number_comm){
  
  sep_community[[i]] <- filter(sample,luvian == i)
  
  sep_community_meas <- sep_community[[i]] %>% 
           activate(nodes) %>%
              mutate(centrality = centrality_degree(),
                     diameter = graph_diameter(),
                     radius = graph_radius(),
                     eccntrcty = node_eccentricity()
                     )

  sep_community_measr_spec <- as.tibble(sep_community_meas)

  graph_cent[[i]] <- ggraph(sep_community_meas, layout = "fr") +
  geom_edge_link(aes(alpha = 0.2),edge_width=0.05) +
  geom_node_point(aes(size = centrality, color = centrality)) +
  geom_node_text(aes(label = name), size = 4,repel = TRUE, segment.color = "#860B62") + 
  scale_color_gradient(low = '#05D9F6', high = '#5011D1') +
  theme_void() +
  labs(title = paste("Figure 3.1.1.",i," | Centrality Measure"),
       color = "Centrality",caption = "The Trial | Chapter 1 to 6") +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = 'bold'))
  
  graph_eccent[[i]] <-  ggraph(sep_community_meas, layout = "fr") +
  geom_edge_link(aes(alpha = 0.2),edge_width=0.05) +
  geom_node_point(aes(size = eccntrcty, color = eccntrcty)) +
  geom_node_text(aes(label = name), size = 4, repel = TRUE, segment.color = "#860B62") + 
  scale_color_gradient(low = '#F2C9E1', high = '#B2076C') +
  theme_void() +
  labs(title = paste("Figure 3.1.2.",i," | Eccentricity Measure"),
       subtitle = paste("Diameter = ",sample(sep_community_measr_spec$diameter, 1), 
                        "|| Radius = ", sample(sep_community_measr_spec$radius, 1)), 
       color = "Eccentricity",caption = "The Trial |  Chapter 1 to 6") +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = 'bold'))
  
  i = i+1

}

##########################################################################
```

``` r
################### Louvain - Centrality Graphs ##########################

graph_cent[[1]]
```

<img src="R_Assign_New_files/figure-markdown_github/louvain_sub_graph_plots_cent-1.png" style="display: block; margin: auto;" />

``` r
graph_cent[[2]]
```

<img src="R_Assign_New_files/figure-markdown_github/louvain_sub_graph_plots_cent-2.png" style="display: block; margin: auto;" />

``` r
graph_cent[[3]]
```

<img src="R_Assign_New_files/figure-markdown_github/louvain_sub_graph_plots_cent-3.png" style="display: block; margin: auto;" />

``` r
graph_cent[[4]]
```

<img src="R_Assign_New_files/figure-markdown_github/louvain_sub_graph_plots_cent-4.png" style="display: block; margin: auto;" />

``` r
##########################################################################
```

#### **Analysis**

In the centrality graphs for all the communities above, we can see that the node size and color depend on the centrality measure of the sub-graphs. Each community shares a unique meaning related to it in context of centrality. The findings from the community graph are as follows:

1.  In figure 3.1.1.1, majority of the nodes are highly connected to each other, and therefore have high centrality or importance in the over-all plot. The words like table, case, night, chance, reason, affair, gentlemen, advantage, someone, wall, proceedings, fact, voice and proceedings clearly indicate towards a discussion in the court. The discussion might be related to a murder.
2.  In figure 3.1.1.2, almost all the noun entities show a very centrality among themselves. The worlds like judge, candle, court, front, gentleman, bank, order, one, hold, men, floor and arms indicate an event happening in a court where it is discussed about how an arm was found in a building by the officials to the court and the judge.
3.  In figure 3.1.1.3, the words like breakfast, morning, photographs, supervisor, captain, policemen, and Lanz have high importance towards the overall mystery. These words clearly indicate a scene where some photographs were shown to Mr. and Mrs. Grubach by captain police-man of Willem. These photographs were related to franz, and he is in police captivity.
4.  In figure 3.1.1.4, the words Miss, friend, Burstner, house, hallway, Josef, lawyer, uncle, usher, student and information-giver contribute most to the overall plot of the book. It shares information about an event where Miss Burstner's house hallway something happened which was shared by Josef who was the layer, and it had to do something with Uncle usher. The information was shared by the student.

``` r
################### Louvain - Eccentricity Graphs ##########################

graph_eccent[[1]]
```

<img src="R_Assign_New_files/figure-markdown_github/louvain_sub_graph_plots_eccent-1.png" style="display: block; margin: auto;" />

``` r
graph_eccent[[2]]
```

<img src="R_Assign_New_files/figure-markdown_github/louvain_sub_graph_plots_eccent-2.png" style="display: block; margin: auto;" />

``` r
graph_eccent[[3]]
```

<img src="R_Assign_New_files/figure-markdown_github/louvain_sub_graph_plots_eccent-3.png" style="display: block; margin: auto;" />

``` r
graph_eccent[[4]]
```

<img src="R_Assign_New_files/figure-markdown_github/louvain_sub_graph_plots_eccent-4.png" style="display: block; margin: auto;" />

``` r
############################################################################
```

#### **Analysis**

In the eccentricity graphs for all the communities above, we can see that the node size and color depend on the eccentricity measure of the sub-graphs. Each community shares a unique meaning related to it in context of eccentricity. The findings from the community graphs are as follows:

1.  In figure 3.1.2.1, the words woman, arrest and sign have high eccentricity as they are very distant from most of the words of their own network. It shows that these words add little meaning to the overall contextual meaning of this community.The remaining nodes are very close to each other within this same network as all of them have the same size.
2.  In figure 3.1.2.2, the words moment, men, judge, court, offices, mind, home and window have high eccentricity as they are very distant from most of the words of their own network. It shows that these words add little meaning to the overall contextual meaning of this community. The remaining nodes are very close to each other within this same network as all of them have the same size.
3.  In figure 3.1.2.3, the words Franz, Montag and Sunday have high eccentricity as they are very distant from most of the words of their own network. It shows that these words add little meaning to the overall contextual meaning of this community. The words like question, clothes, job, book, policeman, Willem belong to the second category because they are of medium size, and the remaining words which are of the smallest size are closest to each other and add contextual meaning to the overall plot.
4.  In figure 3.1.2.4, house, whip-man and evening have the highest eccentricity because of very low contextual meaning as compared to the remaining words of the same community.

#### **3.2. Fast Greedy**

1.  Fast Greedy algorithm assigns each node to the community of its own, and itteratively joins pairs of communities, if it leads to an increase in modularity.

2.  A total of 4 communities were detected with this algorithm.

``` r
################### Community Detection - Fast Greedy ########################

fig3.2 <- ggraph(multiple_grph, layout = "kk") +
  geom_edge_link(aes(alpha = 0.2), show.legend = FALSE) + 
  geom_node_point(aes(color = as.factor(greedy)), size = 5) +
  geom_node_text(aes(label = name), size = 4, repel = TRUE, segment.color = "#860B62") + 
  scale_color_brewer(palette = "Set1") +
  theme_void() +
  labs(title = "Figure 3.2 | Fast-Greedy Community Detection",
       subtitle = paste("Communities = ", max(multiple_grph_values$greedy)), color = "Community",caption = "The Trial | Chapter 1 to 6") 
  
fig3.2
```

<img src="R_Assign_New_files/figure-markdown_github/fast_greedy-1.png" style="display: block; margin: auto;" />

``` r
##########################################################################
```

In the block below, the centrality and eccentricity measures are obtained for all the four sub-communities detected by fast-greedy algorithm.

Since it was not possible to analyse each community in the block above, I decided to cut the graph into separate sub-graphs to study the measures of each of the sub-graphs.

``` r
################### Fast_Greedy - Sub-Graphs ##########################

sample2 <- multiple_grph

sample2 <- sample %>% 
           activate(nodes) 

sample_measure2 <- sample %>% as.tibble()

number_comm2 <- max(sample_measure$greedy)

sep_community2 <- list()
graph_cent2 <- list()
graph_eccent2 <- list()

i <- 1

while(i <= number_comm){
  
  sep_community2[[i]] <- filter(sample,greedy == i)
  
  sep_community_meas2 <- sep_community2[[i]] %>% 
           activate(nodes) %>%
              mutate(centrality = centrality_degree(),
                     diameter = graph_diameter(),
                     radius = graph_radius(),
                     eccntrcty = node_eccentricity()
                     )

  sep_community_measr_spec2 <- as.tibble(sep_community_meas2)

  graph_cent2[[i]] <- ggraph(sep_community_meas2, layout = "fr") +
  geom_edge_link(aes(alpha = 0.2),edge_width=0.05) +
  geom_node_point(aes(size = centrality, color = centrality)) +
  geom_node_text(aes(label = name), size = 4,repel = TRUE, segment.color = "#860B62") + 
  scale_color_gradient(low = '#05D9F6', high = '#5011D1') +
  theme_void() +
  labs(title = paste("Figure 3.2.1.",i," | Centrality Measure"),
       color = "Centrality",caption = "The Trial | Chapter 1 to 6") +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = 'bold'))
  
  graph_eccent2[[i]] <-  ggraph(sep_community_meas2, layout = "fr") +
  geom_edge_link(aes(alpha = 0.2),edge_width=0.05) +
  geom_node_point(aes(size = eccntrcty, color = eccntrcty)) +
  geom_node_text(aes(label = name), size = 4, repel = TRUE, segment.color = "#860B62") + 
  scale_color_gradient(low = '#F2C9E1', high = '#B2076C') +
  theme_void() +
  labs(title = paste("Figure 3.2.2.",i," | Eccentricity Measure"),
       subtitle = paste("Diameter = ",sample(sep_community_measr_spec2$diameter, 1), 
                        "|| Radius = ", sample(sep_community_measr_spec2$radius, 1)), 
       color = "Eccentricity",caption = "The Trial |  Chapter 1 to 6") +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = 'bold'))
  
  i = i+1

}

##########################################################################
```

``` r
################### Louvain - Centrality Graphs ##########################

graph_cent2[[1]]
```

<img src="R_Assign_New_files/figure-markdown_github/greedy_sub_graph_plots_cent-1.png" style="display: block; margin: auto;" />

``` r
graph_cent2[[2]]
```

<img src="R_Assign_New_files/figure-markdown_github/greedy_sub_graph_plots_cent-2.png" style="display: block; margin: auto;" />

``` r
graph_cent2[[3]]
```

<img src="R_Assign_New_files/figure-markdown_github/greedy_sub_graph_plots_cent-3.png" style="display: block; margin: auto;" />

``` r
graph_cent2[[4]]
```

<img src="R_Assign_New_files/figure-markdown_github/greedy_sub_graph_plots_cent-4.png" style="display: block; margin: auto;" />

``` r
##########################################################################
```

#### **Analysis**

In the centrality graphs for all the communities above, we can see that the node size and color depend on the centrality measure of the sub-graphs. Each community shares a unique meaning related to it in context of centrality. The findings from the community graph are as follows:

1.  In figure 3.2.1.1, majority of the nodes are highly connected to each other, and therefore have high centrality or importance in the over-all plot. The words like officials, one, order, arm, court, judge, bank, hold, building, floor, front, moment, man, everyone and candle clearly indicate towards a discussion infront of judge. The discussion might be related to a scene in which official pointed to a building floor where they found arm in the light of candle.
2.  In figure 3.2.1.2, suprisingly almost all the noun entities show a very high centrality (22) among themselves, i.e, they are important for the central plot of the book. The worlds like words, someone, others, reason, night, fingers, fact, voice, proceedings, chance, street and help indicate an event where trial happened in which something was discussed about a voice heard on the street at night asking for help, but someone thought that it was not his affair.
3.  In figure 3.2.1.3, the words like breakfast, morning, photographs, supervisor, captain, policemen, and Lanz are closer to the central meaning of the book becuase of high centrality. These words clearly indicate a scene where some photographs and book were shown to Mr. and Mrs. Grubach by police captain of Willem and supervisor. These photographs were related to franz, and he is in police captivity.
4.  In figure 3.2.1.4, the words like Joseph, lawyer, deputy and director are contextually important to the central core meaning of the book. It shares information about an event where something happened in Miss Burstner's house's hallway which was shared by Josef who was the layer through pictures, and it had to do something with Uncle Leni.

``` r
################### Louvain - Eccentricity Graphs ##########################

graph_eccent2[[1]]
```

<img src="R_Assign_New_files/figure-markdown_github/greedy_sub_graph_plots_eccent-1.png" style="display: block; margin: auto;" />

``` r
graph_eccent2[[2]]
```

<img src="R_Assign_New_files/figure-markdown_github/greedy_sub_graph_plots_eccent-2.png" style="display: block; margin: auto;" />

``` r
graph_eccent2[[3]]
```

<img src="R_Assign_New_files/figure-markdown_github/greedy_sub_graph_plots_eccent-3.png" style="display: block; margin: auto;" />

``` r
graph_eccent2[[4]]
```

<img src="R_Assign_New_files/figure-markdown_github/greedy_sub_graph_plots_eccent-4.png" style="display: block; margin: auto;" />

``` r
############################################################################
```

#### **Analysis**

In the eccentricity graphs for all the communities above, we can see that the node size and color depend on the eccentricity measure of the sub-graphs. Each community shares a unique meaning related to it in context of eccentricity. The findings from the community graphs are as follows:

1.  In figure 3.2.2.1, the words air, moment, men, window, home and mind have high eccentricities as they are very distant from most of the words of their own network. It shows that these words add little meaning to the overall contextual meaning of this community.The remaining nodes are very close to each other within this same network as all of them have the same size, but are father away from the central meaning of the whole book.
2.  In figure 3.2.2.2, the words moment, men, judge, court, offices, mind, home, and window have low eccentricity (1) as they are closest from most of the words of their own network. It shows that these words are closest to the overall contextual meaning of this community.
3.  In figure 3.2.2.3, the words policemen, Montag, Willem, woman, question, job, clothes, arrest and Sunday have high eccentricity as they are very distant from most of the words of their own network. It shows that these words add little meaning to the overall contextual meaning of this community. The words like supervisor, Lanz, house and photographs belong to the second category because they are of medium size, and the remaining words which are of the smallest size are closest to each other and add contextual meaning to the overall plot.
4.  In figure 3.2.2.4, halway, miss, friend and sign have the highest eccentricity because of very low contextual meaning as compared to the remaining words of the same community.

#### **Result:**

Out of the two community detection algorithm, the Louvain is found useful in providing meaning-full communities adding more meaning to the core plot of the book.

### **4. Bigram Approach**

The next NLP technique used to explore the book as a network was by using bi-gram model. The approach was to extract word entities appearing in multiple occurrences so as to have a meaningful insight into the core plot of the book.

In the below block, the chapter wise text files of the book are processed using Bi-gram techniques to form a correlation graph between multiple occurence entities. The whole process is as follows:

1.  Read all the text from the text files using loop and convert them to string.
2.  Remove all the unnecessary symbols and spaces using gsub.
3.  After applying the NLP techniques, the bigram tokenize words are extracted in tibbles.
4.  The extracted entities are processed to rank the bigram words on the basis of tf-idf score.

``` r
########################## Read in Book ###########################

book_title = "TheTrial"
chapters = 1:6
chapter_stem = "TheTrial"
ext <-".txt"
folder<- "E:/New Volume/Academic/NUIG_College/WEB_NETWORK_SCIENCE/Assignment_4_5/book_data/"

###################################################################

################### Bi-gram NLP Technique ########################

book <- tibble()

#read in each chapter
for(i in chapters){
  
  chapter <-paste0(folder,chapter_stem,i,ext)
  
  raw<-readChar(chapter, file.info(chapter)$size)

  chapter_text <- raw %>%
        gsub("[\r\n]+", " ", .) %>%
        gsub('^"',"",.) %>%
        gsub('"$',"",.)

#creates a tibble with 3 cols: book_title, chapter, word
words <- tibble(title = book_title, chapter=i, text = chapter_text) %>%
        unnest_tokens(bigram, text, token = "ngrams", n = 2)

book <- rbind(book, words) # add rows to the book tibble

}

###################################################

################### tf-idf ########################

book %>%  count(bigram, sort = TRUE)

book %>% separate(bigram, c("word1", "word2"), sep = " ") %>%
         filter(!word1 %in% stop_words$word,
               !word2 %in% stop_words$word) %>%
        count(word1, word2, sort = TRUE)

bigram_tf_idf <- book %>%
        count(title, chapter, bigram, sort = TRUE) %>%
        bind_tf_idf(bigram, chapter, n) %>%
        arrange(desc(tf_idf))

###################################################
```

#### **Analysis**

The plot in fig 4.1 shows the important bi-gram words per chapter extracted by bigram technique and ranked from highly to least important words as per the tf-idf approach. These multi-words are the ones which have high contextual meaning in the respective chapters of the book. By having a superficial look at the words in the plot below, we can make out that the novel is about court room drama revolving around a murder.

``` r
################### tf-idf plot ########################


fig4.1 <- bigram_tf_idf %>%
          group_by(chapter) %>%
          top_n(8, wt = tf_idf) %>%
          ungroup() %>%
          mutate(entity = drlib::reorder_within(bigram, tf_idf, chapter)) %>%
          mutate(chapter = factor(chapter, levels = c('1','2','3','4','5','6'))) %>%
          ggplot(aes(entity, tf_idf, fill = chapter)) +
          scale_fill_brewer(palette = 'Dark2')+
          geom_bar(stat = "identity", alpha = .8, show.legend = FALSE) +
          labs(title = "Figure 4.1 | Highest tf-idf Bigram words",
             subtitle = "Coloured by Chapters",
             x = NULL, y = "tf_idf score",caption = "The Trial | Chapter 1 to 6") +
          drlib::scale_x_reordered() +
          facet_wrap(~chapter, ncol = 2, scales = "free") +
          coord_flip()

fig4.1
```

<img src="R_Assign_New_files/figure-markdown_github/Bigram_plot-1.png" style="display: block; margin: auto;" />

``` r
#######################################################
```

In the below block, pair-wise correlation was performed. The general idea of correlation coefficient is to select words which had appeared at least 22 times in the text. The reduction in the filter of occurrence increases the number of nodes being selected, hence the resulting graph becomes populated with less important terms. If the filter of occurrence number is increased, then a small number of nodes are selected, and results in a less meaningful graph.

Similarly, if the correlation score is set too high, then the resulting graph was broken into many smaller communities with less overall meaning. A low correlation score resulted in a very large general meaning graph with low specific meaning.

A selection of '22' bi-words appearance and correlation score of '0.60' was found optimum for my correlation graph.

In the correlation graph measure, multiple edges (A -&gt; B and B -&gt; A) were removed from the graph, and in order to show a neat and self-explanatory graph, communities were detected using louvain algorithm. These communities were further segregated using different colors representing each community.

``` r
################### Bigram word correlation ########################

word_cor_bi <- book %>%
   group_by(bigram) %>%
   filter(n() >= 22) %>% 
   pairwise_cor(item=bigram, feature=chapter) %>% 
   filter(!is.na(correlation), correlation >= 0.60) 


correlation_graph_bi <-as_tbl_graph(word_cor_bi,directed=FALSE) 


graph_values_bi <- correlation_graph_bi %>% 
                   activate(edges) %>%
                   filter(!edge_is_multiple()) %>%
                   activate(nodes) %>%
                   mutate(
                     centrality = centrality_degree(),
                     diameter = graph_diameter(),
                     radius = graph_radius(),
                     eccntrcty = node_eccentricity(),
                     luvian_bi = group_louvain()
                     )


graph_specifction_bi <- as.tibble(graph_values_bi)

###################################################################
```

As per figure 4.2, we can observe that strong correlation (darker edges) exists within the communities and not between the communities.

``` r
################### Bi-correlation plot ########################

fig4.2 <- ggraph(graph_values_bi, layout = "kk") +
  geom_edge_density(aes(fill = correlation)) +
  geom_edge_link(aes(edge_alpha = correlation)) + 
  geom_node_point(aes(color = factor(luvian_bi)), size = 5) +
  geom_node_text(aes(label = name), size = 4, repel = TRUE,segment.color = "#860B62") +
  scale_color_brewer(palette = "Dark2") +
  theme_void() +
  labs(title = "Figure 4.2 | Bi-gram Correlation Graph",
       subtitle = "Coloured By Communities", color = "Communities",caption = "The Trial | Chapter 1 to 6")  

fig4.2
```

<img src="R_Assign_New_files/figure-markdown_github/bi_corltion_graph-1.png" style="display: block; margin: auto;" />

``` r
###################################################################
```

#### **Analysis**

The graph produced using bi-gram helps in extracting meaning easily than the individual word correlation graphs without the use of communities.

As per the graph above, we can see that not many meaningful words have appeared in the correlation graph. The words like 'k s' make not sense in explaning the overall meaning. I think, bigram could have helped in providing meaningfull results with the use of POS tags.

For now, NER technique was helpful in providing meaningful results in the context of this book.

### **5. Book By Section**

In this section, I have tried to explain the graphs produced by different sections of the book by comparing the principle relationships extracted from the first half the book with those extracted from the second half of the book.

In the below block, I have extracted the text from different sections in separate variables using simple tokenize method.

``` r
########################## Read in Book ###########################

book_title = "TheTrial"
chapters = 1:10
chapter_stem = "TheTrial"
ext <-".txt"
folder<- "E:/New Volume/Academic/NUIG_College/WEB_NETWORK_SCIENCE/Assignment_4_5/book_data/"

###################################################################

################### Word Tokenize Technique #######################

book_sec_1 <- tibble()
book_sec_2 <- tibble()

#read in each chapter
for(i in chapters){
  
  chapter <-paste0(folder,chapter_stem,i,ext)
  
  raw<-readChar(chapter, file.info(chapter)$size)

  chapter_text <- raw %>%
        gsub("[\r\n]+", " ", .) %>%
        gsub('^"',"",.) %>%
        gsub('"$',"",.)


  if(i < 5){
    
    #creates a tibble with 3 cols: book_title, chapter, word
    words_sec_1 <- tibble(title = book_title, chapter=i, text = chapter_text) %>%
                   unnest_tokens(word, text) %>% # tokenise the text
                   filter(!word %in% stop_words$word) # remove stop words # remove stop words
  
    book_sec_1 <- rbind(book_sec_1, words_sec_1) # add rows to the book tibble
    
  } else{
        
    #creates a tibble with 3 cols: book_title, chapter, word
    words_sec_2 <- tibble(title = book_title, chapter=i, text = chapter_text) %>%
                   unnest_tokens(word, text) %>% # tokenise the text
                   filter(!word %in% stop_words$word) # remove stop words
  
    book_sec_2 <- rbind(book_sec_2, words_sec_2) # add rows to the book tibble
    
  }

}

###################################################################
```

### **5.1 First Half of the Book**

In the below block, the correlation between words in the first half of the book was extracted and coloured using luvain algorithm.

Interestingly, a community size of '8' is obtained which is double the community size obtained when 6 chapters were taken earlier.

``` r
################### word correlation #########################

word_cor_sec_1 <- book_sec_1 %>% 
                  group_by(word) %>%
                  filter(n() >= 11) %>% 
                  pairwise_cor(item=word, feature=chapter) %>% 
                  filter(!is.na(correlation), correlation >= 0.60) 

correlation_graph_sec_1 <-as_tbl_graph(word_cor_sec_1,directed=FALSE) 


graph_values_sec_1 <- correlation_graph_sec_1 %>% 
                      activate(edges) %>%
                      filter(!edge_is_multiple()) %>%
                      activate(nodes) %>%
                      mutate(
                        centrality = centrality_degree(),
                        diameter = graph_diameter(),
                        radius = graph_radius(),
                        eccntrcty = node_eccentricity(),
                        luvian = group_louvain()
                        )

graph_specifction_sec_1 <- as.tibble(graph_values_sec_1)

############################################################

################### correlation plot ########################

fig5.1 <- ggraph(graph_values_sec_1, layout = "fr") +
  geom_edge_density(aes(fill = correlation)) +
  geom_edge_link(aes(edge_alpha = correlation)) + 
  geom_node_point(aes(color = factor(luvian)), size = 5) +
  geom_node_text(aes(label = name), size = 4, repel = TRUE,segment.color = "#860B62") +
  scale_color_brewer(palette = "Dark2") +
  theme_void() +
  labs(title = "Figure 5.1 | Correlation Graph",
       subtitle = "Coloured By Communities | Book First Half Section", color = "Communities",caption = "The Trial | Chapter 1 to 5")  

fig5.1
```

<img src="R_Assign_New_files/figure-markdown_github/corltion_graph_sec_1-1.png" style="display: block; margin: auto;" />

``` r
############################################################
```

``` r
################### First Section - Sub-Graphs ##########################

sample <- graph_values_sec_1

sample <- sample %>% 
           activate(nodes) 

sample_measure <- as.tibble(sample)

number_comm <- max(sample_measure$luvian)

sep_community5 <- list()
graph_cent5 <- list()
graph_eccent5 <- list()

i <- 1

while(i <= number_comm){
  
  
  sep_community5[[i]] <- filter(sample,luvian == i)
  
  sep_community_meas5 <- sep_community5[[i]] %>% 
           activate(nodes) %>%
              mutate(centrality = centrality_degree(),
                     diameter = graph_diameter(),
                     radius = graph_radius(),
                     eccntrcty = node_eccentricity()
                     )

  sep_community_measr_spec5 <- as.tibble(sep_community_meas5)

  graph_cent5[[i]] <- ggraph(sep_community_meas5, layout = "fr") +
  geom_edge_link(aes(alpha = 0.2),edge_width=0.05) +
  geom_node_point(aes(size = centrality, color = centrality)) +
  geom_node_text(aes(label = name), size = 4,repel = TRUE, segment.color = "#860B62") + 
  scale_color_gradient(low = '#05D9F6', high = '#5011D1') +
  theme_void() +
  labs(title = paste("Figure 5.1.1.",i," | Centrality Measure"),
       color = "Centrality",caption = "The Trial | Chapter 1 to 6") +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = 'bold'))
  
  graph_eccent5[[i]] <-  ggraph(sep_community_meas5, layout = "fr") +
  geom_edge_link(aes(alpha = 0.2),edge_width=0.05) +
  geom_node_point(aes(size = eccntrcty, color = eccntrcty)) +
  geom_node_text(aes(label = name), size = 4, repel = TRUE, segment.color = "#860B62") + 
  scale_color_gradient(low = '#F2C9E1', high = '#B2076C') +
  theme_void() +
  labs(title = paste("Figure 5.1.2.",i," | Eccentricity Measure"),
       subtitle = paste("Diameter = ",sample(sep_community_measr_spec5$diameter, 1), 
                        "|| Radius = ", sample(sep_community_measr_spec5$radius, 1)), 
       color = "Eccentricity",caption = "The Trial |  Chapter 1 to 6") +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = 'bold')) 
  
  i = i+1

}

#######################################################################
```

``` r
############ Louvain - Centrality Graphs - First Section #################

graph_cent5[[1]]
```

<img src="R_Assign_New_files/figure-markdown_github/first_sec_sep_comm_1-1.png" style="display: block; margin: auto;" />

``` r
graph_cent5[[2]]
```

<img src="R_Assign_New_files/figure-markdown_github/first_sec_sep_comm_1-2.png" style="display: block; margin: auto;" />

``` r
graph_cent5[[3]]
```

<img src="R_Assign_New_files/figure-markdown_github/first_sec_sep_comm_1-3.png" style="display: block; margin: auto;" />

``` r
graph_cent5[[4]]
```

<img src="R_Assign_New_files/figure-markdown_github/first_sec_sep_comm_1-4.png" style="display: block; margin: auto;" />

``` r
graph_cent5[[5]]
```

<img src="R_Assign_New_files/figure-markdown_github/first_sec_sep_comm_1-5.png" style="display: block; margin: auto;" />

``` r
graph_cent5[[6]]
```

<img src="R_Assign_New_files/figure-markdown_github/first_sec_sep_comm_1-6.png" style="display: block; margin: auto;" />

``` r
graph_cent5[[7]]
```

<img src="R_Assign_New_files/figure-markdown_github/first_sec_sep_comm_1-7.png" style="display: block; margin: auto;" />

``` r
graph_cent5[[8]]
```

<img src="R_Assign_New_files/figure-markdown_github/first_sec_sep_comm_1-8.png" style="display: block; margin: auto;" />

``` r
############################################################################
```

``` r
############ Louvain - Eccentricity Graphs - First Section #################

graph_eccent5[[1]]
```

<img src="R_Assign_New_files/figure-markdown_github/first_sec_sep_comm_2-1.png" style="display: block; margin: auto;" />

``` r
graph_eccent5[[2]]
```

<img src="R_Assign_New_files/figure-markdown_github/first_sec_sep_comm_2-2.png" style="display: block; margin: auto;" />

``` r
graph_eccent5[[3]]
```

<img src="R_Assign_New_files/figure-markdown_github/first_sec_sep_comm_2-3.png" style="display: block; margin: auto;" />

``` r
graph_eccent5[[4]]
```

<img src="R_Assign_New_files/figure-markdown_github/first_sec_sep_comm_2-4.png" style="display: block; margin: auto;" />

``` r
graph_eccent5[[5]]
```

<img src="R_Assign_New_files/figure-markdown_github/first_sec_sep_comm_2-5.png" style="display: block; margin: auto;" />

``` r
graph_eccent5[[6]]
```

<img src="R_Assign_New_files/figure-markdown_github/first_sec_sep_comm_2-6.png" style="display: block; margin: auto;" />

``` r
graph_eccent5[[7]]
```

<img src="R_Assign_New_files/figure-markdown_github/first_sec_sep_comm_2-7.png" style="display: block; margin: auto;" />

``` r
graph_eccent5[[8]]
```

<img src="R_Assign_New_files/figure-markdown_github/first_sec_sep_comm_2-8.png" style="display: block; margin: auto;" />

``` r
############################################################################
```

#### **Analysis**

The centrality graph and eccentricity graph produced above for all the communities are like the ones produced earlier when 6 chapters were taken.

These new smaller graphs are broken from the big graphs produced in the earlier section of the assignment in which we had 6 chapters.

### **5.1 Second Half of the Book**

The process followed in the first section of the book is followed for the secodn half of the book.

``` r
################### word correlation #########################

word_cor_sec_2 <- book_sec_2 %>% 
                  group_by(word) %>%
                  filter(n() >= 11) %>% 
                  pairwise_cor(item=word, feature=chapter) %>% 
                  filter(!is.na(correlation), correlation >= 0.60) 

correlation_graph_sec_2 <-as_tbl_graph(word_cor_sec_2,directed=FALSE) 


graph_values_sec_2 <- correlation_graph_sec_2 %>% 
                      activate(edges) %>%
                      filter(!edge_is_multiple()) %>%
                      activate(nodes) %>%
                      mutate(centrality = centrality_degree(),
                      diameter = graph_diameter(),
                      radius = graph_radius(),
                      eccntrcty = node_eccentricity(),
                      luvian = group_louvain()
                     )

graph_specifction_sec_2 <- as.tibble(graph_values_sec_2)

############################################################

################### correlation plot #######################


fig5.2 <- ggraph(graph_values_sec_2, layout = "kk") +
  geom_edge_density(aes(fill = correlation)) +
  geom_edge_link(aes(edge_alpha = correlation)) + 
  geom_node_point(aes(color = factor(luvian)), size = 5) +
  geom_node_text(aes(label = name), size = 4, repel = TRUE,segment.color = "#860B62") +
  scale_color_brewer(palette = "Dark2") +
  theme_void() +
  labs(title = "Figure 5.2 | Correlation Graph",
       subtitle = "Coloured By Communities | Book Second Half Section ", color = "Communities",caption = "The Trial | Chapter 6 to 10")  

fig5.2
```

<img src="R_Assign_New_files/figure-markdown_github/corltion_graph_sec_2-1.png" style="display: block; margin: auto;" />

``` r
############################################################
```

``` r
################### Second Section - Sub-Graphs ##########################

sample <- graph_values_sec_2

sample <- sample %>% 
           activate(nodes) 

sample_measure <- as.tibble(sample)

number_comm <- max(sample_measure$luvian)

sep_community6 <- list()
graph_cent6 <- list()
graph_eccent6 <- list()

i <- 1

while(i <= number_comm){
  
  
  sep_community6[[i]] <- filter(sample,luvian == i)
  
  sep_community_meas6 <- sep_community6[[i]] %>% 
           activate(nodes) %>%
              mutate(centrality = centrality_degree(),
                     diameter = graph_diameter(),
                     radius = graph_radius(),
                     eccntrcty = node_eccentricity()
                     )

  sep_community_measr_spec6 <- as.tibble(sep_community_meas6)

  graph_cent6[[i]] <- ggraph(sep_community_meas6, layout = "fr") +
  geom_edge_link(aes(alpha = 0.2),edge_width=0.05) +
  geom_node_point(aes(size = centrality, color = centrality)) +
  geom_node_text(aes(label = name), size = 4,repel = TRUE, segment.color = "#860B62") + 
  scale_color_gradient(low = '#05D9F6', high = '#5011D1') +
  theme_void() +
  labs(title = paste("Figure 5.2.1.",i," | Centrality Measure"),
       color = "Centrality",caption = "The Trial | Chapter 1 to 6") +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = 'bold'))
  
  graph_eccent6[[i]] <-  ggraph(sep_community_meas6, layout = "fr") +
  geom_edge_link(aes(alpha = 0.2),edge_width=0.05) +
  geom_node_point(aes(size = eccntrcty, color = eccntrcty)) +
  geom_node_text(aes(label = name), size = 4, repel = TRUE, segment.color = "#860B62") + 
  scale_color_gradient(low = '#F2C9E1', high = '#B2076C') +
  theme_void() +
  labs(title = paste("Figure 5.2.2.",i," | Eccentricity Measure"),
       subtitle = paste("Diameter = ",sample(sep_community_measr_spec6$diameter, 1), 
                        "|| Radius = ", sample(sep_community_measr_spec6$radius, 1)), 
       color = "Eccentricity",caption = "The Trial |  Chapter 1 to 6") +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = 'bold'))  
  
  i = i+1

}

#######################################################################
```

``` r
############ Louvain - Centrality Graphs - Second Section #################

graph_cent6[[1]]
```

<img src="R_Assign_New_files/figure-markdown_github/secnd_sec_sep_comm_1-1.png" style="display: block; margin: auto;" />

``` r
graph_cent6[[2]]
```

<img src="R_Assign_New_files/figure-markdown_github/secnd_sec_sep_comm_1-2.png" style="display: block; margin: auto;" />

``` r
graph_cent6[[3]]
```

<img src="R_Assign_New_files/figure-markdown_github/secnd_sec_sep_comm_1-3.png" style="display: block; margin: auto;" />

``` r
graph_cent6[[4]]
```

<img src="R_Assign_New_files/figure-markdown_github/secnd_sec_sep_comm_1-4.png" style="display: block; margin: auto;" />

``` r
graph_cent6[[5]]
```

<img src="R_Assign_New_files/figure-markdown_github/secnd_sec_sep_comm_1-5.png" style="display: block; margin: auto;" />

``` r
###########################################################################
```

``` r
############ Louvain - Eccentricity Graphs - Second Section #################

graph_eccent6[[1]]
```

<img src="R_Assign_New_files/figure-markdown_github/secnd_sec_sep_comm_2-1.png" style="display: block; margin: auto;" />

``` r
graph_eccent6[[2]]
```

<img src="R_Assign_New_files/figure-markdown_github/secnd_sec_sep_comm_2-2.png" style="display: block; margin: auto;" />

``` r
graph_eccent6[[3]]
```

<img src="R_Assign_New_files/figure-markdown_github/secnd_sec_sep_comm_2-3.png" style="display: block; margin: auto;" />

``` r
graph_eccent6[[4]]
```

<img src="R_Assign_New_files/figure-markdown_github/secnd_sec_sep_comm_2-4.png" style="display: block; margin: auto;" />

``` r
graph_eccent6[[5]]
```

<img src="R_Assign_New_files/figure-markdown_github/secnd_sec_sep_comm_2-5.png" style="display: block; margin: auto;" />

``` r
###########################################################################
```

#### **Analysis**

As per the above graphs of second section of the book, we can observe that these graphs are similar to the ones obtained in the first section of the book. There is nothing extra obtained from these graphs which are good to be mentioned in the comment section.

**Conclusion:**
---------------

1.  The graph analytics technique used for text analysis helps in getting insight into the summary and relationship between important characters of the book.
2.  With the help of Louvain community detection on the graph obtained from the book helped in understanding the existence of characters within the important sections of the book.
3.  The use of NER NLP technique with tdf-if score was helpful in providing noun entities which shared the important contextual meaning in the different chapters of the book. The same can be verified by comparing the correlation plots obtained from NER technique and Bi-gram NLP technique.
4.  The cutting of communities for further analysis was helpful in developing an understanding in depth of the large communities formed initially.
5.  As I have read the book, I could make out that the characters and situations extracted from the book is accurate to the plot unfolded in the book.

**Reference:**
--------------

\[1\] Kafka, F. (1925). The trial. \[online\] Project Gutenberg. Available at: <http://www.gutenberg.org/ebooks/7849> \[Accessed 8 Apr. 2019\]. \[2\] David Robinson. (2017). David Robinson's Personal R Package. \[online\]. Available at: <https://github.com/dgrtwo/drlib> \[Accessed 4 Apr. 2019\]. \[3\] Dr. Shirin Glander. (2018). Another Game of Thrones network analysis - this time with tidygraph and ggraph. \[online\]. Available at: <https://www.r-bloggers.com/another-game-of-thrones-network-analysis-this-time-with-tidygraph-and-ggraph/> \[Accessed 4 Apr. 2019\]. \[4\] Pedersen, T. (2018). Package 'ggraph'. \[online\]. Available at: <https://cran.r-project.org/web/packages/ggraph/ggraph.pdf> \[5\] Introducing tidygraph. (2017). \[online\]. Available at: <https://www.data-imaginist.com/2017/introducing-tidygraph/>
