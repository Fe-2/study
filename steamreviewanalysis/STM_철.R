library(stm)
library(csv)
library(Rcpp)
library(dplyr)
library(tidytext) #tidy
library(ggplot2) # graph
library(ggcorrplot)
library(igraph)
library(factoextra)




# install.packages('tm')
# install.packages('Rcpp')

data <- read.csv("STMs(새시작).csv")
#data$Week <- as.Date(data$Week, "%Y-%m-%d")

#data["Positive.1"] = lapply(data["Positive.1"], factor)

processed <- textProcessor(data$line, metadata = data,
                           removestopwords = FALSE,
                           removenumbers = FALSE,
                           removepunctuation = FALSE,
                           ucp = FALSE,
                           stem = FALSE,
                           wordLengths = c(2, Inf),)

out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

docs <- out$documents
vocab <- out$vocab
meta <-out$meta



lostarksearch <- searchK(documents = out$documents,
                         vocab = out$vocab,
                         K=10:25,
                         init.type = "Spectral", #Spectral , LDA
                         data = out$meta,
                         prevalence = ~ Recommend + s(Day),
                         max.em.its = 30,
                         seed = 2021711695
                         )

save(lostarksearch, file = "C:/Users/LEE/python/steam(새시작)/STM/result/searchK.rda")

setwd("C:/Users/LEE/python/steam(새시작)/STM/result")
png(paste0("searchK", ".png"),width=6000,height=4000,res=500)
plot(lostarksearch)
dev.off()



# Likelihood Coherence는 높을수록
# Residual은 낮을수록

topicK <- 16

lostark <- stm(documents = out$documents, vocab = out$vocab,
               K = topicK,
               max.em.its = 30,
               data = out$meta,
               prevalence = ~ Recommend + s(Day),
               init.type = "Spectral", #Spectral, LDA
               seed = 2021711695)

save(lostark, file = "C:/Users/LEE/python/steam(새시작)/STM/result/model.rda")

####
load("C:/Users/LEE/python/steam(새시작)/STM/result/model.rda")

setwd("C:/Users/LEE/python/steam(새시작)/STM/result")
png(paste0("K = ",topicK, ".png"),width=3000,height=3000,res=500)
plot(lostark)
dev.off()

topicQuality(lostark,out$documents)




setwd("C:/Users/LEE/python/steam(새시작)/STM/result")
png(paste0("K = ",topicK,"_nonum", ".png"),width=3000,height=3000,res=500)
plot(lostark, topic.names = names)
dev.off()







### Topic-word distribution https://stackoverflow.com/tags

#load("C:/Users/LEE/python/steam(새시작)/STM/result/model.rda")


lostark_topics <- tidy(lostark, matrix = "beta")

write.csv(lostark_topics,file="C:/Users/LEE/python/steam(새시작)/STM/result/topics_beta.csv")

prob <- t(labelTopics(lostark, n = 20,  frexweight = 0.5)$prob)

write.csv(prob,file="C:/Users/LEE/python/steam(새시작)/STM/result/topics_prob.csv")


frex <- t(labelTopics(lostark, n = 20,  frexweight = 0.5)$frex)

write.csv(frex,file="C:/Users/LEE/python/steam(새시작)/STM/result/topics_frex(0.5).csv")


frex0 <- t(labelTopics(lostark, n = 20,  frexweight = 0.0)$frex)

write.csv(frex0,file="C:/Users/LEE/python/steam(새시작)/STM/result/topics_frex(0.0).csv")


frex1 <- t(labelTopics(lostark, n = 20,  frexweight = 0.1)$frex)

write.csv(frex1,file="C:/Users/LEE/python/steam(새시작)/STM/result/topics_frex(0.1).csv")





lostark_top_terms <- lostark_topics %>%
  group_by(topic) %>%
  top_n(topicK, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

lostark_top_terms

lostark_top_terms %>% 
  mutate(term = reorder(term, beta))%>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

### topic proportion ###


dt <- make.dt(lostark, meta = out$meta)
write.csv(dt,file="C:/Users/LEE/python/steam(새시작)/STM/result/topic_proportion.csv")

dt <- make.dt(lostark, meta = NULL)
write.csv(dt,file="C:/Users/LEE/python/steam(새시작)/STM/result/topic_proportion(no_beta).csv")



### Estimate Topic correlation




lostark_topic_corr <- topicCorr(lostark, cutoff = 0.01)


setwd("C:/Users/LEE/python/steam(새시작)/STM/result/corr")
png("corr.png",width=3000,height=3000,res=500)
ggcorrplot(lostark_topic_corr$cor)
dev.off()


############################

write.csv(lostark_topic_corr$cor,file="C:/Users/LEE/python/steam(새시작)/STM/result/topics_corr.csv")


topicnames =c('Server queue',
             'Freemium',
             'Purchase Errors',
             'Developer Issues',
             'Quest',
             'Comparison with mobile',
             'Cutscene',
             'MMORPG',
             'Microtransaction',
             'Class',
             'Game Quality',
             'RMT',
             'Player Experience',
             'RNG Upgrade System',
             'Game Contents',
             'Collectibles & Adventure')


## estimateEffect

out$meta$Recommend <- as.factor(out$meta$Recommend)
predict_topics <- estimateEffect(formula = 1:topicK ~ Recommend + Day,
                                 lostark, metadata = meta, uncertainty = "Global")


cat(capture.output(summary(predict_topics)),file="C:/Users/LEE/python/steam(새시작)/STM/result/predict_topic.txt")

summary(predict_topics)

setwd("C:/Users/LEE/python/steam(새시작)/STM/result")
png("Recommended.png",width=6000,height=6000,res=500)
plot(predict_topics, 
     covariate = "Recommend",
     topics = 1:topicK,
     model = lostark,
     method = "difference",
     cov.value1 = "Recommend",
     cov.value2 = "Not Recommend",
     ci.level = .95,
     cex.main=2,
     cex.lab=1.5,
     xlim = c(-0.11,0.11), 
     width = 100,
     labeltype = "custom", # The default: "number", # option: "custom", "prob", "frex", "score", "lift". 
     custom.labels = paste0("Topic", seq(1:topicK)),
     xlab = "Not Recommended .............. Recommended     ",
     main = "Effect of Recommendation")
dev.off()




setwd("C:/Users/LEE/python/steam(새시작)/STM/result")
png("Recommend(all).png",width=3000,height=3000,res=500)
plot(predict_topics, 
     covariate = "Recommend",
     topics = 1:topicK,
     model = lostark,
     method = "difference",
     cov.value1 = "Recommend",
     cov.value2 = "Not Recommend",
     ci.level = .95,
     xlim = c(-0.11,0.11), 
     labeltype = "custom", # The default: "number", # option: "custom", "prob", "frex", "score", "lift". 
     custom.labels = topicnames,
     xlab = "Not Recommend ......... Recommend     ",
     main = "Effect of Recommendation")
dev.off()

topicnames2 =c('Microtransaction',
              'Developer Issues',
              'RMT',
              'RNG Upgrade System',
              'Game Contents',
              'Collectibles & Adventure')

setwd("C:/Users/LEE/python/steam(새시작)/STM/result")
png("Recommend(Micro).png",width=3000,height=3000,res=500)
plot(predict_topics, 
     covariate = "Recommend",
     topics = c(9,4, 12, 14, 15, 16),
     model = lostark,
     method = "difference",
     cov.value1 = "Recommend",
     cov.value2 = "Not Recommend",
     ci.level = .95,
     xlim = c(-0.05,0.05), 
     labeltype = "custom", # The default: "number", # option: "custom", "prob", "frex", "score", "lift". 
     custom.labels = topicnames2,#ENG,
     xlab = "Not Recommend ......... Recommend     ",
     main = "Effect of Recommendation")
dev.off()





for(i in 1:topicK){
  setwd("C:/Users/LEE/python/steam(새시작)/STM/result/topics_day")
  png(paste0("topic", i, ".png"),width=6000,height=6000,res=500)
  plot(predict_topics, 
       covariate = "Day",
       topics = i,
       model = lostark,
       method = "continuous",
       printlegend = F,
       #add = T,
       ci.level = 0.95,
       labeltype = "custom", # The default: "number", # option: "custom", "prob", "frex", "score", "lift". 
       custom.labels = names[i],
       xlab = "Day",
       main = names[i]
       )
  dev.off()
}

setwd("C:/Users/LEE/python/steam(새시작)/STM/result/topics_day")
png("Alls.png",width=6000,height=6000,res=500)
plot(predict_topics, 
     covariate = "Day",
     topics = 1:topicK,
     model = lostark,
     method = "continuous",

     printlegend = T,
  
     ci.level = 0,
     labeltype = "custom", # The default: "number", # option: "custom", "prob", "frex", "score", "lift". 
     custom.labels = names,
     xlab = "Day",
     main = "Topics",
     legend = 'left'

)
dev.off()


## View documents of particular topics


for(i in 1:topicK){
thoughts<- findThoughts(lostark,texts = out$meta$line, topics=i, n=10)$docs[[1]]

setwd("C:/Users/LEE/python/steam(새시작)/STM/result/thoughts")
png(paste0("thought", i,".png"),width=3000,height=3000,res=300)
plotQuote(thoughts, width = 100, text.cex = .7, maxwidth = 300)
dev.off()
}

thoughts<- findThoughts(lostark,texts = out$meta$line, topics=7, n=15)$docs[[1]]
plotQuote(thoughts, width = 100, text.cex = .7, maxwidth = 300)


#감성분석?
plot(lostark, 
     type="perspectives", 
     topics=c(13,10),
     plabels = c("Topic 13","Topic 10"))


# interaction

topicnames_abcdef =c('Server queue',
              'Freemium',
              'Purchase Errors',
              '(b) Developer Issues',
              'Quest',
              'Comparison with mobile',
              'Cutscene',
              'MMORPG',
              '(a) Microtransaction',
              'Class',
              'Game Quality',
              '(d) RMT',
              'Player Experience',
              '(c) RNG Upgrade System',
              '(e) Game Contents',
              '(f) Collectibles & Adventure')



interaction_topics <- estimateEffect(formula = 1:topicK ~ Recommend + Day + Recommend*Day,
                                 lostark, metadata = meta, uncertainty = "Global")

cat(capture.output(summary(interaction_topics)),file="C:/Users/LEE/python/steam(새시작)/STM/result/predict_topic_interaction.txt")



summary(interaction_topics)


topicxaxis = c(0.2, 
               0.15,
               0.1,
               0.2,
               0.1,
               0.1, 
               0.1, 
               0.15,
               0.15,
               0.1, 
               0.15, 
               0.25, 
               0.1, 
               0.15, 
               0.2,
               0.1
               )
for(i in 1:topicK){
setwd("C:/Users/LEE/python/steam(새시작)/STM/result/interaction")
png(paste0("interaction_topic", i,".png"),width=3000,height=3000,res=300)

plot(interaction_topics, covariate = "Day", model = lostark,
        topics = i,
        method = "continuous", xlab = "Days", moderator = "Recommend",
        moderator.value = "Recommend", linecol = "blue", ylim = c(-0.005, topicxaxis[i]),
        main = topicnames_abcdef[i],
        cex.main = 2.5,
        cex.axis=2.5,
        printlegend = F)

plot(interaction_topics, covariate = "Day", model = lostark,
        topics = i,
        method = "continuous", xlab = "Days", moderator = "Recommend",
        moderator.value = "Not Recommend", linecol = "red", add = T,
        printlegend = F)


legend(0.05, 0.01, c("Recommend", "Not Recommend"),lwd = 2, col = c("blue", "red"))

dev.off()
}


# s(day)

predict_topics_s <- estimateEffect(formula = 1:topicK ~ Recommend + s(Day) + Recommend*s(Day),
                                 lostark, metadata = meta, uncertainty = "Global")

cat(capture.output(summary(predict_topics_s)),file="C:/Users/LEE/python/steam(새시작)/STM/result/predict_topic_interaction_s.txt")


summary(predict_topics_s)


topicxaxis_s = c(0.30, 
               0.15,
               0.15,
               0.2,
               0.1,
               0.1, 
               0.1, 
               0.15,
               0.15,
               0.15, 
               0.15, 
               0.25, 
               0.1, 
               0.15, 
               0.2,
               0.1
)
for(i in 1:topicK){
  setwd("C:/Users/LEE/python/steam(새시작)/STM/result/interaction(s)")
  png(paste0("interaction_topic", i,".png"),width=3000,height=3000,res=300)
  
  plot(predict_topics_s, covariate = "Day", model = lostark,
       topics = i,
       method = "continuous", xlab = "Days", moderator = "Recommend",
       moderator.value = "Recommend", linecol = "blue", ylim = c(-0.005, topicxaxis_s[i]),
       main = topicnames[i],
       cex.main = 2.5,
       cex.axis=2.5,
       printlegend = F)
  
  plot(predict_topics_s, covariate = "Day", model = lostark,
       topics = i,
       method = "continuous", xlab = "Days", moderator = "Recommend",
       moderator.value = "Not Recommend", linecol = "red", add = T,
       printlegend = F)
  
  
  legend(0.05, 0.01, c("Recommend", "Not Recommend"),lwd = 2, col = c("blue", "red"))
  
  dev.off()
}


####### corr(안씀) ###################





setwd("C:/Users/LEE/python/steam(새시작)/STM/result/corr")
png("cluster1.png",width=800,height=800,res=100)
fviz_nbclust(lostark_topic_corr$cor, kmeans, method = "wss")
+ geom_vline(xintercept = 4, linetype = 2)
dev.off()

setwd("C:/Users/LEE/python/steam(새시작)/STM/result/corr")
png("cluster2.png",width=800,height=800,res=100)
fviz_nbclust(lostark_topic_corr$cor, kmeans, method = "silhouette")
dev.off()

setwd("C:/Users/LEE/python/steam(새시작)/STM/result/corr")
png("cluster3.png",width=800,height=800,res=100)
fviz_nbclust(lostark_topic_corr$cor, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)
+geom_vline(xintercept = 4, linetype = 2)
dev.off()


res.hc <- eclust(scale(lostark_topic_corr$cor), "hclust", nboot = 500)

setwd("C:/Users/LEE/python/steam(새시작)/STM/result/corr")
png("cluster_dendrogram.png",width=800,height=800,res=100)
fviz_dend(res.hc, rect = TRUE)
dev.off()

vec_color <- res.hc$cluster
vec_color <- to_factor(vec_color)
levels(vec_color) <- c("red", "yellow",'aquamarien', "green", 'cyan', "blue",'magenta',' #ff3399')



setwd("C:/Users/LEE/python/steam(새시작)/STM/result/corr")
png("topics_cluster.png",width=8000,height=8000,res=500)
plot(lostark_topic_corr,
     vlabels = topicnames,
     vertex.color = vec_color,
     vertex.size = 17,
     vertex.label.cex = 1,
     vertex.label.color = "black")
dev.off()


