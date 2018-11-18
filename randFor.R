ninetiesTracks <- get_playlist_audio_features(username = "spotify",playlist_uris = "37i9dQZF1DX186v583rmzp")

ninetiesArtists <- ninetiesTracks %>% pull(artist_name) %>% unique() %T>% print() 

modelArtists <- c("The Notorious B.I.G.", "Dr. Dre", "2Pac", "Warren G", "Snoop Dogg", "Wu-Tang Clan", "Cypress Hill", "Method Man",
                  "Westside Connection", "Mos Def", "Eminem", "Busta Rhymes", "Bone Thugs-N-Harmony")

trainData <- get_artist_audio_features(modelArtists[1])
for(art in modelArtists[-1]){
  print(art)
  trainData %<>% rbind(get_artist_audio_features(art))
}
trainData %<>% filter(artist_name %in% modelArtists)
trainData %<>% filter(!is_collaboration)
trainData %<>% mutate(d = as.numeric(sapply(str_split(album_release_date, "-"), function(x) x[1]))) %>% filter(d < 2000) %>% select(-d)
trainData %<>% mutate(oldschool = 1) 


nowPlaylist <- get_playlist_audio_features(username = "spotify", playlist_uris = "37i9dQZF1DX0XUsuxWHRQd") 
nowArtists <- nowPlaylist %>% pull(artist_name) %>% unique() %T>% print

nowModelArtists <- c("Kodak Black", "Gucci Mane", "Cardi B", "Aminé", "Quavo", "Young Thug", "Tyga", "Lil Yachty", "Drake",
                     "Travis Scott", "XXXTENTACION")

nowTrainData <- get_artist_audio_features(nowModelArtists[1])
for(art in nowModelArtists[-1]){
  print(art)
  nowTrainData %<>% rbind(get_artist_audio_features(art)) 
}
nowTrainData %<>% filter(artist_name %in% nowModelArtists)
nowTrainData %<>% filter(!is_collaboration)
nowTrainData %<>% mutate(d = as.numeric(sapply(str_split(album_release_date, "-"), function(x) x[1]))) %>% filter(d > 2010) %>% select(-d)
nowTrainData %<>% mutate(oldschool = 0)


compTrainData <- rbind(trainData,nowTrainData)
rfData <- compTrainData %>% select(oldschool, duration_ms, valence, liveness, instrumentalness, danceability, energy, loudness,
                                   tempo, album_popularity, speechiness)

corrPlot(rfData)

trainInd <- createDataPartition(1:nrow(rfData), p = 0.7)[[1]]
rfTrain <- rfData[trainInd,] %>% drop_na()
rfTest <- rfData[-trainInd,] %>% drop_na()

rfTrain %<>% mutate(oldschool = factor(oldschool, label = c("old", "new"), level = c(0,1))) 
rfTest %<>% mutate(oldschool = factor(oldschool, label = c("old", "new"), level = c(0,1))) 
rf <- ranger(oldschool ~ ., data = rfTrain, num.trees = 1000)

fit <- train(
  oldschool ~ .,
  data = rfTrain,
  method = "ranger",
  num.trees = 2000,
  metric = "Accuracy",
  trControl = trainControl(method = "cv", number = 5, verbose = TRUE),
  tuneGrid = expand.grid(mtry = c(3,4,5,6,7), splitrule = "gini", min.node.size = 5:7),
  importance = 'impurity', na.action = na.omit)

pred <- predict(fit, rfTest)
confusionMatrix(pred, rfTest$oldschool)

plot(varImp(fit))