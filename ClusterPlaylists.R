myPlayists <- get_user_playlists("1138453506")

playlists <- list()
i <- 1
for(playUri in myPlayists$playlist_uri[c(3,6,10)]){
  playlists[[i]] <- get_playlist_audio_features("1138453506", playlist_uris = playUri)
  i <- i + 1
}


playlists <- bind_rows(playlists)

modelLists <- playlists %>% select(danceability,
                                   energy, 
                                   liveness, tempo)

mod <- kmeans(modelLists, centers = 3)

clusteredLists<- cbind(playlists, Cluster = mod$cluster)

ggplot(clusteredLists, aes(x = tempo, y = energy)) + 
  geom_point(aes(col = as.factor(playlist_name)))

ggplot(clusteredLists, aes(x = tempo, y = energy)) + 
  geom_point(aes(col = as.factor(Cluster)))


lisaList <- get_playlist_audio_features("1138453506", playlist_uris = "3nX8Kyas0BKytjIuSDY1Tm") 
floList <-  get_playlist_audio_features("1138453506", playlist_uris = "6f4jh46czz8VMVpCPT9KlE") 

ourList <- bind_rows(lisaList, floList)

ourModelList <- ourList %>% select(danceability, energy, speechiness, tempo, acousticness)

ourMod <- kmeans(ourModelList, centers = 2)

ourClusteredList <- cbind(ourList, Cluster = ourMod$cluster)

ggplot(ourList, aes(x = 1:nrow(ourList), y = danceability)) + 
  geom_point(aes(col = as.factor(playlist_name)))

ggplot(ourClusteredList, aes(x = 1:nrow(ourList), y = danceability)) + 
  geom_point(aes(col = as.factor(Cluster)))

