waltz <- get_playlist_audio_features("qt0flfggyj5v1wl2oqob7v6fh", "6VicNih0zpslGopoPXotE9")
salsa <- get_playlist_audio_features("as.sanchez", "1HZ2pRVB4elzbybM7YtFwZ")
tango <- get_playlist_audio_features("1153564737", "0gvgwrQRKAo9H1oUm8HBHE")
bachata <- get_playlist_audio_features("spotify", "37i9dQZF1DX7MTlMMRl0MD")

waltz %<>%  mutate(Genre = "Waltz")
salsa %<>%  mutate(Genre = "Salsa")
tango %<>%  mutate(Genre = "Tango")
bachata %<>%  mutate(Genre = "Bachata")

playlists <- bind_rows(list(waltz, salsa, tango, bachata))

modelLists <- playlists %>% select(danceability,
                                   energy, 
                                   liveness, tempo)
                                   #, duration_ms, loudness, instrumentalness, acousticness, speechiness) 

mod <- kmeans(modelLists, centers = 4)

clusteredLists<- cbind(playlists, Cluster = mod$cluster)

ggplot(clusteredLists, aes(x = 1:nrow(modelLists), y = duration_ms)) + 
  geom_point(aes(col = as.factor(Genre)))

ggplot(clusteredLists, aes(x = 1:nrow(modelLists), y = duration_ms)) + 
  geom_point(aes(col = as.factor(Cluster)))


lisaList <- get_playlist_audio_features("1138453506", playlist_uris = "3nX8Kyas0BKytjIuSDY1Tm") 
floList <-  get_playlist_audio_features("1138453506", playlist_uris = "6f4jh46czz8VMVpCPT9KlE") 

ourList <- bind_rows(lisaList, floList)

ourModelList <- ourList %>% select(danceability, energy, speechiness, tempo, acousticness)

ourMod <- kmeans(ourModelList, centers = 2)

ourClusteredList <- cbind(ourList, Cluster = ourMod$cluster)

ggplot(ourList, aes(x = 1:nrow(ourList), y = tempo)) + 
  geom_point(aes(col = as.factor(playlist_name)))

ggplot(ourClusteredList, aes(x = 1:nrow(ourList), y = tempo)) + 
  geom_point(aes(col = as.factor(Cluster), pch = as.factor(playlist_name)), size = 3)

