library(spotifyr)
library(dplyr)
library(mlr)
library(ggplot2)
library(magrittr)
library(stringr)
library(caret)
library(ranger)
library(tidyr)

# BIG vs Pac --------------------------------------------------------------

pacArtists <- get_artists('2Pac')
pacAlbums <- get_artist_albums(pacArtists$artist_uri[1])
allEyesTracks <- get_album_tracks(pacAlbums[2,])


allEyesFeat <- get_track_audio_features(allEyesTracks)
allEyesPop <- get_track_popularity(allEyesTracks)

allEyesData <- left_join(allEyesFeat, allEyesPop, by = "track_uri")
allEyesData$name <- allEyesTracks$track_name

allEyesData %>% summarize(danceability = mean(danceability),
                          energy = mean(energy),
                          loudness = mean(loudness),
                          speechiness = mean(speechiness),
                          acousticness = mean(acousticness),
                          instrumentalness = mean(instrumentalness),
                          valence = mean(valence),
                          tempo = mean(tempo),
                          duration = mean(duration_ms))


bigArtists <- get_artists('Notorious B.I.G.')
bigAlbums <- get_artist_albums(bigArtists$artist_uri[1])
lifeAfterTracks <- get_album_tracks(bigAlbums[3,])


lifeAfterFeat <- get_track_audio_features(lifeAfterTracks)
lifeAfterPop <- get_track_popularity(lifeAfterTracks)

lifeAfterData <- left_join(lifeAfterFeat, lifeAfterPop, by = "track_uri")
lifeAfterData$name <- lifeAfterTracks$track_name

lifeAfterData %>% summarize(danceability = mean(danceability),
                            energy = mean(energy),
                            loudness = mean(loudness),
                            speechiness = mean(speechiness),
                            acousticness = mean(acousticness),
                            instrumentalness = mean(instrumentalness),
                            valence = mean(valence),
                            tempo = mean(tempo),
                            duration = mean(duration_ms))





# jazz vs classic ------------------------------------------------------------------------------

jazzExample <- get_album_tracks("1lDtUlOPGKp56gQ24MvmNG")
classicExample <- get_album_tracks("49A5GKKvpSjIpgo4ce1Kid")

jazzExTracks <- get_track_audio_features(tracks = jazzExample)
classExTracks <- get_track_audio_features(tracks = classicExample)

jazzExTracks %>% summarize_if(is.numeric, mean)
classExTracks %>% summarize_if(is.numeric, mean)

# kanye west timeline -----------------------------------------------------


rhcp <- get_artist_audio_features("0L8ExT028jH3ddEcZwqJJ5")

rhcpAlbums <- rhcp %>% group_by(album_name)  %>% summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
   filter(!(album_name %in% c("Mothers Milk", "Im With You"))) %>% mutate(Year = as.Date(c("1991-01-01", "2002-01-01",
                                                                                           "1999-01-01", "1985-01-01",
                                                                                           "1995-01-01", "1984-01-01",
                                                                                           "2006-01-01", "2016-01-01", "1987-01-01"))) %>% 
  ungroup() %>% 
  arrange(Year) %>% mutate_if(is.numeric, scale) %>% select(-disc_number, -time_signature, -valence, -album_popularity,
                                                            -track_number,-liveness, -track_popularity,-acousticness) %>% 
  melt(id.vars = c("album_name", "Year"))
#breaks = c("danceability", "energy", "speechiness", "acousticness", "instrumentalness", "tempo", "duration_ms", "track_populraity")
cols <- c("blue", "red", "gray", "green", "black", "darkorange", "darkblue")
ggplot(rhcpAlbums, aes(x = Year, y = value, group = variable)) +
  geom_line(aes(col = as.factor(variable)), lwd = 1, alpha = 0.4) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
         panel.grid.major = element_blank()) +
  scale_color_manual(values =  cols, name = "Audio Features") +
  ggtitle("Change of RHCP music over time")

  
