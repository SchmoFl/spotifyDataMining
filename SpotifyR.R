library(spotifyr)
library(dplyr)
library(mlr)
library(ggplot2)

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



get_related_artists("2Pac")


#get_user_playlists("1138453506")


get_genre_artists("Rap")


kanyeAudio <- get_artist_audio_features("5K4W6rqBFWDnAN6FQUkS6x")

kanyeAudio %>% group_by(album_name) %>% summarise_if(is.numeric, mean, na.rm = TRUE) %>% arrange(desc(album_popularity)) %>% select(album_name, track_popularity)
