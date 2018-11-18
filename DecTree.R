## Decision Tree for Jazz, Rap, Rock, Tango Playlists
Sys.setenv(SPOTIFY_CLIENT_ID = "be759acc63cc44199a18d4b6bbb4aad5")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "e294197f783741f38c7f6c87061d0fb2")
jazz <- get_artist_audio_features("Miles Davis")

rock <- get_artist_audio_features("AC/DC")

pop <- get_artist_audio_features("Shania Twain")

punk <- get_artist_audio_features("Blinker Links")


jazzAlbum <- jazz %>% filter(album_name  %in% c("Kind Of Blue", "Round About Midnight", "Miles Smiles"))

rockAlbum <- rock %>% filter(album_name == "Back In Black")

popAlbum <- pop %>% filter(album_name == "The Woman In Me")

punkAlbum <- punk %>% filter(album_name == "Der liebe Gott sieht alles")


jazzAlbum  %<>% mutate(Genre = "Jazz") %>% select(Genre, energy, acousticness, instrumentalness, loudness, tempo, danceability, duration_ms)
rockAlbum %<>% mutate(Genre = "Rock") %>% select(Genre, energy, acousticness, instrumentalness, loudness, tempo, danceability, duration_ms)
popAlbum %<>%  mutate(Genre = "Pop") %>% select(Genre, energy, acousticness, instrumentalness, loudness, tempo, danceability, duration_ms)
punkAlbum %<>% mutate(Genre = "Punk") %>% select(Genre, energy, acousticness, instrumentalness, loudness, tempo, danceability, duration_ms)

genres <- bind_rows(list(jazzAlbum, rockAlbum, popAlbum, punkAlbum)) %>% drop_na() %>% mutate(Genre = as.factor(Genre))

genres %<>% mutate(duration_ms = duration_ms/1000, instrumentalness = instrumentalness * 100)

tree <- train(Genre ~ ., method = "rpart", data = genres, maxdepth = 4, tuneGrid = expand.grid(cp = seq(0,0.5,0.1)))
prp(tree$finalModel)
