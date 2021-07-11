library(spotifyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(e1071)

# Sys.setenv(SPOTIFY_CLIENT_ID = 'your_client_id')
# Sys.setenv(SPOTIFY_CLIENT_SECRET = 'your_client_secret')

target_pl <- "your_target_playlist_name"
candidate_pl <- "your_candidate_playlist_name"

access_token <- get_spotify_access_token()

pls <- get_my_playlists(limit = 50) %>% 
  as_tibble()

best_pl <- pls %>% 
  filter(name == target_pl)

pl_tracks <- get_playlist_tracks(best_pl %>% pull(id), limit = 100) %>% 
  as_tibble() %>% 
  select(track.name, 
         track.track_number, 
         track.artists, 
         track.album.name, 
         track.album.release_date,  
         track.id)

audio_df <- get_track_audio_features(pl_tracks %>% pull(track.id)) %>% 
  bind_cols(pl_tracks)

n_tracks <- 100
n_run <- 1

while (n_tracks == 100) {
  print(n_run)
  tmp_tracks <- get_playlist_tracks(best_pl %>% pull(id), 
                                    limit = 100, 
                                    offset = n_run * 100) %>% 
    as_tibble() %>% 
    select(track.name, 
           track.track_number, 
           track.artists, 
           track.album.name, 
           track.album.release_date,  
           track.id) 
  
  audio_df <- get_track_audio_features(tmp_tracks %>% pull(track.id)) %>% 
    bind_cols(tmp_tracks) %>% 
    bind_rows(audio_df)
  
  n_tracks <- nrow(tmp_tracks)
  n_run <- n_run + 1
}

audio_df
svm_mod <- svm(audio_df %>% 
                 select(danceability, energy, key, loudness, mode, speechiness,
                        acousticness, instrumentalness, liveness, valence, tempo,
                        time_signature),
               y = NULL,
               type = 'one-classification',
               nu = 0.5,
               scale = TRUE,
               kernel = "radial")

radar_pl <- pls %>% 
  filter(stringr::str_detect(name, "Radar")) %>% 
  slice(1) %>% 
  pull(id) %>% 
  get_playlist_tracks() %>% 
  as_tibble() %>% 
  select(track.name, 
         track.track_number, 
         track.artists, 
         track.album.name, 
         track.album.release_date, 
         track.uri,
         track.id) 

radar_df <- get_track_audio_features(radar_pl %>% pull(track.id)) %>% 
  bind_cols(radar_pl)

radar_pred <-predict(svm_mod, radar_df %>% 
                       select(danceability, energy, key, loudness, mode, speechiness,
                              acousticness, instrumentalness, liveness, valence, tempo,
                              time_signature))
cand_tracks <- radar_df %>% 
  filter(radar_pred) %>% 
  select(track.name, track.artists, track.uri) %>% 
  unnest(track.artists) %>% 
  select(track.name, name, track.uri) %>% 
  group_by(track.name) %>% 
  summarize(name = paste0(name, collapse = " feat. "),
            uri = first(track.uri))


add_tracks_to_playlist(playlist_id = pls %>% 
                         filter(name == candidate_pl) %>%
                         pull(id),
                       uris = cand_tracks %>% pull(uri) %>% str_remove("spotify:track:"))
