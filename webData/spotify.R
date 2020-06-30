
outfolder <- "output/"
if (!dir.exists(outfolder)) dir.create(outfolder) 

library(tidyverse)
library(spotifyr)
library(ggridges)

# To create an app, go here: https://developer.spotify.com/dashboard/
# Sys.setenv(SPOTIFY_CLIENT_ID = 'xxxxxxxxxxxxxxxxxxxxx')
# Sys.setenv(SPOTIFY_CLIENT_SECRET = 'xxxxxxxxxxxxxxxxxxxxx')

access_token <- get_spotify_access_token()

# https://developer.spotify.com/documentation/web-api/reference/tracks/get-several-audio-features/
feature_vars <- c("loudness", "valence", "energy", "danceability", "speechiness", "tempo")
excluded_features <- c("tempo", "loudness")

# Functions ---------------------------------------------------------------

theme_custom <- function(base_family = "Avenir Next Condensed", fill = "white", ...) {
  theme_minimal(base_family = base_family, ...) %+replace%
    theme(
      plot.title = element_text(face = "bold", margin = margin(0, 0, 5, 0), hjust = 0, size = 13),
      plot.subtitle = element_text(face = "italic", margin = margin(0, 0, 5, 0), hjust = 0),
      plot.background = element_rect(fill = fill, size = 0), complete = TRUE,
      axis.title.x = element_text(margin = margin(15, 0, 0, 0)),
      axis.title.y = element_text(angle = 90, margin = margin(0, 20, 0, 0)),
      strip.text = element_text(face = "italic"),
      plot.title.position = "plot"
    )
}

select_features <- function(data_frame) {
  data_frame %>% 
    select(playlist_name, playlist_owner_name, track.album.name, track.artists, track.name, all_of(feature_vars)) %>% 
    select(-all_of(excluded_features))
}

ggfeatures <- function(data_frame) {
  
  data_frame %>% 
    select_features() %>% 
    pivot_longer(cols = -c(track.album.name, track.artists, track.name, playlist_name, playlist_owner_name), names_to = "feature") %>% 
    ggplot(aes(x = value, y = feature)) + 
    geom_jitter(height = 0.1, alpha = 0.5) +
    geom_vline(xintercept = 0.5, linetype = "dashed") +
    stat_summary(fun.data = mean_cl_normal, fun.args = list(mult = 2), color = "steelblue1", size = 2, geom = "linerange") +
    stat_summary(fun = mean, geom = "point", color = "steelblue", size = 4) +
    theme_custom() +
    labs(caption = "\nSource: Spotify + TBWA\\Chiat\\Day",
         subtitle = Sys.Date())
  
}

ggemotions <- function(...) {
  
  df <- map(list(...), select_features) %>% 
    bind_rows() %>% 
    drop_na()
  
  df_valence <- df %>% 
    group_by(playlist_name) %>% 
    summarize(enframe(Hmisc::smean.cl.boot(valence))) %>% 
    pivot_wider(id_cols = c(playlist_name, name), names_prefix = "valence")
  
  df_energy <- df %>% 
    group_by(playlist_name) %>% 
    summarize(enframe(Hmisc::smean.cl.boot(energy))) %>% 
    pivot_wider(id_cols = c(playlist_name, name), names_prefix = "energy")
  
  df_summary <- full_join(df_valence, df_energy)
  
  df %>% 
    group_by(playlist_name) %>% 
    ggplot(aes(valence, energy, color = playlist_name)) + 
    geom_vline(xintercept = 0.5, linetype = "dashed") + 
    geom_hline(yintercept = 0.5, linetype = "dashed") + 
    geom_point(alpha = 1/3) + 
    geom_pointrange(
      data = df_summary,
      mapping = aes(x = valenceMean, xmin = valenceLower, xmax = valenceUpper,
                    y = energyMean), size = 1
    ) +
    geom_pointrange(
      data = df_summary,
      mapping = aes(x = valenceMean, ymin = energyLower, ymax = energyUpper,
                    y = energyMean)
    ) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) + 
    theme_custom() + 
    theme(legend.position = "top") +
    labs(color = NULL) +
    annotate(geom = "text", label = "Turbulent/Angry", x = 0.05, y = 1, family = "Avenir Next Condensed") +
    annotate(geom = "text", label = "Happy/Joyful", x = 0.95, y = 1, family = "Avenir Next Condensed") +
    annotate(geom = "text", label = "Sad/Depressing", x = 0.05, y = 0, family = "Avenir Next Condensed") +
    annotate(geom = "text", label = "Chill/Peaceful", x = 0.95, y = 0, family = "Avenir Next Condensed") +
    labs(subtitle = Sys.Date(), caption = "\nSource: Spotify + TBWA\\Chiat\\Day") 
  
}


# Playlists ---------------------------------------------------------------

blm <- get_playlist_tracks(playlist_id = "37i9dQZF1DWWAqc46ZJdZf")
blm_features <- get_playlist_audio_features(playlist_uris = "37i9dQZF1DWWAqc46ZJdZf")

pride <- get_playlist_tracks("37i9dQZF1DX59HcpGmPXYR")
pride_features <- get_playlist_audio_features(playlist_uris = "37i9dQZF1DX59HcpGmPXYR")

top50_global <- get_playlist_tracks(playlist_id = "37i9dQZEVXbMDoHDwVN2tF")
top50_global_features <- get_playlist_audio_features(playlist_uris = "37i9dQZEVXbMDoHDwVN2tF")

top50_usa <- get_playlist_tracks(playlist_id = "37i9dQZEVXbLRQDuF5jeBp")
top50_usa_features <- get_playlist_audio_features(playlist_uris = "37i9dQZEVXbLRQDuF5jeBp")

top_2019 <- get_playlist_tracks(playlist_id = "37i9dQZF1DXcz8eC5kMSWZ")
top_2019_features <- get_playlist_audio_features(playlist_uris = "37i9dQZF1DXcz8eC5kMSWZ")


# Feature plots -----------------------------------------------------------

ggfeatures(blm_features) + 
  ggtitle("Playlist: Black Lives Matter")

ggsave(paste0(outfolder, "blm_features.png"), device = "png", dpi = "print", width = 10, height = 6)

ggfeatures(pride_features) +
  ggtitle("Playlist: Pride")

ggsave(paste0(outfolder, "pride_features.png"), device = "png", dpi = "print", width = 10, height = 6)

# Sentify rip off ---------------------------------------------------------

ggemotions(pride_features, blm_features) + 
  ggtitle("Spotify Emotional Content")

ggsave(paste0(outfolder, "blm_pride.png"), device = "png", dpi = "print", width = 10, height = 8)

ggemotions(top_2019_features, top50_usa_features, top50_global_features) +
  ggtitle("Spotify Emotional Content")

ggsave(paste0(outfolder, "years_emotions.png"), device = "png", dpi = "print", width = 10, height = 8)

bind_rows(top_2019_features, top50_global_features, top50_usa_features) %>% 
  ggplot(aes(y = playlist_name, x = tempo)) + 
  ggridges::geom_density_ridges()
