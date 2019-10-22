library(blogdown)

new_site(theme = "yoshiharuyamashita/blackburn", 
         sample = TRUE, 
         theme_example = TRUE, 
         empty_dirs = TRUE,
         to_yaml = TRUE)

file.create("netlify.toml")
