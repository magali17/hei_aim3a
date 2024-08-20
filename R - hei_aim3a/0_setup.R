
# Define the directories to create using file.path with ..
dirs <- c(
   
  file.path("Output", "final", "onroad"),
  file.path("Output", "final", "roadside")
  
)

# Use lapply to create directories if they do not exist
lapply(dirs, function(dir) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    cat("Created directory:", dir, "\n")
  } else {
    cat("Directory already exists:", dir, "\n")
  }
})
