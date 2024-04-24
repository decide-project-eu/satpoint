# this script pre-builds the articles for the pkgdown site as they rely
# on files that cannot be committed and therefore they can't be built on
# github

library(knitr)
library(fs)

# set working directory to make certain steps easier
setwd(file.path(rstudioapi::getActiveProject(), "vignettes"))

# collect files to be built
articles.orig <- list.files(pattern = ".Rmd.orig")
articles.rmd <- list.files(pattern = ".Rmd$")

# step 1: remove built Rmd files
if(length(articles.orig) == length(articles.rmd)) {
  lapply(articles.orig, function(file) {
    rmd_file <- gsub(".orig", "", file)
    file.remove(rmd_file)
    file.rename(file, rmd_file)
  })
} else {
  stop("One of more Rmd files, missing an Rmd.orig file. Check manually.")
}

# step 2: edit the files

# step 3: rename them back to .orig
lapply(articles.orig, function(file) {
  rmd_file <- gsub(".orig", "", file)
  file.rename(rmd_file, file)
})

# step 4: build them for commit/push to github
lapply(articles.orig, function(file) {
  rmd_file <- gsub(".orig", "", file)
  knit(file, rmd_file)
})

# # step 5: post process images files
all_lines <- readLines("satpoint.Rmd")
all_lines <- gsub("figure/", "", all_lines)
writeLines(all_lines, "satpoint.Rmd")

################################################################################
# manually move images to vignette folder
pngs_to_move <- dir_ls("figure", regexp = ".png")
walk(pngs_to_move, function(the_file) {
  file_move(the_file, path_file(the_file))
})

dir_delete("figure/")
################################################################################


# step 6: set working directory back in case I forget
setwd(rstudioapi::getActiveProject())

# step 7: build pkgdown site locally to check
pkgdown::build_site()
