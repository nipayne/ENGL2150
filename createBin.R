


createBin <- function(name) {
  path2file <- paste(name, "/data", sep = "")
  
  dirName <- as.character(Sys.time())
  dir <- str_replace_all(dirName, "-|:", "_")
  
  dir.create(paste(path2file, "/", dir, sep = ""))
  fileList <-
    list.files(path2file, full.names = TRUE)
  fileList <- fileList[grepl("txt", fileList)]
  readTextFiles <-
    function(file) {
      message(file)
      rawText = paste(scan(
        file,
        sep = "\n",
        what = "raw",
        strip.white = TRUE
      ))
      output = tibble(filename = gsub(path2file, "", file), text = rawText) %>%
        group_by(filename) %>%
        summarise(text = paste(rawText, collapse = " "))
      return(output)
    }
  
  combinedTexts <-
    tibble(filename = fileList) %>%
    group_by(filename) %>%
    do(readTextFiles(.$filename))
  
  baseFile <- paste(name, "_base", sep = "")
  w2vInput <-
    paste(name, "/data/", dir, "/", baseFile, ".txt", sep = "")
  w2vCleaned <-
    paste(name, "/data/", dir, "/", baseFile, "_cleaned.txt", sep = "")
  w2vBin <-
    paste(name, "/data/", dir, "/", baseFile, ".bin", sep = "")
  combinedTexts$text %>% write_lines(w2vInput)
  
  
  THREADS <- 3
  
  
  prep_word2vec(
    origin = w2vInput,
    destination = w2vCleaned,
    lowercase = T,
    bundle_ngrams = 1
  )
  
  if (!file.exists(w2vBin)) {
    w2vModel <- train_word2vec(
      w2vCleaned,
      output_file = w2vBin,
      vectors = 150,
      threads = THREADS,
      window = 3,
      iter = 10,
      negative_samples = 0
    )
  } else {
    w2vModel <- read.vectors(w2vBin)
  }
}
