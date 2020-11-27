#' @title extract_acoustics
#'
#' @description Function for sound analysis using Soudngen package which will take as input each mp3 file and will generate a csv file
#'
#' @param mp3_file Description of the mp3_file parameter
#' @param scientific_name Description of the scientific_name parameter
#' @param csv_file 
#'
#' @return write a dataframe with the important acoustic parameters
#' @export
#'#' 


extract_acoustics <- function(mp3_file, scientific_name, csv_file) {
  
  # Analyzing fundamental harmonic of each syllable
  ana <- analyze(mp3_file, plot = FALSE)
  anasyl <- filter(ana, voiced == TRUE)
  anasyl <- filter(anasyl, anasyl$dom != "NA")
  
  # Getting the syllables and bursts
  syl <- segment(mp3_file)
  length(syl$syllables$start) # Number of syllables
  sylen <- syl$syllables$sylLen # Getting the list of syllable lenghts
  
  pause <- syl$syllables$pauseLen # List of pause between syllables
  pause <- as.array(pause) 
  pause <- pause[1:length(pause)-1] # Extracting last value of the list (always NA)
  
  # Computing syllable rate
  rate <- 1/mean(pause)
  
  # Creating data frame with max, min, mean sd syllable lenght, syllable rate, 
  # mean dominant frequency, power of harmonics, harmonics to noise ratio, 
  # frequency of harmonics 1 to 3, entropy and slope
  # mean(sylen, na.rm=TRUE)
  
  # Should be generated and add associated name
  df <- data.frame(scientific_name,
                   max(sylen, rm.na = TRUE),
                   min(sylen, rm.na = TRUE),
                   mean(sylen, rm.na = TRUE),
                   sd(sylen),
                   rate,
                   mean(anasyl$dom, rm.na = TRUE),
                   max(anasyl$dom, rm.na = TRUE),
                   min(anasyl$dom, rm.na = TRUE),
                   sd(anasyl$dom),
                   mean(anasyl$harmonics, rm.na = TRUE),
                   max(anasyl$harmonics, rm.na = TRUE),
                   min(anasyl$harmonics, rm.na = TRUE),
                   sd(anasyl$harmonics),
                   mean(anasyl$HNR, rm.na = TRUE),
                   max(anasyl$HNR, rm.na = TRUE),
                   min(anasyl$HNR, rm.na = TRUE),
                   sd(anasyl$HNR),
                   mean(anasyl$f1_freq, rm.na = TRUE),
                   max(anasyl$f1_freq, rm.na = TRUE),
                   min(anasyl$f1_freq, rm.na = TRUE),
                   sd(anasyl$f1_freq),
                   mean(anasyl$f2_freq, rm.na = TRUE),
                   max(anasyl$f2_freq, rm.na = TRUE),
                   min(anasyl$f2_freq, rm.na = TRUE),
                   sd(anasyl$f2_freq),
                   mean(anasyl$f3_freq, rm.na = TRUE),
                   max(anasyl$f3_freq, rm.na = TRUE),
                   min(anasyl$f3_freq, rm.na = TRUE),
                   sd(anasyl$f3_freq),
                   mean(anasyl$entropy, rm.na = TRUE),
                   max(anasyl$entropy, rm.na = TRUE),
                   min(anasyl$entropy, rm.na = TRUE),
                   sd(anasyl$entropy),
                   mean(anasyl$specSlope, rm.na = TRUE),
                   max(anasyl$specSlope, rm.na = TRUE),
                   min(anasyl$specSlope, rm.na = TRUE),
                   sd(anasyl$specSlope))
  
  # Merging dataframe to full dataframe with all analyzed recordings data
  write.csv(df, file = csv_file)
}