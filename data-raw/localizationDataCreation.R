#Code to extract nine 10-second recordings for the vignette.
#Need to have ffmpeg installed, for conversion.

library(tuneR)
library(tools)

#Specify output folder
outputFolder = './data/'

#Path to files (on external hard drive)
files = c("E:/TDLO/TDLO-001/TDLO-001-122/TDLO-001-122_0+1_20200617$090000.wav",
          "E:/TDLO/TDLO-001/TDLO-001-123/TDLO-001-123_0+1_20200617$090000.wav",
          "E:/TDLO/TDLO-001/TDLO-001-124/TDLO-001-124_0+1_20200617$090000.wav",
          "E:/TDLO/TDLO-001/TDLO-001-139/TDLO-001-139_0+1_20200617$090000.wav",
          "E:/TDLO/TDLO-001/TDLO-001-140/TDLO-001-140_0+1_20200617$090000.wav",
          "E:/TDLO/TDLO-001/TDLO-001-141/TDLO-001-141_0+1_20200617$090000.wav",
          "E:/TDLO/TDLO-001/TDLO-001-156/TDLO-001-156_0+1_20200617$090000.wav",
          "E:/TDLO/TDLO-001/TDLO-001-157/TDLO-001-157_0+1_20200617$090000.wav",
          "E:/TDLO/TDLO-001/TDLO-001-158/TDLO-001-158_0+1_20200617$090000.wav")

#Adjustment, in seconds, to correct for file offsets.
adj = c(0,0,0,0,0,1,0,0,0)

#Extract and copy wav files.
i=1
for(i in 1:length(files)) {
  #Note I am taking the time period from 11 to 18 seconds in the recording (9:00:11).
  #Yet I am keeping the original file names,
  #since this is just an example, and I would want to emphasize that people should always start
  #recordings on the minute when possible.
  wav <- readWave(files[i], from = 11 - adj[i], to = 18 - adj[i], units = 'seconds')
  newFile <- paste0(outputFolder, basename(files[i]))
  writeWave(wav, filename = newFile, extensible = FALSE)
}



#list files.
wavs <- list.files('./data/', full.names=T)

#Run mp3 to wav conversion using ffmpeg.
i=1
for(i in 1:length(wavs)) {
  orig_name <- basename(file_path_sans_ext(wavs[i]))
  newname <- paste0('Ex-', i,substr(orig_name,13,nchar(orig_name)))
  newname <-  paste0(dirname(wavs[i]),'/', newname, '.mp3')
  CALL <- paste('ffmpeg -i', wavs[i], "-b:a 96k", newname)
  system(CALL)
}

#Then manually delete wavs.




