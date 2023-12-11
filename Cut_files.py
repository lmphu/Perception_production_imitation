# split the audio files in half and just save the second half... 
from pydub import AudioSegment
import os
import glob



# ok, so I want to cut the first 1.5 seconds from the file 
# assign the folder to work in
folderpath = r"/Users/lenahuttner/Desktop/Dissertation/Exp4_data/Session_913200/binaries" #example folder name
out_path = os.path.join(folderpath,"./cut_stimuli") #create the folder in which to store the cut files
os.mkdir(out_path)
matching_files = glob.glob(os.path.join(folderpath, "*Post_rec.wav")) + glob.glob(os.path.join(folderpath, '*Pre_rec.wav')) #these are the files I want to work on


#loop through everything
for file in matching_files: 
            
   audio_file = AudioSegment.from_wav(file)
   stimulus_speech = audio_file[-1800:]
   filename = os.path.splitext(os.path.basename(file))[0] + "_speech_stimulus" + '.wav'
   output_path = os.path.join(out_path, filename)
   print(filename)
   stimulus_speech.export(output_path, format="wav")
   
