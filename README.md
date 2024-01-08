# Perception_production_imitation
These are the scripts to clean and annotate the audio files and analyze the data


There are several scripts in this repository. 
Here is the workflow in which to use them. 

When downloading the data, each participant's data is stored in a subfolder, comprising a .csv file with their response, and a folder with .wav files (called binaries). 
We first have to deal with the .wav files. 

Step 1 is to cut the first 1.5 seconds out of the signal, since there is no relevant data in that snippet. This makes the automatic annotation a bit easier. 

That is what the cut_files.py script does. Simply paste the name of the subfolder you are working on into the folderpath variable. This script creates a folder in the binaries folder called "cut_stimuli" and stores the cut files in there. 

We can then paste the path to the cut_stimuli subfolder into the annotation R script (Vot_script). That script is literally 2 lines of code. 4 if you want to optimize the parameters first. 
The annotation script uses the getVOT library created by Rasmus Puggaard-Rode. More information about the package can be found here: https://github.com/rpuggaardrode/getVOT 

The next step is to loop thorugh the TextGrids and create a .csv file with the intervalls of interest. We then merge that file with the participant response data and create one df we can work with that is what the VOTannotation.py script does. Again, simply paste the folder you are working on into the folderpath variable. 

The last step then is to merge all the dfs from each participant into one big df. That is what the merge.py script does. 

finally, all further data cleaning and analysis is done in R with the cleanplotanalyze.r script. 
