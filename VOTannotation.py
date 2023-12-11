

from praatio import textgrid 
import pandas as pd
import os
from os.path import join

#set the folder 
folder = r'/Users/lenahuttner/Desktop/Dissertation/Exp4_data/deer_repeat/Session_867989'
subfolder ='/binaries/cut_stimuli/tg'
ext = ('.TextGrid') #define the extension, I only want the script to run on files with this extension 
folderpath =(folder + subfolder)

 # make a function to extract the duration of the anotated interval from each tg file  
def getVOT(tg_input):     #name of function, run on ever tg_input file 
  VOTfile = [] # create empty list 
  VOTList = tg_input.tierDict["vot"].entryList #tell the script the name of the tg tier
  labelList = [entry[2] for entry in VOTList] # generally the thing we'll be interested in is the second interval, name 
  durationList = [] #create empty list to save the duration in
  for start, stop, _ in VOTList: #loop through the entries
      durationList.append(stop - start) # add duration to list
  VOTfile = (labelList + durationList) #put everything together in a file
  return VOTfile


results = []  #create empty list to store the results in
test = [] # create empty list and name it with a name that jsut exudes confidence in your script
for path, dirc, files in os.walk(folderpath):   # loop through the directory
 
    for file in files:  # for every file in the defined path
        if file.endswith(ext): # only run script on the text grids
            file_name = [] # empty list to store the file name in
            file_name.append(file) # add file name to list
            tg_input = textgrid.openTextgrid((join(folderpath,file)), False) # open the file
            test = (file_name + getVOT(tg_input)) # add the file name and the interval of interest to the list
            # print(test) #optional command to print the thing in the folder
            results.append(test) # add test to the resultus list as a nested list
VOT_df = pd.DataFrame(results, columns= ['file', 'intervall', 'VOT'] ) #create data frame from nested list.
VOT_df.to_csv((join(folder,'VOT_csv.csv')), index=True, header=True) # write the resulting df to .csv with header
#print(VOT_df) #print the resulting df
#print(results) #print the list for whatever reason??


def rename(df):
    #df = df.rename(columns = {1 :'file'}) #don't need that we have the file name in there
    df = df.replace( {'rec.*':'rec'}, regex = True )
    df['file'] = df['file'].astype(str)
    #df = df.replace({'.TextGrid': '.wav.pgp'}, regex = True)
    df.set_index('file')
    return(df)
#create column in the results frame with all the necessary info that has the same name as the other df
def combine_cols(df1):
    df1['file'] = df1['shadow_reco'].fillna(df1['Pre_reco'])
    df1['file'] = df1['file'].fillna(df1['Post_reco'])
    df1['file'] = df1['file'].replace({'rec.*':'rec'}, regex = True )
    df1['file'] = df1['file'].astype(str)
    df1.set_index('file')
    #now replace the strings in the file column
    return(df1)

file1 = "VOT" 
file2 = "trials"


df_VOT = pd.DataFrame()
df_exp = pd.DataFrame()
for root, dirc, files in os.walk(folder):  
    #for dir in dirs:
    #for subdirectory in subdirectories:
        for file in files:
            #print(os.path.join(root, file))
            if file.startswith(file1):
                df = pd.read_csv(join(folder,file), index_col=0)
                df_VOT = rename(df)
  
print(df_VOT)

for root, dirc, files in os.walk(folder):
        for file in files:
            if file.startswith(file2):
                df1 = pd.read_csv(join(folder,file)
                df_exp = combine_cols(df1)
               

df_production = df_exp.merge(df_VOT,how='left', left_on='file', right_on='file')

df_production.to_csv((join(folder,'df_production.csv')), index=True, header=True)
