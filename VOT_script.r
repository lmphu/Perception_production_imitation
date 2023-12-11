#this uses the getVOT library created by Rasmus Puggaard-Rode

library(getVOT)
#train the model on a list of parameters, these two lines get commented out after they have run once. 
my_directory = "/Users/lenahuttner/Desktop/Dissertation/Exp4_data/data_1697445516883/Session_824656/binaries/cut_stimuli"
optimized_params <- pos_setParams(directory='/Users/lenahuttner/Desktop/Dissertation/Exp4_data/data_1697445516883/Session_824694/binaries/cut_stimuli')
#have a look at how the script performs, also comment this out 
print(optimized_params)
#now here is the annotation function. I do this folder by folder, participant by participant
VOT2newTG(directory='/Users/lenahuttner/Desktop/Dissertation/Exp4_data/Session_913200/binaries/cut_stimuli', sign='positive', pos_params_list=optimized_params)
# sure, you could just loop through every single folder here, but I like to  
#do this particpant by participant and check the annotations directly afterward 
