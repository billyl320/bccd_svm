#converting images for analysis in R
#importing custom module for analysis
import convert as cvt

#desired directories
#note that each class should be separated into different directories.
#however, for the fucntion to work, multiple directories should be specified.
#thus, an empty folder is utilized for this task
#the empty folder is called "none"

#----------------------------------------------------------
#Platelets class

plates = ["data/Platelets", "none"]
binary = 'data_binary/'

#name of .txt file
name = 'plates.txt'

#converting images
cvt.BinaryHistTXT(name, plates, binary)
cvt.BinaryShapesTXT(name[-0:-4], [binary+plates[0][5:]] )
#----------------------------------------------------------
#WBC class

wbc = ["data/WBC", "none"]
binary = 'data_binary/'

#name of .txt file
name = 'wbc.txt'

#converting images
cvt.BinaryHistTXT(name, wbc, binary)
cvt.BinaryShapesTXT(name[-0:-4], [binary+wbc[0][5:]] )

#----------------------------------------------------------
#RBC class

rbc = ["data/RBC", "none"]
binary = 'data_binary/'

#name of .txt file
name = 'rbc.txt'

#converting images
cvt.BinaryHistTXT(name, rbc, binary)
cvt.BinaryShapesTXT(name[-0:-4], [binary+rbc[0][5:]] )


#
