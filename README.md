The contents for the short-lecture talk for CLM were nearly all derived using the contents in this repository. 

If you are simply looking for a copy of the talk, please check out 'Scripts/Presentation.html'. 
You should be able to download this to a local location and render it from any web browser (e.g. Firefox). 
Once you have opened it 'ctrl + w' should bring it to widescreen mode, while 'ctrl + f' should bring it to full screen. 
To zoom in you can 'ctrl +' just like you would be zooming into anything else. 

If you are interested in seeing the code which was used for making the figures in the introduction you can find that in an R script saved with the file name 'Scripts/plots.R'. 
This script should be able to run pretty well anywhere on your computer, but you will need to modify the working directory which I set at the start of it. 
If you don't modify the working directory, and create a folder 'images' to hold the graphs than they will be saved somewhere weird on your computer. 

While the code to render introduction images *probably* works by just copying the script, it is best to **git clone** the repository. 
Considerable information exists on cloning a Github repository elsewhere on the web. 

## If you want *everything*. 

It is easiest just to *git clone* this repo sagesteppe/CLM_2024_Veg_Ecology.
This will mean that you get a mirror image of the computer set up which was used, which is the smoothest way to reproduce the analysis. 

This repository contains the code used to:

1) Generate the short lecture for this session.  
2) Analyze the data collected in the field. 
3) Present the results of the Analysis. 

Data were acquired from 12 quadrats (1x2m) in two major habitat types. 
Upland White Oak and Swamp White Oak.  

All code is prepared using the statistical programming language R. 
The presentations require Rstudio and Rmarkdown to render. 