# Fluctuating selection on behaviour in a wild population of red squirrels

[![DOI](https://zenodo.org/badge/5166/rwtaylor/2014-female-selection.png)](http://dx.doi.org/10.5281/zenodo.10908)

Date: July 16, 2014

Included is the code for our manuscript concerning selection on female red squirrel behavior.

Please email Ryan Taylor (ryan@ryantaylor.net) if you have any questions or comments.

## How to run this code?
The code split into 6 sections in seprate knitr markdown files. Knitr allows R code to be interwoven with markdown text. All analyses are run in R. The entire analysis takes a few hours to run on a 12 core machine (step 05_competition takes most advantage of parallelization).

  1. Install all necessary R packages by running the rpackages.R file.
     
         source("rpackages.R")
         
  2. 'knit' each .Rmd file. Or, source the 'knit_all.R' file.
      
        library(knitr)
        knit('01_pca.Rmd')
        knit('02_BLUPs.Rmd') # This will take about an hour to finish
        knit('02b_Priors.Rmd')
        knit('03_merge_fitness.Rmd')
        knit('04_temporal_gradients.Rmd')
        knit('05_competition.Rmd') # 60 min on a 12 core machine
      
  3. To convert the resulting markdown files into html or pdfs, you will need pandoc and, additionally, latex for pdfs. Knitr has a nice convenience function to call pandoc from within R.

        knitr::pandoc('01_pca.md')
        knitr::pandoc('02_BLUPs.md')
        knitr::pandoc('02b_Priors.md')
        knitr::pandoc('03_merge_fitness.md')
        knitr::pandoc('04_temporal_gradients.md')
        knitr::pandoc('05_competition.md')
  
  4. Pandoc can combine all analyses into one html or pdf
        
        # html
        pandoc --default-image-extension=png \
        --template=templates/html.template --toc --toc-depth=4  \
        -f markdown -t html -o 00_all_analyses.html *.md
        
        # pdf
        pandoc --default-image-extension=pdf \
        --template=templates/latex.template --toc --toc-depth=4 -s -S \
        --latex-engine=xelatex  -f markdown -t latex -o 00_all_analyses.pdf *.md