# nohup R CMD BATCH --vanilla knit_all.R &
library(knitr)
knit('01_pca.Rmd')
knitr::pandoc('01_pca.md')

knit('02_BLUPs.Rmd')
knitr::pandoc('02_BLUPs.md')

knit('02b_Priors.Rmd')
knitr::pandoc('02b_Priors.md')

knit('03_merge_fitness.Rmd')
knitr::pandoc('03_merge_fitness.md')

knit('04_temporal_gradients.Rmd')
knitr::pandoc('04_temporal_gradients.md')

knit('05_competition.Rmd')
knitr::pandoc('05_competition.md')

## To combine all analyses into one big file
system("pandoc --default-image-extension=png --template=templates/html.template --toc --toc-depth=4  -f markdown -t html -o 00_all_analyses.html *.md")

system("pandoc --default-image-extension=pdf --template=templates/latex.template --toc --toc-depth=4 -s -S --latex-engine=xelatex  -f markdown -t latex -o 00_all_analyses.pdf *.md")
