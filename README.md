# grazing-gradients

This is the archive of LECReefs collaborative grazing project, learning Rmd and Git through writing a scientific paper.

Read the published paper: https://besjournals.onlinelibrary.wiley.com/doi/10.1111/1365-2435.13457

Reproduce the manuscript from code: https://github.com/jpwrobinson/grazing-grads

## Meeting 7 - 14th November 2018

Items to discuss:

* reef habitat: slope, flat, depth may be confounding analyses [REMOVE GBR NON-SLOPE]
* Figure 1: map with biomass? (see Roff et al. 2018, Ecol. Monogr.) [YES]
* Figure 3: decoupling [YES]
* Introduction + study framing
* Results [ALL GOOD]
* Writing strategy [GOOGLE DOCS]

## Grazing functions - update 25th Oct 2018

I've applied current best practice for estimating cropping and browsing functions. We are waiting on Andy Hoey for bite size data but I think he's in the field just now. Just to summarise what's going on:

Cropper bite rates do not scale with body size, but we expect bite volume to increase with body mass. Marshell & Mumby (2015, J. Exp. Mar. Biol.) estimated algal consumption according to the energy requirements of herbivorous fish, which they took from a 1998 study of Caribbean fish and algal food quality (van Rooij et al. J. Fish. Biol.). Basically, cropping function can be worked out by:

1. predict daily algal consumption for a fish of given mass
2. scale up the hourly bite rate up to estimate number of bites in one day
3. estimate the algae consumed per bite
4. estimate the algae consumed per hour per hectare, given the observed bite rate

If you do this, cropping function is pretty much predicted by biomass, and bite rate information doesn't add much information to grazing function. But this is best practice - so it will be good to demonstrate why in situ feeding observations are really important.

For browsers, we don't have good information on nutritional quality of macroalgae. Best practice here is to estimate the mass standardized bite rate (Bellwood in several papers). This is just bite rate * body mass. Because we have very little species-level information on browser bites rates, this ends up with a very tight relationship between biomass and browsing function. Again, highlights the need for in situ feeding studies in different locations.

I expect the scraper function analysis will be strongest. This function is most well-studied, and we have feeding observations for 24 out of 27 observed species already.

Compile [03_function_models.Rmd](writing/models/03_function_models.Rmd) to see the figures behind this update.

***

## Referencing with Rmarkdown

We can use bibtex files and csl style files to generate reference lists in an R markdown document. Every reference software can export a bibtex library that contains unique 'citekeys' - we'll export our grazing-gradient library from Mendeley and keep the bibtex file in this repo.

To make stuff work - make sure that you have done the following in RStudio:

```
install.packages(c('knitcitations', 'bibtex', 'citr'))  
library(knitcitations); library(bibtex); library(citr)	
cleanbib()  
cite_options(citation_format = "pandoc", check.entries=TRUE)    
```

To find all of the citation styles, fork this repo to your account: github.com/Styles

Then, clone it to your computer through terminal:
```
git clone https://github.com/jpwrobinson/styles.git
```

Now, you have all of the citations styles locally on your computer - find the one that you'd like to use (e.g. Global Change Biology), and then copy that .csl file (e.g. global-change-biology.csl) into your PROJECT_NAME/ms (manuscript) folder. This way it can be directly sourced while knitting the document.

What about cite while you write options? The ```citr``` library now does this for R Studio. Copied from https://libscie.github.io/rmarkdown-workshop/handout.html

"citr is an R package that provides an easy-to-use RStudio addin that facilitates inserting citations. The addin will automatically look up the Bib(La)TeX-file(s) specified in the YAML front matter. The references for the inserted citations are automatically added to the documents reference section.

Once citr is installed (install.packages("citr")) and you have restarted your R session, the addin appears in the menus and you can define a keyboard shortcut to call the addin."

***

## Meeting 6 - 4th Oct 2018 - biomass models, main text Figure, introduction

Jeneen, Jan, James:

* discuss biomass models. Need to create main text figure - what should this look like? *4 PANEL, Jeneen on it.*
* Bite rate data and converting biomass to function *Explained and waiting on Andy's comments*
* Introduction + framing of the paper *Brainstorm needed*

*Jan also starting first results paragraph*

***

## Meeting 5 - 21st Sept 2018 - biomass models

Jeneen, Jan, James discussed first model results. Decided to improve benthic + fishing predictors by considering alt. regimes (turf, sand, rubble) and fishable biomass gradient. 

James received bite rate data. Working on bite rate predictive model.

Next steps in [#14](/../../issues/14)

***

*summer was great*

***

## Meeting 4 - 30th May 2018 - bite rate literature and Rmd collaborating

*Tasks*

* Rmd - some crazy shit going on. Let's fix gitignore.
* Bite rate literature - what's the species count at? What kind of data do we have?
* Bite rate functions - lots of (slightly) different functions. Carbon intake, bioerosion, consumption rates. What are we most interested in? Perhaps focus on biomass for now and convert later when the patterns are clearer.

***

## Meeting 3 - 23rd May 2018 - checking in with Rmd and data questions, and bite rate literature search

*Tasks*

* Rmd - troubleshooting, compiling, useful functions (include=F)
* Data questions - check with Issue. Can we add any more?
* Bite rate literature - propose keeping master sheet of bite rate studies by species

It's pretty difficult to transition from R to Rmarkdown, and writing text next to code. R-markdown is very powerful (I only use the basics): here's [R Studio's Rmarkdown cheatsheet](https://www.rstudio.com/wp-content/uploads/2015/02/rmarkdown-cheatsheet.pdf)

***

## Meeting 2 - 15th May 2018 - R markdown for reproducible notebooks and reports

*Tasks*

* Git problems?
* Introduce R-markdown
* Talk about UVC dataset structure
* Species lists and bite rate literature
* Referencing system. Mendeley?

This thing you're reading is a markdown file. It's designed to encourage fast writing with neat formatting, but it functions like a simple text document. Github likes Markdown, so when we write stuff here it gets converted into a simple, well-structured readme file. 

[Check here for all your Markdown tips](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet). All you really need are different numbers of hashtags, to separate out sections. We've been editing this file in a basic text editor, but you can get a free markdown viewer if you want to preview how things will get formatted online. I use [Typora](https://typora.io/)

# Section 1

## Section 2

### Section 3

#### Section 4

##### Section 5

etc.

R Studio has also developed an R - markdown hybrid. It's designed to write quick, readable reports that contain R code, data, figures and tables. [Some examples of what you can do with R markdown reports.](https://rmarkdown.rstudio.com/gallery.html)

We're going to use R-markdown to explore the UVC data, so we can easily share our findings among the group. The document will serve as a lab notebook that keeps track of everything we're working on, growing over time as we figure out how herbivore functions vary across the Indian Ocean.

## Useful bash commands

We won't be using bash for much more than git pull, add -A, commit -m 'message', and git push. Sometimes you will need to navigate around folders and check where you are. For that:

``` 
cd     ## this is the change directory command
cd ..   ##  move 'up' your file structure
cd Documents/git_repos   ##  move into Documents, and then git_repos

pwd      ### 'print working directory'

ls      ## list files in your working directory
ls -a     ## list 'all' files, including hidden ones (like .gitignore)
ls -l      ## list files and some metadata (when they were created, how large they are)
```


## Herbivore metadata

All cleaned herbivore data is at data/wio_gbr_herb_master.Rdata, with the full species list in data/herbivore_species_list.csv. 

raw-data contains all raw csv files from Nick, and these were cleaned by James in scripts/clean_merge_graham_data.R. I'd suggest only use the Rdata data file and subsetting to relevant regions. For example,

```
load('data/wio_gbr_herb_master.Rdata')
str(herb) ## check structure of data frame
unique(herb$dataset) ## check regions in herb

## subset to seychelles data only
seychelles <- herb[herb$dataset == 'Seychelles',]

## write to csv
write.csv(seychelles, file = 'data/seychelles_master.csv')
```

We decided to each explore different region datasets:

* Seychelles - James
* Chagos - Jeneen
* Maldives - Jan
* GBR - Sam

**Column names in wio_gbr_herb_master.Rdata**

*In this dataset, each row is an observation of an individual fish*

*Location stuff* 

date = survey data (this is not standardized, e.g. years for Seychelles and missing for Chagos)  
dataset = region surveyed     
reef = area surveyed (e.g. island or reef section)   
site = site name within area surveyed   
site.number = numeric site names   
management = fished or unfished   
habitat = habitat type (not standaridized, something to discuss)   
unique.id = unique identifier for each transect, indicating reef + site + transect number   
depth = site depth (metres)   
transect = transect number within each site   
transect.area = transect area in m^2   

*Fish stuff*  

family = fish species family   
species = species name   
FG = functional group (grazer, browser, scraper)   
length.cm = length of fish (cm)   
mass.g = weight of fish (grams)   
biomass.kgha = biomass of fish in kg per hectare   
abundance.500m2 = abundance of fish in 500 m^2   

***

## Meeting 1 - 2nd May 2018 - Github setup

We'll be following [Git lessons from Software Carpentry](http://swcarpentry.github.io/git-novice/)

- [ ] Conceptual workflow: 
```
git pull
git add -A
git commit -m 'commit message'
git push
```
- [ ] Git housekeeping - line endings, user names, text editor
- [ ] Github site, adding collaborators, issues system
- [ ] ```git clone```
- [ ] Edits, conflicts and commit history
- [ ] Git ignore
- [ ] Git with R Studio

*Troubleshooting*  

* [Line endings](https://help.github.com/articles/dealing-with-line-endings/#platform-all)
* Configuration issues: check ```git config --list```
* [Detached head](https://stackoverflow.com/questions/10228760/fix-a-git-detached-head?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa)

*Who uses Github and why?*

* [Canada/USA - hake fisheries assessment](https://github.com/andrew-edwards/hake-assessment)
* [Reproducible ecology paper example](https://github.com/dbarneche/fishEggSize)
* [Sharing model code - Aaron MacNeil fish recovery potential paper](https://github.com/mamacneil/Unfished)
	
