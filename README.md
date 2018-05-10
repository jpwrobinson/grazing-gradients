# grazing-gradients

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
Hi James
