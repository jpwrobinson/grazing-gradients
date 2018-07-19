

**Bite volumes**. I can extract the mean size and bite rate area from Table S1 in your MEPS paper, then put that together to estimate bite volume, separately for small + large scrapers. However, it would be more powerful to use the gape sizes + TL of individual fishes that you use to produce the table. Are those data available?



**Bite rates**

Going through this dataset has really helped me to think about how people scale individual fish data up to mean grazing rates. From your summed table on the right, it seems as though the Sum of total.bites data are the number of bites per hour on the grazable area, which is linked to the abundance of fish in the video. Your measure is the grazing pressure on the benthos, given a level of abundance. Right?

If your're interested is the total pressure on the benthic quadrat, we're interested in the per capita grazing of an individual fish. We're need to estimate the grazing rate (bites or bite volume) per fish, so need to control for species and fish size. So, we want the number of bites per hour of each observation, which means we need to correct for entry and exit time in the video.

For example, Fish ID = 1 in camera 1 (first row of the dataset), is 17 bites by cstriatus over 14 seconds. This would convert to 4,371 bites per hour. Corrected for area, this is 7,948 bites per hour per m^2 (but not sure if we need to correct for area in this approach). 

Some fishes have no exit recorded time so I think I'll have to drop those (1297 rows). Also, 12 fish have exit times before the entry time (e.g. row 17 in the excel sheet), and some have longer survey times (row 1441, 56 minutes) than you watch videos (24 mins). Guessing those are typos but not sure how to treat them.

Those procedures produce the attached plot, for bite rate ~ fish size separated by family. The data exclusions seem to drop a lot of siganid.

I'm still getting my head around the bite rate literature so please let me know if this seems like a strange approach. 



![]('../figures/explore/bite_rates_bysize.pdf')











