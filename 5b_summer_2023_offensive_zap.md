---
title: Tokmak Axis Summer 2023 Offensive
layout: template
filename: summer_2023_offensive.md
--- 

# Ukrainian Summer 2023 Offensive: Zaporizhizhia

[Interactive App here](https://bleedrake.shinyapps.io/zapApp/)

The following plots are based on the main project file, though they start their indexing at June 1st to match the current offensive. 

*Strategic Objective*: Eliminate or make unfeasible Russia's land bridge to Crimea
*Result*: Ongoing, but full success unlikely

## Methods
Using data from [Oryx's site](https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html), I've put together a quick tracker to visualize equipment losses since [Russia's February 24th invasion of Ukraine](https://en.wikipedia.org/wiki/Russo-Ukrainian_War). This is only equipment that is independently verified, as noted by Oryx:

> This list only includes destroyed vehicles and equipment of which photo or videographic evidence is available. Therefore, the amount of equipment destroyed is significantly higher than recorded here. Small arms, munitions, civilian vehicles, trailers and derelict equipment (including aircraft) are not included in this list. All possible effort has gone into discerning the status of equipment between captured or abandoned. Many of the entries listed as 'abandoned' will likely end up captured or destroyed. Similarly, some of the captured equipment might be destroyed if it can't be recovered. ATGMs and MANPADS are included in the list but not included in the ultimate count. The Soviet flag is used when the equipment in question was produced prior to 1991. 

Data is drawn from [this public google sheet](https://docs.google.com/spreadsheets/d/1bngHbR0YPS7XH1oSA1VxoL4R34z60SJcR3NxguZM9GI/edit?usp=sharing) which is updated based on the last update for each day. As such it is a lagging indicator, dependent not just on when equipment is lost, but when it is discovered and documented. 

Data is pulled daily from Oryx's site using [Daniel Scarnecchia](https://github.com/scarnecchia)'s [scraper tool](https://github.com/scarnecchia/scrape_oryx), and then pushed to the public google sheet, where synthetic calculations are performed for equipment categories (to preserve transparency). 

Points (red = Russia, blue = Ukraine) represent cumulative losses for each day, bars represent daily losses. The line represents a general additive model smooth on cumulative losses to date; the shaded grey band represents the 95% confidence interval based on extant variation (e.g. point scatter). A wider grey band means more uncertainty, a narrower grey band means less uncertainty. 

Please keep in mind that this is empirical, not interpretive, analysis. A concern raised about the available data is that it undercounts Ukrainian losses. This is possible not just because of bias (note that pro-Russian sources are monitored as well) but because areas under Russian control are less likely to have photo documentation. Fog of war is very real. There is no attempt here to use a modifier to adjust numbers - analysis is strictly empirical. Any bias in the original data will be reflected in the following analyses.

Lastly, if you would like to make edits to descriptions of these data feel free to create a pull request or a new issue. 


## Total Equipment Losses
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/zaporizhizhia/current_total.jpg?)

![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/zaporizhizhia/current_ratio.jpg?)

## Destroyed Equipment
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/zaporizhizhia/current_destroyed.jpg?)

![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/zaporizhizhia/destroyed_ratio.jpg?)


# Lost Equipment Locations
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Maps/orkhiv_all.jpg?)
Along the Tokmak axis, the Russia:Ukraine disparity is largest, with Ukraine's losses far outpacing Russia's. Note that this analsis is restricted only to losses with known coordinates. Note however that even given the disparity, far more losses happen south of the contact area, suggesiting a major difference in deep stike capabilities. 

![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Maps/vn_all.jpg?)
The Velyka Novosilka axis is in less strategically valuable terrain, but here Russian losses outpace Ukrainian ones only slightly. Note again the comparion in deep strike capabilities, with Russian losses occuring far south of the line of contact. 

# Raw Equipment Losses
"Raw" refers to a specific type of vehicle, such as a tank or armored personnel carrier

## Tanks
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/zaporizhizhia/current_tanks.jpg?)
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/zaporizhizhia/tank_ratio.jpg?)



## Armored Fighting Vehicles (AFV)
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/zaporizhizhia/current_afv.jpg?)
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/zaporizhizhia/afv_ratio.jpg?)


## Artillery
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/zaporizhizhia/current_artillery.jpg?)
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/zaporizhizhia/artillery_ratio.jpg?)


# Synthetic Equipment Losses
"Synthetic" refers to a combination of vehicle types to form a theme - such as aircraft (fighters + helicopters + drones) or anti-aircraft (SAM + MANPADS), etc. 

## Armor
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/zaporizhizhia/current_armor.jpg?)
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/zaporizhizhia/armor_ratio.jpg?)


#Analysis
By grouping synthetic vehicles, we can see how different systems have been prioritized by Russia or Ukraine, providing a glimpse into strategy. 

## Unit Type
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/zaporizhizhia/current_unit_type.jpg?)


## FIRMS Maps
### Tokamk Axis
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Maps/tokmak_map.jpg?)
Tokmak is the main axis Ukrainians are focusing on in their summer 2023 
. 

### Velyka Novosilka Axis
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Maps/velyka_novosilka_map.jpg?)
A supporting axis for the Ukrainian Summer 2023 summer offensive is on the Zaporhizhizhia-Donetks oblast border. 

### Bahkmut Map
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Maps/bakhmut_map.jpg?)
Fighting has been intense in Bahkmut since the summer of 2022.


### Kupyansk Map
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Maps/kupyansk_map.jpg?)
Russia launched a localized offensive near Kupyansk in summer of 2023, though mainy gains have since been reversed. 



Map data is provided using a Google maps base layer with troop locations from [Henry Schlottman](https://twitter.com/HN_Schlottman)'s [GitHub repo](https://github.com/simonhuwiler/uawardata). Fire data comes from [NASA FIRMS](https://firms.modaps.eosdis.nasa.gov) VIIRS satellite.  

## North Donbas and Kharkiv
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Maps/north_donbas_map.jpg?)
Russia has concentrated most of its combat forces in the Donbas attempting to breakthrough Ukranian lines established in 2014. FIRMS fire data indicates battles around Izyum, with some progress for Russia to the west. 

## South Donbas and Zaporizhizhia
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Maps/south_donbas_map.jpg?)
Russia has been using light infantry attacks against Ukranian settlements such as Bahkmut, resulting in much lower IR emissions compared to earlier fighting. 

## Kherson
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Maps/kherson_map.jpg?)
Kherson is an occupied city where the Russian advance was halted. FIRMS data does not indicate heavy combat in the area currently. 

## Zaporizhzhia Oblast
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Maps/zaporizhizhia_map.jpg?)
Zaporizhzhia is a comparatively quiet region, but there are isolated artillery strikes around Russian BTGs and missile strikes in population centers. 
