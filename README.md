# Russia-Ukraine War

## Methods
Using data from [Oryx's site](https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html), I've put together a quick tracker to visualize equipment losses since [Russia's February 24th invasion of Ukraine](https://en.wikipedia.org/wiki/Russo-Ukrainian_War). This is only equipment that is independently verified, as noted by Oryx:

> This list only includes destroyed vehicles and equipment of which photo or videographic evidence is available. Therefore, the amount of equipment destroyed is significantly higher than recorded here. Small arms, munitions, civilian vehicles, trailers and derelict equipment (including aircraft) are not included in this list. All possible effort has gone into discerning the status of equipment between captured or abandoned. Many of the entries listed as 'abandoned' will likely end up captured or destroyed. Similarly, some of the captured equipment might be destroyed if it can't be recovered. ATGMs and MANPADS are included in the list but not included in the ultimate count. The Soviet flag is used when the equipment in question was produced prior to 1991. 

Data is drawn from [this public google sheet](https://docs.google.com/spreadsheets/d/1bngHbR0YPS7XH1oSA1VxoL4R34z60SJcR3NxguZM9GI/edit?usp=sharing) which is updated based on the last update for each day. As such it is a lagging indicator, dependent not just on when equipment is lost, but when it is discovered and documented. 

Data is pulled daily from Oryx's site using [Daniel Scarnecchia](https://github.com/scarnecchia)'s [scraper tool](https://github.com/scarnecchia/scrape_oryx), and then pushed to the public google sheet, where synthetic calculations are performed for equipment categories (to preserve transparency). 

Points (red = Russia, blue = Ukraine) represent cumulative losses for each day, bars represent daily losses. The line represents a general additive model smooth on cumulative losses to date; the shaded grey band represents the 95% confidence interval based on extant variation (e.g. point scatter). A wider grey band means more uncertainty, a narrower grey band means less uncertainty. 

Please keep in mind that this is empirical, not interpretive, analysis. A concern raised about the available data is that it undercounts Ukrainian losses. This is possible not just because of bias (note that pro-Russian sources are monitored as well) but because areas under Russian control are less likely to have photo documentation. Fog of war is very real. There is no attempt here to use a modifier to adjust numbers - analysis is strictly empirical. Any bias in the original data will be reflected in the following analyses.

Lastly, if you would like to make edits to descriptions of these data feel free to create a pull request or a new issue. 


In addition to this main page, specific periods of the war have been isolated and treated seperatly, you can find them here: 

1. [Russian Invasion](1_invasion_2022.md)
2. [Russian Donbass Offensive](2_donbass_2022.md)
3. [Ukrainian Kharkiv/Kherson Offensives](3_kharkiv_kherson_2022.md)
4. [Russian Winter 2023 Offensive](4_winter_2023_offensive.md)
5. [Ukrainian Summer 2023 Offensive](5_summer_2023_offensive.md)
6. [Russian Winter 2023/2024 Offensive (current)](6_winter_2023_offensive.md)

You can also look at specific regions such as Avdiivka, Krynky, and Zaporizhizhia [here](naalsio_regions.md) gathered by [Naalsio](https://www.twitter.com/naalsio). 


## Total Equipment Losses
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/current_total.jpg?)
Ukranian and Russian equipment losses started of equivalent, but Russians quickly began to lose more equipment by the third day of the war. 

![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/current_ratio.jpg?)
The evolving ratio of losses increased sharply to Ukraine's favor, though early swings are impacted by Oryx and his team keeping up with data. The ratio stabilized around ~3.5 for most of the first few months of fighting, and reached as high as 4:1 following the Kharkiv Offensive. After that ratios declined to ~3.12 by the start of 2023. 

## Maps
Map data is provided using a Google maps base layer with troop locations from [Henry Schlottman](https://twitter.com/HN_Schlottman)'s [GitHub repo](https://github.com/simonhuwiler/uawardata). Fire data comes from [NASA FIRMS](https://firms.modaps.eosdis.nasa.gov) VIIRS satellite.  

### Tokamk Axis
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Maps/tokmak_map.jpg?)
Tokmak is the main axis Ukrainians are focusing on in their summer 2023 offensive. 

### Velyka Novosilka Axis
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Maps/velyka_novosilka_map.jpg?)
A supporting axis for the Ukrainian Summer 2023 summer offensive is on the Zaporhizhizhia-Donetks oblast border. 

### Bahkmut Map
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Maps/bakhmut_map.jpg?)
Fighting has been intense in Bahkmut since the summer of 2022. 

### Kupyansk Map
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Maps/kupyansk_map.jpg?)
Russia launched a localized offensive near Kupyansk in summer of 2023, though mainy gains have since been reversed. 

## Fire Radiative Power
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/axis_firms_summary_plot.jpg?)

An extended discussion of how to evaluate these analyses are [here](https://bleedrake.medium.com/what-does-satellite-infrared-data-tell-us-about-the-evolving-russian-strategy-in-its-ukraine-99672ae8e4cd). In general, the fire radiative power plots (FRP) are a useful guide to activity, with the lull in IR emissions in late 2022/early 2023 representing a change in how Russia used its artillery. 



## Destroyed Equipment
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/current_destroyed.jpg?)
Destroyed Russian equipment outpaced destroyed Ukranian equipment by the second day of the war, though there are signs it is starting to taper. 

![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/destroyed_ratio.jpg?)
The ratio of destroyed equipment was north of 4:1 until after the Kharkiv Offensive, after which it starts descending to its present value of ~3.1:1

## Abandoned Equipment
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/current_abandoned.jpg?)
Ukranians abandoned equipment more readily in the early days fo the war, but by the third day this rate plateaued. Russian abandonments increased sharply on the third day, and began to taper at the end of the first week.

![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/abandoned_ratio.jpg?)
Russian equipment abandonment spiled to a ratio of 9:1 in summer of 2022 before lowering to near 5:1, where it has remained until spring of 2023. 

## Captured Equipment
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/current_captured.jpg?)
Russians have seen a much higher rate of equipment capture since the start of the war, with a sharp increase in the second week, though this has begun to taper by the start of the third week. 

![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/captured_ratio.jpg?)
Capture ratios are fairly stable at 2.5:1 untl the Kharkiv Offensive in early September 2022, after which they increase to almost 3.5:1. Since then, this metric has been trending downward in the range of ~3.1:1

## Damaged Equipment
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/current_damaged.jpg?)
Damaged equipment for Russia has been a more or less linear increase as the war has worn on. Ukraine initially had low visually confirmed damages until after its Kharkiv Offensive, after which it rise at a rate comparable to Russia's. 

![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/damaged_ratio.jpg?)
The damaged ratio is Ukraine's least favorable metric - damages fluctuated around 3:1 until after the Kharkiv Offensive, where it fell below 2:1.

The degree to which Russians have lost more equipment (in every category type) is very striking. It is partially a product of Ukrainians focusing on documenting Russian losses, though this can't be a complete explanation. 


## Equipment Net Changes
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/current_absolute_total.jpg?)
This calculation subtracts units captured by each combatant with their other loss types (destroyed, damaged, and abandoned). It is arguable if a proportion of abandoned should be included as captures. Ukraine has a net positive change in equipment - meaning that known visible evidence has more cases of new captured eqipment that of losses. Russia however has lost many more units than it has acquired from Ukraine. 

## Ratios
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/ratio_grid.jpg?)
This plot shows the ratios of losses (e.g. proportion of total) for Russia and Ukraine for each loss type.  

# Raw Equipment Losses
"Raw" refers to a specific type of vehicle, such as a tank or armored personnel carrier

## Tanks
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/current_tanks.jpg?)
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/tank_ratio.jpg?)
Tank losses were equivalent in the first four days of the war, with Russian tank loses increasing sharply thereafter, though a jump in Ukranian tank losses can be seen at the start of the second week of the war. Russian tank losses have remained above 3:1 since the start of the war. 

![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/current_percent_total_tanks.jpg?)
This graph, however, highlights the challenges Ukraine still faces. When the sheer scale of [Russian tanks](https://inews.co.uk/news/world/russia-tanks-how-many-putin-armoured-forces-ukraine-nato-explained-1504470) are considered (13,300 vs. 2,100 for Ukraine), the steep Russian losses are not yet bringing parity. In general, Ukraine loses 1 tank for every 3 it takes from Russia. This ratio has to get to 4 or higher to be sustainable. Note that this estimate factors in verified tank captures by both Russia and Ukraine. 

![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/current_percent_deployed_tanks.jpg?)
If we consider estimates of [deployed Russian tanks](https://en.as.com/en/2022/02/24/latest_news/1645729870_894320.html) instead of their total (2,840 vs. 2,100 for Ukraine), the picture is not as dire for Ukraine. This estimate likely is closer to the battlefront picture, as not all Russian tanks could be deployed at once, though Russia can sustain attrition longer than Ukraine. Note that this estimate factors in verified tank captures by both Russia and Ukraine. 

![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/current_percent_total_tanks_baseline.jpg?)
If we consider estimates of [deployed Russian tanks](https://en.as.com/en/2022/02/24/latest_news/1645729870_894320.html) fixed to their total inventory, it is clear that Ukraine is adding to its tank fleet near the rate Russia is losing theirs. Though note that this measure assumes Russia has the capability to use all 13,300 of its tanks in Ukraine if needed. 

![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/current_percent_deployed_tanks_baseline.jpg?)
If we consider estimates of [deployed Russian tanks](https://en.as.com/en/2022/02/24/latest_news/1645729870_894320.html) fixed to their starting number, the picture is considerably more positive for Ukraine. Instead of recording only losses, this fixes the baseline (2,840 for Russia vs. 2,100 for Ukraine) and factors both tank losses and gains (e.g. captures). By this metric Russia has lost well over 5% of its tanks while Ukraine has supplemented its by 2% (e.g. captured more than it has lost).

## Armored Fighting Vehicles (AFV)
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/current_afv.jpg?)
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/afv_ratio.jpg?)
Armored Fighting Vehicles also see parity in losses in the first 4 days of the war, though Russian losses increase less dramatically than is seen with tanks. Note that this estimate factors in verified AFV captures by both Russia and Ukraine. The ratio has remained 3:1 since the start of 2023.

![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/current_percent_afv.jpg?)
When normalized to standing inventory, Russia's losses are a less significant fraction of their total inventory compared to Ukraine. This highlights the steep challenge Ukraine faces. In general, Ukraine loses 1 AFV for every 3 it takes from Russia. This ratio has to get to 4 or higher to be sustainable. 

![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/current_percent_total_afv_baseline.jpg?)
If we consider estimates of [deployed Russian AFV](https://en.as.com/en/2022/02/24/latest_news/1645729870_894320.html) fixed to their total inventory, Ukraine is adding modestly to its AFV inventory at the same rate which Russia is losing its AFV. 

## Artillery
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/current_artillery.jpg?)
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/artillery_ratio.jpg?)
The ratio of Russian to Ukrainian artillery losses is less than 3:1 since the start of 2023.

## Infantry Fighting Vehicles (IFV)
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/current_ifv.jpg?)
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/ifv_ratio.jpg?)
Ukraine lost more Infantry Fighting Vehicles in the first two days of the war, with Russian losses accelerating linearly until the end of the second week. The ratio has drifted down from 5:1 to 3:1 for the duration of the conflict. 

## Armored Personnel Carriers (APC)
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/current_apc.jpg?)
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/apc_ratio.jpg?)
Ukraine lost more Armored Personnel Carriers in the first two days of the war, with Russian losses accelerating linearly until the end of the second week, though the differential is less dramatic than with AFVs. The ratio has seen a secular decline since, and is approaching 1:1.

## Infantry Mobility Vehicles (IMV)
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/current_imv.jpg?)
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/imv_ratio.jpg?)
Infantry Mobility Vehicle losses are more or less equivalent between Ukranian and Russian forces through the first two weeks of the war. Ukraine has steadily lost more as the war has continued. 

# Synthetic Equipment Losses
"Synthetic" refers to a combination of vehicle types to form a theme - such as aircraft (fighters + helicopters + drones) or anti-aircraft (SAM + MANPADS), etc. 

## Aircraft
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/current_aircraft.jpg?)
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/aircraft_ratio.jpg?)
Ukraine has lost aircraft linearly, while Russia lost them almost exponentially in the first two weeks of the war, though this tapered sharply and recently passed below a 2:1 loss ratio. This calculation includes fighters, helicopters, and drones. 

## Anti-air
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/current_antiair.jpg?)
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/antiair_ratio.jpg?)
Ukraine sustained higher rates of loss of anti-air systems through the first week of the war, though Russian losses have since outpaced them by the second week of the war. Losses of these systems has been near 1:1 for most of the conflict. This calculation includes SAMs, self-propelled anti-aircraft guns, radar, and jamming systems. 

## Armor
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/current_armor.jpg?)
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/armor_ratio.jpg?)
Russian and Ukranian armor losses were equivalent in the first three days of the war, with Russian losses significantly outpacing Ukraine's since by more than a 3:1 ratio. This calculation includes both tanks and armored fighting vehicles.

![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/current_percent_total_armor_baseline.jpg?)
If we consider estimates of [deployed Russian armor](https://en.as.com/en/2022/02/24/latest_news/1645729870_894320.html) fixed to their total inventory, Ukraine is adding modestly to its armor inventory at the same rate which Russia is losing its Armor. 

## Infantry
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/current_infantry.jpg?)
Infantry losses were higher among Ukranians at the start of the war, with Russian losses increasing sharply early in the conflict. Ukranian losses taper in the second week. The secular trend has been downward, with the ratio approaching 2:1. This calculation includes infantry fighting vehicles, armored personnel carriers, and infantry mobility vehicles.

## Vehicles (standard transportation)
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/current_vehicles.jpg?)
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/vehicle_ratio.jpg?)
Ukraine lost more vehicles on the first day, with Russian losses accelerating early on. This calculation includes all non-specialized vehicle types. 

## Logistics Systems
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/current_logistics.jpg?)
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/logistics_ratio.jpg?)
Ukraine has lost few logistics systems, while Russian losses increased linearly until the end of the second week when they began to taper. This ratio remains heavily assymetrical at over 6:1. This calculation includes engineering and communications vehicles.

#Analysis
By grouping synthetic vehicles, we can see how different systems have been prioritized by Russia or Ukraine, providing a glimpse into strategy. 

## Loss Type
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/current_loss_type.jpg?)
These ratio plots use Russian equipment losses as the numerator and Ukranian losses as the denominator. The higher the bar, the higher the proportion of Russian losses. While Russians have abandoned vehicles at a higher rate, destructions remain the highest differential between the two armies. 

## Unit Type
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/current_unit_type.jpg?)
Here, Ukranian strategy is abundantly clear. It has targeted Russian logistics operations (higher bar = more Russian losses) to an overwhelming degree. Russians have focused on anti-air systems (lower bar = more Ukranian losses), though remarkably, despite this focus, Ukraine still has an almost 2-1 edge on taking these systems out. 

## More Maps
Map data is provided using a Google maps base layer with troop locations from [Henry Schlottman](https://twitter.com/HN_Schlottman)'s [GitHub repo](https://github.com/simonhuwiler/uawardata). Fire data comes from [NASA FIRMS](https://firms.modaps.eosdis.nasa.gov) VIIRS satellite.  

## Battle of Kyiv
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Maps/battleofkyiv.gif?)
The [Battle of Kyiv](https://en.wikipedia.org/wiki/Battle_of_Kyiv_\(2022\)) concluded at the beginning of April 2022 in a Ukrainian victory. NASA FIRMS analysis indicates that the majority of detectible fire from fighting was in the last two weeks of the conflict (March 15th - April 1st).

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


## Northern FIRMS Summary
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/north_firms_summary_plot.jpg?)
This includes Kyiv and the Donbas. Summing megawatts of energy radiated from regions of Ukraine helps highlight the independent war record. The Battle of Kyiv is clearly visible, with detectable fires concentrated in the last two weeks (March 15th - April 1st). The Battle of Donbas, following a repositioning of Russian forces, shows a more brief but just as intense spike in radiation confined to the first two weeks of May, followed by continues IR emissions into the fall. These IR emissions drop off rapidly following the successful Ukrainian Kharkiv offensive in the region in early September 2022. 

## Southern FIRMS Summary
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/south_firms_summary_plot.jpg?)
This region includes Kherson, Zaporizhizhia, and Crimea. Maximum IR activity occured in late summer 2022, with epidosodic increases likely related to specific attacks. 

# Conclusions
I am not a military expert, but by the second week of the war it is clear that the Russian objective of suppressing Ukranian aircraft and anti-air failed, leading to contested airspace. In contrast, Ukraine succeeded in interfering with Russian logistics, evidenced by their focus on logistics vehicles and fuel lines. This has ground the Russian advance to a halt by the third day, with limited change since then. 

Russia expended tremendous amounts of ammunition in spring and summer of 2022. While they were able to take some territory following their spring breakthrough at Popasna, this has exhausted some offensive combat capability. Since the successful Ukrainian counter-offensive in Kharkiv oblast in September 2022, IR activity as detected by FIRMS sattelites has dropped off dramatically in all regions, suggesting a change in Russian strategy. 
