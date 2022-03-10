# Russia-Ukraine War

Using data from [Oryx's site](https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html), I've put together a quick tracker to visualize equipment losses since [Russia's February 24th invasion of Ukraine](https://en.wikipedia.org/wiki/Russo-Ukrainian_War). 

Data is drawn from [this public google sheet](https://docs.google.com/spreadsheets/d/1bngHbR0YPS7XH1oSA1VxoL4R34z60SJcR3NxguZM9GI/edit?usp=sharing)

## Total Equipment Losses
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/current_total.jpg)
Ukranian and Russian equipment losses started off equivalent, but Russians quickly began to lose more equipment by the third day of the war. 

## Destroyed Equipment
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/current_destroyed.jpg)
Destroyed Russian equipment outpaced destroyed Ukranian equipment by the second day of the war, though there are signs it is starting to taper. 

## Abandoned Equipment
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/current_abandoned.jpg)
Ukranians abandoned equipment more readily in the early days fo the war, but by the third day this rate plateaued. Russian abandonments increased sharply on the third day, and began to taper at the end of the first week.

## Captured Equipment
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/current_captured.jpg)
Russians have seen a much higher rate of equipment capture since the start of the war, with a sharp increase in the second week, though this has begun to taper by the start of the third week. 

The degree to which Russians have lost more equipment (in every category type) is very striking. It is certaintly possible this is a product of Ukranians focusing on documenting Russian losses, though this can't be a complete explanation. 

# Raw Equipment Losses
"Raw" refers to a specific type of vehicle, such as a tank or armored personell carrier

## Tanks
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/current_tanks.jpg)
Tank losses were equivalent in the first four days of the war, with Russian tank loses increasing sharpely thereafter, though a jump in Ukranian tank losses can be seen at the start of the second week of the war. 

## Armored Fighting Vehicles (AFV)
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/current_afv.jpg)
Armored Fighting Vehicles also see parity in losses in the first 4 days of the war, though Russian losses increase less dramatically than is seen with tanks. 

## Infantry Fighting Vehicles (AFV)
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/current_ifv.jpg)
Ukranian's lost more Infrantry Fighting Vehicles in the first two days of the war, with Russian losses acellerating linearly until the end of the second week. 

## Armored Personel Carriers (APC)
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/current_apc.jpg)
Ukranian's lost more Armored Personel Carriers in the first two days of the war, with Russian losses acellerating linearly until the end of the second week, though the differential is less dramatic than with AFVs.

## Infantry Mobility Vehicles (IMV)
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/current_imv.jpg)
Infantry Mobility Vehicle losses are more or less equivalent between Ukranian and Russian forces through the first two weeks of the war. 

# Synthetic Equipment Losses
"Synthetic" refers to a combination of vehicle types to form a theme - such as aircraft (fighters + helicopters + drones) or anti-aircraft (SAM + MANPADS), etc. 

## Aircraft
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/current_aircraft.jpg)
Ukranians have lost aircraft linearly, while Russians lost them almost exponentially in teh first two weeks of the war, though there are signs this is tapering sharpely. This calculation includes fighters, helicotpers, and drones. 

## Anti-air
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/current_antiair.jpg)
Ukranians sustained higher rates of loss of anti-air systems through the first week of the war, though Russian losses have since outpaced them by the second week of the war. This calculation includes MANPADS, SAMs, self-propelled anti-aircraft guns, radar, and jamming systems. 

## Armor
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/current_armor.jpg)
Russian and Ukranian armor losses were equivalent in the first three days of the war, with Russian losses significantly outpacing Ukranians since. This calculaton includes both tanks and armored fighting vehicles.

## Infantry
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/current_infantry.jpg)
Infantry losses were higher among Ukranians at the start of the war, with Russian losses increasing sharply since then. Ukranian losses taper in the second week. This calculation includes infantry fighting vehicles, armored personell carriers, and infantry mobility vehicles.

## Vehicles (standard transportation)
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/current_vehicles.jpg)
Ukranians lost more vehicles on the first day, with Russian losses accelerating since then. This calculation includes all non-specialized vehicle types. 

## Logistics Systems
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/current_logistics.jpg)
Ukranians have lost few logistics systems, while Russian losses increased linearly until the end of the second when they began to taper. This calculation includes engineering and communications vehicles.

#Analysis
By grouping synthetic vehicles, we can see how different systems have been prioritized by Russian or Ukranians, providing a glimpse into strategy. 

## Loss Type
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/current_loss_type.jpg)
These ratio plots use Russian equipment losses as the numerator and Ukranian losses as the denominator. The higher the bar, the higher the proportion of Russian losses. While Russians have abandoned vehicles at a higher rate, destructions remain the highest differential between the two armies. 

##Unit Type
![alt text](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/Plots/current_unit_type.jpg)
Here, Ukranian strategy is abundantly clear. It has targeted Russian logistics operations (higher bar = more Russian losses) to an overwhelming degree. Russians have focusesd on anti-air systems (lower bar = more Ukranian losses), though remarkably despite this focus Ukranian's still have an almost 2-1 edge on taking these systems out. 

# Conclusions

I am not a military expert, but by the second week of the war it is clear that the Russian objective of suppressing Ukranian aircraft and anti-air failed, leading to contested airspace. In contrast, Ukranian's succeeded in interfering with Russian logistics, evidenced by their focus on logistics vehicles and fuel lines. This has ground the Russian advance to a halt by the third day, with limited change since then. 
