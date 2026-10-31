# Annual comparisons

Over the course of a multi-year war, it is possible to identify trends in equipment loss and active fire data. Presenting results briefly here. first, FIRMS:

> **Updated 2026-10-30.** Several data problems were fixed and all annual tables and plots regenerated (see Caveats); the plots had also stopped refreshing after 2026-06-01 because the script failed partway through in the hourly job. The commentary below was written on 2026-06-01 and predates the 2026 summer data: August 2026 is now the busiest month on record in the eastern FIRMS window (about 24,000 detections, against about 15,600 in September 2024), and 2026 year-to-date runs above 2025 in every FIRMS window.

![FIRMS for Eastern Ukraine](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/annual/weekly_firms_east_yearcompare_smoothed.jpg?)
The dominant, but not exclusive, FIRMS signal is seasonality. However, there are noticeable breaks in trends. A sharp uptick can be seen at the end of February 2022 at the start of the war. The sharp drop in FIRMS signal in 2022 is also likely a war signal than a seasonal signal; it drops a month earlier than suceeding years, and is associated with the successful Kharkiv offensive by Ukraine which began in early September of that year.

For Eastern Ukraine, 2024 and 2025 saw the highest levels of FIRMS-relted activity. This is likely related to the multi-year Russian Donbass offensive. This activity peaked in September of 2024, but still saw elevated activity through to autumn of 2025. 2026 is, in contrast, much lower than the front was in the first half of 2024 and 2025; comparable only to 2023 before the current offensive started.

![Tank losses for Russia (top) and Ukraine (bottom)](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/annual/monthly_tanks_yearcompare.jpg?)
Tank losses for both sides also indicate a drop in activity in early 2026 for both sides.  Tank losses for both Russia and Ukraine are at a minima for the entire conflict most of the time.

![Infantry carrier losses for Russia (top) and Ukraine (bottom)](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/annual/monthly_infantry_carriers_yearcompare.jpg?)
The drop in infantry carriers (IFV + APC + IMV) mirrors the lower activity trend for Russia, but not Ukraine. If anything, Ukraine's losses have been increasing with time.

![Artillery losses for Russia (top) and Ukraine (bottom)](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/master/annual/monthly_artillery_yearcompare.jpg?)
Again, same trend as before; Russian losses are declining, while Ukrainian losses are picking up.

So, takeaways:
  - Russia tanks (monthly): 2022 had a March peak of ~315 and an October peak of ~240 — both invisible in the weekly
  smooth. The 2026 floor of 10–30/month is unambiguous against historical 60–140.
  - Russia artillery: 2025 (dark brown) is the lowest full-year line through most of the calendar — already the throttle
   year. 2026 tracks 2025 closely, not below it. The exhaustion-vs-throttle story for artillery looks like "Russia
  already ran low on artillery losses in 2025, before tanks collapsed in 2026."
  - Russia infantry carriers: April 2026 has a single ~190 spike against ~20–50 for the rest of the year — almost
  certainly an Oryx catch-up batch after a posting gap. The monthly absorbs most of this but not all.
  - Ukraine infantry carriers: 2026 (black) is actually running higher than 2023–24 across Jan–May (~75–145 vs ~30–100),
   supporting the "Ukraine attrition elevated in 2026" reading from the weekly view.


**Western-Ukraine note.** The daily pull uses NASA's
`J1_VIIRS_C2_Russia_Asia_24h` regional feed, which starts at lon 26°E.
The first weeks of the war (Feb 24 – May 6, 2022) came from global
files that also cover western Ukraine, which inflated 2022's
country-wide rate by ~14%. The country-wide window is therefore clipped
at 26°E for every year, so read it as "Ukraine east of 26°E", not a
true country-wide count.

## Caveats

- **FIRMS is an indirect proxy.** Active-fire detections include
  agricultural stubble burning (spring and autumn dominate), wildfires,
  industrial heat sources, and the satellite revisit cadence — all of
  which can dwarf combat-related fires. Use the multi-year trend across
  multiple windows; don't read month-to-month wiggles.
- **2026 is partial.** Through Oct 30 (303 days). The bar charts show
  absolute totals *and* per-day rates; 2026 now includes the
  summer/autumn burn season.
- **Oryx changed what it counts on 2026-05-04.** Reconnaissance UAVs and
  UGVs (860 Russian and 810 Ukrainian items) were dropped from Oryx's
  headline totals overnight. Russia's "captured" count fell by 332 at the
  same moment because many of those drones were captured; this was not
  a revision of earlier entries. The annual "units" figures therefore
  exclude trucks, recon UAVs and UGVs from *every* snapshot so years
  stay comparable. Subtracting only trucks, as before, made Russia's
  2026 total look like 105.
- **Oryx revises retrospectively.** Entries get reclassified or removed
  as visual evidence is re-examined; small day-to-day decreases are normal.
- **FIRMS gaps and overlaps.** Consecutive daily pulls overlap, so
  detections are de-duplicated. Days whose satellite passes no saved pull
  covers (missing or empty downloads, or a day whose last run came before
  the night pass; ~8% of days) are filled with the complete-day average of
  the same week or month instead of counting as quiet days.
- **Visually confirmed only.** Oryx counts only items with photo or video
  evidence, so totals are a floor on actual losses, not a true count.

## Top-line findings (as of the run date)

Read with the caveats above.

### Tanks lost per year (visually confirmed)

| Year | Russia | Ukraine |
|---|---|---|
| 2022 | 1,599 | 438 |
| 2023 | 1,008 | 283 |
| 2024 | 1,060 | 293 |
| 2025 | 633 | 355 |
| 2026 YTD | 150 | 102 |

Russia's annual tank attrition halved from the 2022 peak to 2025 — the
shift to motorbike/buggy/quad infantry assaults shows up clearly. The
Russia-to-Ukraine tank-loss ratio also compressed: ~3.6:1 in 2022 → ~1.8:1
in 2025 → roughly 1.5:1 in 2026 YTD.

### Units lost per year, excluding trucks, recon UAVs and UGVs

| Year | Russia | Ukraine |
|---|---|---|
| 2022 | 6,261 | 2,172 |
| 2023 | 4,104 | 1,763 |
| 2024 | 5,050 | 1,997 |
| 2025 | 3,185 | 3,067 |
| 2026 YTD | 907 | 1,909 |

### FIRMS detections per day, by window

| Year | Pokrovsk (tight) | Donbas oper. | Eastern wide | Ukraine east of 26°E |
|---|---|---|---|---|
| 2022 | 5.8 | 52.4 | 86.8 | 232.9 |
| 2023 | 3.1 | 21.0 | 51.3 | 161.3 |
| 2024 | 19.0 | 85.1 | 165.1 | 360.4 |
| 2025 | 17.2 | 57.5 | 96.4 | 239.5 |
| 2026 YTD | 19.4 | 69.5 | 146.0 | 325.8 |

_Table regenerated 2026-10-30 (2026 through Oct 29). The discussion below was written on 2026-06-01; see the note at the top._

**2024 is the loudest year by FIRMS across every window** — driven by the
Avdiivka collapse and the Pokrovsk push. 2025 backed off from that peak
but stayed well above 2022/2023. The 2026 YTD numbers are genuinely
lower, but a meaningful share of that gap is the missing summer/autumn
burn season — defer judgement until late 2026.

Adding the country-wide window mostly confirms what the eastern windows
show: the shape of the year-over-year curve is the same in all four
windows. That's reassuring — the macro pattern isn't a Donbas-only
artifact. The country-wide ratios (e.g. 2024/2023 ≈ 2.2×) are close to
the eastern-wide ratios (3.2×) and the Donbas-operational ratio (4.1×),
which suggests national-scale escalation in 2024 with the sharpest
spike concentrated on the Donbas axes.

The monthly Donbas plot is the most honest view of recent tempo: the
2024-summer peak around the Avdiivka–Pokrovsk axis is unmistakable, and
the summer-2025 peak shows the front re-intensifying after a winter
quiet period. The early-2026 months are tracking similarly to early
2025, not to early 2024.
