# Annual comparisons

Over the course of a multi-year war, it is possible to identify trends in equipment loss and active fire data. Presenting results briefly here. first, FIRMS:

![FIRMS for Eastern Ukraine](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/annual/weekly_firms_east_yearcompare_smoothed.jpg?)
The dominant, but not exclusive, FIRMS signal is seasonality. However, there are noticeable breaks in trends. A sharp uptick can be seen at the end of February 2022 at the start of the war. The sharp drop in FIRMS signal in 2022 is also likely a war signal than a seasonal signal; it drops a month earlier than suceeding years, and is associated with the successful Kharkiv offensive by Ukraine which began in early September of that year.

For Eastern Ukraine, 2024 and 2025 saw the highest levels of FIRMS-relted activity. This is likely related to the multi-year Russian Donbass offensive. This activity peaked in September of 2024, but still saw elevated activity through to autumn of 2025. 2026 is, in contrast, much lower than the front was in the first half of 2024 and 2025; comparable only to 2023 before the current offensive started.

![Tank losses for Russia (top) and Ukraine (bottom)](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/annual/monthly_tanks_yearcompare.jpg?)
Tank losses for both sides also indicate a drop in activity in early 2026 for both sides.  Tank losses for both Russia and Ukraine are at a minima for the entire conflict most of the time.

![Infantry carrier losses for Russia (top) and Ukraine (bottom)](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/annual/monthly_infantry_carriers_yearcompare.jpg?)
The drop in infantry carriers (IFV + APC + IMV) mirrors the lower activity trend for Russia, but not Ukraine. If anything, Ukraine's losses have been increasing with time.

![Artillery losses for Russia (top) and Ukraine (bottom)](https://raw.githubusercontent.com/leedrake5/Russia-Ukraine/annual/monthly_artillery_yearcompare.jpg?)
Again, same trend as before; Russian losses are declining, while Ukrainian losses are picking up.

So, takeaways:
  - Russia tanks (monthly): 2022 had a March peak of ~315 and an October peak of ~240 — both invisible in the weekly
  smooth. The 2026 floor of 10–30/month is unambiguous against historical 60–140.
  - Russia artillery: 2025 (dark brown) is the lowest full-year line through most of the calendar — already the throttle
   year. 2026 tracks 2025 closely, not below it. The exhaustion-vs-throttle story for artillery looks like "Russia
  already ran low on artillery losses in 2025, before tanks collapsed in 2026."
  - Russia infantry carriers: April 2026 has a single ~190 spike against ~20–50 for the rest of the year — almost
  certainly an Oryx catch-up batch after a posting gap. The monthly absorbs most of this but not all. If you want, I can
   add a 3-month rolling mean overlay for the equipment plots to dampen that.
  - Ukraine infantry carriers: 2026 (black) is actually running higher than 2023–24 across Jan–May (~75–145 vs ~30–100),
   supporting the "Ukraine attrition elevated in 2026" reading from the weekly view.


**Western-Ukraine bias note.** The daily pull uses NASA's
`J1_VIIRS_C2_Russia_Asia_24h` regional feed, whose western edge tapers off
around lon 26°E. We do receive hits west of that, but density drops
sharply — ~95% of all in-Ukraine detections in the data lie east of lon
28°. So the Ukraine-wide window is biased toward the eastern half of the
country. That's fine for the year-over-year comparison (the bias is
constant), but the absolute numbers shouldn't be read as a true
country-wide count.

## Caveats

- **FIRMS is an indirect proxy.** Active-fire detections include
  agricultural stubble burning (spring and autumn dominate), wildfires,
  industrial heat sources, and the satellite revisit cadence — all of
  which can dwarf combat-related fires. Use the multi-year trend across
  multiple windows; don't read month-to-month wiggles.
- **2026 is partial.** Through 152 days (Jan 1 → Jun 1). The bar charts
  show absolute totals *and* per-day rates. Note that 2026 YTD has not
  yet hit the heavy summer/autumn agricultural burn season, which alone
  can pull the FIRMS per-day rate up by 2–3× — so a low 2026 per-day
  number for FIRMS is partly a calendar artifact, not just combat tempo.
- **Oryx revises retrospectively.** Entries get reclassified or removed
  as visual evidence is re-examined. The current snapshot shows Russia's
  "captured" count revised down 331 since 2025-12-31, which is why the
  2026 YTD "total units minus trucks" delta for Russia is *negative*.
  Real losses still happened in 2026; they're masked by the
  re-categorisations. This is normal data behaviour, not a bug.
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
| 2026 YTD | 93 | 54 |

Russia's annual tank attrition halved from the 2022 peak to 2025 — the
shift to motorbike/buggy/quad infantry assaults shows up clearly. The
Russia-to-Ukraine tank-loss ratio also compressed: ~3.6:1 in 2022 → ~1.8:1
in 2025 → roughly 1.7:1 in 2026 YTD.

### FIRMS detections per day, by window

| Year | Pokrovsk (tight) | Donbas oper. | Eastern wide | Ukraine-wide |
|---|---|---|---|---|
| 2022 | 5.8 | 52.4 | 86.6 | 263.4 |
| 2023 | 2.9 | 19.7 | 48.9 | 153.0 |
| 2024 | 18.1 | 82.4 | 159.8 | 347.7 |
| 2025 | 16.9 | 56.5 | 95.8 | 237.9 |
| 2026 YTD | 4.0 | 11.8 | 28.9 | 119.8 |

**2024 is the loudest year by FIRMS across every window** — driven by the
Avdiivka collapse and the Pokrovsk push. 2025 backed off from that peak
but stayed well above 2022/2023. The 2026 YTD numbers are genuinely
lower, but a meaningful share of that gap is the missing summer/autumn
burn season — defer judgement until late 2026.

Adding the country-wide window mostly confirms what the eastern windows
show: the shape of the year-over-year curve is the same in all four
windows. That's reassuring — the macro pattern isn't a Donbas-only
artifact. The country-wide ratios (e.g. 2024/2023 ≈ 2.3×) are close to
the eastern-wide ratios (3.3×) and the Donbas-operational ratio (4.2×),
which suggests national-scale escalation in 2024 with the sharpest
spike concentrated on the Donbas axes.

The monthly Donbas plot is the most honest view of recent tempo: the
2024-summer peak around the Avdiivka–Pokrovsk axis is unmistakable, and
the summer-2025 peak shows the front re-intensifying after a winter
quiet period. The early-2026 months are tracking similarly to early
2025, not to early 2024.
