# Laboratorium

Half-baked experiments for our other apps.

## Transit Charts

Generate transit charts for a given event, in reference to a planet (or planets).

```sh
Usage: laboratorium (-d|--date ARG) (-s|--start ARG) (-e|--end ARG)
                    (-p|--planets ARG)
  Plot transit charts for the given natal planets, in the given time range, for
  a specific birth/event date
```

Example:


```sh
cabal new-run laboratorium -- --date "1989-01-07T05:30:00Z" --start "2020-01-01" --end "2021-12-31" --planets "Mars Jupiter"
```

It will output charts in the `charts/` folder, can open them with any SVG viewer.
