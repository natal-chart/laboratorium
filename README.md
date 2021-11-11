# Laboratorium

Half-baked experiments for our other apps. All commands require that ephemeris files
are provided in the `ephe` directory.

## Charts

Generate charts for a given interval, in reference to a planet (or planets).

```sh
[nix-shell:~/code/natalch.art/laboratorium]$ cabal new-run laboratorium  -- charts --help
Usage: laboratorium charts (-d|--date ARG) (-s|--start ARG) (-e|--end ARG)
                           --transited ARG --transiting ARG (-c|--chart ARG) 
                           [--debug]
  Work with transit charts

Available options:
  --transited ARG          space-separated list of transited planets
  --transiting ARG         space-separated list of transiting planets
  --debug                  print debug information
  -h,--help                Show this help text

```

Some example charts:


![transit chart](https://user-images.githubusercontent.com/82133/127580799-a1b91f58-5f74-4587-bc9b-bef2baf2bb48.png)

Here's another example "overview" chart, focused on all planet movements throughout a given period:

![image](https://user-images.githubusercontent.com/82133/122154039-2044d280-ce32-11eb-8edb-5d051b2f4a5b.png)

## Queries

"Almanacs" can be generated for intervals of time, to print events of interest; such as natal transits, moon phases, eclipses, zodiac ingresses. For example, here's
some "mundane" events for the end of October 2021:

```
[nix-shell:~/code/natalch.art/laboratorium]$ time cabal new-run laboratorium -- query -q "WorldAlmanac" -s "2021-10-20" -e "2021-10-31" --ephe-path "./ephe"
All events in EST : 
----------------- 
2021-10-20
============
FullMoon
Starts at: 
2021-10-20 00:00:00.000013411045 EST
Ends at: 
2021-10-24 00:00:00.000013411045 EST
Exact at:
    2021-10-20 09:56:41.524794101715 EST
############################
2021-10-22
============
(Sun,"enters",Scorpio,DirectMotion)
Starts at: 
2021-10-22 00:00:00.000013411045 EST
Ends at: 
2021-10-23 00:00:00.000013411045 EST
Exact at:
    2021-10-22 23:51:10.147928595542 EST
############################
2021-10-24
============
WaningGibbous
Starts at: 
2021-10-24 00:00:00.000013411045 EST
Ends at: 
2021-10-28 00:00:00.000013411045 EST
Exact at:
    2021-10-24 12:33:05.060982406139 EST
############################
2021-10-28
============
LastQuarter
Starts at: 
2021-10-28 00:00:00.000013411045 EST
Ends at: 
2021-10-31 00:00:00.000013411045 EST
Exact at:
    2021-10-28 15:05:10.918101668357 EST
############################
2021-10-30
============
(Mars,"enters",Scorpio,DirectMotion)
Starts at: 
2021-10-30 00:00:00.000013411045 EST
Ends at: 
2021-10-31 00:00:00.000013411045 EST
Exact at:
    2021-10-30 09:21:06.681253910064 EST
############################

real    0m0.127s
user    0m0.087s
sys     0m0.027s
```

## Development

This repository also serves as an informal testing ground for library updates,
to add a nix overlay for a version of a library that's not yet on Hackage, one
can use `cabal2nix`, e.g.

```sh
cabal2nix --revision=019fe42d8718cd24cd523ab7fae11207ec036a52 https://github.com/lfborjas/swiss-ephemeris > nix/extra-pkgs/swiss-ephemeris.nix
```

Where the `revision` argument is the commit/branch one wants to check out.
