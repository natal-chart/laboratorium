# Laboratorium

Half-baked experiments for our other apps. All commands require that ephemeris files
are provided in the `ephe` directory.

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
cabal new-run laboratorium -- charts --date "1989-01-07T05:30:00Z" --start "2021-01-01" --end "2022-01-31" --planets "Jupiter" --ephe-path "./ephe"
```

It will output charts in the `charts/` folder, can open them with any SVG viewer.

Here's an example chart:

![image](https://user-images.githubusercontent.com/82133/122154039-2044d280-ce32-11eb-8edb-5d051b2f4a5b.png)

## Development

This repository also serves as an informal testing ground for library updates,
to add a nix overlay for a version of a library that's not yet on Hackage, one
can use `cabal2nix`, e.g.

```sh
cabal2nix --revision=019fe42d8718cd24cd523ab7fae11207ec036a52 https://github.com/lfborjas/swiss-ephemeris > nix/extra-pkgs/swiss-ephemeris.nix
```

Where the `revision` argument is the commit/branch one wants to check out.
