# Setup

A useful command to figure out what packages are needed.
```
opam list --safe --recursive --depexts --required-by=<packagename>
```

You need pkg-config

Unix:

```
sudo apt-get install pkg-config
sudo apt-get install libx11-dev
```

Mac (Pick One)
```
brew install pkg-config
sudo port install pkgconfig
```

run:

```
opam install graphics
```

You will also need these other opam packages:
- unix
- oUnit
- str
- ANSITerminal
- graphics
- yojson

You will need an XServer to display graphics:

Windows:
XMing
https://sourceforge.net/projects/xming/

Mac:
XQuartz
Install via: https://www.xquartz.org/

Unix:
???

That should be all.