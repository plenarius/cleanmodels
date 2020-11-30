# cleanmodels
Continued development on OldManBeard's CleanModels application for Neverwinter Nights

# Installation

On Ubuntu/Debian
```
$ git clone https://github.com/plenarius/cleanmodels
$ cd cleanmodels
$ sudo apt-get install swi-prolog
$ swipl -p library=/usr/lib/swi-prolog/xpce/prolog/lib -g go -o cleanmodels352 --stand_alone=true -c cleanmodels352.pl
```

This will create an executable cleanmodels352 binary.
