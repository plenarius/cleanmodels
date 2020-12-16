# cleanmodels
Continued development on OldManBeard's CleanModels application for Neverwinter Nights

# Download

You can download binaries from the [Releases](https://github.com/plenarius/cleanmodels/releases) tab.

# Installation

On Ubuntu/Debian
```
$ git clone https://github.com/plenarius/cleanmodels
$ cd cleanmodels
$ sudo apt-get install swi-prolog
$ swipl -g go -o cleanmodels --stand_alone=true --foreign=save -c cleanmodels.pl
```

This will create an executable `cleanmodels` binary which you can run with `cleanmodels last_dirs.pl` or see `cleanmodels --help` for some command line switches.

# Usage

Currently the command line interface is not of very much use without editing `last_dirs.pl` file which is used to set the configuration variables. There are some command line switches though, see `--help` for more information. The full set of configuration options will eventually be available as switches.
