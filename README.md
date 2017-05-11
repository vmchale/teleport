# Teleport

A fork of [teleport](https://github.com/bollu/teleport) with a focus on clean,
functional code.

## Use

Unfortunately, since we can't change the directory of the user (possibly an
upstream bug), we have to wrap `teleport-hask` in a bash script, so put
`bash/teleport` on your `PATH` somewhere.

I also put the following in my `~/.bashrc` so that `teleport` would run in the
correct shell:

```bash
alias go="source $HOME/.local/bin/teleport"
```

## Code

[tokei](https://github.com/Aaronepower/tokei) output for previous package:

```
-------------------------------------------------------------------------------
 Language            Files        Lines         Code     Comments       Blanks
-------------------------------------------------------------------------------
 Cabal                   1           60           53            1            6
 Haskell                 4          383          248           51           84
 Markdown                2           22           22            0            0
 Shell                   1           11            7            3            1
 YAML                    1           29            5           16            8
-------------------------------------------------------------------------------
 Total                   9          505          335           71           99
-------------------------------------------------------------------------------
```

For the forked package:

```
-------------------------------------------------------------------------------
 Language            Files        Lines         Code     Comments       Blanks
-------------------------------------------------------------------------------
 Cabal                   1           51           48            0            3
 Haskell                 3          213          167            9           37
 Markdown                1           23           23            0            0
 YAML                    1            6            6            0            0
-------------------------------------------------------------------------------
 Total                   6          293          244            9           40
-------------------------------------------------------------------------------
```
