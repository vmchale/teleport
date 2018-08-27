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
