## opam-ci: reports on repository pull-requests to guide merge decisions

This bot runs as an HTTP service that wakes up through a GitHub webhook. It's
based on cohttp (and lwt) for the server part, ocaml-git for checking out the
states corresponding to the pull-request, opam-lib (and dose) for linting and
checking installable packages, and ocaml-github to post back the reports.

Dependencies: `cohttp`, `conduit`, `opam-lib`, `git`, `yojson`, `github`.

The program expects a `opam-ci.conf` in the working directory, with the
following format:

```
name: "opam-ci"
port: 8122
token: "xxx"
secret: "yyy"
```

The name and token correspond to the GitHub account and token to use (only
public repository access is needed). The port and secret should correspond to
the webhook configuration, and the service is at `/opam-ci`, so your
configuration should look like:
- http://opam.ocaml.org:8122/opam-ci
- `secret` must be identical, it's used to authenticate requests coming from
  your repository on GitHub

The repository is cloned (bare) under `name%repo.git/.git` in the current
directory, _e.g._ `opam%opam-repository.git/.git`.
