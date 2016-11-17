## opam-ci: reports on repository pull-requests to guide merge decisions

This bot runs as an HTTP service that wakes up through a GitHub webhook. It's
based on cohttp (and lwt) for the server part, ocaml-git for checking out the
states corresponding to the pull-request, opam-lib (and dose) for linting and
checking installable packages, and ocaml-github to post back the reports.

Dependencies: `cohttp`, `conduit`, `opam-lib`, `git`, `yojson`, `github`. The
current version has been tested with:
- camlzip 1.06
- git 1.9.1
- github 2.0.3
- lwt 2.5.2
- nocrypto 0.5.3
- opam-lib pinned to the 1.3 branch at 833e5b22

The program expects a `opam-ci.conf` in the working directory, with the
following format:

```
name: "opam-ci"
port: 8122
token: "xxx"
secret: "yyy"
repo-user: "ocaml"
repo-name: "opam"
```

The name and token correspond to the GitHub account and token to use (only
public repository access is needed); `repo-user:` and `repo-name:` define the
repository the service will be running on (here "ocaml/opam"). The port and
secret should correspond to the webhook configuration, and the service is at
`/opam-ci`, so your configuration in Github should look like:
- http://opam.ocaml.org:8122/opam-ci
- `secret` must be identical, it's used to authenticate requests coming from
  your repository on GitHub

The repository is cloned (bare) under `name%repo.git/.git` in the current
directory, _e.g._ `opam%opam-repository.git/.git`.
