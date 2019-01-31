# CES Analytics

This is the R Shiny analytics platform for PIH Mexico / Compañeros en Salud.

## Getting Started

### What's Where

- `apps/` contains Shiny applications, each in its own directory.

- `apps.yml` tells ShinyProxy what apps are defined (in `apps/`).

- `deploy.sh` updates CES Analytics on the server. Your changes must already
have been committed with `git commit`. `./deploy.sh` will run `git status` as
its first order of business, so you'll see if there are uncommitted files.

- `setup-dev-environment.sh` installs on your computer the things you'll need
to deploy this system to a server. Right now that's just
[Fabric 1.x](https://www.fabfile.org/installing-1.x.html).

- `system/` contains a lot of machinery that you don't need to worry about if
you're just developing Shiny apps.

### Developing

First things first, run `./setup-dev-environment.sh` to install prerequisites.
You'll need to have Python 2 and [pip2](https://www.makeuseof.com/tag/install-pip-for-python/)
already installed on your computer. It won't work with Python 3 / pip3
(you can have these installed, but you need Python 2 / pip2 also).

Okay, now you can create your Shiny app. Create a new directory in `apps/`.
The name of this directory will be the "image name" of your app. Your app
needs two things:

1. The R files that define your application. Probably `app.R`, or `ui.R`
and `server.R`, and whatever other R files support them.
2. A Dockerfile. Mostly you can just copy/paste from other apps. The part
you might need to customize is the line
`RUN R -e install.packages(c('shiny', ...`, which should list all the R
libraries your app depends on.

The other change you'll need to make is to add your app to `apps.yml`. Don't
mess with the format of this file, just add another group of lines like the
ones that already exist (the indentation level is important, also):
```
  - id: <something unique>
    display-name: The Name that ShinyProxy Should Display
    description: Some descriptive text, which will appear under the name
    container-image: the "image name" i.e. the name of the directory in apps/
    container-network: sp-net
```

Once you've got your app working and have updated `apps.yml`,
run `./deploy.sh` to update the server
with your work. Make sure you have your latest changes committed to master
with `git commit`.

## Technical Details

Everything in this project runs in Docker, orchestrated by Docker Compose.

KeyCloak backed by MariaDB is used for authentication.

ShinyProxy serves Shiny apps to signed-in users.

The setup of this whole stack is based on
[this tutorial](https://github.com/brandones/shiny-keycloak/).
Please look there for additional technical details.

The server setup and deployment process is facilitated with
[Fabric 1.x](https://www.fabfile.org/installing-1.x.html).

To understand how deployment works, please see the `deploy` function in 
`system/fabfile.py`, and the script `system/build.sh`. There is a bit of
cleverness involved in hiding technical details from the Shiny app developer.

