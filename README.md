# CES Analytics

This is the R Shiny analytics platform for PIH Mexico / Compañeros en Salud.

## User Documentation

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

Before you can deploy, you'll need access to the server. Ask your system
administrator to create a user for you on the server. That user should have
the CES deploy key at `~/.ssh/id_rsa[.pub]`, and should have your personal
SSH public key in `~/.ssh/authorized_keys`. If you don't have an SSH key,
please 
[create one](https://help.github.com/articles/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent/#generating-a-new-ssh-key)
and send *only the public key*, i.e. `id_rsa.pub`, to your system administrator.

Check that you have access to the server with `ssh username@the.server.address`.

Once all the above is taken care of, you can deploy to the server!
Run `./deploy.sh` to update the server with your work. Make sure you have your
latest changes committed to master with `git commit`.

## Administrator Documentation

### Initial Set-up

The setup of this whole stack is based on
[this tutorial](https://github.com/brandones/shiny-keycloak/).
Mostly you'll just want to follow along there.

You can use `fabfile install_docker install_docker_compose` to get those
steps out of the way.

`fabfile install_analytics` will set up the repository at `/opt/ces-analytics/`.
It will also attempt to copy `credentials-secret.txt`.

### Credentials Secret

The build process on the server will expect to find a file at
`/opt/ces-analytics/system/shinyproxy/credentials-secret.txt`. It should
contain only and exactly the KeyCloak credentials secret. You can find this
in the KeyCloak admin panel, assuming you've set KeyCloak up according to the
tutorial above, accordingly:

1. In the drop-down near the top-left, make sure the realm is set to "Shinyproxy"
(and not "Master").
1. In the left menu under "Configure," click "Clients"
1. In the top bar, click "Credentials," which should be the second tab
1. Copy the **Secret**

### Technical Details

Everything in this project runs in Docker, orchestrated by Docker Compose.

KeyCloak backed by MariaDB is used for authentication.

ShinyProxy serves Shiny apps to signed-in users.

The server setup and deployment process is facilitated with
[Fabric 1.x](https://www.fabfile.org/installing-1.x.html). To find out what
Fabric commands are configured, `cd` into the `system/` directory and run
`fab -l`. To find out more about a command run `fab -d <command>`.

To understand how deployment works, please see the `deploy` function in 
`system/fabfile.py`, and the script `system/build.sh`. There is a bit of
cleverness involved in hiding technical details from the Shiny app developer.

