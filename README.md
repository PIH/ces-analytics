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

Once all the above is taken care of, you can deploy to the server!
Run `./deploy.sh` to update the server with your work. Make sure you have your
latest changes committed to master with `git commit`.

### Troubleshooting

#### Launching my app from ShinyProxy times out with `Container did not respond in time`

It's probably failing to start, probably because it didn't install all the libraries correctly. To debug:

1. Log in to the server
1. Run `docker images` to find your app's image name
1. Run `docker run <my image name>` to start the app in Docker manually

You should see an error message at this point.

#### ShinyProxy sometimes fails to start apps with `500 - NullPointerException`

Due to
[this issue](https://support.openanalytics.eu/t/shinyproxy-keycloak-authenticated-but-cant-open-apps-consistently/770/5),
users must have emails defined in KeyCloak. If they don't, you'll see this error the second time you launch an app in
a session.

## Administrator Documentation

### Initial Set-up

The setup of this whole stack is based on
[this tutorial](https://github.com/brandones/shiny-keycloak/).
Mostly you'll just want to follow along there.

You can use `fabfile install_docker install_docker_compose` to get those
steps out of the way.

`fabfile install_analytics` will set up the repository at `/opt/ces-analytics/`.
It will also attempt to copy `credentials-secret.txt`.

KeyCloak does not support ZeroConf/Bonjour addresses (like "ces-oficina.local").
The server needs to have a static IP assigned. The point at which this is used
(where the ZeroConf address cannot be used) is in ShinyProxy's `application.yml`.
As of this writing, the server is configured (via the router admin page, on
the router for `ces-oficina-1`, under `LAN`) at `192.168.1.63`.

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

### Adding/Editing Users

1. Open a tunnel to the Keycloak port on the server: `ssh -L 9080:localhost:9080 $SERVER_IP`
1. Navigate to `localhost:9080`
1. Sign in as the admin user
1. Make sure the realm, the drop-down on the left, is set to "Shinyproxy" (and not "Master")
1. In the left menu, click on "Users"

Always ensure that every user has an email defined. Failing to do so will result in a confusing error due to
[this bug](https://support.openanalytics.eu/t/shinyproxy-keycloak-authenticated-but-cant-open-apps-consistently/770/5).
