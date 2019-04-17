from __future__ import print_function

import time
from fabric.api import env, put, local, run, sudo
from fabric.context_managers import cd

env.user = "doc"
env.hosts = ["ces-oficina.local"]
env.use_ssh_config = False
env.no_agent = True
env.no_keys = True


def configure():
    """ Does a little bit of Ubuntu system set-up.
    
    Specifically:
        - Configures Ubuntu to automatically add all new users to group "users"
        - Configures Keychain for all login shells
        - that's it for now
    """
    # All users should automatically be in group "users"
    sudo("sed -i 's/#EXTRA_GROUPS\/.*/EXTRA_GROUPS=\"users\"/' /etc/adduser.conf")
    sudo("sed -i 's/#ADD_EXTRA_GROUPS=.*/ADD_EXTRA_GROUPS=1/' /etc/adduser.conf")
    sudo("echo '/usr/bin/keychain -q $HOME/.ssh/id_rsa' >/etc/profile.d/keychain.sh")
    sudo("echo 'source $HOME/.keychain/'$HOSTNAME-sh >>/etc/profile.d/keychain.sh")


def install_docker():
    """ Installs docker, assuming the machine is running Ubuntu. """
    sudo(
        "apt-get install \
            apt-transport-https \
            ca-certificates \
            curl \
            gnupg-agent \
            software-properties-common"
    )
    sudo("curl -fsSL https://download.docker.com/linux/ubuntu/gpg | apt-key add -")
    sudo(
        "add-apt-repository "
        '"deb [arch=amd64] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable"'
    )
    sudo("apt-get update")
    sudo("apt-get install -y docker-ce  docker-ce-cli containerd.io")


def install_docker_compose():
    """ Installs docker, assuming the machine is running some kind of Debian/Ubuntu. """
    sudo(
        'curl -L "https://github.com/docker/compose/releases/download/1.18.0/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose'
    )
    sudo("chmod +rx /usr/local/bin/docker-compose")


def install_analytics():
    """ Copies the CES SSH key to the server and sets up this repository in /opt. """
    put("~/.ssh/id_rsa.ces", "~/.ssh/id_rsa")
    put("~/.ssh/id_rsa.ces.pub", "~/.ssh/id_rsa.pub")
    sudo("chmod 600 ~/.ssh/id_rsa")
    sudo("cp ~/.ssh/id_rsa ~root/.ssh/")
    sudo("chown -R root:root ~root/.ssh/")
    sudo("chmod 600 ~root/.ssh/id_rsa")
    with cd("/opt"):
        sudo("git clone git@github.com:brandones/ces-analytics.git")
        sudo("chown -R doc:users ces-analytics/")
        sudo("chmod -R g+w ces-analytics/")
    send_secret()


def send_secret():
    """ Copies system/shinyproxy/credentials-secret.txt to the target server. """
    put("./shinyproxy/credentials-secret.txt", "/opt/ces-analytics/system/shinyproxy/")


def docker_clean():
    """ Frees up space from docker detritus. """
    run("docker system prune -f")


def deploy():
    """ Syncs this repository, runs `./build.sh`, updates Docker Compose, cleans. """
    print()
    print("Make sure you've committed and pushed everything!\n")
    time.sleep(2)
    local("git status -uno")
    with cd("/opt/ces-analytics"):
        run("whoami")
        run("git pull --force origin master")
        run("./build.sh")
        with cd("system"):
            run("docker-compose -p ces-analytics up -d")
    docker_clean()
