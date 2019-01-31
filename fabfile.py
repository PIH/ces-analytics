from fabric.api import env, put, local, run, sudo
from fabric.context_managers import cd

env.hosts = ["192.168.1.106"]


def configure():
    # All users should automatically be in group "users"
    sudo("sed -i 's/#EXTRA_GROUPS\/.*/EXTRA_GROUPS=\"users\"/' /etc/adduser.conf")
    sudo("sed -i 's/#ADD_EXTRA_GROUPS=.*/ADD_EXTRA_GROUPS=1/' /etc/adduser.conf")


def install_docker():
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
    sudo(
        'curl -L "https://github.com/docker/compose/releases/download/1.18.0/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose'
    )
    sudo("chmod +rx /usr/local/bin/docker-compose")


def install_analytics():
    put("~/.ssh/id_rsa.ces", "~/.ssh/id_rsa")
    sudo("chmod 600 ~/.ssh/id_rsa")
    sudo("cp ~/.ssh/id_rsa ~root/.ssh/")
    sudo("chown -R root:root ~root/.ssh/")
    sudo("chmod 600 ~root/.ssh/id_rsa")
    with cd("/opt"):
        sudo("git clone git@github.com:brandones/ces-analytics.git")
        sudo("chown -R doc:users ces-analytics/")
        sudo("chmod -R g+w ces-analytics/")


def deploy():
    local("git status")
    local("git push origin master")
    with cd("/opt/ces-analytics"):
        run("git pull --force origin master")
        run("./build.sh")
        run("docker-compose up -d")
