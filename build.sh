APPLICATION_YML=$(realpath shinyproxy/application.yml)

echo
echo "== Setting up ShinyProxy =="
# We use `yq` to merge the shinyproxy application config with the specs
# provided in apps.yml. Then we fill in the CREDENTIALS_SECRET.
# Then we build the docker image.
docker run -v ${PWD}:/workdir mikefarah/yq \
    yq m apps.yml system/shinyproxy/application.yml \
    > system/shinyproxy/build/application.yml
cd system
    cd shinyproxy
        mkdir -p build/
        cp Dockerfile build/
        if [ ! -f credentials-secret.txt ]; then
            echo "ERROR: system/shinyproxy/credentials-secret.txt needs to exist.
                It should contain exactly the KeyCloak credentials secret.
                Please see README.md for information about how to obtain it."
            exit 1
        fi
        SECRET=$(<credentials-secret.txt)
        sed -i "s/<CREDENTIALS_SECRET>/$SECRET/" build/application.yml
        cd build/
            docker build . -t shinyproxy
        cd ..
    cd ..
cd ..

echo "== Setting up Applications =="
cd apps
    for APP_DIR in *; do
        if [ -d "${APP_DIR}" ]; then
            cd ${APP_DIR}
                echo
                echo
                echo "-- Setting up ${APP_DIR}"
                docker build . -t ${APP_DIR}
            cd ..
            if ! grep -q ${APP_DIR} ${APPLICATION_YML}; then
                echo WARNING: Shiny app \"${APP_DIR}\" not found in \
                    shinyproxy/application.yml
            fi
        fi
    done
cd ..

