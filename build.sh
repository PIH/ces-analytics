APPLICATION_YML=$(realpath shinyproxy/application.yml)

echo "== Setting up ShinyProxy =="
cd shinyproxy
    mkdir -p build/
    cp Dockerfile build/
    if [ ! -f credentials-secret.txt ]; then
        echo "ERROR: $(pwd)/credentials-secret.txt needs to exist.
            It should contain exactly the KeyCloak credentials secret.
            Please see README.md for information about how to obtain it."
        exit 1
    fi
    SECRET=$(<credentials-secret.txt)
    sed "s/<CREDENTIALS_SECRET>/$SECRET/" application.yml >build/application.yml
    cd build/
        docker build . -t shinyproxy
    cd ..
cd ..

echo "== Setting up Applications =="
cd apps
    for APP_DIR in *; do
        if [ -d "${APP_DIR}" ]; then
            cd ${APP_DIR}
                echo "-- Setting up ${APP_DIR}"
                docker build .
            cd ..
            if ! grep -q ${APP_DIR} ${APPLICATION_YML}; then
                echo WARNING: Shiny app \"${APP_DIR}\" not found in \
                    shinyproxy/application.yml
            fi
        fi
    done
cd ..

