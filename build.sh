echo "== Setting up ShinyProxy =="
cd shinyproxy
    mkdir -p build/
    cp Dockerfile build/
    SECRET=$(<credentials-secret.txt)
    sed "s/<CREDENTIALS_SECRET>/$SECRET/" application.yml >build/application.yml
    cd build/
        docker build . -t shinyproxy
    cd ..
cd ..
