# sandbar

## How to develop on Mac with Docker + XQuartz

1. Build an image.

    ```bash
    $ docker build -t sandbar .
    ```

1. Add localhost to the access control list.

    ```bash
    $ xhost + localhost
    ```

1. Create the container with shared volumes.

    ```bash
    $ docker create -e DISPLAY=host.docker.internal:0 \
        -v (PWD):/app/ \
        -v (PWD)/example.config.yaml:/root/.config/sandbar/config.yaml \
        --name sandbar -it sandbar
    ```

1. Start the container.

    ```bash
    $ docker start sandbar
    ```

1. Enter in the container.

    ```bash
    $ docker exec -it sandbar bash
    ```

1. You can update the code in your container and build it in the container.
