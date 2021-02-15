FROM fpco/stack-build-small

ENV CONFIG_DIR=/root/.config/sandbar
ENV APP_DIR=/app

# Install dependencies
# See: https://github.com/xmonad/xmonad/blob/master/.github/workflows/tests.yml
RUN apt update
RUN apt install -y libasound2 libasound2-dev libxrandr-dev libtinfo-dev libx11-dev libgmp-dev libxss-dev libxft-dev

WORKDIR $APP_DIR
