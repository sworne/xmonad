FROM debian:testing-slim
COPY . /var/tmp/xmonad/
WORKDIR /var/tmp/xmonad/
RUN ["/bin/bash", "/var/tmp/xmonad/deploy.sh", "--ci"]