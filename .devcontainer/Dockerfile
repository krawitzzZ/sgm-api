ARG VARIANT=8.10.7
FROM haskell:${VARIANT}

ENV TZ='Europe/Berlin'

# create user and setup ssh
ARG USERNAME=sgm-api
ARG USER_UID=1001
ARG USER_GID=$USER_UID

RUN groupadd --gid $USER_GID $USERNAME \
  && useradd --uid $USER_UID --gid $USER_GID -m $USERNAME \
  # sudo support
  && apt-get update \
  && apt-get install -y sudo \
  && echo $USERNAME ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/$USERNAME \
  && chmod 0440 /etc/sudoers.d/$USERNAME

# [Option] Install zsh
ARG INSTALL_ZSH="true"
# [Option] Upgrade OS packages to their latest versions
ARG UPGRADE_PACKAGES="true"

WORKDIR /sgm-api

# Install needed packages and setup non-root user. Use a separate RUN statement to add your own dependencies
COPY library-scripts/*.sh /tmp/library-scripts/
RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
  && /bin/bash /tmp/library-scripts/common-debian.sh "${INSTALL_ZSH}" "${USERNAME}" "${USER_UID}" "${USER_GID}" "${UPGRADE_PACKAGES}" "true" "true"\
  && apt-get autoremove -y && apt-get clean -y && rm -rf /var/lib/apt/lists/* /tmp/library-scripts

USER $USERNAME

RUN mkdir -p /home/$USERNAME/.ssh && ln -s /run/secrets/git_ssh_key /home/$USERNAME/.ssh/git_rsa
RUN echo "Host bitbucket.org\n\tHostname altssh.bitbucket.org\n\tPort 443\n\tIdentityFile /home/$USERNAME/.ssh/git_rsa\n" >> /home/$USERNAME/.ssh/config

RUN echo "export PATH='$HOME/.local/bin:$PATH'" >> /home/$USERNAME/.zshrc

# [Optional] Uncomment this section to install additional OS packages.
# RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
#     && apt-get -y install --no-install-recommends <your-package-list-here>
