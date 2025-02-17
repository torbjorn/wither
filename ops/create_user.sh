#!/bin/bash
set -e

# this script is a rewrite of:
# https://github.com/rocker-org/rocker-versioned2/blob/44cfe083241d2a113bff5a57db7c470823f84cbe/scripts/default_user.sh

DEFAULT_USER=${1:-${DEFAULT_USER:-"rstudio"}}
USER_UID=${2:-1000}
USER_GID=${3:-1000}

useradd -s /bin/bash -m "$DEFAULT_USER"
echo "${DEFAULT_USER}:${DEFAULT_USER}" | chpasswd
usermod -a -G staff "${DEFAULT_USER}"

## Rocker's default RStudio settings, for better reproducibility
mkdir -p "/home/${DEFAULT_USER}/.config/rstudio/"
cat <<EOF >"/home/${DEFAULT_USER}/.config/rstudio/rstudio-prefs.json"
{
    "save_workspace": "never",
    "always_save_history": false,
    "reuse_sessions_for_project_links": true,
    "posix_terminal_shell": "bash"
}
EOF
chown -R "${DEFAULT_USER}:${DEFAULT_USER}" "/home/${DEFAULT_USER}"

## configure git not to request password each time
if [ -x "$(command -v git)" ]; then
    git config --system credential.helper 'cache --timeout=3600'
    git config --system push.default simple
fi
