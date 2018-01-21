# Wanderson Ferreira
# Copyright 2017, 2018
# Bash settings


export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8


# general aliases
alias reload="source ~/.bash_profile"
alias ll="ls -laghFG"
alias goCaptalys="source activate captalys"
alias magit='ec -e "(magit-status \"$(pwd)\")"'


if [ $(uname -s) == "Darwin" ]; then
    alias goFlock="source activate flock"
    alias errorFlock="cd ~/.flock/"
    alias ls="gls"
    alias projFlock="cd ~/github/Flock/"
    alias listAppStore="mdfind kMDItemAppStoreHasReceipt=1"
    alias upPythonPkg="twine upload dist/*"
    alias buildPythonPkg="python setup.py bdist_wheel"

    # added by Miniconda3 installer
    export PATH="/Users/wandersonferreira/miniconda3/bin:$PATH"
    export PATH="/Users/wandersonferreira/dotfiles/scripts:$PATH"
    export GOPATH="/Users/wandersonferreira/go"
fi

# verificar o uname -s do pc do trabalho.
if [ $(uname -s) == "OpenSuSe" ]; then
    # added by Miniconda3 installer
    export PATH="/home/wanderson/miniconda3/bin:$PATH"
    export PATH="/home/wanderson/dotfiles/scripts:$PATH"
    export GOPATH="/home/wanderson/go"
fi


# Git
git config --global color.ui true
git config --global format.pretty oneline
git config --global core.autoctrl input
git config --global core.fileMode true
git config --global alias.lg "log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"

alias push="git pull && git push"
alias pull="git pull"
alias clone="git clone $1"
