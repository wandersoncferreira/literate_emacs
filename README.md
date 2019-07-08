# dotfiles

If you desire to install the package on your system, make sure that you do not have any folder on the `home` directory called `~/.emacs.d` or a file called `~.emacs`.

After checking, you can run the following command:

`git clone https://github.com/wandersoncferreira/dotfiles ~/.emacs.d`

If you prefer, you can clone the repository into your `home` folder and later add a `symlink` to `~/.emacs.d`.

```shell
git clone https://github.com/wandersoncferreira/dotfiles
ln -s dotfiles ~/.emacs.d
```

## installing external dependencies

### Python

In order to `Elpy` to work properly. You have to install the following Python packages:
```python
pip install jedi rope autopep8 yapf black flake8
```

You can check if everything went fine by running `M-x elpy-config`


# features

There are settings to handle a bunch of situations for a programmer. However, what can really make some difference on your daily usage is to be familiar with the `custom keybindings` that were added into the project.

look at the file `settings/setup-keybindings.el`.


have fun ;)
