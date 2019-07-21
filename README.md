# dotfiles

Tested on Emacs 25+, but preferences are for Emacs 26+. [How to Install Emacs 26+](http://ubuntuhandbook.org/index.php/2019/02/install-gnu-emacs-26-1-ubuntu-18-04-16-04-18-10/)

If you desire to install the package on your system, make sure that you do not have any folder on the `home` directory called `~/.emacs.d` or a file called `~.emacs`.

After checking, you can run the following command:

`git clone https://github.com/wandersoncferreira/dotfiles ~/.emacs.d`

If you prefer, you can clone the repository into your `home` folder and later add a `symlink` to `~/.emacs.d`.

```shell
git clone https://github.com/wandersoncferreira/dotfiles
ln -s dotfiles ~/.emacs.d
```

## setup your own parameters

In order to avoid changing core configurations, you can override the default behavior
of this configuration by creating files in the **users/** folder.

For example, let's say you do not like the default color theme and want to change it
to *leuven*

```shell
touch users/my-settings.el
```

open the new created file and type:

```emacs
(load-theme 'leuven t)
```

That's it. Any other settings can be placed in this folder, even in separate files
if you feel that this organizes your workflow better.

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
