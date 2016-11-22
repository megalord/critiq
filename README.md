# Critiq

[![Latest release](http://img.shields.io/github/release/megalord/critiq.svg)](https://github.com/megalord/critiq/releases)

Pull Request reviews inside [Neovim](https://neovim.io/).


### About

When you are looking at code, you are more effective using the tools you know.  Want fuzzy file search?  Jump to definition?  These interactions are probably very natural in your local environment, which is why using GitHub to do a code review can be limiting.  The goal of Critiq is to let you review pull requests without ever leaving Neovim.


### Installation

This is a Neovim [remote plugin](https://neovim.io/doc/user/remote_plugin.html) leveraging the [node host](https://github.com/neovim/node-host).

1. Modify your `init.vim` with `Plug 'neovim/node-host'` and run `:PlugInstall`, or adapt that for your plugin manager of choice.
2. Make sure the remote plugin directory is in your runtime path: add `let &rtp = &rtp.','.expand('~').'/.nvim'` to your `init.vim` if necessary.  Run `:set runtimepath?` to check its current value.
3. Download [`critiq.js`](https://github.com/megalord/critiq/releases/latest/) to the node remote plugin directory (`~/.nvim/rplugin/node/`).
4. Run `:UpdateRemotePlugins` to generate the manifest file.
5. Restart Neovim in a directory that is a git repo with an "origin" upstream that points to GitHub.
6. Run `:CritiqPR`


### Development

It's pretty simple, just run `pulp build --to ~/.nvim/rplugin/node/critiq.js` to build the js source in the rplugin directory.  When you start Neovim, set the `NEOVIM_JS_DEBUG` environment variable to a filepath to send the logs to.  Any calls to [`debug`](https://pursuit.purescript.org/packages/purescript-neovim/0.0.5/docs/Neovim.Plugin#v:debug) will log to that file.  Remember to update the manifest if you add a new autocommand, command, or function.


### Roadmap

In no particular order...
* toggle review comments in a file
* define a command to add comments in a new buffer
* make remote parsing more flexible
* open links in browser
* see status of hooks
